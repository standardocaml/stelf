open! Basis

(* Save Basis's OS before any module definitions shadow it *)
module BasisOS = OS

(* ------------------------------------------------------------------ *)
(* Module wiring                                                         *)
(* ------------------------------------------------------------------ *)

(* Ascribe Paths to the bare PATHS signature so it matches what         *)
(* Make_Cst and Make_Recon's S.S both expect.                           *)
module Paths : Paths.Paths_intf.PATHS = Paths.Paths_

(* Create our own Cst from the same ascribed Paths so that              *)
(* Cst.Paths = Paths and Recon.Cst.Paths = Paths, giving consistent     *)
(* type-sharing across the entire pipeline.                              *)
module Cst = Cst.Make_Cst (Paths)
module Names = Modern.Modern.Names
module Syntax = Syntax.IntSyn (Global.Global_.Global)
module Parser = Parsing.Parser.Parser

(* Force Cst/Names/Paths sharing so Cmd and load_string agree on types *)
(* Make_Modern is transparent (no return-type seal), so OCaml infers    *)
(* ModernImpl.Cst = Cst, ModernImpl.Paths = Paths, etc.                  *)
module ModernImpl = Modern.Make_Modern (Paths) (Cst) (Names) (Parser)
module Cmd = Modern.Cmd.Make_Cmd (ModernImpl)

(* Elaboration layer.  All sub-modules are currently stubs that raise   *)
(* descriptive errors.  install1 bypasses them with failwith.           *)
module Recon = Recon.Make_Recon (struct
  module Paths = Paths
  module Cst = Cst
  module Syntax = Syntax
  module Ast = Intsyn.IntSyn
end)

(* ------------------------------------------------------------------ *)
(* Source and mode                                                       *)
(* ------------------------------------------------------------------ *)

type source = File of Fpath.t | Input of string

let mode : [ `Repl | `Lsp | `Other ] ref = ref `Other

(* ------------------------------------------------------------------ *)
(* Global flags                                                          *)
(* ------------------------------------------------------------------ *)

let chatter = Global.Global_.Global.chatter
let double_check = Global.Global_.Global.doubleCheck
let unsafe = Global.Global_.Global.unsafe
let auto_freeze = Global.Global_.Global.autoFreeze
let time_limit = Global.Global_.Global.timeLimit

(* ------------------------------------------------------------------ *)
(* Status                                                               *)
(* ------------------------------------------------------------------ *)

type status = Ok | Abort
let status_to_exit = function Ok -> 0 | Abort -> 1
(* ------------------------------------------------------------------ *)
(* Options store                                                         *)
(* ------------------------------------------------------------------ *)

module Options = struct
  let tbl : (string, string) Hashtbl.t = Hashtbl.create 16
  let get k = Hashtbl.find_opt tbl k
  let set k v = Hashtbl.replace tbl k v
end

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let msg s = Msg.Msg_.Msg.message s

let print_error (label : string) (detail : string) : unit =
  Printf.eprintf "%s: %s\n%!" label detail

(* ------------------------------------------------------------------ *)
(* Install module                                                        *)
(* ------------------------------------------------------------------ *)

module Install = struct
  (* Elaboration cases stub out until Recon sub-modules are implemented *)
  let install1 (cmd : Cst.cmd) : unit =
    match cmd with
    | Cst.SortCmd_ _ ->
        failwith
          "PAL: sort declaration elaboration not yet implemented \
           (Recon.ReconConDec stub)"
    | Cst.TermCmd_ _ ->
        failwith
          "PAL: term declaration elaboration not yet implemented \
           (Recon.ReconConDec stub)"
    | Cst.QueryCmd_ _ ->
        failwith
          "PAL: query elaboration not yet implemented (Recon.ReconQuery stub)"
    | Cst.DefineCmd_ _ ->
        failwith
          "PAL: define elaboration not yet implemented (Recon.ReconQuery stub)"
    | Cst.SolveCmd_ _ ->
        failwith
          "PAL: solve elaboration not yet implemented (Recon.ReconQuery stub)"
    | Cst.StopCmd_ -> ()
    | Cst.QuitCmd_ -> BasisOS.Process.exit BasisOS.Process.success
    | Cst.HelpCmd_ topic ->
        begin match topic with
        | None ->
            msg
              "Commands: sort, term, query, define, solve, quit, help, get, \
               set, version\n"
        | Some t -> msg (("No help available for '" ^ t) ^ "'\n")
        end
    | Cst.GetCmd_ key ->
        begin match Options.get key with
        | Some v -> msg ((key ^ " = ") ^ v ^ "\n")
        | None -> msg (key ^ " is not set\n")
        end
    | Cst.SetCmd_ (key, value) -> Options.set key value
    | Cst.VersionCmd_ -> msg (Frontend.Version.Version.version_string ^ "\n")

  let install (cmds : Cst.cmd list) : unit = List.app install1 cmds
  let reset () : unit = Frontend.Frontend_.Stelf.reset ()
end

(* ------------------------------------------------------------------ *)
(* Loading                                                              *)
(* ------------------------------------------------------------------ *)

let load_string ?(filename = "<input>") (str : string) : status =
  let ns = ref (Names.newNamespace ()) in
  let loc = Cst.ghost in
  try
    let cmds = ModernImpl.run (Cmd.parse ()) ns loc str in
    try
      Install.install cmds;
      Ok
    with exn ->
      print_error filename (Printexc.to_string exn);
      Abort
  with
  | Modern.Modern.ParseError e ->
      print_error filename ("parse error: " ^ e);
      Abort
  | exn ->
      print_error filename (Printexc.to_string exn);
      Abort

let load (src : source) : status =
  match src with
  | Input str -> load_string str
  | File path -> (
      let filename = Fpath.to_string path in
      try
        let ic = TextIO.openIn filename in
        let str = TextIO.inputAll ic in
        let () = TextIO.closeIn ic in
        load_string ~filename str
      with exn ->
        print_error "pal" (Printexc.to_string exn);
        Abort)

let read_decl () : status =
  begin match TextIO.inputLine TextIO.stdIn with
  | Some line -> load_string ~filename:"<stdin>" line
  | None -> Ok
  end

let decl (name : string) : status =
  begin match Names.stringToQid name with
  | None ->
      msg (name ^ " is not a valid qualified identifier\n");
      Abort
  | Some qid ->
      begin match Names.constLookup qid with
      | None ->
          msg (name ^ " has not been declared\n");
          Abort
      | Some cid ->
          let condec = Intsyn.IntSyn.sgnLookup cid in
          msg (Print.Print_.conDecToString condec ^ "\n");
          Ok
      end
  end

(* ------------------------------------------------------------------ *)
(* Configuration file management                                        *)
(* ------------------------------------------------------------------ *)

module Config = struct
  type mfile = { filename : string; mutable mtime : Time.time option }
  type t = { sources : mfile list }

  let suffix = ref "cfg"
  let mk_mfile filename = { filename; mtime = None }

  let is_config item =
    let sfx = "." ^ !suffix in
    let n = Stdlib.String.length item and m = Stdlib.String.length sfx in
    n >= m && Stdlib.String.sub item (n - m) m = sfx

  let mk_rel base path =
    OS.Path.mkCanonical
      (if OS.Path.isAbsolute path then path else OS.Path.concat (base, path))

  let read (src : source) : t =
    let cfg_path =
      match src with
      | File p -> Fpath.to_string p
      | Input _ ->
          failwith "Config.read: cannot use Input source as config file"
    in
    let rec collect (sources, seen) cfg =
      if Stdlib.List.mem cfg seen then (sources, seen)
      else
        let seen' = cfg :: seen in
        let dir = OS.Path.dir cfg in
        let ic = TextIO.openIn cfg in
        let acc = ref (sources, seen') in
        (try
           while true do
             match TextIO.inputLine ic with
             | None -> raise Exit
             | Some raw_line ->
                 let line = Stdlib.String.trim raw_line in
                 let line =
                   match Stdlib.String.index_opt line '%' with
                   | Some i -> Stdlib.String.trim (Stdlib.String.sub line 0 i)
                   | None -> line
                 in
                 if line <> "" then begin
                   let item = mk_rel dir line in
                   let srcs, cfgs = !acc in
                   if is_config item then acc := collect (srcs, cfgs) item
                   else if
                     not
                       (Stdlib.List.exists (fun mf -> mf.filename = item) srcs)
                   then acc := (srcs @ [ mk_mfile item ], cfgs)
                 end
           done 
         with Exit -> ());
        TextIO.closeIn ic;
        !acc
    in
    let sources, _ = collect ([], []) cfg_path in
    { sources }

  let read_without ((src, existing) : source * t) : t =
    let fresh = read src in
    let existing_names =
      Stdlib.List.map (fun mf -> mf.filename) existing.sources
    in
    let sources' =
      Stdlib.List.filter
        (fun mf -> not (Stdlib.List.mem mf.filename existing_names))
        fresh.sources
    in
    { sources = sources' }

  let is_modified mf = mf.mtime = None

  let load_file (mf : mfile) (acc : status) : status =
    match acc with
    | Abort -> Abort
    | Ok ->
        let st = load (File (Fpath.v mf.filename)) in
        if st = Ok then mf.mtime <- Some Time.zeroTime;
        st

  let append (cfg : t) : status =
    let rec from_first_modified = function
      | [] -> []
      | mf :: rest ->
          if is_modified mf then mf :: rest else from_first_modified rest
    in
    let to_load = from_first_modified cfg.sources in
    Stdlib.List.fold_left (fun acc mf -> load_file mf acc) Ok to_load

  let load (cfg : t) : status =
    Install.reset ();
    Stdlib.List.iter (fun mf -> mf.mtime <- None) cfg.sources;
    append cfg

  let define (filenames : string list) : t =
    { sources = Stdlib.List.map mk_mfile filenames }
end

let make (src : source) : status = Config.load (Config.read src)

(* ------------------------------------------------------------------ *)
(* Print settings                                                       *)
(* ------------------------------------------------------------------ *)

module Print = struct
  let implicit = Print.Print_.implicit
  let print_infix = Print.Print_.printInfix
  let depth = Print.Print_.printDepth
  let length = Print.Print_.printLength
  let indent = Print.Print_.Formatter.indent
  let width = Print.Print_.Formatter.pagewidth
  let no_shadow = Print.Print_.noShadow
  let sgn () = Print.Print_.printSgn ()
  let prog () = Print.Print_.ClausePrint.printSgn ()
  let subord () = Subordinate.Subordinate_.Subordinate.show ()
  let def () = Subordinate.Subordinate_.Subordinate.showDef ()
  let domains () = msg (Frontend.Version.Version.version_string ^ "\n")

  module Tex = struct
    let sgn () =
      msg "\\begin{bigcode}\n";
      Print.Print_.PrintTeX.printSgn ();
      msg "\\end{bigcode}\n"

    let prog () =
      msg "\\begin{bigcode}\n";
      Print.Print_.ClausePrintTeX.printSgn ();
      msg "\\end{bigcode}\n"
  end
end

(* ------------------------------------------------------------------ *)
(* Reconstruction trace options                                          *)
(* ------------------------------------------------------------------ *)

module ReconOpts = struct
  type trace_mode = Progressive | Omniscient

  let trace = Recon.ReconTerm.trace
  let trace_mode : trace_mode ref = ref Progressive
end

(* ------------------------------------------------------------------ *)
(* Execution trace settings                                             *)
(* ------------------------------------------------------------------ *)

module Trace = struct
  type 'a spec = None | Some of 'a list | All

  let to_opsem : string spec -> string Opsem.Opsem_.Trace.spec = function
    | None -> Opsem.Opsem_.Trace.None
    | Some lst -> Opsem.Opsem_.Trace.Some lst
    | All -> Opsem.Opsem_.Trace.All

  let trace spec = Opsem.Opsem_.Trace.trace (to_opsem spec)
  let break spec = Opsem.Opsem_.Trace.break (to_opsem spec)
  let detail = Opsem.Opsem_.Trace.detail
  let show () = Opsem.Opsem_.Trace.show ()
  let reset () = Opsem.Opsem_.Trace.reset ()
end

(* ------------------------------------------------------------------ *)
(* Timers                                                               *)
(* ------------------------------------------------------------------ *)

module Timers = struct
  let show () = Timing.Timers.Timers.show ()
  let reset () = Timing.Timers.Timers.reset ()
  let check () = Timing.Timers.Timers.check ()
end

(* ------------------------------------------------------------------ *)
(* Compiler / optimisation settings                                     *)
(* ------------------------------------------------------------------ *)

module Compile = struct
  type opt = No | Linear_heads | Indexing

  let optimize : opt ref = ref Linear_heads
end

(* ------------------------------------------------------------------ *)
(* Meta-theorem prover settings                                         *)
(* ------------------------------------------------------------------ *)

module Prover = struct
  type strategy = Rfs | Frs

  let strategy : strategy ref = ref Frs
  let max_split = M2.MetaGlobal.MetaGlobal.maxSplit
  let max_recurse = M2.MetaGlobal.MetaGlobal.maxRecurse
end

(* ------------------------------------------------------------------ *)
(* Tabling settings                                                     *)
(* ------------------------------------------------------------------ *)

module Table = struct
  type strategy = Variant | Subsumption

  let strategy : strategy ref = ref Variant
  let strengthen = Opsem.TableParam.TableParam.strengthen
  let reset_global_table = Opsem.TableParam.TableParam.resetGlobalTable
  let top () = ()
end

(* ------------------------------------------------------------------ *)
(* OS utilities                                                         *)
(* ------------------------------------------------------------------ *)

module OS = struct
  let chdir = BasisOS.FileSys.chDir
  let getdir = BasisOS.FileSys.getDir
  let exit () = BasisOS.Process.exit BasisOS.Process.success
end

(* ------------------------------------------------------------------ *)
(* Interactive evaluation                                               *)
(* ------------------------------------------------------------------ *)

module Eval = struct
  let eval (cmd : Cst.cmd) : unit = Install.install1 cmd
end

(* ------------------------------------------------------------------ *)
(* Version string                                                       *)
(* ------------------------------------------------------------------ *)

let version : string = Frontend.Version.Version.version_string

(* ------------------------------------------------------------------ *)
(* Interactive top-level (REPL via lambda-term / TUI)                   *)
(* ------------------------------------------------------------------ *)

let top () : int Lwt.t =
  mode := `Repl;
  Tui.Repl.Repl.read (fun line ->
      (try
         let ns = ref (Names.newNamespace ()) in
         let loc = Cst.ghost in
         let cmds = ModernImpl.run (Cmd.parse ()) ns loc line in
         List.app Install.install1 cmds
       with exn ->
         (* TODO, add error handling only when error is not needing more input *)
         Printf.eprintf "Error: %s\n%!" (Printexc.to_string exn));
      (None : _ Lwt.t option))

(* ------------------------------------------------------------------ *)
(* Cmdliner entry point                                                  *)
(* ------------------------------------------------------------------ *)

let run () : unit =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  (* Note: local 'Cmd' alias refers to Cmdliner.Cmd here *)
  let repl_cmd : int Cmd.t =
    Cmd.v (Cmd.info "repl" ~doc:"Start the interactive REPL")
      Term.(const (fun () -> Lwt_main.run (top ())) $ const ())
  in
  let load_cmd : int Cmd.t =
    begin
      let file =
        Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")
      in
      Cmd.v
        (Cmd.info "check" ~doc:"Load a .cfg or source file")
        Term.(const (fun f -> (status_to_exit @@ make (File (Fpath.v f)))) $ file)
    end
  in
  let lsp_cmd : int Cmd.t =
    begin
      Cmd.v
        (Cmd.info "server" ~doc:"Start the LSP server (not yet implemented)")
        Term.(
          const (fun () ->
              mode := `Lsp;
              print_endline "LSP mode is not yet implemented.";
              BasisOS.Process.exit BasisOS.Process.failure)
          $ const ())
    end
  in
  let legacy_cmd : int Cmd.t =
    Cmd.v (Cmd.info "legacy")
      Term.(const (fun () ->
        let module Twelf_server = Server.Server_.Server in
        Twelf_server.server ("stelf", [])) $ const ())
  in
  let main_cmd =
    Cmd.group
      (Cmd.info "stelf" ~version ~doc:"The STELF proof assistant")
      [ repl_cmd; load_cmd; lsp_cmd; legacy_cmd ]
  in
  BasisOS.Process.exit (Cmd.eval' main_cmd)
