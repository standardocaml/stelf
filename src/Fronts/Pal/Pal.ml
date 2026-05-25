module type IMPL = IMPL.IMPL

module Pal = Impl
open Impl
open Basis
(* ------------------------------------------------------------------ *)
(* Interactive top-level (REPL via lambda-term / TUI)                   *)
(* ------------------------------------------------------------------ *)


let top (module M : Tui.REPL.S) : int Lwt.t =
  
  let module Repl = Tui.Repl.Repl (M) in 
  mode := `Repl;
  Repl.read (fun line ->
      (try
         let ns = ref (Names.newNamespace ()) in
         let loc = Cst.ghost in
         let cmds = ModernImpl.run (Cmd.parse ()) ns loc line in
         List.app Install.install1 cmds; Lwt.return (Repl.Continue);
       with exn ->
         (* TODO, add error handling only when error is not needing more input *)
         Printf.eprintf "Error: %s\n%!" (Printexc.to_string exn); Lwt.return (Repl.Fail "An error occurred") ) 
  )
  
(* ------------------------------------------------------------------ *)
(* Cmdliner entry point                                                  *)
(* ------------------------------------------------------------------ *)
 
let run () : unit =
  let module TempM = struct
    let use_color = true
    let use_unicode = true
  end in
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  (* Note: local 'Cmd' alias refers to Cmdliner.Cmd here *)
  let help_cmd : int Cmd.t = 
    let help_info = Cmd.info "help" ~doc:"Display help information"
    and help_term : int Term.t = 
      Term.(const 0) (* TODO *)
    in Cmd.v help_info help_term
in 
  let repl_cmd : int Cmd.t =
    let repl_fn : int Term.t =  

    let+ verbosity = Arg.value Opts.Opts.verbosity
    and+ color = Arg.value Opts.Opts.color
    and+ unicode = Arg.value Opts.Opts.unicode in  
    let module M = struct
      let use_color = color
      let use_unicode = unicode 
      let verbosity = verbosity
    end in
    Lwt_main.run (top (module M)) 
  and repl_info : Cmd.info = Cmd.info "repl" ~doc:"Start the interactive REPL" in 
    Cmd.v
      repl_info 
      repl_fn
  in
  let load_cmd : int Cmd.t =
    begin
      let file =
        Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")
      in
      Cmd.v
        (Cmd.info "check" ~doc:"Load a .cfg or source file")
        Term.(const (fun f -> status_to_exit @@ make (File (Fpath.v f))) $ file)
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
      Term.(
        const (fun () ->
            let module Twelf_server = Server.Server_.Server in
            Twelf_server.server ("stelf", []))
        $ const ())
  in
  let main_cmd =
    Cmd.group
      (Cmd.info "stelf" ~version ~doc:"The STELF proof assistant")
      [ repl_cmd; load_cmd; lsp_cmd; legacy_cmd ]
  in
  BasisOS.Process.exit (Cmd.eval' main_cmd)
