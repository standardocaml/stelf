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
         List.app Install.install1 cmds
       with exn ->
         (* TODO, add error handling only when error is not needing more input *)
         Printf.eprintf "Error: %s\n%!" (Printexc.to_string exn));
      (None : _ Lwt.t option))

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
  let repl_cmd : int Cmd.t =
    Cmd.v
      (Cmd.info "repl" ~doc:"Start the interactive REPL")
      Term.(const (fun () -> Lwt_main.run (top (module TempM))) $ const ())
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
