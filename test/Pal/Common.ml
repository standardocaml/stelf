let test_value (cmds : string list) : exn option =
  let p = new Pal.Pal.pal in
  try
    Printexc.record_backtrace true;
    List.iter (fun cmd -> p#exec cmd) cmds;
    None
  with e -> Some e

class tester = object 
  val mutable p = new Pal.Pal.pal
  method run cmd =
    try      
      Printexc.record_backtrace true;
      p#exec cmd;
      None
    with e -> Some e
  method reset () = p <- new Pal.Pal.pal
end
let test ?(skip = false) ?(failure = false) (name : string) (cmds : string list)
    : string * unit Alcotest.test_case list =
  let () = Printexc.record_backtrace true in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let () = Logs.set_level ~all:false (Some Logs.Debug) in
  let () = Fmt_tty.setup_std_outputs () in
  let () =
    Display.register (fun m ->
        let _ = (Display.fmt Fmt.stdout m.msg ) in 
        Lwt.return ()) 
  in
  let has_failed = ref false in
  let tester' = new tester in
  name, List.mapi(fun i cmd -> Alcotest.test_case (name ^ " - " ^ string_of_int (i + 1)) `Slow (fun () ->
      if skip || !has_failed then Alcotest.skip ()
      else
        match tester'#run cmd with
        | None when failure -> Alcotest.fail "Expected failure, but test passed"
        | Some e when not failure ->
            let bt = Printexc.get_backtrace () in
            has_failed := true;
            Printf.eprintf "Exception: %s\nBacktrace:\n%s\n%!" (Printexc.to_string e) bt;
            Alcotest.failf
              "Expected success, but test failed with exception: %s\nBacktrace:\n%s"
              (Printexc.to_string e) bt
        | None | Some _ -> ())) cmds
