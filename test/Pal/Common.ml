let test_value (cmds : string list) : exn option =
  let p = new Pal.Pal.pal in
  try
    Printexc.record_backtrace true;
    List.iter(fun cmd -> p#exec cmd) cmds;
    None
  with e -> Some e

let test ?(skip=false) ?(failure=false) (name : string) (cmds : string list) : unit Alcotest.test_case = 
  let () = Printexc.record_backtrace true in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let () = Logs.set_level (Some Logs.Debug) in 
  let () = Display.register (fun m -> Lwt.return @@ prerr_endline @@ Fmt.to_to_string Display.Info.Form.fmt m.msg) in 
  Alcotest.test_case name `Slow (fun () -> if skip then Alcotest.skip () else match test_value cmds with
    | None when failure -> Alcotest.fail "Expected failure, but test passed"
    | Some e when not failure -> Alcotest.failf "Expected success, but test failed with exception: %s" (Printexc.to_string e)
    | None | Some _ -> ()

  )
