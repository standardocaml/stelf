module Repl : REPL.REPL = struct

  let term : LTerm.t ref Lwt.t = Lwt.map ref @@ Lazy.force LTerm.stdout

  let history : LTerm_history.t = LTerm_history.create []

  class repl_line term_t history_list = object (self)
    inherit LTerm_read_line.read_line ~history:history_list ()
    inherit [Zed_string.t] LTerm_read_line.term term_t

    initializer
      self#set_prompt (React.S.const (LTerm_text.of_utf8 "stelf> "))
  end

  let stop code = exit code

  let read : type a. (string -> a Lwt.t option) -> a Lwt.t =
    fun callback ->
      let open Lwt.Syntax in
      let* t_ref = term in
      let t = !t_ref in
      let rec loop () =
        let rl = new repl_line t (LTerm_history.contents history) in
        let* line = rl#run in
        LTerm_history.add history line;
        match callback (Zed_string.to_utf8 line) with
        | Some promise -> promise
        | None -> loop ()
      in
      loop ()

  let show fmt = Format.pp_print_string fmt "stelf> "

end
