
module Repl (M : REPL.S) : REPL.REPL = struct

  let term : LTerm.t ref Lwt.t = Lwt.map ref @@ Lazy.force LTerm.stdout

  let history : LTerm_history.t = LTerm_history.create []

  let ends_with_terminator s =
    let len = String.length s in
    let i = ref (len - 1) in
    while !i >= 0 && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r') do
      decr i
    done;
    !i >= 1 && s.[!i] = '.' && s.[!i - 1] = '%'

  let make_prompt str =
    React.S.const (LTerm_text.eval [
      LTerm_text.B_bold true;
      LTerm_text.B_fg LTerm_style.lgreen;
      LTerm_text.S str;
      LTerm_text.E_fg;
      LTerm_text.E_bold;
    ])

  let read_line t prompt_str =
    let open Lwt.Syntax in
    let rl = object (self)
      inherit LTerm_read_line.read_line ~history:(LTerm_history.contents history) ()
      inherit [Zed_string.t] LTerm_read_line.term t
      initializer self#set_prompt (make_prompt prompt_str)
    end in
    let* s = rl#run in
    Lwt.return (Zed_string.to_utf8 s)

  let stop code = exit code

  let read : type a. (string -> a Lwt.t option) -> a Lwt.t =
    fun callback ->
      let open Lwt.Syntax in
      let* t_ref = term in
      let t = !t_ref in
      let orange_arrow =
        LTerm_text.eval [
          LTerm_text.B_bold true;
          LTerm_text.B_fg (LTerm_style.rgb 255 165 0);
          LTerm_text.S "=> \n";
          LTerm_text.E_fg;
          LTerm_text.E_bold;
        ]
      in
      let rec loop () =
        let rec collect acc prompt_str =
          let* line = read_line t prompt_str in
          let acc' = acc @ [line] in
          if ends_with_terminator line
          then Lwt.return acc'
          else collect acc' "...> "
        in
        let* lines = collect [] "Π∀λ> " in
        let content = String.concat "\n" lines in
        LTerm_history.add history (Zed_string.of_utf8 content);
        let* () = LTerm.fprints t orange_arrow in
        match callback content with
        | Some promise -> promise
        | None -> loop ()
      in
      loop ()

  let show fmt = Format.pp_print_string fmt "Π∀λ> "

end
