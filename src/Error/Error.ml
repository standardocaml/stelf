module type ERROR = ERROR.ERROR

module Error : ERROR = struct
  type stage = Lex | Parse | Check | Total | Recon | Unknown | Other of string

  exception Err of stage * Display.Form.t

  let err ?(stage = Unknown) form = raise (Err (stage, form))
end
