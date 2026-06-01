module type ERROR = sig
  type stage = Lex | Parse | Check | Total | Recon | Unknown | Other of string

  exception Err of stage * Display.Form.t

  val err : ?stage:stage -> Display.Form.t -> 'a
end
