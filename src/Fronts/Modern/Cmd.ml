(* FIXME: Make_Cmd should be sealed with `: CMD.CMD` here, but the seal was
   removed for the same reason as Make_Modern — see Modern.ml.  Restore when
   Make_Modern carries proper `with module` constraints. *)
module Make_Cmd(Modern : MODERN.MODERN) = struct
  module Modern = Modern
  module Parser = Modern.Parser
  module Cst = Modern.Cst
  module Paths = Modern.Paths
  module Names = Modern.Names

  type 'a t = 'a Modern.t

  open Parser

  let parse1 () : Cst.cmd t =
    whitespace *>
    (keyword "." *> return (Cst.Cmd.stop ()))
    <|> (keyword "query" *>
         let+ q = Modern.parse_query () in
         Cst.Cmd.query q)
    <|> (keyword "define" *>
         let+ d = Modern.parse_define () in
         Cst.Cmd.define d)
    <|> (keyword "solve" *>
         let+ s = Modern.parse_solve () in
         Cst.Cmd.solve s)
    <|> (keyword "sort" *>
         let+ ds = many (inside "{" "}" (Modern.parse_decl ())) in
         Cst.Cmd.sort ds)
    <|> (keyword "term" *>
         let+ d = Modern.parse_decl () in
         Cst.Cmd.term d)
    <|> (keyword "quit" *> return (Cst.Cmd.Repl.quit ()))
    <|> (keyword "help" *>
         let+ t = option None (let+ id = Modern.parse_var () in Some id) in
         Cst.Cmd.Repl.help t)
    <|> (keyword "get" *>
         let+ id = Modern.parse_var () in
         Cst.Cmd.Repl.get id)
    <|> (keyword "set" *>
         let* id = Modern.parse_var () in
         let+ v = Modern.parse_var () in
         Cst.Cmd.Repl.set id v)

  (* Skip outer text (non-% characters) between commands *)
  let skip_outer : unit t = skip_while (fun c -> c <> '%')

  let parse () : Cst.cmd list t =
    skip_outer *> many (parse1 () <* skip_outer)
end
