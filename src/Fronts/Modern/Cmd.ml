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

  (* Skip outer text (non-% characters) between commands *)
  let skip_outer : unit t = skip_while (fun c -> c <> '%')

  (* Defer a thunk-parser to prevent infinite recursion at construction time.
     Used for %module and %eval which recursively embed cmd lists. *)
  let defer p = return () >>= fun () -> p ()

  let rec parse_cmd_list () : Cst.cmd list t =
    keyword "{" *>
    skip_outer *>
    many (defer parse1 <* skip_outer)
    <* keyword "}"

  and parse1 () : Cst.cmd t =
    whitespace *>
    (keyword "." *> return (Cst.Cmd.stop ()))
    (* querytabled BEFORE query — "query" is a prefix of "querytabled" *)
    <|> (keyword "querytabled" *>
         let+ (n, b, d, q) = Modern.parse_query () in
         Cst.Cmd.query_tabled ~n ~b ~d q)
    <|> (keyword "query" *>
         let+ (n, b, d, q) = Modern.parse_query () in
         Cst.Cmd.query ~n ~b ~d q)
    <|> (keyword "?" *>
         let+ tm = Modern.parse_expr () in
         Cst.Cmd.adhoc_query (Cst.Query.query None tm))
    <|> (keyword "unique" *>
         let+ tm = Modern.parse_expr () in
         Cst.Cmd.unique tm)
    (* module BEFORE mode — "mode" is a prefix of "module" *)
    <|> (keyword "module" *>
         let* id     = Modern.parse_var () in
         let* params = Modern.parse_params () in
         let+ cmds   = parse_cmd_list () in
         Cst.Cmd.module_cmd id params cmds)
    <|> (keyword "mode" *>
         let* id = Modern.parse_var () in
         let+ md = Modern.parse_mode_dec () in
         Cst.Cmd.mode id md)
    <|> (keyword "define" *>
         let+ d = Modern.parse_define () in
         Cst.Cmd.define d)
    <|> (keyword "decl" *>
         let+ tm = Modern.parse_expr () in
         Cst.Cmd.decl_cmd tm)
    <|> (keyword "inline" *>
         let* id = Modern.parse_var () in
         let+ tm = Modern.parse_expr () in
         Cst.Cmd.inline id tm)
    <|> (keyword "symbol" *>
         let* id1 = Modern.parse_var () in
         let+ id2 = Modern.parse_var () in
         Cst.Cmd.symbol id1 id2)
    <|> (keyword "freeze" *>
         let+ ids = Modern.parse_id_list () in
         Cst.Cmd.freeze ids)
    <|> (keyword "thaw" *>
         let+ ids = Modern.parse_id_list () in
         Cst.Cmd.thaw ids)
    <|> (keyword "sort" *>
         let* id = Modern.parse_var () in
         let+ ds = many (inside "{" "}" (Modern.parse_decl ())) in
         Cst.Cmd.sort id ds)
    <|> (keyword "term" *>
         let+ d = Modern.parse_decl () in
         Cst.Cmd.term d)
    <|> (keyword "block" *>
         let* id    = Modern.parse_var () in
         let+ items = many (Modern.parse_block_item ()) in
         Cst.Cmd.block id items)
    <|> (keyword "union" *>
         let* id  = Modern.parse_var () in
         let+ ids = inside "(" ")" (many (Modern.parse_var ())) in
         Cst.Cmd.union id ids)
    <|> (keyword "worlds" *>
         let* ids = inside "(" ")" (many (Modern.parse_var ())) in
         let+ tm  = Modern.parse_expr () in
         Cst.Cmd.worlds ids tm)
    <|> (keyword "deterministic" *>
         let+ ids = Modern.parse_id_list () in
         Cst.Cmd.deterministic ids)
    <|> (keyword "use" *>
         let* id1     = Modern.parse_var () in
         let* id2     = Modern.parse_var () in
         let+ iparams = inside "(" ")" (many (Modern.parse_var ())) in
         Cst.Cmd.use id1 id2 iparams)
    <|> (keyword "open" *>
         let* id  = Modern.parse_var () in
         let+ ids = Modern.parse_id_list () in
         Cst.Cmd.open_cmd id ids)
    <|> (keyword "eval" *>
         let+ cmds = parse_cmd_list () in
         Cst.Cmd.eval cmds)
    <|> (keyword "prec" *>
         let* fix = Modern.parse_fixity_kw () in
         let* n   = Modern.parse_fixity () in
         let+ ids = Modern.parse_id_list () in
         Cst.Cmd.prec fix n ids)
    <|> (keyword "solve" *>
         let+ s = Modern.parse_solve () in
         Cst.Cmd.solve s)
    <|> (keyword "quit" *> return (Cst.Cmd.Repl.quit ()))
    <|> (keyword "help" *>
         let+ t = option None (let+ id = Modern.parse_var () in Some id) in
         Cst.Cmd.Repl.help t)
    <|> (keyword "get" *>
         let+ id = Modern.parse_var () in
         Cst.Cmd.Repl.get id)
    <|> (keyword "set" *>
         let* id = Modern.parse_var () in
         let+ v  = Modern.parse_var () in
         Cst.Cmd.Repl.set id v)
    <|> (keyword "version" *> return (Cst.Cmd.Repl.version ()))

  let parse () : Cst.cmd list t =
    skip_outer *> many (parse1 () <* skip_outer)
end
