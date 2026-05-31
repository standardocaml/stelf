module Make_Modern
    (Paths : Paths.Paths_intf.PATHS)
    (Cst : Cst.CST with module Paths = Paths)
    (Names : Names.Names_intf.NAMES)
    (Parser : Parsing.PARSER.PARSER) :
  MODERN.MODERN
    with module Names = Names
     and module Cst = Cst
     and module Paths = Paths = struct
  module Paths = Paths
  module Cst = Cst
  module Names = Names
  module Parser = Parser

  exception ParseError of string

  let fixities : Names.namespace ref = ref @@ Names.newNamespace ()
  let set_fixities : Names.namespace -> unit = fun ns -> fixities := ns

  open Parser

  type 'a t = 'a Parser.t

  let mk_loc : int -> int -> Cst.loc = fun x y -> Cst.mk_loc x y

  let ident1 =
    let* s = ident in
    if String.length s = 0 then fail "expected identifier" else return s

  let rec combine_fc (r1 : Paths.region) (r2 : Paths.region) : Paths.region =
    assert false

  and parse_arg () : string option t =
    token "_" *> return None
    <|> (let+ s = ident1 in
         Some s)
    <?> "argument"

  and parse_id () : Cst.Term.t t =
    (let+ sym = parse_qualified () in
     Cst.Term.qualified sym)
    <|> (let* name = ident1 in
         let sym = ([], name) in
         if Char.uppercase_ascii name.[0] = name.[0] then
           return (Cst.Term.uppercase sym)
         else return (Cst.Term.lowercase sym))
    <?> "identifier"

  and parse_expr_trail () : Cst.Term.t t =
    (let* d = inside "[" "]" (parse_decl ()) in
     let+ body = parse_expr () in
     Cst.Term.lam [ d ] body)
    <|> (let* d = inside "{" "}" (parse_decl ()) in
         let+ body = parse_expr () in
         Cst.Term.pi [ d ] body)
    <?> "trailing expression"

  and parse_expr_app () : Cst.Term.t t =
    (let* head = parse_expr1 () in
     let+ args = many (parse_expr1 ()) in
     Cst.Term.app head args)
    <?> "application"

  and parse_expr1 () : Cst.Term.t t =
    begin
      parse_id () <|> inside "(" ")" (parse_expr ())
    end
    <?> "small expression"

  and parse_expr () : Cst.Term.t t =
    begin
      fix (fun self ->
          let expr1 = parse_id () <|> inside "(" ")" self in
          (keyword "the" *> commit
          *>
          let* ty = expr1 in
          let+ body = self in
          Cst.Term.has_type body ty)
          <|>
          let* atoms = many expr1 in
          (let+ trail = parse_expr_trail () in
           match atoms with
           | [] -> trail
           | head :: rest -> Cst.Term.app head (rest @ [ trail ]))
          <|>
          match atoms with
          | [] -> fail "expected expression"
          | head :: rest -> return (Cst.Term.app head rest))
    end
    <?> "expression"

  and parse_var () : string t =
    begin
      ident1
    end
    <?> "variable"

  and parse_qualified () : Cst.symbol t =
    begin
      let rec split = function
        | [] -> failwith "Expected qualified name"
        | [ p ] -> ([], p)
        | p :: q ->
            let q0, q1 = split q in
            (List.append [ p ] q0, q1)
      in
      keyword "val"
      *> inside "(" ")"
           (let* ns = many1 ident in
            return @@ split ns)
    end
    <?> "qualified name"

  and parse_text () : string t =
    begin
      string "%\"" *> take_till (fun c -> c = '%') <* string "%\"" <* whitespace
    end
    <?> "string literal"

  and parse_decl () : Cst.Decl.t t =
    begin
      (let* names = inside "(" ")" (many1 (parse_arg ())) in
       let+ typ = option (Cst.Term.omitted ~fc:Cst.ghost) (parse_expr ()) in
       Cst.Decl.decl1 names typ)
      <|> let* name = parse_arg () in
          let+ typ = option (Cst.Term.omitted ~fc:Cst.ghost) (parse_expr ()) in
          Cst.Decl.decl1 [ name ] typ
    end
    <?> "declaration"

  and parse_mode () : Cst.Mode.mode t =
    begin
      keyword "out1" *> return (Cst.Mode.minus1 ())
      <|> keyword "out" *> return (Cst.Mode.minus ())
      <|> keyword "in" *> return (Cst.Mode.plus ())
      <|> keyword "star" *> return (Cst.Mode.star ())
    end
    <?> "mode"

  and parse_mode_dec () : Cst.Mode.modedec t =
    begin
      let* arg =
        many
        @@ inside "{" "}"
             (let* m = parse_mode () and* d = parse_decl () in
              return (m, d))
      and* body = parse_expr () in
      let rec go body = function
        | [] -> Cst.Mode.Full.mode_root body
        | (m, d) :: rest -> Cst.Mode.Full.mode_pi m d (go body rest)
      in
      return @@ Cst.Mode.Full.to_modeDec (go body arg)
    end
    <?> "mode declaration"

  and parse_inst () : Cst.Struct.inst t =
    begin
      let* name, s, e = with_fc ident1 in
      let loc = mk_loc s e in
      let sym = ([], name) in
      token "="
      *>
      let+ tm = parse_expr () in
      Cst.Struct.con_inst (sym, loc) tm
    end
    <?> "instance declaration"

  and parse_sigexp () : Cst.Struct.sigexp t =
    begin
      let* base =
        keyword "the" *> commit *> return (Cst.Struct.thesig ~fc:Cst.ghost)
        <|> let+ name = ident1 in
            Cst.Struct.sig_id name
      in
      let+ wheres = many (keyword "where" *> commit *> parse_inst ()) in
      match wheres with [] -> base | _ -> Cst.Struct.where_sig base wheres
    end

  and parse_sigdef () : Cst.Struct.sigdef t =
    begin
      let+ se = parse_sigexp () in
      Cst.Struct.sig_def None se
    end
    <?> "signature definition"

  and parse_struct_dec () : Cst.Struct.structdec t =
    begin
      let* name = ident1 in
      (token ":"
      *> let+ se = parse_sigexp () in
         Cst.Struct.struct_decl (Some name) se)
      <|> token "="
          *>
          let+ sym = parse_qualified () in
          Cst.Struct.struct_def (Some name) (Cst.Struct.str_exp sym)
    end
    <?> "structure declaration"

  and parse_fixity () : int t =
    begin
      let+ s = take_while1 (fun c -> c >= '0' && c <= '9') <* whitespace in
      int_of_string s
    end
    <?> "fixity level"

  and parse_query () :
      (int option * int option * int option * Cst.Query.query) t =
    begin
      let* n = parse_bound () in
      let* b = parse_bound () in
      let* d = parse_bound () in
      let+ tm = parse_expr () in
      (n, b, d, Cst.Query.query None tm)
    end
    <?> "query"

  and parse_define () : Cst.Query.define t =
    begin
      let* id = parse_var () in
      let* tm = parse_expr1 () in
      let+ ty = Parser.option None @@ ((fun s -> Some s) <$> parse_expr ()) in
      Cst.Query.define (Some id) tm ty
    end
    <?> "definition"

  and parse_solve () : Cst.Query.solve t =
    begin
      let+ term = parse_expr () in
      Cst.Query.solve None term
    end
    <?> "solve command"

  and parse_bound () : int option t =
    begin
      token "_" *> return None
      <|> let+ s = take_while1 (fun c -> c >= '0' && c <= '9') <* whitespace in
          Some (int_of_string s)
    end
    <?> "bound"

  and parse_id_list () : string list t =
    begin
      inside "(" ")" (many (parse_var ()))
      <|> let+ id = parse_var () in
          [ id ]
    end
    <?> "identifier list"

  and parse_block_item () : Cst.block_item t =
    begin
      inside "{" "}"
        (let+ d = parse_decl () in
         Cst.BlockItem.some d)
      <|> inside "[" "]"
            (let+ d = parse_decl () in
             Cst.BlockItem.pi d)
    end
    <?> "block item"

  and parse_fixity_kw () : Cst.fixity t =
    begin
      keyword "left" *> commit *> return Cst.Fixity.left
      <|> keyword "right" *> commit *> return Cst.Fixity.right
      <|> keyword "prefix" *> commit *> return Cst.Fixity.prefix
      <|> keyword "postfix" *> commit *> return Cst.Fixity.postfix
      <|> keyword "middle" *> commit *> return Cst.Fixity.middle
      <|> keyword "none" *> commit *> return Cst.Fixity.none
    end
    <?> "fixity keyword"

  and parse_params () : string list t =
    begin
      inside "(" ")" (many (parse_var ()))
    end
    <?> "parameters"

  and parse_group : 'a. 'a t -> 'a list t = fun p -> many p
  and parse_parens : 'a. 'a t -> 'a t = fun p -> inside "(" ")" p
  and parse_braced : 'a. 'a t -> 'a t = fun p -> inside "{" "}" p
  and parse_bracketed : 'a. 'a t -> 'a t = fun p -> inside "[" "]" p

  and debug_parser : 'a t -> string -> 'a =
   fun p x ->
    match Parser.parse_string ~consume:All (p : _ Parser.t) x with
    | Ok res -> res
    | Error msg -> raise (ParseError msg)

  and run : 'a. 'a t -> Names.namespace ref -> Cst.loc -> string -> 'a =
   fun p _ns _loc s ->
    match Parser.parse_string ~consume:All (whitespace *> p) s with
    | Ok res -> res
    | Error msg -> raise (ParseError msg)
end

module ModernCst = Cst.Make_Cst (Paths.Paths_)

module Modern : MODERN.MODERN =
  Make_Modern (Paths.Paths_) (ModernCst) (Names.Names_) (Parsing.Parser.Parser)

(* Re-export sub-modules so they are accessible outside the library *)
module Cmd = Cmd
module CMD = CMD
module MODERN = MODERN
module Debug_Cmd = Cmd.Make_Cmd (Modern)
