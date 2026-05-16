module Make_Modern
    (Paths : Paths.Paths_intf.PATHS)
    (Cst : Cst.CST with module Paths = Paths)
    (Names : Names.Names_intf.NAMES)
    (Parser : Parser.PARSER) : MODERN.MODERN = struct
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

  let combine_fc (r1 : Paths.region) (r2 : Paths.region) : Paths.region =
    assert false

  let rec parse_expr1 () : Cst.Term.t t =
    begin
      let parse_pi () : Cst.Term.t t =
        let@ r, p0, _ = Parser.inside "{" "}" (parse_decl ()) in
        let@ body, _, p3 = parse_expr () in
        let fc = mk_loc p0 p3 in
        return @@ Cst.Term.pi ~fc [ r ] body
      and parse_lambda () : Cst.Term.t t =
        let@ r, p0, _ = Parser.inside "[" "]" (parse_decl ()) in
        let@ body, _, p3 = parse_expr () in
        let fc = mk_loc p0 p3 in
        return @@ Cst.Term.lam ~fc [ r ] body 
      
      and parse_app () : Cst.Term.t t = Parser.inside "(" ")" (parse_expr_app ()) 

      and parse_ident () : Cst.Term.t t =
        let@ var, p0, p1 = ident in
        let fc = mk_loc p0 p1 in
        assert (var != "");
        if var.[0] == '_' then return @@ Cst.Term.uppercase ~fc ([], var)
        else if var.[0] == '?' then return @@ Cst.Term.exist_var ~fc var
        else return @@ Cst.Term.lowercase ~fc ([], var)

      and parse_the () : Cst.Term.t t =
        let@ _ = keyword "the" in
        let@ ty, p0, _ = parse_expr () in
        let@ tm, _, p1 = parse_expr () in
        let fc = mk_loc p0 p1 in
        return @@ Cst.Term.has_type ~fc tm ty 
    in
    parse_pi () <|> parse_lambda () <|> parse_app () <|> parse_the () <|> parse_ident ()
    end

  and parse_expr_app () : Cst.Term.t t =
    let@ exprs, p0, p1 =
      many1 @@ parse_expr1 ()
    in
    assert (exprs != []);
    let fc = mk_loc p0 p1 in
    return
    @@
    if List.length exprs = 1 then List.hd exprs
    else Cst.Term.app ~fc (List.hd exprs) (List.tl exprs)
  and parse_expr () : Cst.Term.t t = parse_expr_app ()
  and parse_var () : string t = ident

  and parse_qualified () : Cst.symbol t =
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

  and parse_text () : string t = assert false
  and parse_decl () : Cst.Decl.t t = assert false
  and parse_mode () : Cst.Mode.mode t = assert false
  and parse_mode_dec () : Cst.Mode.modedec t = assert false
  and parse_sigexp () : Cst.Struct.sigexp t = assert false
  and parse_inst () : Cst.Struct.inst t = assert false
  and parse_sigdef () : Cst.Struct.sigdef t = assert false
  and parse_struct_dec () : Cst.Struct.structdec t = assert false
  and parse_fixity () : int t = assert false
  and parse_query () : Cst.query t = assert false
  and parse_define () : Cst.define t = assert false
  and parse_solve () : Cst.solve t = assert false
  and parse_group : 'a. 'a t -> 'a list t = assert false
  and parse_parens : 'a. 'a t -> 'a t = assert false
  and parse_braced : 'a. 'a t -> 'a t = assert false
  and parse_bracketed : 'a. 'a t -> 'a t = assert false

  and debug_parser : 'a t -> string -> 'a = fun p x -> match Parser.parse_string ~consume:All (p : _ Parser.t) x with 
    | Ok res -> res
    | Error msg -> raise (ParseError msg)

  and run : 'a. 'a t -> Names.namespace ref -> Cst.loc -> string -> 'a =
    assert false
end

