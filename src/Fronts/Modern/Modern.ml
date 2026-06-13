module Make_Modern
    (Paths : Paths.PATHS.PATHS)
    (Cst : Cst.CST with module Paths = Paths)
    (Names : Names.NAMES.NAMES)
    (Parser : Parsing.PARSER.PARSER) :
  MODERN.MODERN
    with module Names = Names
     and module Cst = Cst
     and module Paths = Paths = struct
  module Paths = Paths
  module Cst = Cst
  module Names = Names
  module N = Names
  module Parser = Parser

  exception ParseError of string

  open Parser

  let currently_uppercase : string list ref = ref []

  let with_uppercase : string list -> (unit -> 'a t) -> 'a t =
   fun names p ->
    let old = !currently_uppercase in
    currently_uppercase := old @ names;
    let* res = p () in
    let+ () = return @@ (currently_uppercase := old) in
    res

  type 'a t = 'a Parser.t

  let rec break (s : string list) : string list * string =
    match s with
    | [] -> ([], "")
    | x :: xs ->
        let y, z = break xs in
        (x :: y, z)

  let mk_loc : int -> int -> Cst.loc = fun x y -> Cst.mk_loc x y

  let whitespace' () : unit t =
    whitespace <|> forget @@ (token "% " *> take_till (fun c -> c = '\n'))

  let ghost' = Cst.View.Loc.(review Ghost)

  let rec combine_fc (r1 : Paths.region) (r2 : Paths.region) : Paths.region =
    assert false

  module FX = Names.Fixity

  let local_fixity : (string, FX.fixity) Hashtbl.t = Hashtbl.create 16

  let register_local_fixity (fix : Cst.fixity) (n : int) (ids : string list) :
      unit =
    let prec = FX.Strength n in
    let open Cst.View.Fixity in
    let fx =
      match !>fix with
      | Left _ -> FX.Infix (prec, FX.Left)
      | Right _ -> FX.Infix (prec, FX.Right)
      | Prefix _ -> FX.Prefix prec
      | Postfix _ -> FX.Postfix prec
      | Middle _ -> FX.Infix (prec, FX.None)
      | None _ -> FX.Infix (prec, FX.None)
    in
    List.iter (fun id -> Hashtbl.replace local_fixity id fx) ids

  type operator =
    | Atom of Cst.Term.t
    | Infix_ of
        (FX.precedence * FX.associativity)
        * (Cst.Term.t * Cst.Term.t -> Cst.Term.t)
    | Prefix_ of FX.precedence * (Cst.Term.t -> Cst.Term.t)
    | Postfix_ of FX.precedence * (Cst.Term.t -> Cst.Term.t)

  let jux_op =
    Infix_ ((FX.inc FX.maxPrec, FX.Left), fun (f, x) -> Cst.Term.app f [ x ])

  let infix_op (infixity, tm) =
    Infix_
      ( infixity,
        fun (tm1, tm2) -> Cst.Term.app (Cst.Term.app tm [ tm1 ]) [ tm2 ] )

  let prefix_op (prec, tm) = Prefix_ (prec, fun tm1 -> Cst.Term.app tm [ tm1 ])
  let postfix_op (prec, tm) = Postfix_ (prec, fun tm1 -> Cst.Term.app tm [ tm1 ])

  let classify (tm : Cst.Term.t) : operator =
    let open Cst.View.Term in
    match !>tm with
    | Lowercase (_, (ns, name))
    | Uppercase (_, (ns, name))
    | Qualified (_, (ns, name)) ->
        let fixity =
          match Hashtbl.find_opt local_fixity name with
          | Some fx -> fx
          | None -> Names.fixityLookup (Names.Qid (ns, name))
        in
        begin match fixity with
        | FX.Nonfix -> Atom tm
        | FX.Infix (prec, assoc) -> infix_op ((prec, assoc), tm)
        | FX.Prefix prec -> prefix_op (prec, tm)
        | FX.Postfix prec -> postfix_op (prec, tm)
        end
    | _ -> Atom tm

  module P = struct
    let rec reduce = function
      | Atom tm2 :: Infix_ (_, con) :: Atom tm1 :: p' ->
          Atom (con (tm1, tm2)) :: p'
      | Atom tm :: Prefix_ (_, con) :: p' -> Atom (con tm) :: p'
      | Postfix_ (_, con) :: Atom tm :: p' -> Atom (con tm) :: p'
      | p ->
          failwith
            (Printf.sprintf "process_app: cannot reduce stack of length %d"
               (List.length p))

    let rec reduce_rec = function [ Atom e ] -> e | p -> reduce_rec (reduce p)

    let reduce_all = function
      | [ Atom e ] -> e
      | Infix_ _ :: _ -> raise (ParseError "Incomplete infix expression")
      | Prefix_ _ :: _ -> raise (ParseError "Incomplete prefix expression")
      | [] -> raise (ParseError "Empty expression")
      | p -> reduce_rec (reduce p)

    let shift_atom (tm, p) =
      match p with
      | Atom _ :: _ -> reduce (Atom tm :: jux_op :: p)
      | _ -> Atom tm :: p

    let rec shift (opr, p) =
      match (opr, p) with
      | (Atom _ as o), (Atom _ :: _ as p') -> reduce (o :: jux_op :: p')
      | Infix_ _, Infix_ _ :: _ ->
          raise (ParseError "Consecutive infix operators")
      | Infix_ _, Prefix_ _ :: _ ->
          raise (ParseError "Infix operator following prefix operator")
      | Infix_ _, [] -> raise (ParseError "Leading infix operator")
      | (Prefix_ _ as o), (Atom _ :: _ as p') -> o :: jux_op :: p'
      | Postfix_ _, Infix_ _ :: _ ->
          raise (ParseError "Postfix operator following infix operator")
      | Postfix_ _, Prefix_ _ :: _ ->
          raise (ParseError "Postfix operator following prefix operator")
      | Postfix_ _, [] -> raise (ParseError "Leading postfix operator")
      | o, p' -> o :: p'

    let rec resolve (opr, p) =
      match (opr, p) with
      | ( (Infix_ ((prec, assoc), _) as o),
          (Atom _ :: Infix_ ((prec', assoc'), _) :: _ as p') ) ->
          begin match (FX.compare (prec, prec'), assoc, assoc') with
          | Greater, _, _ -> shift (o, p')
          | Less, _, _ -> resolve (o, reduce p')
          | Equal, FX.Left, FX.Left -> resolve (o, reduce p')
          | Equal, FX.Right, FX.Right -> shift (o, p')
          | _ ->
              raise
                (ParseError
                   "Ambiguous: infix following infix of identical precedence")
          end
      | (Infix_ ((prec, _), _) as o), (Atom _ :: Prefix_ (prec', _) :: _ as p')
        ->
          begin match FX.compare (prec, prec') with
          | Greater -> shift (o, p')
          | Less -> resolve (o, reduce p')
          | Equal ->
              raise
                (ParseError
                   "Ambiguous: infix following prefix of identical precedence")
          end
      | (Prefix_ _ as o), p' -> shift (o, p')
      | (Postfix_ (prec, _) as o), (Atom _ :: Prefix_ (prec', _) :: _ as p') ->
          begin match FX.compare (prec, prec') with
          | Greater -> reduce (shift (o, p'))
          | Less -> resolve (o, reduce p')
          | Equal ->
              raise
                (ParseError
                   "Ambiguous: postfix following prefix of identical precedence")
          end
      | (Postfix_ (prec, _) as o), (Atom _ :: Infix_ ((prec', _), _) :: _ as p')
        ->
          begin match FX.compare (prec, prec') with
          | Greater -> reduce (shift (o, p'))
          | Less -> resolve (o, reduce p')
          | Equal ->
              raise
                (ParseError
                   "Ambiguous: postfix following infix of identical precedence")
          end
      | (Postfix_ _ as o), ([ Atom _ ] as p') -> reduce (shift (o, p'))
      | o, p' -> shift (o, p')
  end

  let process_app (ts : Cst.Term.t list) : Cst.Term.t =
    let rec go p = function
      | [] -> P.reduce_all p
      | t :: rest ->
          let p' =
            match classify t with
            | Atom tm -> P.shift_atom (tm, p)
            | opr -> P.resolve (opr, p)
          in
          go p' rest
    in
    match ts with
    | [] -> failwith "process_app: called with empty list"
    | _ -> go [] ts

  let rec parse_arg () : string option t =
    token "_" *> return None
    <|> (let+ s = ident1 in
         Some s)
    <?> "argument"

  and parse_id () : Cst.Term.t t =
    keyword "val" *> commit
    *> (inside "(" ")"
          (let@ ns, s, e = many1 ident1 in
           let loc = mk_loc s e in
           let rec split = function
             | [] -> failwith "Expected qualified name"
             | [ p ] -> ([], p)
             | p :: q ->
                 let q0, q1 = split q in
                 (p :: q0, q1)
           in
           return (Cst.Term.qualified ~fc:loc (split ns)))
       <|> let@ name, s, e = ident1 in
           let loc = mk_loc s e in
           return (Cst.Term.qualified ~fc:loc ([], name)))
    <|>
    let@ name, s, e = ident1 in
    let loc = mk_loc s e in
    let is_upper =
      String.length name > 0
      && (name.[0] = '_' || (name.[0] >= 'A' && name.[0] <= 'Z'))
    in
    return
      (if is_upper || List.mem name !currently_uppercase then
         Cst.Term.uppercase ~fc:loc ([], name)
       else Cst.Term.lowercase ~fc:loc ([], name))

  and parse_expr_trail () : Cst.Term.t t =
    (let@ d, s, e = inside "[" "]" (parse_decl ()) in
     let loc = mk_loc s e in
     let+ body = parse_expr () in
     Cst.Term.lam ~fc:loc [ d ] body)
    <|> (let@ d, s, e = inside "{" "}" (parse_decl ()) in
         let loc = mk_loc s e in
         let+ body = parse_expr () in
         Cst.Term.pi ~fc:loc [ d ] body)
    <|> ((let* ids = inside "{{" "}}" (many @@ parse_var ()) in
          let+ body = with_uppercase ids parse_expr in
          body)
        <?> "expression with implicit variables")
    <|> parse_id () <?> "trailing expression"

  and parse_expr_app () : Cst.Term.t t =
    (let* head = parse_expr1 () in
     let+ args = many (parse_expr1 ()) in
     process_app (head :: args))
    <?> "application"

  and parse_expr1 () : Cst.Term.t t =
    begin
      choice
        [ parse_id (); inside "(" ")" (return () >>= fun () -> parse_expr ()) ]
    end
    <?> "small expression"

  and parse_expr () : Cst.Term.t t =
    begin
      (keyword "the" *> commit
      *>
      let* ty = parse_expr1 () in
      let+ body = parse_expr () in
      Cst.Term.has_type body ty)
      <|>
      (* %-> A %-> B  %-> C  ==>  {_ A} {_ B} C  (last arg is the body) *)
      (keywords [ "->"; "if" ]
      *> commit
      *>
      let+ args =
        sep_by1 (option () @@ (keyword "->" *> commit)) (parse_expr1 ())
      in
      let rev = List.rev args in
      let body = List.hd rev in
      let init = List.rev (List.tl rev) in
      List.fold_right
        (fun t acc -> Cst.Term.pi [ Cst.Decl.decl1 [ None ] t ] acc)
        init body)
      <|>
      (* 
            %<- A
            %<- B 
            %<- C  ==>  {_ C} {_ B} A  (first arg is the body) *)
      (keywords [ "<-"; "do" ]
      *> commit
      *>
      let+ args =
        sep_by1 (option () @@ (keyword "<-" *> commit)) (parse_expr1 ())
      in
      let body = List.hd args in
      let rest_rev = List.rev (List.tl args) in
      List.fold_right
        (fun t acc -> Cst.Term.pi [ Cst.Decl.decl1 [ None ] t ] acc)
        rest_rev body)
      <|>
      let* atoms = many @@ parse_expr1 () in
      let* trail_opt =
        option None
          (let+ t = parse_expr_trail () in
           Some t)
      in
      match (atoms, trail_opt) with
      | [], None -> fail "expected expression"
      | [], Some trail -> return trail
      | head :: rest, None -> return (process_app (head :: rest))
      | head :: rest, Some trail ->
          return (process_app (head :: (rest @ [ trail ])))
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
      keyword "val" *> commit
      *> ((let* ident in
           return ([], ident))
         <|> inside "(" ")"
               (let* ns = many1 ident in
                return @@ split ns))
    end
    <?> "qualified name"

  and parse_text () : string t =
    begin
      string "%\"" *> take_till (fun c -> c = '%')
      <* string "%\"" <* whitespace' ()
    end
    <?> "string literal"

  and parse_decl () : Cst.Decl.t t =
    begin
      (let@ names, s, e = inside "(" ")" (many1 (parse_arg ())) in
       let loc = mk_loc s e in
       let+ typ = option (Cst.Term.omitted ~fc:Cst.ghost) (parse_expr ()) in
       Cst.Decl.decl1 ~fc:loc names typ)
      <|> let@ name, s, e = parse_arg () in
          let loc = mk_loc s e in
          let+ typ = option (Cst.Term.omitted ~fc:Cst.ghost) (parse_expr ()) in
          Cst.Decl.decl1 ~fc:loc [ name ] typ
    end
    <?> "declaration"

  and parse_mode () : Cst.Mode.mode t =
    begin
      (let@ (), s, e = keyword "out1" *> commit *> return () in
       return (Cst.Mode.minus1 ~fc:(mk_loc s e) ()))
      <|> (let@ (), s, e = keyword "out" *> commit *> return () in
           return (Cst.Mode.minus ~fc:(mk_loc s e) ()))
      <|> (let@ (), s, e = keyword "in" *> commit *> return () in
           return (Cst.Mode.plus ~fc:(mk_loc s e) ()))
      <|> let@ (), s, e = keyword "star" *> commit *> return () in
          return (Cst.Mode.star ~fc:(mk_loc s e) ())
    end
    <?> "mode"

  and parse_mode_dec () : Cst.Mode.modedec t =
    begin
      let* braced_args =
        many
        @@ inside "{" "}"
             (let* m = parse_mode () and* d = parse_decl () in
              return (m, d))
      in
      let* body = parse_expr () in
      let+ bare_modes = many (parse_mode ()) in
      let rec go_bare body = function
        | [] -> Cst.Mode.Full.mode_root body
        | m :: rest ->
            Cst.Mode.Full.mode_pi m (Cst.Decl.decl0 [ None ])
              (go_bare body rest)
      in
      let rec go_braced inner = function
        | [] -> inner
        | (m, d) :: rest -> Cst.Mode.Full.mode_pi m d (go_braced inner rest)
      in
      Cst.Mode.Full.to_modeDec (go_braced (go_bare body bare_modes) braced_args)
    end
    <?> "mode declaration"

  and parse_simple_mode_dec () : Cst.Mode.modedec t =
    parse_mode_dec () <?> "simple mode declaration"

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
      let+ s = take_while1 (fun c -> c >= '0' && c <= '9') <* whitespace' () in
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

  and parse_define () : Cst.define t =
    begin
      let* id =
        (fun s -> Some s) <$> parse_var () <|> Parser.string "_" *> return None
      in

      let* ty =
        let+ t = parse_expr1 () in
        match Cst.View.Term.view t with
        | Cst.View.Term.Uppercase (_, ([], "_")) -> None
        | _ -> Some t
      in
      let+ tm = parse_expr () in
      Cst.View.(Define.(review @@ Define (Loc.(review Ghost), id, tm, ty)))
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
      <|> let+ s =
            take_while1 (fun c -> c >= '0' && c <= '9') <* whitespace' ()
          in
          Some (int_of_string s)
    end
    <?> "bound"

  and parse_id_list () : string list t =
    begin
      inside "(" ")" (many1 (parse_var ()))
      <|> let+ id = parse_var () in
          [ id ]
    end
    <?> "identifier list"

  and parse_reduces_rel () : string t =
    begin
      token "<=" *> commit *> return "<="
      <|> token ">=" *> commit *> return ">="
      <|> token "<" *> commit *> return "<"
      <|> token ">" *> commit *> return ">"
      <|> token "=" *> commit *> return "="
    end
    <?> "reduces predicate"

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
    Display.(
      debug @@ ((style Style.bold @@ string "Parsing") ++ nl () ++ string x));
    match Parser.parse_string ~consume:All (p : _ Parser.t) x with
    | Ok res -> res
    | Error msg -> raise (ParseError msg)

  and debug_parser_with_ops : (string * FX.fixity) list -> 'a t -> string -> 'a
      =
   fun f p x ->
    List.iter (fun (id, fix) -> Hashtbl.replace local_fixity id fix) f;
    debug_parser p x

  and run : 'a. 'a t -> N.namespace ref -> Cst.loc -> string -> 'a =
   fun p _ns _loc s ->
    (* TODO use namespace and loc *)
    match Parser.parse_string ~consume:All (whitespace' () *> p) s with
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
