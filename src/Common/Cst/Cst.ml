open Basis
module type CST = CST.CST

module Make_Cst (Paths : Paths.Paths_intf.PATHS) = struct
  module Paths = Paths

  (* Location type for source tracking - use simple int-based representation *)
  type loc = { start_pos : int; end_pos : int }
    [@@deriving show {with_path = false}, eq]

  (* Basic type aliases *)
  type name = string
  type namespace = string list
  type symbol = namespace * name

  (* Location helpers *)
  let mk_loc (start_ : int) (end_ : int) : loc = { start_pos = start_; end_pos = end_ }
  let loc_to_region (loc : loc) : Paths.region = Paths.Reg (loc.start_pos, loc.end_pos)
  let ghost : loc = { start_pos = 0; end_pos = 0 }

  (* Mutual recursion: decl and term *)
  type decl = Dec_ of string option list * term * loc

  and term =
    | Arrow_ of term * term
    | Pi_ of decl * term
    | Lam_ of decl * term
    | App_ of term * term
    | Hastype_ of term * term
    | Omitted_ of loc
    | Lcid_ of string list * string * loc
    | Ucid_ of string list * string * loc
    | Quid_ of string list * string * loc
    | Scon_ of string * loc
    | Evar_ of string * loc
    | Fvar_ of string * loc
    | Typ_ of loc
    [@@deriving show {with_path = false}, eq]

  (* Constant/block declarations *)
  type conDec =
    | ConstantDecl_ of decl
    | BlockDecl_ of string * decl list * decl list
    | BlockDef_ of string * symbol list
    | ConstantDef_ of string * term * term option

  (* Query/Define/Solve payload types *)
  type query = Query_ of string option * term
  type define = Define_ of string option * term * term option
  type solve = Solve_ of string option * term

  (* Mode syntax *)
  type mode = Plus_ | Star_ | Minus_ | Minus1_
  type modeTerm =
    | ModeTermRoot_ of term
    | ModeTermPi_ of mode * decl * modeTerm
  type modeSpine = ModeSpineInternal_ of (mode * string option) list
  type modeDec = ModeDec_ of modeTerm

  (* Structure expressions and signature expressions *) 
  type strexp = StrExp_ of symbol
  type inst =
    | ConInst_ of symbol * loc * term
    | StrInst_ of symbol * loc * strexp

  type sigexp =
    | TheSig_
    | SigId_ of string
    | WhereSig_ of sigexp * inst list

  type sigdef = SigDef_ of string option * sigexp
  type structDec =
    | StructDecl_ of string option * sigexp
    | StructDef_ of string option * strexp

  type structDef = StructDef_ of string option * strexp

  type fixity = Left_ | Right_ | Prefix_ | Postfix_ | Middle_ | FNone_

  type block_item =
    | BlockSome_ of decl
    | BlockPi_   of decl

  (* Top-level commands *)
  type cmd =
    | QueryCmd_         of int option * int option * int option * query
    | QueryTabledCmd_   of int option * int option * int option * query
    | AdhocQueryCmd_    of query
    | UniqueCmd_        of term
    | ModeCmd_          of string * modeDec
    | DefineCmd_        of define
    | DeclCmd_          of term
    | InlineCmd_        of string * term
    | SymbolCmd_        of string * string
    | FreezeCmd_        of string list
    | ThawCmd_          of string list
    | SortCmd_          of string * decl list
    | TermCmd_          of decl
    | BlockCmd_         of string * block_item list
    | UnionCmd_         of string * string list
    | WorldsCmd_        of string list * term
    | DeterministicCmd_ of string list
    | ModuleCmd_        of string * string list * cmd list
    | UseCmd_           of string * string * string list
    | OpenCmd_          of string * string list
    | EvalCmd_          of cmd list
    | PrecCmd_          of fixity * int * string list
    | SolveCmd_         of solve
    | StopCmd_
    | QuitCmd_
    | HelpCmd_          of string option
    | GetCmd_           of string
    | SetCmd_           of string * string
    | VersionCmd_

  (* Term constructor module *)
  module Term = struct
    type nonrec t = term

    let lowercase ?fc:(loc_ = ghost) (namespace, name) =
      Lcid_ (namespace, name, loc_)

    let uppercase ?fc:(loc_ = ghost) (namespace, name) =
      Ucid_ (namespace, name, loc_)

    let qualified ?fc:(loc_ = ghost) (namespace, name) =
      Quid_ (namespace, name, loc_)

    let text ?fc:(loc_ = ghost) str =
      Scon_ (str, loc_)

    let exist_var ?fc:(loc_ = ghost) name =
      Evar_ (name, loc_)

    let free_var ?fc:(loc_ = ghost) name =
      Fvar_ (name, loc_)

    module Sugar = struct
      let arrow ?fc:(loc_ = ghost) tm1 tm2 =
        Arrow_ (tm1, tm2)

      let backarrow ?fc:(loc_ = ghost) tm1 tm2 =
        Arrow_ (tm2, tm1)
    end

    let pi ?fc:(loc_ = ghost) decls body =
      let rec fold_right f lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> f x (fold_right f xs acc)
      in
      fold_right (fun d acc -> Pi_ (d, acc)) decls body

    let lam ?fc:(loc_ = ghost) decls body =
      let rec fold_right f lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> f x (fold_right f xs acc)
      in
      fold_right (fun d acc -> Lam_ (d, acc)) decls body

    let app ?fc:(loc_ = ghost) head args =
      match args with
      | [] -> head
      | _ ->
          let rec fold_left f acc lst =
            match lst with
            | [] -> acc
            | x :: xs -> fold_left f (f acc x) xs
          in
          fold_left (fun acc arg -> App_ (acc, arg)) head args

    let has_type ?fc:(loc_ = ghost) tm ty =
      Hastype_ (tm, ty)

    let[@warning "-16"] omitted ?fc:(loc_ = ghost) =
      Omitted_ loc_

    let typ ?fc:(loc_ = ghost) () =
      Typ_ loc_
  end

  (* Declaration constructor module *)
  module Decl = struct
    type nonrec t = decl

    let decl1 ?fc:(loc_ = ghost) names typ =

      Dec_ (names, typ, loc_)

    let decl0 ?fc:(loc_ = ghost) names =
      Dec_ (names, Omitted_ ghost, loc_)
  end

  (* Constant/block declaration constructor module *)
  module ConDec = struct
    type nonrec t = conDec

    let constant_decl ?fc:(loc_ = ghost) decl =
      ConstantDecl_ decl

    let block_decl ?fc:(loc_ = ghost) name decls1 decls2 =
      BlockDecl_ (name, decls1, decls2)

    let block_def ?fc:(loc_ = ghost) name symbols =
      BlockDef_ (name, symbols)

    let constant_def ?fc:(loc_ = ghost) name term1 term2_opt =
      ConstantDef_ (name, term1, term2_opt)
  end

  (* Mode constructor module *)
  module Mode = struct
    type nonrec mode = mode
    type nonrec term = modeTerm
    type nonrec modedec = modeDec

    let plus ?fc:(loc_ = ghost) () = Plus_
    let star ?fc:(loc_ = ghost) () = Star_
    let minus ?fc:(loc_ = ghost) () = Minus_
    let minus1 ?fc:(loc_ = ghost) () = Minus1_

    module Short = struct
      type nonrec term = modeTerm
      type nonrec spine = modeSpine

      let mode_nil ?fc:(loc_ = ghost) () =
        ModeSpineInternal_ []

      let mode_app ?fc:(loc_ = ghost) (m, name_opt) (ModeSpineInternal_ spine) =
        ModeSpineInternal_ ((m, name_opt) :: spine)

      let mode_root ?fc:(loc_ = ghost) (ns, name) (ModeSpineInternal_ _spine) =
        ModeTermRoot_ (Quid_ (ns, name, loc_))

      let to_modeDec ?fc:(loc_ = ghost) mt = ModeDec_ mt
    end

    module Full = struct
      let mode_root ?fc:(loc_ = ghost) tm = ModeTermRoot_ tm

      let mode_pi ?fc:(loc_ = ghost) m d body = ModeTermPi_ (m, d, body)

      let to_modeDec ?fc:(loc_ = ghost) mt = ModeDec_ mt
    end
  end

  (* Module/signature constructor module *)
  module Struct = struct
    type nonrec strexp = strexp
    type nonrec inst = inst
    type nonrec sigexp = sigexp
    type nonrec sigdef = sigdef
    type nonrec structdec = structDec

    let str_exp ?fc:(loc_ = ghost) symbol =
      StrExp_ symbol

    let con_inst ?fc:(loc_ = ghost) (symbol, loc2) term =
      ConInst_ (symbol, loc2, term)

    let str_inst ?fc:(loc_ = ghost) (symbol, loc2) strexp =
      StrInst_ (symbol, loc2, strexp)

    let[@warning "-16"] thesig ?fc:(loc_ = ghost) =
      TheSig_

    let sig_id ?fc:(loc_ = ghost) name =
      SigId_ name

    let where_sig ?fc:(loc_ = ghost) sigexp insts =
      WhereSig_ (sigexp, insts)

    let sig_def ?fc:(loc_ = ghost) name_opt sigexp =
      SigDef_ (name_opt, sigexp)

    let struct_decl ?fc:(loc_ = ghost) name_opt sigexp =
      StructDecl_ (name_opt, sigexp)

    let struct_def ?fc:(loc_ = ghost) name_opt strexp : structDec =
      StructDef_ (name_opt, strexp)
  end

  (* Query/Define/Solve constructor module *)
  module Query = struct
    type nonrec query = query
    type nonrec define = define
    type nonrec solve = solve

    let query ?fc:(loc_ = ghost) name_opt term =
      Query_ (name_opt, term)

    let define ?fc:(loc_ = ghost) name_opt term1 term2_opt =
      Define_ (name_opt, term1, term2_opt)

    let solve ?fc:(loc_ = ghost) name_opt term =
      Solve_ (name_opt, term)
  end

  (* Command constructor module *)
  module Cmd = struct
    let query ?fc:(_ = ghost) ~n ~b ~d q = QueryCmd_ (n, b, d, q)
    let query_tabled ?fc:(_ = ghost) ~n ~b ~d q = QueryTabledCmd_ (n, b, d, q)
    let adhoc_query ?fc:(_ = ghost) q = AdhocQueryCmd_ q
    let unique ?fc:(_ = ghost) tm = UniqueCmd_ tm
    let mode ?fc:(_ = ghost) id md = ModeCmd_ (id, md)
    let define ?fc:(_ = ghost) d = DefineCmd_ d
    let decl_cmd ?fc:(_ = ghost) tm = DeclCmd_ tm
    let inline ?fc:(_ = ghost) id tm = InlineCmd_ (id, tm)
    let symbol ?fc:(_ = ghost) id1 id2 = SymbolCmd_ (id1, id2)
    let freeze ?fc:(_ = ghost) ids = FreezeCmd_ ids
    let thaw ?fc:(_ = ghost) ids = ThawCmd_ ids
    let sort ?fc:(_ = ghost) id decls = SortCmd_ (id, decls)
    let term ?fc:(_ = ghost) d = TermCmd_ d
    let block ?fc:(_ = ghost) id items = BlockCmd_ (id, items)
    let union ?fc:(_ = ghost) id ids = UnionCmd_ (id, ids)
    let worlds ?fc:(_ = ghost) ids tm = WorldsCmd_ (ids, tm)
    let deterministic ?fc:(_ = ghost) ids = DeterministicCmd_ ids
    let module_cmd ?fc:(_ = ghost) id params cmds = ModuleCmd_ (id, params, cmds)
    let use ?fc:(_ = ghost) id1 id2 ps = UseCmd_ (id1, id2, ps)
    let open_cmd ?fc:(_ = ghost) id ids = OpenCmd_ (id, ids)
    let eval ?fc:(_ = ghost) cmds = EvalCmd_ cmds
    let prec ?fc:(_ = ghost) fix n ids = PrecCmd_ (fix, n, ids)
    let solve ?fc:(_ = ghost) s = SolveCmd_ s
    let stop ?fc:(_ = ghost) () = StopCmd_

    module Repl = struct
      let quit    ?fc:(_ = ghost) ()    = QuitCmd_
      let help    ?fc:(_ = ghost) t     = HelpCmd_ t
      let get     ?fc:(_ = ghost) s     = GetCmd_ s
      let set     ?fc:(_ = ghost) s v   = SetCmd_ (s, v)
      let version ?fc:(_ = ghost) ()    = VersionCmd_
    end
  end

  module Fixity = struct
    let left    = Left_
    let right   = Right_
    let prefix  = Prefix_
    let postfix = Postfix_
    let middle  = Middle_
    let none    = FNone_
  end

  module BlockItem = struct
    let some d = BlockSome_ d
    let pi   d = BlockPi_   d
  end

  module Thm = struct
    type order =
      | Varg_ of loc * string list
      | Lex_ of loc * order list
      | Simul_ of loc * order list

    let varg (r, names) = Varg_ (r, names)
    let lex (r, orders) = Lex_ (r, orders)
    let simul (r, orders) = Simul_ (r, orders)

    type callpats = (string * string option list * loc) list

    let callpats callpats_ = callpats_

    type tdecl = order * callpats

    let tdecl (order_, callpats_) = (order_, callpats_)

    type predicate = string * loc

    let predicate predicate_ = predicate_

    type rdecl = predicate * order * order * callpats

    let rdecl (predicate_, order1_, order2_, callpats_) =
      (predicate_, order1_, order2_, callpats_)

    type tableddecl = string * loc
    let tableddecl tableddecl_ = tableddecl_

    type keepTabledecl = string * loc
    let keepTabledecl keepTabledecl_ = keepTabledecl_

    type prove = int * tdecl
    let prove prove_ = prove_

    type establish = int * tdecl
    let establish establish_ = establish_

    type assert_ = callpats
    let assert_ assert__ = assert__

    type decs = decl list
    type theorem =
      | Top_
      | Exists_ of decs * theorem
      | Forall_ of decs * theorem
      | ForallStar_ of decs * theorem
      | ForallG_ of (decs * decs) list * theorem

    type theoremdec = string * theorem

    let null = []
    let decl (decs_, decl_) = decs_ @ [ decl_ ]
    let top = Top_
    let exists (decs_, theorem_) = Exists_ (decs_, theorem_)
    let forall (decs_, theorem_) = Forall_ (decs_, theorem_)
    let forallStar (decs_, theorem_) = ForallStar_ (decs_, theorem_)
    let forallG (decs_, theorem_) = ForallG_ (decs_, theorem_)
    let dec (name_, theorem_) = (name_, theorem_)

    type wdecl = (string list * string) list * callpats
    let wdecl wdecl_ = wdecl_
  end

  module View = struct
    let term_loc = function
      | Omitted_ loc
      | Lcid_ (_, _, loc)
      | Ucid_ (_, _, loc)
      | Quid_ (_, _, loc)
      | Scon_ (_, loc)
      | Evar_ (_, loc)
      | Fvar_ (_, loc)
      | Typ_ loc -> Some loc
      | Arrow_ _
      | Pi_ _
      | Lam_ _
      | App_ _
      | Hastype_ _ -> None

    let term_lcid = function Lcid_ (ns, n, _) -> Some (ns, n) | _ -> None
    let term_ucid = function Ucid_ (ns, n, _) -> Some (ns, n) | _ -> None
    let term_quid = function Quid_ (ns, n, _) -> Some (ns, n) | _ -> None
    let term_scon = function Scon_ (s, _) -> Some s | _ -> None
    let term_evar = function Evar_ (s, _) -> Some s | _ -> None
    let term_fvar = function Fvar_ (s, _) -> Some s | _ -> None
    let term_typ = function Typ_ _ -> true | _ -> false
    let term_omitted = function Omitted_ _ -> true | _ -> false

    let term_arrow = function Arrow_ (a, b) -> Some (a, b) | _ -> None
    let term_pi = function Pi_ (d, t) -> Some (d, t) | _ -> None
    let term_lam = function Lam_ (d, t) -> Some (d, t) | _ -> None
    let term_app = function App_ (a, b) -> Some (a, b) | _ -> None
    let term_has_type = function Hastype_ (a, b) -> Some (a, b) | _ -> None

    let decl_fields (Dec_ (names, t, loc)) = (names, t, loc)

    let condec_constant_decl = function ConstantDecl_ d -> Some d | _ -> None

    let condec_constant_def = function
      | ConstantDef_ (n, t1, t2) -> Some (n, t1, t2)
      | _ -> None

    let condec_block_decl = function
      | BlockDecl_ (n, d1, d2) -> Some (n, d1, d2)
      | _ -> None

    let condec_block_def = function
      | BlockDef_ (n, syms) -> Some (n, syms)
      | _ -> None

    let query_fields (Query_ (n, t)) = (n, t)
    let define_fields (Define_ (n, t1, t2)) = (n, t1, t2)
    let solve_fields (Solve_ (n, t)) = (n, t)

    let mode_view = function
      | Plus_ -> `Plus
      | Star_ -> `Star
      | Minus_ -> `Minus
      | Minus1_ -> `Minus1

    let mode_short = function
      | ModeDec_ (ModeTermRoot_ (Quid_ (ns, id, _))) ->
          Some ((ns, id), [])
      | _ -> None

    let mode_full = function
      | ModeDec_ (ModeTermPi_ _ as mt) ->
          let rec go = function
            | ModeTermPi_ (m, Dec_ (names, _ty, _), body) ->
                let name_opt = match names with n :: _ -> n | [] -> None in
                let (modes, root) = go body in
                ((m, name_opt) :: modes, root)
            | ModeTermRoot_ tm -> ([], tm)
          in
          Some (go mt)
      | _ -> None

    let struct_strexp_symbol (StrExp_ s) = Some s

    let struct_inst_con = function
      | ConInst_ (s, loc, t) -> Some (s, loc, t)
      | _ -> None

    let struct_inst_str = function
      | StrInst_ (s, loc, e) -> Some (s, loc, e)
      | _ -> None

    let struct_sigexp_id = function SigId_ id -> Some id | _ -> None

    let struct_sigexp_where = function
      | WhereSig_ (s, insts) -> Some (s, insts)
      | _ -> None

    let struct_sigdef_fields (SigDef_ (name, sigexp)) = (name, sigexp)

    let struct_structdecl_decl = function
      | StructDecl_ (name, sigexp) -> Some (name, sigexp)
      | _ -> None

    let struct_structdecl_def (d : structDec) =
      match d with
      | StructDef_ (name, strexp) -> Some (name, strexp)
      | StructDecl_ _ -> None

    let thm_order_varg = function Thm.Varg_ (r, names) -> Some (r, names) | _ -> None
    let thm_order_lex = function Thm.Lex_ (r, os) -> Some (r, os) | _ -> None
    let thm_order_simul = function Thm.Simul_ (r, os) -> Some (r, os) | _ -> None

    let thm_callpats cps = cps
    let thm_tdecl (o, cps) = (o, cps)
    let thm_predicate p = p
    let thm_rdecl r = r
    let thm_tableddecl t = t
    let thm_keepTabledecl t = t
    let thm_prove p = p
    let thm_establish p = p
    let thm_assert a = a

    let thm_theorem_top = function Thm.Top_ -> true | _ -> false

    let thm_theorem_exists = function
      | Thm.Exists_ (d, t) -> Some (d, t)
      | _ -> None

    let thm_theorem_forall = function
      | Thm.Forall_ (d, t) -> Some (d, t)
      | _ -> None

    let thm_theorem_forallStar = function
      | Thm.ForallStar_ (d, t) -> Some (d, t)
      | _ -> None

    let thm_theorem_forallG = function
      | Thm.ForallG_ (g, t) -> Some (g, t)
      | _ -> None

    let thm_decs_nil = []
    let thm_decs_list d = d
    let thm_theoremdec td = td
    let thm_wdecl wd = wd
  end
end   

module Cst : CST = Make_Cst (Paths.Paths_)
 