open Basis
module type CST = CST.CST

module Make_Cst (Paths : Paths.Paths_intf.PATHS) = struct

  (* Location type for source tracking - use simple int-based representation *)
  type loc = { start_pos : int; end_pos : int }

  (* Basic type aliases *)
  type name = string
  type namespace = string list
  type symbol = namespace * name

  (* Location helpers *)
  let mk_loc (start_ : int) (end_ : int) : loc = { start_pos = start_; end_pos = end_ }
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
    [@@deriving show {with_path = false}]

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
  type modeTerm = ModeTermInternal_ of term
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

  (* Top-level commands *)
  type cmd =
    | QueryCmd_ of query
    | DefineCmd_ of define
    | SolveCmd_ of solve
    | SortCmd_ of decl list
    | TermCmd_ of decl
    | StopCmd_
    | QuitCmd_
    | HelpCmd_ of string option
    | GetCmd_ of string
    | SetCmd_ of string * string
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

    let omitted ?fc:(loc_ = ghost) () =
      Omitted_ loc_
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
        ModeTermInternal_ (Quid_ (ns, name, loc_))

      let to_modeDec ?fc:(loc_ = ghost) (ModeTermInternal_ tm) =
        ModeDec_ (ModeTermInternal_ tm)
    end

    module Full = struct
      let mode_root ?fc:(loc_ = ghost) tm =
        ModeTermInternal_ tm

      let mode_pi ?fc:(loc_ = ghost) m (Dec_ (name, ty, _)) (ModeTermInternal_ body) =
        ModeTermInternal_ (Pi_ (Dec_ (name, ty, loc_), body))

      let to_modeDec ?fc:(loc_ = ghost) (ModeTermInternal_ tm) =
        ModeDec_ (ModeTermInternal_ tm)
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

    let thesig ?fc:(loc_ = ghost) () =
      TheSig_

    let sig_id ?fc:(loc_ = ghost) name =
      SigId_ name

    let where_sig ?fc:(loc_ = ghost) sigexp insts =
      WhereSig_ (sigexp, insts)

    let sig_def ?fc:(loc_ = ghost) name_opt sigexp =
      SigDef_ (name_opt, sigexp)

    let struct_decl ?fc:(loc_ = ghost) name_opt sigexp =
      StructDecl_ (name_opt, sigexp)

    let struct_def ?fc:(loc_ = ghost) name_opt strexp =
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
    let query ?fc:(loc_ = ghost) (Query_ (name_opt, term)) =
      QueryCmd_ (Query_ (name_opt, term))

    let define ?fc:(loc_ = ghost) (Define_ (name_opt, term1, term2_opt)) =
      DefineCmd_ (Define_ (name_opt, term1, term2_opt))

    let solve ?fc:(loc_ = ghost) (Solve_ (name_opt, term)) =
      SolveCmd_ (Solve_ (name_opt, term))

    let sort ?fc:(loc_ = ghost) decls =
      SortCmd_ decls

    let term ?fc:(loc_ = ghost) decl =
      TermCmd_ decl

    let stop ?fc:(loc_ = ghost) () =
      StopCmd_

    module Repl = struct
      let quit ?fc:(loc_ = ghost) () =
        QuitCmd_

      let help ?fc:(loc_ = ghost) topic_opt =
        HelpCmd_ topic_opt

      let get ?fc:(loc_ = ghost) setting =
        GetCmd_ setting

      let set ?fc:(loc_ = ghost) setting value =
        SetCmd_ (setting, value)

      let version ?fc:(loc_ = ghost) () =
        VersionCmd_
    end
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
end   