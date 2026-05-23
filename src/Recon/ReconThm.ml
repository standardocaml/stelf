module type RECON_THM = RECON_THM.RECON_THM

module Make_ReconThm
    (M  : S.S)
    (RT : RECON_TERM.RECON_TERM with module M = M)
  : RECON_THM with module M = M = struct
  module M      = M
  module Cst    = M.Cst
  module Ast    = M.Ast
  module Paths  = M.Paths
  module Syntax = M.Syntax

  module ThmSyn = Thm.Thm_.ThmSyn

  exception Error of string

  let error (r, msg) = raise (Error (Paths.wrap (r, msg)))

  (* Build an IntSyn ctx from a decl list (leftmost = outermost). *)
  let makectx decls =
    let rec go ctx = function
      | [] -> ctx
      | d :: rest -> go (IntSyn.Decl (ctx, d)) rest
    in
    go IntSyn.Null decls

  let ctxAppend g1 g2 =
    let rec go = function
      | IntSyn.Null -> g1
      | IntSyn.Decl (g', d) -> IntSyn.Decl (go g', d)
    in
    go g2

  let ctxMap f ctx =
    let rec go = function
      | IntSyn.Null -> IntSyn.Null
      | IntSyn.Decl (g, d) -> IntSyn.Decl (go g, f d)
    in
    go ctx

  (* ------------------------------------------------------------------ *)
  (* Order conversion *)

  let rec convertOrder ord =
    match Cst.View.thm_order_varg ord with
    | Some (_loc, names) -> (ThmSyn.Varg names, Cst.loc_to_region _loc)
    | None ->
    match Cst.View.thm_order_lex ord with
    | Some (loc0, ords) ->
      let rec go = function
        | [] -> ([], Cst.loc_to_region loc0)
        | o :: rest ->
          let os, r' = go rest in
          let o', r = convertOrder o in
          (o' :: os, Paths.join (r, r'))
      in
      let os, r1 = go ords in
      (ThmSyn.Lex os, r1)
    | None ->
    match Cst.View.thm_order_simul ord with
    | Some (loc0, ords) ->
      let rec go = function
        | [] -> ([], Cst.loc_to_region loc0)
        | o :: rest ->
          let os, r' = go rest in
          let o', r = convertOrder o in
          (o' :: os, Paths.join (r, r'))
      in
      let os, r1 = go ords in
      (ThmSyn.Simul os, r1)
    | None -> raise (Error "convertOrder: unrecognised order variant")

  (* ------------------------------------------------------------------ *)
  (* Call-pattern helpers *)

  let checkArgNumber (i, v_, args, r) =
    let rec go i v_ args =
      match (i, v_, args) with
      | 0, IntSyn.Uni IntSyn.Type, [] -> ()
      | 0, IntSyn.Pi (_, v2_), _ :: rest -> go 0 v2_ rest
      | 0, IntSyn.Pi (_, _), [] -> error (r, "Missing arguments in call pattern")
      | 0, IntSyn.Uni IntSyn.Type, _ :: _ -> error (r, "Extraneous arguments in call pattern")
      | i, IntSyn.Pi (_, v2_), args -> go (i - 1) v2_ args
      | _ -> ()
    in
    go i v_ args

  let checkCallPat (cd, p_, r) =
    match cd with
    | IntSyn.ConDec (_, _, i, IntSyn.Normal, v_, IntSyn.Kind) ->
      checkArgNumber (i, v_, p_, r)
    | IntSyn.ConDec (a, _, _, IntSyn.Constraint _, _, _) ->
      error (r, "Illegal constraint constant " ^ a ^ " in call pattern")
    | IntSyn.ConDec (a, _, _, IntSyn.Foreign _, _, _) ->
      error (r, "Illegal foreign constant " ^ a ^ " in call pattern")
    | IntSyn.ConDec (a, _, _, _, _, IntSyn.Type) ->
      error (r, "Constant " ^ a ^ " in call pattern not a type family")
    | IntSyn.ConDef (a, _, _, _, _, _, _) ->
      error (r, "Illegal defined constant " ^ a ^ " in call pattern")
    | IntSyn.AbbrevDef (a, _, _, _, _, _) ->
      error (r, "Illegal abbreviation " ^ a ^ " in call pattern")
    | IntSyn.BlockDec (a, _, _, _) ->
      error (r, "Illegal block identifier " ^ a ^ " in call pattern")
    | IntSyn.SkoDec (a, _, _, _, _) ->
      error (r, "Illegal Skolem constant " ^ a ^ " in call pattern")
    | _ -> ()

  let resolveCallPat (name, p_, loc) =
    let r = Cst.loc_to_region loc in
    let qid = Names.Qid ([], name) in
    match Names.constLookup qid with
    | None ->
      error (r, "Undeclared identifier "
        ^ Names.qidToString (valOf (Names.constUndef qid))
        ^ " in call pattern")
    | Some cid ->
      checkCallPat (IntSyn.sgnLookup cid, p_, r);
      ((cid, p_), r)

  let resolveCallpats cps_cst =
    let raw = Cst.View.thm_callpats cps_cst in
    let rec go = function
      | [] -> ([], [])
      | cp :: rest ->
        let cps, rs = go rest in
        let cp', r = resolveCallPat cp in
        (cp' :: cps, r :: rs)
    in
    let cps, rs = go raw in
    (ThmSyn.Callpats cps, rs)

  (* ------------------------------------------------------------------ *)
  (* tdecl / rdecl / tabled / keepTabled *)

  let tdeclTotDecl td =
    let (ord, cps_cst) = Cst.View.thm_tdecl td in
    let (o_, r) = convertOrder ord in
    let (cp'_, rs) = resolveCallpats cps_cst in
    (ThmSyn.TDecl (o_, cp'_), (r, rs))

  let rdeclTorDecl rd =
    let (pred_cst, o1_cst, o2_cst, cps_cst) = Cst.View.thm_rdecl rd in
    let (pred_str, _pred_loc) = Cst.View.thm_predicate pred_cst in
    let pred_ = match pred_str with
      | "LESS"  -> ThmSyn.Less
      | "LEQ"   -> ThmSyn.Leq
      | "EQUAL" -> ThmSyn.Eq
      | s -> raise (Error ("Unknown predicate: " ^ s))
    in
    let (o1_, r1) = convertOrder o1_cst in
    let (o2_, _r2) = convertOrder o2_cst in
    let r = r1 in
    let (cp'_, rs) = resolveCallpats cps_cst in
    (ThmSyn.RDecl (ThmSyn.RedOrder (pred_, o1_, o2_), cp'_), (r, rs))

  let tableddeclTotabledDecl td =
    let (name, loc) = Cst.View.thm_tableddecl td in
    let r = Cst.loc_to_region loc in
    let qid = Names.Qid ([], name) in
    match Names.constLookup qid with
    | None ->
      error (r, "Undeclared identifier "
        ^ Names.qidToString (valOf (Names.constUndef qid))
        ^ " in tabled declaration")
    | Some cid -> (ThmSyn.TabledDecl cid, r)

  let keepTabledeclToktDecl ktd =
    let (name, loc) = Cst.View.thm_keepTabledecl ktd in
    let r = Cst.loc_to_region loc in
    let qid = Names.Qid ([], name) in
    match Names.constLookup qid with
    | None ->
      error (r, "Undeclared identifier "
        ^ Names.qidToString (valOf (Names.constUndef qid))
        ^ " in keepTable declaration")
    | Some cid -> (ThmSyn.KeepTableDecl cid, r)

  (* ------------------------------------------------------------------ *)
  (* prove / establish / assert *)

  let proveToProve pv =
    let (n, td) = Cst.View.thm_prove pv in
    let (td_, rrs) = tdeclTotDecl td in
    (ThmSyn.PDecl (n, td_), rrs)

  let establishToEstablish es =
    let (n, td) = Cst.View.thm_establish es in
    let (td_, rrs) = tdeclTotDecl td in
    (ThmSyn.PDecl (n, td_), rrs)

  let assertToAssert a =
    let cps_cst = Cst.View.thm_assert a in
    resolveCallpats cps_cst

  (* ------------------------------------------------------------------ *)
  (* Context-block helpers (for forallG) *)

  let ctxBlockToString (g0_, (g1_, g2_)) =
    let _ = Names.varReset IntSyn.Null in
    let g0' = Names.ctxName g0_ in
    let g1' = Names.ctxLUName g1_ in
    let g2' = Names.ctxLUName g2_ in
    let some_part = match g1' with
      | IntSyn.Null -> ""
      | _ -> "some " ^ Print.ctxToString (g0', g1') ^ "\n"
    in
    Print.ctxToString (IntSyn.Null, g0') ^ "\n"
    ^ some_part
    ^ "pi "
    ^ Print.ctxToString (ctxAppend g0' g1', g2')

  let checkFreevars (g0_, (g1_, g2_), r) =
    match g0_ with
    | IntSyn.Null -> ()
    | _ ->
      let _ = Names.varReset IntSyn.Null in
      let g0' = Names.ctxName g0_ in
      let g1' = Names.ctxLUName g1_ in
      let g2' = Names.ctxLUName g2_ in
      error (r,
        "Free variables in context block after term reconstruction:\n"
        ^ ctxBlockToString (g0', (g1', g2')))

  let abstractCtxPair (g1_cst, g2_cst) =
    let r =
      match (RT.ctxRegion g1_cst, RT.ctxRegion g2_cst) with
      | Some r1, Some r2 -> Paths.join (r1, r2)
      | _, Some r2 -> r2
      | Some r1, None -> r1
      | None, None -> Paths.Reg (0, 0)
    in
    let (RT.JWithCtx (g1_, RT.JWithCtx (g2_, _))) =
      RT.recon (RT.jwithctx (g1_cst, RT.jwithctx (g2_cst, RT.jnothing)))
    in
    let g0_, ctxs =
      try Abstract.abstractCtxs [g1_; g2_]
      with Constraints.Error c_ ->
        error (r,
          "Constraints remain in context block after term reconstruction:\n"
          ^ ctxBlockToString (IntSyn.Null, (g1_, g2_))
          ^ "\n"
          ^ Print.cnstrsToString c_)
    in
    let (g1'_, g2'_) = match ctxs with
      | [a; b] -> (a, b)
      | _ -> assert false
    in
    let _ = checkFreevars (g0_, (g1'_, g2'_), r) in
    (g1'_, g2'_)

  (* ------------------------------------------------------------------ *)
  (* theoremToTheorem — recursive traversal of Cst.Thm.theorem *)

  let theoremToTheorem t =
    let rec go theorem (gbs, g_cst, m_ctx, k) =
      match Cst.View.thm_theorem_exists theorem with
      | Some (decs, rest) ->
        let g' = makectx (Cst.View.thm_decs_list decs) in
        let m' = ctxMap (fun _ -> Modes.Modes_.ModeSyn.Minus) g' in
        go rest (gbs, ctxAppend g_cst g', ctxAppend m_ctx m', k)
      | None ->
      match Cst.View.thm_theorem_forall theorem with
      | Some (decs, rest) ->
        let g' = makectx (Cst.View.thm_decs_list decs) in
        let m' = ctxMap (fun _ -> Modes.Modes_.ModeSyn.Plus) g' in
        go rest (gbs, ctxAppend g_cst g', ctxAppend m_ctx m', k)
      | None ->
      match Cst.View.thm_theorem_forallStar theorem with
      | Some (decs, rest) ->
        let g' = makectx (Cst.View.thm_decs_list decs) in
        let m' = ctxMap (fun _ -> Modes.Modes_.ModeSyn.Plus) g' in
        go rest (gbs, ctxAppend g_cst g', ctxAppend m_ctx m', IntSyn.ctxLength g')
      | None ->
      match Cst.View.thm_theorem_forallG theorem with
      | Some (gblist, rest) ->
        let gbs' = List.map (fun (d1, d2) ->
          (makectx (Cst.View.thm_decs_list d1),
           makectx (Cst.View.thm_decs_list d2))) gblist
        in
        go rest (gbs', IntSyn.Null, IntSyn.Null, 0)
      | None ->
        (* thm_theorem_top *)
        (gbs, g_cst, m_ctx, k)
    in
    let (gbs_cst, g_cst, m_ctx, k) = go t ([], IntSyn.Null, IntSyn.Null, 0) in
    let _ = Names.varReset IntSyn.Null in
    let gBs_ = List.map abstractCtxPair gbs_cst in
    let (RT.JWithCtx (g_, _)) = RT.recon (RT.jwithctx (g_cst, RT.jnothing)) in
    ThmSyn.ThDecl (gBs_, g_, m_ctx, k)

  let theoremDecToTheoremDec td =
    let (name, thm) = Cst.View.thm_theoremdec td in
    (name, theoremToTheorem thm)

  (* ------------------------------------------------------------------ *)
  (* wdecl *)

  let wdeclTowDecl wd =
    let (w_raw, cps_cst) = Cst.View.thm_wdecl wd in
    let w' = List.map (fun (ids, id) -> ThmSyn.Names.Qid (ids, id)) w_raw in
    let (cp'_, rs) = resolveCallpats cps_cst in
    (ThmSyn.WDecl (w', cp'_), rs)
end
