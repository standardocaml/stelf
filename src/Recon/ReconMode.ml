module type RECON_MODE = RECON_MODE.RECON_MODE

module ModeDec = Modes.Modedec.MakeModeDec ()
let ghost_region = Paths.Paths_.Paths.Reg (0, 0)

module Make_ReconMode (M : S.S) : RECON_MODE with module M = M = struct
  module M = M
  module Syntax = M.Syntax
  module Cst = M.Cst
  module Ast = M.Ast
  module Paths = M.Paths
  module Modes = Modes.Modesyn.ModeSyn

  exception Error of string

  let modeToMode md =
    begin match Cst.View.mode_short md with
    | Some ((ns, id), _spine) ->
        let qid = Qid (ns, id) in
        begin match constLookup qid with
        | None ->
            raise (Error
              ("Undeclared identifier "
               ^ qidToString (valOf (constUndef qid))
               ^ " in mode declaration"))
        | Some cid ->
            let mS = ModeDec.shortToFull (cid, Modes.Mnil, ghost_region) in
            let r = Paths.Reg (0, 0) in
            ((cid, mS), r)
        end
    | None ->
        begin match Cst.View.mode_full md with
        | None -> raise (Error "Invalid mode declaration")
        | Some (modes, root_tm) ->
            let rec head tm =
              match Cst.View.term_app tm with
              | Some (f, _) -> head f
              | None -> tm
            in
            begin match Cst.View.term_quid (head root_tm) with
            | None -> raise (Error "Mode declaration root is not a constant")
            | Some (ns, id) ->
                let qid = Qid (ns, id) in
                begin match constLookup qid with
                | None ->
                    raise (Error
                      ("Undeclared identifier "
                       ^ qidToString (valOf (constUndef qid))
                       ^ " in mode declaration"))
                | Some cid ->
                    let convert_mode m =
                      match Cst.View.mode_view m with
                      | `Plus -> Modes.Plus
                      | `Star -> Modes.Star
                      | `Minus -> Modes.Minus
                      | `Minus1 -> Modes.Minus1
                    in
                    let rec build_spine = function
                      | [] -> Modes.Mnil
                      | (m, name_opt) :: rest ->
                          Modes.Mapp (Modes.Marg (convert_mode m, name_opt), build_spine rest)
                    in
                    let mS = build_spine modes in
                    ModeDec.checkFull (cid, mS, ghost_region);
                    ((cid, mS), Paths.Reg (0, 0))
                end
            end
        end
    end
end
