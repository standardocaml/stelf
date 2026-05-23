module type RECON_THM = sig
  module M : S.S
  module Cst = M.Cst
  module Ast = M.Ast
  module Paths = M.Paths
  module ThmSyn : Thm.Thmsyn.THMSYN

  exception Error of string

  val tdeclTotDecl : Cst.Thm.tdecl -> ThmSyn.tDecl * (Paths.region * Paths.region list)
  val rdeclTorDecl : Cst.Thm.rdecl -> ThmSyn.rDecl * (Paths.region * Paths.region list)
  val tableddeclTotabledDecl : Cst.Thm.tableddecl -> ThmSyn.tabledDecl * Paths.region

  val keepTabledeclToktDecl :
    Cst.Thm.keepTabledecl -> ThmSyn.keepTableDecl * Paths.region

  val theoremToTheorem : Cst.Thm.theorem -> ThmSyn.thDecl
  val theoremDecToTheoremDec : Cst.Thm.theoremdec -> string * ThmSyn.thDecl
  val proveToProve : Cst.Thm.prove -> ThmSyn.pDecl * (Paths.region * Paths.region list)

  val establishToEstablish :
    Cst.Thm.establish -> ThmSyn.pDecl * (Paths.region * Paths.region list)

  val assertToAssert : Cst.Thm.assert_ -> ThmSyn.callpats * Paths.region list
  val wdeclTowDecl : Cst.Thm.wdecl -> ThmSyn.wDecl * Paths.region list
end
