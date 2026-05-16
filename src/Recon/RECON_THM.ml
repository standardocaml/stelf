module type RECON_THM = sig 
include S.S
    module ThmSyn : Thm.Thmsyn.THMSYN

  exception Error of string

  val tdeclTotDecl : ThmSyn.tDecl -> ThmSyn.tDecl * (Paths.region * Paths.region list)
  val rdeclTorDecl : ThmSyn.rDecl -> ThmSyn.rDecl * (Paths.region * Paths.region list)
  val tableddeclTotabledDecl : ThmSyn.tabledDecl -> ThmSyn.tabledDecl * Paths.region

  val keepTabledeclToktDecl :
    ThmSyn.keepTableDecl -> ThmSyn.keepTableDecl * Paths.region

  val theoremToTheorem : Cst.Thm.theorem -> ThmSyn.thDecl
  val theoremDecToTheoremDec : Cst.Thm.theoremdec -> string * ThmSyn.thDecl
  val proveToProve : Cst.Thm.prove -> ThmSyn.pDecl * (Paths.region * Paths.region list)

  val establishToEstablish :
    Cst.Thm.establish -> ThmSyn.pDecl * (Paths.region * Paths.region list)

  val assertToAssert : Cst.Thm.assert_ -> ThmSyn.callpats * Paths.region list
  val wdeclTowDecl : Cst.Thm.wdecl -> ThmSyn.wDecl * Paths.region list
end

 