module type RECON_THM = RECON_THM.RECON_THM

module StubThmSyn : Thm.Thmsyn.THMSYN =
  (val (Obj.magic () : (module Thm.Thmsyn.THMSYN)))

module Make_ReconThm (M : S.S) : RECON_THM = struct
  include M

  module ThmSyn = StubThmSyn

  exception Error of string

  let tdeclTotDecl (_ : ThmSyn.tDecl) =
    raise (Error "Make_ReconThm.tdeclTotDecl: stub")

  let rdeclTorDecl (_ : ThmSyn.rDecl) =
    raise (Error "Make_ReconThm.rdeclTorDecl: stub")

  let tableddeclTotabledDecl (_ : ThmSyn.tabledDecl) =
    raise (Error "Make_ReconThm.tableddeclTotabledDecl: stub")

  let keepTabledeclToktDecl (_ : ThmSyn.keepTableDecl) =
    raise (Error "Make_ReconThm.keepTabledeclToktDecl: stub")

  let theoremToTheorem (_ : Cst.Thm.theorem) =
    raise (Error "Make_ReconThm.theoremToTheorem: stub")

  let theoremDecToTheoremDec (_ : Cst.Thm.theoremdec) =
    raise (Error "Make_ReconThm.theoremDecToTheoremDec: stub")

  let proveToProve (_ : Cst.Thm.prove) =
    raise (Error "Make_ReconThm.proveToProve: stub") 

  let establishToEstablish (_ : Cst.Thm.establish) =
    raise (Error "Make_ReconThm.establishToEstablish: stub")

  let assertToAssert (_ : Cst.Thm.assert_) =
    raise (Error "Make_ReconThm.assertToAssert: stub")

  let wdeclTowDecl (_ : Cst.Thm.wdecl) =
    raise (Error "Make_ReconThm.wdeclTowDecl: stub")
end
