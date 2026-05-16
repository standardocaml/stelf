module type RECON_CONDEC = RECON_CONDEC.RECON_CONDEC

module Make_ReconConDec (M : S.S) : RECON_CONDEC = struct
  include M

  exception Error of string

  let condecToConDec (_ : Cst.conDec * Paths.location * bool) =
    raise (Error "Make_ReconConDec.condecToConDec: stub")

  let internalInst (_ : Ast.conDec * Ast.conDec * Paths.region) =
    raise (Error "Make_ReconConDec.internalInst: stub")

  let externalInst (_ : Ast.conDec * Cst.term * Paths.region) =
    raise (Error "Make_ReconConDec.externalInst: stub")
end
