module type RECON_CONDEC = sig 
include S.S 

  exception Error of string

  val condecToConDec :
    Cst.conDec * Paths.location * bool ->
    Ast.conDec option * Paths.occConDec option

  (* optional ConDec is absent for anonymous definitions *)
  (* bool = true means that condec is an abbreviation *)
  val internalInst :
    Ast.conDec * Ast.conDec * Paths.region -> Ast.conDec

  val externalInst : Ast.conDec * Cst.term * Paths.region -> Ast.conDec
end

