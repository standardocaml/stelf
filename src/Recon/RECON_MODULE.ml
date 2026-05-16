module type RECON_MODULE = sig 
include S.S 
  module ModSyn : Modules.Modsyn.MODSYN

  exception Error of string

  type whereclause

  type structDec =
    | StructDec of string option * ModSyn.module_ * whereclause list
    | StructDef of string option * Ast.mid

  val strexpToStrexp : Cst.strexp -> Ast.mid

  val sigexpToSigexp :
    Cst.sigexp * ModSyn.module_ option -> ModSyn.module_ * whereclause list

  val sigdefToSigdef :
    Cst.sigdef * ModSyn.module_ option ->
    string option * ModSyn.module_ * whereclause list

  val structdecToStructDec : Cst.structDec * ModSyn.module_ option -> structDec
  val moduleWhere : ModSyn.module_ * whereclause -> ModSyn.module_
end
