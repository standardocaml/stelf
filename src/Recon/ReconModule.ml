module type RECON_MODULE = RECON_MODULE.RECON_MODULE

module StubModSyn : Modules.Modsyn.MODSYN =
  (val (Obj.magic () : (module Modules.Modsyn.MODSYN)))

module Make_ReconModule (M : S.S) : RECON_MODULE = struct
  module M = M
  module Cst = M.Cst
  module Ast = M.Ast
  module Paths = M.Paths
  
  module ModSyn = StubModSyn

  exception Error of string

  type whereclause = unit

  type structDec =
    | StructDec of string option * ModSyn.module_ * whereclause list
    | StructDef of string option * Ast.mid

  let strexpToStrexp (_ : Cst.strexp) =
    raise (Error "Make_ReconModule.strexpToStrexp: stub")

  let sigexpToSigexp (_ : Cst.sigexp * ModSyn.module_ option) =
    raise (Error "Make_ReconModule.sigexpToSigexp: stub")

  let sigdefToSigdef (_ : Cst.sigdef * ModSyn.module_ option) =
    raise (Error "Make_ReconModule.sigdefToSigdef: stub")

  let structdecToStructDec (_ : Cst.structDec * ModSyn.module_ option) =
    raise (Error "Make_ReconModule.structdecToStructDec: stub")

  let moduleWhere (_ : ModSyn.module_ * whereclause) =
    raise (Error "Make_ReconModule.moduleWhere: stub")
end
