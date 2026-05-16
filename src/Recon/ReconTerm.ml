module type RECON_TERM = RECON_TERM.RECON_TERM

module Make_ReconTerm (M : S.S) : RECON_TERM = struct
  include M

  exception Error of string

  let resetErrors (_ : string) = ()
  let checkErrors (_ : Paths.region) = ()

  type traceMode = Progressive | Omniscient

  let trace = ref false
  let traceMode = ref Progressive

  type job = unit

  let jnothing = ()
  let jand (_ : job * job) = ()
  let jwithctx (_ : Cst.decl Ast.ctx * job) = ()
  let jterm (_ : Cst.term) = ()
  let jclass (_ : Cst.term) = ()
  let jof (_ : Cst.term * Cst.term) = ()
  let jof' (_ : Cst.term * Ast.exp) = ()

  type job_ =
    | JNothing
    | JAnd of job_ * job_
    | JWithCtx of Cst.decl Ast.ctx * job_
    | JTerm of (Ast.exp * Paths.occExp) * Ast.exp * Ast.uni
    | JClass of (Ast.exp * Paths.occExp) * Ast.uni
    | JOf of (Ast.exp * Paths.occExp) * (Ast.exp * Paths.occExp) * Ast.uni
 
  let recon (_ : job) = raise (Error "Make_ReconTerm.recon: stub")
  let reconQuery (_ : job) = raise (Error "Make_ReconTerm.reconQuery: stub")
  let reconWithCtx (_ : Ast.dctx * job) =
    raise (Error "Make_ReconTerm.reconWithCtx: stub")

  let reconQueryWithCtx (_ : Ast.dctx * job) =
    raise (Error "Make_ReconTerm.reconQueryWithCtx: stub")

  let termRegion (_ : Cst.term) : Paths.region = Obj.magic ()
  let decRegion (_ : Cst.decl) : Paths.region = Obj.magic ()
  let ctxRegion (_ : Cst.decl Ast.ctx) : Paths.region option = None

  let internalInst (_ : 'a) : 'b = raise (Error "Make_ReconTerm.internalInst: stub")
  let externalInst (_ : 'a) : 'b = raise (Error "Make_ReconTerm.externalInst: stub")
end
