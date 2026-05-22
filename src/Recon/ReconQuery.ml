module type RECON_QUERY = RECON_QUERY.RECON_QUERY

module Make_ReconQuery (M : S.S) : RECON_QUERY = struct
  module M = M
  module Cst = M.Cst
  module Ast = M.Ast
  module Paths = M.Paths
  module Syntax = M.Syntax
  exception Error of string

  let queryToQuery (_ : Cst.query * Paths.location) =
    raise (Error "Make_ReconQuery.queryToQuery: stub")

  let solveToSolve (_ : Cst.define list * Cst.solve * Paths.location) =
    raise (Error "Make_ReconQuery.solveToSolve: stub")
end
