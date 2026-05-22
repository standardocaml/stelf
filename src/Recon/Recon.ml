open ReconConDec
open ReconMode
open ReconModule
open ReconQuery
open ReconThm
open ReconTerm
 
module type RECON = RECON.RECON 
module type RECON_TERM = RECON_TERM.RECON_TERM
module type RECON_THM = RECON_THM.RECON_THM
module type RECON_CONDEC = RECON_CONDEC.RECON_CONDEC
module type RECON_MODE = RECON_MODE.RECON_MODE
module type RECON_MODULE = RECON_MODULE.RECON_MODULE
module type RECON_QUERY = RECON_QUERY.RECON_QUERY

module Make_Recon (M : S.S) : RECON = struct
include M
module Ast = M.Ast
module Cst = M.Cst
module ReconTerm = Make_ReconTerm(M)(struct
  module Names = Names
  module Approx = Approx
  module Whnf = Whnf
  module Unify = UnifyTrail
  module Abstract = Abstract
  module Print = Print
  module StringTree = TableInstances.StringRedBlackTree
  module Msg = Msg
  module CsManager = Solvers.CsManager
end)
module ReconThm = Make_ReconThm(M)
module ReconConDec = Make_ReconConDec(M)
module ReconMode = Make_ReconMode(M)
module ReconModule = Make_ReconModule(M)
module ReconQuery = Make_ReconQuery(M)

end        