module type RECON_MODE = sig 
  include S.S 
  module Modes : Modes.Modesyn.MODESYN
  exception Error of string

  val modeToMode :
    Cst.modeDec -> (Ast.cid * Modes.modeSpine) * Paths.region
end

