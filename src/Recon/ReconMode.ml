module type RECON_MODE = RECON_MODE.RECON_MODE

module StubModes : Modes.Modesyn.MODESYN =
  (val (Obj.magic () : (module Modes.Modesyn.MODESYN)))

module Make_ReconMode (M : S.S) : RECON_MODE = struct
  include M

  module Modes = StubModes

  exception Error of string

  let modeToMode (_ : Cst.modeDec) =
    raise (Error "Make_ReconMode.modeToMode: stub")
end
