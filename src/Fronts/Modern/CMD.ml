module type CMD = sig

  module Modern : MODERN.MODERN
  module Parser = Modern.Parser
  module Cst = Modern.Cst
  module Paths = Modern.Paths
  module Names = Modern.Names

  type 'a t = 'a Modern.t
  val parse1 : unit -> Cst.cmd t
  val parse : unit -> Cst.cmd list t  

end 