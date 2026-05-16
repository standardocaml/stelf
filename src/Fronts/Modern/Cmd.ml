module Make_Cmd(Modern : MODERN.MODERN) : CMD.CMD = struct
  module Modern = Modern
  module Parser = Modern.Parser
  module Cst = Modern.Cst
  module Paths = Modern.Paths
  module Names = Modern.Names


  type 'a t = 'a Modern.t

   let parse1 () : Cst.cmd t = assert false 
   let parse () : Cst.cmd list t = assert false

end