(* # 1 "src/frontend/Twelf_.sig.ml" *)
open! Basis


module type PAL = sig 
  module Names : Names.Names_intf.NAMES 
  module Parser : Parser.PARSER
  module Syntax : Syntax.SYNTAX 
  module Cst : Cst.CST

  module Install : sig 
    val install : Cst.cmd list -> unit
  end 
  module Load : sig 
    
    val load_file : Fpath.t -> int 
    val load_string : string -> int
  end

  module Config : sig 
    
  end
end 

