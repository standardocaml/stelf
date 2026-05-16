module type S = sig 
  module Cst : Cst.CST
  module Syntax : Syntax.SYNTAX
  module Ast : module type of Syntax.Ast
  module Paths : module type of Cst.Paths
end 