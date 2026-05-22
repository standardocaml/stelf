module type S = sig
  module Paths : Paths.Paths_intf.PATHS
  module Cst : Cst.CST with module Paths = Paths
  module Syntax : Syntax.SYNTAX
  module Ast = Intsyn.IntSyn
end