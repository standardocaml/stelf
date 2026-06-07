module type RESUGAR = sig
  module Syntax : Syntax.SYNTAX
                    module Cst : Cst.CST
  type t
  
