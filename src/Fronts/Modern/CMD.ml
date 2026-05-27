module type CMD = sig
  module Cst : Cst.CST
  module Modern : MODERN.MODERN with module Cst = Cst

  type 'a t = 'a Modern.t

  val parse1 : unit -> Cst.cmd t
  val parse : unit -> Cst.cmd list t
end
