module type PAL' = sig
  module M : IMPL.IMPL
    val install : M.Cst.cmd -> unit
    val parse : string -> M.Cst.cmd list
    val exec : string -> unit
  end
module type PAL = sig
  module M : IMPL.IMPL 

  exception Error of exn

  module Start () : PAL' with module M = M

  val run : unit -> unit
end
