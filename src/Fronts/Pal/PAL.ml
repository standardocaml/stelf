module type PAL = sig
  module M : IMPL.IMPL

  exception Error of exn

  class pal : object
    method install : M.Cst.cmd -> unit
    method parse : string -> M.Cst.cmd list
    method exec : string -> unit
  end

  val run : unit -> unit
end
