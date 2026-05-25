
module type S = sig 
  type t
  val setup : unit -> t
  val flush : t -> unit -> Info.t array Lwt.t
  val display : t -> Info.t -> unit
end
module type DISPLAY = sig 
  include S
  val setup' : unit -> unit
  val flush' : unit -> Info.t array Lwt.t 
  val display' : Info.t -> unit
end 
