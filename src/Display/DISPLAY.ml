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

  val message :
    ?src:Info.src -> ?kind:Info.kind -> ?level:Info.level -> Form.Form.t -> unit

  val debug : ?src:Info.src -> ?level:Info.level -> Form.Form.t -> unit
  val info : ?src:Info.src -> ?level:Info.level -> Form.Form.t -> unit
  val warning : ?src:Info.src -> ?level:Info.level -> Form.Form.t -> unit
  val error : ?src:Info.src -> ?level:Info.level -> Form.Form.t -> unit
end

module Form = Form.Form
