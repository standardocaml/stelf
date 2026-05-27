val setup_log : level:Logs.level -> unit -> unit

module Level : sig
  type t = Debug | Info | Warning | Error | App

  val log_level : t -> Logs.level

  val from_chatter : int -> t
  [@@depracated
    "Don't use this, it's only for backwards compatibility with the old \
     chatter levels. Use the log levels directly instead."]
end

module Group : sig
  val approx : Logs.src
  val check : Logs.src
  val compile : Logs.src
  val typecheck : Logs.src
  val unify : Logs.src
  val cover : Logs.src
  val parse : Logs.src
  val reduce : Logs.src
  val meta : Logs.src
  val pal : Logs.src
  val default : Logs.src
end

val msg' :
  ?src:Logs.src ->
  ?level:Level.t ->
  (Format.formatter -> 'a -> unit) ->
  'a ->
  unit

val msg : ?src:Logs.src -> ?level:Level.t -> unit Fmt.t -> unit

module Fmt : sig
  include module type of Fmt

  val exact : string -> 'a Fmt.t
  val shown : ('a -> string) -> 'a Fmt.t
  val shown_exact : ('a -> string) -> 'a -> 'b Fmt.t
end
