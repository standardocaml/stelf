
module type COLORS = sig 
  type t 
  val black : t
  val red : t
  val green : t
  val yellow : t
  val blue : t
  val magenta : t
  val cyan : t
  val white : t
end
module type FORM = sig 
  type t 
  type style 
  type 'a scribe = 'a -> t
  module Style : sig 
    val bold : style
    val italic : style
    val underline : style
    module Fore : COLORS with type t := style
    module Back : COLORS with type t := style
  end
  
  val (++) : t -> t -> t
  val style : style -> t -> t
  val styles : style list -> t -> t
  val empty : t
  val concat : ?sep:t -> t list -> t
  val string : string -> t
  val int : int -> t
  val char : char -> t
  val bool : bool -> t
  val sp : ?n:int -> unit -> t
  val cut : unit -> t
  val shown : ('a -> string) -> 'a -> t
  val inside : t * t -> t -> t
  val nl : ?n:int -> unit -> t
  val each : ?sep:t -> ('a -> t) -> 'a list -> t

  val markup : t -> LTerm_text.t
  val fmt : t Fmt.t
end  