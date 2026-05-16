module type PARSER = sig 
  include module type of Angstrom 
  
  val with_fc : 'a t -> ('a * int * int) t
  val inside : string -> string -> 'a t -> 'a t
  val whitespace : unit t
  val ident : string t
  val keyword : string -> unit t
  val token : string -> unit t

  val ( let| ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and| ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  val ( let@ ) : 'a t -> (('a * int * int) -> 'b t) -> 'b t

end 