type 'a flag_parser = 

  (** Whether or not [A-Z] charecters are treated as uppercase*)
  | UppercaseLatin : bool flag_parser

  (** If [->] is a valid alternative to [%->] (and the same for [<-])*)
  | ArrowReserved : bool flag_parser
  
  (** Whether [->] [<-] can occur infix*)
  | ArrowInfix : bool flag_parser

  (** Whether [|] can be used in context in a world / block *)
  | BarInContext : bool flag_parser

  (** Whether [:] can be used in decl *)
  | ColonInDecl : bool flag_parser

  (** Whether [=] can be used in def *)
  | EqualInDef : bool flag_parser

  (** Whether [.] is a alternative to [.] *)
  | StopReserved : bool flag_parser

module type CONFIG = sig 
  type 'a t 
  val get : 'a t -> 'a 
  val set : 'a t -> 'a -> bool
  val init : unit -> unit
end