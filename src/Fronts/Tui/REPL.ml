module type REPL = sig 
  val stop : int -> unit
  (** Exit with a code *)

  val read : (string -> 'a Lwt.t option) -> 'a Lwt.t 
  (** Reads from the terminal until a line break proceeds to give [Some _] as an answer *)


  val show : Format.formatter -> unit
  (** Show the REPL prompt. *)
end  