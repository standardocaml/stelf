
module type OPTS = sig 
  type 'a t = 'a Cmdliner.Arg.t
  val verbosity : Debug.Level.t t
  val color : bool t
  val unicode : bool t 
  val file_list : string list t
  val help : string option t 
end  