let rec help : string Fmt.t = fun fmt s -> help' s fmt ()

and help' : string -> unit Fmt.t = function
  | "" -> assert false
  | "help" | "%help" -> assert false
  | "sort" | "%sort" -> assert false
  | "expr" -> assert false
