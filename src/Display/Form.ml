module type FORM = FORM.FORM 

module Form : FORM = struct 
  type t = 
  | Space of int
  | Break of int
  | Cut of int 
  | Exact of string 
  | Empty 
  | Concat of t list
  | Fg of LTerm_style.color * t
  | Bg of LTerm_style.color * t
  | Bold of t
  | Italic of t
  | Underline of t

  type style = t -> t
  type 'a scribe = 'a -> t
  let (++) x y = Concat [x; y]
  let empty = Empty
  let concat ?(sep = empty) xs = List.fold_left (fun acc x -> acc ++ sep ++ x) empty xs
  let string s = Exact s
  let int n = string (string_of_int n)
  let char c = string (String.make 1 c)
  let bool b = string (string_of_bool b)

  let cut () = string "\n"
  let shown f x = string (f x)
  let shown_exact f x = string (f x)
  let inside (open_, close) x = open_ ++ x ++ close
  let nl ?(n = 1) () = string (String.make n '\n')
  let each ?(sep = empty) f xs = concat ~sep (List.map f xs)
  let sp ?(n = 1) () = Space n

  module Style = struct
    let bold x = Bold x
    let italic x = Italic x
    let underline x = Underline x
    module Fore = struct 
      let black x = Fg (LTerm_style.black, x)
      let red x = Fg (LTerm_style.red, x)
      let green x = Fg (LTerm_style.green, x)
      let yellow x = Fg (LTerm_style.yellow, x)
      let blue x = Fg (LTerm_style.blue, x)
      let magenta x = Fg (LTerm_style.magenta, x)
      let cyan x = Fg (LTerm_style.cyan, x)
      let white x = Fg (LTerm_style.white, x)
    end 
    module Back = struct 
      let black x = Bg (LTerm_style.black, x)
      let red x = Bg (LTerm_style.red, x)
      let green x = Bg (LTerm_style.green, x)
      let yellow x = Bg (LTerm_style.yellow, x)
      let blue x = Bg (LTerm_style.blue, x)
      let magenta x = Bg (LTerm_style.magenta, x)
      let cyan x = Bg (LTerm_style.cyan, x)
      let white x = Bg (LTerm_style.white, x)
    end 
  end

  let style f x = f x
  let styles fs x = List.fold_left (fun acc f -> style f acc) x fs
  let markup' (x : t) : LTerm_text.markup = 
    let rec aux : t -> LTerm_text.markup = function 
    | Space n -> [LTerm_text.S (String.make n ' ')]
    | Break n -> [LTerm_text.S (String.make n '\n')]
    | Cut n -> [LTerm_text.S (String.make n '\n')]
    | Exact s -> [LTerm_text.S s]
    | Empty -> []
    | Concat xs -> List.concat (List.map aux xs)
    | Fg (c, x) -> LTerm_text.([B_fg c] @ aux x @ [E_fg])
    | Bg (c, x) -> LTerm_text.([B_bg c] @ aux x @ [E_bg])
    | Bold x -> LTerm_text.([B_bold true] @ aux x @ [E_bold])
    | Italic x -> assert false 
    | Underline x -> LTerm_text.([B_underline true] @ aux x @ [E_underline])
    in aux x
  let markup x = LTerm_text.eval (markup' x)
  let fmt : t Fmt.t = assert false
  
   
end 