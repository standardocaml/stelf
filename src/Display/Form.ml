module type FORM = FORM.FORM

module Form : FORM = struct
  type box = HBox | VBox | HVBox

  type t =
    | Space of int
    | NonbreakingSpace of int
    | Cut of int
    | Exact of string
    | Empty
    | Concat of t list
    | Fg of LTerm_style.color * t
    | Bg of LTerm_style.color * t
    | Bold of t
    | Italic of t
    | Underline of t
    | Marked of t * t  (** Ie, with carats *)
    | Boxed of box * t list  (** Box with style *)

  type style = t -> t
  type 'a scribe = 'a -> t

  let ( +++ ) x y = Concat [ x; y ]
  let empty = Empty

  let concat ?(sep = empty) xs =
    List.fold_left (fun acc x -> acc +++ sep +++ x) empty xs

  let string s = Exact s
  let int n = string (string_of_int n)
  let char c = string (String.make 1 c)
  let bool b = string (string_of_bool b)
  let cut () = string "\n"
  let shown f x = string (f x)
  let shown_exact f x = string (f x)
  let shown_many ?(sep = empty) f xs = concat ~sep (List.map (shown f) xs)
  let inside (open_, close) x = open_ +++ x +++ close
  let nl ?(n = 1) () = string (String.make n '\n')
  let each ?(sep = empty) f xs = concat ~sep (List.map f xs)
  let space ?(n = 1) () = Space n
  let non_breaking_space ?(n = 1) () = NonbreakingSpace n
  let hbox xs = Boxed (HBox, xs)
  let vbox xs = Boxed (VBox, xs)
  let hvbox xs = Boxed (HVBox, xs)
  let optional ?def f = function
    | None -> (match def with Some d -> d | None -> empty)
    | Some x -> f x

  let (++) x y = x +++ space () +++ y
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
      let orange x = Fg (LTerm_style.rgb 255 165 0, x)
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
      let orange x = Bg (LTerm_style.rgb 255 165 0, x)
    end
  end

  let style f x = f x
  let styles fs x = List.fold_left (fun acc f -> style f acc) x fs

  let markup' (x : t) : LTerm_text.markup =
    let rec aux : t -> LTerm_text.markup = function
      | Space n -> [ LTerm_text.S (String.make n ' ') ]
      | NonbreakingSpace n -> [ LTerm_text.S (String.make n ' ') ]
      | Cut n -> [ LTerm_text.S (String.make n '\n') ]
      | Exact s -> [ LTerm_text.S s ]
      | Empty -> []
      | Concat xs -> List.concat (List.map aux xs)
      | Fg (c, x) -> LTerm_text.([ B_fg c ] @ aux x @ [ E_fg ])
      | Bg (c, x) -> LTerm_text.([ B_bg c ] @ aux x @ [ E_bg ])
      | Bold x -> LTerm_text.([ B_bold true ] @ aux x @ [ E_bold ])
      | Italic x -> aux x (* Italic not supported by LTerm_text on all platforms; render plain *)
      | Underline x ->
          LTerm_text.([ B_underline true ] @ aux x @ [ E_underline ])
      | Marked (carats, x) -> aux carats @ aux x
      | Boxed (box, xs) -> (
          match box with
          | HBox -> List.concat (List.map aux xs)
          | VBox ->
              let rec intersperse sep = function
                | [] -> []
                | [ y ] -> aux y
                | y :: ys -> aux y @ (LTerm_text.S "\n" :: intersperse sep ys)
              in
              intersperse (LTerm_text.S "\n") xs
          | HVBox ->
              let rec intersperse_space = function
                | [] -> []
                | [ y ] -> aux y
                | y :: ys -> aux y @ (LTerm_text.S " " :: intersperse_space ys)
              in
              intersperse_space xs)
    in
    aux x

  let markup x = LTerm_text.eval (markup' x)
  let rec to_plain : t -> string = function
    | Space n | NonbreakingSpace n -> String.make n ' '
    | Cut n -> String.make n '\n'
    | Exact s -> s
    | Empty -> ""
    | Concat xs -> String.concat "" (List.map to_plain xs)
    | Fg (_, x) | Bg (_, x) | Bold x | Italic x | Underline x -> to_plain x
    | Marked (_carats, x) -> to_plain x
    | Boxed (box, xs) -> (
        match box with
        | HBox -> String.concat "" (List.map to_plain xs)
        | VBox -> String.concat "\n" (List.map to_plain xs)
        | HVBox -> String.concat " " (List.map to_plain xs))

  let fmt ppf x = Format.pp_print_string ppf (to_plain x)
end
