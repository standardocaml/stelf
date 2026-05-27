type src =
  | Approx
  | Check
  | Compile
  | Typecheck
  | Unify
  | Cover
  | Parse
  | Reduce
  | Meta
  | Pal
  | Default

type kind = Debug | Info | Warning | Error | Response

(* Corresponds to chatter 5 4 3 2 1 0, respectively *)
type level = VeryVerbose | Verbose | Normal | Quiet | VeryQuiet | Silent

let from_chatter x =
  assert (x >= 0);
  match x with
  | 0 -> Silent
  | 1 -> VeryQuiet
  | 2 -> Quiet
  | 3 -> Normal
  | 4 -> Verbose
  | _ -> VeryVerbose

module Form = Form.Form

type form = Form.t
type t = { src : src option; kind : kind option; level : level; msg : form }

let msg ?(src : src option) ?(kind : kind option) ?(level = Normal) (fmt : form)
    : t =
  { src; kind; level; msg = fmt }

let to_int : level -> int = function
  | Silent -> 0
  | VeryQuiet -> 1
  | Quiet -> 2
  | Normal -> 3
  | Verbose -> 4
  | VeryVerbose -> 5

let ( >= ) x y = to_int x >= to_int y
let ( > ) x y = to_int x > to_int y
let ( =< ) x y = to_int x <= to_int y
let ( < ) x y = to_int x < to_int y
