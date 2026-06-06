open Base
(** Lens based interface to CST

    Rather than having just one CST, STELF has a view based interface for the
    CST, allowing for new concrete syntax to be used without replacing any code
*)

module type S = sig
  type t
  type u

  val view : t -> u
  (** Unwrap "one layer" of the interface (ie, destruct) *)

  val review : u -> t
  (** Wrap "one layer" of the interface (ie, construct) *)
end

(** A lens is a pair of types, [t] and [u], with operations to convert between
    them In general, the following things should be true
    - [view] should be a left inverse to [review], i.e. [view (review x) = x]
      for all [x : u] (but they need not be right inverses)
    - [t] {e should not} be exposed
    - [u] {e should} be exposed
    - [t] should not mention [u]
    - [u] {e should} reference [t]
    - [u] should not reference itself

    These operations combined allow us to acheive lenses (actually, [u] should
    be complete and thus actually like a prism)

    In general, this should be used as [LENS with type t := t and type u := u]
*)
module type LENS = sig
  type t
  (** The concrete type we abstract over, should not be exposed *)

  type u
  (** The abstract type we almost always {e must} expose*)

  (** @inline *)
  include S with type t := t and type u := u

  val ( !> ) : t -> u
  (** Infix version of [view] *)

  val ( !< ) : u -> t
  (** Infix version of [review] *)
end

(** {2 LENS} *)

module type VIEW = sig
  exception Lacking

  module Paths : Paths.Paths_intf.PATHS
  (** Module of paths and regions, which we allow to be shared *)

  (** Abstract syntax tree type. (for internals) *)

  type loc
  (** Source Loc.tation carried by CST nodes. *)

  type name = string
  (** Unqualified identifier. *)

  type namespace = string list
  (** Qualified namespace path. *)

  type symbol = namespace * name
  (** Qualified symbol as [(namespace, name)]. *)

  val mk_loc : int -> int -> loc
  (** Create a Loc.tation from start and end lexer positions. *)

  val loc_to_region : loc -> Paths.region
  (** Convert a source Loc.tation to a Paths region. *)

  val ghost : loc
  (** Synthetic Loc.tation used for generated nodes. *)

  module Loc : sig
    type t

    type u =
      | Loc of Fpath.t option * int * int
          (** A actual location, with path, start, end*)
      | Ghost  (** Synthetic location used for generated nodes *)

    include LENS with type t := t and type u := u
  end

  (** {3 Term Syntax} *)
  module rec Term : sig
    type t

    type u =
      | Lowercase of Loc.t * symbol  (** An lowercase identifier in scope *)
      | Uppercase of Loc.t * symbol  (** An uppercase identifier in scope *)
      | Qualified of Loc.t * symbol
          (** A explicitly qualified identifier (eg, [%val (x y)])*)
      | Text of Loc.t * string  (** A literal string (currently unused) *)
      | ExistVar of Loc.t * string
          (** An explicitly existential variable, you're probably looking for
              {!Uppercase} *)
      | FreeVar of Loc.t * string
          (** A free variable, you're probably looking for {!Lowercase} *)
      | Pi of Loc.t * Decl.t list * t
          (** A {m \Pi} type, which takes in a number of possible contexts*)
      | Lam of Loc.t * Decl.t list * t
      | App of Loc.t * t * t list
      | HasType of Loc.t * t * t
      | Omitted of Loc.t
      | Typ of Loc.t
      | Arrow of Loc.t * t * t
      | BackArrow of Loc.t * t * t
      | Foreign of Loc.t * t
      | Internal of int

    include LENS with type t := t and type u := u
  end

  (** Binder declaration constructors. *)
  and Decl : sig
    type t

    type u =
      | Decl1 of Loc.t * string option list * Term.t * Term.t
      | Decl0 of Loc.t * string option list * Term.t

    include LENS with type t := t and type u := u
  end

  (** Top-level declaration constructors. *)
  module ConDec : sig
    type t

    type u =
      | ConstantDecl of Loc.t * Decl.t
      | BlockDecl of Loc.t * string * Decl.t list * Decl.t list
      | BlockDef of Loc.t * string * symbol list
      | ConstantDef of Loc.t * string * Term.t * Term.t option

    include LENS with type t := t and type u := u
  end

  (** Mode syntax constructors. *)
  module Mode : sig
    type t
    type u = Plus of Loc.t | Star of Loc.t | Minus of Loc.t | Minus1 of Loc.t

    include LENS with type t := t and type u := u

    type t_mode := t

    module Spine : sig
      type t

      type u =
        | ModeNil of Loc.t
        | ModeApp of Loc.t * (t_mode * string option) * t

      include LENS with type t := t and type u := u
    end

    module Term : sig
      type t

      type u =
        | ModeTerm of Loc.t * symbol * Spine.t
        | ModePi of Loc.t * Decl.t * t * t

      include LENS with type t := t and type u := u
    end

    module Dec : sig
      type t
      type u = ModeDec of Loc.t * (t_mode * string option) list * Term.t

      include LENS with type t := t and type u := u
    end
  end

  (** Module/signature syntax constructors. *)
  module Struct : sig
    module StrExp : sig
      type t
      type u = StrExp of Loc.t * symbol

      include LENS with type t := t and type u := u
    end

    module Inst : sig
      type t

      type u =
        | ConInst of Loc.t * symbol * Loc.t * Term.t
        | StrInst of Loc.t * symbol * Loc.t * StrExp.t

      include LENS with type t := t and type u := u
    end

    module SigExp : sig
      type t

      type u =
        | Thesig of Loc.t
        | SigId of Loc.t * string
        | WhereSig of Loc.t * t * Inst.t list

      include LENS with type t := t and type u := u
    end

    module SigDef : sig
      type t
      type u = SigDef of Loc.t * string option * SigExp.t

      include LENS with type t := t and type u := u
    end

    module StructDec : sig
      type t

      type u =
        | StructDecl of Loc.t * string option * SigExp.t
        | StructDef of Loc.t * string option * StrExp.t

      include LENS with type t := t and type u := u
    end
  end

  module Query : sig
    type t
    type u = Query of Loc.t * string option * Term.t

    include LENS with type t := t and type u := u
  end

  module Define : sig
    type t
    type u = Define of Loc.t * string option * Term.t * Term.t option

    include LENS with type t := t and type u := u
  end

  module Solve : sig
    type t
    type u = Solve of Loc.t * string option * Term.t

    include LENS with type t := t and type u := u
  end

  module Fixity : sig
    type t

    type u =
      | Left of Loc.t
      | Right of Loc.t
      | Prefix of Loc.t
      | Postfix of Loc.t
      | Middle of Loc.t
      | None of Loc.t

    include LENS with type t := t and type u := u
  end

  module BlockItem : sig
    type t
    type u = Any of Loc.t * Decl.t | All of Loc.t * Decl.t

    include LENS with type t := t and type u := u
  end

  module Thm : sig
    type t
    type u = |

    module Order : sig
      type t

      type u =
        | Varg of Loc.t * string list
        | Lex of Loc.t * t list
        | Simul of Loc.t * t list

      include LENS with type t := t and type u := u
    end

    module CallPats : sig
      type t
      type u = CallPats of (string * string option list * Loc.t) list

      include LENS with type t := t and type u := u
    end

    module TDecl : sig
      type t
      type u = TDecl of Order.t * CallPats.t

      include LENS with type t := t and type u := u
    end

    module Predicate : sig
      type t
      type u = Predicate of string * Loc.t

      include LENS with type t := t and type u := u
    end

    module RDecl : sig
      type t
      type u = RDecl of Predicate.t * Order.t * Order.t * CallPats.t

      include LENS with type t := t and type u := u
    end

    module TabledDecl : sig
      type t
      type u = TabledDecl of string * Loc.t

      include LENS with type t := t and type u := u
    end

    module KeepTableDecl : sig
      type t
      type u = KeepTableDecl of string * Loc.t

      include LENS with type t := t and type u := u
    end

    module Prove : sig
      type t
      type u = Prove of int * TDecl.t

      include LENS with type t := t and type u := u
    end

    module Establish : sig
      type t
      type u = Establish of int * TDecl.t

      include LENS with type t := t and type u := u
    end

    module Assert : sig
      type t
      type u = Assert of CallPats.t

      include LENS with type t := t and type u := u
    end

    module Decs : sig
      type t
      type u = DecsNil of Loc.t | DecsList of t * Decl.t list

      include LENS with type t := t and type u := u
    end

    module Thm : sig
      type t

      type u =
        | Top of Loc.t
        | Exists of Loc.t * Decs.t * t
        | Forall of Loc.t * Decs.t * t
        | ForallStar of Loc.t * Decs.t * t
        | ForallG of Loc.t * (Decs.t * Decs.t) list * t

      include LENS with type t := t and type u := u
    end

    include LENS with type t := t and type u := u

    module ThmDec : sig
      type t
      type u = ThmDec of string * Thm.t

      include LENS with type t := t and type u := u
    end

    module WDecl : sig
      type t
      type u = WDecl of (string list * string) list * CallPats.t

      include LENS with type t := t and type u := u
    end
  end

  module Cmd : sig
    type t

    type u =
      | Query of Loc.t * int option * int option * int option * Query.t
      | QueryTabled of Loc.t * int option * int option * int option * Query.t
      | AdhocQuery of Loc.t * Query.t
      | Unique of Loc.t * Term.t
      | Mode of Loc.t * Mode.Dec.t
      | Define of Loc.t * Define.t
      | DeclCmd of Loc.t * Term.t
      | Inline of Loc.t * string * Term.t
      | Symbol of Loc.t * string * string
      | Freeze of Loc.t * string list
      | Thaw of Loc.t * string list
      | Sort of Loc.t * string list * Decl.t list
      | Term of Loc.t * Decl.t
      | Block of Loc.t * string * BlockItem.t list
      | Union of Loc.t * string * string list
      | Worlds of Loc.t * string list * Term.t
      | Deterministic of Loc.t * string list
      | ModuleCmd of Loc.t * string * string list * t list
      | Use of Loc.t * string * string * string list
      | OpenCmd of Loc.t * string * string list
      | Eval of Loc.t * t list
      | Prec of Loc.t * Fixity.t * int * string list
      | Solve of Loc.t * Solve.t
      | Stop of Loc.t * unit
      | ReplQuit of Loc.t * unit
      | ReplHelp of Loc.t * string option
      | ReplGet of Loc.t * string
      | ReplSet of Loc.t * string * string
      | ReplVersion of Loc.t * unit
      | Total of Loc.t * Thm.Order.t list * Term.t list
      | Terminates of Loc.t * Thm.Order.t list * Term.t list
      | Covers of Loc.t * Mode.Dec.t
      | Name of Loc.t * string
      | Reduces of Loc.t * string * Term.t list

    include LENS with type t := t and type u := u
  end
end
