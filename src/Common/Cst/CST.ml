open Base

(** Concrete syntax tree interface used by the front-end parser. *)
module type CST = sig
  module Paths : Paths.Paths_intf.PATHS

  type query
  (** Query payload. *)

  type define
  (** Define payload. *)

  type solve
  (** Solve payload. *)

  type strexp
  (** Structure expression. *)

  type inst
  (** Structure instantiation clause. *)

  type sigexp
  (** Signature expression. *)

  type sigdef
  (** Signature definition. *)

  type structDec
  (** Structure declaration. *)

  type structDef
  (** Structure definition. *)

  type mode
  (** Mode marker. *)

  type modeDec
  (** Mode declaration. *)

  type modeTerm
  (** Mode term. *)

  type modeSpine
  (** Mode spine. *)

  type term
  (** Term node. *)

  type conDec
  (** Top-level constant declaration node. *)

  type decl
  (** Binder declaration node. *)

  type cmd
  (** Top-level command node. *)

  type loc
  (** Source location carried by CST nodes. *)

  type name = string
  (** Unqualified identifier. *)

  type namespace = string list
  (** Qualified namespace path. *)

  type symbol = namespace * name
  (** Qualified symbol as [(namespace, name)]. *)

  val mk_loc : int -> int -> loc
  (** Create a location from start and end lexer positions. *)
 
  val ghost : loc
  (** Synthetic location used for generated nodes. *)

  (** Term constructors. *)
  module Term : sig
    type nonrec t = term

    val lowercase : ?fc:loc -> symbol -> term
    (** Lowercase identifier (does not start with [_]). *)

    val uppercase : ?fc:loc -> symbol -> term
    (** Uppercase identifier. *)

    val qualified : ?fc:loc -> symbol -> term
    (** Qualified identifier. *)

    val text : ?fc:loc -> string -> term
    (** Quoted text literal (currently not parsed from source). *)

    val exist_var : ?fc:loc -> string -> term
    (** Existential variable, usually written as [?x]. *)

    val free_var : ?fc:loc -> string -> term
    (** Free variable identifier. *)

    (** Derived surface-level constructors. *)
    module Sugar : sig
      (** Function type constructor (not used directly). *)
      val arrow : ?fc:loc -> term -> term -> term
      (** This isn't used *)

      (* tm -> tm *)
      val backarrow : ?fc:loc -> term -> term -> term
      (** this isnt used *)
    end

    val pi : ?fc:loc -> decl list -> term -> term
    (** Dependent product over a list of declarations. *)

    val lam : ?fc:loc -> decl list -> term -> term
    (** Lambda abstraction over a list of declarations. *)

    val app : ?fc:loc -> term -> term list -> term
    (** Application of a head term to arguments. *)

    val has_type : ?fc:loc -> term -> term -> term
    (** Explicit type ascription. *)

    val omitted : ?fc:loc -> term
    (** Placeholder for an omitted term. *)
  end

  (** Binder declaration constructors. *)
  module Decl : sig
    type nonrec t = decl

    val decl1 : ?fc:loc -> string option list -> Term.t -> decl
    (** [decl1 names typ] creates a declaration that binds [names] with type
        [typ].

        The [names] list corresponds to grouped declarations such as
        [(x y z) T]. *)

    val decl0 : ?fc:loc -> string option list -> decl
    (** [decl0 names] is like {!decl1} but without an explicit type. *)
  end

  (** Top-level declaration constructors. *)
  module ConDec : sig
    type nonrec t = conDec

    val constant_decl : ?fc:loc -> Decl.t -> t
    (** Lift a local declaration into a top-level [%term] declaration. *)

    val block_decl : ?fc:loc -> string -> Decl.t list -> Decl.t list -> t
    (** Block declaration.

        [%block B X Y] declares block [B] with declaration groups [X] and [Y].
    *)

    val block_def : ?fc:loc -> string -> symbol list -> t
    val constant_def : ?fc:loc -> string -> Term.t -> Term.t option -> t
  end

  (** Mode syntax constructors. *)
  module Mode : sig
    type mode

    val plus : ?fc:loc -> unit -> mode
    (** Positive mode marker. *)

    val star : ?fc:loc -> unit -> mode
    (** Star mode marker. *)

    val minus : ?fc:loc -> unit -> mode
    (** Negative mode marker. *)

    val minus1 : ?fc:loc -> unit -> mode
    (** Strict negative mode marker. *)

    type modedec

    (** Short mode syntax. *)
    module Short : sig
      type term = modeTerm
      type spine = modeSpine

      val mode_nil : ?fc:loc -> unit -> spine
      (** Empty mode spine. *)

      val mode_app : ?fc:loc -> mode * string option -> spine -> spine
      (** Extend a mode spine with one argument mode. *)

      val mode_root : ?fc:loc -> symbol -> spine -> term
      (** Build a short mode root from a symbol and spine. *)

      val to_modeDec : ?fc:loc -> term -> modeDec
      (** Convert a short mode term into a mode declaration. *)
    end

    (** Full mode syntax. *)
    module Full : sig
      val mode_root : ?fc:loc -> Term.t -> term
      (** Root mode term from a regular term. *)

      val mode_pi : ?fc:loc -> mode -> Decl.t -> term -> term
      (** Pi-mode binder. *)

      val to_modeDec : ?fc:loc -> term -> modeDec
      (** Convert a full mode term into a mode declaration. *)
    end
  end

  (** Module/signature syntax constructors. *)
  module Struct : sig
    type strexp

    val str_exp : ?fc:loc -> symbol -> strexp

    type inst

    val con_inst : ?fc:loc -> symbol * loc -> Term.t -> inst
    val str_inst : ?fc:loc -> symbol * loc -> strexp -> inst
    val str_inst : ?fc:loc -> symbol * loc -> strexp -> inst

    type sigexp

    val thesig : ?fc:loc -> sigexp
    val sig_id : ?fc:loc -> string -> sigexp
    val where_sig : ?fc:loc -> sigexp -> inst list -> sigexp

    type sigdef

    val sig_def : ?fc:loc -> string option -> sigexp -> sigdef

    type structdec

    val struct_decl : ?fc:loc -> string option -> sigexp -> structdec
    val struct_def : ?fc:loc -> string option -> strexp -> structdec
  end

  module Query : sig
    type query

    val query : ?fc:loc -> string option -> Term.t -> query
    (** Query declaration. *)

    type define
    (** Define declaration. *)

    val define : ?fc:loc -> string option -> Term.t -> Term.t option -> define
    (** Define declaration with optional right-hand side. *)

    type solve

    val solve : ?fc:loc -> string option -> Term.t -> solve
    (** Solve declaration. *)
  end

  (** Top-level command constructors. *)
  module Cmd : sig
    val query : ?fc:loc -> Query.query -> cmd
    (** Query command. *)

    val define : ?fc:loc -> Query.define -> cmd
    (** Define command.

        This corresponds to [%define t x y], as a surface variation of
        [x : t = y]. *)

    val solve : ?fc:loc -> Query.solve -> cmd
    (** Solve command. *)

    val sort : ?fc:loc -> decl list -> cmd
    (** Sort command.

        [%sort A {X1 A1} ... {Xn An}] corresponds internally to
        [A : {X1 : A1} ... {Xn : An} type]. *)

    val term : ?fc:loc -> decl -> cmd
    (** Term command.

        Binds one or more names to a type (surface form [%term ...]). *)

    val stop : ?fc:loc -> unit -> cmd
    (** Stop command.

        It either returns control to outer mode or terminates a REPL command,
        similar to [;;] in OCaml. *)

    (** REPL-specific commands. *)
    module Repl : sig
      val quit : ?fc:loc -> unit -> cmd
      (** Quit the REPL. *)

      val help : ?fc:loc -> string option -> cmd
      (** Show help, optionally for one topic. *)

      val get : ?fc:loc -> string -> cmd
      (** Get the value of a setting. *)

      val set : ?fc:loc -> string -> string -> cmd
      (** Set a named setting. *)

      val version : ?fc:loc -> unit -> cmd
      (** Print version information. *)
    end
  end

  module Thm : sig 

  (*! structure Paths : PATHS  !*)
  type order

  val varg : Paths.region * string list -> order
  val lex : Paths.region * order list -> order
  val simul : Paths.region * order list -> order

  type callpats

  val callpats : (string * string option list * Paths.region) list -> callpats

  type tdecl

  val tdecl : order * callpats -> tdecl

  (* -bp *)
  type predicate

  val predicate : string * Paths.region -> predicate

  (* -bp *)
  type rdecl

  val rdecl : predicate * order * order * callpats -> rdecl

  type tableddecl

  val tableddecl : string * Paths.region -> tableddecl

  type keepTabledecl

  val keepTabledecl : string * Paths.region -> keepTabledecl

  type prove

  val prove : int * tdecl -> prove

  type establish

  val establish : int * tdecl -> establish

  type assert_

  val assert_ : callpats -> assert_

  type decs
  type theorem
  type theoremdec

  val null : decs
  val decl : decs * decl -> decs
  val top : theorem
  val exists : decs * theorem -> theorem
  val forall : decs * theorem -> theorem
  val forallStar : decs * theorem -> theorem
  val forallG : (decs * decs) list * theorem -> theorem
  val dec : string * theorem -> theoremdec

  (* world checker *)
  type wdecl

  val wdecl : (string list * string) list * callpats -> wdecl
  end 
end
