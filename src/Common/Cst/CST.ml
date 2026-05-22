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

  val loc_to_region : loc -> Paths.region
  (** Convert a source location to a Paths region. *)

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
    type term = modeTerm

    val plus : ?fc:loc -> unit -> mode
    (** Positive mode marker. *)

    val star : ?fc:loc -> unit -> mode
    (** Star mode marker. *)

    val minus : ?fc:loc -> unit -> mode
    (** Negative mode marker. *)

    val minus1 : ?fc:loc -> unit -> mode
    (** Strict negative mode marker. *)

    type modedec = modeDec

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

    type sigexp

    val thesig : ?fc:loc -> sigexp
    val sig_id : ?fc:loc -> string -> sigexp
    val where_sig : ?fc:loc -> sigexp -> inst list -> sigexp

    type sigdef

    val sig_def : ?fc:loc -> string option -> sigexp -> sigdef

    type structdec = structDec

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

  val varg : loc * string list -> order
  val lex : loc * order list -> order
  val simul : loc * order list -> order

  type callpats

  val callpats : (string * string option list * loc) list -> callpats

  type tdecl

  val tdecl : order * callpats -> tdecl

  (* -bp *)
  type predicate

  val predicate : string * loc -> predicate

  (* -bp *)
  type rdecl

  val rdecl : predicate * order * order * callpats -> rdecl

  type tableddecl

  val tableddecl : string * loc -> tableddecl

  type keepTabledecl

  val keepTabledecl : string * loc -> keepTabledecl

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

  val show_term : term -> string
  (** Debug-print a term to a string. *)

  val pp_term : Stdlib.Format.formatter -> term -> unit
  (** Pretty-print a term to a formatter. *)

  (** Read-only deconstructors for opaque CST values. *)
  module View : sig
    val term_loc : term -> loc option

    val term_lcid : term -> symbol option
    val term_ucid : term -> symbol option
    val term_quid : term -> symbol option
    val term_scon : term -> string option
    val term_evar : term -> string option
    val term_fvar : term -> string option
    val term_typ : term -> bool
    val term_omitted : term -> bool

    val term_arrow : term -> (term * term) option
    val term_pi : term -> (decl * term) option
    val term_lam : term -> (decl * term) option
    val term_app : term -> (term * term) option
    val term_has_type : term -> (term * term) option

    val decl_fields : decl -> string option list * term * loc

    val condec_constant_decl : conDec -> decl option
    val condec_constant_def : conDec -> (string * term * term option) option
    val condec_block_decl : conDec -> (string * decl list * decl list) option
    val condec_block_def : conDec -> (string * symbol list) option

    val query_fields : query -> string option * term
    val define_fields : define -> string option * term * term option
    val solve_fields : solve -> string option * term

    val mode_view : mode -> [ `Plus | `Star | `Minus | `Minus1 ]
    val mode_short : modeDec -> (symbol * (mode * string option) list) option
    val mode_full : modeDec -> ((mode * string option) list * term) option

    val struct_strexp_symbol : strexp -> symbol option
    val struct_inst_con : inst -> (symbol * loc * term) option
    val struct_inst_str : inst -> (symbol * loc * strexp) option
    val struct_sigexp_id : sigexp -> string option
    val struct_sigexp_where : sigexp -> (sigexp * inst list) option
    val struct_sigdef_fields : sigdef -> string option * sigexp
    val struct_structdecl_decl : structDec -> (string option * sigexp) option
    val struct_structdecl_def : structDec -> (string option * strexp) option

    val thm_order_varg : Thm.order -> (loc * string list) option
    val thm_order_lex : Thm.order -> (loc * Thm.order list) option
    val thm_order_simul : Thm.order -> (loc * Thm.order list) option

    val thm_callpats : Thm.callpats -> (string * string option list * loc) list
    val thm_tdecl : Thm.tdecl -> Thm.order * Thm.callpats
    val thm_predicate : Thm.predicate -> string * loc
    val thm_rdecl : Thm.rdecl -> Thm.predicate * Thm.order * Thm.order * Thm.callpats
    val thm_tableddecl : Thm.tableddecl -> string * loc
    val thm_keepTabledecl : Thm.keepTabledecl -> string * loc
    val thm_prove : Thm.prove -> int * Thm.tdecl
    val thm_establish : Thm.establish -> int * Thm.tdecl
    val thm_assert : Thm.assert_ -> Thm.callpats

    val thm_theorem_top : Thm.theorem -> bool
    val thm_theorem_exists : Thm.theorem -> (Thm.decs * Thm.theorem) option
    val thm_theorem_forall : Thm.theorem -> (Thm.decs * Thm.theorem) option
    val thm_theorem_forallStar : Thm.theorem -> (Thm.decs * Thm.theorem) option
    val thm_theorem_forallG : Thm.theorem -> ((Thm.decs * Thm.decs) list * Thm.theorem) option

    val thm_decs_nil : Thm.decs
    val thm_decs_list : Thm.decs -> decl list
    val thm_theoremdec : Thm.theoremdec -> string * Thm.theorem
    val thm_wdecl : Thm.wdecl -> (string list * string) list * Thm.callpats
  end
end
