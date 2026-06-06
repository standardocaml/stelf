
open Base
(**
   The concrete syntax tree (CST) interface (in Twelf, it bore the name [ExtSyn]. 
   
   In both the original twelf and in STELF, the CST has two, distinct but related purposes.
   {ol
   {- To serve as the target for parsing. This is the primary purpose of this module, which outlines the abstract interface }
   {- To serve as the {e input} of elaboration (term reconstruction), for more on this, see {!module:Cst} and {!module:Recon} }.
   }

   We choose to implement these two purposes in the same way that Twelf did, albiet seperately (Twelf had this embeded in the elaboration).
   Thus, using only these, we should be able to {e create} any CST for any term that we may so desire.
   
   @author Asher Frost
   @see {!module:Cst} for the actual implementation of this interface.
*)

(** {2 CST} *)
module type CST = sig
  module Paths : Paths.Paths_intf.PATHS
  (** Module of paths and regions, which we allow to be shared *)


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

  type fixity
  (** Fixity kind for %prec declarations. *)

  type block_item
  (** One item in a %block world declaration. *)

  type order [@@deriving show { with_path = false }, eq]
  (** Termination/totality order (Varg, Lex, Simul). *)

  type cmd [@@deriving show { with_path = false }, eq]
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

  (** {3 Term Syntax} *)
  module Term : sig
    type t = term


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

    val pi : ?fc:loc -> decl list -> term -> term
    (** The pi type, which covers both kinds and types
        @param fc Optional source location for the node.
        @param decls
          List of bind declerations {!type:decl}, which introduces terms into
          the context
        @param body The body of the pi type *)

    val lam : ?fc:loc -> decl list -> term -> term
    (** Lambda abstraction over a list of declarations
        @param fc Optional source location for the node.
        @param decls
          List of bind declerations {!type:decl}, which introduces terms into
          the context
        @param body The body of the lambda *)

    val app : ?fc:loc -> term -> term list -> term
    (** Application of a head term to arguments, which applies both to terms in
        normal form and not in normal form *)

    val has_type : ?fc:loc -> term -> term -> term
    (** Explicit type ascription. *)

    val omitted : ?fc:loc -> term
    (** Placeholder [_] for an omitted term. *)

    val typ : ?fc:loc -> unit -> term
    (** Note that while this term does not exist externally, internally, we
        translate [%sort] to use this, as to be similar to the original Twelf *)

    (** {4 Syntax Sugar} *)
    module Sugar : sig
      (** Function type constructor (not used directly). *)
      val arrow : ?fc:loc -> term -> term -> term
      (** This isn't used *)

      (* tm -> tm *)
      val backarrow : ?fc:loc -> term -> term -> term
      (** this isnt used *)
    end
  end

  (** Binder declaration constructors. *)
  module Decl : sig
    type t = decl

    val decl1 : ?fc:loc -> string option list -> term -> decl
    (** [decl1 names typ] creates a declaration that binds [names] with type
        [typ].

        The [names] list corresponds to grouped declarations such as
        [(x y z) T]. *)

    val decl0 : ?fc:loc -> string option list -> decl
    (** [decl0 names] is like {!decl1} but without an explicit type. *)
  end

  (** Top-level declaration constructors. *)
  module ConDec : sig
    type t = conDec

    val constant_decl : ?fc:loc -> decl -> t
    (** Lift a local declaration into a top-level [%term] declaration. *)

    val block_decl : ?fc:loc -> string -> decl list -> decl list -> t
    (** Block declaration.

        [%block B X Y] declares block [B] with declaration groups [X] and [Y].
    *)

    val block_def : ?fc:loc -> string -> symbol list -> t
    val constant_def : ?fc:loc -> string -> term -> term option -> t
  end

  (** Mode syntax constructors. *)
  module Mode : sig
    type mode
    type nonrec modeTerm = modeTerm

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
      type nonrec modeTerm = modeTerm
      type nonrec modeSpine = modeSpine

      val mode_nil : ?fc:loc -> unit -> modeSpine
      (** Empty mode spine. *)

      val mode_app : ?fc:loc -> mode * string option -> modeSpine -> modeSpine
      (** Extend a mode spine with one argument mode. *)

      val mode_root : ?fc:loc -> symbol -> modeSpine -> modeTerm
      (** Build a short mode root from a symbol and spine. *)

      val to_modeDec : ?fc:loc -> modeTerm -> modeDec
      (** Convert a short mode term into a mode declaration. *)
    end

    (** Full mode syntax. *)
    module Full : sig
      val mode_root : ?fc:loc -> term -> modeTerm
      (** Root mode term from a regular term. *)

      val mode_pi : ?fc:loc -> mode -> decl -> modeTerm -> modeTerm
      (** Pi-mode binder. *)

      val to_modeDec : ?fc:loc -> modeTerm -> modeDec
      (** Convert a full mode term into a mode declaration. *)
    end
  end

  (** Module/signature syntax constructors. *)
  module Struct : sig
    type strexp

    val str_exp : ?fc:loc -> symbol -> strexp

    type inst

    val con_inst : ?fc:loc -> symbol * loc -> term -> inst
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

    val query : ?fc:loc -> string option -> term -> query
    (** Query declaration. *)

    type define
    (** Define declaration. *)

    val define : ?fc:loc -> string option -> term -> term option -> define
    (** Define declaration with optional right-hand side. *)

    type solve

    val solve : ?fc:loc -> string option -> term -> solve
    (** Solve declaration. *)
  end

  (** Fixity constructors. *)
  module Fixity : sig
    val left : fixity
    val right : fixity
    val prefix : fixity
    val postfix : fixity
    val middle : fixity
    val none : fixity
  end

  (** Block item constructors for %block declarations. *)
  module BlockItem : sig
    val some : decl -> block_item
    (** [{decl}] — existentially bound hypothesis. *)

    val pi : decl -> block_item
    (** [[decl]] — universally bound hypothesis. *)
  end

  (** Top-level command constructors. *)
  module Cmd : sig
    val query :
      ?fc:loc ->
      n:int option ->
      b:int option ->
      d:int option ->
      Query.query ->
      cmd
    (** [%query n b d expr] — logic programming query with bounds. *)

    val query_tabled :
      ?fc:loc ->
      n:int option ->
      b:int option ->
      d:int option ->
      Query.query ->
      cmd
    (** [%querytabled n b d expr] — tabled query with bounds. *)

    val adhoc_query : ?fc:loc -> Query.query -> cmd
    (** [%? expr] — ad-hoc REPL query. *)

    val unique : ?fc:loc -> term -> cmd
    (** [%unique expr] — assert expr has at most one inhabitant. *)

    val mode : ?fc:loc -> modeDec -> cmd
    (** [%mode hyps] — declare input/output polarity. *)

    val define : ?fc:loc -> Query.define -> cmd
    (** [%define id expr] — transparent definition. *)

    val decl_cmd : ?fc:loc -> term -> cmd
    (** [%decl expr] — raw elaboration-level declaration. *)

    val inline : ?fc:loc -> string -> term -> cmd
    (** [%inline id expr] — always-unfolded definition. *)

    val symbol : ?fc:loc -> string -> string -> cmd
    (** [%symbol id id] — associate a symbolic name. *)

    val freeze : ?fc:loc -> string list -> cmd
    (** [%freeze id_list] — freeze type families. *)

    val thaw : ?fc:loc -> string list -> cmd
    (** [%thaw id_list] — unfreeze type families. *)

    val sort : ?fc:loc -> string list -> decl list -> cmd
    (** [%sort id {decl}+] — declare a type family. *)

    val term : ?fc:loc -> decl -> cmd
    (** [%term decl] — declare a term-level constant. *)

    val block : ?fc:loc -> string -> block_item list -> cmd
    (** [%block id block_item*] — define a named context schema. *)

    val union : ?fc:loc -> string -> string list -> cmd
    (** [%union id ids] — union of block labels. *)

    val worlds : ?fc:loc -> string list -> term -> cmd
    (** [%worlds ids expr] — assert expr lives in the named world. *)

    val deterministic : ?fc:loc -> string list -> cmd
    (** [%deterministic id_list] — mark type families as deterministic. *)

    val module_cmd : ?fc:loc -> string -> string list -> cmd list -> cmd
    (** [%module id params body] — declare a parameterised module. *)

    val use : ?fc:loc -> string -> string -> string list -> cmd
    (** [%use id id iparams] — instantiate a module. *)

    val open_cmd : ?fc:loc -> string -> string list -> cmd
    (** [%open id id_list] — bring names from a module into scope. *)

    val eval : ?fc:loc -> cmd list -> cmd
    (** [%eval %{ cmds %}] — evaluate a command block. *)

    val prec : ?fc:loc -> fixity -> int -> string list -> cmd
    (** [%prec fixity n id_list] — set operator fixity and precedence. *)

    val solve : ?fc:loc -> Query.solve -> cmd
    (** [%solve] — solve command. *)

    val stop : ?fc:loc -> unit -> cmd
    (** [%.] — end-of-command marker. *)

    (** REPL-specific commands. *)
    module Repl : sig
      val quit : ?fc:loc -> unit -> cmd
      val help : ?fc:loc -> string option -> cmd
      val get : ?fc:loc -> string -> cmd
      val set : ?fc:loc -> string -> string -> cmd
      val version : ?fc:loc -> unit -> cmd
    end

    val total : ?fc:loc -> order list -> term list -> cmd
    (** [%total hyps modes] — declare a totality check. *)

    val terminates : ?fc:loc -> order list -> term list -> cmd
    (** [%terminates hyps modes] — declare a termination check. *)
    
    val covers : ?fc:loc -> modeDec -> cmd
    (** [%covers hyps modes] — declare a coverage check. *)
    
    val name : ?fc:loc -> string -> cmd
    (** [%name id] — declare a name for the next definition. *)

    val reduces : ?fc:loc -> string -> term list -> cmd
    (** [%reduces pred order_out order_in call_pats] — declare a reduction relation. *)

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
  
  (** {2 Views} *)

  (** Views should eventually supplant the rest of this module *)
  module View : sig 
    include LENS.VIEW
    with type Term.t = term
    and type Decl.t = decl
    and type ConDec.t = conDec
    and type Mode.t = mode
    and type Mode.Term.t = modeTerm
    and type Mode.Dec.t = modeDec
    and type Struct.StrExp.t = strexp
    and type Struct.Inst.t = inst
    and type Struct.SigExp.t = sigexp
    and type Struct.SigDef.t = sigdef
    and type Struct.StructDec.t = structDec 
    and type Query.t = query
    and type Solve.t = solve
    and type Define.t = define
    and type Fixity.t = fixity
    and type Cmd.t = cmd


  end 
  
 
end
   
