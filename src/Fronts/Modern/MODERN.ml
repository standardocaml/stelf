
module type MODERN = sig 
    module Paths : Paths.Paths_intf.PATHS
    module Cst : Cst.CST with module Paths = Paths
    module Names : Names.Names_intf.NAMES

    module Parser : Parser.PARSER
    type 'a t = 'a Parser.t
    exception ParseError of string
    val set_fixities : Names.namespace -> unit

    
    val parse_expr1 : unit -> Cst.Term.t t
    val parse_expr : unit -> Cst.Term.t t
    val parse_var : unit -> string t
    
    val parse_qualified : unit -> Cst.symbol t
    (** {v %val ( ... ) v} *)

    val parse_text : unit -> string t


    val parse_decl : unit -> Cst.Decl.t t
    val parse_mode : unit -> Cst.Mode.mode t
    val parse_mode_dec : unit -> Cst.Mode.modedec t
    val parse_sigexp : unit -> Cst.Struct.sigexp t
    val parse_inst : unit -> Cst.Struct.inst t
    val parse_sigexp : unit -> Cst.Struct.sigexp t
    val parse_sigdef : unit -> Cst.Struct.sigdef t
    val parse_struct_dec : unit -> Cst.Struct.structdec t
    val parse_fixity : unit -> int t
    val parse_query : unit -> Cst.query t 
    val parse_define : unit -> Cst.define t
    val parse_solve : unit -> Cst.solve t



    (* TODO Add rest of parsers *)

    val debug_parser : 'a t -> string -> 'a [@@alert debug "This should only be used in the REPL"]
    val run : 'a t -> Names.namespace ref -> Cst.loc -> string -> 'a

end 

