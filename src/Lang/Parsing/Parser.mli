(** {1 STELF Parser Combinators}

	The public parser module exposes the reusable combinators that STELF syntax
	will build on. The eventual grammar-specific productions belong in
	[Grammar]; this layer stays focused on parser state, repetition, and error
	reporting. *)

module type PARSER = PARSER.PARSER

module Parser : PARSER 