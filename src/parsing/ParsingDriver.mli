(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Generic lexing/parsing driver for input files.
 *
 * File contributors : Ulrich Kühne, Étienne André, Jaime Arias
 * Created           : 2014/03/15
 *
 ************************************************************)

(** Generic lexing/parsing driver for input files. *)


(** Exception raised when the input file cannot be opened; the argument is a human-readable error message *)
exception InputFileNotFound of string

(** Exception raised when a lexing or parsing error occurs; the argument is a human-readable error message *)
exception ParsingFailure of string

(** Generic parsing function: takes a parser entry point, a lexer rule, a lexing buffer, a function returning the whole input as a string (used to build error messages), and the file name; returns the parsed structure.
    @raise ParsingFailure in case of lexing/parsing error *)
val parser_lexer_gen : ((Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'structure) -> (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> (unit -> string) -> string -> 'structure

(** Parse a file with the given parser entry point and lexer rule, and return the parsed structure.
    @raise InputFileNotFound if the file cannot be opened
    @raise ParsingFailure in case of lexing/parsing error *)
val parser_lexer_from_file : ((Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'structure) -> (Lexing.lexbuf -> 'token) -> string -> 'structure

(** Temporarily enable or disable the injected `end` token workaround for the temporary on-the-fly mode. *)
(* val set_force_included_file_terminator : bool -> unit

(** Decide whether the current file should receive a synthetic trailing `end` before the real EOF. *)
val file_needs_terminal_end : string -> bool

(** Wrap a model lexer so that a missing trailing `end` is injected before the real EOF for included models only. *)
val model_token_stream_adapter : (Lexing.lexbuf -> ModelParser.token) -> string -> (Lexing.lexbuf -> ModelParser.token) *)

(** Parse a model file and return its (unexpanded) parsing structure.
    @raise InputFileNotFound if the file cannot be opened
    @raise ParsingFailure in case of lexing/parsing error *)
val parse_model_from_file : string -> ParsingStructure.unexpanded_parsed_model

(** Parse a property file and return its (unexpanded) parsing structure.
    @raise InputFileNotFound if the file cannot be opened
    @raise ParsingFailure in case of lexing/parsing error *)
val parse_property_from_file : string -> ParsingStructure.unexpanded_parsed_property


(** Parse an update and return its (unexpanded) parsing structure.
    @raise InputFileNotFound if the file cannot be opened
    @raise ParsingFailure in case of lexing/parsing error *)
val parse_update_from_file : string -> ParsingStructure.unexpanded_parsed_location list