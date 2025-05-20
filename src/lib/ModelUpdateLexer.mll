(*****************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Parser for on-the-fly model updates
 *
 * File contributors : Étienne André
 * Created           : 2025/05/20
 *****************************************************************)

{
open Lexing
open Exceptions
(* open ImitatorUtilities *)
open ModelUpdateParser

(* OCaml style comments *)
let comment_depth = ref 0;;

let line=ref 1;;

}

(* Support Windows CRLF line endings *)
let newline = '\r' | '\n' | "\r\n"

rule token = parse
	  newline             { line := !line + 1 ; token lexbuf }     (* skip new lines *)
	| [' ' '\t']         { token lexbuf }     (* skip blanks *)


 	(* All keywords *)
 	| "newlocation"    { CT_NEWLOCATION }

	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
(*
	| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT lxm }
*)

	(*(* Comparison operators *)
	| "<="             { OP_LEQ }
	| ">="             { OP_GEQ }
	| '<'              { OP_L }
	| '='              { OP_EQ }
	| "<>"             { OP_NEQ }
	| '>'              { OP_G }

	(* Assignment *)
	| ":="             { OP_ASSIGN }
	| "<-"             { OP_ASSIGN }

	(* Boolean operators *)
	| '&'              { OP_CONJUNCTION }
	| "&&"             { OP_CONJUNCTION }
	| '|'              { OP_DISJUNCTION }
	| "||"             { OP_DISJUNCTION }
	| "=>"             { OP_IMPLIES }

	(* Arithmetic operators *)
	| '+'              { OP_PLUS }
	| '-'              { OP_MINUS }
	| '*'              { OP_MUL }
	| '/'              { OP_DIV }

	(* Parentheses and the like *)
	*)
	| '('              { LPAREN }
	| ')'              { RPAREN }

	(*| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }

	| '\''             { APOSTROPHE }
	| ':'              { COLON }
	| ','              { COMMA }
(* 	| ".."             { DOUBLEDOT } *)
*)
	| ';'              { SEMICOLON }

	| eof              { EOF}
	| _ { failwith("Unexpected symbol '" ^ (Lexing.lexeme lexbuf) ^ "' at line " ^ string_of_int !line)}

