(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created       : 2009/09/07
 * Last modified : 2015/07/18
*****************************************************************)

{
open ModelParser

(* OCaml style comments *)
let comment_depth = ref 0;;

let line=ref 1;;

}

rule token = parse
	  ['\n']             { line := !line + 1 ; token lexbuf }     (* skip new lines *)
	| [' ' '\t']         { token lexbuf }     (* skip blanks *)
	| "--" [^'\n']* '\n' { line := !line + 1 ; token lexbuf }     (* skip Hytech-style comments *)

	(* C style comments *)
	| "/*"
		{ comment_depth := 1;
		comment_c lexbuf;
		token lexbuf }
	(* OCaml style comments *)
	| "(*"
		{ comment_depth := 1;
		comment_ocaml lexbuf;
		token lexbuf }

 	| "always"         { CT_ALWAYS }
	| "and"            { CT_AND }
	| "automaton"      { CT_AUTOMATON }
	| "bad"            { CT_BAD }
 	| "before"         { CT_BEFORE }
	| "carto"          { CT_CARTO }
	| "clock"          { CT_CLOCK }
	| "constant"       { CT_CONSTANT }
	| "discrete"       { CT_DISCRETE }
	| "do"             { CT_DO }
	| "end"            { CT_END }
 	| "eventually"     { CT_EVENTUALLY }
 	| "everytime"      { CT_EVERYTIME }
	| "False"          { CT_FALSE }
	| "goto"           { CT_GOTO }
 	| "happened"       { CT_HAPPENED }
 	| "has"            { CT_HAS }
	| "if"             { CT_IF }
	| "init"           { CT_INIT }
	| "initially"      { CT_INITIALLY }
	| "loc"            { CT_LOC }
	| "locations"      { CT_LOCATIONS }
	| "next"           { CT_NEXT }
	| "not"            { CT_NOT }
 	| "once"           { CT_ONCE }
	| "or"             { CT_OR }
	| "parameter"      { CT_PARAMETER }
 	| "projectresult"  { CT_PROJECTRESULT }
 	| "property"       { CT_PROPERTY }
	| "region"         { CT_REGION }
	| "sequence"       { CT_SEQUENCE }
	| "stop"           { CT_STOP }
	| "sync"           { CT_SYNC }
	| "synclabs"       { CT_SYNCLABS }
 	| "then"           { CT_THEN }
	| "True"           { CT_TRUE }
 	| "unreachable"    { CT_UNREACHABLE }
	| "var"            { CT_VAR }
	| "wait"           { CT_WAIT }
	| "when"           { CT_WHEN }
	| "while"          { CT_WHILE }
	| "within"         { CT_WITHIN }

	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
	| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT lxm } 
	| ['0'-'9']+ as lxm { INT(NumConst.numconst_of_string lxm) }
	| '"' [^'"']* '"' as lxm { STRING lxm } (* a string between double quotes *)

	| "<="             { OP_LEQ }
	| ">="             { OP_GEQ }
	| '<'              { OP_L }
	| '='              { OP_EQ }
	| '>'              { OP_G }
	| ":="             { OP_ASSIGN }

	| '+'              { OP_PLUS }
	| '-'              { OP_MINUS }
	| '*'              { OP_MUL }
	| '/'              { OP_DIV }

	| '('              { LPAREN }
	| ')'              { RPAREN }
	| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }

	| '&'              { AMPERSAND }
	| ','              { COMMA }
	| '\''             { APOSTROPHE }
	| '|'              { PIPE }
	| ':'              { COLON }
	| ';'              { SEMICOLON }

	| eof              { EOF}
	| _ { failwith("Unexpected symbol '" ^ (Lexing.lexeme lexbuf) ^ "' at line " ^ string_of_int !line)}



(* C style comments *)
and comment_c = parse
    "/*"  { incr comment_depth; comment_c lexbuf }
  | "*/"  { decr comment_depth;
            if !comment_depth == 0 then () else comment_c lexbuf }
  | eof
    { failwith "End of file inside a comment." }
	
  | '\n'  { line := !line + 1 ; comment_c lexbuf }
  | _     { comment_c lexbuf }
  
(* OCaml style comments *)
and comment_ocaml = parse
    "(*"  { incr comment_depth; comment_ocaml lexbuf }
  | "*)"  { decr comment_depth;
            if !comment_depth == 0 then () else comment_ocaml lexbuf }
  | eof
    { failwith "End of file inside a comment." }
  | '\n'  { line := !line + 1 ; comment_ocaml lexbuf }
  | _     { comment_ocaml lexbuf }
