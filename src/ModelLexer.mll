(*****************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, LORIA, CNRS, France
 *
 * Author:        Étienne André
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2009/09/07
 * Last modified     : 2020/01/31
*****************************************************************)

{
open ModelParser
open Lexing

(* OCaml style comments *)
let comment_depth = ref 0;;

let line=ref 1;;

}

rule token = parse
	  ['\n']             { line := !line + 1 ; token lexbuf }     (* skip new lines *)
	| [' ' '\t']         { token lexbuf }     (* skip blanks *)

	(* C style include *)
	| "#include \""   ( [^'"' '\n']* as filename) '"'
    {
			let top_file = lexbuf.lex_start_p.pos_fname in
			let absolute_filename = FilePath.make_absolute (FilePath.dirname top_file) filename in

			let c = open_in absolute_filename in
			let lb = Lexing.from_channel c in
			lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = absolute_filename };

			let p : ParsingStructure.parsed_model = ModelParser.main token lb in
			INCLUDE p
    }

	(* OCaml style comments *)
	| "(*"
		{ comment_depth := 1;
		comment_ocaml lexbuf;
		token lexbuf }

	(* NOTE: just to forbid their use in the input model *)
 	| "automatically_generated_observer"       { CT_OBSERVER } (* to forbid this keyword, potentially used in the observer *)
 	| "automatically_generated_x_obs"       { CT_OBSERVER_CLOCK } (* to forbid this keyword, potentially used in the observer *)
 	| "special_0_clock" {CT_SPECIAL_RESET_CLOCK_NAME} (* to forbid this keyword, used when a special reset clock is defined *)

 	(* All keywords *)
 	| "accepting"      { CT_ACCEPTING }
	| "always"         { CT_ALWAYS }
	| "and"            { CT_AND }
	| "automaton"      { CT_AUTOMATON }
	| "bad"            { CT_BAD }
 	| "before"         { CT_BEFORE }
	| "boolean"        { CT_BOOLEAN }
	| "clock"          { CT_CLOCK }
	| "constant"       { CT_CONSTANT }
	| "do"             { CT_DO }
	| "else"           { CT_ELSE }
	| "end"            { CT_END }
 	| "eventually"     { CT_EVENTUALLY }
 	| "everytime"      { CT_EVERYTIME }
	| "False"          { CT_FALSE }
	| "goto"           { CT_GOTO }
 	| "happened"       { CT_HAPPENED }
 	| "has"            { CT_HAS }
	| "if"             { CT_IF }
	| "in"             { CT_IN }
	| "init"           { CT_INIT }
	| "initially"      { CT_INITIALLY }
	| "invariant"      { CT_INVARIANT }
	| "loc"            { CT_LOC }
	| "locations"      { CT_LOCATIONS }
	| "next"           { CT_NEXT }
	| "not"            { CT_NOT }
 	| "once"           { CT_ONCE }
	| "or"             { CT_OR }
	| "parameter"      { CT_PARAMETER }
	| "rational"       { CT_RATIONAL }
	| "region"         { CT_REGION }
	| "sequence"       { CT_SEQUENCE }
	| "stop"           { CT_STOP }
	| "string"         { CT_STRING }
	| "stringset"      { CT_STRINGSET }
	| "sync"           { CT_SYNC }
	| "synclabs"       { CT_SYNCLABS }
 	| "then"           { CT_THEN }
	| "True"           { CT_TRUE }
 	| "urgent"         { CT_URGENT }
	| "var"            { CT_VAR }
	| "wait"           { CT_WAIT }
	| "when"           { CT_WHEN }
	| "while"          { CT_WHILE }
	| "within"         { CT_WITHIN }



	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
	| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT lxm }
	| ['0'-'9']+ as lxm { INT(NumConst.numconst_of_string lxm) }
(*	| '"' [^'"']* '"' as lxm { STRING lxm } *) (* a string between double quotes *)

	| "<="             { OP_LEQ }
	| ">="             { OP_GEQ }
	| '<'              { OP_L }
	| '='              { OP_EQ }
	| "<>"             { OP_NEQ }
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
	| ".."             { DOUBLEDOT }
	| ','              { COMMA }
	| '\''             { APOSTROPHE }
	| '|'              { PIPE }
	| ':'              { COLON }
	| ';'              { SEMICOLON }

	| eof              { EOF}
	| _ { failwith("Unexpected symbol '" ^ (Lexing.lexeme lexbuf) ^ "' at line " ^ string_of_int !line)}



(* OCaml style comments *)
and comment_ocaml = parse
    "(*"  { incr comment_depth; comment_ocaml lexbuf }
  | "*)"  { decr comment_depth;
            if !comment_depth == 0 then () else comment_ocaml lexbuf }
  | eof
    { failwith "End of file inside a comment in model." }
  | '\n'  { line := !line + 1 ; comment_ocaml lexbuf }
  | _     { comment_ocaml lexbuf }
