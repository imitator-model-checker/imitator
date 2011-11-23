(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created       : 2011/11/23
 * Last modified : 2011/11/23
*****************************************************************)

{
open GMLParser

(* Intricated comments *)
let comment_depth = ref 0;;

let line=ref 1;;

}

    
rule token = parse
	  ['\n']             { line := !line + 1 ; token lexbuf }     (* skip new lines *)
	| [' ' '\t']         { token lexbuf }     (* skip blanks *)
	| "--" [^'\n']* '\n' { line := !line + 1 ; token lexbuf }     (* skip Hytech-style comments *)

	(* OCaml style comments *)
	| "<!--"
		{ comment_depth := 1;
		comment_ocaml lexbuf;
		token lexbuf }

 	| "<?xml"		{ OPEN_XML } 	
	| "?>"			{ CLOSE_XML }

	| "<attribute"	{ OPEN_ATTRIBUTE }
 	| "<model"		{ OPEN_MODEL }
	| "<node"		{ OPEN_NODE }
 	
	| "</attribute"	{ OPEN_END_ATTRIBUTE }
	| "</model"		{ OPEN_END_MODEL }
	| "</node"		{ OPEN_END_NODE }
	
	| ">"			{ CLOSE }
	| "/>"			{ SINGLE_CLOSE }
	
	| "\"and\""				{ STR_AND }
	| "\"boolExpr\""		{ STR_BOOLEXPR }
	| "\"clocks\""			{ STR_CLOCKS }
	| "\"const\""			{ STR_CONST }
	| "\"constants\""		{ STR_CONSTANTS }
	| "\"discrete\""		{ STR_DISCRETE }
	| "\"finalState\""		{ STR_FINALSTATE }
	| "\"globalconstants\""	{ STR_GLOBALCONSTANTS }
	| "\"guard\""			{ STR_GUARD }
	| "\"initialState\""	{ STR_INITIALSTATE }
	| "\"invariant\""		{ STR_INVARIANT }
	| "\"name\""			{ STR_NAME }
	| "\"parameters\""		{ STR_PARAMETERS }
	| "\"state\""			{ STR_STATE }
	| "\"type\""			{ STR_TYPE }
	| "\"UTF-8\""			{ STR_UTF8 }
	| "\"variables\""		{ STR_VARIABLES }
	| "\"http://alligator.lip6.fr/timed-automata.fml\"" { STR_FORMALISM_URL }
	| "\"http://gml.lip6.fr/model\"" { STR_XMLNS }
	
	
	
	| '"' ['0'-'9']+ '"' as lxm { 
		(* Remove " " *)
		let digits = String.sub lxm 1 (String.length lxm - 2) in
		STR_INT(NumConst.numconst_of_string digits)
		}
	| '"' ['0'-'9']*'.'['0'-'9']+ '"' as lxm {
		(* Remove " " *)
		let digits = String.sub lxm 1 (String.length lxm - 2) in
		STR_FLOAT digits
		} 

	| "encoding"		{ CT_ENCODING}
	| "formalismUrl"	{ CT_FORMALISMURL}
	| "id"				{ CT_ID}
	| "name"			{ CT_NAME}
	| "nodeType"		{ CT_NODETYPE}
	| "version"			{ CT_VERSION}
	| "xmlns"			{ CT_XMLNS}

	| ['0'-'9']+ as lxm { INT(NumConst.numconst_of_string lxm) }

	| ['a'-'z''A'-'Z''_''0'-'9']+ as lxm { NAME lxm }
	| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT lxm } 

	| '='              { OP_EQ }

	| '+'              { OP_PLUS }
	| '-'              { OP_MINUS }
	| '*'              { OP_MUL }
	| '/'              { OP_DIV }

	| eof              { EOF}
	| _ { failwith("Unexpected symbol '" ^ (Lexing.lexeme lexbuf) ^ "' at line " ^ string_of_int !line)}



(* XML style comments (intricated) *)
and comment_ocaml = parse
    "<!--"  { incr comment_depth; comment_ocaml lexbuf }
  | "-->"  { decr comment_depth;
            if !comment_depth == 0 then () else comment_ocaml lexbuf }
  | eof
    { failwith "End of file inside a comment." }
  | '\n'  { line := !line + 1 ; comment_ocaml lexbuf }
  | _     { comment_ocaml lexbuf }
