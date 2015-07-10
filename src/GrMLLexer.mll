(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created       : 2011/11/23
 * Last modified : 2015/07/10
*****************************************************************)

{
open GrMLParser

(* Intricated comments *)
let comment_depth = ref 0;;

let line=ref 1;;

}

    
rule token = parse
	  ['\n']             { line := !line + 1 ; token lexbuf }     (* skip new lines *)
	| [' ' '\t']         { token lexbuf }     (* skip blanks *)
	| "--" [^'\n']* '\n' { line := !line + 1 ; token lexbuf }     (* skip Hytech-style comments *)

	(* Nested style comments *)
	| "<!--"
		{ comment_depth := 1;
		comment_ocaml lexbuf;
		token lexbuf }

(* 	| "<?xml"		{ OPEN_XML } 	
	| "?>"			{ CLOSE_XML }*)
 	| "<?xml" [^ '?']+ "?>"			{ XML_HEADER } (*WARNING: il faudrait plutot specifier "touf sauf '?>' " *)

 	| "<model" [^ '>']+ ">"						{ MODEL_HEADER }
 	| "<ns" ['0'-'9']+ ":model" [^ '>']+ ">"	{ MODEL_HEADER }

 	| "<arc"		{ OPEN_ARC }
	| "<attribute"	{ OPEN_ATTRIBUTE }
	| "<node"		{ OPEN_NODE }

	(* WARNING: un peu moche *)
	| "<ns" ['0'-'9']+ ":arc"			{ OPEN_ARC }
	| "<ns" ['0'-'9']+ ":attribute"	{ OPEN_ATTRIBUTE }
	| "<ns" ['0'-'9']+ ":node"			{ OPEN_NODE }
 	
	| "</arc"		{ OPEN_END_ARC }
	| "</attribute"	{ OPEN_END_ATTRIBUTE }
	| "</model"		{ OPEN_END_MODEL }
	| "</node"		{ OPEN_END_NODE }
	
	| "</ns" ['0'-'9']+ ":arc"		{ OPEN_END_ARC }
	| "</ns" ['0'-'9']+ ":attribute"	{ OPEN_END_ATTRIBUTE }
	| "</ns" ['0'-'9']+ ":model"		{ OPEN_END_MODEL }
	| "</ns" ['0'-'9']+ ":node"		{ OPEN_END_NODE }



	
	| ">"			{ CLOSE }
	| "/>"			{ SINGLE_CLOSE }
	
	| "initial"	    { INITIAL }
	| "final"	    { FINAL }
	| "Normal"	    { NORMAL }
	| "true"		{ TRUE }

	| "\"and\""				{ STR_AND }
	| "\"boolExpr\""		{ STR_BOOLEXPR }
	| "\"boolValue\""		{ STR_BOOLVALUE }
	| "\"clock\""			{ STR_CLOCK }
	| "\"clocks\""			{ STR_CLOCKS }
	| "\"numValue\""			{ STR_CONST }
	| "\"constants\""		{ STR_CONSTANTS }
	| "\"declaration\""		{ STR_DECLARATION }
	| "\"discrete\""		{ STR_DISCRETE }
	| "\"discretes\""		{ STR_DISCRETES }
	| "\"expr\""			{ STR_EXPR }
	| "\"globalconstants\""	{ STR_GLOBALCONSTANTS }
	| "\"guard\""			{ STR_GUARD }
	| "\"initialConstraint\"" { STR_INITIALCONSTRAINT }
	| "\"invariant\""		{ STR_INVARIANT }
	| "\"label\""			{ STR_LABEL }
	| "\"name\""			{ STR_NAME }
	| "\"parameter\""		{ STR_PARAMETER }
	| "\"parameters\""		{ STR_PARAMETERS }
	| "\"state\""			{ STR_STATE }
	| "\"type\""			{ STR_TYPE }
	| "\"Transition\""		{ STR_TRANSITION }
	| "\"update\""			{ STR_UPDATE }
	| "\"updates\""			{ STR_UPDATES }
(* 	| "\"UTF-8\""			{ STR_UTF8 } *)
	| "\"variables\""		{ STR_VARIABLES }

	| "\"less\""			{ STR_OPL }
	| "\"lessEqual\""		{ STR_OPLEQ }
	| "\"equal\""			{ STR_OPEQ }
	| "\"greaterEqual\""	{ STR_OPGEQ }
	| "\"greater\""			{ STR_OPG }
	
	| "\"*\""				{ STR_OPMUL }
	
(* 	| "\"http://formalisms.cosyverif.org/parametric-timed-automaton.fml\"" { STR_FORMALISM_URL } (*NOTE: vraiment moche (on devrait autoriser n'importe quoi) *) *)
(* 	| "\"http://cosyverif.org/ns/model\"" { STR_XMLNS } (*NOTE: vraiment moche (on devrait autoriser n'importe quoi) *) *)
	
	
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

(* 	| "\"" [^'"']* "\"" { ANYSTRING } *)

	| "arcType"			{ CT_ARCTYPE }
(* 	| "encoding"		{ CT_ENCODING } *)
(* 	| "formalismUrl"	{ CT_FORMALISMURL } *)
	| "id"				{ CT_ID }
	| "name"			{ CT_NAME }
	| "nodeType"		{ CT_NODETYPE }
	| "source"			{ CT_SOURCE }
	| "target"			{ CT_TARGET }
(* 	| "version"			{ CT_VERSION } *)
(* 	| "xmlns"			{ CT_XMLNS } *)
(* 	| "xmlns:ns" ['0'-'9']+ ""			{ CT_XMLNS } *)

	| ['0'-'9']+ as lxm { INT(NumConst.numconst_of_string lxm) }

	| ['a'-'z''A'-'Z''_''0'-'9']+ as lxm { NAME lxm }
	| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT lxm } 
	
	| '='              { OP_EQ }

(*	| '+'              { OP_PLUS }
	| '*'              { OP_MUL }*)
	| '-'              { OP_MINUS }
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
