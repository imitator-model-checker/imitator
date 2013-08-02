(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created       : 2010/01/04
 * Last modified : 2013/03/05
*****************************************************************)

{
open V0Parser
open Global

(* OCaml style comments *)
let comment_depth = ref 0;;

(* exception Unexpected_token *)

}

rule token =
	parse
	[' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
	| "--" [^'\n']* '\n'     { token lexbuf }     (* skip Hytech-style comments *)

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


	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
	| ['0'-'9']+ as lxm { INT(NumConst.numconst_of_string lxm) }

	| '='              { OP_EQ }

	| '+'              { OP_PLUS }
	| '-'              { OP_MINUS }
	| '/'              { OP_DIV }
	| '*'              { OP_MULT }

	| '('              { LPAREN }
	| ')'              { RPAREN }

	| '&'              { AMPERSAND }
	| ".."              { DOUBLEDOT }
	| ';'              { SEMICOLON }

	| eof              { EOF}
	| _ as c           { raise (UnexpectedToken c) }
	

(* C style comments *)
and comment_c = parse
    "/*"  { incr comment_depth; comment_c lexbuf }
  | "*/"  { decr comment_depth;
            if !comment_depth == 0 then () else comment_c lexbuf }
  | eof
    { failwith "End of file inside a comment." }
  | _     { comment_c lexbuf }
  
(* OCaml style comments *)
and comment_ocaml = parse
    "(*"  { incr comment_depth; comment_ocaml lexbuf }
  | "*)"  { decr comment_depth;
            if !comment_depth == 0 then () else comment_ocaml lexbuf }
  | eof
    { failwith "End of file inside a comment." }
  | _     { comment_ocaml lexbuf }


