(*****************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Parser for the input model
 *
 * File contributors : Étienne André
 * Created           : 2019/10/08
 * Last modified     : 2019/12/13
*****************************************************************)

{
open PropertyParser
open Lexing

(* OCaml style comments *)
let comment_depth = ref 0;;

let line=ref 1;;

}

rule token = parse
	  ['\n']             { line := !line + 1 ; token lexbuf }     (* skip new lines *)
	| [' ' '\t']         { token lexbuf }     (* skip blanks *)

(*	(* C style include *)
	| "#include \""   ( [^'"' '\n']* as filename) '"'
    {
			let top_file = lexbuf.lex_start_p.pos_fname in
			let absolute_filename = FilePath.make_absolute (FilePath.dirname top_file) filename in

			let c = open_in absolute_filename in
			let lb = Lexing.from_channel c in
			lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = absolute_filename };

			let p = ModelParser.main token lb in
			INCLUDE p
    }*)

	(* OCaml style comments *)
	| "(*"
		{ comment_depth := 1;
		comment_ocaml lexbuf;
		token lexbuf }

 	| "#exhibit"       { CT_EXHIBIT }
 	| "#witness"       { CT_EXHIBIT }
 	| "#synth"         { CT_SYNTH }
 	
	| "EF"             { CT_EF }
	| "AG"             { CT_AG }

	| "accloop"        { CT_ACCLOOP }
	| "always"         { CT_ALWAYS }
	| "and"            { CT_AND }
	| "before"         { CT_BEFORE }
 	| "deadlockfree"   { CT_DEADLOCKFREE }
 	| "eventually"     { CT_EVENTUALLY }
 	| "everytime"      { CT_EVERYTIME }
	| "False"          { CT_FALSE }
 	| "happened"       { CT_HAPPENED }
 	| "has"            { CT_HAS }
	| "if"             { CT_IF }
	| "in"             { CT_IN }
	| "is"             { CT_IS }
 	| "inversemethod"  { CT_INVERSEMETHOD }
	| "loc"            { CT_LOC }
	| "loop"           { CT_LOOP }
	| "maximize"       { CT_MAXIMIZE }
	| "minimize"       { CT_MINIMIZE }
	| "next"           { CT_NEXT }
	| "not"            { CT_NOT }
 	| "once"           { CT_ONCE }
	| "or"             { CT_OR }
 	| "projectresult"  { CT_PROJECTRESULT }
 	| "property"       { CT_PROPERTY }
	| "sequence"       { CT_SEQUENCE }
 	| "then"           { CT_THEN }
 	| "tracepreservation" { CT_TRACEPRESERVATION }
	| "True"           { CT_TRUE }
 	| "unreachable"    { CT_UNREACHABLE }
	| "when"           { CT_WHEN }
	| "within"         { CT_WITHIN }



	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
	| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT lxm }
	| ['0'-'9']+ as lxm { INT(NumConst.numconst_of_string lxm) }
	| '"' [^'"']* '"' as lxm { STRING lxm } (* a string between double quotes *)

	| "<="             { OP_LEQ }
	| ">="             { OP_GEQ }
	| '<'              { OP_L }
	| '='              { OP_EQ }
	| "<>"             { OP_NEQ }
	| '>'              { OP_G }
	| ":="             { OP_ASSIGN }

	| '+'              { OP_PLUS }
	| '-'              { OP_MINUS }
	| '/'              { OP_DIV }
	| '*'              { OP_MUL }

	| '('              { LPAREN }
	| ')'              { RPAREN }
	| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }

	| '&'              { AMPERSAND }
	| "&&"             { DOUBLEAMPERSAND }
	| ".."             { DOUBLEDOT }
	| ','              { COMMA }
	| '|'              { PIPE }
	| "||"             { DOUBLEPIPE }
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
    { failwith "End of file inside a comment in property." }
  | '\n'  { line := !line + 1 ; comment_ocaml lexbuf }
  | _     { comment_ocaml lexbuf }
