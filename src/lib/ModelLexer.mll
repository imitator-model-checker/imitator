(*****************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, LORIA, CNRS, France
 *
 * Author:        Étienne André
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2009/09/07
*****************************************************************)

{
open Lexing
open Exceptions
(* open ImitatorUtilities *)
open ModelParser

(* OCaml style comments *)
let comment_depth = ref 0;;

let line=ref 1;;

}

rule token = parse
	  ['\n']             { line := !line + 1 ; token lexbuf }     (* skip new lines *)
	| [' ' '\t']         { token lexbuf }     (* skip blanks *)

	(* C style include *)
	| "#include \""   ( [^'"' '\n']* as file_name) '"'
    {
			let top_file = lexbuf.lex_start_p.pos_fname in
			let absolute_filename = FilePath.make_absolute (FilePath.dirname top_file) file_name in

			let c = try(
				open_in absolute_filename
			)with
				| Sys_error _ ->
					(* Abort properly *)
(* 					print_error(e); *)
					raise (IncludeFileNotFound file_name);
			in
			let lb = Lexing.from_channel c in
			lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = absolute_filename };

			let p : ParsingStructure.parsed_model_unexpanded = ModelParser.main token lb in
			INCLUDE p
    }

	(* OCaml style comments *)
	| "(*"
		{ comment_depth := 1;
		comment_ocaml lexbuf;
		token lexbuf }

	(*** NOTE: just to forbid their use in the input model and property ***)
 	| "automatically_generated_observer"    { CT_OBSERVER }                (* to forbid this keyword, potentially used in the observer *)
 	| "automatically_generated_x_obs"       { CT_OBSERVER_CLOCK }          (* to forbid this keyword, potentially used in the observer *)
 	| "nosync_obs"                          { CT_NOSYNCOBS}                (* to forbid this keyword, used when an observer is defined *)
 	| "special_0_clock"                     { CT_SPECIAL_RESET_CLOCK_NAME} (* to forbid this keyword, used when a special reset clock is defined *)

 	(* All keywords *)
 	| "accepting"      { CT_ACCEPTING }
	| "action"         { CT_ACTION }
	| "actions"        { CT_ACTIONS }
  	| "array"          { CT_ARRAY }
	| "automaton"      { CT_AUTOMATON }
	| "begin"          { CT_BEGIN }
	| "binary"         { CT_BINARY_WORD }
	| "bool"           { CT_BOOL }
	| "clock"          { CT_CLOCK }
	| "constant"       { CT_CONSTANT }
	| "continuous"     { CT_CONTINUOUS }
	| "controllable"   { CT_CONTROLLABLE }
	| "discrete"       { CT_RATIONAL }
	| "do"             { CT_DO }
	| "done"           { CT_DONE }
	| "downto"         { CT_DOWNTO }
	| "else"           { CT_ELSE }
	| "end"            { CT_END }
	| "False"          { CT_FALSE }
	| "flow"           { CT_FLOW }
	| "fn"             { CT_FUN   }
	| "for"            { CT_FOR }
	| "from"           { CT_FROM }
	| "function"       { CT_FUN   }
	| "goto"           { CT_GOTO }
	| "if"             { CT_IF }
	| "in"             { CT_IN }
	| "init"           { CT_INIT }
	| "instantiate"    { CT_INSTANTIATE }
	| "int"            { CT_INT }
	| "inside"         { CT_INSIDE }
	| "invariant"      { CT_INVARIANT }
	| "is"             { CT_IS }
	| "list"           { CT_LIST }
	| "loc"            { CT_LOC }
	| "not"            { CT_NOT }
(* 	| "or"             { CT_OR } *)
	| "parameter"      { CT_PARAMETER }
	| "queue"          { CT_QUEUE }
  	| "rat"            { CT_RATIONAL }
	| "rational"       { CT_RATIONAL }
	| "return"         { CT_RETURN }
	| "stack"          { CT_STACK }
	| "stop"           { CT_STOP }
	| "sync"           { CT_SYNC }
	| "synclabs"       { CT_SYNCLABS }
  | "synt_var"     { CT_SYNT_VAR }
	| "template"       { CT_TEMPLATE }
 	| "then"           { CT_THEN }
	| "to"             { CT_TO }
	| "True"           { CT_TRUE }
	| "uncontrollable" { CT_UNCONTROLLABLE }
 	| "urgent"         { CT_URGENT }
	| "var"            { CT_VAR }
  	| "void"           { CT_VOID }
	| "wait"           { CT_WAIT }
	| "when"           { CT_WHEN }
	| "while"          { CT_WHILE }

	(* Unused in models but added to avoid conflicts with properties *)
	| "inf"            { CT_INFINITY }
	| "infinity"       { CT_INFINITY }


	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
	| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT lxm }
	| ['0'-'9']+ as lxm { INT(NumConst.numconst_of_string lxm) }
  | "0b"['0'-'9']+ as lxm { BINARYWORD lxm }
(*	| '"' [^'"']* '"' as lxm { STRING lxm } *) (* a string between double quotes *)

	(* Comparison operators *)
	| "<="             { OP_LEQ }
	| ">="             { OP_GEQ }
	| '<'              { OP_L }
	| '='              { OP_EQ }
	| "<>"             { OP_NEQ }
	| '>'              { OP_G }

	(* Assignment *)
	| ":="             { OP_ASSIGN }

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
	| '('              { LPAREN }
	| ')'              { RPAREN }
	| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }

	| '\''             { APOSTROPHE }
	| ':'              { COLON }
	| ','              { COMMA }
	| ".."             { DOUBLEDOT }
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
