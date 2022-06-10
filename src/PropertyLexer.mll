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

	(* Synthesis type *)
 	| "#exemplification"{ CT_EXEMPLIFY }
 	| "#exemplify"     { CT_EXEMPLIFY }
 	| "#exhibit"       { CT_EXHIBIT }
 	| "#witness"       { CT_EXHIBIT }
 	| "#synth"         { CT_SYNTH }

 	(* Keywords for properties *)
	| "1cOpacity"      { CT_ONECLOCKOPACITY }
	| "AccCycle"       { CT_ACCEPTINGCYCLE }
	| "AcceptingCycle" { CT_ACCEPTINGCYCLE }
	| "AccLoop"        { CT_ACCEPTINGCYCLE }
	| "AcceptingLoop"  { CT_ACCEPTINGCYCLE }
	| "AG"             { CT_AG }
	| "AGnot"          { CT_AGnot }
 	| "DeadlockFree"   { CT_DEADLOCKFREE }
 	| "BCcover"        { CT_COVERCARTOGRAPHY }
 	| "BClearn"        { CT_BCLEARN }
 	| "BCshuffle"      { CT_BCSHUFFLE }
 	| "BCborder"       { CT_BCBORDER }
 	| "BCrandom"       { CT_BCRANDOM }
 	| "BCrandomseq"    { CT_BCRANDOMSEQ }
	| "Cycle"          { CT_INFCYCLE }
	| "CycleThrough"   { CT_INFCYCLETHROUGH }
	| "EF"             { CT_EF }
	| "EFpmax"         { CT_EFpmax }
	| "EFpmin"         { CT_EFpmin }
	| "EFtmin"         { CT_EFtmin }
 	| "IM"             { CT_TRACEPRESERVATION }
 	| "IMconvex"       { CT_IMCONVEX }
 	| "IMK"            { CT_IMK }
 	| "IMunion"        { CT_IMUNION }
 	| "InverseMethod"  { CT_TRACEPRESERVATION }
	| "Loop"           { CT_INFCYCLE }
	| "LoopThrough"    { CT_INFCYCLETHROUGH }
	| "NZCycle"        { CT_NZCYCLE }
 	| "PRP"            { CT_PRP }
 	| "PRPC"           { CT_PRPC }
 	| "TracePreservation" { CT_TRACEPRESERVATION }

	| "accepting"      { CT_ACCEPTING }
	| "always"         { CT_ALWAYS }
	| "and"            { SYMBOL_AND }
	| "before"         { CT_BEFORE }
 	| "eventually"     { CT_EVENTUALLY }
 	| "everytime"      { CT_EVERYTIME }
	| "False"          { CT_FALSE }
 	| "happened"       { CT_HAPPENED }
 	| "has"            { CT_HAS }
	| "if"             { CT_IF }
	| "in"             { CT_IN }
	| "is"             { CT_IS }
	| "loc"            { CT_LOC }
	| "next"           { CT_NEXT }
	| "not"            { CT_NOT }
 	| "once"           { CT_ONCE }
	| "or"             { SYMBOL_OR }
	| "pattern"        { CT_PATTERN }
 	| "projectresult"  { CT_PROJECTRESULT }
 	| "property"       { CT_PROPERTY }
	| "sequence"       { CT_SEQUENCE }
	| "step"           { CT_STEP }
 	| "then"           { CT_THEN }
	| "True"           { CT_TRUE }
	| "within"         { CT_WITHIN }
  | "list"           { CT_LIST }
  | "stack"          { CT_STACK }
  | "queue"          { CT_QUEUE }



	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
	| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT lxm }
	| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | "0b"['0'-'9']+ as lxm { BINARYWORD lxm }
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

	| '&'              { SYMBOL_AND }
	| "&&"             { SYMBOL_AND }
	| ".."             { DOUBLEDOT }
	| ','              { COMMA }
	| '|'              { SYMBOL_OR }
	| "||"             { SYMBOL_OR }
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
