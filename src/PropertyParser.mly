/************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, LORIA, CNRS, France
 *
 * Module description: Parser for the input model
 *
 * File contributors : Étienne André
 * Created           : 2019/10/08
 * Last modified     : 2019/10/08
 *
 ************************************************************/


%{
open ParsingStructure;;
open Exceptions;;
open NumConst;;
open ImitatorUtilities;;


let parse_error s =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	raise (ParsingError (symbol_start, symbol_end))
;;


(* TODO: is it included twice ? *)
let include_list = ref [];;

let f (decl_l, aut_l, init_l, prop_l) (decl,aut,init,prop,_,_,_) = (decl@decl_l,aut@aut_l, init@init_l, prop::prop_l);;
let unzip l = List.fold_left f ([],[],[], []) (List.rev l);;
let filter_opt = List.filter (function | None -> false | Some _ -> true);;

let resolve_property l =
	match filter_opt l with
	| [] -> None
	| [p] -> p
	| _ -> raise Parsing.Parse_error;
;;

%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> NAME
%token <string> STRING
%token <ParsingStructure.parsing_structure> INCLUDE

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_NEQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token AMPERSAND APOSTROPHE COLON COMMA DOUBLEDOT PIPE SEMICOLON
%token CT_IF CT_THEN CT_ELSE CT_END /* tokens for conditions on transitions*/

%token
	CT_ACCEPTING CT_ALWAYS CT_AND
	CT_BEFORE
	CT_EVENTUALLY CT_EVERYTIME
	CT_FALSE
	CT_HAPPENED CT_HAS
	CT_IF CT_IN
	CT_NEXT CT_NOT
	CT_ONCE CT_OR
	CT_PROJECTRESULT
	CT_SEQUENCE
	CT_THEN CT_TRUE
	CT_WHEN CT_WITHIN


%token EOF

%left PIPE CT_OR        /* lowest precedence */
%left AMPERSAND CT_AND  /* medium precedence */
%left DOUBLEDOT         /* high precedence */
%nonassoc CT_NOT        /* highest precedence */

%left OP_PLUS OP_MINUS  /* lowest precedence */
%left OP_MUL OP_DIV     /* highest precedence */


%start main             /* the entry point */
%type <ParsingStructure.parsing_structure> main
%%
