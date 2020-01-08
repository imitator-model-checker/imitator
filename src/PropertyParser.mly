/************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Parser for the input model
 *
 * File contributors : Étienne André
 * Created           : 2019/10/08
 * Last modified     : 2020/01/08
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

(*
let f (decl_l, aut_l, init_l, prop) (decl,aut,init,prop,_,_,_) = (decl@decl_l,aut@aut_l, init@init_l, prop::prop);;
let unzip l = List.fold_left f ([],[],[], []) (List.rev l);;
let filter_opt = List.filter (function | None -> false | Some _ -> true);;

let resolve_property l =
	match filter_opt l with
	| [] -> None
	| [p] -> p
	| _ -> raise Parsing.Parse_error;
;;*)

%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> NAME
%token <string> STRING

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_NEQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token COLON COMMA DOUBLEDOT SEMICOLON SYMBOL_AND SYMBOL_OR

%token
	CT_ACCEPTING CT_ACCLOOP CT_AG CT_ALWAYS CT_AND
	CT_BEFORE
	CT_EF CT_EVENTUALLY CT_EVERYTIME CT_EXHIBIT
	CT_DEADLOCKFREE
	CT_FALSE
	CT_HAPPENED CT_HAS
	CT_IF CT_IN CT_INVERSEMETHOD CT_IS
	CT_LOC CT_LOOP
	CT_MAXIMIZE CT_MINIMIZE
	CT_NEXT CT_NOT
	CT_ONCE CT_OR
	CT_PROJECTRESULT
	CT_PROPERTY
	CT_SEQUENCE CT_SYNTH
	CT_THEN CT_TRACEPRESERVATION CT_TRUE
	CT_UNREACHABLE 
	CT_WHEN CT_WITHIN


%token EOF

%left SYMBOL_OR              /* lowest precedence */
%left SYMBOL_AND             /* medium precedence */
%left DOUBLEDOT              /* high precedence */
%nonassoc CT_NOT             /* highest precedence */

%left OP_PLUS OP_MINUS       /* lowest precedence */
%left OP_MUL OP_DIV          /* highest precedence */


%start main             /* the entry point */
%type <ParsingStructure.parsed_property> main
%%

/************************************************************/
main:
/************************************************************/
	property_kw_opt quantified_property semicolon_opt EOF { $2 }
;

/************************************************************/
property_kw_opt:
/************************************************************/
	| CT_PROPERTY OP_ASSIGN {}
	| {}
;

/************************************************************/
quantified_property:
/************************************************************/
	synth_or_exhibit property {
		{
			synthesis_type	= $1;
			property		= $2;
			(* Projection *)
			(*** TODO ***)
			projection		= None;
		}
	}
;

synth_or_exhibit:
	| CT_EXHIBIT { Parsed_exhibition }
	| CT_SYNTH   { Parsed_synthesis }
;

/************************************************************/
property:
/************************************************************/
	/*------------------------------------------------------------*/
	/* Non-nested CTL */
	/*------------------------------------------------------------*/

	/* Reachability */
	| CT_EF state_predicate { Parsed_EF $2 }



	/** TODO **/



;

/************************************************************/
state_predicate:
/************************************************************/
	| state_predicate SYMBOL_OR state_predicate { Parsed_state_predicate_OR ($1, $3) }
	| state_predicate_term { Parsed_state_predicate_term $1 }
;

state_predicate_term:
	| state_predicate_term SYMBOL_AND state_predicate_term { Parsed_state_predicate_term_AND ($1, $3) }
	| state_predicate_factor { Parsed_state_predicate_factor $1 }
;

state_predicate_factor:
	| simple_predicate { Parsed_simple_predicate $1 }
	| CT_NOT state_predicate_factor { Parsed_state_predicate_factor_NOT $2 }
	| LPAREN state_predicate RPAREN { Parsed_state_predicate $2 }
;

/* A single definition of one bad location or one bad discrete definition */
simple_predicate:
	| discrete_boolean_predicate { Parsed_discrete_boolean_expression($1) }
	| loc_predicate { Parsed_loc_predicate ($1) }
;


/************************************************************/
loc_predicate:
/************************************************************/
	/* loc[my_pta] = my_loc */
	| CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME { Parsed_loc_predicate_EQ ($3, $6) }
	/* my_pta IS IN my_loc */
	| NAME CT_IS CT_IN NAME { Parsed_loc_predicate_EQ ($1, $4) }
	
	/* loc[my_pta] <> my_loc */
	| CT_LOC LSQBRA NAME RSQBRA OP_NEQ NAME { Parsed_loc_predicate_NEQ ($3, $6) }
	/* my_pta IS NOT IN my_loc */
	| NAME CT_IS CT_NOT CT_IN NAME { Parsed_loc_predicate_EQ ($1, $5) }
;
/** NOTE:

"Pourquoi pas PTA in Bad (ou PTA is in Bad) plutôt que loc[PTA] = Bad ? Ce n'est pas plus long mais plus lisible, non ?"
	Didier
*/


/************************************************************/
discrete_boolean_predicate:
/************************************************************/
	/* expr ~ expr */
	| discrete_expression op_bool discrete_expression { Parsed_expression ($1, $2, $3) }
	/* expr in [expr .. expr] */
	| discrete_expression CT_IN LSQBRA discrete_expression COMMA discrete_expression RSQBRA { Parsed_expression_in ($1, $4, $6) }
	| discrete_expression CT_IN LSQBRA discrete_expression DOUBLEDOT discrete_expression RSQBRA { Parsed_expression_in ($1, $4, $6) }
;

discrete_expression:
	| discrete_expression OP_PLUS discrete_term { Parsed_DAE_plus ($1, $3) }
	| discrete_expression OP_MINUS discrete_term { Parsed_DAE_minus ($1, $3) }
	| discrete_term { Parsed_DAE_term $1 }
;

discrete_term:
	| discrete_term OP_MUL discrete_factor { Parsed_DT_mul ($1, $3) }
	| discrete_term OP_DIV discrete_factor { Parsed_DT_div ($1, $3) }
	| discrete_factor { Parsed_DT_factor $1 }
;

discrete_factor:
	| NAME { Parsed_DF_variable $1 }
	| positive_rational { Parsed_DF_constant $1 }
	| RPAREN discrete_expression LPAREN { Parsed_DF_expression $2 }
	| OP_MINUS discrete_factor { Parsed_DF_unary_min $2 }
;



/************************************************************/
op_bool:
/************************************************************/
	| OP_L   { ParsingStructure.PARSED_OP_L }
	| OP_LEQ { ParsingStructure.PARSED_OP_LEQ }
	| OP_EQ  { ParsingStructure.PARSED_OP_EQ }
	| OP_NEQ { ParsingStructure.PARSED_OP_NEQ }
	| OP_GEQ { ParsingStructure.PARSED_OP_GEQ }
	| OP_G   { ParsingStructure.PARSED_OP_G }
;


/************************************************************/
rational_linear_term:
/************************************************************/
	| positive_rational_with_div { $1 }
	/*** NOTE: or should it be OP_MINUS rational_with_div? ***/
	| OP_MINUS rational_linear_term { NumConst.neg $2 }
	| LPAREN rational_linear_term RPAREN { $2 }
;

positive_rational_with_div:
	| pos_integer { $1 }
	| pos_float { $1 }
	| pos_integer OP_DIV pos_integer { (NumConst.div $1 $3) }
;

positive_rational:
	| pos_integer { $1 }
	| pos_float { $1 }
;

pos_integer:
	| INT { $1 }
;

pos_float:
  | FLOAT {
		NumConst.numconst_of_string $1
	}
;


/************************************************************/
/*** WARNING: call these rules "and" or "or" is accepted by OCaml Yak but not by the subsequent OCaml compiler ***/
/* or_rule:*/
/************************************************************/
/*	| SYMBOL_OR {}
;

and_rule:
	| SYMBOL_AND {}
;*/








property_definition:
/*
// TODO: improve the bad definitions
	// NOTE: Old version
// 	| CT_BAD OP_ASSIGN loc_expression SEMICOLON { $3 }

	// Case: action
	// TODO: reintroduce
// 	| CT_BAD OP_ASSIGN CT_EXISTS_ACTION NAME SEMICOLON { [Exists_action $4] }

	// NOTE: only one allowed before version 2.6 and ICECCS paper
	// Case: location
	// | CT_BAD OP_ASSIGN CT_EXISTS_LOCATION loc_predicate SEMICOLON { let a,b = $4 in [(Exists_location (a , b))] }
*/
	/* Pattern */
	| CT_PROPERTY OP_ASSIGN pattern semicolon_opt { Some $3 }

	/*** TODO: re-enable file inclusion ***/
/* 	| include_file { let _, _, _, property, _, _, _ = $1 in property } */

;

projection_definition:
	| CT_PROJECTRESULT LPAREN name_nonempty_list RPAREN semicolon_opt { Some $3 }

	/* Case: no projection */
	|  { None }

;

optimization_definition:
	| CT_MINIMIZE LPAREN NAME RPAREN semicolon_opt { Parsed_minimize $3 }
	| CT_MAXIMIZE LPAREN NAME RPAREN semicolon_opt { Parsed_maximize $3 }

	/* Case: no min/max */
	|  { No_parsed_optimization }

;


/* List of patterns */
pattern:
	/* Safety */
	| CT_UNREACHABLE state_predicate { Parsed_EF ($2) }

	/* if a2 then a1 has happened before */
/*	| CT_IF NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_BEFORE { Action_precedence_acyclic ($4, $2) }*/
	/* everytime a2 then a1 has happened before */
/*	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_BEFORE { Action_precedence_cyclic ($4, $2) }*/
	/* everytime a2 then a1 has happened once before */
/*	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_ONCE CT_BEFORE { Action_precedence_cyclicstrict ($4, $2) }*/


	/* PATTERNS NOT IMPLEMENTED */
/* 	/ * if a1 then eventually a2 * /
 	| CT_IF NAME CT_THEN CT_EVENTUALLY NAME { Eventual_response_acyclic ($2, $5) }
 	/ * everytime a1 then eventually a2 * /
 	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME { Eventual_response_cyclic ($2, $5) }
 	/ * everytime a1 then eventually a2 once before next * /
 	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_ONCE CT_BEFORE CT_NEXT { Eventual_response_cyclicstrict ($2, $5) }
*/

	/* a within d */
/*	| NAME CT_WITHIN linear_expression { Action_deadline ($1, $3) }*/

	/* if a2 then a1 happened within d before */
/*	| CT_IF NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_WITHIN linear_expression CT_BEFORE { TB_Action_precedence_acyclic ($4, $2, $8) }*/
	/* everytime a2 then a1 happened within d before */
/*	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_WITHIN linear_expression CT_BEFORE { TB_Action_precedence_cyclic ($4, $2, $8) }*/
	/* everytime a2 then a1 happened once within d before */
/*	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_ONCE CT_WITHIN linear_expression CT_BEFORE { TB_Action_precedence_cyclicstrict ($4, $2, $9) }*/

	/* if a1 then eventually a2 within d */
/*	| CT_IF NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression { TB_response_acyclic ($2, $5, $7) }*/
	/* everytime a1 then eventually a2 within d */
/*	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression { TB_response_cyclic ($2, $5, $7) }*/
	/* everytime a1 then eventually a2 within d once before next */
/*	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression CT_ONCE CT_BEFORE CT_NEXT { TB_response_cyclicstrict ($2, $5, $7) }*/

	/* sequence a1, …, an */
/*	| CT_SEQUENCE name_nonempty_list { Sequence_acyclic ($2) }
	| CT_SEQUENCE LPAREN name_nonempty_list RPAREN { Sequence_acyclic ($3) }*/ /* with parentheses */
	/* always sequence a1, …, an */
/*	| CT_ALWAYS CT_SEQUENCE name_nonempty_list { Sequence_cyclic ($3) }
	| CT_ALWAYS CT_SEQUENCE LPAREN name_nonempty_list RPAREN { Sequence_cyclic ($4) } */ /* with parentheses */
;


/* A single definition of one bad location or one bad discrete definition */
/*bad_simple_predicate:
	| discrete_predicate { Parsed_unreachable_discrete($1) }
	| loc_predicate { Parsed_unreachable_loc($1) }
;
*/
/* A global definition of several bad locations and/or bad discrete definitions */
/*bad_global_predicate:
	| bad_global_predicate AMPERSAND bad_global_predicate { List.rev_append $1 $3 }
	| LPAREN bad_global_predicate RPAREN { $2 }
	| bad_simple_predicate { [$1] }
;

bad_global_predicates:
	| bad_global_predicate CT_OR bad_global_predicates { $1 :: $3 }
	| bad_global_predicate { [$1] }
;
*/

	
	
	

/************************************************************/
/** LINEAR EXPRESSIONS */
/************************************************************/

/*** BADPROG: duplicate code from ModelParser ***/

/* Linear expression over variables and rationals */
linear_expression:
	| linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) } /* linear_term a la deuxieme place */
;

/* Linear term over variables and rationals (no recursion, no division) */
linear_term:
	| rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| OP_MINUS NAME { Variable (NumConst.minus_one, $2) }
	| NAME { Variable (NumConst.one, $1) }
	| LPAREN linear_term RPAREN { $2 }
;

rational:
	integer { $1 }
	| float { $1 }
	| integer OP_DIV pos_integer { (NumConst.div $1 $3) }
;

integer:
	pos_integer { $1 }
	| OP_MINUS pos_integer { NumConst.neg $2 }
;

pos_integer:
	INT { $1 }
;

float:
  pos_float { $1 }
	| OP_MINUS pos_float { NumConst.neg $2 }
;

pos_float:
  FLOAT {
		NumConst.numconst_of_string $1
	}
;


/************************************************************/
/** NAMES, etc. */
/************************************************************/

comma_opt:
	| COMMA { }
	| { }
;


name_nonempty_list:
	NAME COMMA name_nonempty_list { $1 :: $3}
	| NAME comma_opt { [$1] }
;


/************************************************************/
/** MISC. */
/************************************************************/

semicolon_opt:
	| SEMICOLON { }
	| { }
;
