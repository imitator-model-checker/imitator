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
 * Last modified     : 2019/10/09
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

let f (decl_l, aut_l, init_l, prPARSED_OP_L) (decl,aut,init,prop,_,_,_) = (decl@decl_l,aut@aut_l, init@init_l, prop::prPARSED_OP_L);;
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
%token PARSED_OP_L PARSED_OP_LEQ PARSED_OP_EQ PARSED_OP_NEQ PARSED_OP_GEQ PARSED_OP_G /* OP_ASSIGN */

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token AMPERSAND COLON COMMA DOUBLEAMPERSAND DOUBLEDOT DOUBLEPIPE PIPE SEMICOLON
%token CT_IF CT_THEN CT_ELSE CT_END /* tokens for conditions on transitions*/

%token
	CT_ACCEPTING CT_AG CT_ALWAYS CT_AND
	CT_BEFORE
	CT_EF CT_EVENTUALLY CT_EVERYTIME
	CT_FALSE
	CT_HAPPENED CT_HAS
	CT_IF CT_IN CT_IS
	CT_NEXT CT_NOT
	CT_ONCE CT_OR
	CT_PROJECTRESULT
	CT_SEQUENCE
	CT_THEN CT_TRUE
	CT_WHEN CT_WITHIN


%token EOF

%left DOUBLEPIPE PIPE CT_OR  /* lowest precedence */
%left AMPERSAND CT_AND       /* medium precedence */
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
	quantified_property EOF { $1 }
;

/************************************************************/
quantified_property:
/************************************************************/
	synth_or_exhibit property {
		{
			synthesis_type	= $1;
			property		= $2
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
	| CT_EF state_predicate { EF $1 }



	/** TODO **/



;

/************************************************************/
state_predicate:
/************************************************************/
	| state_predicate or state_predicate { Parsed_state_predicate_OR ($1, $3) }
	| state_predicate_term { Parsed_state_predicate_term ($1) }
;

state_predicate_term:
	| state_predicate_term and state_predicate_term { Parsed_state_predicate_term_AND ($1, $3) }
	| state_predicate_factor { Parsed_state_predicate_factor ($1) }
;

state_predicate_factor:
	| simple_predicate { Parsed_simple_predicate $1 }
	| NOT state_predicate_factor { Parsed_state_predicate_factor_NOT $2 }
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
	| CT_LOC LSQBRA NAME RSQBRA PARSED_OP_EQ NAME { Parsed_loc_predicate_EQ ($3, $6) }
	/* my_pta IS IN my_loc */
	| NAME CT_IS CT_IN NAME { Parsed_loc_predicate_EQ ($1, $4) }
	
	/* loc[my_pta] <> my_loc */
	| CT_LOC LSQBRA NAME RSQBRA PARSED_OP_NEQ NAME { Parsed_loc_predicate_NEQ ($3, $6) }
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
	| discrete_expression op_bool discrete_expression { Expression ($1, $2, $3) }
	/* expr in [expr .. expr] */
	| discrete_expression CT_IN LSQBRA discrete_expression COMMA discrete_expression RSQBRA { Expression_in ($1, $4, $6) }
	| discrete_expression CT_IN LSQBRA discrete_expression DOUBLEDOT discrete_expression RSQBRA { Expression_in ($1, $4, $6) }
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
	| OP_MINUS rational_linear_term { NumConst.sub NumConst.zero $2 }
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
or:
/************************************************************/
	| CT_OR {}
	| DOUBLEPIPE {}
	| PIPE {}
;

and:
	| CT_AND {}
	| AMPERSAND {}
	| DOUBLEAMPERSAND {}
;
