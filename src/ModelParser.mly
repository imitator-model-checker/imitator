/***********************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created       : 2009/09/07
 * Last modified : 2012/10/08
***********************************************/

%{
open ParsingStructure;;
open Global;;
open NumConst;;
  
  
let parse_error s =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	raise (ParsingError (symbol_start, symbol_end))
;;
 
%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> NAME
%token <string> STRING

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token AMPERSAND APOSTROPHE COLON COMMA PIPE SEMICOLON

/* CT_ALL CT_ANALOG CT_ASAP CT_BACKWARD CT_CLDIFF CT_D  CT_ELSE CT_EMPTY  CT_ENDHIDE CT_ENDIF CT_ENDREACH CT_ENDWHILE CT_FORWARD CT_FREE CT_FROM  CT_HIDE CT_HULL CT_INTEGRATOR CT_ITERATE CT_NON_PARAMETERS CT_OMIT CT_POST CT_PRE CT_PRINT CT_PRINTS CT_PRINTSIZE CT_REACH  CT_STOPWATCH CT_THEN CT_TO CT_TRACE CT_USING  CT_WEAKDIFF CT_WEAKEQ CT_WEAKGE CT_WEAKLE  */

%token CT_AND CT_AUTOMATON CT_BAD CT_CLOCK CT_DISCRETE CT_DO CT_END CT_FALSE CT_GOTO CT_IF CT_INIT CT_INITIALLY CT_LOC CT_LOCATIONS CT_NOT CT_OR CT_PARAMETER CT_REGION CT_STOP CT_SYNC CT_SYNCLABS CT_TRUE CT_VAR CT_WAIT CT_WHEN CT_WHILE

%token EOF

%left PIPE CT_OR        /* lowest precedence */
%left AMPERSAND CT_AND  /* medium precedence */
%nonassoc CT_NOT        /* highest precedence */


%start main             /* the entry point */
%type <ParsingStructure.parsing_structure> main
%%

/**********************************************/
main:
	 automata_descriptions commands EOF
	{
		let decl, automata = $1 in
		let init, bad = $2 in
		decl, automata, init, bad
	}
;

/***********************************************
  MAIN DEFINITIONS
***********************************************/

automata_descriptions:
	declarations automata { $1, $2 }
;

/**********************************************/

declarations:
	CT_VAR decl_var_lists { $2 }
;


/**********************************************/

/**********************************************/

decl_var_lists:
	decl_var_list COLON var_type SEMICOLON decl_var_lists { (($3, $1) :: $5) }
	| { [] }
;

/**********************************************/

decl_var_list:
	| NAME comma_opt { [($1, None)] }
	| NAME OP_EQ rational comma_opt { [($1, Some $3)] }
	
	| NAME COMMA decl_var_list { ($1, None) :: $3 }
	| NAME OP_EQ rational COMMA decl_var_list { ($1, Some $3) :: $5 }
;

/**********************************************/

var_type:
	| CT_CLOCK { Var_type_clock }
	| CT_DISCRETE { Var_type_discrete }
	| CT_PARAMETER { Var_type_parameter }
;

/**********************************************/

/**********************************************/

automata:
	automaton automata { $1 :: $2 }
	| { [] }
;

/**********************************************/

automaton:
	CT_AUTOMATON NAME prolog locations CT_END
	{
		($2, $3, $4)
	}
;

/**********************************************/

prolog:
	| initialization sync_labels { $2 }
	| sync_labels initialization { $1 }
	| sync_labels { $1 } /* L'initialisation n'est pas prise en compte, et est donc facultative */
	| initialization { [] }
	| { [] }
;

/**********************************************/

initialization:
	| CT_INITIALLY NAME state_initialization SEMICOLON {}
;

/**********************************************/

state_initialization:
	AMPERSAND convex_predicate {}
	| {}
;

/**********************************************/

sync_labels:
	CT_SYNCLABS COLON name_list SEMICOLON { $3 }
;

/**********************************************/

name_list:
	name_nonempty_list { $1 }
	| { [] }
;

/**********************************************/

name_nonempty_list:
	NAME COMMA name_nonempty_list { $1 :: $3}
	| NAME comma_opt { [$1] }
;

/**********************************************/

locations:
	location locations { $1 :: $2}
	| { [] }
;

/**********************************************/

location:
// 	| CT_LOC location_name COLON CT_WHILE convex_predicate stopwatches CT_WAIT braces_opt transitions {
	| CT_LOC location_name COLON CT_WHILE convex_predicate stopwatches wait_opt transitions {
		let name, cost = $2 in
			name, cost, $5, $6, $8
		}
;

location_name:
	| NAME { $1, None }
	| NAME LSQBRA linear_expression RSQBRA { $1, Some $3 }
;

wait_opt:
	| CT_WAIT { }
	| CT_WAIT LBRACE RBRACE { }
	| LBRACE RBRACE { }
	| { }
;

/**********************************************/

stopwatches:
	| CT_STOP LBRACE name_list RBRACE { $3 }
	| { [] }
;

/**********************************************/

transitions:
	transition transitions { $1 :: $2 }
	| { [] }
;

/**********************************************/

transition:
	CT_WHEN convex_predicate update_synchronization CT_GOTO NAME SEMICOLON
	{
		let update_list, sync = $3 in
			$2, update_list, sync, $5
	}
;

/**********************************************/

/* A l'origine de 3 conflits ("2 shift/reduce conflicts, 1 reduce/reduce conflict.") donc petit changement */
update_synchronization:
	| { [], NoSync }
	| updates { $1, NoSync }
	| syn_label { [], (Sync $1) }
	| updates syn_label { $1, (Sync $2) }
	| syn_label updates { $2, (Sync $1) }
;

/**********************************************/

updates:
	CT_DO LBRACE update_list RBRACE { $3 }
;

/**********************************************/

update_list:
	update_nonempty_list { $1 }
	| { [] }
;

/**********************************************/

update_nonempty_list:
	update COMMA update_nonempty_list { $1 :: $3}
	| update { [$1] }
;

/**********************************************/

update:
	NAME APOSTROPHE OP_EQ linear_expression { ($1, $4) }
;

/**********************************************/

syn_label:
	CT_SYNC NAME { $2 }
;


/**********************************************/
/** RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES */
/***********************************************/

/* We allow an optional "&" at the beginning of a convex predicate (sometimes useful) */
convex_predicate:
	ampersand_opt convex_predicate_fol { $2 }
;

convex_predicate_fol:
	linear_constraint AMPERSAND convex_predicate { $1 :: $3 }
	| linear_constraint { [$1] }
;

linear_constraint:
	linear_expression relop linear_expression { Linear_constraint ($1, $2, $3) }
	| CT_TRUE { True_constraint }
	| CT_FALSE { False_constraint }
;

relop:
	  OP_L { OP_L }
	| OP_LEQ { OP_LEQ }
	| OP_EQ { OP_EQ }
	| OP_GEQ { OP_GEQ }
	| OP_G { OP_G }
;

linear_expression:
	linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) } /* linear_term a la deuxieme place */
;

linear_term:
	rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| NAME { Variable (NumConst.one, $1) }
// 	| LPAREN linear_expression RPAREN { $2 }
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
		let fstr = $1 in
		let point = String.index fstr '.' in
		(* get integer part *)
		let f = if point = 0 then ref NumConst.zero else (
			let istr = String.sub fstr 0 point in
		  ref (NumConst.numconst_of_int (int_of_string istr))
		) in		
		(* add decimal fraction part *)
		let numconst_of_char = function
			| '0' -> NumConst.zero
			| '1' -> NumConst.one
			| '2' -> NumConst.numconst_of_int 2
			| '3' -> NumConst.numconst_of_int 3
			| '4' -> NumConst.numconst_of_int 4
			| '5' -> NumConst.numconst_of_int 5
			| '6' -> NumConst.numconst_of_int 6
			| '7' -> NumConst.numconst_of_int 7
			| '8' -> NumConst.numconst_of_int 8
			| '9' -> NumConst.numconst_of_int 9
			| _ ->  raise (ParsingError (0,0)) in
		let ten = NumConst.numconst_of_int 10 in
		let dec = ref (NumConst.numconst_of_frac 1 10) in
		for i = point+1 to (String.length fstr) - 1 do
			let c = fstr.[i] in
			let d = numconst_of_char c in
			f := NumConst.add !f (NumConst.mul !dec d);
			dec := NumConst.div !dec ten 
		done;		
		!f
	} 
;

/***********************************************
  ANALYSIS COMMANDS
***********************************************/

commands:
	| init_declaration init_definition bad_definition rest_of_commands { ($2, $3) }
	| init_declaration init_definition rest_of_commands { ($2, []) }
;


init_declaration:
	| CT_VAR regions COLON CT_REGION SEMICOLON { }
	| { }
;

regions:
	| { }
	| region_names { }
;

region_names:
	| region_name COMMA region_names { }
	| region_name { }
;

region_name:
	| NAME { }
	| CT_INIT { }
	| CT_BAD { }
;

rest_of_commands:
	/* print (reach forward from init endreach); */
/*	| CT_PRINT LPAREN CT_REACH CT_FORWARD CT_FROM region_name CT_ENDREACH RPAREN SEMICOLON { }
	| { }*/
	/* Allow anything from here! (to ensure compatibility with HyTech or other similar tools) */
	| anything rest_of_commands { }
	| { }
;

anything:
	| LPAREN { }
	| region_name { }
	| RPAREN { }
	| SEMICOLON { }
;

init_definition:
	CT_INIT OP_ASSIGN region_expression SEMICOLON { $3 }
;

bad_definition:
	| CT_BAD OP_ASSIGN loc_expression SEMICOLON { $3 }
;

loc_expression:
	| loc_predicate { [ $1 ] }
	| loc_predicate AMPERSAND loc_expression { $1 :: $3 }
	| loc_predicate loc_expression { $1 :: $2 }
;

// We allow here an optional "&" at the beginning
region_expression:
	ampersand_opt region_expression_fol { $2 }
;

region_expression_fol:
	| state_predicate { [ $1 ] }
	| LPAREN region_expression_fol RPAREN { $2 }
	| region_expression_fol AMPERSAND region_expression_fol { $1 @ $3 }
;

loc_predicate:
	CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME { Loc_assignment ($3, $6) }
;

state_predicate:
	| loc_predicate { $1 } 
	| linear_constraint { Linear_predicate $1 }
;

comma_opt:
	| COMMA { }
	| { }
;

ampersand_opt:
	| AMPERSAND { }
	| { }
;
