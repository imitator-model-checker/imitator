/************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 *
 * Module description: Parser for the input model
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2009/09/07
 * Last modified     : 2019/07/05
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

/* CT_ALL CT_ANALOG CT_ASAP CT_BACKWARD CT_CLDIFF CT_D  CT_ELSE CT_EMPTY  CT_ENDHIDE CT_ENDIF CT_ENDREACH CT_ENDWHILE CT_FORWARD CT_FREE CT_FROM  CT_HIDE CT_HULL CT_INTEGRATOR CT_ITERATE CT_NON_PARAMETERS CT_OMIT CT_POST CT_PRE CT_PRINT CT_PRINTS CT_PRINTSIZE CT_REACH  CT_STOPWATCH CT_THEN CT_TO CT_TRACE CT_USING  CT_WEAKDIFF CT_WEAKEQ CT_WEAKGE CT_WEAKLE  */

%token
	CT_ACCEPTING CT_ALWAYS CT_AND CT_AUTOMATON
	CT_BAD CT_BEFORE
	CT_CARTO CT_CLOCK CT_CONSTANT
	CT_DISCRETE CT_DO
	CT_END CT_EVENTUALLY CT_EVERYTIME
	CT_FALSE
	CT_GOTO
	CT_HAPPENED CT_HAS
	CT_IF CT_IN CT_INIT CT_INITIALLY CT_INVARIANT
	CT_LOC CT_LOCATIONS
	CT_MAXIMIZE CT_MINIMIZE
	CT_NEXT CT_NOT
	CT_ONCE CT_OR
	CT_PARAMETER CT_PROJECTRESULT CT_PROPERTY
	CT_REGION
	CT_SEQUENCE CT_STOP CT_SYNC CT_SYNCLABS
	CT_THEN CT_TRUE
	CT_UNKNOWN CT_UNREACHABLE CT_URGENT
	CT_VAR
	CT_WAIT CT_WHEN CT_WHILE CT_WITHIN
	/* NOTE: just to forbid their use in the input model */
	CT_OBSERVER CT_OBSERVER_CLOCK CT_SPECIAL_RESET_CLOCK_NAME


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

/**********************************************/
main:
	include_file_list automata_descriptions commands EOF
	{
		let included_files = $1 in
		print_endline (string_of_int (List.length included_files));
		let decl, automata = $2 in
		let init_definition, bad, projection_definition, optimization_definition, carto = $3 in
		decl, automata, init_definition, bad, projection_definition, optimization_definition, carto
	}
;

/***********************************************
	INCLUDES
***********************************************/
include_file_list:
	| INCLUDE SEMICOLON include_file_list  { $1 :: $3 }
	| { [] }
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
	| NAME OP_EQ rational_linear_expression comma_opt { [($1, Some $3)] }

	| NAME COMMA decl_var_list { ($1, None) :: $3 }
	| NAME OP_EQ rational_linear_expression COMMA decl_var_list { ($1, Some $3) :: $5 }
;

/**********************************************/

var_type:
	| CT_CLOCK { Var_type_clock }
	| CT_CONSTANT { Var_type_constant }
	| CT_DISCRETE { Var_type_discrete }
	| CT_PARAMETER { Var_type_parameter }
;

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
	| sync_labels { $1 } /* Initialization is NOT taken into account, and is only allowed for backward-compatibility with HyTech */
	| initialization { [] }
	| { [] }
;

/**********************************************/

/* WARNING: deprecated syntax */
initialization:
	| CT_INITIALLY NAME state_initialization SEMICOLON {
		(* Print a warning because this syntax is deprecated and not taken into account! *)
		print_warning ("The syntax 'initially " ^ $2 ^ "' is deprecated and is NOT taken into account. Initial locations must be defined in the initial state definition.");
		()
	}
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

while_or_invariant_or_nothing:
	/* From 2018/02/22, "while" can be replaced with invariant */
	| CT_WHILE {}
	| CT_INVARIANT {}
	| {}
;

location:
	| loc_urgency_accepting_type location_name_and_costs COLON while_or_invariant_or_nothing convex_predicate stopwatches wait_opt transitions {
		let urgency, accepting = $1 in
		let name, cost = $2 in
		{
			(* Name *)
			name = name;
			(* Urgent or not? *)
			urgency = urgency;
			(* Accepting or not? *)
			acceptance = accepting;
			(* Cost *)
			cost = cost;
			(* Invariant *)
			invariant = $5;
			(* List of stopped clocks *)
			stopped = $6;
			(* Transitions starting from this location *)
			transitions = $8;
		}
	}
;

loc_urgency_accepting_type:
	| CT_LOC { Parsed_location_nonurgent, Parsed_location_nonaccepting }
	| CT_URGENT CT_LOC { Parsed_location_urgent, Parsed_location_nonaccepting }
	| CT_ACCEPTING CT_LOC { Parsed_location_nonurgent, Parsed_location_accepting }
	| CT_URGENT CT_ACCEPTING CT_LOC { Parsed_location_urgent, Parsed_location_accepting }
	| CT_ACCEPTING CT_URGENT CT_LOC { Parsed_location_urgent, Parsed_location_accepting }
;

location_name_and_costs:
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
	update COMMA update_list { Normal $1 :: $3}
	| update { [Normal $1] }
	| condition_update COMMA update_list { Condition $1 :: $3}
	| condition_update { [Condition $1] }

/**********************************************/

/** Normal updates */
update:
	| NAME APOSTROPHE OP_EQ arithmetic_expression { ($1, $4) }
	/** NOTE: from 2018/02/22: assign becomes recommended */
	| NAME APOSTROPHE OP_ASSIGN arithmetic_expression { ($1, $4) }
	/** NOTE: from 2018/02/22: apostrophe becomes optional */
	| NAME OP_EQ arithmetic_expression { ($1, $3) }
	/** NOTE: from 2018/02/22: assign becomes recommended */
	| NAME OP_ASSIGN arithmetic_expression { ($1, $3) }
;

/** List containing only normal updates.
		NOTE: it is used to avoid nested conditional updates */
normal_update_list:
	update COMMA normal_update_list { $1 :: $3}
	| update { [$1]}
	| { [] }

/** Condition updates **/
condition_update:
	| CT_IF LPAREN boolean_expression RPAREN CT_THEN normal_update_list CT_END { ($3, $6, []) }
	| CT_IF LPAREN boolean_expression RPAREN CT_THEN normal_update_list CT_ELSE normal_update_list CT_END { ($3, $6,  $8) }
;

/**********************************************/

syn_label:
	CT_SYNC NAME { $2 }
;

/**********************************************/
/** ARITHMETIC EXPRESSIONS */
/***********************************************/

arithmetic_expression:
	| arithmetic_term { Parsed_UAE_term $1 }
	| arithmetic_expression OP_PLUS arithmetic_term { Parsed_UAE_plus ($1, $3) }
	| arithmetic_expression OP_MINUS arithmetic_term { Parsed_UAE_minus ($1, $3) }
;

/* Term over variables and rationals (includes recursion with arithmetic_expression) */
arithmetic_term:
	| arithmetic_factor { Parsed_UT_factor $1 }
	/* Shortcut for syntax rational NAME without the multiplication operator */
	| rational NAME { Parsed_UT_mul (Parsed_UT_factor (Parsed_UF_constant $1), Parsed_UF_variable $2) }
	| arithmetic_term OP_MUL arithmetic_factor { Parsed_UT_mul ($1, $3) }
	| arithmetic_term OP_DIV arithmetic_factor { Parsed_UT_div ($1, $3) }
	| OP_MINUS arithmetic_term { Parsed_UT_mul($2, Parsed_UF_constant NumConst.minus_one) }
;

arithmetic_factor:
	| rational { Parsed_UF_constant $1 }
	| NAME { Parsed_UF_variable $1 }
	| LPAREN arithmetic_expression RPAREN { Parsed_UF_expression $2 }
;


/**********************************************/
/** RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES */
/***********************************************/

/* We allow an optional "&" at the beginning of a convex predicate (sometimes useful) */
convex_predicate:
	ampersand_opt convex_predicate_fol { $2 }
;

convex_predicate_fol:
	| linear_constraint AMPERSAND convex_predicate { $1 :: $3 }
	| linear_constraint { [$1] }
;

linear_constraint:
	| linear_expression relop linear_expression { Linear_constraint ($1, $2, $3) }
	| CT_TRUE { True_constraint }
	| CT_FALSE { False_constraint }
;

relop:
	  OP_L { OP_L }
	| OP_LEQ { OP_LEQ }
	| OP_EQ { OP_EQ }
	| OP_NEQ { OP_NEQ }
	| OP_GEQ { OP_GEQ }
	| OP_G { OP_G }
;

/* Linear expression over variables and rationals */
linear_expression:
	| linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) } /* linear_term a la deuxieme place */
;

/* Linear term over variables and rationals (no recursion, no division) */
linear_term:
	rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| OP_MINUS NAME { Variable (NumConst.minus_one, $2) }
	| NAME { Variable (NumConst.one, $1) }
// 	| LPAREN linear_expression RPAREN { $2 }
	| LPAREN linear_term RPAREN { $2 }
;

/* Linear expression over rationals only */
rational_linear_expression:
	rational_linear_term { $1 }
	| rational_linear_expression OP_PLUS rational_linear_term { NumConst.add $1 $3 }
	| rational_linear_expression OP_MUL rational_linear_term { NumConst.mul $1 $3 }
	| rational_linear_expression OP_MINUS rational_linear_term { NumConst.sub $1 $3 } /* linear_term a la deuxieme place */
;

/* Linear term over rationals only */
rational_linear_term:
	rational { $1 }
	| OP_MINUS rational { NumConst.sub NumConst.zero $2 }
	| LPAREN rational_linear_term RPAREN { $2 }
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

/**********************************************/
/** BOOLEAN EXPRESSIONS */
/***********************************************/
boolean_expression:
	| CT_TRUE { True }
	| CT_FALSE { False }
	| OP_NEQ LPAREN boolean_expression RPAREN { Not $3 }
	| boolean_expression AMPERSAND boolean_expression { And ($1, $3) }
	| boolean_expression PIPE boolean_expression { Or ($1, $3) }
	| arithmetic_expression relop arithmetic_expression { Expression ($1, $2, $3) }

/***********************************************/
/** ANALYSIS COMMANDS */
/***********************************************/

commands:
	| init_declaration_opt init_definition property_definition projection_definition optimization_definition carto_definition rest_of_commands_opt { ($2, $3, $4, $5, $6) }
// 	| init_declaration_opt init_definition bad_definition { ($2, $3, ([] , (NumConst.zero,NumConst.zero) , (NumConst.zero,NumConst.zero))) }
;


/* For backward-compatibility with HyTech only */
init_declaration_opt:
	| init_declaration_useless { }
	| { }
;

/* For backward-compatibility with HyTech only */
init_declaration_useless:
	| CT_VAR regions COLON CT_REGION SEMICOLON { }
;

/* For backward-compatibility with HyTech only */
regions:
	| { }
	| region_names { }
;

/* For backward-compatibility with HyTech only */
region_names:
	| region_name COMMA region_names { }
	| region_name { }
;

/* For backward-compatibility with HyTech only */
region_name:
	| NAME { }
	| CT_INIT { }
	| CT_BAD { }
;

rest_of_commands_opt:
	/* print (reach forward from init endreach); */
/*	| CT_PRINT LPAREN CT_REACH CT_FORWARD CT_FROM region_name CT_ENDREACH RPAREN SEMICOLON { }
	| { }*/
	/* Allow anything from here! (to ensure compatibility with HyTech or other similar tools) */
	| CT_END rest_of_commands { }
	| { }

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
	| CT_INIT OP_ASSIGN region_expression SEMICOLON { $3 }
;


/* We allow here an optional "&" at the beginning */
region_expression:
	| ampersand_opt region_expression_fol { $2 }
	| { [ ] }
;

region_expression_fol:
	| init_state_predicate { [ $1 ] }
	| LPAREN region_expression_fol RPAREN { $2 }
	| region_expression_fol AMPERSAND region_expression_fol { $1 @ $3 }
;

/* Used in the init definition */
init_state_predicate:
	| loc_predicate { let a,b = $1 in (Loc_assignment (a,b)) }
	| linear_constraint { Linear_predicate $1 }
;

loc_predicate:
	| CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME { ($3, $6) }
;

discrete_predicate:
	| NAME OP_L rational { Parsed_discrete_l($1, $3) }
	| NAME OP_LEQ rational { Parsed_discrete_leq($1, $3) }
	| NAME OP_EQ rational { Parsed_discrete_equal($1, $3) }
	| NAME OP_GEQ rational { Parsed_discrete_geq($1, $3) }
	| NAME OP_G rational { Parsed_discrete_g($1, $3) }
	// Two forms allowed for intervals: d in [a, b] or d in [a..b]
	| NAME CT_IN LSQBRA rational COMMA rational RSQBRA { Parsed_discrete_interval($1, $4, $6) }
	| NAME CT_IN LSQBRA rational DOUBLEDOT rational RSQBRA { Parsed_discrete_interval($1, $4, $6) }
;





property_definition:
// TODO: improve the bad definitions
	// NOTE: Old version
// 	| CT_BAD OP_ASSIGN loc_expression SEMICOLON { $3 }

	// Case: action
	// TODO: reintroduce
// 	| CT_BAD OP_ASSIGN CT_EXISTS_ACTION NAME SEMICOLON { [Exists_action $4] }

	// NOTE: only one allowed before version 2.6 and ICECCS paper
	// Case: location
	// | CT_BAD OP_ASSIGN CT_EXISTS_LOCATION loc_predicate SEMICOLON { let a,b = $4 in [(Exists_location (a , b))] }

	// Pattern
	| CT_PROPERTY OP_ASSIGN pattern semicolon_opt { Some $3 }

	// Case: no property
	|  { None }

;

projection_definition:
	| CT_PROJECTRESULT LPAREN name_nonempty_list RPAREN semicolon_opt { Some $3 }

	// Case: no projection
	|  { None }

;

optimization_definition:
	| CT_MINIMIZE LPAREN NAME RPAREN semicolon_opt { Parsed_minimize $3 }
	| CT_MAXIMIZE LPAREN NAME RPAREN semicolon_opt { Parsed_maximize $3 }

	// Case: no min/max
	|  { No_parsed_optimization }

;


// List of patterns
pattern:
	// Unreachability
	| CT_UNREACHABLE bad_global_predicates { Parsed_unreachable_locations ($2) }

	/* if a2 then a1 has happened before */
	| CT_IF NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_BEFORE { Action_precedence_acyclic ($4, $2) }
	/* everytime a2 then a1 has happened before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_BEFORE { Action_precedence_cyclic ($4, $2) }
	/* everytime a2 then a1 has happened once before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_ONCE CT_BEFORE { Action_precedence_cyclicstrict ($4, $2) }

	// PATTERNS NOT IMPLEMENTED
// 	/* if a1 then eventually a2 */
// 	| CT_IF NAME CT_THEN CT_EVENTUALLY NAME { Eventual_response_acyclic ($2, $5) }
// 	/* everytime a1 then eventually a2 */
// 	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME { Eventual_response_cyclic ($2, $5) }
// 	/* everytime a1 then eventually a2 once before next */
// 	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_ONCE CT_BEFORE CT_NEXT { Eventual_response_cyclicstrict ($2, $5) }

	/* a within d */
	| NAME CT_WITHIN linear_expression { Action_deadline ($1, $3) }

	/* if a2 then a1 happened within d before */
	| CT_IF NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_WITHIN linear_expression CT_BEFORE { TB_Action_precedence_acyclic ($4, $2, $8) }
	/* everytime a2 then a1 happened within d before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_WITHIN linear_expression CT_BEFORE { TB_Action_precedence_cyclic ($4, $2, $8) }
	/* everytime a2 then a1 happened once within d before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_ONCE CT_WITHIN linear_expression CT_BEFORE { TB_Action_precedence_cyclicstrict ($4, $2, $9) }

	/* if a1 then eventually a2 within d */
	| CT_IF NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression { TB_response_acyclic ($2, $5, $7) }
	/* everytime a1 then eventually a2 within d */
	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression { TB_response_cyclic ($2, $5, $7) }
	/* everytime a1 then eventually a2 within d once before next */
	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression CT_ONCE CT_BEFORE CT_NEXT { TB_response_cyclicstrict ($2, $5, $7) }

	/* sequence a1, ..., an */
	| CT_SEQUENCE name_nonempty_list { Sequence_acyclic ($2) }
	| CT_SEQUENCE LPAREN name_nonempty_list RPAREN { Sequence_acyclic ($3) } /* with parentheses */
	/* always sequence a1, ..., an */
	| CT_ALWAYS CT_SEQUENCE name_nonempty_list { Sequence_cyclic ($3) }
	| CT_ALWAYS CT_SEQUENCE LPAREN name_nonempty_list RPAREN { Sequence_cyclic ($4) } /* with parentheses */
;


/* A single definition of one bad location or one bad discrete definition */
bad_simple_predicate:
	| discrete_predicate { Parsed_unreachable_discrete($1) }
	| loc_predicate { Parsed_unreachable_loc($1) }
;

/* A global definition of several bad locations and/or bad discrete definitions */
bad_global_predicate:
	| bad_global_predicate AMPERSAND bad_global_predicate { List.rev_append $1 $3 }
	| LPAREN bad_global_predicate RPAREN { $2 }
	| bad_simple_predicate { [$1] }
;

bad_global_predicates:
	| bad_global_predicate CT_OR bad_global_predicates { $1 :: $3 }
	| bad_global_predicate { [$1] }
;


convex_predicate_with_nature:
	// TODO
// 	| convex_predicate LBRACE CT_GOOD RBRACE { $1, Good }
// 	| convex_predicate LBRACE CT_BAD RBRACE { $1, Bad }
// 	| convex_predicate LBRACE CT_UNKNOWN RBRACE { $1, Unknown }
// 	| convex_predicate LBRACE RBRACE { $1, Unknown }
	| convex_predicate { $1, Unknown }

carto_definition:
	| CT_CARTO OP_ASSIGN carto_definition_interval OP_MUL carto_definition_interval convex_predicate_with_nature carto_definition_foll SEMICOLON { $6 :: $7 , $3 , $5 }
// 	| CT_CARTO OP_ASSIGN convex_predicate_with_nature carto_definition_foll SEMICOLON { $3 :: $4 , (NumConst.zero,NumConst.zero) , (NumConst.zero,NumConst.zero) }
// 	| CT_CARTO OP_ASSIGN   SEMICOLON { [] , (NumConst.zero,NumConst.zero) , (NumConst.zero,NumConst.zero) }
	// WARNING: bad prog below
	|  { [] , (NumConst.zero,NumConst.zero) , (NumConst.zero,NumConst.zero) }
;

carto_definition_interval:
	| LPAREN pos_integer COMMA pos_integer RPAREN { ($2,$4) }
;

carto_definition_foll:
	| PIPE convex_predicate_with_nature carto_definition_foll { $2 :: $3 }
	| { [] }

//// NOTE: Old version
// loc_expression:
// 	| loc_predicate { [ $1 ] }
// 	| loc_predicate AMPERSAND loc_expression { $1 :: $3 }
// 	| loc_predicate loc_expression { $1 :: $2 }
// ;

comma_opt:
	| COMMA { }
	| { }
;

semicolon_opt:
	| SEMICOLON { }
	| { }
;

ampersand_opt:
	| AMPERSAND { }
	| { }
;
