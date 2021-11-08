/************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Parser for the input model
 *
 * File contributors : Étienne André, Jaime Arias, Benjamin Loillier, Laure Petrucci
 * Created           : 2009/09/07
 * Last modified     : 2021/10/26
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


(*** TODO (Jaime): is it included twice ? ***)
let include_list = ref [];;

let add_parsed_model_to_parsed_model_list parsed_model_list parsed_model =
	{
		variable_declarations	= List.append parsed_model.variable_declarations parsed_model_list.variable_declarations;
		automata				= List.append parsed_model.automata parsed_model_list.automata;
		init_definition			= List.append parsed_model.init_definition parsed_model_list.init_definition;
	}
;;

let unzip l = List.fold_left
	add_parsed_model_to_parsed_model_list
	{
		variable_declarations	= [];
		automata				= [];
		init_definition			= [];
	}
	(List.rev l)
;;

%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> BINARYWORD
%token <string> NAME
/* %token <string> STRING */
%token <ParsingStructure.parsed_model> INCLUDE

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_NEQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token AMPERSAND APOSTROPHE COLON COMMA DOUBLEDOT PIPE SEMICOLON

%token
	CT_ACCEPTING CT_ALWAYS CT_AND CT_AUTOMATON
	CT_BEFORE
	CT_CLOCK CT_CONSTANT
	CT_DISCRETE CT_INT CT_BOOL CT_BINARY_WORD CT_ARRAY CT_DO
	CT_ELSE CT_END CT_EVENTUALLY CT_EVERYTIME
	CT_FALSE CT_FLOW
	CT_GOTO
	CT_HAPPENED CT_HAS
	CT_IF CT_IN CT_INIT CT_CONTINUOUS CT_INITIALLY CT_INVARIANT CT_IS
	CT_LOC
	CT_NEXT CT_NOT
	CT_ONCE CT_OR
	CT_PARAMETER
	CT_SEQUENCE CT_STOP CT_SYNC CT_SYNCLABS
	CT_THEN CT_TRUE
	CT_UNKNOWN CT_URGENT
	CT_VAR
	CT_WAIT CT_WHEN CT_WHILE CT_WITHIN
	/*** NOTE: just to forbid their use in the input model and property ***/
	CT_NOSYNCOBS CT_OBSERVER CT_OBSERVER_CLOCK CT_SPECIAL_RESET_CLOCK_NAME
    CT_BUILTIN_FUNC_RATIONAL_OF_INT CT_POW CT_SHIFT_LEFT CT_SHIFT_RIGHT CT_FILL_LEFT CT_FILL_RIGHT
    CT_LOG_AND CT_LOG_OR CT_LOG_XOR CT_LOG_NOT CT_ARRAY_CONCAT CT_LIST


%token EOF

%right OP_ASSIGN
%right OP_EQ

%left PIPE CT_OR        /* lowest precedence */
%left AMPERSAND CT_AND  /* medium precedence */
%left DOUBLEDOT         /* high precedence */
%nonassoc CT_NOT        /* highest precedence */

%left OP_PLUS OP_MINUS  /* lowest precedence */
%left OP_MUL OP_DIV     /* highest precedence */


%start main             /* the entry point */
%type <ParsingStructure.parsed_model> main
%%

/************************************************************/
main:
	declarations automata init_definition_option
	end_opt EOF
	{
		let declarations	= $1 in
		let automata		= $2 in
		let init_definition	= $3 in

		let main_model =
		{
			variable_declarations	= declarations;
			automata				= automata;
			init_definition			= init_definition;
		}
		in

		let included_model = unzip !include_list in

		(* Return the parsed model *)
		add_parsed_model_to_parsed_model_list included_model main_model
	}
;

end_opt:
	| CT_END { }
	| { }
;


/************************************************************
	INCLUDES
************************************************************/
include_file:
	| INCLUDE SEMICOLON { $1 }
;

include_file_list:
	| include_file include_file_list  { $1 :: $2 }
	| { [] }
;

/************************************************************
  DECLARATIONS
************************************************************/

/************************************************************/

declarations:
	| include_file_list CT_VAR decl_var_lists { $3 }
	| { []}
;


/************************************************************/

/************************************************************/

decl_var_lists:
	| decl_var_list COLON var_type SEMICOLON decl_var_lists { (($3, $1) :: $5) }
	| { [] }
;

/************************************************************/

decl_var_list:
	| NAME comma_opt { [($1, None)] }
	| NAME OP_EQ init_value_expression comma_opt { [($1, Some $3)] }

	| NAME COMMA decl_var_list { ($1, None) :: $3 }
	| NAME OP_EQ init_value_expression COMMA decl_var_list { ($1, Some $3) :: $5 }
;

/************************************************************/

var_type:
	| CT_CLOCK { Var_type_clock }
	| CT_CONSTANT { Var_type_constant }
	| CT_PARAMETER { Var_type_parameter }
	| var_type_discrete { Var_type_discrete $1 }
;

var_type_discrete:
    | var_type_discrete_number { Var_type_discrete_number $1 }
    | CT_BOOL { Var_type_discrete_bool }
    /* TODO benjamin try to use directly int instead of numconst */
    | CT_BINARY_WORD LPAREN pos_integer RPAREN { Var_type_discrete_binary_word (NumConst.to_bounded_int $3) }
    | var_type_discrete_array { $1 }
    | var_type_discrete_list { $1 }
;

var_type_discrete_array:
  | var_type_discrete CT_ARRAY LPAREN pos_integer RPAREN { Var_type_discrete_array ($1, NumConst.to_bounded_int $4) }
  | var_type_discrete_array CT_ARRAY LPAREN pos_integer RPAREN { Var_type_discrete_array ($1, NumConst.to_bounded_int $4) }
;

var_type_discrete_list:
  | var_type_discrete CT_LIST { Var_type_discrete_list $1 }
  | var_type_discrete_list CT_LIST { Var_type_discrete_list $1 }
;

var_type_discrete_number:
    | CT_DISCRETE { Var_type_discrete_rational }
    | CT_INT { Var_type_discrete_int }
;

/************************************************************
  AUTOMATA
************************************************************/

/************************************************************/

automata:
	| automaton automata { $1 :: $2 }
	| include_file automata { include_list := $1 :: !include_list; $2 }
	| { [] }
;

/************************************************************/

automaton:
	| CT_AUTOMATON NAME prolog locations CT_END
	{
		($2, $3, $4)
	}
;

/************************************************************/

prolog:
	/* Initialization is NOT taken into account, and is only allowed for backward-compatibility with HyTech */
	| initialization sync_labels { $2 }
	| sync_labels initialization { $1 }
	| sync_labels { $1 }
	| initialization { [] }
	| { [] }
;

/************************************************************/

/* WARNING: deprecated syntax */
initialization:
	| CT_INITIALLY NAME state_initialization SEMICOLON {
		(* Print a warning because this syntax is deprecated and not taken into account! *)
		print_warning ("The syntax `initially " ^ $2 ^ "` is deprecated and is NOT taken into account. Initial locations must be defined in the initial state definition.");
		()
	}
;

/************************************************************/

state_initialization:
	| AMPERSAND convex_predicate {}
	| {}
;

/************************************************************/

sync_labels:
	| CT_SYNCLABS COLON name_list SEMICOLON { $3 }
;

/************************************************************/

name_list:
	| name_nonempty_list { $1 }
	| { [] }
;

/************************************************************/

name_nonempty_list:
	NAME COMMA name_nonempty_list { $1 :: $3}
	| NAME comma_opt { [$1] }
;

/************************************************************/

locations:
	location locations { $1 :: $2}
	| { [] }
;

/************************************************************/

while_or_invariant_or_nothing:
	/* From 2018/02/22, "while" may be be replaced with invariant */
	/* From 2019/12, "while" should be be replaced with invariant */
	| CT_WHILE {
		print_warning ("The syntax `while [invariant]` is deprecated; you should use `invariant [invariant]` instead.");
		()
		}
	| CT_INVARIANT {}
	| {}
;

location:
	| loc_urgency_accepting_type location_name_and_costs COLON while_or_invariant_or_nothing nonlinear_convex_predicate stopwatches_and_flow_opt wait_opt transitions {
		let urgency, accepting = $1 in
		let name, cost = $2 in
		let stopwatches, flow = $6 in
		{
			(* Name *)
			name		= name;
			(* Urgent or not? *)
			urgency		= urgency;
			(* Accepting or not? *)
			acceptance	= accepting;
			(* Cost *)
			cost		= cost;
			(* Invariant *)
			invariant	= $5;
			(* List of stopped clocks *)
			stopped		= stopwatches;
			(* Flow of clocks *)
			flow		= flow;
			(* Transitions starting from this location *)
			transitions = $8;
		}
	}
;

loc_urgency_accepting_type:
	| CT_LOC { Parsed_location_nonurgent, Parsed_location_nonaccepting }
	| CT_URGENT CT_LOC { Parsed_location_urgent, Parsed_location_nonaccepting }
	| CT_ACCEPTING CT_LOC { (Parsed_location_nonurgent, Parsed_location_accepting) }
	| CT_URGENT CT_ACCEPTING CT_LOC { (Parsed_location_urgent, Parsed_location_accepting) }
	| CT_ACCEPTING CT_URGENT CT_LOC { (Parsed_location_urgent, Parsed_location_accepting) }
;

location_name_and_costs:
	| NAME { $1, None }
	| NAME LSQBRA linear_expression RSQBRA { $1, Some $3 }
;

wait_opt:
	| CT_WAIT {
			print_warning ("The syntax `wait` in invariants is deprecated.");
		()
	}
	| CT_WAIT LBRACE RBRACE {
			print_warning ("The syntax `wait {}` in invariants is deprecated.");
		()
	}
	/* Now deprecated and not accepted anymore */
/* 	| LBRACE RBRACE { } */
	| { }
;


/************************************************************/

stopwatches_and_flow_opt:
	| stopwatches flow { $1, $2 }
	| flow stopwatches { $2, $1 }
	| stopwatches { $1, [] }
	| flow { [], $1 }
	| { [], [] }
;

/************************************************************/

flow:
	| CT_FLOW LBRACE flow_list RBRACE { $3 }
;


/************************************************************/

flow_list:
	| flow_nonempty_list { $1 }
	| { [] }
;

/************************************************************/

flow_nonempty_list:
	| single_flow COMMA flow_nonempty_list { $1 :: $3 }
	| single_flow comma_opt { [$1] }
;

/************************************************************/

single_flow:
	| NAME APOSTROPHE OP_EQ rational_linear_expression { ($1, $4) }
;

/************************************************************/

stopwatches:
	| CT_STOP LBRACE name_list RBRACE { $3 }
;

/************************************************************/

transitions:
	| transition transitions { $1 :: $2 }
	| { [] }
;

/************************************************************/

transition:
	| CT_WHEN nonlinear_convex_predicate update_synchronization CT_GOTO NAME SEMICOLON
	{
		let update_list, sync = $3 in
			$2, update_list, sync, $5
	}
;

/************************************************************/

/* A l'origine de 3 conflits ("2 shift/reduce conflicts, 1 reduce/reduce conflict.") donc petit changement */
update_synchronization:
	| { [], NoSync }
	| updates { $1, NoSync }
	| syn_label { [], (Sync $1) }
	| updates syn_label { $1, (Sync $2) }
	| syn_label updates { $2, (Sync $1) }
;

/************************************************************/

updates:
	| CT_DO LBRACE update_list RBRACE { $3 }
;

/************************************************************/

update_list:
	| update_nonempty_list { $1 }
	| { [] }
;

/************************************************************/

update_nonempty_list:
	| update COMMA update_nonempty_list { Normal $1 :: $3}
	| update comma_opt { [Normal $1] }
	| condition_update COMMA update_nonempty_list { Condition $1 :: $3}
	| condition_update comma_opt { [Condition $1] }
;

/************************************************************/

/* Variable or variable access */
variable_access:
  | NAME { Variable_name $1 }
  | variable_access LSQBRA arithmetic_expression RSQBRA { Variable_access ($1, $3) }
;

/** Normal updates */
update:
	/*** NOTE: deprecated syntax ***/
	| NAME APOSTROPHE OP_EQ expression {
		print_warning ("The syntax `var' = value` in updates is deprecated. Please use `var := value`.");
		(Variable_name $1, $4)
		}

		/** NOT ALLOWED FROM 3.2 (2021/10) */
/*	| NAME APOSTROPHE OP_ASSIGN expression {
		print_warning ("The syntax `var' := value` in updates is deprecated. Please use `var := value`.");
		(Variable_name $1, $4)
	}*/
	/*** NOTE: deprecated syntax ***/
	| NAME OP_EQ expression {
		print_warning ("The syntax `var = value` in updates is deprecated. Please use `var := value`.");
		(Variable_name $1, $3)
	}

	| variable_access OP_ASSIGN expression { ($1, $3) }
;

/** List containing only normal updates.
		NOTE: it is used to avoid nested conditional updates */
normal_update_list:
	| update COMMA normal_update_list { $1 :: $3}
	| update comma_opt { [$1]}
	| { [] }
;

normal_update_list_par_opt:
	 | normal_update_list { $1 }
	 | LPAREN normal_update_list RPAREN { $2 }
;

boolean_expression_par_opt:
	 | boolean_expression { $1 }
	 | LPAREN boolean_expression RPAREN { $2 }
;

/** Condition updates **/
condition_update:
	| CT_IF boolean_expression_par_opt CT_THEN normal_update_list_par_opt CT_END { ($2, $4, []) }
	| CT_IF boolean_expression_par_opt CT_THEN normal_update_list_par_opt CT_ELSE normal_update_list_par_opt CT_END { ($2, $4, $6) }
;

/************************************************************/

syn_label:
	CT_SYNC NAME { $2 }
;

/************************************************************/
/** ARITHMETIC EXPRESSIONS */
/************************************************************/

arithmetic_expression:
	| arithmetic_term { Parsed_DAE_term $1 }
	| arithmetic_expression OP_PLUS arithmetic_term { Parsed_DAE_plus ($1, $3) }
	| arithmetic_expression OP_MINUS arithmetic_term { Parsed_DAE_minus ($1, $3) }
;

/* Term over variables and rationals (includes recursion with arithmetic_expression) */
arithmetic_term:
	| arithmetic_factor { Parsed_DT_factor $1 }
	/* Shortcut for syntax rational NAME without the multiplication operator */
	| number NAME { Parsed_DT_mul (Parsed_DT_factor (Parsed_DF_constant ($1)), Parsed_DF_variable $2) }
	| arithmetic_term OP_MUL arithmetic_factor { Parsed_DT_mul ($1, $3) }
	| arithmetic_term OP_DIV arithmetic_factor { Parsed_DT_div ($1, $3) }
	| OP_MINUS arithmetic_factor { Parsed_DT_factor(Parsed_DF_unary_min $2) }
;

/*
postfix_arithmetic_factor:
  | arithmetic_factor { Parsed_DT_factor $1 }
  | postfix_arithmetic_factor LSQBRA pos_integer RSQBRA { Parsed_DF_access ($1, NumConst.to_int $3) }
;
*/

arithmetic_factor:
  | arithmetic_factor LSQBRA arithmetic_expression RSQBRA { Parsed_DF_access ($1, $3) }
  | number { Parsed_DF_constant ($1) }
  | CT_TRUE { Parsed_DF_constant (DiscreteValue.Bool_value true) }
  | CT_FALSE { Parsed_DF_constant (DiscreteValue.Bool_value false) }
  | binary_word { Parsed_DF_constant $1 }
  | literal_array { Parsed_DF_array (Array.of_list $1) }
  | CT_LIST LPAREN literal_array RPAREN { Parsed_DF_list $3 }
  | NAME { Parsed_DF_variable $1 }
  | LPAREN arithmetic_expression RPAREN { Parsed_DF_expression $2 }
  | function_call { $1 }
;

literal_array:
  /* Empty array */
  | LSQBRA RSQBRA { [] }
  /* Non-empty array */
  | LSQBRA literal_array_fol RSQBRA { $2 }
;

literal_array_fol:
	| boolean_expression COMMA literal_array_fol { $1 :: $3 }
	| boolean_expression { [$1] }
;

function_call:
	| CT_BUILTIN_FUNC_RATIONAL_OF_INT LPAREN arithmetic_expression RPAREN { Parsed_rational_of_int_function $3 }
	| CT_POW LPAREN arithmetic_expression COMMA arithmetic_expression RPAREN { Parsed_pow_function ($3, $5) }
	| CT_SHIFT_LEFT LPAREN arithmetic_factor COMMA arithmetic_expression RPAREN { Parsed_shift_function (Parsed_shift_left, $3, $5) }
	| CT_SHIFT_RIGHT LPAREN arithmetic_factor COMMA arithmetic_expression RPAREN { Parsed_shift_function (Parsed_shift_right, $3, $5) }
	| CT_FILL_LEFT LPAREN arithmetic_factor COMMA arithmetic_expression RPAREN { Parsed_shift_function (Parsed_fill_left, $3, $5) }
	| CT_FILL_RIGHT LPAREN arithmetic_factor COMMA arithmetic_expression RPAREN { Parsed_shift_function (Parsed_fill_right, $3, $5) }
	| CT_LOG_AND LPAREN arithmetic_factor COMMA arithmetic_factor RPAREN { Parsed_bin_log_function (Parsed_log_and, $3, $5) }
	| CT_LOG_OR LPAREN arithmetic_factor COMMA arithmetic_factor RPAREN { Parsed_bin_log_function (Parsed_log_or, $3, $5) }
	| CT_LOG_XOR LPAREN arithmetic_factor COMMA arithmetic_factor RPAREN { Parsed_bin_log_function (Parsed_log_xor, $3, $5) }
	| CT_LOG_NOT LPAREN arithmetic_factor RPAREN { Parsed_log_not $3 }
  | CT_ARRAY_CONCAT LPAREN arithmetic_factor COMMA arithmetic_factor RPAREN { Parsed_array_concat ($3, $5) }
;

number:
	| integer { DiscreteValue.Number_value $1 }
	| float { DiscreteValue.Rational_value $1 }
	/*| integer OP_DIV pos_integer { ( DiscreteValue.Rational_value (NumConst.div $1 $3)) }*/
;

binary_word:
        BINARYWORD { DiscreteValue.Binary_word_value (BinaryWord.binaryword_of_string $1) }
;

/************************************************************/
/** RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES */
/************************************************************/

/* We allow an optional "&" at the beginning of a convex predicate (sometimes useful) */
convex_predicate:
	| ampersand_opt convex_predicate_fol { $2 }
;

convex_predicate_fol:
	| linear_constraint AMPERSAND convex_predicate { $1 :: $3 }
	| linear_constraint { [$1] }
;

/* We allow an optional "&" at the beginning of a convex predicate (sometimes useful) */
nonlinear_convex_predicate:
	| ampersand_opt nonlinear_convex_predicate_fol { $2 }
;

nonlinear_convex_predicate_fol:
	| nonlinear_constraint AMPERSAND nonlinear_convex_predicate { $1 :: $3 }
	| nonlinear_constraint { [$1] }
;

linear_constraint:
	| linear_expression relop linear_expression { Parsed_linear_constraint ($1, $2, $3) }
	| CT_TRUE { Parsed_true_constraint }
	| CT_FALSE { Parsed_false_constraint }
;

nonlinear_constraint:
	| discrete_boolean_expression { Parsed_nonlinear_constraint $1 }
;

relop:
	| OP_L { PARSED_OP_L }
	| OP_LEQ { PARSED_OP_LEQ }
	| OP_EQ { PARSED_OP_EQ }
	| OP_NEQ { PARSED_OP_NEQ }
	| OP_GEQ { PARSED_OP_GEQ }
	| OP_G { PARSED_OP_G }
;

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

/* Init expression for variable */
init_value_expression:
    | expression { $1 }
;

/* Linear expression over rationals only */
rational_linear_expression:
	| rational_linear_term { $1 }
	| rational_linear_expression OP_PLUS rational_linear_term { NumConst.add $1 $3 }
	| rational_linear_expression OP_MUL rational_linear_term { NumConst.mul $1 $3 }
	| rational_linear_expression OP_DIV rational_linear_term { NumConst.div $1 $3 }
	| rational_linear_expression OP_MINUS rational_linear_term { NumConst.sub $1 $3 } /* linear_term a la deuxieme place */
;

/* Linear term over rationals only */
rational_linear_term:
	| rational { $1 }
	| OP_MINUS rational_linear_term { NumConst.neg $2 }
	| LPAREN rational_linear_expression RPAREN { $2 }
;

rational:
	| integer { $1 }
	| float { $1 }
	| integer OP_DIV pos_integer { (NumConst.div $1 $3) }
;

integer:
	| pos_integer { $1 }
	| OP_MINUS pos_integer { NumConst.neg $2 }
;

pos_integer:
	| INT { $1 }
;

float:
	| pos_float { $1 }
	| OP_MINUS pos_float { NumConst.neg $2 }
;

pos_float:
  FLOAT {
		NumConst.numconst_of_string $1
	}
;

/************************************************************/
/** BOOLEAN EXPRESSIONS */
/************************************************************/

boolean_expression:
	| discrete_boolean_expression { Parsed_Discrete_boolean_expression $1 }
	| boolean_expression AMPERSAND boolean_expression { Parsed_And ($1, $3) }
	| boolean_expression PIPE boolean_expression { Parsed_Or ($1, $3) }
;

discrete_boolean_expression:
	| arithmetic_expression { Parsed_arithmetic_expression $1 }
	/* Discrete arithmetic expression of the form Expr ~ Expr */
	| discrete_boolean_expression relop discrete_boolean_expression { Parsed_expression ($1, $2, $3) }
	/* Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' */
	| arithmetic_expression CT_IN LSQBRA arithmetic_expression COMMA arithmetic_expression RSQBRA { Parsed_expression_in ($1, $4, $6) }
	/* allowed for convenience */
	| arithmetic_expression CT_IN LSQBRA arithmetic_expression SEMICOLON arithmetic_expression RSQBRA { Parsed_expression_in ($1, $4, $6) }
	/* Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) */
	| LPAREN boolean_expression RPAREN { Parsed_boolean_expression $2 }
	| CT_NOT LPAREN boolean_expression RPAREN { Parsed_Not $3 }
;

/************************************************************/
/** INIT DEFINITION */
/************************************************************/

init_definition_option:
    | old_init_definition {
		(* Print a warning because this syntax is deprecated *)
		print_warning ("Old syntax detected for the initial state definition. You are advised to use the new syntax (from 3.1).");
		$1
		}
    | init_definition { $1 }
    | { [ ] }
;

/* Old init style (until 3.0), kept for backward-compatibility */
old_init_definition:
	| CT_INIT OP_ASSIGN init_expression SEMICOLON { $3 }
;


/* We allow here an optional "&" at the beginning and at the end */
init_expression:
	| ampersand_opt init_expression_fol ampersand_opt { $2 }
	| { [ ] }
;

init_expression_fol:
	| init_state_predicate { [ $1 ] }
	| LPAREN init_expression_fol RPAREN { $2 }
	| init_expression_fol AMPERSAND init_expression_fol { $1 @ $3 }
;

/* Used in the init definition */
init_state_predicate:
	| init_loc_predicate { let a,b = $1 in (Parsed_loc_assignment (a,b)) }
    | linear_constraint { Parsed_linear_predicate $1 }
;

init_loc_predicate:
	/* loc[my_pta] = my_loc */
	| CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME { ($3, $6) }
	/* my_pta IS IN my_loc */
	| NAME CT_IS CT_IN NAME { ($1, $4) }
;

new_init_loc_predicate:
	/* loc[my_pta] = my_loc */
	| CT_LOC LSQBRA NAME RSQBRA OP_ASSIGN NAME { ($3, $6) }
	/* my_pta IS IN my_loc */
	| NAME CT_IS CT_IN NAME { ($1, $4) }
;

/************************************************************/
/** NEW INIT DEFINITION SECTION from 3.1: SEPARATION OF DISCRETE AND CONTINUOUS */
/************************************************************/

init_definition:
	| CT_INIT OP_ASSIGN LBRACE new_init_discrete_continuous_definition RBRACE semicolon_opt { $4 }
;

new_init_discrete_continuous_definition:
    | new_init_discrete_definition { $1 }
    | new_init_continuous_definition { $1 }
    | new_init_discrete_definition new_init_continuous_definition { $1 @ $2 }
    | new_init_continuous_definition new_init_discrete_definition { $2 @ $1 }
;

new_init_discrete_definition:
    | CT_DISCRETE OP_EQ new_init_discrete_expression SEMICOLON { $3 }
;

new_init_continuous_definition:
    | CT_CONTINUOUS OP_EQ new_init_continuous_expression SEMICOLON { $3 }
;


new_init_discrete_expression:
	| comma_opt init_discrete_expression_nonempty_list { $2 }
	| { [ ] }
;

init_discrete_expression_nonempty_list :
	| new_init_discrete_state_predicate COMMA init_discrete_expression_nonempty_list  { $1 :: $3 }
	| new_init_discrete_state_predicate comma_opt { [ $1 ] }
;

new_init_discrete_state_predicate:
	| new_init_loc_predicate { let a,b = $1 in (Parsed_loc_assignment (a,b)) }
	| LPAREN new_init_discrete_state_predicate  RPAREN { $2 }
	| NAME OP_ASSIGN expression { Parsed_discrete_predicate ($1, $3) }
;

new_init_continuous_expression:
	| ampersand_opt init_continuous_expression_nonempty_list { $2 }
	| { [ ] }
;

init_continuous_expression_nonempty_list :
	| new_init_continuous_state_predicate AMPERSAND init_continuous_expression_nonempty_list  { $1 :: $3 }
	| new_init_continuous_state_predicate ampersand_opt { [ $1 ] }
;

new_init_continuous_state_predicate:
    | LPAREN new_init_continuous_state_predicate RPAREN { $2 }
    | linear_constraint { Parsed_linear_predicate $1 }
;

expression:
    | boolean_expression { Parsed_global_expression $1 }
;



/************************************************************/
/** MISC. */
/************************************************************/

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
