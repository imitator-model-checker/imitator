/************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Parser for on-the-fly model updates
 *
 * File contributors : Étienne André
 * Created           : 2025/05/20
 *
 ************************************************************/


%{
open ParsingStructure;;
open ParsingStructureUtilities;;
open Exceptions;;
open ImitatorUtilities;;
open DiscreteType;;



(* Check whether n1/n2 is defined and, if not (i.e., if n2 = 0), raises an exception *)
let check_absence_of_division_by_0_or_abort (n1 : NumConst.t) (n2 : NumConst.t) : unit =
	if NumConst.equal n1 NumConst.zero then(
(* 		print_error ("Division by 0 (" ^ (NumConst.string_of_numconst n1) ^ "/" ^ (NumConst.string_of_numconst n2) ^ ") spotted during the parsing!"); *)
		raise (Static_division_by_0 ("" ^ (NumConst.string_of_numconst n1) ^ "/" ^ (NumConst.string_of_numconst n2) ^ ""))
	);
	()


%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> BINARYWORD
%token <string> NAME
/* %token <string> STRING */
%token <ParsingStructure.unexpanded_parsed_model> INCLUDE

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_NEQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token APOSTROPHE COLON COMMA /*DOUBLEDOT */OP_CONJUNCTION OP_DISJUNCTION OP_IMPLIES SEMICOLON

%token
	CT_ACCEPTING CT_ACTION CT_ACTIONS CT_ARRAY CT_AUTOMATON
	CT_BEGIN CT_BINARY_WORD CT_BOOL
	CT_CLOCK CT_CONSTANT CT_CONTINUOUS CT_CONTROLLABLE
	CT_DO CT_DONE CT_DOWNTO
	CT_ELSE CT_END
	CT_FALSE CT_FLOW CT_FOR CT_FORALL CT_FROM CT_FUN
	CT_GOTO
	CT_IF CT_IN  CT_INIT CT_INSIDE CT_INSTANTIATE CT_INT CT_INVARIANT CT_IS
	CT_LOC
	CT_NOT
	CT_PARAMETER
	CT_RATIONAL CT_RETURN
	CT_STOP CT_SYNC CT_SYNCLABS CT_SYNT_VAR
	CT_TEMPLATE CT_THEN CT_TO CT_TRUE
	CT_UNCONTROLLABLE CT_URGENT
	CT_VAR CT_VOID
	CT_WAIT CT_WHEN CT_WHILE
	/*** NOTE: just to forbid their use in the input model and property ***/
	CT_INFINITY CT_NOSYNCOBS CT_OBSERVER CT_OBSERVER_CLOCK CT_SPECIAL_RESET_CLOCK_NAME
    /* CT_BUILTIN_FUNC_RATIONAL_OF_INT  CT_POW CT_SHIFT_LEFT CT_SHIFT_RIGHT CT_FILL_LEFT CT_FILL_RIGHT
    CT_LOG_AND CT_LOG_OR CT_LOG_XOR CT_LOG_NOT CT_ARRAY_CONCAT CT_LIST_CONS */ CT_LIST CT_STACK CT_QUEUE


%token EOF

%right OP_ASSIGN
%right OP_EQ

%left OP_IMPLIES           /* lowest precedence */
%left OP_DISJUNCTION /* CT_OR */
%left OP_CONJUNCTION       /* medium precedence */
/*%left DOUBLEDOT      */      /* high precedence */
%nonassoc CT_NOT           /* highest precedence */

%left OP_PLUS OP_MINUS     /* lowest precedence */
%left OP_MUL OP_DIV        /* highest precedence */


%start              
/* the entry point */
locations
%type <ParsingStructure.parsed_location list> locations
%%
/************************************************************/
/* Parse one or more locations */
/************************************************************/

locations:
	| location location_list EOF { $1 :: $2 }
;


/************************************************************/

location_list:
	| location location_list { $1 :: $2 }
	| { [] }
;


/************************************************************/

location:
	| loc_urgency_accepting_type
	  location_name_and_costs
	  COLON
	  while_or_invariant_or_nothing
	  guard_nonlinear_convex_predicate
	  stopwatches_and_flow_opt
	  wait_opt
	  transitions
	{
		let urgency, accepting = $1 in
		let name, cost = $2 in
		let stopwatches, flow = $6 in
		{
			(* Name *)
			unexpanded_name = name;

			(* Urgent or not? *)
			unexpanded_urgency = urgency;

			(* Accepting or not? *)
			unexpanded_acceptance = accepting;

			(* Cost *)
			unexpanded_cost = cost;

			(* Invariant *)
			unexpanded_invariant = $5;

			(* List of stopped clocks *)
			unexpanded_stopped = stopwatches;

			(* Flow of clocks *)
			unexpanded_flow = flow;

			(* Transitions starting from this location *)
			unexpanded_transitions = $8;
		}
	}
;


seq_code_bloc:
  | { [] }
  | seq_code_bloc_nonempty_list { $1 }
;

/* Bloc of code (instructions, declarations, conditionals, loops) */
seq_code_bloc_nonempty_list:
  | instruction semicolon_or_comma seq_code_bloc_nonempty_list { $1 :: $3 }
  | control_structure seq_code_bloc_nonempty_list { $1 :: $2 }
  | instruction semicolon_or_comma_opt { [$1] }
  | control_structure { [$1] }
;

semicolon_or_comma_opt:
  | {}
  | semicolon_or_comma {}
;

instruction:
  /* local declaration */
  | CT_VAR checked_name_decl COLON var_type_discrete OP_EQ boolean_expression { Parsed_local_decl (($2, Parsing.symbol_start ()), $4, $6) }
  /* assignment */
  | update_without_deprecated { (Parsed_assignment $1) }
  /* instruction without return */
  | boolean_expression { (Parsed_instruction $1) }

;



/** Normal updates without deprecated (avoid parsing errors on function)*/
update_without_deprecated:
	| parsed_scalar_or_index_update_type OP_ASSIGN boolean_expression { $1, $3 }
;

/* Variable or variable access */
parsed_scalar_or_index_update_type:
  | NAME { Parsed_scalar_update ($1, 0) }
  | parsed_scalar_or_index_update_type LSQBRA arithmetic_expression RSQBRA { Parsed_indexed_update ($1, $3) }
;


control_structure:
  /* for loop */
  | CT_FOR NAME CT_FROM arithmetic_expression loop_dir arithmetic_expression CT_DO seq_code_bloc CT_DONE { Parsed_for_loop (($2, Parsing.symbol_start ()), $4, $6, $5, $8) }
  /* while loop */
  | CT_WHILE boolean_expression CT_DO seq_code_bloc CT_DONE { Parsed_while_loop ($2, $4) }
  /* conditional */
  | CT_IF boolean_expression CT_THEN seq_code_bloc CT_END { Parsed_if ($2, $4, None) }
  | CT_IF boolean_expression CT_THEN LPAREN seq_code_bloc RPAREN CT_END { Parsed_if ($2, $5, None) }
  | CT_IF boolean_expression CT_THEN seq_code_bloc CT_ELSE seq_code_bloc CT_END { Parsed_if ($2, $4, Some $6) }
  | CT_IF boolean_expression CT_THEN LPAREN seq_code_bloc RPAREN CT_ELSE LPAREN seq_code_bloc RPAREN CT_END { Parsed_if ($2, $5, Some $9) }
;

loop_dir:
  | CT_TO { Parsed_for_loop_up }
  | CT_DOWNTO { Parsed_for_loop_down }
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
	| name_or_array_access APOSTROPHE OP_EQ arithmetic_expression { ($1, $4) }
;

/************************************************************/

stopwatches:
	| CT_STOP LBRACE name_or_array_access_list RBRACE { $3 }
;

/************************************************************/

transitions:
	| transition transitions { $1 :: $2 }
	| { [] }
;

/************************************************************/

transition:
	| CT_WHEN guard_nonlinear_convex_predicate update_synchronization CT_GOTO NAME SEMICOLON
	{
		let update_list, sync = $3 in
			$2, update_list, sync, $5
	}
;

/************************************************************/

/* A l'origine de 3 conflits ("2 shift/reduce conflicts, 1 reduce/reduce conflict.") donc petit changement */
update_synchronization:
	| { [], UnexpandedNoSync }
	| updates { $1, UnexpandedNoSync }
	| sync_action { [], (UnexpandedSync $1) }
	| updates sync_action { $1, (UnexpandedSync $2) }
	| sync_action updates { $2, (UnexpandedSync $1) }
;

/************************************************************/

updates:
  | CT_DO LBRACE seq_code_bloc RBRACE { $3 }
;

/************************************************************/

sync_action:
	CT_SYNC name_or_array_access { $2 }
;

/************************************************************/

name_or_array_access:
  | NAME { Var_name $1 }
  | NAME LSQBRA arithmetic_expression RSQBRA { Var_array_access ($1, $3) }
;


/************************************************************/
/** ARITHMETIC EXPRESSIONS */
/************************************************************/

arithmetic_expression:
	| arithmetic_term { Parsed_term $1 }
	| arithmetic_expression sum_diff arithmetic_term { Parsed_sum_diff ($1, $3, $2) }
;

sum_diff:
  | OP_PLUS { Parsed_plus }
  | OP_MINUS { Parsed_minus }
;

/* Term over variables and rationals (includes recursion with arithmetic_expression) */
arithmetic_term:
	| arithmetic_factor { Parsed_factor $1 }
	/* Shortcut for syntax rational NAME without the multiplication operator */
	| number NAME { Parsed_product_quotient (Parsed_factor (Parsed_constant ($1)), Parsed_variable ($2, 0), Parsed_mul) }

	| arithmetic_term op_mul_or_div arithmetic_factor {
		(* Try to simplify whenever possible, and detect division by zero (i.e., two numbers of the same type) *)
		match $1, $3 with
		| Parsed_factor (Parsed_constant (ParsedValue.Weak_number_value n1)) , Parsed_constant (ParsedValue.Weak_number_value n2) ->
			if $2 = Parsed_mul then
			Parsed_factor(Parsed_constant (ParsedValue.Weak_number_value (NumConst.mul n1 n2)))
			else(
			(* Check division by 0 *)
			check_absence_of_division_by_0_or_abort n1 n2;
			Parsed_factor(Parsed_constant (ParsedValue.Weak_number_value (NumConst.div n1 n2)))
			)

		| Parsed_factor (Parsed_constant (ParsedValue.Rat_value n1)) , Parsed_constant (ParsedValue.Rat_value n2) ->
			if $2 = Parsed_mul then
			Parsed_factor(Parsed_constant (ParsedValue.Rat_value (NumConst.mul n1 n2)))
			else(
			(* Check division by 0 *)
			check_absence_of_division_by_0_or_abort n1 n2;
			Parsed_factor(Parsed_constant (ParsedValue.Rat_value (NumConst.div n1 n2)))
			)

		| _ -> Parsed_product_quotient ($1, $3, $2)
		}

	| OP_MINUS arithmetic_factor {
		(* Try to simplify whenever possible (i.e., two numbers of the same type) *)
		match $2 with
		| Parsed_constant (ParsedValue.Weak_number_value n) -> Parsed_factor(Parsed_constant (ParsedValue.Weak_number_value (NumConst.neg n)))
		| Parsed_constant (ParsedValue.Rat_value n) -> Parsed_factor(Parsed_constant (ParsedValue.Rat_value (NumConst.neg n)))
		| _ -> Parsed_factor(Parsed_unary_min $2) }
;

op_mul_or_div:
  | OP_MUL { Parsed_mul }
  | OP_DIV { Parsed_div }
;

arithmetic_factor:
  | arithmetic_factor LSQBRA arithmetic_expression RSQBRA { Parsed_access ($1, $3) }
  | NAME LPAREN function_argument_fol RPAREN { Parsed_function_call ($1, $3) }
  | literal_scalar_constant { Parsed_constant $1 }
  | literal_non_scalar_constant { $1 }
  | NAME { Parsed_variable ($1, 0) }
  | LPAREN arithmetic_expression RPAREN { Parsed_nested_expr $2 }
;

literal_scalar_constant:
  | number { $1 }
  | CT_TRUE { ParsedValue.Bool_value true }
  | CT_FALSE { ParsedValue.Bool_value false }
  | binary_word { $1 }
;

literal_non_scalar_constant:
  | literal_array { Parsed_sequence ($1, Parsed_array) }
  | CT_LIST LPAREN literal_array RPAREN { Parsed_sequence ($3, Parsed_list) }
  | CT_STACK LPAREN RPAREN { Parsed_sequence ([], Parsed_stack) }
  | CT_QUEUE LPAREN RPAREN { Parsed_sequence ([], Parsed_queue) }
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

function_argument_fol:
  | boolean_expression COMMA function_argument_fol { $1 :: $3 }
  | boolean_expression { [$1] }
  | { [] }
;

number:
	| integer { ParsedValue.Weak_number_value $1 }
	| float { ParsedValue.Rat_value $1 }
;

binary_word:
        BINARYWORD { ParsedValue.Bin_value (BinaryWord.binaryword_of_string $1) }
;


/************************************************************/
/** RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES */
/************************************************************/

/* We allow an optional "&" at the beginning of a convex predicate (sometimes useful) */
guard_nonlinear_convex_predicate:
	| ampersand_opt guard_nonlinear_convex_predicate_fol { $2 }
;

guard_nonlinear_convex_predicate_fol:
	/** NOTE: this part of the code is necessary (?) to detect convex constraints (with parameters), BUT forbids the use of Boolean expressions without parentheses */
	| discrete_boolean_expression OP_CONJUNCTION guard_nonlinear_convex_predicate_fol { $1 :: $3 }
	| discrete_boolean_expression { [$1] }
;

/** NOTE: more general than a Boolean expression!! notably includes all expressions */
boolean_expression:
	| discrete_boolean_expression { Parsed_discrete_bool_expr $1 }
	| boolean_expression OP_CONJUNCTION boolean_expression { Parsed_conj_dis ($1, $3, Parsed_and) }
	| boolean_expression OP_DISJUNCTION boolean_expression { Parsed_conj_dis ($1, $3, Parsed_or) }
	/* Translate 'a => b' to 'NOT a OR b' */
	| boolean_expression OP_IMPLIES boolean_expression { Parsed_conj_dis ((Parsed_discrete_bool_expr (Parsed_not $1)), $3, Parsed_or) }
;

discrete_boolean_expression:
	| arithmetic_expression { Parsed_arithmetic_expr $1 }

	/* Discrete arithmetic expression of the form Expr ~ Expr */
	| discrete_boolean_expression relop discrete_boolean_expression { Parsed_comparison ($1, $2, $3) }

	/* Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' */
	| arithmetic_expression CT_INSIDE LSQBRA arithmetic_expression semicolon_or_comma arithmetic_expression RSQBRA { Parsed_comparison_in ($1, $4, $6) }

	/* Parsed boolean expression of the form Expr ~ Expr, with ~ in { & | } or not (Expr) */
	| LPAREN boolean_expression RPAREN { Parsed_nested_bool_expr $2 }
	| CT_NOT LPAREN boolean_expression RPAREN { Parsed_not $3 }
;

relop:
	| OP_L { PARSED_OP_L }
	| OP_LEQ { PARSED_OP_LEQ }
	| OP_EQ { PARSED_OP_EQ }
	| OP_NEQ { PARSED_OP_NEQ }
	| OP_GEQ { PARSED_OP_GEQ }
	| OP_G { PARSED_OP_G }
;


/************************************************************/
/** NUMBERS */
/************************************************************/

rational:
	| integer { $1 }
	| float { $1 }
	| integer OP_DIV pos_integer {
		check_absence_of_division_by_0_or_abort $1 $3;
		NumConst.div $1 $3
	}
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
/** MISC. */
/************************************************************/

forall_common_prefix:
  | CT_FORALL NAME CT_IN LSQBRA arithmetic_expression COMMA arithmetic_expression RSQBRA COLON
  {
    { forall_index_name = $2; forall_lb = $5; forall_ub = $7}
  }
;

semicolon_or_comma:
  | SEMICOLON {}
  | COMMA {}
;

comma_opt:
	| COMMA { }
	| { }
;

semicolon_opt:
	| SEMICOLON { }
	| { }
;

ampersand_opt:
	| OP_CONJUNCTION { }
	| { }
;

