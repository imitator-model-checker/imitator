/************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Parser for the input model
 *
 * File contributors : Étienne André, Jaime Arias, Benjamin Loillier, Laure Petrucci
 * Created           : 2009/09/07
 *
 ************************************************************/


%{
open ParsingStructure;;
open Exceptions;;
open ImitatorUtilities;;
open DiscreteType;;


let parse_error _ =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	raise (ParsingError (symbol_start, symbol_end))
;;

(*** TODO (Jaime): is it included twice ? ***)
let include_list = ref [];;

let add_parsed_model_to_parsed_model_list parsed_model_list (parsed_model : unexpanded_parsed_model) =
	let merged_controllable_actions : ParsingStructure.unexpanded_parsed_controllable_actions = match parsed_model.unexpanded_controllable_actions, parsed_model_list.unexpanded_controllable_actions with
			| Unexpanded_parsed_no_controllable_actions, Unexpanded_parsed_no_controllable_actions
				-> Unexpanded_parsed_no_controllable_actions

			| Unexpanded_parsed_no_controllable_actions, Unexpanded_parsed_controllable_actions action_names
			| Unexpanded_parsed_controllable_actions action_names, Unexpanded_parsed_no_controllable_actions
				-> Unexpanded_parsed_controllable_actions action_names

			| Unexpanded_parsed_controllable_actions action_names_1, Unexpanded_parsed_controllable_actions action_names_2
				-> Unexpanded_parsed_controllable_actions (OCamlUtilities.list_append action_names_1 action_names_2)

			| Unexpanded_parsed_uncontrollable_actions action_names_1, Unexpanded_parsed_uncontrollable_actions action_names_2
				-> Unexpanded_parsed_uncontrollable_actions (OCamlUtilities.list_append action_names_1 action_names_2)

			| Unexpanded_parsed_no_controllable_actions, Unexpanded_parsed_uncontrollable_actions action_names
			| Unexpanded_parsed_uncontrollable_actions action_names, Unexpanded_parsed_no_controllable_actions
				-> Unexpanded_parsed_uncontrollable_actions action_names

			| Unexpanded_parsed_uncontrollable_actions u_action_names, Unexpanded_parsed_controllable_actions c_action_names
			| Unexpanded_parsed_controllable_actions c_action_names, Unexpanded_parsed_uncontrollable_actions u_action_names
				->
				(*** WARNING (2023/07/10): should be an error ***)
				print_warning ("The submodels define contradictory controllable list of actions (" ^ (ImitatorUtilities.string_of_list_of_name_or_access_with_sep ", " c_action_names) ^ ") AND uncontrollable list of actions (" ^ (ImitatorUtilities.string_of_list_of_name_or_access_with_sep ", " u_action_names) ^ "); the model is ill-formed and its behavior is unspecified!");
				Unexpanded_parsed_controllable_actions c_action_names
		in

	{
                unexpanded_controllable_actions  = merged_controllable_actions;
                unexpanded_variable_declarations = List.append parsed_model.unexpanded_variable_declarations parsed_model_list.unexpanded_variable_declarations;
                unexpanded_fun_definitions       = List.append parsed_model.unexpanded_fun_definitions parsed_model_list.unexpanded_fun_definitions;
                unexpanded_automata              = List.append parsed_model.unexpanded_automata parsed_model_list.unexpanded_automata;
                unexpanded_init_definition       = List.append parsed_model.unexpanded_init_definition parsed_model_list.unexpanded_init_definition;
                template_definitions             = List.append parsed_model.template_definitions parsed_model_list.template_definitions;
                template_calls                   = List.append parsed_model.template_calls parsed_model_list.template_calls;
                synt_declarations                = List.append parsed_model.synt_declarations parsed_model_list.synt_declarations;
	}
;;

let unzip l = List.fold_left
	add_parsed_model_to_parsed_model_list
	{
                unexpanded_controllable_actions  = Unexpanded_parsed_no_controllable_actions;
                unexpanded_variable_declarations = [];
                unexpanded_fun_definitions       = [];
                unexpanded_automata              = [];
                unexpanded_init_definition       = [];
                template_definitions             = [];
                template_calls                   = [];
                synt_declarations                = [];
	}
	(List.rev l)
;;

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
%token APOSTROPHE COLON COMMA DOUBLEDOT OP_CONJUNCTION OP_DISJUNCTION OP_IMPLIES SEMICOLON

%token
  CT_ACCEPTING CT_ACTION CT_ACTIONS CT_ARRAY CT_AUTOMATON
	CT_BEGIN CT_BINARY_WORD CT_BOOL
	CT_CLOCK CT_CONSTANT CT_CONTINUOUS CT_CONTROLLABLE
	CT_DO CT_DONE CT_DOWNTO
	CT_ELSE CT_END
	CT_FALSE CT_FLOW CT_FOR CT_FROM CT_FUN
	CT_GOTO
	CT_IF CT_IN CT_INFINITY CT_INIT CT_INSIDE CT_INSTANTIATE CT_INT CT_INVARIANT CT_IS
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
	CT_NOSYNCOBS CT_OBSERVER CT_OBSERVER_CLOCK CT_SPECIAL_RESET_CLOCK_NAME
    CT_BUILTIN_FUNC_RATIONAL_OF_INT /* CT_POW CT_SHIFT_LEFT CT_SHIFT_RIGHT CT_FILL_LEFT CT_FILL_RIGHT
    CT_LOG_AND CT_LOG_OR CT_LOG_XOR CT_LOG_NOT CT_ARRAY_CONCAT CT_LIST_CONS */ CT_LIST CT_STACK CT_QUEUE


%token EOF

%right OP_ASSIGN
%right OP_EQ

%left OP_IMPLIES           /* lowest precedence */
%left OP_DISJUNCTION /* CT_OR */
%left OP_CONJUNCTION       /* medium precedence */
%left DOUBLEDOT            /* high precedence */
%nonassoc CT_NOT           /* highest precedence */

%left OP_PLUS OP_MINUS     /* lowest precedence */
%left OP_MUL OP_DIV        /* highest precedence */


%start main             /* the entry point */
%type <ParsingStructure.unexpanded_parsed_model> main
%%

/************************************************************/
main:
	controllable_actions_option include_file_list variables_declarations synt_var_decls  decl_fun_lists template_defs automata template_calls init_definition_option
	end_opt EOF
	{
		let controllable_actions = $1 in
		let declarations         = $3 in
    let synt_declarations    = $4 in
		let fun_definitions      = $5 in
		let template_definitions = $6 in
		let automata             = $7 in
		let template_calls       = $8 in
		let init_definition      = $9 in

		let main_model =
{
                        unexpanded_controllable_actions  = controllable_actions;
                        unexpanded_variable_declarations = declarations;
                        unexpanded_fun_definitions       = fun_definitions;
                        unexpanded_automata              = automata;
                        unexpanded_init_definition       = init_definition;
                        template_definitions  = template_definitions;
                        template_calls        = template_calls;
                        synt_declarations     = synt_declarations;
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
  CONTROLLABLE ACTIONS
************************************************************/
controllable_actions_option:
	| CT_CONTROLLABLE CT_ACTIONS COLON name_or_array_access_list SEMICOLON { Unexpanded_parsed_controllable_actions $4 }
	| CT_UNCONTROLLABLE CT_ACTIONS COLON name_or_array_access_list SEMICOLON { Unexpanded_parsed_uncontrollable_actions $4 }
	| { Unexpanded_parsed_no_controllable_actions }
;

/************************************************************
  VARIABLE DECLARATIONS
************************************************************/

/************************************************************/

synt_var_decls:
  | CT_SYNT_VAR synt_var_lists { $2 }
  | { [] }
;

synt_var_lists:
  | synt_var_list COLON synt_var_type SEMICOLON synt_var_lists { ($3, $1) :: $5 }
  | { [] }
;

synt_var_list:
  | checked_name_decl comma_opt { [$1] }
  | checked_name_decl COMMA synt_var_list { $1 :: $3 }
;

synt_var_type:
  | CT_CLOCK CT_ARRAY LPAREN arithmetic_expression RPAREN { $4, Clock_synt_array }
  | CT_ACTION CT_ARRAY LPAREN arithmetic_expression RPAREN { $4, Action_synt_array }
;

/************************************************************
  VARIABLE DECLARATIONS
************************************************************/

/************************************************************/

variables_declarations:
	| CT_VAR decl_var_lists { $2 }
	| { [] }
;


/************************************************************
	INCLUDES
************************************************************/
include_file_list:
	| include_file include_file_list  { $1 :: $2 }
	| { [] }
;

include_file:
	| INCLUDE SEMICOLON { $1 }
;


/************************************************************/

/************************************************************/

decl_var_lists:
	| decl_var_list COLON var_type SEMICOLON decl_var_lists { (($3, $1) :: $5) }
	| { [] }
;

/************************************************************/

decl_var_list:
	| checked_name_decl comma_opt { [($1, None)] }
	| checked_name_decl OP_EQ boolean_expression comma_opt { [($1, Some $3)] }

	| checked_name_decl COMMA decl_var_list { ($1, None) :: $3 }
	| checked_name_decl OP_EQ boolean_expression COMMA decl_var_list { ($1, Some $3) :: $5 }
;

/************************************************************/

template_var_type:
  | var_type { Regular_type $1 }
  | CT_ACTION { Template_action_var }
;

var_type:
	| CT_CLOCK { Var_type_clock }
	| CT_CONSTANT { Var_type_discrete (Dt_number Dt_rat) }
	| CT_PARAMETER { Var_type_parameter }
	| var_type_discrete { Var_type_discrete $1 }
;

var_type_discrete:
    | var_type_discrete_number { Dt_number $1 }
    | CT_VOID { Dt_void }
    | CT_BOOL { Dt_bool }
    | CT_BINARY_WORD LPAREN pos_integer RPAREN { Dt_bin (NumConst.to_bounded_int $3) }
    | var_type_discrete_array { $1 }
    | var_type_discrete_list { $1 }
    | var_type_discrete_stack { $1 }
    | var_type_discrete_queue { $1 }
;

var_type_discrete_array:
  | var_type_discrete CT_ARRAY LPAREN pos_integer RPAREN { Dt_array ($1, NumConst.to_bounded_int $4) }
;

var_type_discrete_list:
  | var_type_discrete CT_LIST { Dt_list $1 }
;

var_type_discrete_stack:
  | var_type_discrete CT_STACK { Dt_stack $1 }
;

var_type_discrete_queue:
  | var_type_discrete CT_QUEUE { Dt_queue $1 }
;

var_type_discrete_number:
    | CT_RATIONAL { Dt_rat }
    | CT_INT { Dt_int }
;

/************************************************************/

decl_fun_lists:
	| decl_fun_nonempty_list { List.rev $1 }
	| { [] }
;

/* Declaration function list */
decl_fun_nonempty_list:
  | decl_fun_def { [$1] }
  | decl_fun_nonempty_list decl_fun_def { $2 :: $1 }
;

/* Function definition */
decl_fun_def:
  | CT_FUN NAME LPAREN fun_parameter_list RPAREN COLON var_type_discrete CT_BEGIN seq_code_bloc return_opt CT_END
  {
    {
      name = $2;
      parameters = List.rev $4;
      return_type = $7;
      body = $9, $10;
    }
  }
;

return_opt:
  | CT_RETURN boolean_expression semicolon_opt { Some $2 }
  | { None }
;

fun_parameter_list:
  | { [] }
  | fun_parameter_nonempty_list { $1 }
;

/* Function parameters list (separated by whitespace) */
fun_parameter_nonempty_list:
  | NAME COLON var_type_discrete { [(($1, Parsing.symbol_start ()), $3)] }
  | fun_parameter_nonempty_list COMMA NAME COLON var_type_discrete { (($3, Parsing.symbol_start ()), $5) :: $1 }
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

/************************************************************/

/************************************************************
  TEMPLATES
************************************************************/

/************************************************************/

template_defs:
  | template_def template_defs { $1 :: $2 }
  | { [] }
;

/************************************************************/

template_def:
  | CT_TEMPLATE NAME LPAREN template_parameter_list RPAREN prolog locations CT_END {
      let body = ($6, $7) in
      { template_name       = $2
      ; template_parameters = List.rev $4
      ; template_body       = body
      }
  }
;

/************************************************************/

template_parameter_list:
  | { [] }
  | template_parameter_nonempty_list { $1 }
;

/************************************************************/

/* TODO: var_type correctly represents the types accepted by templates? */
template_parameter_nonempty_list:
  | NAME COLON template_var_type { [($1, $3)] }
  | template_parameter_nonempty_list COMMA NAME COLON template_var_type { ($3, $5) :: $1 }
;

/************************************************************/

template_calls:
  | template_call template_calls { $1 :: $2 }
  | { [] }
;

/************************************************************/

template_call:
	| CT_INSTANTIATE NAME OP_ASSIGN NAME LPAREN template_args_list RPAREN SEMICOLON
	{
		($2, $4, List.rev $6)
	}
;

template_args_list:
  | { [] }
  | template_args_nonempty_list { $1 }
;

template_args_nonempty_list:
  | template_args_nonempty_list COMMA template_args_elem { $3 :: $1 }
  | template_args_elem { [$1] }
;

template_args_elem:
  | NAME     { Arg_name $1    }
  | integer  { Arg_int $1     }
  | float    { Arg_float $1   }
  | CT_TRUE  { Arg_bool true  }
  | CT_FALSE { Arg_bool false }
;

/************************************************************/

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
	| actions_declarations { $1 }
	| { [] }
;

/************************************************************/


/************************************************************/

actions_declarations:
	| CT_ACTIONS COLON name_or_array_access_list SEMICOLON { $3 }
	/** NOTE: deprecated since 3.4 */
	| CT_SYNCLABS COLON name_or_array_access_list SEMICOLON {
			print_warning ("The syntax `synclabs` is deprecated since version 3.4; please use `actions` instead.");
	$3 }
;

/************************************************************/

name_or_array_access_list:
	| name_or_array_access_nonempty_list { $1 }
	| { [] }
;

/************************************************************/

name_or_array_access_nonempty_list:
	| name_or_array_access COMMA name_or_array_access_nonempty_list { $1 :: $3 }
	| name_or_array_access comma_opt { [$1] }
;

/************************************************************/

locations:
	location locations { $1 :: $2}
	| { [] }
;

/************************************************************/


location:
	| loc_urgency_accepting_type location_name_and_costs COLON while_or_invariant_or_nothing guard_nonlinear_convex_predicate stopwatches_and_flow_opt wait_opt transitions {
		let urgency, accepting = $1 in
		let name, cost = $2 in
		let stopwatches, flow = $6 in
		{
			(* Name *)
			unexpanded_name		= name;
			(* Urgent or not? *)
			unexpanded_urgency		= urgency;
			(* Accepting or not? *)
			unexpanded_acceptance	= accepting;
			(* Cost *)
			unexpanded_cost		= cost;
			(* Invariant *)
			unexpanded_invariant	= $5;
			(* List of stopped clocks *)
			unexpanded_stopped		= stopwatches;
			(* Flow of clocks *)
			unexpanded_flow		= flow;
			(* Transitions starting from this location *)
			unexpanded_transitions = $8;
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
	| name_or_array_access APOSTROPHE OP_EQ name_or_num_lit { ($1, $4) }
;

/************************************************************/

name_or_num_lit:
  /* TODO: In case of array access, should not accept rational values, only integer */
        | rational_linear_expression { Literal $1 }
        | NAME { Const_var $1 }

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
  | NAME LSQBRA name_or_num_lit RSQBRA { Var_array_access ($1, $3) }
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

/************************************************************/
/** OLD INIT DEFINITION SECTION <= 3.0: DISCRETE AND CONTINUOUS mixed together */
/************************************************************/

/* Old init style (until 3.0), kept for backward-compatibility */
old_init_definition:
	| CT_INIT OP_ASSIGN old_init_expression SEMICOLON { $3 }
;


/* We allow here an optional "&" at the beginning and at the end */
old_init_expression:
	| ampersand_opt old_init_expression_fol ampersand_opt { $2 }
	| { [ ] }
;

old_init_expression_fol:
	| old_init_state_predicate { [ $1 ] }
	| LPAREN old_init_expression_fol RPAREN { $2 }
	| old_init_expression_fol OP_CONJUNCTION old_init_expression_fol { $1 @ $3 }
;

/* Used in the init definition */
old_init_state_predicate:
	| old_init_loc_predicate { let a,b = $1 in (Parsed_loc_assignment (a,b)) }
    | init_linear_constraint { Parsed_linear_predicate $1 }
;

old_init_loc_predicate:
	/* loc[my_pta] = my_loc */
	| CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME { ($3, $6) }
	/* my_pta IS IN my_loc */
	| NAME CT_IS CT_IN NAME { ($1, $4) }
;




/************************************************************/
/** NEW INIT DEFINITION SECTION from 3.1: SEPARATION OF DISCRETE AND CONTINUOUS */
/************************************************************/

init_definition:
	| CT_INIT OP_ASSIGN LBRACE init_discrete_continuous_definition RBRACE semicolon_opt { $4 }
;

init_discrete_continuous_definition:
    | init_discrete_definition { $1 }
    | init_continuous_definition { $1 }
    | init_discrete_definition init_continuous_definition { $1 @ $2 }
    | init_continuous_definition init_discrete_definition { $2 @ $1 }
;

init_discrete_definition:
    | CT_RATIONAL OP_EQ init_discrete_expression SEMICOLON { $3 }
;

init_continuous_definition:
    | CT_CONTINUOUS OP_EQ init_continuous_expression SEMICOLON { $3 }
;


init_discrete_expression:
	| comma_opt init_discrete_expression_nonempty_list { $2 }
	| { [ ] }
;

init_discrete_expression_nonempty_list :
	| init_discrete_state_predicate COMMA init_discrete_expression_nonempty_list  { $1 :: $3 }
	| init_discrete_state_predicate comma_opt { [ $1 ] }
;

init_discrete_state_predicate:
	| init_loc_predicate { let a,b = $1 in (Parsed_loc_assignment (a,b)) }
	| LPAREN init_discrete_state_predicate  RPAREN { $2 }
	| NAME OP_ASSIGN boolean_expression { Parsed_discrete_predicate ($1, $3) }
;

init_continuous_expression:
	| ampersand_opt init_continuous_expression_nonempty_list { $2 }
	| { [ ] }
;

init_continuous_expression_nonempty_list :
	| init_continuous_state_predicate OP_CONJUNCTION init_continuous_expression_nonempty_list  { $1 :: $3 }
	| init_continuous_state_predicate ampersand_opt { [ $1 ] }
;

init_continuous_state_predicate:
    | LPAREN init_continuous_state_predicate RPAREN { $2 }
    | init_linear_constraint { Parsed_linear_predicate $1 }
;

init_loc_predicate:
	/* loc[my_pta] = my_loc */
	| CT_LOC LSQBRA NAME RSQBRA OP_ASSIGN NAME { ($3, $6) }
	/* my_pta IS IN my_loc */
	| NAME CT_IS CT_IN NAME { ($1, $4) }
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
	| arithmetic_term product_quotient arithmetic_factor { Parsed_product_quotient ($1, $3, $2) }
	| OP_MINUS arithmetic_factor { Parsed_factor(Parsed_unary_min $2) }
;

product_quotient:
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
/* LINEAR EXPRESSIONS IN INIT DEFINITIONS */
/************************************************************/

init_linear_constraint:
	| linear_expression relop linear_expression { Parsed_linear_constraint ($1, $2, $3) }
	| CT_TRUE { Parsed_true_constraint }
	| CT_FALSE { Parsed_false_constraint }
;

/* Linear expression over variables and rationals */
linear_expression:
	| linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) }
;

/* Linear term over variables and rationals (no recursion, no division) */
linear_term:
	| rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| OP_MINUS NAME { Variable (NumConst.minus_one, $2) }
	| NAME { Variable (NumConst.one, $1) }
	| LPAREN linear_term RPAREN { $2 }
  | NAME LSQBRA pos_integer RSQBRA { Variable (NumConst.one, gen_access_id $1 (NumConst.to_bounded_int $3)) }
;


/* Linear expression over rationals only */
rational_linear_expression:
	| rational_linear_term { $1 }
	| rational_linear_expression OP_PLUS rational_linear_term { NumConst.add $1 $3 }
	| rational_linear_expression OP_MUL rational_linear_term { NumConst.mul $1 $3 }
	| rational_linear_expression OP_DIV rational_linear_term {
		if NumConst.equal $3 NumConst.zero then(
			print_error "Division by 0 spotted during the parsing!";
			raise (InvalidModel)
		)else
			NumConst.div $1 $3
		}
	| rational_linear_expression OP_MINUS rational_linear_term { NumConst.sub $1 $3 } /* linear_term a la deuxieme place */
;

/* Linear term over rationals only */
rational_linear_term:
	| rational { $1 }
	| OP_MINUS rational_linear_term { NumConst.neg $2 }
	| LPAREN rational_linear_expression RPAREN { $2 }
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
		if NumConst.equal $3 NumConst.zero then(
			print_error "Division by 0 spotted during the parsing!";
			raise (InvalidModel)
		)else
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

checked_name_decl:
  | NAME {
    let bad = ref false in
    for i = 0 to String.length $1 - 3 do
      if String.get $1 i = '_' && String.get $1 (i + 1) = '_' && String.get $1 (i + 2) = '_' then
        bad := true;
    done;
    if !bad then
      print_warning "Identifiers with 3 consecutive '_' should not be defined to avoid clashing with expansion of syntatic arrays.";
    $1
  }

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
