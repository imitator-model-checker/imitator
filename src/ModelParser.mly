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
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2009/09/07
 * Last modified     : 2020/02/04
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


(*------------------------------------------------------------*)
(* Hash table for variable declarations *)
(*------------------------------------------------------------*)
(*** NOTE: the initial size is (an upper bound on) the number of all possible types, including constants ***)
let variables_per_type : (parsed_var_type , variable_name list) Hashtbl.t = Hashtbl.create 10;;

let constants : (variable_name * constant_value) list ref = ref [];;

let unassigned_constants: variable_name list ref = ref [];;


(*------------------------------------------------------------*)
(*** TODO (Jaime): is it included twice ? ***)
let include_list = ref [];;

(*let add_parsed_model_to_parsed_model_unified parsed_model_unified parsed_model =
	(* We need to fuse variable declarations *)

	(* Easy part: append lists *)
	let constants = List.append parsed_model.variable_declarations.constants parsed_model_unified.variable_declarations.constants in
	let unassigned_constants = List.append parsed_model.variable_declarations.unassigned_constants parsed_model_unified.variable_declarations.unassigned_constants in

	
	(* Less easy part: fuse Hashtable *)
	
	(* Get the unified hash table *)
	let unified_hashtable = parsed_model_unified.variable_declarations.variables_per_type in
	
	(* Iterate on the smaller model to fuse into the unified model *)
	Hashtbl.iter (fun parsed_var_type variable_names ->
		(* Try to get the previous list *)
		let existing_list = try (
			Hashtbl.find unified_hashtable parsed_var_type
		) with Not_found -> [] in
		
		(* Replace with the new list *)
		Hashtbl.replace unified_hashtable parsed_var_type (List.rev_append existing_list variable_names);
		
	) parsed_model.variable_declarations.variables_per_type;
	
	let variables_declarations = {
		variables_per_type	= unified_hashtable;
		constants			= constants;
		unassigned_constants= unassigned_constants;
	} in



	{
		variable_declarations	= variables_declarations;
		automata				= List.append parsed_model.automata parsed_model_unified.automata;
		init_definition			= List.append parsed_model.init_definition parsed_model_unified.init_definition;
	}
;;

let unzip l = List.fold_left
	add_parsed_model_to_parsed_model_unified
	{
		variable_declarations	= {
			variables_per_type	= variables_per_type;
			constants			= [];
			unassigned_constants= [];
		};
		automata				= [];
		init_definition			= [];
	}
	(List.rev l)
;;
(*------------------------------------------------------------*)*)



(** We also defined two types to differentiate assigned and unassigned constants *)
type variable_declaration = 
	| Parsed_variable_declaration of variable_name
	| Parsed_constant_declaration of variable_name * constant_value


%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> NAME
/* %token <string> STRING */
%token <ParsingStructure.parsed_model> INCLUDE

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_NEQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token AMPERSAND APOSTROPHE COLON COMMA DOUBLEDOT PIPE SEMICOLON

%token
	CT_ACCEPTING CT_ALWAYS CT_AND CT_AUTOMATON
	CT_BAD CT_BEFORE CT_BOOLEAN
	CT_CLOCK CT_CONSTANT
	CT_DO
	CT_ELSE CT_END CT_EVENTUALLY CT_EVERYTIME
	CT_FALSE
	CT_GOTO
	CT_HAPPENED CT_HAS
	CT_IF CT_IN CT_INIT CT_INITIALLY CT_INVARIANT
	CT_LOC CT_LOCATIONS
	CT_NEXT CT_NOT
	CT_ONCE CT_OR
	CT_PARAMETER
	CT_RATIONAL CT_REGION
	CT_SEQUENCE CT_STOP CT_STRING CT_STRINGSET CT_SYNC CT_SYNCLABS
	CT_THEN CT_TRUE
	CT_UNKNOWN CT_URGENT
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
%type <ParsingStructure.parsed_model> main
%%

/************************************************************/
main:
	declarations automata init_definition end_opt EOF
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
		
(*		let included_model = unzip !include_list in

		(* Return the parsed model *)
		add_parsed_model_to_parsed_model_unified included_model main_model*)
(*		{
			variable_declarations	= (List.append incl_decl declarations);
			automata				= (List.append incl_automata automata);
			init_definition			= (List.append incl_init init_definition);
		}*)
		
		
		(*** NOTE: Temporarily disable file inclusion ***)
		main_model

	(*** TODO: cleanup ***)
(* 		resolve_property (bad::incl_prop), projection_definition, optimization_definition, carto *)
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
	| include_file_list CT_VAR decl_var_lists {
		(* Return the built hash table and the constants *)
		
		{
			(* Normal variable names: only name, stored in a hash table with their type as key *)
			variables_per_type	= variables_per_type;
			(* Properly defined constants 'name=value', either in the 'constants' type, or in another type *)
			constants			= !constants;
			(* Improperly defined constants 'name' without value, in the 'constants' type *)
			unassigned_constants= !unassigned_constants;
		}
	
	}
	
	| {
		(* Empty list: still return the (empty) structures *)
		{
			(* Normal variable names: only name, stored in a hash table with their type as key *)
			variables_per_type	= variables_per_type;
			(* Properly defined constants 'name=value', either in the 'constants' type, or in another type *)
			constants			= !constants;
			(* Improperly defined constants 'name' without value, in the 'constants' type *)
			unassigned_constants= !unassigned_constants;
		}
	}
;


/************************************************************/

/************************************************************/

decl_var_lists:
	| decl_var_list COLON var_type SEMICOLON decl_var_lists {
(* 		(($3, $1) :: $5) *)
		
		(*** NOTE: an exception to the usual parsing customs here: we already simplify the way variables and constants declarations are passed to the ModelConverter ***)
		
		let var_type		= $3 in
		let decl_var_list	= $1 in
(* 		let decl_var_lists = $5 in *)
		
		(* First differentiate between assigned and unassigned variable names *)
		let assigned_variables, unassigned_variables = List.partition
			(function 
				| Parsed_constant_declaration _ -> true
				| _ -> false
			)
			decl_var_list
		in
		
		(* Remove the tags *)
		let assigned_variables = List.map
			(function 
				| Parsed_constant_declaration (constant_name, value) -> (constant_name, value)
				| _ -> raise (InternalError "Something different from Parsed_constant_declaration was found in a list in ModelParser.mly, although it was filtered before")
			)
			assigned_variables
		in
		let unassigned_variables = List.map
			(function 
				| Parsed_variable_declaration variable_name -> variable_name
				| _ -> raise (InternalError "Something different from Parsed_variable_declaration was found in a list in ModelParser.mly, although it was filtered before")
			)
			unassigned_variables
		in

		(* Filter out constants (assigned names) and add them to the constants list *)
		constants := List.rev_append assigned_variables !constants;
		
		(* If current type is constants *)
		if var_type = Parsed_var_type_constant then(
			(* Unassigned variable names are ill-formed: add to unassigned constants *)
			unassigned_constants := List.rev_append unassigned_variables !unassigned_constants;
			
		(* Otherwise: normal type => add the variables to the proper type *)
		)else(
			(* Get the list of variables already declared and corresponding to this type 'var_type' *)
			let already_declared_variables_for_this_type = try (Hashtbl.find variables_per_type var_type)
				with Not_found -> []
			in
			
			(* Add the list *)
			Hashtbl.replace variables_per_type var_type (List.rev_append unassigned_variables already_declared_variables_for_this_type);
			
		)
		;
		
		(* Return nothing for this rule *)
		()
		
	}
	
	| { () }
;

/************************************************************/

decl_var_list:
	/* Last element */
	| variable_declaration comma_opt { [$1] }

	/* Not last element */
	| variable_declaration COMMA decl_var_list { $1 :: $3 }
;

/************************************************************/

/* Single variable declaration with or without assignment */
variable_declaration:
	| NAME { Parsed_variable_declaration $1 }
	/* TODO: add type for this constant! */
	| NAME OP_EQ rational_expression { Parsed_constant_declaration ($1, $3) }
;


/************************************************************/

var_type:
	/* Clocks */
	| CT_CLOCK { Parsed_var_type_clock }

	/* Constants */
	| CT_CONSTANT { Parsed_var_type_constant }
	
	/* Parameters */
	| CT_PARAMETER { Parsed_var_type_parameter }
	
	/* Discrete variables */
	| CT_BOOLEAN	{ Parsed_var_type_discrete Parsed_boolean }
	| CT_RATIONAL	{ Parsed_var_type_discrete Parsed_rational }
	| CT_STRING		{ Parsed_var_type_discrete Parsed_string }
	| CT_STRINGSET	{ Parsed_var_type_discrete Parsed_stringset }
;

/************************************************************
  AUTOMATA
************************************************************/

/************************************************************/

automata:
	| automaton automata { $1 :: $2 }
	| include_file automata {
		include_list := $1 :: !include_list;
		$2
		}
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
	| sync_labels { $1 }
	| { [] }
	/* Initialization is NOT taken into account anymore */
/* 	| initialization sync_labels { $2 } */
/* 	| sync_labels initialization { $1 } */
/* 	| initialization { [] } */
;

/************************************************************/

/*** NOTE: syntax removed from version 3 */
/*
initialization:
	| CT_INITIALLY NAME state_initialization SEMICOLON {
		(* Print a warning because this syntax is deprecated and not taken into account! *)
		print_warning ("The syntax 'initially " ^ $2 ^ "' is deprecated and is NOT taken into account. Initial locations must be defined in the initial state definition.");
		()
	}
;

state_initialization:
	| AMPERSAND convex_predicate {}
	| {}
;
*/

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

whilekw_or_invariantkw_or_nothing:
	/* From 2018/02/22, "while" may be be replaced with invariant */
	/* From 2019/12, "while" should be be replaced with invariant */
	| CT_WHILE {
		print_warning ("The syntax 'while [invariant]' is deprecated; you should use 'invariant [invariant]' instead.");
		()
		}
	| CT_INVARIANT {}
	| {}
;

location:
	| loc_urgency_accepting_type location_name_and_costs COLON whilekw_or_invariantkw_or_nothing invariant stopwatches wait_opt transitions {
		let urgency, accepting = $1 in
		let name, cost = $2 in
		{
			(* Name *)
			name			= name;
			(* Urgent or not? *)
			urgency			= urgency;
			(* Accepting or not? *)
			acceptance		= accepting;
			(* Cost *)
(* 			cost = cost; *)
			(* Invariant *)
			invariant		= $5;
			(* List of stopped clocks *)
			stopped			= $6;
			(* Transitions starting from this location *)
			transitions		= $8;
		}
	}
;

invariant:
	| convex_continuous_boolean_expressions { $1 }
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
	/*** NOTE: costs temporarily disabled
	| NAME LSQBRA linear_expression RSQBRA { $1, Some $3 }
	*/
;

wait_opt:
	| CT_WAIT {
			print_warning ("The syntax 'wait' in invariants is deprecated.");
		()
	}
	| CT_WAIT LBRACE RBRACE {
			print_warning ("The syntax 'wait {}' in invariants is deprecated.");
		()
	}
	/* Now deprecated and not accepted anymore */
/* 	| LBRACE RBRACE { } */

	/* Default: nothing */
	| { }
;

/************************************************************/

stopwatches:
	| CT_STOP LBRACE name_list RBRACE { $3 }
	| { [] }
;

/************************************************************/

transitions:
	| transition transitions { $1 :: $2 }
	| { [] }
;

/************************************************************/

transition:
	| CT_WHEN guard update_synchronization CT_GOTO NAME SEMICOLON
	{
		let update_list, sync = $3 in
			$2, update_list, sync, $5
	}
;


/************************************************************/

guard:
	| convex_continuous_boolean_expressions { $1 }
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
	| update COMMA update_list { Normal $1 :: $3}
	| update { [Normal $1] }
	| condition_update COMMA update_list { Condition $1 :: $3}
	| condition_update { [Condition $1] }
;

/************************************************************/

/** Normal updates */
update:
	/** NOTE: from 2018/02/22: assign becomes recommended; main syntax from version 3 */
	| NAME OP_ASSIGN continuous_arithmetic_expression { ($1, Parsed_continuous_term $3) }

	/* Deprecated since version 3 */
	| NAME APOSTROPHE OP_EQ continuous_arithmetic_expression {
		print_warning ("The update syntax \"" ^ $1 ^ "' = …\" is deprecated. Consider using \"" ^ $1 ^ " := …\" instead.");
		($1, Parsed_continuous_term $4)
	}
	
	/** NOTE: from 2018/02/22: assign becomes recommended; deprecated since version 3 */
	| NAME APOSTROPHE OP_ASSIGN continuous_arithmetic_expression {
		print_warning ("The update syntax \"" ^ $1 ^ "' := …\" is deprecated. Consider using \"" ^ $1 ^ " := …\" instead.");
		($1, Parsed_continuous_term $4)
	}
	
	/** NOTE: from 2018/02/22: apostrophe becomes optional; deprecated since version 3  */
	| NAME OP_EQ continuous_arithmetic_expression {
		print_warning ("The update syntax \"" ^ $1 ^ " = …\" is deprecated. Consider using \"" ^ $1 ^ " := …\" instead.");
		($1, Parsed_continuous_term $3)
	}
;

/** List containing only normal updates.
		NOTE: it is used to avoid nested conditional updates */
normal_update_list:
	| update COMMA normal_update_list { $1 :: $3}
	| update { [$1]}
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

continuous_arithmetic_expression:
	| continuous_arithmetic_term { Parsed_CAE_term $1 }
	| continuous_arithmetic_expression OP_PLUS continuous_arithmetic_term { Parsed_CAE_plus ($1, $3) }
	| continuous_arithmetic_expression OP_MINUS continuous_arithmetic_term { Parsed_CAE_minus ($1, $3) }
;

/* Term over variables and rationals (includes recursion with continuous_arithmetic_expression) */
continuous_arithmetic_term:
	| continuous_arithmetic_factor { Parsed_CT_factor $1 }
	/* Shortcut for syntax rational NAME without the multiplication operator */
	| rational NAME { Parsed_CT_mul (Parsed_CT_factor (Parsed_CF_constant $1), Parsed_CF_variable $2) }
	| continuous_arithmetic_term OP_MUL continuous_arithmetic_factor { Parsed_CT_mul ($1, $3) }
	| continuous_arithmetic_term OP_DIV continuous_arithmetic_factor { Parsed_CT_div ($1, $3) }
	| OP_MINUS continuous_arithmetic_term { Parsed_CT_mul($2, Parsed_CF_constant NumConst.minus_one) }
;

continuous_arithmetic_factor:
	| rational { Parsed_CF_constant $1 }
	| NAME { Parsed_CF_variable $1 }
	| LPAREN continuous_arithmetic_expression RPAREN { Parsed_CF_expression $2 }
;


/************************************************************/
/** RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES */
/************************************************************/

/* We allow an optional "&" at the beginning of a convex predicate (sometimes useful) */
convex_continuous_boolean_expressions:
	| ampersand_opt convex_continuous_boolean_expressions_fol { $2 }
;

convex_continuous_boolean_expressions_fol:
	| convex_continuous_boolean_expression AMPERSAND convex_continuous_boolean_expressions_fol { $1 :: $3 }
	| convex_continuous_boolean_expression { [$1] }
;


convex_continuous_boolean_expression:
	| CT_TRUE { Parsed_CCBE_True }
	| CT_FALSE { Parsed_CCBE_False }
	| rational_boolean_expression { Parsed_CCBE_continuous_arithmetic_expression $1 }
;

/*
convex_predicate:
	| ampersand_opt convex_predicate_fol { $2 }
;

convex_predicate_fol:
	| linear_constraint AMPERSAND convex_predicate { $1 :: $3 }
	| linear_constraint { [$1] }
;

linear_constraint:
	| linear_expression relop linear_expression { Parsed_linear_constraint ($1, $2, $3) }
	| CT_TRUE { Parsed_true_constraint }
	| CT_FALSE { Parsed_false_constraint }
;
*/

relop:
	| OP_L   { PARSED_OP_L }
	| OP_LEQ { PARSED_OP_LEQ }
	| OP_EQ  { PARSED_OP_EQ }
	| OP_NEQ { PARSED_OP_NEQ }
	| OP_GEQ { PARSED_OP_GEQ }
	| OP_G   { PARSED_OP_G }
;

convex_relop:
	| OP_L   { PARSED_OP_L }
	| OP_LEQ { PARSED_OP_LEQ }
	| OP_EQ  { PARSED_OP_EQ }
	| OP_GEQ { PARSED_OP_GEQ }
	| OP_G   { PARSED_OP_G }
;

/* Linear expression over variables and rationals */
/*
linear_expression:
	| linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) }
;
*/

/* Linear term over variables and rationals (no recursion, no division) */
/*
linear_term:
	| rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| OP_MINUS NAME { Variable (NumConst.minus_one, $2) }
	| NAME { Variable (NumConst.one, $1) }
	| LPAREN linear_term RPAREN { $2 }
;
*/

/* Linear expression over rationals only */
rational_expression:
	| rational_linear_term { $1 }
	| rational_expression OP_PLUS rational_linear_term { NumConst.add $1 $3 }
	| rational_expression OP_MUL rational_linear_term { NumConst.mul $1 $3 }
	| rational_expression OP_MINUS rational_linear_term { NumConst.sub $1 $3 } /* linear term at second position */
;

/* Linear term over rationals only */
rational_linear_term:
	| rational { $1 }
	| OP_MINUS rational { NumConst.neg $2 }
	| LPAREN rational_linear_term RPAREN { $2 }
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
	| CT_TRUE { Parsed_True }
	| CT_FALSE { Parsed_False }
	| OP_NEQ LPAREN boolean_expression RPAREN { Parsed_Not $3 }
	| boolean_expression AMPERSAND boolean_expression { Parsed_And ($1, $3) }
	| boolean_expression PIPE boolean_expression { Parsed_Or ($1, $3) }
	| rational_boolean_expression { Parsed_continuous_boolean_expression $1 }
;

rational_boolean_expression:
	/* Discrete arithmetic expression of the form Expr ~ Expr */
	| continuous_arithmetic_expression relop continuous_arithmetic_expression { Parsed_expression ($1, $2, $3) }

	/* Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' */
	| continuous_arithmetic_expression CT_IN LSQBRA continuous_arithmetic_expression COMMA continuous_arithmetic_expression RSQBRA { Parsed_expression_in ($1, $4, $6) }
	/* allowed for convenience */
	| continuous_arithmetic_expression CT_IN LSQBRA continuous_arithmetic_expression SEMICOLON continuous_arithmetic_expression RSQBRA { Parsed_expression_in ($1, $4, $6) }
;

/************************************************************/
/** INIT DEFINITION */
/************************************************************/

init_definition:
	| CT_INIT OP_ASSIGN init_expression SEMICOLON { $3 }
	| { [ ] }
;


/* We allow here an optional "&" at the beginning */
init_expression:
	| ampersand_opt init_expression_fol { $2 }
	| { [ ] }
;

init_expression_fol:
	| init_state_predicate { [ $1 ] }
	| LPAREN init_expression_fol RPAREN { $2 }
	| init_expression_fol AMPERSAND init_expression_fol { $1 @ $3 }
;

/* Used in the init definition */
init_state_predicate:
	| loc_predicate { let a,b = $1 in (Parsed_loc_assignment (a,b)) }
	| rational_boolean_expression { Parsed_linear_predicate $1 }
;

loc_predicate:
	| CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME { ($3, $6) }
;


/************************************************************/
/** MISC. */
/************************************************************/

comma_opt:
	| COMMA { }
	| { }
;

/*
semicolon_opt:
	| SEMICOLON { }
	| { }
;
*/

ampersand_opt:
	| AMPERSAND { }
	| { }
;
