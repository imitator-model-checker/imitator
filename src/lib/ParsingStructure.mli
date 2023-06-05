(*****************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2009/09/08
 *
 ****************************************************************)

(****************************************************************)
(** Names *)
(****************************************************************)

type variable_index = int
type automaton_name	= string
type location_name	= string
type sync_name		= string

type variable_name = string
type variable_id = int
type variable_ref = variable_name * variable_id

(****************************************************************)
(** Operators *)
(****************************************************************)

(** Boolean operators *)

type parsed_relop = PARSED_OP_L | PARSED_OP_LEQ | PARSED_OP_EQ | PARSED_OP_NEQ | PARSED_OP_GEQ | PARSED_OP_G


(****************************************************************)
(** Declarations *)
(****************************************************************)

(* Type of the sequence *)
type parsed_sequence_type =
    | Parsed_array
    | Parsed_list
    | Parsed_stack
    | Parsed_queue

(* Type of boolean operator *)
type parsed_conj_dis =
    | Parsed_and
    | Parsed_or

(****************************************************************)
(** Boolean expressions *)
(****************************************************************)
type parsed_boolean_expression =
    | Parsed_conj_dis of parsed_boolean_expression * parsed_boolean_expression * parsed_conj_dis (* Conjunction / Disjunction *)
	| Parsed_discrete_bool_expr of parsed_discrete_boolean_expression

and parsed_discrete_boolean_expression =
    | Parsed_arithmetic_expr of parsed_discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Parsed_comparison of parsed_discrete_boolean_expression * parsed_relop * parsed_discrete_boolean_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Parsed_comparison_in of parsed_discrete_arithmetic_expression * parsed_discrete_arithmetic_expression * parsed_discrete_arithmetic_expression
	(** Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Parsed_nested_bool_expr of parsed_boolean_expression
    (** Parsed boolean expression of the form not(Expr ~ Expr), with ~ = { &, | } *)
	| Parsed_not of parsed_boolean_expression (** Negation *)

(****************************************************************)
(** Arithmetic expressions for discrete variables *)
(****************************************************************)
and parsed_discrete_arithmetic_expression =
    | Parsed_sum_diff of parsed_discrete_arithmetic_expression * parsed_discrete_term * parsed_sum_diff
	| Parsed_term of parsed_discrete_term

and parsed_sum_diff =
    | Parsed_plus
    | Parsed_minus

and parsed_discrete_term =
	| Parsed_product_quotient of parsed_discrete_term * parsed_discrete_factor * parsed_product_quotient
	| Parsed_factor of parsed_discrete_factor

and parsed_product_quotient =
    | Parsed_mul
    | Parsed_div

and parsed_discrete_factor =
	| Parsed_variable of variable_ref
	| Parsed_constant of ParsedValue.parsed_value
	| Parsed_sequence of parsed_boolean_expression list * parsed_sequence_type
    | Parsed_access of parsed_discrete_factor * parsed_discrete_arithmetic_expression
	| Parsed_nested_expr of parsed_discrete_arithmetic_expression
	| Parsed_unary_min of parsed_discrete_factor
	| Parsed_function_call of variable_name (* name *) * parsed_boolean_expression list (* arguments *)



(* We allow for some variables (i.e., parameters and constants) a value *)
type variable_declaration = DiscreteType.var_type * (variable_name * parsed_boolean_expression option) list
type variable_declarations = variable_declaration list

(****************************************************************)
(** Convex predicates, linear and non-linear expressions *)
(****************************************************************)

(** Linear expressions *)

type linear_term =
	| Constant of  NumConst.t
	| Variable of  NumConst.t * variable_name


type linear_expression =
	| Linear_term of linear_term
	| Linear_plus_expression of linear_expression * linear_term
	| Linear_minus_expression of linear_expression * linear_term


type linear_constraint =
	| Parsed_true_constraint (** True *)
	| Parsed_false_constraint (** False *)
	| Parsed_linear_constraint of linear_expression * parsed_relop * linear_expression

(** Non-linear expressions *)
type nonlinear_constraint = parsed_discrete_boolean_expression

type convex_predicate = nonlinear_constraint list

(****************************************************************)
(** Automata *)
(****************************************************************)
(* Type of locations: urgent or not *)
type parsed_urgency =
	| Parsed_location_urgent
	| Parsed_location_nonurgent

(* Type of locations: accepting or not *)
type parsed_acceptance =
	| Parsed_location_accepting
	| Parsed_location_nonaccepting

type sync =
	| Sync of sync_name
	| NoSync

type guard = convex_predicate
type invariant = convex_predicate

(* Variable name or variable access (x or x[index]) *)
type parsed_scalar_or_index_update_type =
    | Parsed_scalar_update of variable_ref
    | Parsed_indexed_update of parsed_scalar_or_index_update_type * parsed_discrete_arithmetic_expression

type update_mode = Scalar_update_mode | Indexed_update_mode

(** basic updating *)
type normal_update = parsed_scalar_or_index_update_type * parsed_boolean_expression

type parsed_loop_dir =
    | Parsed_for_loop_up
    | Parsed_for_loop_down

(****************************************************************)
(** Bloc of sequential code *)
(****************************************************************)
type parsed_instruction =
    | Parsed_local_decl of variable_ref * DiscreteType.var_type_discrete * parsed_boolean_expression (* init expr *)
    | Parsed_assignment of normal_update
    | Parsed_instruction of parsed_boolean_expression
    | Parsed_for_loop of variable_ref * parsed_discrete_arithmetic_expression (* from *) * parsed_discrete_arithmetic_expression (* to *) * parsed_loop_dir (* up or down *) * parsed_seq_code_bloc (* inner bloc *)
    | Parsed_while_loop of parsed_boolean_expression (* condition *) * parsed_seq_code_bloc (* inner bloc *)
    | Parsed_if of parsed_boolean_expression (* condition *) * parsed_seq_code_bloc (* then *) * parsed_seq_code_bloc option (* else *)

and parsed_seq_code_bloc = parsed_instruction list

(****************************************************************)
(** User functions *)
(****************************************************************)

(* Metadata of a function *)
type function_metadata = {
    name : variable_name;
    parameter_refs : variable_ref list;
    signature_constraint : FunctionSig.signature_constraint;
    side_effect : bool;
}

(* Parsed function definition *)
type parsed_fun_definition = {
    name : variable_name; (* function name *)
    parameters : (variable_ref * DiscreteType.var_type_discrete) list; (* parameter names, ids and types *)
    return_type : DiscreteType.var_type_discrete; (* return type *)
    body : parsed_seq_code_bloc * parsed_boolean_expression option; (* body *)
}

(* Parsed function definition list *)
type parsed_fun_definition_list = parsed_fun_definition list

(* Shortcuts to hash table types *)
type functions_meta_table = (string, function_metadata) Hashtbl.t
type parsed_functions_table = (string, parsed_fun_definition) Hashtbl.t

(* A list of pairs (clock, rational) *)
type parsed_flow = (variable_name * NumConst.t) list

(* Transition = Guard * update list * sync label * destination location *)
type transition = guard * parsed_seq_code_bloc * sync * location_name

(* Location = Name * Urgent type * Accepting type * Cost * Invariant * list of stopped clocks * transitions *)
type parsed_location = {
	(* Name *)
	name        : location_name;
	(* Urgent or not? *)
	urgency     : parsed_urgency;
	(* Accepting or not? *)
	acceptance  : parsed_acceptance;
	(* Cost *)
	cost        : linear_expression option;
	(* Invariant *)
	invariant   : invariant;
	(* List of stopped clocks *)
	stopped     : (variable_name list);
	(* Flow of clocks *)
	flow        : parsed_flow;
	(* Transitions starting from this location *)
	transitions : transition list;
}


type parsed_automaton = automaton_name * sync_name list * parsed_location list




(****************************************************************)
(** Init definition *)
(****************************************************************)

(** State predicates *)

type parsed_init_state_predicate =
	| Parsed_loc_assignment of automaton_name * location_name
	| Parsed_linear_predicate of linear_constraint
	| Parsed_discrete_predicate of variable_name * parsed_boolean_expression

type init_definition = parsed_init_state_predicate list

(****************************************************************)
(** Definition of the property *)
(****************************************************************)

type parsed_duration = linear_expression


(****************************************************************)
(** Projection definition *)
(****************************************************************)

type parsed_projection = (variable_name list) option


(****************************************************************)
(** Input model *)
(****************************************************************)

type parsed_model = {
	controllable_actions	: sync_name list;
	variable_declarations	: variable_declarations;
	fun_definitions         : parsed_fun_definition_list;
	automata				: parsed_automaton list;
	init_definition			: init_definition;
}



(****************************************************************)
(** Parsed valuation and valuation domains *)
(****************************************************************)

type parsed_pval = (string *  NumConst.t) list

type parsed_pdomain = (string * NumConst.t * NumConst.t) list


(****************************************************************)
(** Predicates for properties *)
(****************************************************************)

type parsed_loc_predicate =
	| Parsed_loc_predicate_EQ of automaton_name * location_name
	| Parsed_loc_predicate_NEQ of automaton_name * location_name


type parsed_simple_predicate =
	| Parsed_discrete_boolean_expression of parsed_discrete_boolean_expression
	| Parsed_loc_predicate of parsed_loc_predicate
	| Parsed_state_predicate_true
	| Parsed_state_predicate_false
	| Parsed_state_predicate_accepting

type parsed_state_predicate_factor =
	| Parsed_state_predicate_factor_NOT of parsed_state_predicate_factor
	| Parsed_simple_predicate of parsed_simple_predicate
	| Parsed_state_predicate of parsed_state_predicate

and parsed_state_predicate_term =
	| Parsed_state_predicate_term_AND of parsed_state_predicate_term * parsed_state_predicate_term
	| Parsed_state_predicate_factor of parsed_state_predicate_factor

and parsed_state_predicate =
	| Parsed_state_predicate_OR of parsed_state_predicate * parsed_state_predicate
	| Parsed_state_predicate_term of parsed_state_predicate_term

	
(****************************************************************)
(** Parsed property *)
(****************************************************************)

type parsed_synthesis_type =
	| Parsed_witness
	| Parsed_synthesis
	| Parsed_exemplify


(* Observer patterns [Andre13] *)
type parsed_pattern =
	(* if a2 then a1 has happened before *)
	| Parsed_action_precedence_acyclic of sync_name * sync_name
	(* everytime a2 then a1 has happened before *)
	| Parsed_action_precedence_cyclic of sync_name * sync_name
	(* everytime a2 then a1 has happened once before *)
	| Parsed_action_precedence_cyclicstrict of sync_name * sync_name

	(* a within d *)
	| Parsed_action_deadline of sync_name * parsed_duration
	
	(* if a2 then a1 happened within d before *)
	| Parsed_TB_Action_precedence_acyclic of sync_name * sync_name * parsed_duration
	(* everytime a2 then a1 happened within d before *)
	| Parsed_TB_Action_precedence_cyclic of sync_name * sync_name * parsed_duration
	(* everytime a2 then a1 happened once within d before *)
	| Parsed_TB_Action_precedence_cyclicstrict of sync_name * sync_name * parsed_duration

	(* if a1 then eventually a2 within d *)
	| Parsed_TB_response_acyclic of sync_name * sync_name * parsed_duration
	(* everytime a1 then eventually a2 within d *)
	| Parsed_TB_response_cyclic of sync_name * sync_name * parsed_duration
	(* everytime a1 then eventually a2 within d once before next *)
	| Parsed_TB_response_cyclicstrict of sync_name * sync_name * parsed_duration

	(* sequence a1, …, an *)
	| Parsed_Sequence_acyclic of sync_name list
	(* always sequence a1, …, an *)
	| Parsed_Sequence_cyclic of sync_name list



type parsed_property_type =

	(*------------------------------------------------------------*)
	(* Basic properties *)
	(*------------------------------------------------------------*)

	(* Validity *)
	| Parsed_Valid


	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| Parsed_EF of parsed_state_predicate
	
	(* Safety *)
	| Parsed_AGnot of parsed_state_predicate
	
	
(*	
	(* Unavoidability *)
	| Parsed_AF of parsed_state_predicate
	
	(* Liveness *)
	| Parsed_AG of parsed_state_predicate*)
	
	
	(*------------------------------------------------------------*)
	(* Optimized reachability *)
	(*------------------------------------------------------------*)
	
	(* Reachability with minimization of a parameter *)
	| Parsed_EFpmin of parsed_state_predicate * variable_name
	
	(* Reachability with maximization of a parameter *)
	| Parsed_EFpmax of parsed_state_predicate * variable_name
	
	(* Reachability with minimal-time *)
	| Parsed_EFtmin of parsed_state_predicate


	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Accepting infinite-run (cycle) through a state predicate *)
	| Parsed_Cycle_Through of parsed_state_predicate

	(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Parsed_Cycle_Through_generalized of parsed_state_predicate list

	(** Infinite-run (cycle) with non-Zeno assumption: method by checking whether the PTA is already a CUB-PTA for some valuation *)
	| Parsed_NZ_Cycle
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Parsed_Deadlock_Freeness

	
	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	(* Inverse method with complete, non-convex result *)
	| Parsed_IM of parsed_pval

	(* Non-complete, non-deterministic inverse method with convex result *)
	| Parsed_ConvexIM of parsed_pval

	(* Parametric reachability preservation *)
	| Parsed_PRP of parsed_state_predicate * parsed_pval

	(* Variant IMK of the Inverse method *)
	| Parsed_IMK of parsed_pval

	(* Variant IMunion of the Inverse method *)
	| Parsed_IMunion of parsed_pval
	

	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)
	
	(* Cartography *)
	| Parsed_Cover_cartography of parsed_pdomain * NumConst.t

	(** Cover the whole cartography using learning-based abstractions *)
	| Parsed_Learning_cartography of parsed_state_predicate * parsed_pdomain * NumConst.t
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Parsed_Shuffle_cartography of parsed_pdomain * NumConst.t
	
	(** Look for the border using the cartography*)
	| Parsed_Border_cartography of parsed_pdomain * NumConst.t
	
	(** Randomly pick up values for a given number of iterations *)
	| Parsed_Random_cartography of parsed_pdomain * int * NumConst.t
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| Parsed_RandomSeq_cartography of parsed_pdomain * int * NumConst.t

	(* Parametric reachability preservation *)
	| Parsed_PRPC of parsed_state_predicate * parsed_pdomain * NumConst.t


	(*------------------------------------------------------------*)
	(* Observer patterns *)
	(*------------------------------------------------------------*)
	
	| Parsed_pattern of parsed_pattern

	(*------------------------------------------------------------*)
	(* Games *)
	(*------------------------------------------------------------*)

	(* Parametric timed game: reachability condition *)
	| Parsed_Win of parsed_state_predicate



type parsed_property = {
	(* Emptiness or synthesis *)
	synthesis_type	: parsed_synthesis_type;
	(* Property *)
	property		: parsed_property_type;
	(* Projection *)
	projection		: parsed_projection;
}

type declarations_info = {
    clock_names : variable_name list;
    parameter_names : variable_name list;
    discrete_names : variable_name list;
    variable_refs : (variable_ref, DiscreteType.var_type) Hashtbl.t;
}

(************************************************************)
(************************************************************)
(** Useful data structure to avoid multiple parameters in functions *)
(************************************************************)
(************************************************************)
type constants_table = (Automaton.variable_name , AbstractValue.abstract_value) Hashtbl.t

type variable_infos = {
	constants : constants_table;
	variables : variable_name array;
    variable_names : variable_name list;
	index_of_variables : (Automaton.variable_name , Automaton.variable_index) Hashtbl.t;
	type_of_variables : Automaton.variable_index -> DiscreteType.var_type;
	removed_variable_names : variable_name list;
	discrete : Automaton.variable_index list;
	variable_refs : (variable_ref, DiscreteType.var_type) Hashtbl.t;
	fun_meta : (Automaton.variable_name, function_metadata) Hashtbl.t;
}

type useful_parsing_model_information = {
	(* The locations for each automaton: automaton_index -> location_index -> location_name *)
	actions								: Automaton.action_name array;
	array_of_location_names				: location_name array array;
	automata							: Automaton.automaton_index list;
	automata_names						: (Automaton.automaton_index -> automaton_name);
	index_of_actions					: (Automaton.action_name , Automaton.action_index) Hashtbl.t;
	index_of_automata					: (Automaton.automaton_name , Automaton.automaton_index) Hashtbl.t;
	index_of_locations					: ((Automaton.location_name, Automaton.location_index) Hashtbl.t) array;
	nb_clocks							: int;
	nb_parameters						: int;
	parameter_names						: variable_name list;
	removed_action_names				: Automaton.action_name list;
	variable_infos                      : variable_infos;
}
