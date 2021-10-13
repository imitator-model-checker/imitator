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
 * Last modified     : 2021/09/16
 *
 ****************************************************************)

(****************************************************************)
(** Names *)
(****************************************************************)

type automaton_name	= string
type location_name	= string
type variable_name	= string
type sync_name		= string


(****************************************************************)
(** Operators *)
(****************************************************************)

(** Boolean operators *)

type parsed_relop = PARSED_OP_L | PARSED_OP_LEQ | PARSED_OP_EQ | PARSED_OP_NEQ | PARSED_OP_GEQ | PARSED_OP_G


(****************************************************************)
(** Declarations *)
(****************************************************************)

(* Specific type of number *)
type var_type_discrete_number =
    | Var_type_discrete_rational
    | Var_type_discrete_int

(* Specific type of discrete variables *)
type var_type_discrete =
    | Var_type_discrete_number of var_type_discrete_number
    | Var_type_discrete_bool
    | Var_type_discrete_binary_word of int
    | Var_type_discrete_array of var_type_discrete * int

(* Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_constant
	| Var_type_discrete of var_type_discrete
	| Var_type_parameter

(****************************************************************)
(** Global expression *)
(****************************************************************)
type global_expression =
    | Parsed_global_expression of parsed_boolean_expression

(****************************************************************)
(** Boolean expressions *)
(****************************************************************)

and parsed_discrete_boolean_expression =
    | Parsed_arithmetic_expression of parsed_discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Parsed_expression of parsed_discrete_boolean_expression * parsed_relop * parsed_discrete_boolean_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Parsed_expression_in of parsed_discrete_arithmetic_expression * parsed_discrete_arithmetic_expression * parsed_discrete_arithmetic_expression
	(** Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Parsed_boolean_expression of parsed_boolean_expression
    (** Parsed boolean expression of the form not(Expr ~ Expr), with ~ = { &, | } *)
	| Parsed_Not of parsed_boolean_expression (** Negation *)


and parsed_boolean_expression =
	| Parsed_True (** True *)
	| Parsed_False (** False *)
	| Parsed_And of parsed_boolean_expression * parsed_boolean_expression (** Conjunction *)
	| Parsed_Or of parsed_boolean_expression * parsed_boolean_expression (** Disjunction *)
	| Parsed_Discrete_boolean_expression of parsed_discrete_boolean_expression

(****************************************************************)
(** Arithmetic expressions for discrete variables *)
(****************************************************************)
and parsed_discrete_arithmetic_expression =
	| Parsed_DAE_plus of parsed_discrete_arithmetic_expression * parsed_discrete_term
	| Parsed_DAE_minus of parsed_discrete_arithmetic_expression * parsed_discrete_term
	| Parsed_DAE_term of parsed_discrete_term

and parsed_discrete_term =
	| Parsed_DT_mul of parsed_discrete_term * parsed_discrete_factor
	| Parsed_DT_div of parsed_discrete_term * parsed_discrete_factor
	| Parsed_DT_factor of parsed_discrete_factor

and parsed_discrete_factor =
	| Parsed_DF_variable of variable_name
	| Parsed_DF_constant of DiscreteValue.discrete_value
	| Parsed_DF_array of parsed_boolean_expression array
    | Parsed_DF_access of parsed_discrete_factor * parsed_discrete_arithmetic_expression
	| Parsed_DF_expression of parsed_discrete_arithmetic_expression
	| Parsed_DF_unary_min of parsed_discrete_factor
	(* Functions *)
	| Parsed_rational_of_int_function of parsed_discrete_arithmetic_expression
	| Parsed_pow_function of parsed_discrete_arithmetic_expression * parsed_discrete_arithmetic_expression

    (* All shift functions of the form : factor * arithmetic_expression *)
	| Parsed_shift_function of parsed_shift_function_type * parsed_discrete_factor * parsed_discrete_arithmetic_expression
    (* All binary log functions of the form : factor * factor *)
    | Parsed_bin_log_function of parsed_bin_log_function_type * parsed_discrete_factor * parsed_discrete_factor
	| Parsed_log_not of parsed_discrete_factor

	| Parsed_array_concat of parsed_discrete_factor * parsed_discrete_factor
(*    | Parsed_user_function of string (* name *) * list (global_expression * var_type_discrete) (* arguments and types *) * var_type_discrete (* return type *)*)

and parsed_shift_function_type =
    | Parsed_shift_left
    | Parsed_shift_right
    | Parsed_fill_left
    | Parsed_fill_right

and parsed_bin_log_function_type =
    | Parsed_log_and
    | Parsed_log_or
    | Parsed_log_xor

(* We allow for some variables (i.e., parameters and constants) a value *)
type variable_declaration = var_type * (variable_name * global_expression option) list
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

type nonlinear_constraint =
    | Parsed_true_nonlinear_constraint (* True *)
    | Parsed_false_nonlinear_constraint (* False *)
    | Parsed_nonlinear_constraint of parsed_discrete_boolean_expression

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


(** Updates on transitions *)
type update =
	| Normal of normal_update (** Updates withput conditions *)
	| Condition of condition_update (** Updates with conditions *)
and variable_access =
    | Variable_name of variable_name
    | Variable_access of variable_access * parsed_discrete_arithmetic_expression
(** basic updating *)
and normal_update = variable_access * global_expression
(** conditional updating *)
and condition_update = parsed_boolean_expression * normal_update list * normal_update list

(* A list of pairs (clock, rational) *)
type parsed_flow = (variable_name * NumConst.t) list

(* Transition = Guard * update list * sync label * destination location *)
type transition = guard * update list * sync * location_name

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
	| Parsed_discrete_predicate of string * global_expression

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
	variable_declarations	: variable_declarations;
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



type parsed_property = {
	(* Emptiness or synthesis *)
	synthesis_type	: parsed_synthesis_type;
	(* Property *)
	property		: parsed_property_type;
	(* Projection *)
	projection		: parsed_projection;
}

(************************************************************)
(************************************************************)
(** Useful data structure to avoid multiple parameters in functions *)
(************************************************************)
(************************************************************)

type useful_parsing_model_information = {
	(* The locations for each automaton: automaton_index -> location_index -> location_name *)
	actions								: Automaton.action_name array;
	array_of_location_names				: location_name array array;
	automata							: Automaton.automaton_index list;
	automata_names						: (Automaton.automaton_index -> automaton_name);
	constants							: (Automaton.variable_name , DiscreteValue.discrete_value) Hashtbl.t;
	discrete							: Automaton.variable_index list;
	index_of_actions					: (Automaton.action_name , Automaton.action_index) Hashtbl.t;
	index_of_automata					: (Automaton.automaton_name , Automaton.automaton_index) Hashtbl.t;
	index_of_locations					: ((Automaton.location_name, Automaton.location_index) Hashtbl.t) array;
	index_of_variables					: (Automaton.variable_name , Automaton.variable_index) Hashtbl.t;
	nb_clocks							: int;
	nb_parameters						: int;
	parameter_names						: variable_name list;
	removed_action_names				: Automaton.action_name list;
	type_of_variables					: Automaton.variable_index -> DiscreteValue.var_type;
	variable_names						: variable_name list;
	variables							: variable_name array;
	removed_variable_names				: variable_name list;
}

type variable_infos = {
	constants							: (Automaton.variable_name , DiscreteValue.discrete_value) Hashtbl.t;
    variable_names						: variable_name list;
	index_of_variables					: (Automaton.variable_name , Automaton.variable_index) Hashtbl.t;
	type_of_variables					: Automaton.variable_index -> DiscreteValue.var_type;
	removed_variable_names				: variable_name list;
}
