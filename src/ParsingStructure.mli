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
 * Last modified     : 2020/02/12
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

(* The types for global, discrete variables *)
type parsed_var_type_discrete =
	| Parsed_boolean
	| Parsed_rational
	| Parsed_string
	| Parsed_stringset


(* Type of variable in declarations *)
type parsed_var_type =
	| Parsed_var_type_clock
	| Parsed_var_type_constant
	| Parsed_var_type_discrete of parsed_var_type_discrete
	| Parsed_var_type_parameter

(* We allow for some variables (i.e., parameters and constants) a value *)
type constant_value = NumConst.t

(*type variable_declaration =
	| Parsed_variable_declaration of variable_name
	| Parsed_constant_declaration of variable_name * constant_value*)

type variables_declarations = {
	(* Normal variable names: only name, stored in a hash table with their type as key *)
	variables_per_type	: (parsed_var_type , variable_name list) Hashtbl.t;
	(* Properly defined constants 'name=value', either in the 'constants' type, or in another type *)
	constants			: (variable_name * constant_value) list;
	(* Improperly defined constants 'name' without value, in the 'constants' type *)
	unassigned_constants: variable_name list;
}

(*type typed_variable_declarations = parsed_var_type * variable_declaration list

type variable_declarations = typed_variable_declarations list*)


(****************************************************************)
(** Continuous arithmetic expressions *)
(****************************************************************)
type parsed_continuous_arithmetic_expression =
	| Parsed_CAE_plus of parsed_continuous_arithmetic_expression * parsed_continuous_term
	| Parsed_CAE_minus of parsed_continuous_arithmetic_expression * parsed_continuous_term
	| Parsed_CAE_term of parsed_continuous_term

and parsed_continuous_term =
	| Parsed_CT_mul of parsed_continuous_term * parsed_continuous_factor
	| Parsed_CT_div of parsed_continuous_term * parsed_continuous_factor
	| Parsed_CT_factor of parsed_continuous_factor

and parsed_continuous_factor =
	| Parsed_CF_variable of variable_name
	| Parsed_CF_constant of constant_value
	| Parsed_CF_expression of parsed_continuous_arithmetic_expression
	| Parsed_CF_unary_min of parsed_continuous_factor



(****************************************************************)
(** Continuous Boolean expressions *)
(****************************************************************)



(*(** Linear expressions *)

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


type convex_predicate = linear_constraint list*)


(** Boolean expressions *)

type parsed_continuous_inequality =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Parsed_expression of parsed_continuous_arithmetic_expression * parsed_relop * parsed_continuous_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Parsed_expression_in of parsed_continuous_arithmetic_expression * parsed_continuous_arithmetic_expression * parsed_continuous_arithmetic_expression


type parsed_continuous_boolean_expression =
	| Parsed_CBE_True (** True *)
	| Parsed_CBE_False (** False *)
	| Parsed_CBE_Not of parsed_continuous_boolean_expression (** Negation *)
	| Parsed_CBE_And of parsed_continuous_boolean_expression * parsed_continuous_boolean_expression (** Conjunction *)
	| Parsed_CBE_Or of parsed_continuous_boolean_expression * parsed_continuous_boolean_expression (** Disjunction *)
	| Parsed_CBE_continuous_inequality of parsed_continuous_inequality


(** Convex Boolean expression on discrete and/or continuous variables *)
type parsed_convex_continuous_boolean_expression =
	| Parsed_CCBE_True (** True *)
	| Parsed_CCBE_False (** False *)
	| Parsed_CCBE_continuous_inequality of parsed_continuous_inequality

type parsed_convex_continuous_boolean_expressions = parsed_convex_continuous_boolean_expression list


(****************************************************************)
(** Automata *)
(****************************************************************)

(*------------------------------------------------------------*)
(* Updates *)
(*------------------------------------------------------------*)

type parsed_discrete_term =
	(*** NOTE: for parsing, this includes normal clock updates ***)
	| Parsed_continuous_term of parsed_continuous_arithmetic_expression
	(*** TODO ***)
	| Parsed_string_term of string

(** basic updating *)
type normal_update = variable_name * parsed_discrete_term

(** conditional updating - NOTE: it does not support nested conditions *)
type condition_update = parsed_continuous_boolean_expression * normal_update list * normal_update list

(** Updates on transitions *)
type parsed_update =
	| Normal_update of normal_update (** Updates without conditions *)
	| Condition_update of condition_update (** Updates with conditions *)


(*------------------------------------------------------------*)
(* Guards and invariants *)
(*------------------------------------------------------------*)

type parsed_guard		= parsed_convex_continuous_boolean_expressions
type parsed_invariant	= parsed_convex_continuous_boolean_expressions

(*------------------------------------------------------------*)
(* Transitions *)
(*------------------------------------------------------------*)

type parsed_sync =
	| Sync of sync_name
	| NoSync


(* Transition = Guard * update list * sync label * destination location *)
type parsed_transition = {
	parsed_guard		: parsed_guard;
	parsed_updates		: parsed_update list;
	parsed_label		: parsed_sync;
	target_name			: location_name;
}

(*------------------------------------------------------------*)
(* Locations *)
(*------------------------------------------------------------*)

(* Type of locations: urgent or not *)
type parsed_urgency =
	| Parsed_location_urgent
	| Parsed_location_nonurgent

(* Type of locations: accepting or not *)
type parsed_acceptance =
	| Parsed_location_accepting
	| Parsed_location_nonaccepting

(* Location = Name * Urgent type * Accepting type * Cost * Invariant * list of stopped clocks * transitions *)
type parsed_location = {
	(* Name *)
	name        : location_name;
	(* Urgent or not? *)
	urgency     : parsed_urgency;
	(* Accepting or not? *)
	acceptance  : parsed_acceptance;
	(* Cost *)
(* 	cost        : linear_expression option; *)
	(* Invariant *)
	invariant   : parsed_invariant;
	(* List of stopped clocks *)
	stopped     : (variable_name list);
	(* Transitions starting from this location *)
	transitions : parsed_transition list;
}

(* type location = location_name * loc_type * linear_expression option * invariant * (variable_name list) * (transition list) *)

type parsed_automaton = automaton_name * sync_name list * parsed_location list




(****************************************************************)
(** Init definition *)
(****************************************************************)

(** State predicates *)

type parsed_init_state_predicate =
	| Parsed_loc_assignment of automaton_name * location_name
	| Parsed_linear_predicate of parsed_continuous_inequality


type init_definition = parsed_init_state_predicate list


(****************************************************************)
(** Definition of the property *)
(****************************************************************)

(*** TODO: reintroduce ***)
(* type parsed_duration = linear_expression *)


(*** NOTE: for now, we restrict to constants (later should be extended to at least constant expressions) *)
(* type discrete_value = NumConst.t *)

(** Predicates for the definition of the correctness property *)

(*
type parsed_property =
  | Parsed_unreachable_locations of parsed_unreachable_global_location list

  (* DEPRECATED *)
  (* 	| Unreachable_action of sync_name *)

  (* if a2 then a1 has happened before *)
  | Action_precedence_acyclic of sync_name * sync_name
  (* everytime a2 then a1 has happened before *)
  | Action_precedence_cyclic of sync_name * sync_name
  (* everytime a2 then a1 has happened exactly once before *)
  | Action_precedence_cyclicstrict of sync_name * sync_name

  (*** NOT IMPLEMENTED ***)
  (*	(* if a1 then eventually a2 *)
    	| Eventual_response_acyclic of sync_name * sync_name
    	(* everytime a1 then eventually a2 *)
    	| Eventual_response_cyclic of sync_name * sync_name
    	(* everytime a1 then eventually a2 once before next *)
    	| Eventual_response_cyclicstrict of sync_name * sync_name*)

  (* a no later than d *)
  | Action_deadline of sync_name * duration

  (* if a2 then a1 happened within d before *)
  | TB_Action_precedence_acyclic of sync_name * sync_name * duration
  (* everytime a2 then a1 happened within d before *)
  | TB_Action_precedence_cyclic of sync_name * sync_name * duration
  (* everytime a2 then a1 happened once within d before *)
  | TB_Action_precedence_cyclicstrict of sync_name * sync_name * duration

  (* if a1 then eventually a2 within d *)
  | TB_response_acyclic of sync_name * sync_name * duration
  (* everytime a1 then eventually a2 within d *)
  | TB_response_cyclic of sync_name * sync_name * duration
  (* everytime a1 then eventually a2 within d once before next *)
  | TB_response_cyclicstrict of sync_name * sync_name * duration

  (* sequence: a1, ..., an *)
  | Sequence_acyclic of sync_name list
  (* sequence: always a1, ..., an *)
  | Sequence_cyclic of sync_name list


type property_definition  = parsed_property option*)


(****************************************************************)
(** Projection definition *)
(****************************************************************)

type parsed_projection = (variable_name list) option


(****************************************************************)
(** Input model *)
(****************************************************************)

type parsed_model = {
	variable_declarations	: variables_declarations;
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
	| Parsed_rational_boolean_predicate of parsed_continuous_inequality
	| Parsed_loc_predicate of parsed_loc_predicate


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
	| Parsed_state_predicate_true
	| Parsed_state_predicate_false

	
(****************************************************************)
(** Parsed property *)
(****************************************************************)

type parsed_synthesis_type =
	| Parsed_witness
	| Parsed_synthesis



type parsed_property_type =

	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| Parsed_EF of parsed_state_predicate
	
	(* Safety *)
	| Parsed_AGnot of parsed_state_predicate
	
	
(*	(*** TODO: observers! ***)
	(* Reachability *)
	| Parsed_Action_deadline of sync_name * parsed_duration
	
	
	(* Unavoidability *)
	| Parsed_AF of parsed_state_predicate
	
	(* Liveness *)
	| Parsed_AG of parsed_state_predicate*)
	
	
	(*------------------------------------------------------------*)
	(* Optimized reachability *)
	(*------------------------------------------------------------*)
	
	(* Reachability with minimization of a parameter *)
	| Parsed_EFpmin of parsed_state_predicate * variable_name
	
	(* Reachability with minimal-time *)
	| Parsed_EFtmin of parsed_state_predicate

	(*** TODO: EFmin, EFmax, EFsynthmin, EFsynthmax, EF_synth_min_priority_queue ***)
	
	(** EF-synthesis with examples of (un)safe words *)
(* 	| Parsed_EFexemplify of parsed_state_predicate *)
	

	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Infinite-run (cycle) *)
	| Parsed_Cycle

	(** Accepting infinite-run (cycle) *)
	| Parsed_Acc_Cycle of parsed_state_predicate

	(*(** Infinite-run (cycle) with non-Zeno assumption *)
	| Parsed_NZCycle
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Parsed_Deadlock_Freeness

	
	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	*)
	(* Inverse method with complete, non-convex result *)
	| Parsed_IM of parsed_pval

(*	(* Non-complete inverse method with convex result *)
	| Parsed_ConvexIM of parsed_pval

	(* Parametric reachability preservation *)
	| Parsed_PRP of parsed_pval

	
	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)
	
	(* Cartography *)
	| Parsed_Cover_cartography of parsed_pdomain

	
	(** Cover the whole cartography using learning-based abstractions *)
	| Parsed_Learning_cartography of parsed_pdomain
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Parsed_Shuffle_cartography of parsed_pdomain
	
	(** Look for the border using the cartography*)
	| Parsed_Border_cartography of parsed_pdomain
	
	(** Randomly pick up values for a given number of iterations *)
	| Parsed_Random_cartography  of parsed_pdomain * int
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| Parsed_RandomSeq_cartography  of parsed_pdomain * int

	(* Parametric reachability preservation *)
	| Parsed_PRPC of parsed_pdomain*)



type parsed_property = {
	(* Emptiness or synthesis *)
	synthesis_type	: parsed_synthesis_type;
	(* Property *)
	property		: parsed_property_type;
	(* Projection *)
	projection		: parsed_projection;
}

