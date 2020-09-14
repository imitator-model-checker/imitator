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
 * Last modified     : 2020/09/14
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
(* Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_constant
	| Var_type_discrete
	| Var_type_parameter

(* We allow for some variables (i.e., parameters and constants) a value *)
type var_value = NumConst.t

type variable_declaration = var_type * (variable_name * var_value option) list

type variable_declarations = variable_declaration list


(****************************************************************)
(** Arithmetic expressions for discrete variables *)
(****************************************************************)
type parsed_discrete_arithmetic_expression =
	| Parsed_DAE_plus of parsed_discrete_arithmetic_expression * parsed_discrete_term
	| Parsed_DAE_minus of parsed_discrete_arithmetic_expression * parsed_discrete_term
	| Parsed_DAE_term of parsed_discrete_term

and parsed_discrete_term =
	| Parsed_DT_mul of parsed_discrete_term * parsed_discrete_factor
	| Parsed_DT_div of parsed_discrete_term * parsed_discrete_factor
	| Parsed_DT_factor of parsed_discrete_factor

and parsed_discrete_factor =
	| Parsed_DF_variable of variable_name
	| Parsed_DF_constant of var_value
	| Parsed_DF_expression of parsed_discrete_arithmetic_expression
	| Parsed_DF_unary_min of parsed_discrete_factor



(****************************************************************)
(** Convex predicates and linear expressions *)
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


type convex_predicate = linear_constraint list


(** boolean expressions *)

type parsed_discrete_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Parsed_expression of parsed_discrete_arithmetic_expression * parsed_relop * parsed_discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Parsed_expression_in of parsed_discrete_arithmetic_expression * parsed_discrete_arithmetic_expression * parsed_discrete_arithmetic_expression


type parsed_boolean_expression =
	| Parsed_True (** True *)
	| Parsed_False (** False *)
	| Parsed_Not of parsed_boolean_expression (** Negation *)
	| Parsed_And of parsed_boolean_expression * parsed_boolean_expression (** Conjunction *)
	| Parsed_Or of parsed_boolean_expression * parsed_boolean_expression (** Disjunction *)
	| Parsed_Discrete_boolean_expression of parsed_discrete_boolean_expression




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
(** basic updating *)
and normal_update = variable_name * parsed_discrete_arithmetic_expression
(** conditional updating - NOTE: it does not support nested conditions *)
and condition_update = parsed_boolean_expression * normal_update list * normal_update list

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
	(* Transitions starting from this location *)
	transitions : transition list;
}

(* type location = location_name * loc_type * linear_expression option * invariant * (variable_name list) * (transition list) *)

type parsed_automaton = automaton_name * sync_name list * parsed_location list




(****************************************************************)
(** Init definition *)
(****************************************************************)

(** State predicates *)

type parsed_init_state_predicate =
	| Parsed_loc_assignment of automaton_name * location_name
	| Parsed_linear_predicate of linear_constraint


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
	(* Reachability and specification illustration *)
	(*------------------------------------------------------------*)
	
	(** EF-synthesis with examples of (un)safe words *)
 	| Parsed_EFexemplify of parsed_state_predicate
	

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



type parsed_property = {
	(* Emptiness or synthesis *)
	synthesis_type	: parsed_synthesis_type;
	(* Property *)
	property		: parsed_property_type;
	(* Projection *)
	projection		: parsed_projection;
}

