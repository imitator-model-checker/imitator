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
 * Last modified     : 2019/10/09
 *
 ****************************************************************)


(****************************************************************)
(** Names *)
(****************************************************************)

type automaton_name = string
type location_name = string
type variable_name = string
type sync_name = string


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
	| True_constraint (** True *)
	| False_constraint (** False *)
	| Linear_constraint of linear_expression * parsed_relop * linear_expression


type convex_predicate = linear_constraint list


	(** boolean expressions *)
type boolean_expression =
	| True (** True *)
	| False (** False *)
	| Not of boolean_expression (** Negation *)
	| And of boolean_expression * boolean_expression (** Conjunction *)
	| Or of boolean_expression * boolean_expression (** Disjunction *)
	| Discrete_boolan_expression of parsed_discrete_boolan_expression


type parsed_discrete_boolan_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of parsed_discrete_arithmetic_expression * parsed_relop * parsed_discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of parsed_discrete_arithmetic_expression * parsed_discrete_arithmetic_expression * parsed_discrete_arithmetic_expression



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
and condition_update = boolean_expression * normal_update list * normal_update list

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

type automaton = automaton_name * sync_name list * parsed_location list

type automata = automaton list


(****************************************************************)
(** Init definition *)
(****************************************************************)

(** State predicates *)

type state_predicate =
	| Loc_assignment of automaton_name * location_name
	| Linear_predicate of linear_constraint


type init_definition = state_predicate list


(****************************************************************)
(** Definition of the property *)
(****************************************************************)

type duration = linear_expression

(*** NOTE: for now, we restrict to constants (later should be extended to at least constant expressions) *)
type discrete_value = NumConst.t

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

type projection = (variable_name list) option


(****************************************************************)
(** Optimization of a parameter *)
(****************************************************************)

type parsed_optimization =
	| No_parsed_optimization
	| Parsed_minimize of variable_name
	| Parsed_maximize of variable_name



(****************************************************************)
(** Input program *)
(****************************************************************)

(* TODO: transform to structure *)
type parsing_structure =
	variable_declarations
	* automata
	* init_definition
	* property_definition
	* projection
	* parsed_optimization



(****************************************************************)
(** Parsed valuation and valuation domains *)
(****************************************************************)

type pi0 = (string *  NumConst.t) list

type v0 = (string * NumConst.t * NumConst.t) list


(****************************************************************)
(** Predicates for properties *)
(****************************************************************)

type parsed_loc_predicate =
	| Parsed_loc_predicate_EQ of automaton_name * location_name
	| Parsed_loc_predicate_NEQ of automaton_name * location_name


type parsed_simple_predicate =
	| Parsed_discrete_boolean_expression of parsed_discrete_boolan_expression
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

	
(****************************************************************)
(** Parsed property *)
(****************************************************************)

type parsed_synthesis_type =
	| Parsed_exhibition
	| Parsed_synthesis



type parsed_property_type =

	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF of parsed_state_predicate
	
	(* Unavoidability *)
	| AF of parsed_state_predicate
	
	(* Liveness *)
	| AG of parsed_state_predicate
	
	
	(*------------------------------------------------------------*)
	(* Optimized reachability *)
	(*------------------------------------------------------------*)
	
	(* Reachability with minimization *)
	| EFmin of parsed_state_predicate * TODO
	
	(*** TODO: EFmin, EFmax, EFsynthmin, EFsynthmax, EF_synth_min_priority_queue ***)
	
	(** EF-synthesis with examples of (un)safe words *)
	| EFexemplify of parsed_state_predicate
	

	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Infinite-run (cycle) *)
	| Cycle

	(** Accepting infinite-run (cycle) *)
	| Acc_Cycle

	(** Infinite-run (cycle) with non-Zeno assumption *)
	| NZCycle
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Deadlock_Freeness

	
	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	
	(* Inverse method with complete, non-convex result *)
	| IM of pi0

	(* Non-complete inverse method with convex result *)
	| ConvexIM of pi0

	(* Parametric reachability preservation *)
	| PRP of pi0

	
	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)
	
	(* Cartography *)
	| Cover_cartography of v0

	
	(** Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography of v0
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography of v0
	
	(** Look for the border using the cartography*)
	| Border_cartography of v0
	
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography  of v0 * int
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography  of v0 * int

	(* Parametric reachability preservation *)
	| PRPC of v0



type parsed_property = {
	(* Emptiness or synthesis *)
	synthesis_type : parsed_synthesis_type;
	(* Property *)
	property       : parsed_property_type;
}

