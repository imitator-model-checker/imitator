(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Property description
 *
 * File contributors : Étienne André
 * Created           : 2019/10/08
 * Last modified     : 2019/10/09
 *
 ************************************************************)



(************************************************************)
(* Modules *)
(************************************************************)
open Automaton
(* open AbstractModel *)


(****************************************************************)
(** Operators *)
(****************************************************************)

(** Boolean operators *)

type relop = OP_L | OP_LEQ | OP_EQ | OP_NEQ | OP_GEQ | OP_G


(****************************************************************)
(** Arithmetic expressions for discrete variables *)
(****************************************************************)
type discrete_arithmetic_expression =
	| DAE_plus of discrete_arithmetic_expression * discrete_term
	| DAE_minus of discrete_arithmetic_expression * discrete_term
	| DAE_term of discrete_term

and discrete_term =
	| DT_mul of discrete_term * discrete_factor
	| DT_div of discrete_term * discrete_factor
	| DT_factor of discrete_factor

and discrete_factor =
	| DF_variable of variable_name
	| DF_constant of variable_value
	| DF_expression of discrete_arithmetic_expression
	| DF_unary_min of discrete_factor


(****************************************************************)
(** Boolean expressions for discrete variables *)
(****************************************************************)

type discrete_boolan_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression


(****************************************************************)
(** Predicates for properties *)
(****************************************************************)

type loc_predicate =
	| Loc_predicate_EQ of automaton_name * location_name
	| Loc_predicate_NEQ of automaton_name * location_name


type simple_predicate =
	| Discrete_boolean_expression of discrete_boolan_expression
	| Loc_predicate of loc_predicate


type state_predicate_factor =
	| State_predicate_factor_NOT of state_predicate_factor
	| Simple_predicate of simple_predicate
	| State_predicate of state_predicate

and state_predicate_term =
	| State_predicate_term_AND of state_predicate_term * state_predicate_term
	| State_predicate_factor of state_predicate_factor

and state_predicate =
	| State_predicate_OR of state_predicate * state_predicate
	| State_predicate_term of state_predicate_term

(*

(************************************************************)
(** Definition of correctness property *)
(************************************************************)
(** predicates for bad definition *)

type duration = LinearConstraint.p_linear_term


(** Definition of the property by the end user *)
type property =
  (* DEPRECATED *)
  (* 	| Exists_action of action_index *)

  (* An "OR" list of global locations *)
  | Unreachable_locations of unreachable_global_location list

  (* if a2 then a1 has happened before *)
  | Action_precedence_acyclic of action_index * action_index
  (* everytime a2 then a1 has happened before *)
  | Action_precedence_cyclic of action_index * action_index
  (* everytime a2 then a1 has happened exactly once before *)
  | Action_precedence_cyclicstrict of action_index * action_index

  (*** NOTE: not implemented ***)
  (*	(* if a1 then eventually a2 *)
    	| Eventual_response_acyclic of action_index * action_index
    	(* everytime a1 then eventually a2 *)
    	| Eventual_response_cyclic of action_index * action_index
    	(* everytime a1 then eventually a2 once before next *)
    	| Eventual_response_cyclicstrict of action_index * action_index*)

  (* a no later than d *)
  | Action_deadline of action_index * duration

  (* if a2 then a1 happened within d before *)
  | TB_Action_precedence_acyclic of action_index * action_index * duration
  (* everytime a2 then a1 happened within d before *)
  | TB_Action_precedence_cyclic of action_index * action_index * duration
  (* everytime a2 then a1 happened once within d before *)
  | TB_Action_precedence_cyclicstrict of action_index * action_index * duration

  (* if a1 then eventually a2 within d *)
  | TB_response_acyclic of action_index * action_index * duration
  (* everytime a1 then eventually a2 within d *)
  | TB_response_cyclic of action_index * action_index * duration
  (* everytime a1 then eventually a2 within d once before next *)
  | TB_response_cyclicstrict of action_index * action_index * duration

  (* sequence: a1, ..., an *)
  | Sequence_acyclic of action_index list
  (* sequence: always a1, ..., an *)
  | Sequence_cyclic of action_index list

  (* Would be better to have an "option" type *)
  | Noproperty



type correctness_condition = reachability_property option

type projection = (parameter_index list) option

type optimization =
  | No_optimization
  | Minimize of parameter_index
  | Maximize of parameter_index*)


(************************************************************)
(** The actual property *)
(************************************************************)

