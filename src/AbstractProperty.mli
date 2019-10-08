(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, LORIA, CNRS, France
 *
 * Module description: Property description
 *
 * File contributors : Étienne André
 * Created           : 2019/10/08
 * Last modified     : 2019/10/08
 *
 ************************************************************)



(************************************************************)
(* Modules *)
(************************************************************)
open Automaton




(************************************************************)
(** Definition of correctness property *)
(************************************************************)
(** predicates for bad definition *)

type duration = LinearConstraint.p_linear_term

type unreachable_location = automaton_index * location_index

(*** TODO: allow more!!! ***)
type discrete_constraint =
	| Discrete_l of discrete_index * discrete_value
	| Discrete_leq of discrete_index * discrete_value
	| Discrete_equal of discrete_index * discrete_value
	| Discrete_neq of discrete_index * discrete_value
	| Discrete_geq of discrete_index * discrete_value
	| Discrete_g of discrete_index * discrete_value
	| Discrete_interval of discrete_index * discrete_value * discrete_value

(* A global location is a list of locations (at most one per IPTA) and of simple atomic constraints on discrete variables (at most one constraint per discrete variable) *)
type unreachable_global_location = {
  unreachable_locations: unreachable_location list;
  discrete_constraints :  discrete_constraint list;
}

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


type property_definition  = property


(** Reduction to (non-)reachability checking *)

type reachability_property =
  (* Location never reachable *)
  | Unreachable of unreachable_global_location list

  (* Location reachable for each trace *)
  (*** NOTE: not implemented ***)
  | Reachable of unreachable_global_location list (*automaton_index * location_index*)

  (* Combining the two properties *)
  (*** NOTE: not implemented ***)
  | Unreachable_and_reachable of (unreachable_global_location list) * (unreachable_global_location list) (*automaton_index * location_index * automaton_index * location_index*)


type correctness_condition = reachability_property option

type projection = (parameter_index list) option

type optimization =
  | No_optimization
  | Minimize of parameter_index
  | Maximize of parameter_index


(************************************************************)
(** The actual property *)
(************************************************************)

type state_predicate =
	

