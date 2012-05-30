(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne, Romain Soulat
 * Created:       2010/07/22
 * Last modified: 2012/05/30
 *
 **************************************************)

open Global
open AbstractImitatorFile
open Options
open Graph
open LinearConstraint

(** Constraint returned by the inverse method *)
type returned_constraint =
	(** Constraint under convex form *)
	| Convex_constraint of LinearConstraint.linear_constraint
	(** Disjunction of constraints *)
	| Union_of_constraints of LinearConstraint.linear_constraint list


val create_initial_state : abstract_program -> state

val post : abstract_program -> pi0 -> reachability_graph -> state_index -> state_index list

val post_star :
	abstract_program -> pi0 -> state ->
	(* K0 * reachability_graph * nb_iterations * counter_value *)
	returned_constraint * Graph.reachability_graph * int * float

val branch_and_bound :
	abstract_program -> pi0 -> state -> unit

val print_stats: unit -> unit
