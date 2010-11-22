(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne, Romain Soulat
 * Created:       2010/07/22
 * Last modified: 2010/11/04
 *
 **************************************************)

open Global
open AbstractImitatorFile
open Options
open Graph
open LinearConstraint

val create_initial_state : abstract_program -> state

val post : abstract_program -> imitator_options -> pi0 -> reachability_graph -> state_index -> state_index list

(*val post_star : abstract_program -> imitator_options -> pi0 -> state -> 
					 		 -> reachability_graph * int * float*)				 		 
(*         val post_star :
           AbstractImitatorFile.abstract_program ->
           < dynamic : bool; post_limit : int option;
             time_limit : int option; .. > ->
           (LinearConstraint.variable -> LinearConstraint.coef) ->
           Automaton.location * LinearConstraint.linear_constraint ->
           LinearConstraint.linear_constraint * Graph.reachability_graph *
           int * float
*)
         val post_star :
           abstract_program -> imitator_options -> pi0 -> state ->
           (* K0 * reachability_graph * nb_iterations * counter_value *)
           returned_constraint * Graph.reachability_graph * int * float




val print_stats: unit -> unit