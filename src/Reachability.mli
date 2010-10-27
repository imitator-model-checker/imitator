(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/22
 * Last modified: 2010/07/22
 *
 **************************************************)

open AbstractImitatorFile
open Options
open Graph
open LinearConstraint

val create_initial_state : abstract_program -> state

val post : abstract_program -> pi0 -> reachability_graph -> state_index -> state_index list

val post_star : abstract_program -> imitator_options -> pi0 -> state -> 
					 		 reachability_graph * linear_constraint list * int * float
	
val print_stats: unit -> unit