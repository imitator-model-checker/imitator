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

open Automaton
open AbstractImitatorFile
open Graph
open Options
open LinearConstraint

(** Function to look up states by an index *)
type lookup_function = state_index -> state

val create_initial_state : unit -> state

val time_elapse : state -> linear_constraint

val post : lookup_function -> state_index -> state_index list

val post_star : state -> reachability_graph * linear_constraint * int * float
	
val print_stats: unit -> unit
