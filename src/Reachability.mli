(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne, Romain Soulat
 * Created:       2010/07/22
 * Last modified: 2013/03/20
 *
 **************************************************)

open Global
open AbstractModel
open Options
open StateSpace
open LinearConstraint


val create_initial_state : abstract_model -> state

val print_stats: unit -> unit


(************************************************************)
(* Clock elimination *)
(************************************************************)
(* Create data structures for detecting useless clocks (to be called once per model) *)
(** WARNING: should maybe be somewhere else? *)
val prepare_clocks_elimination : abstract_model -> unit


(************************************************************)
(* Full state space exploration *)
(************************************************************)
val full_state_space_exploration : abstract_model -> state -> unit


(************************************************************)
(* Main inverse method function *)
(************************************************************)
val inverse_method_gen : abstract_model -> state -> returned_constraint * StateSpace.reachability_graph * tile_nature * bool * int * float
val inverse_method : abstract_model -> state -> unit
