(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/06/18
 * Last modified: 2014/06/20
 *
 ****************************************************************)


(* List version of pi0 for PaTATOR *)
type pi0_list = (Automaton.variable_index * NumConst.t) list


(************************************************************)
(** Behavioral cartography auxiliary functions *)
(************************************************************)
(*** BADPROG ***)
(* type current_pi0 = NumConst.t array *)

val bc_initialize : unit -> unit

val bc_process_im_result : Reachability.im_result -> unit

val bc_finalize : unit -> unit

val bc_result : unit -> AbstractModel.returned_constraint list

val compute_initial_pi0 : unit -> unit

val find_next_pi0 : AbstractModel.tile_nature option -> (bool * bool)
val find_next_pi0_shuffle : AbstractModel.tile_nature option -> (bool * bool)

(** Get the current pi0 in the form of a list (for PaTATOR) *)
val get_current_pi0 : unit -> pi0_list

(** Get the list of *all* points in V0 (for PaTATOR) *)
val compute_all_pi0 : unit -> pi0_list list

val pi0_in_returned_constraint:
  (LinearConstraint.variable -> LinearConstraint.coef) ->
  AbstractModel.returned_constraint -> bool

(* Move to the next uncovered pi0 and do not move if the current pi0 is still not covered; update global variable current_pi0 (if necessary) *)
val move_to_next_uncovered_pi0 : unit -> bool

(** Try to generate an uncovered random pi0, and gives up after n tries *)
val random_pi0 : int -> bool

(** Compute an array made of *all* points in V0 (for PaTATOR) *)
val compute_all_pi0 : unit -> unit

(** Shuffle the array made of *all* points in V0 (for PaTATOR) *)
val shuffle_all_pi0 : unit -> unit


(************************************************************)
(** Behavioral cartography algorithms *)
(************************************************************)

val random_behavioral_cartography : AbstractModel.abstract_model -> AbstractModel.v0 -> int -> (*AbstractModel.returned_constraint list*)unit

val cover_behavioral_cartography : AbstractModel.abstract_model -> AbstractModel.v0 -> (*AbstractModel.returned_constraint list*)unit


(*
 *  functions used by the coordinator in the distributed-unsupervised
 *  cartography (the coordinator maintaints a list of points instead of
 *  a single one
 *)
val constraint_list_init : int -> unit
val constraint_list_random : unit -> pi0_list option
val constraint_list_update : Reachability.im_result -> unit
val constraint_list_empty : unit -> bool
