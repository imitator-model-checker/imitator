(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/06/18
 * Last modified: 2014/10/10
 *
 ****************************************************************)


(* List version of pi0 for PaTATOR *)
(* type pi0_list = (Automaton.variable_index * NumConst.t) list *)


(************************************************************)
(** Behavioral cartography auxiliary functions *)
(************************************************************)
(*** BADPROG ***)
(* type current_pi0 = NumConst.t array *)

val bc_initialize : unit -> unit

val bc_process_im_result : Reachability.im_result -> bool

val bc_finalize : unit -> unit

val bc_result : unit -> AbstractModel.returned_constraint list

val compute_initial_pi0 : unit -> unit

(** Compute the next pi0 by sequentially trying all points until a point not covered is found; and then directly modify the internal variable 'current_pi0' (standard BC)
 * Return (found_pi0 : bool, nb_useless_points : int)
 *)
val find_next_pi0 : AbstractModel.tile_nature option -> (bool * bool)

val find_next_pi0_shuffle : AbstractModel.tile_nature option -> (bool * bool)

(** Get the current pi0 (for PaTATOR) *)
val get_current_pi0 : unit -> AbstractModel.pi0

(** Get the list of *all* points in V0 (for PaTATOR) *)
val compute_all_pi0 : unit -> (*pi0_list*)PVal.pval list

val pi0_in_returned_constraint: AbstractModel.pi0 -> AbstractModel.returned_constraint -> bool

(* Move to the next uncovered pi0 and do not move if the current pi0 is still not covered; update global variable current_pi0 (if necessary); return true if indeed moved *)
val move_to_next_uncovered_pi0 : unit -> bool

(** Try to generate an uncovered random pi0, and gives up after n tries *)
val random_pi0 : int -> bool

(** Compute an array made of *all* points in V0 (for PaTATOR) *)
val compute_all_pi0 : unit -> unit

(** Shuffle the array made of *all* points in V0 (for PaTATOR) *)
val shuffle_all_pi0 : unit -> unit

val test_pi0_uncovered : AbstractModel.pi0 -> bool ref -> unit 


(************************************************************)
(** Behavioral cartography algorithms *)
(************************************************************)

val random_behavioral_cartography : AbstractModel.abstract_model -> AbstractModel.v0 -> int -> (*AbstractModel.returned_constraint list*)unit

val cover_behavioral_cartography : AbstractModel.abstract_model -> AbstractModel.v0 -> (*AbstractModel.returned_constraint list*)unit


(*
 *  functions used by the coordinator in the distributed-unsupervised
 *  cartography (the coordinator maintaints a list of points instead of
 *  a single one (added by Sami Evangelista?!)
 *)
val constraint_list_init : int -> unit
val constraint_list_random : unit -> PVal.pval option
val constraint_list_update : Reachability.im_result -> unit
val constraint_list_empty : unit -> bool
