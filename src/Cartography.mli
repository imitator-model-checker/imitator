(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/06/18
 * Last modified: 2014/04/16
 *
 ****************************************************************)



(************************************************************)
(** Behavioral cartography auxiliary functions *)
(************************************************************)
(*** BADPROG ***)
type current_pi0 = NumConst.t array

(*** WARNING: inconsistent functions, i.e., somes times v0 and the model are included, some times not ***)

val bc_init : AbstractModel.abstract_model -> AbstractModel.v0 -> unit

val bc_process_im_result : Reachability.im_result -> unit

val bc_finalize : unit -> unit

val bc_result : unit -> AbstractModel.returned_constraint list

val find_next_pi0 : AbstractModel.abstract_model ->  Reachability.im_result -> current_pi0 -> (bool * bool)


(************************************************************)
(** Behavioral cartography algorithms *)
(************************************************************)

val random_behavioral_cartography : AbstractModel.abstract_model -> AbstractModel.v0 -> int -> AbstractModel.returned_constraint list

val cover_behavioral_cartography : AbstractModel.abstract_model -> AbstractModel.v0 -> AbstractModel.returned_constraint list


