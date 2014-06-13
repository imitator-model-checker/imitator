(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre, Camille Coti
 * 
 * Created:       2014/03/24
 * Last modified: 2014/04/16
 *
 ****************************************************************)


(****************************************************************)
(** Types *)
(****************************************************************)
type rank = int

type pull_request =
	| PullOnly of rank
	| PullAndResult of rank * Reachability.im_result
	| OutOfBound of rank


type work_assignment =
	| Work of AbstractModel.pi0
	| Stop

type pi0_list = (Automaton.variable_index * NumConst.t) list


(****************************************************************)
(** Constants *)
(****************************************************************)

(** Who is the master? *)
val masterrank : int


(****************************************************************)
(** Functions *)
(****************************************************************)


val size : unit -> int
val rank : unit -> int

val send_result : (*LinearConstraint.p_linear_constraint*)Reachability.im_result -> unit

val send_pi0 : pi0_list -> rank -> unit

val send_work_request : unit -> unit

val send_finished : rank -> unit

val receive_pull_request : unit -> pull_request

val receive_work : unit -> work_assignment

val serialize_pi0 : (Automaton.variable_index * NumConst.t) list -> string
val unserialize_pi0 : string -> (Automaton.variable_index * NumConst.t) list
val serialize_im_result : Reachability.im_result -> string
val unserialize_im_result : string -> Reachability.im_result
