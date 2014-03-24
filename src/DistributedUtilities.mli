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
 * Last modified: 2014/03/24
 *
 ****************************************************************)


(****************************************************************)
(** Types *)
(****************************************************************)
type rank = int

type pull_request =
	| PullOnly of rank
	| PullAndResult of rank * LinearConstraint.p_linear_constraint
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

val send_constraint : LinearConstraint.p_linear_constraint -> unit

val send_pi0 : pi0_list -> rank -> unit

val send_work_request : unit -> unit

val send_finished : rank -> unit

val receive_pull_request : unit -> pull_request

val receive_work : unit -> work_assignment
