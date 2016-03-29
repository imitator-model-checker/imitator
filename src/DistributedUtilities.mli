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
 * Last modified: 2016/03/29
 *
 ****************************************************************)


(****************************************************************)
(** Types *)
(****************************************************************)

type rank = int

(** Tags sent by workers *)
type pull_request =
	| PullOnly of rank
	| Tile of rank * Result.abstract_im_result
	| OutOfBound of rank
	(* Subdomain tags *)
(* 	| Tiles of rank * (Result.abstract_im_result list) *)
(* 	| BC_result of rank * Result.bc_result *)
	| Pi0 of rank * PVal.pval
	| UpdateRequest of rank


(** Tags sent by the master *)
type work_assignment =
	| Work of PVal.pval
	| Stop
	(* Subdomain tags *)
	| Subdomain of HyperRectangle.hyper_rectangle
	| TileUpdate of Result.abstract_im_result
	| Terminate
	| Continue



(****************************************************************)
(** Access functions *)
(****************************************************************)


val get_nb_nodes : unit -> int
val get_rank : unit -> rank

(* Check if a node is the master (for master-worker scheme) *)
val is_master : unit -> bool

(* Check if a node is the coordinator (for collaborator-based scheme) *)
val is_coordinator : unit -> bool


(****************************************************************)
(** Send functions *)
(****************************************************************)

(*------------------------------------------------------------*)
(* Send to master / coordinator *)
(*------------------------------------------------------------*)

val send_abstract_im_result : Result.abstract_im_result -> unit

(* val send_abstract_im_result_list : Result.abstract_im_result list -> unit *)

val send_bc_result : Result.bc_result -> unit

val send_work_request : unit -> unit

val send_update_request : unit -> unit


(*------------------------------------------------------------*)
(* Send to worker / collaborator *)
(*------------------------------------------------------------*)
val send_pi0 : PVal.pval -> rank -> unit

(* val send_pi0_worker : PVal.pval -> unit *)


val send_stop : rank -> unit


(** Used for dynamic subdomain *)
val send_subdomain : HyperRectangle.hyper_rectangle -> rank -> unit
val send_terminate : rank -> unit
(** Master sends a tile update to a worker *)
val send_tileupdate : Result.abstract_im_result -> rank -> unit

val send_continue : rank -> unit




(****************************************************************)
(** Receive functions *)
(****************************************************************)

val receive_pull_request : unit -> pull_request

val receive_work : unit -> work_assignment

(* Function used for collaborator - coordinator static distribution scheme *)
val receive_bcresult : unit -> rank * Result.bc_result


