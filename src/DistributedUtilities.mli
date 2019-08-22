(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: All common functions needed for the interface with MPI
 * 
 * File contributors : Étienne André, Camille Coti
 * Created           : 2014/03/24
 * Last modified     : 2016/08/15
 *
 ************************************************************)

(************************************************************)
(** Types *)
(************************************************************)

type rank = int

(** Tags sent by workers *)
type pull_request =
	| PullOnly of rank
	| Tile of rank * Result.abstract_point_based_result
	| OutOfBound of rank
	(* Subdomain tags *)
(* 	| Tiles of rank * (Result.abstract_point_based_result list) *)
(* 	| BC_result of rank * Result.cartography_result *)
	| Pi0 of rank * PVal.pval
	| UpdateRequest of rank
	| Good_or_bad_constraint of rank * Result.good_or_bad_constraint 


(** Tags sent by the master *)
type work_assignment =
	| Work of PVal.pval
	| Stop
	(* Subdomain tags *)
	| Subdomain of HyperRectangle.hyper_rectangle
	| TileUpdate of Result.abstract_point_based_result
	| Terminate
	| Continue
	| Initial_state of int



(************************************************************)
(** Access functions *)
(************************************************************)


val get_nb_nodes : unit -> int
val get_rank : unit -> rank

(* Check if a node is the master (for master-worker scheme) *)
val is_master : unit -> bool

(* Check if a node is the coordinator (for collaborator-based scheme) *)
val is_coordinator : unit -> bool


(************************************************************)
(** Send functions *)
(************************************************************)

(*------------------------------------------------------------*)
(* Send to master / coordinator *)
(*------------------------------------------------------------*)

val send_abstract_point_based_result : Result.abstract_point_based_result -> unit

(* val send_abstract_point_based_result_list : Result.abstract_point_based_result list -> unit *)

val send_cartography_result : Result.cartography_result -> unit

val send_work_request : unit -> unit

val send_update_request : unit -> unit

(* Function to send a point from a worker to the master *)
val send_point_to_master : PVal.pval -> unit


(*------------------------------------------------------------*)
(* Send to worker / collaborator *)
(*------------------------------------------------------------*)
val send_pi0 : PVal.pval -> rank -> unit



val send_stop : rank -> unit


(** Used for dynamic subdomain *)
val send_subdomain : HyperRectangle.hyper_rectangle -> rank -> unit
val send_terminate : rank -> unit
(** Master sends a tile update to a worker *)
val send_tileupdate : Result.abstract_point_based_result -> rank -> unit

val send_continue : rank -> unit




(************************************************************)
(** Receive functions *)
(************************************************************)

val receive_pull_request : unit -> pull_request

val receive_work : unit -> work_assignment

(* Function used for collaborator - coordinator static distribution scheme *)
val receive_cartography_result : unit -> rank * Result.cartography_result






(**********NZCUB************)
val send_init_state : int -> rank -> unit

val receive_work_NZCUB : unit -> work_assignment

val receive_pull_request_NZCUB : unit -> pull_request

val send_good_or_bad_constraint : Result.good_or_bad_constraint -> unit

