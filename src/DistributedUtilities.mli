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
 * Last modified: 2016/02/03
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
	(* Subpart tags *)
	| Tiles of rank * (Result.abstract_im_result list) (** NEW TAG **)
	| Pi0 of rank * PVal.pval
	| UpdateRequest of rank


(** Tags sent by the master *)
type work_assignment =
	| Work of PVal.pval
	| Stop
	(* Subpart tags *)
	| Subpart of HyperRectangle.hyper_rectangle
	| TileUpdate of Result.abstract_im_result
	| Terminate
	| Continue



(****************************************************************)
(** Constants *)
(****************************************************************)

(** Who is the master? *)
val masterrank : rank


(****************************************************************)
(** Access functions *)
(****************************************************************)


val get_nb_nodes : unit -> int
val get_rank : unit -> rank


(****************************************************************)
(** Send functions *)
(****************************************************************)

val send_result : Result.abstract_im_result -> unit

(** Master sends a tile update to a worker *)
val send_tileupdate : Result.abstract_im_result -> rank -> unit

(** Sends a list of tiles from the worker to the master *)
val send_tiles : Result.abstract_im_result list -> unit

val send_pi0 : PVal.pval -> rank -> unit

val send_pi0_worker : PVal.pval -> unit

val send_work_request : unit -> unit

val send_update_request : unit -> unit

val send_subpart : HyperRectangle.hyper_rectangle -> rank -> unit

val send_finished : rank -> unit

val send_terminate : rank -> unit

val send_continue : rank -> unit


(****************************************************************)
(** Receive functions *)
(****************************************************************)

val receive_pull_request : unit -> pull_request

val receive_work : unit -> work_assignment


(****************************************************************)
(** Serialization functions *)
(****************************************************************)

val serialize_pi0 : PVal.pval -> string

val unserialize_pi0 : string -> PVal.pval

val serialize_im_result : Result.abstract_im_result -> string

val unserialize_im_result : string -> Result.abstract_im_result

val unserialize_im_result_list : string -> Result.abstract_im_result list

(** Convert a list of serialized im_result into a serialized list of im_result (ad-hoc function to save time in subparts handling) *)
val serialized_imresultlist_of_serializedimresult_list : string list -> string
