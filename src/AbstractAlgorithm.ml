(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Mode and algorithms for IMITATOR
 *
 * File contributors : Étienne André
 * Created           : 2019/12/18
 * Last modified     : 2020/09/10
 *
 ************************************************************)


(************************************************************)
(** Internal modules *)
(************************************************************)
open Exceptions


(************************************************************)
(** Available translations *)
(************************************************************)

type translation =
	| HyTech
	| IMI
	| JPG
	| PDF
	| PNG
	| TikZ
	| Uppaal


(************************************************************)
(** Synthesis *)
(************************************************************)


(** Mode for IMITATOR *)
type imitator_mode =
	(** No analysis, syntactic check only *)
	| Syntax_check

	(** Translation to another language: no analysis *)
	| Translation of translation

	(** Full state space exploration, until fully explored or some preliminary termination *)
	| State_space_computation

	(** Synthesis algorithm *)
	| Algorithm (*of synthesis_algorithm*)



(************************************************************)
(** Options *)
(************************************************************)

type distribution_mode =
	(** Normal mode *)
	| Non_distributed

	(** Distributed mode: static distribution mode (each node has its own part with no communication) *)
	| Distributed_static

	(** Distributed mode: Master slave with sequential pi0 *)
	| Distributed_ms_sequential
	(** Distributed mode: Master slave with sequential pi0 shuffled *)
	| Distributed_ms_shuffle
	(** Distributed mode: Master slave with random pi0 and n retries before switching to sequential mode *)
	| Distributed_ms_random of int
	(** Distributed mode: Master slave with subpart distribution *)
	| Distributed_ms_subpart

	(**  Distributed mode: Workers live their own lives and communicate results to the coordinator  **)
	| Distributed_unsupervised
	(**  Distributed mode: multi-threaded version of Distributed_unsupervised  **)
	| Distributed_unsupervised_multi_threaded


type exploration_order =
	(** Layer-BFS: all states at depth i are computed, and then their successors at depth i+1 [original version] *)
	| Exploration_layer_BFS
	(** Queue-BFS: basic queue, independent of the depth [ANP17] *)
	| Exploration_queue_BFS
	(** Queue-BFS: queue-based, independent of the depth, with ranking system for the selection of the next state [ANP17] *)
	| Exploration_queue_BFS_RS
	(** Queue-BFS: queue-based, independent of the depth, with prior for the selection of the next state [ANP17] *)
	| Exploration_queue_BFS_PRIOR
(*	(** NDFS: standard Nested Depth-First Search **)
	| Exploration_NDFS
	(** NDFSsub: NDFS with subsumption [NPvdP18] **)
	| Exploration_NDFS_sub
	(** layerNDFS: NDFS with layers [NPvdP18] **)
	| Exploration_layer_NDFS
	(** layerNDFSsub: NDFS with subsumption  and layers [NPvdP18] **)
	| Exploration_layer_NDFS_sub*)

type pending_order =
	(** NDFS with layers: order in the pending list exploration **)
	(* no particular order *)
	| Pending_none
	(* biggest parametric projection of zone first *)
	| Pending_param
	(* accepting states first *)
	| Pending_accept
	(* biggest zone first *)
	| Pending_zone

type merge_heuristic =
	(** Merge_always: merge after every processed state *)
	| Merge_always
	(** Merge_always: merge after every processed state for which the target state is a successor of the current state *)
	| Merge_targetseen
	(** Merge_always: merge after every processed state, for every 10th added state to PQ *)
	| Merge_pq10
	(** Merge_always: merge after every processed state, for every 100th added state to PQ *)
	| Merge_pq100
	(** Merge_always: merge after every 10th processed state *)
	| Merge_iter10
	(** Merge_always: merge after every 100th processed state *)
	| Merge_iter100


(** Style of graphical state space to output *)
type graphical_state_space =
	(* No graphical state space *)
	| Graphical_state_space_none
	(* State space with state numbers only*)
	| Graphical_state_space_nodetails
	(* State space with state numbers and locations *)
	| Graphical_state_space_normal
	(* State space with state numbers, locations, constraints and parameter constraints *)
	| Graphical_state_space_verbose


type cycle_algorithm =
	(* Standard BFS algorithm using a variant of Tarjan's strongly connected components algorithm *)
	| Loop
	(* NDFS based algorithm [NPvdP18] *)
	| NDFS


(************************************************************)
(** Predicates on mode *)
(************************************************************)

(* Does the algorithm require a second (property) file in addition to the model? *)
type second_file_need =
	| Second_file_required
	| Second_file_optional
	| Second_file_useless

let property_needed = function
	| Syntax_check
	| State_space_computation
		-> Second_file_useless

	| Translation _
		-> Second_file_optional

	| Algorithm
		-> Second_file_required


let cartography_drawing_possible = function
	| Syntax_check
	| State_space_computation
	| Translation _
		-> false
	| Algorithm (*of synthesis_algorithm*)
		-> true


(************************************************************)
(** Conversions of modes to string *)
(************************************************************)

let string_of_translation = function
	| HyTech -> "HyTech"
	| IMI    -> "IMITATOR"
	| JPG    -> "JPG"
	| PDF    -> "PDF"
	| PNG    -> "PNG"
	| TikZ   -> "TikZ"
	| Uppaal -> "Uppaal"


let string_of_mode (imitator_mode : imitator_mode) : string = match imitator_mode with
	(** No analysis, syntactic check only *)
	| Syntax_check -> "syntax check"

	(** Translation to another language: no analysis *)
	| Translation translation -> "translation to " ^ (string_of_translation translation)

	(** Translation to another language: no analysis *)
	| State_space_computation -> "full symbolic state space exploration "

	(** Synthesis algorithm *)
	| Algorithm (*synthesis_algorithm*) -> "algorithm" (*** TODO: not so precise! ***)



let string_of_exploration_order (exploration_order : exploration_order) : string = match exploration_order with
	(** Layer-BFS: all states at depth i are computed, and then their successors at depth i+1 [original version] *)
	| Exploration_layer_BFS -> "layer-based BFS"
	(** Queue-BFS: basic queue, independent of the depth [ANP17] *)
	| Exploration_queue_BFS -> "queue-based BFS [ACN17]"
	(** Queue-BFS: queue-based, independent of the depth, with ranking system for the selection of the next state [ANP17] *)
	| Exploration_queue_BFS_RS -> "queue-based BFS with ranking system [ACN17]"
	(** Queue-BFS: queue-based, independent of the depth, with prior for the selection of the next state [ANP17] *)
	| Exploration_queue_BFS_PRIOR -> "queue-based BFS with priority [ACN17]"
	(** NDFS: standard Nested Depth-First Search **)
(*	| Exploration_NDFS -> "standard NDFS [NPvdP18]"
	(** NDFSsub: NDFS with subsumption [NPvdP18] **)
	| Exploration_NDFS_sub -> "NDFS with subsumption [NPvdP18]"
	(** layerNDFS: NDFS with layers [NPvdP18] **)
	| Exploration_layer_NDFS -> "NDFS with layers [NPvdP18]"
	(** layerNDFSsub: NDFS with subsumption  and layers [NPvdP18] **)
	| Exploration_layer_NDFS_sub -> "NDFS with subsumption and layers [NPvdP18]"*)


let string_of_cycle_algorithm (cycle_algorithm : cycle_algorithm) : string = match cycle_algorithm with
	| Loop -> "Loop"
	| NDFS -> "NDFS"
