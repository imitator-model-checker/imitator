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
 * Last modified     : 2020/01/06
 *
 ************************************************************)



(************************************************************)
(** Available algorithms *)
(************************************************************)

type algorithm =
	(** Reachability *)
	| EFsynth of AbstractProperty.state_predicate
	
(*	(** EF-synthesis w.r.t. unsafe locations *)
	| EFunsafe_synthesis
	
	(** EF-minimization *)
	| EF_min
	
	(** EF-maximization *)
	| EF_max
	
	(** EF-synthesis with minimization *)
	| EF_synth_min
	
	(** EF-synthesis with maximization *)
	| EF_synth_max
	
	(** Optimal reachability with priority queue: queue-based, with priority to the earliest successor for the selection of the next state [ABPP19] *)
	| EF_synth_min_priority_queue

	(** EF-synthesis with examples of (un)safe words *)
	| EFexemplify
	
	(** AF-synthesis *)
	| AF_synthesis
	
	(** Parametric loop synthesis *)
	| Loop_synthesis
	
	(** Parametric accepting loop synthesis *)
	| Acc_loop_synthesis
	
	(** Parametric accepting loop synthesis with NDFS exploration *)
	| Acc_loop_synthesis_NDFS

	(** Parametric Büchi-emptiness checking with non-Zenoness (method: check whether the PTA is CUB) *)
	| Parametric_NZ_CUBcheck
	
	(** Parametric Büchi-emptiness checking with non-Zenoness (method: transformation into a CUB-PTA) *)
	| Parametric_NZ_CUBtransform
	
	(** Parametric Büchi-emptiness checking with non-Zenoness (method: transformation into a CUB-PTA, distributed version) *)
	| Parametric_NZ_CUBtransformDistributed
	
	(** Parametric Büchi-emptiness checking with non-Zenoness on a CUB-PTA: hidden option (mainly for testing) *)
	| Parametric_NZ_CUB
	
	(** Parametric deadlock-checking *)
	| Parametric_deadlock_checking
	
	(** Inverse method with convex, and therefore possibly incomplete result *)
	| Inverse_method
	
	(** Inverse method with full, non-convex result*)
	| Inverse_method_complete
	
	(** Parametric reachability preservation *)
	| PRP
	
	(** Cover the whole cartography *)
	| Cover_cartography
	
	(** Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography
	
	(** Look for the border using the cartography*)
	| Border_cartography
	
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography of int
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography of int

	(** Synthesis using iterative calls to PRP *)
	| PRPC*)
	

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
	| State_space_exploration
	
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
	(** NDFS: standard Nested Depth-First Search **)
	| Exploration_NDFS
	(** NDFSsub: NDFS with subsumption [NPvdP18] **)
	| Exploration_NDFS_sub
	(** layerNDFSsub: NDFS with subsumption  and layers [NPvdP18] **)
	| Exploration_layer_NDFS_sub
(*	(** synNDFSsub: NDFS synthesis with subsumption **)
	| Exploration_syn_NDFS_sub
	(** synlayerNDFSsub: NDFS synthesis with subsumption and layers [NPvdP18] **)
	| Exploration_syn_layer_NDFS_sub*)
	(** synMixedNDFS: NDFS synthesis with a mix of subsumption and layers **)
(* 	| Exploration_syn_mixed_NDFS *)


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


(************************************************************)
(** Predicates on mode *)
(************************************************************)

val property_needed              : imitator_mode -> bool
(*val is_mode_IM                   : imitator_mode -> bool*)
val is_mode_cartography          : imitator_mode -> bool
val cartography_drawing_possible : imitator_mode -> bool


(************************************************************)
(** Conversions of modes to string *)
(************************************************************)

val string_of_mode : imitator_mode -> string


