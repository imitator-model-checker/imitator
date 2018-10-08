(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: Useful and general functions for IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2014/10/24
 * Last modified     : 2018/10/08
 *
 ************************************************************)



(************************************************************)
(** Versioning *)
(************************************************************)

(* Name + version *)
val program_name_and_version : unit -> string

(* Name + version + nickname *)
val program_name_and_version_and_nickname : unit -> string

(* Name + version + build *)
val program_name_and_version_and_build : unit -> string

(* Name + version + nickname + build number *)
val program_name_and_version_and_nickname_and_build : unit -> string

(* Name + version + nickname + build number + build time *)
val program_name_and_version_and_nickname_and_build_time : unit -> string

(** GitHub branch and first 7 characters of git hash (if applicable) *)
val git_branch_and_hash : string

(** GitHub branch and full git hash (if applicable) *)
val git_branch_and_full_hash : string

(* URL of IMITATOR without http:// *)
val imitator_url : string


(************************************************************)
(** Verbosity modes *)
(************************************************************)

type verbose_mode =
	| Verbose_mute
	| Verbose_warnings
	| Verbose_standard
	| Verbose_experiments
	| Verbose_low
	| Verbose_medium
	| Verbose_high
	| Verbose_total


(* Return true if the global verbose mode is greater than 'verbose_mode', false otherwise *)
val verbose_mode_greater : verbose_mode -> bool

(* Convert a string into a verbose_mode; raise Not_found if not found *)
val verbose_mode_of_string : string -> verbose_mode

(* Set the verbose mode *)
val set_verbose_mode : verbose_mode -> unit

(* Get the verbose mode *)
val get_verbose_mode : unit -> verbose_mode


(************************************************************)
(** Global types *)
(************************************************************)

(** Mode for IMITATOR *)
type imitator_mode =
	(** Translation to another language: no analysis *)
	| Translation
	
	(** Classical state space exploration *)
	| State_space_exploration
	
	(** EF-synthesis *)
	| EF_synthesis
	
	(** EF-synthesis w.r.t. unsafe locations *)
	| EFunsafe_synthesis
	
	(** EF-minimization *)
	| EF_min
	
	(** EF-maximization *)
	| EF_max
	
	(** EF-synthesis with minimization *)
	| EF_synth_min
	
	(** EF-synthesis with maximization *)
	| EF_synth_max
	
	(** Optimal reachability with priority queue: queue-based, with priority to the earliest successor for the selection of the next state [work in progress André, Bloemen, Petrucci] *)
	| EF_synth_min_priority_queue

	(** AF-synthesis *)
	| AF_synthesis
	
	(** Parametric loop synthesis *)
	| Loop_synthesis
	
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
	| PRPC
	

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
(** Global time counter *)
(************************************************************)

(* Compute the duration in ms between 2 times *)
(* val duration : float -> float -> float *)

(** Get the value of the counter *)
val get_time : unit -> float

(** Compute the duration since time t *)
val time_from : float -> float

(** Print a number of seconds *)
val string_of_seconds : float -> string

(** Convert a % to a nice string *)
val string_of_percent : float -> string

(** Create a string of the form 'after x seconds', where x is the time since the program started *)
val after_seconds : unit -> string

(** Set the timed mode *)
val set_timed_mode : unit -> unit





(**************************************************)
(** System functions *)
(**************************************************)

val delete_file : string -> unit

(** Obtain a string giving information on the memory used *)
val memory_used : unit -> string

(** Print info on the memory used *)
(* val print_memory_used : verbose_mode -> unit *)


(************************************************************)
(** Messages *)
(************************************************************)

type shell_highlighting_type =
	| Shell_bold
	| Shell_error
	| Shell_normal
	| Shell_result
	| Shell_soundness
	| Shell_warning

(* Print a message if global_verbose_mode >= message_verbose_mode *)
val print_message : verbose_mode -> string -> unit

(* Print a message with some highlighting *)
val print_highlighted_message : shell_highlighting_type -> verbose_mode -> string -> unit

(* Print a warning *)
val print_warning : string -> unit

(* Print an error *)
val print_error : string -> unit

(* Print the name of the contributors *)
val print_contributors : unit -> unit

val print_header_string : unit -> unit



(************************************************************)
(** Terminating functions *)
(************************************************************)
(* Abort program *)
val abort_program : unit -> unit

(* Terminate program *)
val terminate_program : unit -> unit
