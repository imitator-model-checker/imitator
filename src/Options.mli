(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/05/10
 * Last modified: 2015/03/24
 *
 ****************************************************************)
 
open ImitatorUtilities


type distribution_mode =
	(** Normal mode *)
	| Non_distributed
	
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



class imitator_options :
	object
		
		method acyclic : bool
		method acyclic_unset : unit
		method branch_and_bound : bool
		method branch_and_bound_unset : unit
		method cart : bool
		method cartonly : bool
		method check_point : bool
		method completeIM : bool
		method counterex : bool
(* 		method dynamic : bool *)
		method distribution_mode : distribution_mode
		method distributedKillIM : bool
		method dynamic_clock_elimination : bool
		method efim : bool
		method fancy : bool
		method file : string
		method files_prefix : string
(* 		method forcePi0 : bool *)
		method fromGML : bool
		method imitator_mode : imitator_mode
		method inclusion : bool
		method nb_args : int
		method merge : bool
		method merge_before : bool
		method no_random : bool
		method output_cart_x_min : int option
		method output_cart_x_max : int option
		method output_cart_y_min : int option
		method output_cart_y_max : int option
		method pi0file : string
		method pi_compatible : bool
		method post_limit : int option
		method precomputepi0 : bool
		method pta2clp : bool
		method pta2gml : bool
		method pta2jpg : bool
		method pta2tikz : bool
		method states_limit : int option
		method statistics : bool
		method step : NumConst.t
		method sync_auto_detection : bool
		method time_limit : int option
		method timed_mode : bool
		method tree : bool
		method union : bool
		method with_dot : bool
		method with_graphics_source : bool
		method with_log : bool
(* 		method with_parametric_log : bool *)
		method parse : unit
		
		(* Recall options *)
		method recall : unit -> unit
		
end
