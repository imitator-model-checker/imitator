(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/05/10
 * Last modified: 2016/08/13
 *
 ****************************************************************)
 
open ImitatorUtilities


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



class imitator_options :
	object
		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Get methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		
		method acyclic : bool
(* 		method acyclic_unset : unit *)
		method branch_and_bound : bool
(* 		method branch_and_bound_unset : unit *)
		method cart : bool
		method cartonly : bool
		method carto_tiles_limit : int option
		method carto_time_limit : int option
		method check_ippta : bool
		method check_point : bool
		method completeIM : bool
		(** HACK: property input from CosyVerif *)
		method cosyprop : string
(* 		method counterex : bool *)
		method depth_limit : int option
		method distribution_mode : distribution_mode
		method distributedKillIM : bool
(* 		method dynamic : bool *)
		method dynamic_clock_elimination : bool
		method efim : bool
		method fancy : bool
		method files_prefix : string
		method fromGML : bool
		method imitator_mode : imitator_mode
		(* experimental variant for EFsynth *)
		method new_ef_mode : bool
		method inclusion : bool
		method merge : bool
		method merge_before : bool
		method model_input_file_name : string
		method nb_args : int
		method no_random : bool
		method no_time_elapsing : bool
		method output_bc_cart : bool
		method output_bc_result : bool
		method output_cart_x_min : int option
		method output_cart_x_max : int option
		method output_cart_y_min : int option
		method output_cart_y_max : int option
		method output_result : bool
		method output_tiles_files : bool
		method pi_compatible : bool
		method precomputepi0 : bool
		method pta2clp : bool
		method pta2gml : bool
		method pta2hytech : bool
		method pta2imi : bool
		method pta2jpg : bool
		method pta2tikz : bool
		method second_file_name : string
		method states_limit : int option
		method statistics : bool
		method step : NumConst.t
		method sync_auto_detection : bool
		method time_limit : int option
		method timed_mode : bool
		method tree : bool
		method union : bool
		method output_trace_set : bool
		method output_trace_set_verbose: bool
		method with_graphics_source : bool
		method with_log : bool
(* 		method with_parametric_log : bool *)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Set methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		
		(*** NOTE: set methods are only used for the learning-based abstraction construction ***)
		
		method set_file : string -> unit

		method set_files_prefix : string -> unit

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Other methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		
		method parse : unit
		
		(* Recall options *)
		method recall : unit -> unit
		
end
