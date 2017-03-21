(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/05/10
 * Last modified: 2017/03/21
 *
 ****************************************************************)
 
open ImitatorUtilities




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
		method exploration_order : exploration_order
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
		method no_variable_autoremove : bool
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
(* 		method pta2clp : bool *)
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
