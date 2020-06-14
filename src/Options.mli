(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Options definitions
 *
 * File contributors : Étienne André, Laure Petrucci
 * Created:       2012/05/10
 * Last modified: 2020/08/28
 *
 ************************************************************)


open AbstractAlgorithm




class imitator_options :
	object

		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Get methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		method acyclic : bool
(* 		method best_worst_case : bool *)
		method branch_and_bound : bool
(* 		method branch_and_bound_unset : unit *)
		method carto_tiles_limit : int option
		method carto_time_limit : int option
		method check_ippta : bool
		method check_point : bool
		method depth_limit : int option
		method depth_init : int option
		method distribution_mode : distribution_mode
		method distributedKillIM : bool
		method draw_cart : bool
(* 		method dynamic : bool *)
		method dynamic_clock_elimination : bool
		method exploration_order : exploration_order
		method files_prefix : string
		method imitator_mode : imitator_mode

		method inclusion : bool
		method is_set_inclusion : bool
		method set_inclusion: bool -> unit

		method inclusion2 : bool

		method merge : bool
		method is_set_merge : bool
		method set_merge : bool -> unit

		(* 		method merge_before : bool *)
		method mergeq : bool
		method is_set_mergeq : bool
		method set_mergeq : bool -> unit

(* 		method merge_before : bool *)
		method merge_heuristic : merge_heuristic
		method model_file_name : string
		method no_acceptfirst : bool
		method nb_args : int
(* 		method no_initprune : bool *)
		method no_leq_test_in_ef : bool
		method no_lookahead : bool
(* 		method no_pending_ordered : bool *)
		method no_random : bool
		method no_time_elapsing : bool
		method no_variable_autoremove : bool
		method output_bc_cart : bool
		method output_bc_result : bool
		method output_cart_x_min : int option
		method output_cart_x_max : int option
		method output_cart_y_min : int option
		method output_cart_y_max : int option
		method output_float : bool

		method output_result : bool
		method is_set_output_result : bool
		method set_output_result : bool -> unit

		method output_tiles_files : bool
		method pending_order : pending_order
		method pi_compatible : bool
		method precomputepi0 : bool
		method property_file_name : string option
		method states_limit : int option
		method statistics : bool
		method sync_auto_detection : bool
		method time_limit : int option
		method timed_mode : bool
		method tree : bool
		method union : bool
		method graphical_state_space : graphical_state_space
		method with_graphics_source : bool
		method states_description : bool

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Set methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		(*** NOTE: these set methods are only used for the learning-based abstraction construction ***)

		method set_file : string -> unit

		method set_files_prefix : string -> unit


		(*** NOTE: this set method is only used for the CUB NZ algorithms ***)
		method set_no_time_elapsing : unit


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Other methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		method parse : unit

		(* Recall options, print info, and check compatibility with the actual algorithm *)
		method recall_and_warn : AbstractModel.abstract_model -> AbstractProperty.abstract_property option -> unit


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Force some options when the mode is cartography *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method set_options_for_cartography : bool -> unit


end
