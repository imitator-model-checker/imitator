(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Options definitions
 *
 * File contributors : Étienne André, Laure Petrucci
 * Created:       2012/05/10
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

		method acyclic						: bool
(* 		method best_worst_case : bool *)
		method carto_tiles_limit			: int option
		method carto_time_limit				: int option
		method check_ippta					: bool
		method check_point					: bool

		method comparison_operator			: AbstractAlgorithm.state_comparison_operator
		method is_set_comparison_operator	: bool
		method set_comparison_operator		: AbstractAlgorithm.state_comparison_operator -> unit

		method coverage_pruning				: bool
		(* Cumulative pruning: when a new state is computed, check whether it is included into the previously computed constraints *)
		(*** NOTE: might be expensive in the case of thousands of disjuncts in the computed constraints, cf. [AHW18], therefore this option can be set to false when needed ***)
		method cumulative_pruning			: bool

		(* Algorithm for cycle detection in cycle synthesis algorithms *)
		method cycle_algorithm				: AbstractAlgorithm.cycle_algorithm
		method is_set_cycle_algorithm		: bool
		method set_cycle_algorithm			: AbstractAlgorithm.cycle_algorithm -> unit

		method depth_limit					: int option
		method depth_init					: int option
		method distribution_mode			: distribution_mode
		method distributedKillIM			: bool
		method draw_cart					: bool
		method dynamic_clock_elimination	: bool
		method no_global_time_in_comparison	: bool

		method exploration_order			: AbstractAlgorithm.exploration_order
		method is_set_exploration_order		: bool
		method set_exploration_order		: AbstractAlgorithm.exploration_order -> unit

		method extrapolation				: extrapolation

		method files_prefix					: string
		method imitator_mode				: AbstractAlgorithm.imitator_mode

		method layer						: bool
		method is_set_layer					: bool
		method set_layer					: bool -> unit

		method merge_jump_algorithm			: AbstractAlgorithm.merge_jump_algorithm

		(* Merging heuristic for EFsynthminpq *)
		method merge_EFsynthminpq_heuristic	: AbstractAlgorithm.merge_EFsynthminpq_heuristic

        (* New merge options from IMITATOR 3.3 *)
        method merge_algorithm				: AbstractAlgorithm.merge_algorithm
		method is_set_merge_algorithm		: bool
		method set_merge_algorithm			: AbstractAlgorithm.merge_algorithm -> unit
		
        method merge_candidates				: AbstractAlgorithm.merge_candidates
		method is_set_merge_candidates		: bool
		method set_merge_candidates			: AbstractAlgorithm.merge_candidates -> unit

        method merge_update     			: AbstractAlgorithm.merge_update
		method is_set_merge_update  		: bool
		method set_merge_update 			: AbstractAlgorithm.merge_update -> unit

        method merge_restart 				: bool
		method is_set_merge_restart			: bool
		method set_merge_restart			: bool -> unit

        method model_file_name				: string
		method model_local_file_name		: string
		method no_acceptfirst				: bool
		method nb_args						: int
		method no_green						: bool
		method no_lookahead					: bool
		method no_random					: bool
		method no_time_elapsing				: bool
		method no_variable_autoremove		: bool

		(* New queue-based version of EF (EXPERIMENTAL) *)
		method new_queue_based_EU			: bool

		(* Method used for infinite-run (cycle) with non-Zeno assumption *)
		method nz_method					: AbstractAlgorithm.nz_method
		method is_set_nz_method				: bool
		method set_nz_method				: AbstractAlgorithm.nz_method -> unit

		method output_bc_cart				: bool
		method output_bc_result				: bool
		method output_cart_x_min			: int option
		method output_cart_x_max			: int option
		method output_cart_y_min			: int option
		method output_cart_y_max			: int option
		method output_float					: bool

		method output_result				: bool
		method is_set_output_result			: bool
		method set_output_result			: bool -> unit

		method output_tiles_files			: bool

		(* In game algorithms: set the mode for generation of controller *)
		method ptg_controller_mode			: ptg_controller_mode

		(* In game algorithms: perform the algorithm on-the-fly rather than first build the state space, and then synthesize *)
		method ptg_notonthefly				: bool

		(* In game algorithms: propagate losing states *)
		method ptg_propagate_losing_states	: bool

		method pi_compatible				: bool
		method precomputepi0				: bool
		method property_file_name			: string option
		method states_limit					: int option
		method statistics					: bool

		method subsumption					: bool
		method is_set_subsumption			: bool
		method set_subsumption				: bool -> unit

		method sync_auto_detection			: bool
		method time_limit					: int option
		method timed_mode					: bool
		method graphical_state_space		: graphical_state_space
		method with_graphics_source			: bool
		method states_description			: bool

		method recompute_green				: bool
		method pending_order				: pending_order
		method depth_step					: int option

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
