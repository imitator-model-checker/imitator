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
 * File contributors : Ulrich Kühne, Étienne André, Laure Petrucci
 * Created           : 2010
 * Last modified     : 2020/09/09
 *
 ************************************************************)



(************************************************************)
(* External modules *)
(************************************************************)
open Arg


(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open OCamlUtilities
open ImitatorUtilities
open AbstractAlgorithm


(************************************************************)
(* Class-independent functions *)
(************************************************************)

(* Returns a `type` from a `type option`. Raises Exception InternalError if it is equal to None ***)
let value_of_option option_name (a : 'a option) : 'a = match a with
	| Some value -> value
	| None -> raise (InternalError ("Option `" ^ option_name ^ "` is not yet initialized."))


(* Warn if an option is already set; this helps to detect cases when both `-inclusion` and `-no-inclusion` are called, for example *)
let warn_if_set option_value option_name =
	if option_value <> None then(
		print_warning ("Option `" ^ option_name ^ "` may be set to two different values. Behavior is unspecified.");
	)



(************************************************************)
(* The class *)
(************************************************************)

class imitator_options =
	object (self)
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		val mutable nb_args = 0


		(* INPUT OPTIONS *)

		(* imitator model input file *)
		val mutable model_file_name = "uninitialized model input file name"



		(* OUTPUT OPTIONS *)

		(* only plot cartography *)
		val mutable cartonly = false

		(* Plot cartography; in cartography mode, this option means ANY tile will output a cartography (activated if both `-draw-cart` and `-tiles-files` are true) *)
		val mutable draw_cart = false

		(* prefix for output files *)
		val mutable files_prefix = ""

		(* plot cartography for BC; this options means that the global cartography of all tiles will be generated (activated if -draw-cart is true) *)
		val mutable output_bc_cart = false

		(* Output result for BC to a file *)
		val mutable output_bc_result = false

		(* min/max values for the cartography *)
		val mutable output_cart_x_min = None
		val mutable output_cart_x_max = None
		val mutable output_cart_y_min = None
		val mutable output_cart_y_max = None

		(* Output the approximate float value of discrete variables *)
		val mutable output_float = false

		(* Output result to a file *)
		(*** NOTE: type option since it can be modified to default values depending on the property, after the property is parsed ***)
		val mutable output_result = None

		(* In cartography mode, output all tiles to files *)
		val mutable output_tiles_files = false

		(* Gives statistics on number of calls *)
		val mutable statistics = false

		(* print time stamps *)
		val mutable timed_mode = false

		(* Print graph of reachable states *)
		val mutable graphical_state_space = AbstractAlgorithm.Graphical_state_space_none

		(* Keep the source file used for dot *)
		val mutable with_graphics_source = false

		(* Print logs *)
		val mutable states_description = false


		(* ALGORITHIMS *)

		(* yet another (testing) mode *)
		val mutable branch_and_bound = false

		(* imitator mode *)
		(*** NOTE: arbitrary initialization ***)
		val mutable imitator_mode = Syntax_check

		(* Exploration order *)
		(*** HACK: hard-coded default value ***)
		val mutable exploration_order : AbstractAlgorithm.exploration_order option = Some Exploration_layer_BFS

		(* Best worst-case clock value for EFsynthminpq *)
(* 		val mutable best_worst_case = ref false *)


		(* ANALYSIS OPTIONS *)

		(* acyclic mode: only compare inclusion or equality of a new state with former states of the same iteration (graph depth) *)
		val mutable acyclic = false

		(* limit on number of tiles computed by a cartography *)
		val mutable carto_tiles_limit = None

		(* limit on global runtime for cartography *)
		val mutable carto_time_limit = None

		(* Check whether each constraint contains an integer point *)
		val mutable check_ippta = false

		(* Check whether the accumulated constraint is restricted to pi0 *)
		val mutable check_point = false

		(* Limit the depth in a BFS algorithm or in NDFS for early backtracking *)
		val mutable depth_limit = None

		(* first depth to explore for the iterative deepening in NDFS algorithm *)
		val mutable depth_init = None

		(* Distributed version of IMITATOR *)
		val mutable distribution_mode = AbstractAlgorithm.Non_distributed

		(* For distributed version: kill IM heuristics *)
		val mutable distributedKillIM = false

		(* On-the-fly intersection (DEPRECATED) *)
(* 		val mutable dynamic = ref false *)

		(* Remove useless clocks (slightly experimental) *)
		val mutable dynamic_clock_elimination = false

		(* inclusion mode *)
		val mutable inclusion : bool option = None

		(* Double inclusion mode *)
		val mutable inclusion2 = false

		(* Layered NDFS *)
		val mutable layer : bool option = None

		(* Merging states on the fly *)
		val mutable merge : bool option = None
		val mutable mergeq : bool option = None
		(* Merging states on the fly (after pi0-compatibility check) *)
(* 		val mutable merge_before = false *)

		(* Merging heuristic *)
		val mutable merge_heuristic = Merge_iter10

		(* do not put accepting states at the head of successors list in NDFS *)
		val mutable no_acceptfirst = false

		(* do not use pruning of initial zone in NDFS *)
(* 		val mutable no_initprune = false *)

		(* No leq test of the new states wrt the computed constraint in EFsynth *)
		val mutable no_leq_test_in_ef = false

		(* do not use lookahead in NDFS *)
		val mutable no_lookahead = false

		(* do not order the pending list with bigger zones first in NDFS synthesis *)
		val mutable no_pending_ordered = false

		(* do not use random values *)
		val mutable no_random = false

		(* no time elapsing in zones (in fact, time elapsing is performed before taking a transition, not after) *)
		val mutable no_time_elapsing = false

		(* No automatic removal of variables declared but never used *)
		val mutable no_variable_autoremove = false

		(* Pending list exploration order *)
		val mutable pending_order = Pending_none

		(* Returns contraint K ("algo IMK") *)
		val mutable pi_compatible = false

		(* Pre-compute pi0 ? (in PaTATOR mode only) *)
		val mutable precomputepi0 = false

		(* Name for the file containing the property *)
		val mutable property_file_name = None

		(* process again green states *)
		val mutable recompute_green = false

		(* limit number of states *)
		val mutable states_limit = None

		(* Step for NDFS *)
		val mutable step = NumConst.one

		(* Subsumption for NDFS *)
		val mutable subsumption : bool option = None

		(* autodetect sync actions *)
		val mutable sync_auto_detection = false

		(* limit on runtime *)
		val mutable time_limit = None

		(* tree mode: never compare inclusion or equality of any new state with a former state *)
		val mutable tree = false


		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Get methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		method acyclic = acyclic
(* 		method best_worst_case = best_worst_case *)
(* 		method branch_and_bound = branch_and_bound *)
		method carto_tiles_limit = carto_tiles_limit
		method carto_time_limit = carto_time_limit
		method check_ippta = check_ippta
		method check_point = check_point
		method depth_limit = depth_limit
		method depth_init = depth_init
		method distribution_mode = distribution_mode
		method distributedKillIM = distributedKillIM
		method draw_cart = draw_cart
		(* method dynamic = dynamic *)
		method dynamic_clock_elimination = dynamic_clock_elimination
		
		method exploration_order = value_of_option "exploration_order" exploration_order
		method is_set_exploration_order = exploration_order <> None
		method set_exploration_order new_exploration_order = exploration_order <- Some new_exploration_order
		
		method files_prefix = files_prefix
		method imitator_mode = imitator_mode

		method inclusion = value_of_option "inclusion" inclusion
		method is_set_inclusion = inclusion <> None
		method set_inclusion b = inclusion <- Some b

		method inclusion2 = inclusion2

		method layer = value_of_option "layer" layer
		method is_set_layer = layer <> None
		method set_layer b = layer <- Some b

		method merge = value_of_option "merge" merge
		method is_set_merge = merge <> None
		method set_merge b = merge <- Some b

		method mergeq = value_of_option "mergeq" mergeq
		method is_set_mergeq = mergeq <> None
		method set_mergeq b = mergeq <- Some b

(* 		method merge_before = merge_before *)
		method merge_heuristic = merge_heuristic
		method model_file_name = model_file_name
		method nb_args = nb_args
		method no_acceptfirst = no_acceptfirst
		method no_leq_test_in_ef = no_leq_test_in_ef
		method no_lookahead = no_lookahead
		method no_pending_ordered = no_pending_ordered
		method no_time_elapsing = no_time_elapsing
		method no_random = no_random
		method no_variable_autoremove = no_variable_autoremove
		method output_bc_cart = output_bc_cart
		method output_bc_result = output_bc_result
		method output_cart_x_min = output_cart_x_min
		method output_cart_x_max = output_cart_x_max
		method output_cart_y_min = output_cart_y_min
		method output_cart_y_max = output_cart_y_max
		method output_float = output_float

		method output_result = value_of_option "output_result" output_result
		method is_set_output_result = output_result <> None
		method set_output_result b = output_result <- Some b

		method output_tiles_files = output_tiles_files
		method pi_compatible = pi_compatible
		method precomputepi0 = precomputepi0
		method property_file_name = property_file_name
		method states_limit = states_limit
		method statistics = statistics

		method subsumption = value_of_option "subsumption" subsumption
		method is_set_subsumption = subsumption <> None
		method set_subsumption b = subsumption <- Some b

		method sync_auto_detection = sync_auto_detection
		method time_limit = time_limit
		method timed_mode = timed_mode
		method tree = tree
		method graphical_state_space = graphical_state_space
		method with_graphics_source = with_graphics_source
		method states_description = states_description

		method recompute_green = recompute_green
		method pending_order = pending_order
		method step = step


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Set methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		(*** NOTE: these set methods are only used for the learning-based abstraction construction ***)

		method set_file file_name =
			model_file_name <- file_name

		method set_files_prefix file_name =
			files_prefix <- file_name

		(*** NOTE: this set method is only used for the CUB NZ algorithms ***)
		method set_no_time_elapsing =
			no_time_elapsing <- true


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Parse method *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


		method parse =
			let usage_msg = "Usage: " ^ (Sys.argv.(0)) ^ " model" ^ Constants.model_extension ^ " [property" ^ Constants.property_extension ^ "] [options]" in

			(* Get the verbose mode *)
			let rec set_verbose_mode_ref verbose_mode =
				let mode = try verbose_mode_of_string verbose_mode
					with Not_found ->
					print_error ("The verbose mode `" ^ verbose_mode ^ "` is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1); in
				set_verbose_mode mode

			(* Get the mode *)
			and set_mode mode =
				(* Case: simple syntax check *)
				if mode = "checksyntax" then
					imitator_mode <- Syntax_check

				(* Case: state space exploration *)
				else if mode = "statespace" then
					imitator_mode <- State_space_computation


			(* Get the distributed mode *)
			and set_distributed mode =
				(* Case: no distributed mode *)
				if mode = "no" then
					distribution_mode <- Non_distributed

				(*** TODO / BADPROG: handle better this switch! ***)

				(* Case: distributed in unsupervised version *)
				else if mode = "unsupervised" then
					distribution_mode <- Distributed_unsupervised
				else if mode = "unsupervised-multi-threaded" then
					distribution_mode <- Distributed_unsupervised_multi_threaded

				(* Case: distributed master-slave with sequential selection *)
				else if mode = "static" then
					distribution_mode <- Distributed_static
				(* Case: distributed master-slave with sequential selection *)
				else if mode = "sequential" then
					distribution_mode <- Distributed_ms_sequential
				(* Case: distributed master-slave with shuffle selection *)
				else if mode = "shuffle" then
					distribution_mode <- Distributed_ms_shuffle
				(* Case: distributed master-slave with subpart distribution *)
				else if mode = "dynamic" then
					distribution_mode <- Distributed_ms_subpart
				(* Case: distributed master-slave random generation with a bounded number of attempts *)
				else try (
					(* Find the 'random' string *)
					if not (String.sub mode 0 6 = "random") then raise (Failure "this string is never used");
					(* Find the number *)
					let number = String.sub mode 6 (String.length mode - 6) in
					distribution_mode <- Distributed_ms_random (int_of_string number)
				) with Failure _ | Invalid_argument _-> (
					print_error ("The distribution mode `" ^ mode ^ "` is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1);
				)

			and set_exploration_order order =
				(*  *)
				if order = "layerBFS" then
					exploration_order <- Some Exploration_layer_BFS
				else if order = "queueBFS" then
					exploration_order <- Some Exploration_queue_BFS
				else if order = "queueBFSRS" then
					exploration_order <- Some Exploration_queue_BFS_RS
				else if order = "queueBFSPRIOR" then
					exploration_order <- Some Exploration_queue_BFS_PRIOR
(*				else if order = "NDFS" then
					exploration_order <- Some Exploration_NDFS
				else if order = "NDFSsub" then
					exploration_order <- Some Exploration_NDFS_sub
				else if order = "layerNDFS" then
					exploration_order <- Some Exploration_layer_NDFS
				else if order = "layerNDFSsub" then
					exploration_order <- Some Exploration_layer_NDFS_sub*)
				else(
					print_error ("The exploration order `" ^ order ^ "` is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1);
				)

			and set_pending_order order =
				(*  *)
				if order = "none" then
					pending_order <- Pending_none
				else if order = "param" then
					pending_order <- Pending_param
				else if order = "accepting" then
					pending_order <- Pending_accept
				else if order = "zone" then
					pending_order <- Pending_zone
				else(
					print_error ("The exploration order `" ^ order ^ "` is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1);
				)

			and set_merge_heuristic heuristic =
				(*  *)
				if heuristic = "always" then
					merge_heuristic <- Merge_always
				else if heuristic = "targetseen" then
					merge_heuristic <- Merge_targetseen
				else if heuristic = "pq10" then
					merge_heuristic <- Merge_pq10
				else if heuristic = "pq100" then
					merge_heuristic <- Merge_pq100
				else if heuristic = "iter10" then
					merge_heuristic <- Merge_iter10
				else if heuristic = "iter100" then
					merge_heuristic <- Merge_iter100
				else(
					print_error ("The merge heuristic `" ^ heuristic ^ "` is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1);
				)

			and set_statespace mode =
				if mode = "undetailed" then
					graphical_state_space <- Graphical_state_space_nodetails
				else if mode = "normal" then
					graphical_state_space <- Graphical_state_space_normal
				else if mode = "full" then
					graphical_state_space <- Graphical_state_space_verbose
				else(
					print_error ("The value of `-draw-statespace` `" ^ mode ^ "` is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1);
				)


			(* Very useful option (April fool 2017) *)
			and call_romeo () =
				print_message Verbose_standard "Calling the Romeo model-checker instead of the IMITATOR core engine.";
				print_warning "option -romeo is still very experimental";
				print_warning "Romeo is a very unreliable tool to be manipulated with the highest care.";
				print_warning "The use of Romeo is highly unadvised in the presence of children.";
				print_warning "Romeo is developed in a city where it rains too much.";
				print_message Verbose_standard "Converting model into a parametric timed Petri net… OK";
				Unix.sleep 2;
				print_message Verbose_standard "Romeo currently checking the syntax… OK";
				Unix.sleep 3;
				print_message Verbose_standard "Romeo now running…";
				Unix.sleep 5;
				print_message Verbose_standard "Verification by Romeo in progress… Please wait…";
				Unix.sleep 6;
				print_message Verbose_standard "Romeo still running…";
				Unix.sleep 6;
				print_message Verbose_standard "Romeo still running and it might take a little more time…";
				Unix.sleep 8;
				print_message Verbose_standard "Romeo might soon output a result… (but might also not)";
				(*Unix.sleep 8;
				print_message Verbose_standard "Wait a little longer please…";*)
				for i = 1 to 8 do
					let messages = [
						"Did you know? The first computation of Romeo started in 2004. We are still awaiting its result.";
						"Did you know? A recent study showed that the Juliet model checker is by far more efficient in verifying concurrent systems.";
						"Verification in progress. Please wait.";
						"Romeo almost completed…";
						"Romeo currently running verification…";
						"Romeo just found a Petri net transition!";
						"Romeo just found a Petri net place!";
						"Romeo just computed a symbolic marking!";
						"Romeo just computed a parametric firing time!";
						"Would you seriously rely on a model-checker developed in a city that does not even know with full certainty whether it is or not part of Brittany?";
						"The computation by Romeo will still last for a few more seconds. Consider having a beer (or two (or three (or four))).";
						(*** NOTE: having the definition in the while loop ensures a new % at each iteration ***)
						"The chance of termination of Romeo is now evaluated to about " ^ (string_of_int (random_element (list_of_interval 2 49))) ^ "%.";
						"The expected time before getting a result from Romeo is now evaluated to about " ^ (string_of_int (random_element (list_of_interval 4 42))) ^ " seconds. (Maybe.)";
						"Memory occupancy by Romeo is now " ^ (string_of_int (random_element (list_of_interval 82 99))) ^ "%.";
					] in
					Unix.sleep (random_element (list_of_interval 4 8));
					(random_element[print_message Verbose_standard; print_message Verbose_standard; print_message Verbose_standard; print_warning]) (random_element messages);
				done;
				print_message Verbose_standard "Romeo just answered!";
				Unix.sleep 2;
				print_message Verbose_standard "Processing answer…";
				Unix.sleep 2;
				let answers = [
					"Segmentation fault";
					"I don't know";
					"Maybe";
					"42";
				] in
				print_message Verbose_standard ("Answer: `" ^ (random_element answers) ^ "`.");
				terminate_program ();
				()

			(* Options *)
			and speclist = [
				("-acyclic", Unit (fun () -> acyclic <- true), " Test if a new state was already encountered only with states of the same depth. To be set only if the system is fully acyclic (no backward branching, i.e., no cycle). Default: disabled.
				");

(* 				("-best-worst-case", (fun () -> best_worst_case <- true), " Instead of the minimum global time, compute the best worst-case time bound in the EFsynthminpq mode. Default: false."); *)

(* 				Temporarily disabled (March 2014) *)
(* 				("-bab", (fun () -> branch_and_bound <- true), " Experimental new feature of IMITATOR, based on cost optimization (WORK IN PROGRESS). Default: disabled"); *)

				("-cart-tiles-limit", Int (fun i -> carto_tiles_limit <- Some i), " Set a maximum number of tiles computed for the cartography. Default: no limit.
				");

				("-cart-time-limit", Int (fun i -> carto_time_limit <- Some i), " Set a global time limit (in seconds) for the cartography (in which case the -time-limit option only applies to each call to IM). Default: no limit.
				");

				(* 				("-dynamic", Set dynamic, "Perform the on-the-fly intersection. Default: disabled"); *)

				("-check-ippta", Unit (fun () -> check_ippta <- true), " Check that every new symbolic state contains an integer point; raises an exception if not. Default: disabled.
				");

				("-check-point", Unit (fun () -> check_point <- true), " For IM, check at each iteration whether the accumulated parameter constraint is restricted to pi0 (warning! very costly). Default: disabled.
				");

				("-contributors", Unit (fun _ ->
					print_contributors();
					exit 0), " Print contributors and exit.
				");

				("-depth-limit", Int (fun i -> depth_limit <- Some i), " Limits the depth of the exploration of the state space. Default: no limit.
				");

				("-depth-init", Int (fun i -> depth_init <- Some i), " Initial depth for iterative deepening in NDFS exploration of the state space.
				");
				(*** TODO: what's default? ***)

				("-distributed", String set_distributed, " Distributed version of the behavioral cartography and PRPC.
        Use `no` for the non-distributed mode (default).
        Use `static` for a static domain partitioning [ACN15].
        Use `sequential` for a master-worker scheme with sequential point distribution [ACE14].
        Use `randomXX` for a master-worker scheme with random point distribution (e.g., random5 or random10); after XX successive unsuccessful attempts (where the generated point is already covered), the algorithm will switch to an exhaustive sequential iteration [ACE14].
        Use `shuffle` for a master-worker scheme with shuffle point distribution [ACN15].
        Use `dynamic` for a master-worker dynamic subdomain decomposition [ACN15].
				");

				("-distributedKillIM", Unit (fun () -> distributedKillIM <- true), " In distributed cartography, kill processes covered by other tiles [ACN15]; only works with selected distribution schemes. Default: disabled.
				");

				("-draw-cart", Unit (fun () -> draw_cart <- true), " Plot cartography before terminating the program. Uses the first two parameters with ranges. Default: disabled.
				");

				(*** WARNING: only works partially ***)
				("-draw-cart-x-min", Int (fun n -> output_cart_x_min <- Some n), " Set minimum value for the x axis when plotting the cartography (not entirely functional yet). Default: 0.");
				("-draw-cart-x-max", Int (fun n -> output_cart_x_max <- Some n), " Set maximum value for the x axis when plotting the cartography (not entirely functional yet). Default: automatic.");
				("-draw-cart-y-min", Int (fun n -> output_cart_y_min <- Some n), " Set minimum value for the y axis when plotting the cartography (not entirely functional yet). Default: 0.");
				("-draw-cart-y-max", Int (fun n -> output_cart_y_max <- Some n), " Set maximum value for the y axis when plotting the cartography (not entirely functional yet). Default: automatic.
				");

				("-draw-statespace", String set_statespace, " Draw the state space in a graphical form (using `dot`).
       Use value `undetailed` for the structure only (no location names).
       Use value `normal` for location names.
       Use value `full` for location names and constraints.
				");

				("-dynamic-elimination", Unit (fun () -> dynamic_clock_elimination <- true), " Dynamic clock elimination [FSFMA13]. Default: disabled.
				");

				("-explOrder", String set_exploration_order, " Exploration order [EXPERIMENTAL].
        Use `layerBFS` for a layer-based breadth-first search (default for most algorithms).
        Use `queueBFS` for a queue-based breadth-first search. [ANP17]
        Use `queueBFSRS` for a queue-based breadth-first search with ranking system. [ANP17]
        Use `queueBFSPRIOR` for a priority-based BFS with ranking system. [ANP17]
        Default: layerBFS.
				");
(*        Use `NDFS` for standard NDFS. [NPvdP18]
        Use `NDFSsub` for standard NDFS with subsumption. [NPvdP18]
        Use `layerNDFS` for layered NDFS. [NPvdP18]
        Use `layerNDFSsub` for layered NDFS with subsumption. [NPvdP18] (default for NDFS algorithms)*)

				("-graphics-source", Unit (fun () -> with_graphics_source <- true), " Keep file(s) used for generating graphical output. Default: disabled.
				");

				("-imi2HyTech", Unit (fun _ ->
					imitator_mode <- Translation HyTech
				), "Translate the model into a HyTech model, and exit without performing any analysis. Default: disabled");

				("-imi2IMI", Unit (fun _ ->
					imitator_mode <- Translation IMI
				), "Regenerate the model into an IMITATOR model, and exit without performing any analysis. Default: disabled");

				("-imi2JPG", Unit (fun _ ->
					imitator_mode <- Translation JPG
				), "Translate the model into a graphics, and exit without performing any analysis. Default: disabled");

				("-imi2PDF", Unit (fun _ ->
					imitator_mode <- Translation PDF
				), "Translate the model into a graphics, and exit without performing any analysis. Default: disabled");

				("-imi2PNG", Unit (fun _ ->
					imitator_mode <- Translation PNG
				), "Translate the model into a graphics, and exit without performing any analysis. Default: disabled");

				("-imi2TikZ", Unit (fun _ ->
					imitator_mode <- Translation TikZ
				), "Translate the model into LaTeX TikZ code (no positioning yet), and exit without performing any analysis. Default: disabled");

				("-imi2Uppaal", Unit (fun _ ->
					imitator_mode <- Translation Uppaal
				), "Translate the model into an Uppaal model, and exit without performing any analysis. Some features may not be translated, see user manual. Default: disabled
				");


				("-inclusion", Unit (fun () -> warn_if_set inclusion "inclusion"; inclusion <- Some true), " Consider a monodirectional inclusion of symbolic zones (new <= old) instead of the equality when checking for a fixpoint. Default: depending on the algorithm");
				("-no-inclusion", Unit (fun () -> warn_if_set inclusion "inclusion"; inclusion <- Some false), " Do not consider a monodirectional inclusion of symbolic zones (new <= old) instead of the equality when checking for a fixpoint. Default: depending on the algorithm.
				");

				("-inclusion-bidir", Unit (fun () -> inclusion2 <- true), " Consider a bidirectional inclusion of symbolic zones (new <= old or old <= new) instead of the equality when checking for a fixpoint. Default: disabled.
				");

				("-layer", Unit (fun () -> warn_if_set layer "layer"; layer <- Some true), " Layered NDFS (for NDFS algorithms only) [NPvdP18]. Default: disabled.");
				("-no-layer", Unit (fun () -> warn_if_set layer "layer"; layer <- Some false), " No layered NDFS (for NDFS algorithms only) [NPvdP18]. Default: disabled.
				");

				("-merge", Unit (fun () -> warn_if_set merge "merge"; merge <- Some true), " Use the merging technique of [AFS13]. Default: depending on the algorithm");
				("-no-merge", Unit (fun () -> warn_if_set merge "merge"; merge <- Some false), " Do not use the merging technique of [AFS13]. Default: depending on the algorithm.
				");

(*				("-merge-before", Unit (fun () -> merge_before <- true) , " Use the merging technique of [AFS13] but merges states before pi0-compatibility test (EXPERIMENTAL). Default: disabled (disable)");*)

				("-mergeq", Unit (fun () -> warn_if_set mergeq "mergeq"; mergeq <- Some true; merge <- Some true), "Use the merging technique of [AFS13] on the queue only. Default: depending on the algorithm");
				("-no-mergeq", Unit (fun () -> warn_if_set mergeq "mergeq"; mergeq <- Some false), " Do not use the merging technique of [AFS13] on the queue only. Default: depending on the algorithm.
				");

				("-merge-heuristic", String set_merge_heuristic, " Merge heuristic for EFsynthminpq. Options are `always`, `targetseen`, `pq10`, `pq100`, `iter10`, `iter100`. Default: `iter10`.
				");

				("-mode", String set_mode, " Mode for " ^ Constants.program_name ^ ".
        Use `checksyntax` for a simple syntax check and no analysis.
        Use `statespace` for the generation of the entire parametric state space."
        );

				("-no-acceptfirst", Unit (fun () -> no_acceptfirst <- true), "In NDFS, do not put accepting states at the head of the successors list. Default: enabled.
				");

				("-no-inclusion-test-in-EF", Unit (fun () -> no_leq_test_in_ef <- true), " In EFsynth, no inclusion test of the new states constraints in the already computed constraint. Default: enabled (i.e., inclusion test).
				");

(* 				("-no-initprune", Unit (fun () -> no_initprune <- true), " In collecting NDFS, no pruning if the initial constraint is included in the collected zone. Default: disabled."); *)

				("-no-lookahead", Unit (fun () -> no_lookahead <- true), " In NDFS, no lookahead for finding successors closing an accepting cycle. Default: enabled (i.e., lookahead).
				");

(* 				("-no-pending-ordered", Unit (fun () -> no_pending_ordered <- true), " In NDFS synthesis, do not order the pending queue with larger zones first. Default: enabled."); *)

				("-no-random", Unit (fun () -> no_random <- true), " In IM, no random selection of the pi0-incompatible inequality (select the first found). Default: enabled (i.e., random).
				");

				("-no-var-autoremove", Unit (fun () -> no_variable_autoremove <- true), " Prevent the automatic removal of variables (discrete, clocks, parameters) declared in the header but never used in the IPTAs. Default: enabled (auto-remove).
				");

				("-output-prefix", String (fun new_prefix -> files_prefix <- new_prefix), " Set the prefix for output files. Default: [./model-name].
				");

				("-output-float", Unit (fun () -> output_float <- true), " Approximates the value of discrete variables as floats. Default: disabled.
				");

(* 				("-output-result", Unit (fun () -> output_result <- Some true), " Write the result to a file. Default: true."); *)
				("-no-output-result", Unit (fun () -> output_result <- Some false), " Do not write the result to a file. Default (for most algorithms): enabled, i.e., result is written.
				");

				("-pendingOrder", String set_pending_order, " Pending list exploration order [EXPERIMENTAL].
        Use `accepting` for a layered NDFS where pending list has accepting states first.
        Use `none` for a layered NDFS where pending list has no ordering policy.
        Use `param` for a layered NDFS where pending list has bigger parametric zones first.
        Use `zone` for a layered NDFS where pending list has bigger full zone first.
        Default: none.
				");

				("-precomputepi0", Unit (fun () -> precomputepi0 <- true), " Compute the next pi0 before the next reception of a constraint (in PaTATOR mode for cartography only). Default: disabled.
				");

				("-recompute-green", Unit (fun () -> recompute_green <- true), " In NDFS, process green states again if found at a lower depth. Default: disabled.
				");

				(* Hidden option (April fool 2017) *)
				(*** NOTE: "Beware: options that have an empty doc string will not be included in the list." ***)
				("-romeo", Unit call_romeo, "");

				("-states-description", Unit (fun () -> states_description <- true), " Generate the description of all reachable states in a text file. Default: disabled.
				");

				("-states-limit", Int (fun i -> states_limit <- Some i), " States limit: will try to stop after reaching this number of states. Warning: the program may have to first finish computing the current iteration before stopping. Default: no limit.
				");

				("-statistics", Unit (fun _ -> statistics <- true; Statistics.enable_all_counters()), " Print info on number of calls to PPL, and other statistics. Default: disabled.
				");

				("-step", String (fun i -> (* TODO: SHOULD CHECK HERE THAT STEP IS EITHER A FLOAT OR AN INT *) step <- (NumConst.numconst_of_string i)), " Step for NDFS iterative deepening. Default: 1.
				");

				("-subsumption", Unit (fun () -> warn_if_set subsumption "subsumption"; subsumption <- Some true), " NDFS with subsumption (for NDFS algorithms only) [NPvdP18]. Default: enabled.");
				("-no-subsumption", Unit (fun () -> warn_if_set subsumption "subsumption"; subsumption <- Some false), " NDFS without subsumption (for NDFS algorithms only) [NPvdP18]. Default: enabled.
				");

				("-sync-auto-detect", Unit (fun () -> sync_auto_detection <- true), " Detect automatically the synchronized actions in each automaton. Default: disabled (consider the actions declared by the user).
				");

				("-tiles-files", Unit (fun () -> output_tiles_files <- true), " In cartography, generate the required files for each tile (i.e., the .res file and the cartography files, if any). Default: disabled.
				");

				("-time-elapsing-after", Unit (fun () -> no_time_elapsing <- true), " No time elapsing in zone computation (i.e., time elapsing is performed before taking a transition, not after). Default: disabled.
				");

				("-time-limit", Int (fun i -> time_limit <- Some i), " Time limit in seconds. Warning: no guarantee that the program will stop exactly after the given amount of time. In cartography, this limit applies to each call to IM; use -cart-time-limit for a global limit. Default: no limit.
				");

				("-timed", Unit (fun () -> timed_mode <- true), " Adds a timing information to each output of the program. Default: disabled.
				");

				("-tree", Unit (fun () -> tree <- true), " Does not test if a new state was already encountered. To be set ONLY if the state space is a tree (otherwise analysis may loop). Default: disabled.
				");

				("-verbose", String set_verbose_mode_ref, " Print more or less information. Can be set to `mute`, `warnings`, `standard`, `experiments`, `low`, `medium`, `high`, `total`. Default: `standard`.
				");

				("-version", Unit (fun _ ->
					print_string ("GitHub branch and hash: " ^ ImitatorUtilities.git_branch_and_full_hash);
					print_newline();
					exit 0), " Print version number and exit.
				");

			] in

			(* function for parsing arguments *)
			let anon_fun = (fun arg ->
				(* If 1st argument: main file *)
				if nb_args = 0 then(
					nb_args <- nb_args + 1;
					model_file_name <- arg;
				)
				(* If 2nd argument: property file *)
				else if nb_args = 1 then(
					nb_args <- nb_args + 1;
					property_file_name <- Some arg;
					(* Property => mode = algorithm *)
					imitator_mode <- Algorithm;
				)
				(* If more than two arguments : warns *)
				else (
					print_warning ("The argument `" ^ arg ^ "` will be ignored.");
				)
			) in

			(* Actual parsing *)
			Arg.parse speclist anon_fun usage_msg;

			(* Case no file *)
			if nb_args < 1 then(
				print_error ("Please give a file name for the model.");
				Arg.usage speclist usage_msg;
				abort_program ();
				exit(1)
			);

			(* Case no property file, although it is needed *)
			if nb_args = 1 && (property_needed imitator_mode = Second_file_required) then(
				print_error ("Please give a file name for the property.");
				Arg.usage speclist usage_msg;
				abort_program ();
				exit(1)
			);

			(* Set prefix for files *)
			(* Case the file prefix is not overwritten by the user *)
			if files_prefix = "" then(

				(* Remove the beginning of the path, and set the prefix to the current directory, i.e., output the files in the current directory *)

				(* Split the string according to "/" *)
				let split_file_prefix = Str.split (Str.regexp "/") model_file_name in

				(* Keep the last one *)
				let last_part_path = list_last split_file_prefix in

				(* Update *)
				files_prefix <- last_part_path
			);

			(* Remove the ".imi" at the end of the program prefix, if any *)
			let model_extension_size = String.length Constants.model_extension in
			if String.length files_prefix > model_extension_size then(
				(* Get the last signs *)
				let last = String.sub files_prefix ((String.length files_prefix) - model_extension_size) model_extension_size in
				(* Check if it corresponds to ".imi" *)
				if last = Constants.model_extension then(
					(* Remove the last signs *)
					files_prefix <- String.sub files_prefix 0 ((String.length files_prefix) - model_extension_size);
				);
			);




		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Recall options, print info, and check compatibility with the actual algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method recall_and_warn (model : AbstractModel.abstract_model) (property_option : AbstractProperty.abstract_property option) : unit =
			(* Function to access the property if the mode is Algorithm *)
			let get_property () = match property_option with
			| Some property -> property
			| None -> raise (InternalError "A property should be defined in recall_and_warn.get_property")
			in

			(* File *)
			print_message Verbose_standard ("Model: " ^ model_file_name);
			(* File prefix *)
			print_message Verbose_low ("Prefix for output files: " ^ files_prefix);
			(* Print full command *)
			(*** WARNING: this command drops the "" or `` (if any) ***)
			print_message Verbose_low ("Command: " ^ (OCamlUtilities.string_of_array_of_string_with_sep " " Sys.argv));


			(* Print mode or property *)
			begin
			match imitator_mode with
			| Algorithm ->
				print_message Verbose_standard ("Algorithm: " ^ (AlgorithmOptions.text_of_property (get_property () )));

			| _ ->
				let mode_string = string_of_mode imitator_mode in
				print_message Verbose_standard ("Mode: " ^ mode_string ^ ".");
			end;


			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			(* Check compatibility between options *)
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			
			if acyclic && tree then (
				acyclic <- false;
				print_warning ("Ayclic mode is set although tree mode is already set. Only tree mode will be considered.");
			);

			(* Set some options depending on the IMITATOR mode *)
			let is_cartography = match property_option with
				| None -> false
				| Some property -> AlgorithmOptions.is_cartography property
			in

			(* No cart options if not in cartography *)
			(*** TODO : reintroduce ***)
			if not is_cartography && carto_tiles_limit <> None then print_warning ("A maximum number of tiles has been set, but " ^ Constants.program_name ^ " does not run in cartography mode. Ignored.");
			if not is_cartography && carto_time_limit <> None then print_warning ("A maximum computation for the cartography has been set, but " ^ Constants.program_name ^ " does not run in cartography mode. Ignored.");
			if not is_cartography && output_tiles_files then print_warning ("The option `-tiles-files` outputs files for each iteration of a cartography algorithm, but " ^ Constants.program_name ^ " does not run in cartography mode. Ignored.");


			(* No no_leq_test_in_ef if not EF *)
			if imitator_mode <> Algorithm && no_leq_test_in_ef then
(*			if no_leq_test_in_ef && (imitator_mode <> EF_synthesis && imitator_mode <> EF_min && imitator_mode <> EF_max && imitator_mode <> EF_synth_min && imitator_mode <> EF_synth_max && imitator_mode <> EF_synth_min_priority_queue && imitator_mode <> EFunsafe_synthesis && imitator_mode <> EFexemplify && imitator_mode <> PRP) then*)(
				print_warning ("The option `-no-inclusion-test-in-EF` is reserved for EF and PRP. It will thus be ignored.");
			);


			(*** TODO: check compatibility for #witness and #synthesis wrt the various algorithms ***)


			(*** TODO ***)
(*			(* AF is not safe with incl or merging *)
			if imitator_mode = AF_synthesis then(
				if !inclusion then print_warning "The state inclusion option may not preserve the correctness of AFsynth.";
				if !merge || !mergeq then print_warning "The merging option may not preserve the correctness of AFsynth.";
			);*)

			if imitator_mode <> Algorithm && draw_cart then print_warning ("The `-draw-cart` option is reserved for synthesis algorithms. Ignored.");



			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			(* Recall modes *)
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

			(* Exploration order *)
			begin
			match exploration_order with
				| Some exploration_order -> print_message Verbose_experiments ("Exploration order: " ^ AbstractAlgorithm.string_of_exploration_order exploration_order)
				
				| None -> print_message Verbose_low ("No exploration order set.")
			end;

            (* Merge heuristic *)
            begin
			match merge_heuristic with
				| Merge_always -> print_message Verbose_experiments ("Merge heuristic: always.")
				| Merge_targetseen -> print_message Verbose_experiments ("Merge heuristic: targetseen.")
				| Merge_pq10 -> print_message Verbose_experiments ("Merge heuristic: pq10.")
				| Merge_pq100 -> print_message Verbose_experiments ("Merge heuristic: pq100.")
				| Merge_iter10 -> print_message Verbose_experiments ("Merge heuristic: iter10.")
				| Merge_iter100 -> print_message Verbose_experiments ("Merge heuristic: iter100.")
			end;



			(*** TODO ***)
(*			(* Variant of the inverse method *)
			if !inclusion then
				(*** NOTE: why this test??? better to warn if this option is used in another context ***)
				begin
				match imitator_mode with
				| Inverse_method | Inverse_method_complete | Cover_cartography | Learning_cartography | Shuffle_cartography | Border_cartography | Random_cartography _ | RandomSeq_cartography _
					-> print_message Verbose_standard ("Considering variant of IM with inclusion in the fixpoint [AS11].")
				| _ -> print_message Verbose_standard ("Considering fixpoint variant with monodirectional inclusion of symbolic zones (instead of equality).")
				end
			else
				print_message Verbose_medium ("No fixpoint variant (default).");

			(* Variant of the inverse method *)
			if !inclusion2 then
				(*** NOTE: why this test??? better to warn if this option is used in another context ***)
				begin
				match imitator_mode with
				| Inverse_method | Inverse_method_complete | Cover_cartography | Learning_cartography | Shuffle_cartography | Border_cartography | Random_cartography _ | RandomSeq_cartography _
					-> print_message Verbose_standard ("Considering variant of IM with bidirectional inclusion in the fixpoint.")
				| _ -> print_message Verbose_standard ("Considering fixpoint variant with bidirectional inclusion of symbolic zones (instead of equality).")
				end
			else
				print_message Verbose_medium ("No bidirectional fixpoint variant (default).");
*)

			if no_time_elapsing then
				print_message Verbose_standard ("Time elapsing will be applied at the beginning of the computation of a new state.")
			else
				print_message Verbose_medium ("Time elapsing will be applied at the end of the computation of a new state (default).")
			;

			begin
			match distribution_mode with
			| Non_distributed ->
				print_message Verbose_medium ("Non-distributed mode (default).");
			| Distributed_unsupervised ->(
				print_message Verbose_standard ("Considering a distributed mode with unsupervised workers (work in progress).");
				if not is_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_unsupervised_multi_threaded ->(
				print_message Verbose_standard ("Considering a distributed mode with unsupervised multi-threaded workers (work in progress).");
				if not is_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_static ->(
				print_message Verbose_standard ("Considering a distributed mode with static splitting [ACN15].");
				if not is_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_ms_sequential ->(
				print_message Verbose_standard ("Considering a distributed mode with sequential enumeration of pi0 points [ACE14].");
				if not is_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_ms_shuffle ->(
				print_message Verbose_standard ("Considering a distributed mode with \"shuffle\" enumeration of pi0 points.");
				if not is_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_ms_random max -> (
				print_message Verbose_standard ("Considering a distributed mode with random generation of pi0 points with up to " ^ (string_of_int max) ^ " successive failure before switching to exhaustive enumeration [ACE14].");
				if not is_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			(*************)
			| Distributed_ms_subpart -> (
				print_message Verbose_standard ("Considering a distributed mode with a dynamic domain decomposition [ACN15].");
				if not is_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			end;

			if distributedKillIM then(
				print_message Verbose_standard ("Heuristics to kill a process when its point is covered by another tile, in the distributed cartography [ACN15]; only works with some distribution schemes.");
				if not is_cartography || distribution_mode = Non_distributed then(
					print_warning "The killIM heuristics is only valid for the distributed cartography. Option will be ignored.";
				);
			)else
				print_message Verbose_medium ("No killIM heuristics (default).")
			;

			if precomputepi0 then(
				print_message Verbose_standard ("Compute the next pi0 before the next reception of a constraint.");
				if distribution_mode = Non_distributed then
					print_warning("The -precomputepi0 option is only valid in distributed mode. It will hence be ignored.");
				)
			else
				print_message Verbose_medium ("Compute the next pi0 on-demand, in distributed mode (default).")
			;


(*			if branch_and_bound then
				print_message Verbose_standard ("Considering branch and bound (work in progress!!).")
			else
				print_message Verbose_medium ("No branch and bound mode (default).");*)
				
				
			(* OPTIONS *)
			begin match mergeq with
			| Some true ->
				print_message Verbose_standard ("Merging technique of [AFS13] enabled on queue only.");
			| Some false ->
				begin match merge with
				| Some true ->
					print_message Verbose_standard ("Merging technique of [AFS13] enabled.");
				| Some false ->
					print_message Verbose_standard ("Merging technique of [AFS13] disabled.")
				| None ->
					print_message Verbose_medium ("Merging technique of [AFS13] enabled if requested by the algorithm.")
				end;
			| None ->
				print_message Verbose_medium ("Merging technique of [AFS13] enabled if requested by the algorithm.")
			end;

(*			if !merge_before then
				print_message Verbose_standard ("Variant of the merging technique of [AFS13] enabled. States will be merged before pi0-compatibility test (EXPERIMENTAL).")
			else
				print_message Verbose_medium ("Variant of the merging technique of [AFS13] disabled.")
			;*)

			(*if !dynamic then
				print_message Verbose_standard ("Dynamic mode (optimization by RS).")
			else
				print_message Verbose_medium ("No dynamic mode (default).");*)

			if sync_auto_detection then
				print_message Verbose_standard ("Auto-detection mode for sync actions.")
			else
				print_message Verbose_medium ("No auto-detection mode for sync actions (default).");

			(*** TODO: check that only in NDFS mode ***)
			if no_acceptfirst then
				print_message Verbose_standard ("Not reordering successors (accepting states first) in NDFS search.")
			else
				print_message Verbose_medium ("Reordering successors (accepting states first) in NDFS (default).");

(* 			if !no_initprune then
				print_message Verbose_standard ("No initial zone prune in collecting NDFS.")
			else
				print_message Verbose_medium ("Pruning of initial constraint in NDFS (default)."); *)

			begin
			match layer with
			| None			-> print_message Verbose_high ("Option `-layer` for NDFS unspecified.")
			| Some true		-> print_message Verbose_experiments ("Option `-layer` for NDFS enabled.")
			| Some false	-> print_message Verbose_experiments ("Option `-layer` for NDFS disabled.")
			end;

			begin
			match subsumption with
			| None			-> print_message Verbose_high ("Option `-subsumption` for NDFS unspecified.")
			| Some true		-> print_message Verbose_experiments ("Option `-subsumption` for NDFS enabled.")
			| Some false	-> print_message Verbose_experiments ("Option `-subsumption` for NDFS disabled.")
			end;

			if no_lookahead then
				print_message Verbose_standard ("No lookahead in NDFS search.")
			else
				print_message Verbose_medium ("Lookahead for succesors closing accepting cycles in NDFS (default).");

			if recompute_green then
				print_message Verbose_standard ("Re-processing green states in NDFS.")
			else
				print_message Verbose_medium ("No reprocessing of green states in NDFS (default).");

(* 			if !no_pending_ordered then
				print_message Verbose_standard ("No ordering of pending list with larger zones first in NDFS synthesis.")
			else
				print_message Verbose_medium ("Ordering pending list with larger zones first in NDFS synthesis (default)."); *)
			begin
			match pending_order with
			| Pending_accept ->
				print_message Verbose_standard ("Ordering pending list with accepting states first.")
			| Pending_param ->
				print_message Verbose_standard ("Ordering pending list with larger parameter zones first.")
			| Pending_zone ->
				print_message Verbose_standard ("Ordering pending list with larger zones first.")
			| Pending_none -> ()
			end;

			(*** TODO: check that only in IM/BC mode ***)
			if no_random then
				print_message Verbose_standard ("No random selection for pi0-incompatible inequalities.")
			else
				print_message Verbose_medium ("Standard random selection for pi0-incompatible inequalities (default).");

			if no_variable_autoremove then
				print_message Verbose_standard ("No automatic removal of variables declared but not used.")
			else
				print_message Verbose_medium ("Automatic removal of variables declared but not used (default).");

			if acyclic then
				print_message Verbose_standard ("Acyclic mode: will only check inclusion or equality of a new state into a former state of the same iteration (graph depth).")
			else
				print_message Verbose_medium ("No acyclic mode (default).");

(*			if !best_worst_case then
				print_message Verbose_standard ("Computing the best worst-case bound for EFsynthminpq.")
			else
				print_message Verbose_medium ("No best-worst case bound for EFsynthminpq (default).");*)

			if tree then
				print_message Verbose_standard ("Tree mode: will never check inclusion or equality of a new state into a former state.")
			else
				print_message Verbose_medium ("No tree mode (default).");

			if dynamic_clock_elimination then
				print_message Verbose_standard ("Dynamic clock elimination activated.")
			else
				print_message Verbose_medium ("No dynamic clock elimination (default).");

			if check_ippta then
				print_message Verbose_standard ("Check that each generated state contains an integer point. Raises an exception otherwise.")
			else
				print_message Verbose_medium ("No check of the constraint containment of an integer point (default).");

			(*** TODO: check that only in IM/BC mode ***)
(*			if !check_point then(
				print_message Verbose_standard ("At each iteration, it will be checked whether the parameter constraint is restricted to the sole pi0 point (experimental and costly!).");
				if imitator_mode <> Inverse_method && imitator_mode <> Inverse_method_complete then
					print_warning("The -check-point option is only valid for the inverse method. It will hence be ignored.");
			)
			else
				print_message Verbose_medium ("No check of the constraint equality with pi0 (default).");*)


			(************************************************************)
			(* Recall output options *)
			(************************************************************)

			if draw_cart then
				print_message Verbose_standard ("The cartography will be drawn.")
			else
				print_message Verbose_medium ("No graphical output for the cartography (default).");

			(* Check that if output_cart_x_max / etc. are defined, then cart should be active too *)

			begin
			match output_cart_x_min with
				| None -> print_message Verbose_medium ("No specified minimum value for the x axis for the cartography (default).");
				| Some n ->
					if not draw_cart then (print_warning "A minimum value for the x axis for the cartography is specified, but no cartography will be output. Ignored.")
					else print_message Verbose_low ("The minimum value for the x axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			begin
			match output_cart_x_max with
				| None -> print_message Verbose_medium ("No specified minimum value for the x axis for the cartography (default).");
				| Some n ->
					if not draw_cart then (print_warning "A maximum value for the x axis for the cartography is specified, but no cartography will be output. Ignored.")
					else print_message Verbose_low ("The maximum value for the x axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			begin
			match output_cart_y_min with
				| None -> print_message Verbose_medium ("No specified minimum value for the y axis for the cartography (default).");
				| Some n ->
					if not draw_cart then (print_warning "A minimum value for the y axis for the cartography is specified, but no cartography will be output. Ignored.")
					else print_message Verbose_low ("The minimum value for the y axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			begin
			match output_cart_y_max with
				| None -> print_message Verbose_medium ("No specified minimum value for the y axis for the cartography (default).");
				| Some n -> print_message Verbose_low ("The maximum value for the y axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;


			if output_float then
				print_message Verbose_standard ("The approximate value of all discrete variables will be given.")
			else
				print_message Verbose_medium ("No approximate value of discrete variables will be given (default).")
			;

			begin match output_result with
			| Some true ->
				print_message Verbose_low ("The result will be written to a file.")
			| Some false ->
				print_message Verbose_low ("No result written into a file.")
			| None ->
				print_message Verbose_medium ("The result will be written to a file if requested by the algorithm.")
			end;

			if graphical_state_space <> Graphical_state_space_none then
				print_message Verbose_standard ("The trace set(s) will be generated in a graphical mode.")
			else
				print_message Verbose_medium ("No graphical output for trace set(s) (default).")
			;

			if states_description then
				print_message Verbose_standard ("Description of states will be output.")
			else
				print_message Verbose_medium ("No state description (default).");


			(************************************************************)
			(* Limit options *)
			(************************************************************)

			(* Depth limit *)
			let _ =
			match depth_limit with
				| None -> print_message Verbose_medium "Considering no limit for the depth of the state space (default)."
				| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the depth of the state space.")
			in ();

			(* Limit of the number of states *)
			begin
			match states_limit with
				| None -> print_message Verbose_medium "Considering no limit for the number of states (default)."
				| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the number of states.")
			end;

			(* Time limit *)
			begin
			match time_limit with
				| None -> print_message Verbose_medium "Considering no time limit (default)."
				| Some limit -> print_warning (Constants.program_name ^ " will try to stop after " ^ (string_of_int limit) ^ " seconds.")
			end;

			(* Cartography: Tiles limit *)
			begin
			match carto_tiles_limit with
				| None -> print_message Verbose_medium "Considering no limit of tiles for the cartography (default)."
				| Some limit -> print_warning (Constants.program_name ^ " will stop after the cartography computed (at most) " ^ (string_of_int limit) ^ " tiles.")
			end;

			(* Cartography: Time limit *)
			begin
			match carto_time_limit with
				| None -> print_message Verbose_medium "Considering no time limit for the cartography (default)."
				| Some limit -> print_warning (Constants.program_name ^ " will try to stop the cartography after " ^ (string_of_int limit) ^ " seconds.")
			end;





			(**************************************************)
			(* Timed mode *)
			(**************************************************)
			if timed_mode then (
				(* Print some information *)
				print_message Verbose_standard ("Timed mode is on.");
				(* Set the timed mode *)
				set_timed_mode ();
			) else (
				print_message Verbose_medium ("Timed mode is off (default).");
			);


		;


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Force some options when the mode is cartography *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method set_options_for_cartography is_cartography =
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			(* Force options *)
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

			(* If a time limit is defined for BC but NOT for IM, then define it for IM too (otherwise may yield an infinite loop in IM…) *)
			if is_cartography && carto_time_limit <> None && time_limit = None then(
				print_warning ("A time limit is defined for BC but not for IM: forcing time limit for IM too.");
				let limit = match carto_time_limit with
					| None -> raise (InternalError ("Impossible situation in options, `-cart-time-limit` should be set at that point"))
					| Some limit -> limit
				in
				time_limit <- Some limit;
			);


			(* Handling BC tiles files output *)
			if is_cartography then(
				(* Case cartograpy output requested *)
				if draw_cart then(
					(* Enable cartography for BC *)
					output_bc_cart <- true;
					(* Disable cartography for instances unless requested *)
					if not output_tiles_files then draw_cart <- false
				);

				(* Case result output requested *)
				if self#output_result then(
					(* Enable result for BC *)
					output_bc_result <- true;
					(* Disable cartography for instances unless requested *)
					if not output_tiles_files then output_result <- (Some false)
				);

			);


			()
			(*** TODO: warning if output_tiles_files but no cartography ***)


	end
