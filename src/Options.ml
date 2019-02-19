(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: Options definitions
 * 
 * File contributors : Ulrich Kühne, Étienne André
 * Created           : 2010
 * Last modified     : 2019/02/19
 *
 ************************************************************)



(************************************************************)
(* External modules *)
(************************************************************)
open Arg
open OCamlUtilities


(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open ImitatorUtilities


(*let do_unit something =
	Unit (fun () -> something)*)

			

class imitator_options =
	object
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		val mutable nb_args = 0
	
	
		(* INPUT OPTIONS *)
		
		(*** WARNING: why so many mutable ref, although mutable would do ?? ***)
		
		(* imitator model input file *)
		val mutable model_input_file_name = ""
		
		(* pi0 file *)
		val mutable second_file_name = ""
		
		(* Create a "fake" pi0 file (USELESS) *)
(* 		val mutable forcePi0 = ref false *)
		
		(* GML syntax *)
(* 		val mutable fromGML = false *)
		
		
	
		(* OUTPUT OPTIONS *)
		
		(* Plot cartography; in cartography mode, this option means ANY tile will output a cartography (activated if both -cart and -output-tiles-files are true) *)
		val mutable cart = false
		
		(* only plot cartography *)
		val mutable cartonly = false
		
		(* Give location detais in dot *)
(* 		val mutable fancy = ref true *)
		
		(* prefix for output files *)
		val mutable files_prefix = ref ""
		
		(* plot cartography for BC; this options means that the global cartography of all tiles will be generated (activated if -cart is true) *)
		val mutable output_bc_cart = ref false
		
		(* Output result for BC to a file *)
		val mutable output_bc_result = ref false
		
		(* min/max values for the cartography *)
		val mutable output_cart_x_min = ref None
		val mutable output_cart_x_max = ref None
		val mutable output_cart_y_min = ref None
		val mutable output_cart_y_max = ref None

		(* Output the approximate float value of discrete variables *)
		val output_float = ref false
		
		(* Output result to a file *)
		val mutable output_result = ref false
		
		(* In cartography mode, output all tiles to files *)
		val mutable output_tiles_files = ref false

		(* Gives statistics on number of calls *)
		val mutable statistics = ref false
		
		(* print time stamps *)
		val mutable timed_mode = ref false
		
		(* Print graph of reachable states *)
		val mutable graphical_state_space = Graphical_state_space_none
		
		(* Print graph of reachable states in verbose mode *)
(* 		val mutable output_trace_set_verbose = ref false *)
		
		(* Keep the source file used for dot *)
		val mutable with_graphics_source = ref false
		
		(* Print logs *)
		val mutable with_log = ref false
		
(*		(* print parametric logs *)
		val mutable with_parametric_log = ref false*)


		
		(* ALGORITHIMS *)

		(* yet another (testing) mode *)
		val mutable branch_and_bound = ref false
		
		(* Complete version of IM (experimental) *)
(* 		val mutable completeIM = ref false *)
		
		(* Property input via CosyVerif *)
(* 		val mutable cosyprop = ref "" *)
		
		(* imitator mode *)
		val mutable imitator_mode = Inverse_method
		
		(* Exploration order *)
		val mutable exploration_order = Exploration_layer_BFS
		
		(* experimental variant for EFsynth *)
		val mutable new_ef_mode = false

		(* Best worst-case clock value for EFsynthminpq *)
(* 		val mutable best_worst_case = ref false *)

		(* Terminate once a single valuation is found for EFsynthminpq *)
		val mutable early_terminate = ref false
		
		
		(* ANALYSIS OPTIONS *)

		(* acyclic mode: only compare inclusion or equality of a new state with former states of the same iteration (graph depth) *)
		val mutable acyclic = ref false 
		
		(* limit on number of tiles computed by a cartography *)
		val mutable carto_tiles_limit = None
		
		(* limit on global runtime for cartography *)
		val mutable carto_time_limit = None
		
		(* stop the analysis as soon as a counterexample is found *)
		val mutable counterex = ref false

		(* Check whether each constraint contains an integer point *)
		val mutable check_ippta = ref false
		
		(* Check whether the accumulated constraint is restricted to pi0 *)
		val mutable check_point = ref false
		
		(* Limit the depth in a BFS algorithm *)
		val mutable depth_limit = ref None
		
		(* Distributed version of IMITATOR *)
		val mutable distribution_mode = ref Non_distributed
		
		(* For distributed version: kill IM heuristics *)
		val mutable distributedKillIM = ref false
		
		(* On-the-fly intersection (DEPRECATED) *)
(* 		val mutable dynamic = ref false *)
		
		(* Remove useless clocks (slightly experimental) *)
		val mutable dynamic_clock_elimination = ref false
		
		(* New algorithm by Etienne and Youcheng *)
		val mutable efim = ref false
		
		(* inclusion mode *)
		val mutable inclusion = ref false
		
		(* Double inclusion mode *)
		val mutable inclusion2 = ref false
		
		(* No leq test of the new states wrt the computed constraint in EFsynth *)
		val mutable no_leq_test_in_ef = false

		(* do not use random values *)
		val mutable no_random = ref false
		
		(* no time elapsing in zones (in fact, time elapsing is performed before taking a transition, not after) *)
		val mutable no_time_elapsing = ref false
		
		(* No automatic removal of variables declared but never used *)
		val mutable no_variable_autoremove = ref false

		(* Returns contraint K ("algo IMK") *)
		val mutable pi_compatible = ref false 
		
		(* Pre-compute pi0 ? (in PaTATOR mode only) *)
		val mutable precomputepi0 = ref false
		
		(* limit number of states *)
		val mutable states_limit = ref None
		
		(* autodetect sync actions *)
		val mutable sync_auto_detection = ref false
		
		(* limit on runtime *)
		val mutable time_limit = ref None
		
		(* tree mode: never compare inclusion or equality of any new state with a former state *)
		val mutable tree = ref false 
		
		(* Union of last states (algo "IMunion") *)
		val mutable union = ref false
		
		(* Step for the cartography *)
		val mutable step = ref NumConst.one

		
		
		(* TRANSLATION *)
		
(*		(* Translate PTA model into a CLP program *)
		val mutable pta2clp = ref false*)
		
		(* Translate PTA model into a GML model *)
(* 		val mutable pta2gml = ref false *)
		
		(* Translate PTA model into a HyTech file *)
		val mutable pta2hytech = ref false

		(* Translate PTA model into a new IMITATOR file *)
		val mutable pta2imi = ref false

		(* Translate PTA model into a graphics *)
		val mutable pta2jpg = ref false
		val mutable pta2pdf = ref false
		val mutable pta2png = ref false

		(* Translate PTA model into a TikZ LaTeX code *)
		val mutable pta2tikz = ref false
		
		
		(* SPECIALIZED OPTIONS*)
		
		(* Merging states on the fly *)
		val mutable merge = ref false
		(* Merging states on the fly (after pi0-compatibility check) *)
		val mutable merge_before = ref false
	
		(* Merging heuristic *)
		val mutable merge_heuristic = Merge_iter10
		


		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Get methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		
		method acyclic = !acyclic
(* 		method acyclic_unset = (acyclic := false) *)
(* 		method best_worst_case = !best_worst_case *)
		method branch_and_bound = !branch_and_bound
(* 		method branch_and_bound_unset = (branch_and_bound := false) *)
		method cart = cart
		method carto_tiles_limit = carto_tiles_limit
		method carto_time_limit = carto_time_limit
		method cartonly = cartonly
		method check_ippta = !check_ippta
		method check_point = !check_point
(* 		method completeIM = !completeIM *)
(* 		method cosyprop = !cosyprop *)
		method counterex = !counterex
		method depth_limit = !depth_limit
		method distribution_mode = !distribution_mode
		method distributedKillIM = !distributedKillIM
		(* method dynamic = !dynamic *)
		method dynamic_clock_elimination = !dynamic_clock_elimination
		method early_terminate = !early_terminate
		method efim = !efim
		method exploration_order = exploration_order
(* 		method fancy = !fancy *)
		method files_prefix = !files_prefix
(* 		method fromGML = fromGML *)
		method imitator_mode = imitator_mode
		method new_ef_mode = new_ef_mode
		method inclusion = !inclusion
		method inclusion2 = !inclusion2
		method merge = !merge
		method merge_before = !merge_before
		method merge_heuristic = merge_heuristic
		method model_input_file_name = model_input_file_name
		method nb_args = nb_args
		method no_leq_test_in_ef = no_leq_test_in_ef
		method no_time_elapsing = !no_time_elapsing
		method no_random = !no_random
		method no_variable_autoremove = !no_variable_autoremove
		method output_bc_cart = !output_bc_cart
		method output_bc_result = !output_bc_result
		method output_cart_x_min = !output_cart_x_min
		method output_cart_x_max = !output_cart_x_max
		method output_cart_y_min = !output_cart_y_min
		method output_cart_y_max = !output_cart_y_max
		method output_float = !output_float
		method output_result = !output_result
		method output_tiles_files = !output_tiles_files
		method pi_compatible = !pi_compatible
		method precomputepi0 = !precomputepi0
(* 		method pta2clp = !pta2clp *)
(* 		method pta2gml = !pta2gml *)
		method pta2imi = !pta2imi
		method pta2hytech = !pta2hytech
		method pta2jpg = !pta2jpg
		method pta2pdf = !pta2pdf
		method pta2png = !pta2png
		method pta2tikz = !pta2tikz
		method second_file_name = second_file_name
		method states_limit = !states_limit
		method statistics = !statistics
		method step = !step
		method sync_auto_detection = !sync_auto_detection
		method time_limit = !time_limit
		method timed_mode = !timed_mode
		method tree = !tree
		method union = !union
		method graphical_state_space = graphical_state_space
(* 		method output_trace_set_verbose = !output_trace_set_verbose *)
		method with_graphics_source = !with_graphics_source
		method with_log = !with_log
(* 		method with_parametric_log = !with_parametric_log *)

		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Set methods *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		
		(*** NOTE: set methods are only used for the learning-based abstraction construction ***)
		
		method set_file file_name =
			model_input_file_name <- file_name

		method set_files_prefix file_name =
			files_prefix <- ref file_name
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Parse method *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		
		method parse =
			let usage_msg = "Usage: " ^ (Sys.argv.(0)) ^ " model" ^ Constants.model_extension ^ " [reference_valuation.pi0] [options]" in

			(* Get the verbose mode *)
			let rec set_verbose_mode_ref verbose_mode =
					let mode = try verbose_mode_of_string verbose_mode
						with Not_found ->
						(*** HACK: print header now ***)
						print_header_string();
						print_error ("The verbose mode '" ^ verbose_mode ^ "' is not valid.");
						Arg.usage speclist usage_msg;
						abort_program ();
						exit(1); in
					set_verbose_mode mode

			(* Get the mode *)
			and set_mode mode =
				(* Case: state space exploration *)
				if mode = "statespace" then 
					imitator_mode <- State_space_exploration
					
				(* Case: old version of EF-synthesis using a list of constraints *)
				else if mode = "EFold" then 
					imitator_mode <- EF_synthesis
					
				(* Case: new EF-synthesis using PointSetPowerSet *)
				else if mode = "EF" then(
					new_ef_mode <- true;
					imitator_mode <- EF_synthesis
				)
					
				(* Case: EF-synthesis w.r.t. bad states (unsafe) *)
				else if mode = "EFunsafe" then(
					imitator_mode <- EFunsafe_synthesis
				)
					
				(* Case: EF-minimization *)
				else if mode = "EFmin" then 
					imitator_mode <- EF_min
					
				(* Case: EF-maximization *)
				else if mode = "EFmax" then 
					imitator_mode <- EF_max
					
				(* Case: EF-synthesis with minimization *)
				else if mode = "EFsynthmin" then 
					imitator_mode <- EF_synth_min
					
				(* Case: EF-maximization *)
				else if mode = "EFsynthmax" then 
					imitator_mode <- EF_synth_max
					
				(** Optimal reachability with priority queue: queue-based, with priority to the earliest successor for the selection of the next state [work in progress André, Bloemen, Petrucci] *)
				else if mode = "EFsynthminpq" then 
					imitator_mode <- EF_synth_min_priority_queue
					
				(* Case: AF-synthesis *)
				else if mode = "AF" then 
					imitator_mode <- AF_synthesis
					
				(* Case: Parametric loop synthesis *)
				else if mode = "LoopSynth" then 
					imitator_mode <- Loop_synthesis
				
				(** Case: Parametric Büchi-emptiness checking with non-Zenoness (method: check whether the PTA is CUB) *)
				else if mode = "NZCUBcheck" then(
					imitator_mode <- Parametric_NZ_CUBcheck;
					
					(*** NOTE: very important! This algorithm requires the alternative definition of time-elapsing ***)
					no_time_elapsing := true;
				)
					
				(* Case: Parametric Büchi-emptiness checking with non-Zenoness (method: transformation into a CUB-PTA) *)
				else if mode = "NZCUBtrans" then(
					imitator_mode <- Parametric_NZ_CUBtransform;
					
					(*** NOTE: very important! This algorithm requires the alternative definition of time-elapsing ***)
					no_time_elapsing := true;
				)
					
				(* Case: Parametric Büchi-emptiness checking with non-Zenoness (method: transformation into a CUB-PTA; distributed version) *)
				else if mode = "NZCUBtransdist" then(
					imitator_mode <- Parametric_NZ_CUBtransformDistributed;
					
					(*** NOTE: very important! This algorithm requires the alternative definition of time-elapsing ***)
					no_time_elapsing := true;
				)
					
				(* Case: Parametric Büchi-emptiness checking with non-Zenoness on a CUB-PTA: hidden option (mainly for testing) *)
				else if mode = "NZCUB" then(
					imitator_mode <- Parametric_NZ_CUB;
					
					(*** NOTE: very important! This algorithm requires the alternative definition of time-elapsing ***)
					no_time_elapsing := true;
					
					(*** HACK!!! otherwise Graphics won't generate the .jpg file to test...) ***)
					graphical_state_space <- Graphical_state_space_normal;
				)
					
				(* Case: Parametric deadlock checking *)
				else if mode = "PDFC" then 
					imitator_mode <- Parametric_deadlock_checking
					
				(* Case: Inverse method with convex, and therefore possibly incomplete result *)
				else if mode = "inversemethod" then 
					imitator_mode <- Inverse_method
					
				(* Case: Inverse method with full, non-convex result *)
				else if mode = "IMcomplete" then 
					imitator_mode <- Inverse_method_complete
					
				(* Case: PRP *)
				else if mode = "PRP" then 
					imitator_mode <- PRP
					
				(* Case: cover *)
				else if mode = "cover" then 
					imitator_mode <- Cover_cartography
					
				(* Case: learning *)
				else if mode = "coverlearning" then 
					imitator_mode <- Learning_cartography
					
				(* Case: shuffle *)
				else if mode = "shuffle" then 
					imitator_mode <- Shuffle_cartography
					
				(* Case: border *)
				else if mode = "border" then 
					imitator_mode <- Border_cartography
					
				(* Case: PRPC *)
				else if mode = "PRPC" then 
					imitator_mode <- PRPC
					
				(* Case: number of iterations *)
				else try (
					(* Find the 'randomseq' string *)
					if String.length mode >= 9 && String.sub mode 0 9 = "randomseq" then(
						(* Find the number *)
						let number = String.sub mode 9 (String.length mode - 9) in
						imitator_mode <- (RandomSeq_cartography (int_of_string number))
					)
					(* Find the 'random' string *)
					else if String.length mode >= 6 && String.sub mode 0 6 = "random" then (
						(* Find the number *)
						let number = String.sub mode 6 (String.length mode - 6) in
						imitator_mode <- (Random_cartography (int_of_string number))
					)
					else raise (Failure "'randomseq' and 'random' not found")
				) with Failure e | Invalid_argument e-> (
					(*** HACK: print header now ***)
					print_header_string();
(* 					print_error("Error '" ^ e ^ "'"); *)
					print_error ("The mode '" ^ mode ^ "' is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1);
				)
			
			(* Get the distributed mode *)
			and set_distributed mode =
				(* Case: no distributed mode *)
				if mode = "no" then 
					distribution_mode := Non_distributed
					
				(*** TODO / BADPROG: handle better this switch! ***)
				
				(* Case: distributed in unsupervised version *)
				else if mode = "unsupervised" then 
					distribution_mode := Distributed_unsupervised
				else if mode = "unsupervised-multi-threaded" then 
					distribution_mode := Distributed_unsupervised_multi_threaded

				(* Case: distributed master-slave with sequential selection *)
				else if mode = "static" then 
					distribution_mode := Distributed_static
				(* Case: distributed master-slave with sequential selection *)
				else if mode = "sequential" then 
					distribution_mode := Distributed_ms_sequential
				(* Case: distributed master-slave with shuffle selection *)
				else if mode = "shuffle" then 
					distribution_mode := Distributed_ms_shuffle
				(* Case: distributed master-slave with subpart distribution *)
				else if mode = "dynamic" then 
					distribution_mode := Distributed_ms_subpart
				(* Case: distributed master-slave random generation with a bounded number of attempts *)
				else try (
					(* Find the 'random' string *)
					if not (String.sub mode 0 6 = "random") then raise (Failure "this string is never used");
					(* Find the number *)
					let number = String.sub mode 6 (String.length mode - 6) in
					distribution_mode := Distributed_ms_random (int_of_string number)
				) with Failure _ | Invalid_argument _-> (
					(*** HACK: print header now ***)
					print_header_string();
					print_error ("The distribution mode '" ^ mode ^ "' is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1);
				)
				
			and set_exploration_order order =
				(*  *)
				if order = "layerBFS" then
					exploration_order <- Exploration_layer_BFS
				else if order = "queueBFS" then
					exploration_order <- Exploration_queue_BFS
				else if order = "queueBFSRS" then
					exploration_order <- Exploration_queue_BFS_RS
				else if order = "queueBFSPRIOR" then
					exploration_order <- Exploration_queue_BFS_PRIOR
				else(
					(*** HACK: print header now ***)
					print_header_string();
					print_error ("The exploration order '" ^ order ^ "' is not valid.");
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
					(*** HACK: print header now ***)
					print_header_string();
					print_error ("The merge heuristic '" ^ heuristic ^ "' is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1);
				)
			

			(* Very useful option (April fool 2017) *)
			and call_romeo () =
				(*** HACK: print header now ***)
				print_header_string();
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
				print_message Verbose_standard ("Answer: '" ^ (random_element answers) ^ "'.");
				terminate_program ();
				()

			(* Options *)
			and speclist = [
				("-acyclic", Set acyclic, " Test if a new state was already encountered only with states of the same depth. To be set only if the system is fully acyclic (no backward branching, i.e., no cycle). Default: 'false'");
				
(* 				("-best-worst-case", Set best_worst_case, " Instead of the minimum global time, compute the best worst-case time bound in the EFsynthminpq mode. Default: false."); *)

(* 				Temporarily disabled (March 2014) *)
(* 				("-bab", Set branch_and_bound, " Experimental new feature of IMITATOR, based on cost optimization (WORK IN PROGRESS). Default: 'false'"); *)
				
				("-cart-tiles-limit", Int (fun i -> carto_tiles_limit <- Some i), " Set a maximum number of tiles computed for the cartography. Default: no limit.");

				("-cart-time-limit", Int (fun i -> carto_time_limit <- Some i), " Set a global time limit (in seconds) for the cartography (in which case the -time-limit option only applies to each call to IM). Default: no limit.");

				("-cartonly", Unit (fun () -> cart <- true; cartonly <- true; imitator_mode <- Translation), " Only outputs a cartography. Default: false.");

				(* 				("-dynamic", Set dynamic, "Perform the on-the-fly intersection. Defaut : 'false'"); *)
				
				("-check-ippta", Set check_ippta, " Check that every new symbolic state contains an integer point; raises an exception if not. Default: false.");
				
				("-check-point", Set check_point, " For IM, check at each iteration whether the accumulated parameter constraint is restricted to pi0 (warning! very costly). Default: false.");
				
(* 				("-completeIM", Set completeIM, " Experimental version of IM that outputs a complete (full) result. Default: false."); *)
				
				(*** HACK: property input from CosyVerif ***)
(* 				("-cosyProp", Set_string cosyprop, " File name containing the property (for the CosyVerif input only! This option should not be called manually). Default: none."); *)
		
				("-contributors", Unit (fun _ ->
					(*** HACK: print header now ***)
					print_header_string();
					print_contributors();
					exit 0), " Print contributors and exit.");
				
				("-counterexample", Set counterex, " For EF or PRP, stop the analysis as soon as a bad state is discovered. Default: false.");
				
				("-depth-limit", Int (fun i -> depth_limit := Some i), " Limits the depth of the exploration of the state space. Default: no limit.");

				("-distributed", String set_distributed, " Distributed version of the behavioral cartography.
        Use 'no' for the non-distributed mode (default).
        Use 'static' for a static domain partitioning [ACN15].
        Use 'sequential' for a master-worker scheme with sequential point distribution [ACE14].
        Use 'randomXX' for a master-worker scheme with random point distribution (e.g., random5 or random10); after XX successive unsuccessful attempts (where the generated point is already covered), the algorithm will switch to an exhaustive sequential iteration [ACE14].
        Use 'shuffle' for a master-worker scheme with shuffle point distribution [ACN15].
        Use 'dynamic' for a master-worker dynamic subdomain decomposition [ACN15].
				");
				
				("-distributedKillIM", Set distributedKillIM, " In distributed cartography, kill processes covered by other tiles [ACN15]; only works with selected distribution schemes. Default: false.");
				
				
				("-dynamic-elimination", Set dynamic_clock_elimination, " Dynamic clock elimination [FSFMA13]. Default: false.");
				
				("-early-terminate", Set early_terminate, " Provide a single valuation that minimizes global time, instead of all valuations in the EFsynthminpq mode. Default: false.");

				("-explOrder", String set_exploration_order, " Exploration order.
        Use 'layerBFS' for a layer-based breadth-first search.
        Use 'queueBFS' for a queue-based breadth-first search. [EXPERIMENTAL]
        Use 'queueBFSRS' for a queue-based breadth-first search with ranking system. [WORK IN PROGRES]
        Use 'queueBFSPRIOR' for a priority-based BFS with ranking system. [WORK IN PROGRES]
        Use 'optTimeQueue' for optimal reachability with priority queue [WORK IN PROGRESS]
        Default: layerBFS.
				");
				
(* 				("-fromGrML", Unit (fun () -> fromGML <- true), "GrML syntax for input files (experimental). Defaut : 'false'"); *)
				
				("-IMK", Set pi_compatible, " Algorithm IMoriginal (defined in [AS11]): return a constraint such that no pi-incompatible state can be reached. Default: 'false'");
				
				("-IMunion", Set union, " Algorithm IMUnion (defined in [AS11]): Returns the union of the constraint on the parameters associated to the last state of each trace. Default: 'false'");
				
				("-incl", Set inclusion, " Consider a monodirectional inclusion of symbolic zones (new <= old) instead of the equality when checking for a fixpoint. Default: 'false'");
				
				("-incl2", Set inclusion2, " Consider a bidirectional inclusion of symbolic zones (new <= old or old <= new) instead of the equality when checking for a fixpoint. Default: 'false'");
				
				("-merge", Set merge, " Use the merging technique of [AFS13]. Default: 'false' (disable)");
				
				("-merge-before", Set merge_before , " Use the merging technique of [AFS13] but merges states before pi0-compatibility test (EXPERIMENTAL). Default: 'false' (disable)");
				
				("-merge-heuristic", String set_merge_heuristic, " Merge heuristic for EFsynthminpq. Options are 'always', 'targetseen', 'pq10', 'pq100', 'iter10', 'iter100'. Default: iter10.");

				("-mode", String set_mode, " Mode for " ^ Constants.program_name ^ ".
        Use 'statespace' for the generation of the entire parametric state space (no pi0 needed).
        Use 'EF' for a parametric non-reachability analysis (no pi0 needed). [AHV93,JLR15]
        Use 'EFmin' for a parametric non-reachability analysis with parameter minimization (no pi0 needed).
        Use 'EFmax' for a parametric non-reachability analysis with parameter maximization (no pi0 needed).
        Use 'EFsynthminpq' for a parametric non-reachability analysis with global time minimization (no pi0 needed).
        Use 'PDFC' for parametric non-deadlock checking (no pi0 needed). [Andre16]
        Use 'LoopSynth' for cycle-synthesis (without non-Zeno assumption). [ANPS17]
        Use 'NZCUBcheck' for cycle-synthesis (with non-Zeno assumption, using a CUB-detection). [EXPERIMENTAL] [ANPS17]
        Use 'NZCUBtrans' for cycle-synthesis (with non-Zeno assumption, using a transformation into a CUB-PTA). [EXPERIMENTAL] [ANPS17]
        Use 'inversemethod' for the inverse method with convex, and therefore potentially incomplete, result. [ACEF09]
        Use 'IMcomplete' for the inverse method with complete, possibly non-convex result. [AM15]
        Use 'PRP' for parametric reachability preservation. [ALNS15]
        Use 'PRPC' for parametric reachability preservation cartography. [ALNS15]
        For the behavioral cartography algorithm, use 'cover' to cover all the points within V0, 'border' to find the border between a small-valued good and a large-valued bad zone (experimental), or 'randomXX' where XX is a number to iterate random calls to IM (e.g., random5 or random10000). [AF10]
        Default: 'inversemethod'.");
				(*** NOTE: hidden option! 'shuffle' to cover all the points within v0 after shuffling the array. (Reason for hiding: only useful in the distributed cartography) ***)
				(*** NOTE: hidden option! or 'randomseqXX' where XX is a number to iterate random calls to IM followed by a sequential check (e.g., randomseq5 or randomseq10000) (Reason for hiding: only useful in the distributed cartography) ***)
		
				("-no-inclusion-test-in-EF", Unit (fun () -> no_leq_test_in_ef <- true), " In EFsynth, no inclusion test of the new states constraints in the already computed constraint. Default: false.");

				("-no-random", Set no_random, " In IM, no random selection of the pi0-incompatible inequality (select the first found). Default: false.");

				("-no-time-elapsing", Set no_time_elapsing, " No time elapsing in zone computation (i.e., time elapsing is performed before taking a transition, not after). Default: false.");

				("-no-var-autoremove", Set no_variable_autoremove, " Prevent the automatic removal of variables (discrete, clocks, parameters) declared in the header but never used in the IPTAs. Default: false.");

				(* 				("-PTA2CLP", Unit (fun _ -> pta2clp := true; imitator_mode <- Translation), "Translate PTA into a CLP program, and exit without performing any analysis. Work in progress! Defaut : 'false'"); *)
				
				("-output-cart", Unit (fun () -> cart <- true), " Plot cartography before terminating the program. Uses the first two parameters with ranges. Default: false.");
				
				(*** WARNING: only works partially ***)
				("-output-cart-x-min", Int (fun n -> output_cart_x_min := Some n), " Set minimum value for the x axis when plotting the cartography (not entirely functional yet). Default: 0.");
				("-output-cart-x-max", Int (fun n -> output_cart_x_max := Some n), " Set maximum value for the x axis when plotting the cartography (not entirely functional yet). Default: automatic.");
				("-output-cart-y-min", Int (fun n -> output_cart_y_min := Some n), " Set minimum value for the y axis when plotting the cartography (not entirely functional yet). Default: 0.");
				("-output-cart-y-max", Int (fun n -> output_cart_y_max := Some n), " Set maximum value for the y axis when plotting the cartography (not entirely functional yet). Default: automatic.");
				
				("-output-graphics-source", Set with_graphics_source, " Keep file(s) used for generating graphical output. Default: false.");

(* 				("-output-parametric-states", Set with_parametric_log, " Adds the elimination of the clock variables to the constraints in the description of all reachable states. Default: false."); *)

				("-output-prefix", Set_string files_prefix, " Set the prefix for output files. Default: [model].");
				
				("-output-float", Set output_float, " Approximates the value of discrete variables as floats. Default: false.");
				
				("-output-result", Set output_result, " Write the result to a file. Default: false.");
				
				("-output-states", Set with_log, " Generate the description of all reachable states in a file. Default: false.");
				
				("-output-tiles-files", Set output_tiles_files, " In cartography, generate the required files for each tile (works together with -output-cart, -output-result). Default: false.");
				
				(*** TODO: merge these 3 options using "-output-trace-set nodetails", "-output-trace-set normal", "-output-trace-set verbose" ***)
				("-output-trace-set", Unit (fun _ -> graphical_state_space <- Graphical_state_space_normal), " Output trace set under a graphical form (using 'dot') with location names. Default: none.");
				
				("-output-trace-set-nodetails", Unit (fun _ -> graphical_state_space <- Graphical_state_space_nodetails), " Output trace set under a graphical form (using 'dot') without location names. Default: none.");

				("-output-trace-set-verbose", Unit (fun _ -> graphical_state_space <- Graphical_state_space_verbose), " Output trace set under a graphical form (using 'dot') with location names and constraints. Default: none.");
				
				
				("-precomputepi0", Set precomputepi0, " Compute the next pi0 before the next reception of a constraint (in PaTATOR mode only). Default: false.");

				("-PRP", Set efim, " Reachability-preservation algorithm mixing IM and EFsynth [ALNS15]. Default: false. WARNING: deprecated option (use -mode PRP or -mode PRPC)");
				
				("-PTA2HyTech", Unit (fun _ -> pta2hytech := true; imitator_mode <- Translation), "Translate the model into a HyTech model, and exit without performing any analysis. Defaut : 'false'");
				
				("-PTA2IMI", Unit (fun _ -> pta2imi := true; imitator_mode <- Translation), "Regenerate the model into a IMITATOR model, and exit without performing any analysis. Defaut : 'false'");
				
				("-PTA2JPG", Unit (fun _ ->
					pta2jpg := true;
					(*** HACK ***)
					graphical_state_space <- Graphical_state_space_normal;
					imitator_mode <- Translation
				), "Translate the model into a graphics, and exit without performing any analysis. Defaut : 'false'");
				
				("-PTA2PDF", Unit (fun _ ->
					pta2pdf := true;
					(*** HACK ***)
					graphical_state_space <- Graphical_state_space_normal;
					imitator_mode <- Translation
				), "Translate the model into a graphics, and exit without performing any analysis. Defaut : 'false'");
				
				("-PTA2PNG", Unit (fun _ ->
					pta2png := true;
					(*** HACK ***)
					graphical_state_space <- Graphical_state_space_normal;
					imitator_mode <- Translation
				), "Translate the model into a graphics, and exit without performing any analysis. Defaut : 'false'");
				
				("-PTA2TikZ", Unit (fun _ -> pta2tikz := true; imitator_mode <- Translation), "Translate the model into LaTeX TikZ code (no positioning yet), and exit without performing any analysis. Defaut : 'false'");
				
				(* Hidden option (April fool 2017) *)
				(*** NOTE: "Beware: options that have an empty doc string will not be included in the list." ***)
				("-romeo", Unit call_romeo, "");
				
				("-states-limit", Int (fun i -> states_limit := Some i), " States limit: will try to stop after reaching this number of states. Warning: the program may have to first finish computing the current iteration before stopping. Default: no limit.");
				
				("-statistics", Unit (fun _ -> statistics := true; Statistics.enable_all_counters()), " Print info on number of calls to PPL, and other statistics. Default: 'false'");
				
				("-step", String (fun i -> (* TODO: SHOULD CHECK HERE THAT STEP IS EITHER A FLOAT OR AN INT *) step := (NumConst.numconst_of_string i)), " Step for the cartography. Default: 1/1.");
				
				("-sync-auto-detect", Set sync_auto_detection, " Detect automatically the synchronized actions in each automaton. Default: false (consider the actions declared by the user)");
				
				("-time-limit", Int (fun i -> time_limit := Some i), " Time limit in seconds. Warning: no guarantee that the program will stop exactly after the given amount of time. In cartography, this limit applies to each call to IM; use -cart-time-limit for a global limit. Default: no limit.");
				
				("-timed", Set timed_mode, " Adds a timing information to each output of the program. Default: none.");
				
				("-tree", Set tree, " Does not test if a new state was already encountered. To be set ONLY if the reachability graph is a tree (otherwise analysis may loop). Default: 'false'");
				
				("-verbose", String set_verbose_mode_ref, " Print more or less information. Can be set to 'mute', 'warnings', 'standard', 'experiments', 'low', 'medium', 'high', 'total'. Default: 'standard'");
				
				("-version", Unit (fun _ ->
					(*** HACK: print header now ***)
					print_header_string();
					print_string ("GitHub branch and hash: " ^ ImitatorUtilities.git_branch_and_full_hash);
					print_newline();
					exit 0), " Print version number and exit.");
			] in
					
			(* function for parsing arguments *)
			let anon_fun = (fun arg ->
				(* If 1st argument: main file *)
				if nb_args = 0 then(
					nb_args <- nb_args + 1;
					model_input_file_name <- arg;
				)
				(* If 2nd argument: pi0 file *)
				else if nb_args = 1 then(
					nb_args <- nb_args + 1;
					second_file_name <- arg;
				)
				(* If more than one argument : warns *)
				else (
					print_warning ("The argument '" ^ arg ^ "' will be ignored.");
				)
			) in

			Arg.parse speclist anon_fun usage_msg;

			(* Case no file (except case translation) *)
			if nb_args < 1 then(
				(*** HACK: print header now ***)
				print_header_string();
				print_error ("Please give a file name for the model.");
				Arg.usage speclist usage_msg;
				abort_program (); exit(1)
			);
			
			(* Case no pi0 file *)
			(*** TODO: do something less horrible here! ***)
			if nb_args = 1 && (imitator_mode != State_space_exploration) && (imitator_mode != EF_synthesis) && (imitator_mode != AF_synthesis) && (imitator_mode != EFunsafe_synthesis) && (imitator_mode != EF_min) && (imitator_mode != EF_max) && (imitator_mode != EF_synth_min) && (imitator_mode != EF_synth_max) && (imitator_mode != EF_synth_min_priority_queue) && (imitator_mode != Loop_synthesis) && (imitator_mode != Parametric_NZ_CUBtransform) && (imitator_mode != Parametric_NZ_CUBtransformDistributed) && (imitator_mode != Parametric_NZ_CUBcheck) && (imitator_mode != Parametric_NZ_CUB) && (imitator_mode != Parametric_deadlock_checking) && (imitator_mode != Translation) then(
				(*** HACK: print header now ***)
				print_header_string();
				print_error ("Please give a file name for the reference valuation.");
				Arg.usage speclist usage_msg;
				abort_program (); exit(1)
			);
			
			(* Set prefix for files *)
			if !files_prefix = "" then
				(*** WHAT ? ***)
			  files_prefix := model_input_file_name
			;
			  
			(* Remove the ".imi" at the end of the program prefix, if any *)
			let model_extension_size = String.length Constants.model_extension in
			if String.length !files_prefix > model_extension_size then(
				(* Get the last signs *)
				let last = String.sub !files_prefix ((String.length !files_prefix) - model_extension_size) model_extension_size in
				(* Check if it corresponds to ".imi" *)
				if last = Constants.model_extension then(
					(* Remove the last signs *)
					files_prefix := String.sub !files_prefix 0 ((String.length !files_prefix) - model_extension_size);
				);
			);

			
			

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Recall options and print info *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method recall() =
			(* File *)
			print_message Verbose_standard ("Model: " ^ model_input_file_name);
			(* File prefix *)
			print_message Verbose_low ("Prefix for output files: " ^ !files_prefix);
			(* Print full command *)
			(*** WARNING: this command drops the "" or '' (if any) ***)
			print_message Verbose_low ("Command: " ^ (OCamlUtilities.string_of_array_of_string_with_sep " " Sys.argv));

			(* Global mode *)
			let message = match imitator_mode with
				| Translation -> "translation"
				| State_space_exploration -> "parametric state space exploration"
				| EF_synthesis -> "EF-synthesis"
				| EFunsafe_synthesis -> "EFunsafe-synthesis"
				| EF_min -> "EF-minimization"
				| EF_max -> "EF-maximization"
				| EF_synth_min -> "EF-synth with minimization"
				| EF_synth_max -> "EF-synth with maximization"
				| EF_synth_min_priority_queue -> "EF-synth with minimal reachability"
				| AF_synthesis -> "AF-synthesis"
				| Loop_synthesis -> "loop-synthesis"
				| Parametric_NZ_CUBcheck -> "parametric non-Zeno emptiness checking (CUB checking)"
				| Parametric_NZ_CUBtransform -> "parametric non-Zeno emptiness checking (CUB transformation)"
				| Parametric_NZ_CUBtransformDistributed -> "parametric non-Zeno emptiness checking (CUB transformation), distributed version"
				| Parametric_NZ_CUB -> "parametric non-Zeno emptiness checking [testing mode without transformation]"
				| Parametric_deadlock_checking -> "Parametric deadlock-checking"
				| Inverse_method -> "inverse method"
				| Inverse_method_complete -> "inverse method with complete result"
				| PRP -> "parametric reachability preservation"
				| Cover_cartography -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst !step)
				| Learning_cartography -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst !step) ^ " and using learning-based abstractions"
				| Shuffle_cartography -> "behavioral cartography algorithm with full coverage (shuffled version) and step " ^ (NumConst.string_of_numconst !step)
				| Border_cartography -> "behavioral cartography algorithm with border detection (experimental) and step " ^ (NumConst.string_of_numconst !step)
				| Random_cartography nb -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations and step " ^ (NumConst.string_of_numconst !step)
				| RandomSeq_cartography nb -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations + sequential phase and step " ^ (NumConst.string_of_numconst !step)
				| PRPC -> "parametric reachability preservation cartography"
			in print_message Verbose_standard ("Mode: " ^ message ^ ".");


			(*** TODO : print the user-defined correctness condition, if any ***)
			
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			(* Some useful variables *)
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			
			(* Shortcut *)
			let in_cartography_mode =
				match imitator_mode with
				| Translation | State_space_exploration | EF_synthesis| EFunsafe_synthesis | EF_min | EF_max | EF_synth_min | EF_synth_max | EF_synth_min_priority_queue | AF_synthesis | Loop_synthesis | Parametric_NZ_CUBtransform | Parametric_NZ_CUBtransformDistributed | Parametric_NZ_CUBcheck | Parametric_NZ_CUB | Parametric_deadlock_checking | Inverse_method | Inverse_method_complete | PRP -> false
				| Cover_cartography | Learning_cartography | Shuffle_cartography | Border_cartography | Random_cartography _  | RandomSeq_cartography _ | PRPC -> true
			in
			
			
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			(* Force options *) 
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			
			(* If a time limit is defined for BC but NOT for IM, then define it for IM too (otherwise may yield an infinite loop in IM...) *)
			if in_cartography_mode && carto_time_limit <> None && !time_limit = None then(
				print_warning ("A time limit is defined for BC but not for IM: forcing time limit for IM too.");
				let limit = match carto_time_limit with
					| None -> raise (InternalError ("Impossible situation in options, carto_time_limit is set at that point"))
					| Some limit -> limit
				in
				time_limit := Some limit;
			);
			
			
			(* Handling BC tiles files output *)
			if in_cartography_mode then(
				(* Case cartograpy output requested *)
				if cart then(
					(* Enable cartography for BC *)
					output_bc_cart := true;
					(* Disable cartography for instances unless requested *)
					if not !output_tiles_files then cart <- false
				);
			
				(* Case result output requested *)
				if !output_result then(
					(* Enable result for BC *)
					output_bc_result := true;
					(* Disable cartography for instances unless requested *)
					if not !output_tiles_files then output_result := false
				);
			
			);
			
			
			
			(*** TODO: warning if output_tiles_files but no cartography ***)
			

			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			(* Check compatibility between options *) 
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			
			(*** TODO: add warning if Learning_cartography is used with some incompatible options (such as -PRP) ***)
			
			if nb_args = 2 then(
				if imitator_mode = Translation then
					print_warning ("The second file " ^ second_file_name ^ " will be ignored since this is a translation.")
				;
				if imitator_mode = State_space_exploration then
					print_warning ("The second file " ^ second_file_name ^ " will be ignored since this is a state space exploration.")
				;
				if imitator_mode = EF_synthesis || imitator_mode = EFunsafe_synthesis || imitator_mode = EF_min || imitator_mode = EF_synth_min || imitator_mode = EF_synth_max || imitator_mode = EF_synth_min_priority_queue || imitator_mode = EF_max|| imitator_mode = AF_synthesis then
					print_warning ("The second file " ^ second_file_name ^ " will be ignored since this is a synthesis with respect to a property.")
				;
				if imitator_mode = Loop_synthesis then
					print_warning ("The second file " ^ second_file_name ^ " will be ignored since this is a loop synthesis.")
				;
				if imitator_mode = Parametric_NZ_CUBcheck || imitator_mode = Parametric_NZ_CUBtransform  || imitator_mode = Parametric_NZ_CUBtransformDistributed then
					print_warning ("The second file " ^ second_file_name ^ " will be ignored since this is a non-Zeno parametric model checking.")
				;
				if imitator_mode = Parametric_NZ_CUB then
					print_warning ("The second file " ^ second_file_name ^ " will be ignored since this is a non-Zeno parametric model checking.")
				;
				if imitator_mode = Parametric_deadlock_checking then
					print_warning ("The second file " ^ second_file_name ^ " will be ignored since this is parametric deadlock checking.")
				;
			(*	if !forcePi0 then
					print_warning ("The second " ^ !second_file_name ^ " will be ignored since this the pi0 file is automatically generated.")
				;*)
			);

			if !acyclic && !tree then (
				acyclic := false;
				print_warning ("Ayclic mode is set although tree mode is already set. Only tree mode will be considered.");
			);

(*			if !with_parametric_log && not !with_log then (
				print_warning ("Parametric log was asked, but log was not asked. No log will be output.");
			);*)

			
			(* No cart options if not in cartography *)
			if not in_cartography_mode && carto_tiles_limit <> None then print_warning ("A maximum number of tiles has been set, but " ^ Constants.program_name ^ " does not run in cartography mode. Ignored.");
			if not in_cartography_mode && carto_time_limit <> None then print_warning ("A maximum computation for the cartography has been set, but " ^ Constants.program_name ^ " does not run in cartography mode. Ignored.");
			if not in_cartography_mode && (NumConst.neq !step NumConst.one) then
				print_warning (Constants.program_name ^ " is not run in cartography mode; the option regarding to the step of the cartography algorithm will thus be ignored.");
			
			(* Options for variants of IM, but not in IM mode *)
			(*** TODO: do something less horrible here! ***)
			if (imitator_mode = State_space_exploration || imitator_mode = Translation || imitator_mode = EF_synthesis || imitator_mode = EFunsafe_synthesis || imitator_mode = EF_min || imitator_mode = EF_max || imitator_mode = EF_synth_min || imitator_mode = EF_synth_max || imitator_mode = EF_synth_min_priority_queue || imitator_mode = AF_synthesis || imitator_mode = Loop_synthesis || imitator_mode = Parametric_NZ_CUBcheck || imitator_mode = Parametric_NZ_CUBtransform || imitator_mode = Parametric_NZ_CUBtransformDistributed || imitator_mode = Parametric_NZ_CUB || imitator_mode = Parametric_deadlock_checking) && (!union || !pi_compatible) then
				print_warning (Constants.program_name ^ " is run in state space exploration mode; options regarding to the variant of the inverse method will thus be ignored.");

			
			(* No no_leq_test_in_ef if not EF *)
			if no_leq_test_in_ef && (imitator_mode <> EF_synthesis && imitator_mode <> EF_min && imitator_mode <> EF_max && imitator_mode <> EF_synth_min && imitator_mode <> EF_synth_max && imitator_mode <> EF_synth_min_priority_queue && imitator_mode <> EFunsafe_synthesis && imitator_mode <> PRP) then(
				print_warning ("The option '-no-inclusion-test-in-EF' is reserved for EF and PRP. It will thus be ignored.");
			);
				
			(* No counterex if not EF *)
			if !counterex && (imitator_mode <> EF_synthesis && imitator_mode <> EFunsafe_synthesis && imitator_mode <> PRP) then(
				print_warning ("The option '-counterexample' is reserved for EF and PRP. It will thus be ignored.");
			);
			
			
			(* AF is not safe with incl or merging *)
			if imitator_mode = AF_synthesis then(
				if !inclusion then print_warning "The state inclusion option may not preserve the correctness of AFsynth.";
				if !merge then print_warning "The merging option may not preserve the correctness of AFsynth.";
			);
			
			(*** TODO: add warning if -cart but mode translation or statespace ***)



			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			(* Recall modes *) 
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

			(* Exploration order *)
			begin
			match exploration_order with
				| Exploration_layer_BFS -> print_message Verbose_experiments ("Exploration order: layer-based BFS.")
				| Exploration_queue_BFS -> print_message Verbose_standard ("Exploration order: queue-based BFS [ACN17].")
				| Exploration_queue_BFS_RS -> print_message Verbose_standard ("Exploration order: queue-based BFS with ranking system [ACN17].")
				| Exploration_queue_BFS_PRIOR -> print_message Verbose_standard ("Exploration order: queue-based BFS with priority [ACN17].")
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



			(* Variant of the inverse method *)
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

			if !no_time_elapsing then
				print_message Verbose_standard ("Time elapsing will be applied at the beginning of the computation of a new state.")
			else
				print_message Verbose_medium ("Time elapsing will be applied at the end of the computation of a new state (default).")
			;

			if !union then
				print_message Verbose_standard ("Considering return variant IMunion [AS11].")
			else
				print_message Verbose_medium ("No IMunion return variant (default).");

			if !pi_compatible then
				print_message Verbose_standard ("Considering return variant IMoriginal [AS11].")
			else
				print_message Verbose_medium ("No IMoriginal return variant (default).");

			(* Should add a warning in case of incompatible mode (IMoriginal incompatible with IMunion) + VARIANT ROMAIN *)


			if !efim then(
				print_message Verbose_standard ("Considering algorithm PRP [ALNS15].");
				print_warning ("Option -prp is deprecated. Use '-mode PRP' or '-mode PRPC' instead.");
			)
			else
				print_message Verbose_medium ("No PRP algorithm (default).")
			;


			if (imitator_mode = EF_synthesis || imitator_mode = EFunsafe_synthesis || imitator_mode = PRP) then(
				if !counterex then(
					print_message Verbose_standard ("Counterexample mode: the analysis will stop as soon as a target state is found.");
				)else(
					print_message Verbose_medium ("No counterexample mode (default).");
				);
			);

			
			begin
			match !distribution_mode with
			| Non_distributed -> 
				print_message Verbose_medium ("Non-distributed mode (default).");
			| Distributed_unsupervised ->(
				print_message Verbose_standard ("Considering a distributed mode with unsupervised workers (work in progress).");
				if not in_cartography_mode then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_unsupervised_multi_threaded ->(
				print_message Verbose_standard ("Considering a distributed mode with unsupervised multi-threaded workers (work in progress).");
				if not in_cartography_mode then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_static ->(
				print_message Verbose_standard ("Considering a distributed mode with static splitting [ACN15].");
				if not in_cartography_mode then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_ms_sequential ->(
				print_message Verbose_standard ("Considering a distributed mode with sequential enumeration of pi0 points [ACE14].");
				if not in_cartography_mode then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_ms_shuffle ->(
				print_message Verbose_standard ("Considering a distributed mode with \"shuffle\" enumeration of pi0 points.");
				if not in_cartography_mode then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_ms_random max -> (
				print_message Verbose_standard ("Considering a distributed mode with random generation of pi0 points with up to " ^ (string_of_int max) ^ " successive failure before switching to exhaustive enumeration [ACE14].");
				if not in_cartography_mode then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			(*************)
			| Distributed_ms_subpart -> (
				print_message Verbose_standard ("Considering a distributed mode with a dynamic domain decomposition [ACN15].");
				if not in_cartography_mode then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			end;

			if !distributedKillIM then(
				print_message Verbose_standard ("Heuristics to kill a process when its point is covered by another tile, in the distributed cartography [ACN15]; only works with some distribution schemes.");
				if not in_cartography_mode || !distribution_mode = Non_distributed then(
					print_warning "The killIM heuristics is only valid for the distributed cartography. Option will be ignored.";
				);
			)else
				print_message Verbose_medium ("No killIM heuristics (default).")
			;

			if !precomputepi0 then(
				print_message Verbose_standard ("Compute the next pi0 before the next reception of a constraint.");
				if !distribution_mode = Non_distributed then
					print_warning("The -precomputepi0 option is only valid in distributed mode. It will hence be ignored.");
				)
			else
				print_message Verbose_medium ("Compute the next pi0 on-demand, in distributed mode (default).")
			;

				
			if !branch_and_bound then
				print_message Verbose_standard ("Considering branch and bound (work in progress!!).")
			else
				print_message Verbose_medium ("No branch and bound mode (default).");



(*			(* Syntax *)
			if fromGML then
				print_message Verbose_standard ("GrML syntax used.");*)

			(* Syntax *)
			(*if !forcePi0 then
				print_warning ("Pi0 is automatically generated.");*)


			(* OPTIONS *)

			(*** TODO: check that only in IM/BC mode ***)
(*			if !completeIM then (
				print_message Verbose_standard ("IM will output a complete, possibly non-convex, constraint.");
			) else
				print_message Verbose_medium ("IM will output a possibly incomplete, but convex, constraint (default).")
			;*)


			if !merge then (
				print_message Verbose_standard ("Merging technique of [AFS13] enabled.");
			) else
				print_message Verbose_medium ("Merging technique of [AFS13] disabled (default).")
			;
			if !merge_before then
				print_message Verbose_standard ("Variant of the merging technique of [AFS13] enabled. States will be merged before pi0-compatibility test (EXPERIMENTAL).")
			else
				print_message Verbose_medium ("Variant of the merging technique of [AFS13] disabled.")
			;

			(*if !dynamic then
				print_message Verbose_standard ("Dynamic mode (optimization by RS).")
			else
				print_message Verbose_medium ("No dynamic mode (default).");*)

			if !sync_auto_detection then
				print_message Verbose_standard ("Auto-detection mode for sync actions.")
			else
				print_message Verbose_medium ("No auto-detection mode for sync actions (default).");

			(*** TODO: check that only in IM/BC mode ***)
			if !no_random then
				print_message Verbose_standard ("No random selection for pi0-incompatible inequalities.")
			else
				print_message Verbose_medium ("Standard random selection for pi0-incompatible inequalities (default).");

			if !no_variable_autoremove then
				print_message Verbose_standard ("No automatic removal of variables declared but not used.")
			else
				print_message Verbose_medium ("Automatic removal of variables declared but not used (default).");

			if !acyclic then
				print_message Verbose_standard ("Acyclic mode: will only check inclusion or equality of a new state into a former state of the same iteration (graph depth).")
			else
				print_message Verbose_medium ("No acyclic mode (default).");

(*			if !best_worst_case then
				print_message Verbose_standard ("Computing the best worst-case bound for EFsynthminpq.")
			else
				print_message Verbose_medium ("No best-worst case bound for EFsynthminpq (default).");*)

			if !tree then
				print_message Verbose_standard ("Tree mode: will never check inclusion or equality of a new state into a former state.")
			else
				print_message Verbose_medium ("No tree mode (default).");

			if !dynamic_clock_elimination then
				print_message Verbose_standard ("Dynamic clock elimination activated.")
			else
				print_message Verbose_medium ("No dynamic clock elimination (default).");

			if !check_ippta then
				print_message Verbose_standard ("Check that each generated state contains an integer point. Raises an exception otherwise.")
			else
				print_message Verbose_medium ("No check of the constraint containment of an integer point (default).");

			(*** TODO: check that only in IM/BC mode ***)
			if !check_point then(
				print_message Verbose_standard ("At each iteration, it will be checked whether the parameter constraint is restricted to the sole pi0 point (experimental and costly!).");
				if imitator_mode <> Inverse_method && imitator_mode <> Inverse_method_complete then
					print_warning("The -check-point option is only valid for the inverse method. It will hence be ignored.");
			)
			else
				print_message Verbose_medium ("No check of the constraint equality with pi0 (default).");

			if !early_terminate then
				print_message Verbose_standard ("Early termination chosen for EFsynthminpq, the algorithm will stop once a single valuation is found that minimizes global_time.")
			else
				print_message Verbose_medium ("No early termination, computing all valuations for EFsynthminpq (default).");

			
			(************************************************************)
			(* Recall output options *)
			(************************************************************)

			if cart then
				print_message Verbose_standard ("The cartography will be drawn.")
			else
				print_message Verbose_medium ("No graphical output for the cartography (default).");
			
			(* Check that if output_cart_x_max / etc. are defined, then cart should be active too *)
			
			begin
			match !output_cart_x_min with
				| None -> print_message Verbose_medium ("No specified minimum value for the x axis for the cartography (default).");
				| Some n ->
					if not cart then (print_warning "A minimum value for the x axis for the cartography is specified, but no cartography will be output. Ignored.")
					else print_message Verbose_low ("The minimum value for the x axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			begin
			match !output_cart_x_max with
				| None -> print_message Verbose_medium ("No specified minimum value for the x axis for the cartography (default).");
				| Some n ->
					if not cart then (print_warning "A maximum value for the x axis for the cartography is specified, but no cartography will be output. Ignored.")
					else print_message Verbose_low ("The maximum value for the x axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			begin
			match !output_cart_y_min with
				| None -> print_message Verbose_medium ("No specified minimum value for the y axis for the cartography (default).");
				| Some n ->
					if not cart then (print_warning "A minimum value for the y axis for the cartography is specified, but no cartography will be output. Ignored.")
					else print_message Verbose_low ("The minimum value for the y axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			begin
			match !output_cart_y_max with
				| None -> print_message Verbose_medium ("No specified minimum value for the y axis for the cartography (default).");
				| Some n -> print_message Verbose_low ("The maximum value for the y axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			
			
			if !output_float then
				print_message Verbose_standard ("The approximate value of all discrete variables will be given.")
			else
				print_message Verbose_medium ("No approximate value of discrete variables will be given (default).")
			;
			
			if !output_result then
				print_message Verbose_standard ("The result will be written to a file.")
			else
				print_message Verbose_medium ("No result written into a file (default).")
			;
			
			if graphical_state_space <> Graphical_state_space_none then
				print_message Verbose_standard ("The trace set(s) will be generated in a graphical mode.")
			else
				print_message Verbose_medium ("No graphical output for trace set(s) (default).")
			;
			
(*			if !output_trace_set_verbose then
				print_message Verbose_standard ("The trace set(s) will be generated in a graphical mode with verbose information (with all constraints).")
			else
				print_message Verbose_medium ("No verbose graphical output for trace set(s) (default).")
			;*)
			
(*			if !fancy then
				print_message Verbose_medium ("Locations will be detailed in the graphical trace set (default).")
			else
				print_message Verbose_standard ("No location details in the graphical trace set.")
			;
			*)
			
			if !with_log then
				print_message Verbose_standard ("Description of states will be output.")
			else
				print_message Verbose_medium ("No state description (default).");

(*			if !with_parametric_log then
				print_message Verbose_standard ("Parametric description of states will be generated.")
			else
				print_message Verbose_medium ("No parametric description of states (default).");*)

				
			(************************************************************)
			(* Limit options *)
			(************************************************************)
			
			(* Depth limit *)
			let _ =
			match !depth_limit with
				| None -> print_message Verbose_medium "Considering no limit for the depth of the state space (default)."
				| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the depth of the state space.")
			in ();

			(* Limit of the number of states *)
			begin
			match !states_limit with
				| None -> print_message Verbose_medium "Considering no limit for the number of states (default)."
				| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the number of states.")
			end;

			(* Time limit *)
			begin
			match !time_limit with
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
			if !timed_mode then (
				(* Print some information *)
				print_message Verbose_standard ("Timed mode is on.");
				(* Set the timed mode *)
				set_timed_mode ();
			) else (
				print_message Verbose_medium ("Timed mode is off (default).");
			);


		;
	end
