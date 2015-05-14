(*****************************************************************
 *
 *                     IMITATOR II
 *
 * Convert a parsing structure into an abstract program
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Ulrich Kuehne, Etienne Andre
 * Created:       2010
 * Last modified: 2015/05/14
 *
 ****************************************************************)
 
open Arg
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


class imitator_options =
	object
		val mutable nb_args = 0
	
	
		(* INPUT OPTIONS *)
		
		(* imitator model input file *)
		val mutable file = ref ""
		
		(* pi0 file *)
		val mutable pi0file = ref ""
		
		(* Create a "fake" pi0 file (USELESS) *)
(* 		val mutable forcePi0 = ref false *)
		
		(* GML syntax *)
		val mutable fromGML = ref false
		
		
	
		(* OUTPUT OPTIONS *)
		
		(* plot cartography *)
		val mutable cart = ref false
		
		(* only plot cartography *)
		val mutable cartonly = ref false
		
		(* plot fancy states in dot *)
		val mutable fancy = ref false
		
		(* prefix for output files *)
		val mutable files_prefix = ref ""
		
		(* min/max values for the cartography *)
		val mutable output_cart_x_min = ref None
		val mutable output_cart_x_max = ref None
		val mutable output_cart_y_min = ref None
		val mutable output_cart_y_max = ref None

		(* Result output to a file *)
		val mutable output_result = ref false

		(* Gives statistics on number of calls *)
		val mutable statistics = ref false
		
		(* print time stamps *)
		val mutable timed_mode = ref false
		
		(* Print graph of reachable states *)
		val mutable with_dot = ref false
		
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
		val mutable completeIM = ref false
		
		(* imitator mode *)
		val mutable imitator_mode = ref Inverse_method

		
		
		(* ANALYSIS OPTIONS *)

		(* acyclic mode: only compare inclusion or equality of a new state with former states of the same iteration (graph depth) *)
		val mutable acyclic = ref false 
		
		(* stop the analysis as soon as a counterexample is found *)
		val mutable counterex = ref false

		(* Check whether each constraint contains an integer point *)
		val mutable check_ippta = ref false
		
		(* Check whether the accumulated constraint is restricted to pi0 *)
		val mutable check_point = ref false
		
		(* On-the-fly intersection (DEPRECATED) *)
(* 		val mutable dynamic = ref false *)
		
		(* Distributed version of IMITATOR *)
		val mutable distribution_mode = ref Non_distributed
		
		(* For distributed version: kill IM heuristics *)
		val mutable distributedKillIM = ref false
		
		(* Remove useless clocks (slightly experimental) *)
		val mutable dynamic_clock_elimination = ref false
		
		(* New algorithm by Etienne and Youcheng *)
		val mutable efim = ref false
		
		(* inclusion mode *)
		val mutable inclusion = ref false
		
		(* do not use random values *)
		val mutable no_random = ref false
		
		(* Returns contraint K ("algo IMK") *)
		val mutable pi_compatible = ref false 
		
		(* limit number of iterations *)
		val mutable post_limit = ref None
		
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
		
		(* Translate PTA into a CLP program *)
		val mutable pta2clp = ref false
		
		(* Translate PTA into a GML program *)
		val mutable pta2gml = ref false
		
		(* Translate PTA into a graphics *)
		val mutable pta2jpg = ref false

		(* Translate PTA into a TikZ LaTeX code *)
		val mutable pta2tikz = ref false
		
		
		(* SPECIALIZED OPTIONS*)
		
		(* Merging states on the fly *)
		val mutable merge = ref false
		(* Merging states on the fly (after pi0-compatibility check) *)
		val mutable merge_before = ref false


		
		method acyclic = !acyclic
		method acyclic_unset = (acyclic := false)
		method branch_and_bound = !branch_and_bound
		method branch_and_bound_unset = (branch_and_bound := false)
		method cart = !cart
		method cartonly = !cartonly
		method check_ippta = !check_ippta
		method check_point = !check_point
		method completeIM = !completeIM
		method counterex = !counterex
		(* method dynamic = !dynamic *)
		method distribution_mode = !distribution_mode
		method distributedKillIM = !distributedKillIM
		method dynamic_clock_elimination = !dynamic_clock_elimination
		method efim = !efim
		method fancy = !fancy
		method file = !file
		method files_prefix = !files_prefix
(* 		method forcePi0 = !forcePi0 *)
		method fromGML = !fromGML
		method imitator_mode = !imitator_mode
		method inclusion = !inclusion
		method nb_args = nb_args
		method merge = !merge
		method merge_before = !merge_before
		method no_random = !no_random
		method output_cart_x_min = !output_cart_x_min
		method output_cart_x_max = !output_cart_x_max
		method output_cart_y_min = !output_cart_y_min
		method output_cart_y_max = !output_cart_y_max
		method output_result = !output_result
		method pi_compatible = !pi_compatible
		method post_limit = !post_limit
		method precomputepi0 = !precomputepi0
		method pta2clp = !pta2clp
		method pta2gml = !pta2gml
		method pta2jpg = !pta2jpg
		method pta2tikz = !pta2tikz
		method states_limit = !states_limit
		method statistics = !statistics
		method step = !step
		method sync_auto_detection = !sync_auto_detection
		method time_limit = !time_limit
		method timed_mode = !timed_mode
		method tree = !tree
		method union = !union
		method with_dot = !with_dot
		method with_graphics_source = !with_graphics_source
		method with_log = !with_log
(* 		method with_parametric_log = !with_parametric_log *)

		method pi0file = !pi0file

		
		method parse =
			let usage_msg = "Usage: " ^ (*program_name*)(Sys.argv.(0)) ^ " model" ^ Constants.model_extension ^ " [reference_valuation.pi0] [options]" in

			(* Get the debug mode *)
			let rec set_debug_mode_ref debug_mode =
					let mode = try debug_mode_of_string debug_mode
						with Not_found ->
						print_error ("The debug mode '" ^ debug_mode ^ "' is not valid.");
						Arg.usage speclist usage_msg;
						abort_program ();
						exit(1); in
					set_debug_mode mode

			(* Get the mode *)
			and set_mode mode =
				(* Case: state space exploration *)
				if mode = "statespace" then 
					imitator_mode := State_space_exploration
				(* Case: EF-synthesis *)
				else if mode = "EF" then 
					imitator_mode := EF_synthesis
				(* Case: inverse method *)
				else if mode = "inversemethod" then 
					imitator_mode := Inverse_method
				(* Case: cover *)
				else if mode = "cover" then 
					imitator_mode := Cover_cartography
				(* Case: border *)
				else if mode = "border" then 
					imitator_mode := Border_cartography
				(* Case: number of iterations *)
				else try (
					(* Find the 'random' string *)
					if not (String.sub mode 0 6 = "random") then raise (Failure "toto");
					(* Find the number *)
					let number = String.sub mode 6 (String.length mode - 6) in
					imitator_mode := (Random_cartography (int_of_string number))
				) with Failure _ | Invalid_argument _-> (
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
					print_error ("The distribution mode '" ^ mode ^ "' is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(1);
				)

			(* Options *)
			and speclist = [
				("-acyclic", Set acyclic, " Test if a new state was already encountered only with states of the same depth. To be set only if the system is fully acyclic (no backward branching, i.e., no cycle). Default: 'false'");
				
(* 				Temporarily disabled (March 2014) *)
(* 				("-bab", Set branch_and_bound, " Experimental new feature of IMITATOR, based on cost optimization (WORK IN PROGRESS). Default: 'false'"); *)
				
				("-cartonly", Unit (fun _ -> cart := true; cartonly := true; imitator_mode := Translation), " Only outputs a cartography. Default: false.");

				(* 				("-dynamic", Set dynamic, "Perform the on-the-fly intersection. Defaut : 'false'"); *)
				
				("-check-ippta", Set check_ippta, " Check that every new state contains an integer point; raises an exception if not. Default: false.");
				
				("-check-point", Set check_point, " Check at each iteration whether the accumulated constraint is restricted to pi0 (warning! very costly). Default: false.");
				
				("-completeIM", Set completeIM, " Experimental version of IM that outputs a complete (full) result. Default: false.");
				
				("-contributors", Unit (fun _ -> print_contributors(); exit 0), " Print contributors and exit.");
				
				("-counterex", Set counterex, " Stop the analysis as soon as a bad state is discovered (work in progress). Default: false.");
				
				("-depth-limit", Int (fun i -> post_limit := Some i), " Limits the depth of the exploration of the reachability graph. Default: no limit.");
				
				("-distributed", String set_distributed, " Distributed version of the behavioral cartography.
        Use 'no' for the non-distributed mode (default).
        Use 'static' for a static domain partitioning.
        Use 'sequential' for a master-worker scheme with sequential point distribution [ACE14].
        Use 'randomXX' for a master-worker scheme with random point distribution (e.g., random5 or random10); after XX successive unsuccessful attempts (where the generated point is already covered), the algorithm will switch to an exhaustive sequential iteration [ACE14].
        Use 'shuffle' for a master-worker scheme with shuffle point distribution.
        Use 'dynamic' for a master-worker dynamic subdomain decomposition.
				");
				
				("-distributedKillIM", Set distributedKillIM, " In distributed cartography, kill processes covered by other tiles. Default: false.");
				
				("-dynamic-elimination", Set dynamic_clock_elimination, " Dynamic clock elimination [FSFMA13]. Default: false.");
				
				("-fancy", Set fancy, " Generate detailed state information for dot output. Default: false.");

				(* 				("-forcePi0", Set forcePi0, "Create a predefined pi0 file of the form p1 = 1, p2 = 2, etc. Defaut : 'false'"); *)
				
				("-fromGrML", Set fromGML, "GrML syntax for input files (experimental). Defaut : 'false'");
				
				("-incl", Set inclusion, " Consider an inclusion of symbolic zones instead of the equality when checking for a fixpoint. Default: 'false'");
				
				("-IMK", Set pi_compatible, " Algorithm IMoriginal (defined in [AS11]): return a constraint such that no pi-incompatible state can be reached. Default: 'false'");
				
				("-IMunion", Set union, " Algorithm IMUnion (defined in [AS11]): Returns the union of the constraint on the parameters associated to the last state of each trace. Default: 'false'");
				
				("-merge", Set merge, " Use the merging technique of [AFS13]. Default: 'false' (disable)");
				
				("-merge-before", Set merge_before , " Use the merging technique of [AFS13] but merges states before pi0-compatibility test (EXPERIMENTAL). Default: 'false' (disable)");
				
				("-mode", String set_mode, " Mode for " ^ Constants.program_name ^ ".
        Use 'statespace' for a parametric state space exploration (no pi0 needed).
        Use 'EF' for a parametric non-reachability analysis (no pi0 needed).
        Use 'inversemethod' for the inverse method.
        For the behavioral cartography algorithm, use 'cover' to cover all the points within V0, 'border' to find the border between a small-valued good and a large-valued bad zone (experimental), or 'randomXX' where XX is a number to iterate random calls to IM (e.g., random5 or random10000). Default: 'inversemethod'.");
				
				("-no-random", Set no_random, " No random selection of the pi0-incompatible inequality (select the first found). Default: false.");

				(* 				("-PTA2CLP", Unit (fun _ -> pta2clp := true; imitator_mode := Translation), "Translate PTA into a CLP program, and exit without performing any analysis. Work in progress! Defaut : 'false'"); *)
				
				("-output-cart", Set cart, " Plot cartography before terminating the program. Uses the first two parameters with ranges. Default: false.");
				
				(*** WARNING: only works partially ***)
				("-output-cart-x-min", Int (fun n -> output_cart_x_min := Some n), " Set minimum value for the x axis when plotting the cartography (not entirely functional yet). Default: 0.");
				("-output-cart-x-max", Int (fun n -> output_cart_x_max := Some n), " Set maximum value for the x axis when plotting the cartography (not entirely functional yet). Default: automatic.");
				("-output-cart-y-min", Int (fun n -> output_cart_y_min := Some n), " Set minimum value for the y axis when plotting the cartography (not entirely functional yet). Default: 0.");
				("-output-cart-y-max", Int (fun n -> output_cart_y_max := Some n), " Set maximum value for the y axis when plotting the cartography (not entirely functional yet). Default: automatic.");
				
				("-output-graphics-source", Set with_graphics_source, " Keep file(s) used for generating graphical output. Default: false.");

(* 				("-output-parametric-states", Set with_parametric_log, " Adds the elimination of the clock variables to the constraints in the description of all reachable states. Default: false."); *)

				("-output-prefix", Set_string files_prefix, " Sets the prefix for output files. Default: [model].");
				
				("-output-result", Set output_result, " Writes the result to a file. Default: false.");
				
				("-output-states", Set with_log, " Generation of the description of all reachable states in a file. Default: false.");
				
				("-output-trace-set", Set with_dot, " Trace set under a graphical form (using 'dot'). Default: false.");
				
				("-precomputepi0", Set precomputepi0, " Compute the next pi0 before the next reception of a constraint (in PaTATOR mode only). Default: false.");

				("-PRP", Set efim, " Reachability-preservation algorithm mixing IM and EF [ALNS15]. Default: false.");
				
				("-PTA2GrML", Unit (fun _ -> pta2gml := true; imitator_mode := Translation), "Translate the model into a GrML program, and exit without performing any analysis. Defaut : 'false'");
				
				("-PTA2JPG", Unit (fun _ -> pta2jpg := true; with_dot:= true; imitator_mode := Translation), "Translate the model into a graphics, and exit without performing any analysis. Defaut : 'false'");
				
				("-PTA2TikZ", Unit (fun _ -> pta2tikz := true; imitator_mode := Translation), "Translate the model into LaTeX TikZ code (no positioning yet), and exit without performing any analysis. Defaut : 'false'");
				
				("-states-limit", Int (fun i -> states_limit := Some i), " States limit: will try to stop after reaching this number of states. Warning: the program may have to first finish computing the current iteration before stopping. Default: no limit.");
				
				("-statistics", Set statistics, " Print info on number of calls to PPL, and other statistics. Default: 'false'");
				
				("-step", String (fun i -> (* TODO: SHOULD CHECK HERE THAT STEP IS EITHER A FLOAT OR AN INT *) step := (NumConst.numconst_of_string i)), " Step for the cartography. Default: 1/1.");
				
				("-sync-auto-detect", Set sync_auto_detection, " Detect automatically the synchronized actions in each automaton. Default: false (consider the actions declared by the user)");
				
				("-time-limit", Int (fun i -> time_limit := Some i), " Time limit in seconds. Warning: no guarantee that the program will stop exactly after the given amount of time. Default: no limit.");
				
				("-timed", Set timed_mode, " Adds a timing information to each output of the program. Default: none.");
				
				("-tree", Set tree, " Does not test if a new state was already encountered. To be set ONLY if the reachability graph is a tree (otherwise analysis may loop). Default: 'false'");
				
				("-verbose", String set_debug_mode_ref, " Print more or less information. Can be set to 'mute', 'warnings', 'standard', 'low', 'medium', 'high', 'total'. Default: 'standard'");
				
				("-version", Unit (fun _ -> print_string ("\n" ^ Constants.program_name ^ " " ^ Constants.version_string ^ "\nBuild: " ^ BuildInfo.build_number ^ " (" ^ BuildInfo.build_time ^ ")\n"); exit 0), " Print version number and exit.");
			] in
					
			(* function for parsing arguments *)
			let anon_fun = (fun arg ->
				(* If 1st argument: main file *)
				if nb_args = 0 then(
					nb_args <- nb_args + 1;
					file := arg;
				)
				(* If 2nd argument: pi0 file *)
				else if nb_args = 1 then(
					nb_args <- nb_args + 1;
					pi0file := arg;
				)
				(* If more than one argument : warns *)
				else (
					print_warning ("The argument '" ^ arg ^ "' will be ignored.");
				)
			) in

			Arg.parse speclist anon_fun usage_msg;

			(* Case no file (except case translation) *)
			if nb_args < 1 then(
				print_error ("Please give a file name for the model.");
				Arg.usage speclist usage_msg;
				abort_program (); exit(1)
			);
			
			(* Case no pi0 file *)
			if nb_args = 1 && (!imitator_mode != State_space_exploration) && (!imitator_mode != EF_synthesis) && (!imitator_mode != Translation) (*&& not !forcePi0*) then(
				print_error ("Please give a file name for the reference valuation.");
				Arg.usage speclist usage_msg;
				abort_program (); exit(1)
			);
			
			
			(* Set prefix for files *)
			if !files_prefix = "" then
				(*** WHAT ? ***)
			  files_prefix := !file
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

			
			
			
			
		(* Recall options and print info *)
		method recall() =
			(* File *)
			print_message Verbose_standard ("Model: " ^ !file);
			(* File prefix *)
			print_message Verbose_low ("Prefix for output files: " ^ !files_prefix);

			(* Global mode *)
			let message = match !imitator_mode with
				| Translation -> "translation"
				| State_space_exploration -> "parametric state space exploration"
				| EF_synthesis -> "EF-synthesis"
				| Inverse_method -> "inverse method"
				| Cover_cartography -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst !step)
				| Border_cartography -> "behavioral cartography algorithm with border detection (experimental) and step " ^ (NumConst.string_of_numconst !step)
				| Random_cartography nb -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations and step " ^ (NumConst.string_of_numconst !step)
			in print_message Verbose_standard ("Mode: " ^ message ^ ".");


			(*** TODO : print the user-defined correctness condition, if any ***)
			
			
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			(* Check compatibility between options *) 
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			if nb_args = 2 then(
				if !imitator_mode = Translation then
					print_warning ("The pi0 file " ^ !pi0file ^ " will be ignored since this is a translation.")
				;
				if !imitator_mode = State_space_exploration then
					print_warning ("The pi0 file " ^ !pi0file ^ " will be ignored since this is a state space exploration.")
				;
				if !imitator_mode = EF_synthesis then
					print_warning ("The pi0 file " ^ !pi0file ^ " will be ignored since this is a synthesis with respect to a property.")
				;
			(*	if !forcePi0 then
					print_warning ("The pi0 file " ^ !pi0file ^ " will be ignored since this the pi0 file is automatically generated.")
				;*)
			);

			if !acyclic && !tree then (
				acyclic := false;
				print_warning ("Ayclic mode is set although tree mode is already set. Only tree mode will be considered.");
			);

(*			if !with_parametric_log && not !with_log then (
				print_warning ("Parametric log was asked, but log was not asked. No log will be output.");
			);*)



			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
			(* Recall modes *) 
			(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

			(* Variant of the inverse method *)
			if !inclusion then
				begin
				match !imitator_mode with
				| Inverse_method | Cover_cartography | Border_cartography | Random_cartography _
					-> print_message Verbose_standard ("Considering variant of IM with inclusion in the fixpoint [AS11].")
				| _ -> print_message Verbose_standard ("Considering fixpoint variant with inclusion of symbolic zones (instead of equality).")
				end
			else
				print_message Verbose_medium ("No fixpoint variant (default).");

			if !union then
				print_message Verbose_standard ("Considering return variant IMunion [AS11].")
			else
				print_message Verbose_medium ("No IMunion return variant (default).");

			if !pi_compatible then
				print_message Verbose_standard ("Considering return variant IMoriginal [AS11].")
			else
				print_message Verbose_medium ("No IMoriginal return variant (default).");

			(* Should add a warning in case of incompatible mode (IMoriginal incompatible with IMunion) + VARIANT ROMAIN *)


			if !efim then
				print_message Verbose_standard ("Considering algorithm EFIM [ALNS15].")
			else
				print_message Verbose_medium ("No EFIM algorithm (default).")
			;


			begin
			match !distribution_mode with
			| Non_distributed -> 
				print_message Verbose_medium ("Non-distributed mode (default).");
			| Distributed_unsupervised ->(
				print_message Verbose_standard ("Considering a distributed mode with unsupervised workers (work in progress).");
				if !imitator_mode <> Cover_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_unsupervised_multi_threaded ->(
				print_message Verbose_standard ("Considering a distributed mode with unsupervised multi-threaded workers (work in progress).");
				if !imitator_mode <> Cover_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_static ->(
				print_message Verbose_standard ("Considering a distributed mode with static splitting.");
				if !imitator_mode <> Cover_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_ms_sequential ->(
				print_message Verbose_standard ("Considering a distributed mode with sequential enumeration of pi0 points [ACE14].");
				if !imitator_mode <> Cover_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_ms_shuffle ->(
				print_message Verbose_standard ("Considering a distributed mode with \"shuffle\" enumeration of pi0 points.");
				if !imitator_mode <> Cover_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			| Distributed_ms_random max -> (
				print_message Verbose_standard ("Considering a distributed mode with random generation of pi0 points with up to " ^ (string_of_int max) ^ " successive failure before switching to exhaustive enumeration [ACE14].");
				if !imitator_mode <> Cover_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			(*************)
			| Distributed_ms_subpart -> (
				print_message Verbose_standard ("Considering a distributed mode with \"shuffle\" enumeration of pi0 points.");
				if !imitator_mode <> Cover_cartography then(
					print_warning "The distributed mode is only valid for the cartography. Option will be ignored.";
				)
			)
			end;

			if !distributedKillIM then(
				print_message Verbose_standard ("Heuristics to kill a process when its point is covered by another tile, in the distributed cartography (work in progress).");
				if !imitator_mode <> Cover_cartography || !distribution_mode = Non_distributed then(
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



			(* Syntax *)
			if !fromGML then
				print_message Verbose_standard ("GrML syntax used.");

			(* Syntax *)
			(*if !forcePi0 then
				print_warning ("Pi0 is automatically generated.");*)


			(* OPTIONS *)

			if !completeIM then (
				print_message Verbose_standard ("IM will output a complete, possibly non-convex, constraint.");
			) else
				print_message Verbose_medium ("IM will output a possibly incomplete, but convex, constraint (default).")
			;


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

			if !no_random then
				print_message Verbose_standard ("No random selection for pi0-incompatible inequalities.")
			else
				print_message Verbose_medium ("Standard random selection for pi0-incompatible inequalities (default).");

			if !acyclic then
				print_message Verbose_standard ("Acyclic mode: will only check inclusion or equality of a new state into a former state of the same iteration (graph depth).")
			else
				print_message Verbose_medium ("No acyclic mode (default).");

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

			if !check_point then
				print_message Verbose_standard ("At each iteration, it will be checked whether the constraint is restricted to the sole pi0 point (experimental and costly!).")
			else
				print_message Verbose_medium ("No check of the constraint equality with pi0 (default).");

				
				
			(*============================================================*)
			(* OUTPUT *)
			(*============================================================*)

			if !cart then
				print_message Verbose_standard ("The cartography will be output in a graphical mode.")
			else
				print_message Verbose_medium ("No graphical output for the cartography (default).");
			
			(* Check that if output_cart_x_max / etc. are defined, then cart should be active too *)
			
			begin
			match !output_cart_x_min with
				| None -> print_message Verbose_medium ("No specified minimum value for the x axis for the cartography (default).");
				| Some n ->
					if not !cart then (print_warning "A minimum value for the x axis for the cartography is specified, but no cartography will be output. Ignored.")
					else print_message Verbose_low ("The minimum value for the x axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			begin
			match !output_cart_x_max with
				| None -> print_message Verbose_medium ("No specified minimum value for the x axis for the cartography (default).");
				| Some n ->
					if not !cart then (print_warning "A maximum value for the x axis for the cartography is specified, but no cartography will be output. Ignored.")
					else print_message Verbose_low ("The maximum value for the x axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			begin
			match !output_cart_y_min with
				| None -> print_message Verbose_medium ("No specified minimum value for the y axis for the cartography (default).");
				| Some n ->
					if not !cart then (print_warning "A minimum value for the y axis for the cartography is specified, but no cartography will be output. Ignored.")
					else print_message Verbose_low ("The minimum value for the y axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			begin
			match !output_cart_y_max with
				| None -> print_message Verbose_medium ("No specified minimum value for the y axis for the cartography (default).");
				| Some n -> print_message Verbose_low ("The maximum value for the y axis for the cartography will be " ^ (string_of_int n) ^ ".");
			end;
			
			
			if !output_result then
				print_message Verbose_standard ("The result will be written to a file.")
			else
				print_message Verbose_medium ("No result written into a file (default).");
			
			if !with_dot then
				print_message Verbose_standard ("The trace set(s) will be generated in a graphical mode.")
			else
				print_message Verbose_medium ("No graphical output for trace set(s) (default).");
			
			
			if !with_log then
				print_message Verbose_standard ("Description of states will be output.")
			else
				print_message Verbose_medium ("No state description (default).");

(*			if !with_parametric_log then
				print_message Verbose_standard ("Parametric description of states will be generated.")
			else
				print_message Verbose_medium ("No parametric description of states (default).");*)

			(* LIMIT OF POST *)
			let _ =
			match !post_limit with
				| None -> print_message Verbose_medium "Considering no limit for the depth of the Post operation (default)."
				| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the depth of the Post operation.")
			in ();

			(* LIMIT OF POST *)
			begin
			match !states_limit with
				| None -> print_message Verbose_medium "Considering no limit for the number of states (default)."
				| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the number of states.")
			end;

			(* TIME LIMIT *)
			let _ =
			match !time_limit with
				| None -> print_message Verbose_medium "Considering no time limit (default)."
				| Some limit -> print_warning ("The program will try to stop after " ^ (string_of_int limit) ^ " seconds.")
			in ();


			(* Verification of incompatibilities between options *)

			if (!imitator_mode = State_space_exploration || !imitator_mode = Translation) && (!union || !pi_compatible) then
				print_warning ("The program will be launched in state space exploration mode; options regarding to the variant of the inverse method will thus be ignored.");

			if (!imitator_mode = State_space_exploration || !imitator_mode = Translation || !imitator_mode = Inverse_method) && (NumConst.neq !step NumConst.one) then
				print_warning ("The program will be launched in state space exploration mode; option regarding to the step of the cartography algorithm will thus be ignored.");





			(**************************************************)
			(* Timed mode *)
			(**************************************************)
			if !timed_mode then (
				(* Debug *)
				print_message Verbose_standard ("Timed mode is on.");
				(* Set the timed mode *)
				set_timed_mode ();
			) else (
				print_message Verbose_medium ("Timed mode is off (default).");
			);


		;
	end
