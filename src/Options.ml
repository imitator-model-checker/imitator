(*****************************************************************
 *
 *                     IMITATOR II
 *
 * Convert a parsing structure into an abstract program
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Ulrich Kuehne, Etienne Andre
 * Created:       2010
 * Last modified: 2013/03/20
 *
 ****************************************************************)
 
open Arg
open Global

class imitator_options =
	object
		val mutable nb_args = 0
	
	
		(* INPUT OPTIONS *)
		
		(* imitator program file *)
		val mutable file = ref ""
		(* pi0 file *)
		val mutable pi0file = ref ""
		(* Create a "fake" pi0 file *)
		val mutable forcePi0 = ref false
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
		(* print parametric logs *)
		val mutable with_parametric_log = ref false


		(* ANALYSIS OPTIONS *)

		(* yet another (testing) mode *)
		val mutable branch_and_bound = ref false
		(* stop the analysis as soon as a counterexample is found *)
		val mutable counterex = ref false
		(* yet another (testing) mode *)
		val mutable dynamic_clock_elimination = ref false
		(* limit number of states *)
		val mutable states_limit = ref None
		(* limit number of iterations *)
		val mutable post_limit = ref None
		(* limit on runtime *)
		val mutable time_limit = ref None
		(* imitator mode *)
		val mutable imitator_mode = ref Global.Inverse_method
		(* acyclic mode: only compare inclusion or equality of a new state with former states of the same iteration (graph depth) *)
		val mutable acyclic = ref false 
		(* tree mode: never compare inclusion or equality of any new state with a former state *)
		val mutable tree = ref false 
		(* inclusion mode *)
		val mutable inclusion = ref false
		(* do not use random values *)
		val mutable no_random = ref false
		(* autodetect sync actions *)
		val mutable sync_auto_detection = ref false
		(*On-the-fly intersection*)
		val mutable dynamic = ref false
		(*Union of last states*)
		val mutable union = ref false
		(*Returns contraint K*)
		val mutable pi_compatible = ref false 
		(* Step for the cartography *)
		val mutable step = ref NumConst.one

		(* TRANSLATION *)
		(* Translate PTA into a CLP program *)
		val mutable pta2clp = ref false
		(* Translate PTA into a GML program *)
		val mutable pta2gml = ref false
		(* Translate PTA into a graphics *)
		val mutable pta2jpg = ref false

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
		method counterex = !counterex
		(* method dynamic = !dynamic *)
		method dynamic_clock_elimination = !dynamic_clock_elimination
		method fancy = !fancy
		method file = !file
		method files_prefix = !files_prefix
		method forcePi0 = !forcePi0
		method fromGML = !fromGML
		method imitator_mode = !imitator_mode
		method inclusion = !inclusion
		method nb_args = nb_args
		method merge = !merge
		method merge_before = !merge_before
		method no_random = !no_random
		method pi_compatible = !pi_compatible
		method post_limit = !post_limit
		method pta2clp = !pta2clp
		method pta2gml = !pta2gml
		method pta2jpg = !pta2jpg
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
		method with_parametric_log = !with_parametric_log

		method pi0file = !pi0file

		
		method parse =
			let usage_msg = "Usage: " ^ program_name ^ " model.imi [reference_valuation.pi0] [options]" in

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

			(* Options *)
			and speclist = [
				("-acyclic", Set acyclic, " Test if a new state was already encountered only with states of the same depth. To be set only if the system is fully acyclic (no backward branching, i.e., no cycle). Default: 'false'");
				("-bab", Set branch_and_bound, " Experimental new feature of IMITATOR, based on cost optimization (WORK IN PROGRESS). Default: 'false'");
				("-cart", Set cart, " Plot cartography before terminating the program. Uses the first two parameters with ranges. Default: false.");
				("-cartonly", Unit (fun _ -> cart := true; cartonly := true; imitator_mode := Translation), " Only prints a cartography. Default: false.");
(* 				("-dynamic", Set dynamic, "Perform the on-the-fly intersection. Defaut : 'false'"); *)
				("-counterex", Set counterex, " Stop the analysis as soon as a bad state is discovered (work in progress). Default: false.");
				("-depth-limit", Int (fun i -> post_limit := Some i), " Limits the depth of the exploration of the reachability graph. Default: no limit.");
				("-dynamic-elimination", Set dynamic_clock_elimination, " Dynamic clock elimination (experimental). Default: false.");
				("-fancy", Set fancy, " Generate detailed state information for dot output. Default: false.");
(* 				("-forcePi0", Set forcePi0, "Create a predefined pi0 file of the form p1 = 1, p2 = 2, etc. Defaut : 'false'"); *)
				("-fromGrML", Set fromGML, "GrML syntax for input files (experimental). Defaut : 'false'");
				("-incl", Set inclusion, " Consider an inclusion of region instead of the equality when performing the Post operation (e.g., as in algorithm IMincl defined in [AS11]). Default: 'false'");
				("-IMK", Set pi_compatible, " Algorithm IMoriginal (defined in [AS11]): return a constraint such that no pi-incompatible state can be reached. Default: 'false'");
				("-IMunion", Set union, " Algorithm IMUnion (defined in [AS11]): Returns the union of the constraint on the parameters associated to the last state of each trace. Default: 'false'");
				("-log-prefix", Set_string files_prefix, " Sets the prefix for output files. Default: [model].");
				("-merge", Set merge, " Use the merging technique of [AFS12]. Default: 'false' (disable)");
				("-merge-before", Set merge_before , " Use the merging technique of [AFS12] but merges states before pi0-compatibility test (EXPERIMENTAL). Default: 'false' (disable)");
				("-mode", String set_mode, " Mode for " ^ program_name ^ ".
        Use 'statespace' for a parametric state space exploration (no pi0 needed).
        Use 'EF' for a parametric non-reachability analysis (no pi0 needed).
        Use 'inversemethod' for the inverse method.
        For the behavioral cartography algorithm, use 'cover' to cover all the points within V0, 'border' to find the border between a small-valued good and a large-valued bad zone (experimental), or 'randomXX' where XX is a number to iterate randomly algorithm (e.g., random5 or random100). Default: 'inversemethod'.");
				("-no-random", Set no_random, " No random selection of the pi0-incompatible inequality (select the first found). Default: false.");
(* 				("-PTA2CLP", Unit (fun _ -> pta2clp := true; imitator_mode := Translation), "Translate PTA into a CLP program, and exit without performing any analysis. Work in progress! Defaut : 'false'"); *)
				("-PTA2GrML", Unit (fun _ -> pta2gml := true; imitator_mode := Translation), "Translate PTA into a GrML program, and exit without performing any analysis. Defaut : 'false'");
				("-PTA2JPG", Unit (fun _ -> pta2jpg := true; with_dot:= true; imitator_mode := Translation), "Translate PTA into a graphics, and exit without performing any analysis. Defaut : 'false'");
				("-states-limit", Int (fun i -> states_limit := Some i), " States limit: will try to stop after reaching this number of states. Warning: the program may have to first finish computing the current iteration before stopping. Default: no limit.");
				("-statistics", Set statistics, " Print info on number of calls to PPL, and other statistics. Default: 'false'");
				("-step", String (fun i -> (* TODO: SHOULD CHECK HERE THAT STEP IS EITHER A FLOAT OR AN INT *) step := (NumConst.numconst_of_string i)), " Step for the cartography. Default: 1/1.");
				("-sync-auto-detect", Set sync_auto_detection, " Detect automatically the synchronized actions in each automaton. Default: false (consider the actions declared by the user)");
				("-time-limit", Int (fun i -> time_limit := Some i), " Time limit in seconds. Warning: no guarantee that the program will stop exactly after the given amount of time. Default: no limit.");
				("-timed", Set timed_mode, " Adds a timing information to each output of the program. Default: none.");
				("-tree", Set tree, " Does not test if a new state was already encountered. To be set ONLY if the reachability graph is a tree (otherwise analysis may loop). Default: 'false'");
				("-verbose", String set_debug_mode_ref, " Print more or less information. Can be set to 'mute', 'standard', 'low', 'medium', 'high', 'total'. Default: 'standard'");
				("-version", Unit (fun _ -> print_string ("\n" ^ program_name ^ " " ^ version_string); exit 0), " Print version number and exit.");
				("-with-dot", Set with_dot, " Trace set under a graphical form (using 'dot'). Default: false.");
				("-with-graphics-source", Set with_graphics_source, " Keep file(s) used for generating graphical output. Default: false.");
				("-with-log", Set with_log, " Generation of log files (description of states). Default: false.");
				("-with-parametric-log", Set with_parametric_log, " Adds the elimination of the clock variables in the constraints in the log files. Default: false.");

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
			if nb_args = 1 && (!imitator_mode != State_space_exploration) && (!imitator_mode != EF_synthesis) && (!imitator_mode != Translation) && not !forcePi0 then(
				print_error ("Please give a file name for the reference valuation.");
				Arg.usage speclist usage_msg;
				abort_program (); exit(1)
			);
			
			
			(* Set prefix for files *)
			if !files_prefix = "" then
				(* WHAT ? *)
			  files_prefix := !file
			;
			  
			(* Remove the ".imi" at the end of the program prefix, if any *)
			let model_extension_size = String.length model_extension in
			if String.length !files_prefix > model_extension_size then(
				(* Get the last signs *)
				let last = String.sub !files_prefix ((String.length !files_prefix) - model_extension_size) model_extension_size in
				(* Check if it corresponds to ".imi" *)
				if last = model_extension then(
					(* Remove the last signs *)
					files_prefix := String.sub !files_prefix 0 ((String.length !files_prefix) - model_extension_size);
				);
			);

	end
