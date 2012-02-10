open Arg
open Global


class imitator_options =
	object
		val mutable nb_args = 0
	
		(* imitator program file *)
		val mutable file = ref ""
		(* pi0 file *)
		val mutable pi0file = ref ""
	
		(* print logs *)
		val mutable no_log = ref false
		(* print dot of reachable states *)
		val mutable no_dot = ref false
		(* plot fancy states in dot *)
		val mutable fancy = ref false
		(* limit number of iterations *)		
		val mutable post_limit = ref None
		(* limit on runtime *)
		val mutable time_limit = ref None
		(* limit on refinement steps *)
		val mutable cegar_limit = ref None
		(* imitator mode *)
		val mutable imitator_mode = ref Inverse_method
		(* acyclic mode *)
		val mutable acyclic = ref false 
		(* inclusion mode *)
		val mutable inclusion = ref false
		(* merging mode *)
		val mutable merge = ref false
  	(* union mode *)
		val mutable union = ref false
		(* prefix for output files *)
		val mutable program_prefix = ref ""
		(* do not use random values *)
		val mutable no_random = ref false
		(* autodetect sync actions *)
		val mutable sync_auto_detection = ref false
		(* print time stamps *)
		val mutable timed_mode = ref false
		(* print parametric logs *)
		val mutable with_parametric_log = ref false 
		(* plot cartography *)
		val mutable cart = ref false
		(* pre-image *)
		val mutable pre = ref false
		(* On-the-fly intersection*)
		val mutable dynamic = ref false
		(* variables to plot *)
		val mutable plot_vars = ref []
		(* limits for plots *)
		val mutable plot_limits = ref (Hashtbl.create 0)
		(* number of partitions for affine variables *)
		val mutable nb_part = ref 4;
				
		method nb_args = nb_args
		method file = !file
		method pi0file = !pi0file
		method no_dot = !no_dot
		method no_log = !no_log
		method fancy = !fancy
		method with_parametric_log = !with_parametric_log
		method post_limit = !post_limit
		method time_limit = !time_limit
		method cegar_limit = !cegar_limit
		method program_prefix = !program_prefix
		method imitator_mode = !imitator_mode
		method inclusion = !inclusion
		method merge = !merge
		method union = !union
		method acyclic = !acyclic
		method sync_auto_detection = !sync_auto_detection
		method no_random = !no_random
		method timed_mode = !timed_mode
		method cart = !cart
		method pre = !pre
		method dynamic = !dynamic
		method plot_vars = !plot_vars
		method plot_limits = !plot_limits
		method nb_partitions = !nb_part
		
		method parse =
			let usage_msg = "Usage: IMITATOR program_file [pi0_file] [options]" in
			
			(* auxiliary variables *)
			let plot_vars_x = ref [] in
			let plot_vars_y = ref [] in
			let limit_var = ref "" in
			let limit_lower = ref "" in
			
			(* Get the debug mode *)
			let rec set_debug_mode_ref debug_mode =
					let mode = try debug_mode_of_string debug_mode
						with Not_found ->
						print_error ("The debug mode '" ^ debug_mode ^ "' is not valid.");
						Arg.usage speclist usage_msg;
						abort_program ();
						exit(0); in
					set_debug_mode mode

			(* Get the mode *)
			and set_mode mode =
				(* Case: 'reachability' *)
				if mode = "reachability" then 
					imitator_mode := Reachability_analysis
				(* Case: inverse method *)
				else if mode = "inversemethod" then 
					imitator_mode := Inverse_method
				(* Case: cover *)
				else if mode = "cover" then 
					imitator_mode := Cover_cartography
				(* Case: number of iterations *)
				else if mode = "cegar" then
					imitator_mode := AbstractReachability
				else try (
					(* Find the 'random' string *)
					if not (String.sub mode 0 6 = "random") then raise (Failure "toto");
					(* Find the number *)
					let number = String.sub mode 6 (String.length mode - 6) in
					imitator_mode := (Random_cartography (int_of_string number))
				) with _ -> (
					print_error ("The mode '" ^ mode ^ "' is not valid.");
					Arg.usage speclist usage_msg;
					abort_program ();
					exit(0);
				)

			and add_plot_x var_x =
				plot_vars_x := var_x :: !plot_vars_x

			and add_plot_y var_y =
				plot_vars_y := var_y :: !plot_vars_y

			and add_plot_limit upper =
				Hashtbl.add !plot_limits !limit_var (!limit_lower, upper)								

			(* Options *)
			and speclist = [
				("-acyclic", Set acyclic, " Does not test if a new state was already encountered. To be set ONLY if the system is acyclic. Default: 'false'");		
				("-debug", String set_debug_mode_ref, " Print more or less debug information. Can be set to 'nodebug', 'standard', 'low', 'medium', 'high', 'total'. Default: 'standard'");
				("-inclusion", Set inclusion, " Consider an inclusion of region instead of the equality when performing the Post operation. Default: 'false'");
				("-union", Set union, " Consider last states for deriving constraint K. Default: 'false'");
				("-merge", Set merge, " Try to merge states when performing the Post operation. Default: 'false'");
				("-log-prefix", Set_string program_prefix, " Sets the prefix for log files. Default: [program_file].");
				("-mode", String set_mode, " Mode for IMITATOR II. Use 'reachability' for a parametric reachability analysis (no pi0 needed). Use 'inversemethod' for the inverse method. For the behavioral cartography algorithm, use 'cover' to cover all the points within V0, or 'randomXX' where XX is a number to iterate randomly algorithm. Use 'cegar' for reachability based on counterexample guided abstraction refinement. Default: 'inversemethod'.");
				("-pre", Set pre, " Use pre-image instead of post-image computation. Default: false.");
				("-no-dot", Set no_dot, " No graphical output using 'dot'. Default: false.");
				("-no-log", Set no_log, " No generation of log files. Default: false.");
				("-cart", Set cart, " Plot cartography before terminating the program. Uses the first two parameters with ranges. Default: false.");
				("-plot", Tuple [String add_plot_x; String add_plot_y], " Generate 2D plot of rechable states projected on the two given variables. Default: false.");
				("-limits", Tuple [Set_string limit_var; Set_string limit_lower; String add_plot_limit], " Set limits of the displayed plot area for a variable. Default: automatic");
				("-part", Int (fun i -> nb_part := i), " Number of partitions for each affine variable. Default: 4.");
				("-fancy", Set fancy, " Generate detailed state information for dot output. Default: false.");
				("-no-random", Set no_random, " No random selection of the pi0-incompatible inequality (select the first found). Default: false.");
				("-sync-auto-detect", Set sync_auto_detection, " Detect automatically the synchronized actions in each automaton. Default: false (consider the actions declared by the user)");
				("-time-limit", Int (fun i -> time_limit := Some i), " Time limit in seconds. Warning: no guarantee that the program will stop exactly after the given amount of time. Default: no limit.");
				("-post-limit", Int (fun i -> post_limit := Some i), " Limits the depth of the Post exploration. Default: no limit.");
				("-cegar-limit", Int (fun i -> cegar_limit := Some i), " Limits the number of refinement steps in CEGAR mode. Default: no limit.");				
				("-timed", Set timed_mode, " Adds a timing information to each output of the program. Default: none.");
				("-with-parametric-log", Set with_parametric_log, " Adds the elimination of the clock variables in the constraints in the log files. Default: false.");
				("-version", Unit (fun _ -> print_version_string (); exit 0), " Print version string and exit.");
				("-dynamic", Set dynamic, "Perform the on-the-fly intersection. Currently : Does not work. Defaut : 'false'");
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
					print_warning ("The program argument '" ^ arg ^ "' will be ignored.");
				)
			) in

			let usage_msg = "Usage: IMITATOR program_file [pi0_file] [options]" in
			
			(* set default debug level *)
			set_debug_mode Debug_standard;
			
			Arg.parse speclist anon_fun usage_msg;

		  (* merge plot variables *)
			plot_vars := List.combine !plot_vars_x !plot_vars_y;

			(* Case no file *)
			if nb_args < 1 then(
				print_error ("Please give a source file name.");
				Arg.usage speclist usage_msg;
				abort_program (); exit(0)
			);
			
			(* Case no pi0 file *)
			if nb_args = 1 && (!imitator_mode != Reachability_analysis && !imitator_mode != AbstractReachability) then(
				print_error ("Please give a reference valuation file name.");
				Arg.usage speclist usage_msg;
				abort_program (); exit(0)
			);
			
			(* set program prefix *)
			if !program_prefix = "" then
			  program_prefix := !file

	end
