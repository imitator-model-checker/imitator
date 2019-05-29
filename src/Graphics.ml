(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: All graphics handling (cartography, trace set…)
 * 
 * File contributors : Étienne André, Ulrich Kühne
 * Created           : 2010/07/05
 * Last modified     : 2019/05/29
 *
 ************************************************************)
 

(************************************************************)
(* Modules *)
(************************************************************)

open OCamlUtilities
open Exceptions
open Constants
open ImitatorUtilities
open Statistics
open AbstractModel
open StateSpace
open Result


(************************************************************)
(** Statistics *)
(************************************************************)
let counter_graphics_statespace = create_time_counter_and_register "state space drawing" Graphics_counter Verbose_standard


(************************************************************)
(* Plot (graph) Functions *)
(************************************************************)

(*------------------------------------------------------------*)
(* Convert a tile_index into a color for graph (actually an integer from 1 to 5) *)
(*------------------------------------------------------------*)
let graph_color_of_int tile_index statespace_nature dotted =
	(* Retrieve model *)
	let model = Input.get_model() in
	
	(* Definition of the color *)
	let color_index =
	(* If bad state defined *)
	if model.correctness_condition <> None then(
		(* Go for a good / bad coloring *)
		match statespace_nature with
		| StateSpace.Good -> 2 (* green *)
		| StateSpace.Bad -> 1 (* red *)
		| StateSpace.Unknown -> 5 (* cyan *)
	(* Else random coloring *)
	)else(
		(* Only 5 colors from 1 to 5 *)
		(tile_index mod 5) + 1
	)
	in
	(* Add an offset of 20 for dotted, and convert to string *)
	string_of_int (color_index + (if dotted then 20 else 0))



(*------------------------------------------------------------*)
(* Create a file name with radical cartography_file_prefix and number file_index *)
(*------------------------------------------------------------*)
let make_file_name cartography_file_prefix file_index =
	cartography_file_prefix ^ "_points_" ^ (string_of_int file_index) ^ ".txt"



(*------------------------------------------------------------*)
(** Draw the cartography corresponding to a list of constraints. Takes as second argument the file name prefix. *)
(*------------------------------------------------------------*)
exception CartographyError


let draw_cartography (returned_constraint_list : (LinearConstraint.p_convex_or_nonconvex_constraint * StateSpace.statespace_nature) list) cartography_file_prefix =
	(* Create counter *)
	let counter_graphics_cartography = create_time_counter_and_register "cartography drawing" Graphics_counter Verbose_standard in

	(* Statistics *)
	counter_graphics_cartography#start;
	
try(
	(* No cartography if no zone *)
	if returned_constraint_list = [] then(
		print_warning ("No cartography can be drawn since the list of constraints is empty.");
		raise CartographyError
	);

	print_message Verbose_standard ("\nDrawing the cartography…");

	(* Retrieve the model *)
	let model = Input.get_model () in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	print_message Verbose_low "Starting to compute graphical cartography…";
	
	(* Converting to convex constraints *)
	(*** BADPROG: creating a list of singletons and lists, and then flatten ***)
	let returned_constraint_list = List.flatten (List.map (function
		| LinearConstraint.Convex_p_constraint p_linear_constraint, statespace_nature -> [LinearConstraint.render_non_strict_p_linear_constraint p_linear_constraint , statespace_nature]
		| LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint, statespace_nature ->
			(* Get the convex constraints *)
			let p_linear_constraints = LinearConstraint.p_linear_constraint_list_of_p_nnconvex_constraint p_nnconvex_constraint in
			(* Render non-strict *)
			List.map (fun p_linear_constraint -> LinearConstraint.render_non_strict_p_linear_constraint p_linear_constraint, statespace_nature) p_linear_constraints
		) returned_constraint_list
	)  in
	
(*	(* For all returned_constraint *)
	let new_returned_constraint_list = List.map (
		fun returned_constraint -> match returned_constraint with
			| Convex_constraint (k, tn) -> Convex_constraint (LinearConstraint.render_non_strict_p_linear_constraint k , tn)
			| Union_of_constraints (list_of_k, tn) -> Union_of_constraints (List.map LinearConstraint.render_non_strict_p_linear_constraint list_of_k , tn)
			| NNCConstraint _ -> raise (InternalError ("NNCCs are not available everywhere yet."))
	) returned_constraint_list
	in*)
	
	(* From now on, we work with a list [(LinearConstraint.p_linear_constraint, statespace_nature), …] *)


(*	(*** HORRIBLE IMPERATIVE (and not tail recursive) programming !!!!! ***)
	(* For all returned_constraint *)
	for k = 0 to List.length returned_constraint_list -1 do
		(* For all linear_constraint *)
		for i=0 to List.length constraint_list -1 do
			let inequality_list = ppl_Polyhedron_get_constraints (List.nth constraint_list i) in 
			let new_inequality_list = ref [] in
			for j=0 to List.length inequality_list -1 do 
				new_inequality_list := (strict_to_not_strict_inequality (List.nth inequality_list j))::!new_inequality_list;
			done;
			new_constraint_list := make !new_inequality_list::!new_constraint_list;
		done;
	done;*)
	
	
	(*** TODO!!! for EF-synthesis, first draw a green background, since the non-red is necessarily green (well, unless approximations are used) ***)
	
	let x_index = 0 in
	let y_index = 1 in

	
	(* First find the dimensions *)

	print_message Verbose_low "Looking for dimensions…";
	let range_params : int list ref = ref [] in
	let bounds = ref (Array.make 2 (NumConst.zero, NumConst.zero)) in

	(* If EF-synthesis / IM: choose the first two parameters *)
	(*** TODO: take the projection into account! ***)
	begin
	match options#imitator_mode with
		(*** TODO: better use an option "is_cartography" ***)
		| EF_synthesis | EFunsafe_synthesis | EF_min | EF_max | EF_synth_min | EF_synth_max | EF_synth_min_priority_queue | AF_synthesis | Loop_synthesis | Parametric_NZ_CUBcheck | Parametric_NZ_CUBtransform | Parametric_NZ_CUB | Parametric_deadlock_checking | Inverse_method | Inverse_method_complete | PRP ->
			print_message Verbose_low "Pick up the first 2 parameters to draw the cartography";
			(* First check that there are at least 2 parameters *)
			if model.nb_parameters < 2 then(
				print_error ("Could not plot cartography (which requires 2 parameters) because there is only " ^ (string_of_int model.nb_parameters) ^ " parameter.");
				raise CartographyError
			);
			(* Choose the first 2 *)
			range_params := [ List.nth model.parameters 0 ; List.nth model.parameters 1];
			
			(* Prepare the "V0" rectangle *)
			let ef_x_min =
			match options#output_cart_x_min with
				| None -> NumConst.numconst_of_int 0 (*** WARNING: quite random thing here ! ***)
				| Some n -> NumConst.numconst_of_int n
			in
			let ef_x_max =
			match options#output_cart_x_max with
				| None -> NumConst.numconst_of_int 50 (*** WARNING: quite random thing here ! ***)
				| Some n -> NumConst.numconst_of_int n
			in
			let ef_y_min =
			match options#output_cart_y_min with
				| None -> NumConst.numconst_of_int 0 (*** WARNING: quite random thing here ! ***)
				| Some n -> NumConst.numconst_of_int n
			in
			let ef_y_max =
			match options#output_cart_y_max with
				| None -> NumConst.numconst_of_int 50 (*** WARNING: quite random thing here ! ***)
				| Some n -> NumConst.numconst_of_int n
			in
			!bounds.(0) <- ef_x_min, ef_x_max;
			!bounds.(1) <- ef_y_min, ef_y_max;
	
	(* If cartography: find indices of first two variables with a parameter range *)
	(*** TODO: better use an option "cartography mode" ***)
	(*** TODO: take the projection into account! ***)
		| Cover_cartography | Shuffle_cartography | Random_cartography _ | RandomSeq_cartography _ | Learning_cartography | Border_cartography | PRPC ->
			(* Retrieve the V0 *)
			(*** NOTE: only retrieve here because, in other mode (e.g., EF or IM) this object is not defined ***)
			let v0 = Input.get_v0 () in
			
			print_message Verbose_low "Case real cartography: first 2 parameters with a range";
			for index = 0 to model.nb_parameters - 1 do
(* 			Array.iteri (fun index (a,b) ->  *)
				let a = v0#get_min index in
				let b = v0#get_max index in
				if NumConst.neq a b then(
					(* Add one more parameter *)
					print_message Verbose_medium "Found a parameter!";
					range_params := index :: !range_params;
				)
			(* ) v0;*)
			done;
			range_params := List.rev !range_params;
			
			if (List.length !range_params) < 2 then(
				print_error "Could not plot cartography (region of interest has too few dimensions)";
				raise CartographyError
			);

			(* Update bounds *)
			!bounds.(x_index) <- v0#get_min (List.nth !range_params 0), v0#get_max (List.nth !range_params 0);
			!bounds.(y_index) <- v0#get_min (List.nth !range_params 1), v0#get_max (List.nth !range_params 1);

		
	(* Else: no reason to draw a cartography *)
		| _ -> 
			print_error "Cartography cannot be drawn in this mode.";
			raise CartographyError
	end;
	
	(*** WARNING: only works partially ***)
	(* Shrink V0 bounds if options specified *)
	begin
	match options#output_cart_x_min with
		| None -> ()
		| Some n -> let (old_min, old_max) = !bounds.(x_index) in
			let new_min = NumConst.numconst_of_int n in
			if NumConst.g new_min old_min then
				!bounds.(x_index) <- (new_min, old_max);
	end;
	begin
	match options#output_cart_x_max with
		| None -> ()
		| Some n -> let (old_min, old_max) = !bounds.(x_index) in
			let new_max = NumConst.numconst_of_int n in
			if NumConst.l new_max old_max then
				!bounds.(x_index) <- (old_min, new_max);
	end;
	begin
	match options#output_cart_y_min with
		| None -> ()
		| Some n -> let (old_min, old_max) = !bounds.(y_index) in
			let new_min = NumConst.numconst_of_int n in
			if NumConst.g new_min old_min then
				!bounds.(y_index) <- (new_min, old_max);
	end;
	begin
	match options#output_cart_y_max with
		| None -> ()
		| Some n -> let (old_min, old_max) = !bounds.(y_index) in
			let new_max = NumConst.numconst_of_int n in
			if NumConst.l new_max old_max then
				!bounds.(y_index) <- (old_min, new_max);
	end;
	
	(* Now start *)
	
	let x_pos = 0 in
	let y_pos = 1 in
	
	(* Retrieve parameters *)
	let x_param = List.nth !range_params x_pos in
	let y_param = List.nth !range_params y_pos in

	let x_name = model.variable_names x_param in
	let y_name = model.variable_names y_param in

	(* Print some information *)
(* 	print_message Verbose_standard ("Cartography will be drawn in 2D for parameters " ^ x_name ^  " and " ^ y_name ^  "."); *)
	
	(* Create a script that will print the cartography *)
	let script_name = cartography_file_prefix ^ ".sh" in
	let script = open_out script_name in
	
	(* Create a temp file containing the V0 coordinates *)
	let file_v0_name = cartography_file_prefix ^ "_v0.txt" in
	let file_rectangle_v0 = open_out file_v0_name in
	
	
	(* Convert a num_const to a string, specifically for Graph *)
	let graph_string_of_numconst n = 
		(* Check that it is an integer *)
		if not (NumConst.is_integer n) then(
			raise (InternalError("Only integers can be handled for the cartography, so far. Found: '" ^ (NumConst.string_of_numconst n) ^ "'"))
		);
		(* Convert to a string, and add a "." at the end *)
		(NumConst.string_of_numconst n) ^ "."
	in
	
	(* Print some information *)
	print_message Verbose_low ("Computing the zone…");
	
	let str_rectangle_v0 =
				(graph_string_of_numconst (fst (!bounds.(x_pos))))
		^" "^(graph_string_of_numconst (snd (!bounds.(y_pos))))
		^"\n"^(graph_string_of_numconst (snd (!bounds.(x_pos))))
		^" "^ (graph_string_of_numconst (snd (!bounds.(y_pos))))
		^"\n"^(graph_string_of_numconst (snd (!bounds.(x_pos))))
		^" "^ (graph_string_of_numconst (fst (!bounds.(y_pos))))
		^"\n"^(graph_string_of_numconst (fst (!bounds.(x_pos))))
		^" "^ (graph_string_of_numconst (fst (!bounds.(y_pos))))
		^"\n"^(graph_string_of_numconst (fst (!bounds.(x_pos))))
		^" "^ (graph_string_of_numconst (snd (!bounds.(y_pos))))
	in
	output_string file_rectangle_v0 str_rectangle_v0;
	close_out file_rectangle_v0;

	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total ("str_rectangle_v0 = \n" ^ str_rectangle_v0);

	(* Beginning of the script *)
	let script_line = ref ("graph -T " ^ cartography_extension ^ " -C -X \"" ^ x_name ^ "\" -Y \"" ^ y_name ^ "\" ") in
	(* find the minimum and maximum abscissa and ordinate for each constraint and store them in a list *)
	
	(* Print some information *)
	print_message Verbose_low ("Finding zone corners…");

	(* get corners of bounds *)
	let init_min_abs, init_max_abs = !bounds.(x_pos) in
	let init_min_ord, init_max_ord = !bounds.(y_pos) in
(*		(* convert to float *)
	let init_min_abs = float_of_int init_min_abs in
	let init_max_abs = float_of_int init_max_abs in
	let init_min_ord = float_of_int init_min_ord in
	let init_max_ord = float_of_int init_max_ord in
	
	(* find mininma and maxima for axes (version Daphne) *)
	let min_abs, max_abs, min_ord, max_ord =
	List.fold_left (fun limits constr -> 
		let points, _ = shape_of_poly x_param y_param constr in			
		List.fold_left (fun limits (x,y) ->
			let current_min_abs, current_max_abs, current_min_ord, current_max_ord = limits in
			let new_min_abs = min current_min_abs x in
			let new_max_abs = max current_max_abs x in
			let new_min_ord = min current_min_ord y in
			let new_max_ord = max current_max_ord y in
			(new_min_abs, new_max_abs, new_min_ord, new_max_ord) 
		) limits points  		 
	) (init_min_abs, init_max_abs, init_min_ord, init_max_ord) new_returned_constraint_list in*)

	(* Conversion to float, because all functions handle floats *)
	
	(*** WARNING! very dangerous here! may not work for big integers ***)
	let bad_float_of_num_const n = float_of_string (NumConst.string_of_numconst n) in

	(* Print some information *)
	print_message Verbose_low ("Finding minima and maxima for axes…");

	(* Find mininma and maxima for axes (version Etienne, who finds imperative here better ) *)
	let min_abs = ref (bad_float_of_num_const init_min_abs) in
	let max_abs = ref (bad_float_of_num_const init_max_abs) in
	let min_ord = ref (bad_float_of_num_const init_min_ord) in
	let max_ord = ref (bad_float_of_num_const init_max_ord) in
	(* Update min / max for ONE linear_constraint *)
	let update_min_max linear_constraint =
		let points, _ = LinearConstraint.shape_of_poly x_param y_param linear_constraint in
		List.iter (fun (x,y) ->
			min_abs := min !min_abs x;
			max_abs := max !max_abs x;
			min_ord := min !min_ord y;
			max_ord := max !max_ord y;
		) points;
	in
	(* Update min / max for all returned constraint *)
(*	List.iter (function
		| Convex_constraint (k, _) -> update_min_max k
		| Union_of_constraints (list_of_k, _) -> List.iter update_min_max list_of_k
		| NNCConstraint _ -> raise (InternalError ("NNCCs are not available everywhere yet."))
	) new_returned_constraint_list;*)
	List.iter (function (p_linear_constraint, _) -> update_min_max p_linear_constraint) returned_constraint_list;

	(* Print some information *)
	print_message Verbose_low ("Adding a 1-unit margin…");

	(* Add a margin of 1 unit *)
	min_abs := !min_abs -. 1.0;
	max_abs := !max_abs +. 1.0;
	min_ord := !min_ord -. 1.0;
	max_ord := !max_ord +. 1.0;
	
	(* Overwrite bounds if options specified *)
	(*** WARNING: only works partially ***)
	begin
	match options#output_cart_x_min with
		| None -> ()
		| Some n -> min_abs := float_of_int n;
(*		let (_, old_max) = !bounds.(x_index) in
			!bounds.(x_index) <- (NumConst.numconst_of_int n, old_max);*)
	end;
	begin
	match options#output_cart_x_max with
		| None -> ()
		| Some n -> max_abs := float_of_int n;
		(*let (old_min, _) = !bounds.(x_index) in
			!bounds.(x_index) <- (old_min, NumConst.numconst_of_int n);*)
	end;
	begin
	match options#output_cart_y_min with
		| None -> ()
		| Some n -> min_ord := float_of_int n;
		(*let (_, old_max) = !bounds.(y_index) in
			!bounds.(y_index) <- (NumConst.numconst_of_int n, old_max);*)
	end;
	begin
	match options#output_cart_y_max with
		| None -> ()
		| Some n -> max_ord := float_of_int n;
		(*let (old_min, _) = !bounds.(y_index) in
			!bounds.(y_index) <- (old_min, NumConst.numconst_of_int n);*)
	end;
	(*** TODO!!! do something if some min > max ***)

	
	(* print_message Verbose_standard ((string_of_float !min_abs)^"  "^(string_of_float !min_ord)); *)
	(* Create a new file for each constraint *)
(*		for i=0 to List.length !new_constraint_list-1 do
		let file_name = cartography_file_prefix^"_points_"^(string_of_int i)^".txt" in
		let file_out = open_out file_name in
		(* find the points satisfying the constraint *)
		let s=plot_2d (x_param) (y_param) (List.nth !new_constraint_list i) min_abs min_ord max_abs max_ord in
		(* print in the file the coordinates of the points *)
		output_string file_out (snd s);
		(* close the file and open it in a reading mode to read the first line *)
		close_out file_out;			
		let file_in = open_in file_name in
		let s2 = input_line file_in in
		(* close the file and open it in a writting mode to copy the whole string in it and ensure that the polygon is closed*)
		close_in file_in;
		let file_out_bis = open_out file_name in
		output_string file_out_bis ((snd s)^s2);
		close_out file_out_bis;
		(* instructions to have the zones colored. If fst s = true then the zone is infinite *)
		if fst s
			then script_line := !script_line^"-m "^(string_of_int((i mod 5)+1+20))^" -q 0.3 "^file_name^" "
			else script_line := !script_line^"-m "^(string_of_int((i mod 5)+1))^" -q 0.7 "^file_name^" "
	done;*)

	(*** BAD PROG : bouh pas beau ***)
	(*** WARNING: it looks like file_index = tile_index, always! ***)
	let file_index = ref 0 in
	let tile_index = ref 0 in
	(* Creation of files (Daphne wrote this?) *)
	let create_file_for_constraint k statespace_nature =
	
		(* Increment the file index *)
		file_index := !file_index + 1;

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			print_message Verbose_low ("Computing points for constraint " ^ (string_of_int !tile_index) ^ " \n " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names k) ^ ".");
		);
		
		let file_name = make_file_name cartography_file_prefix !file_index in
		let file_out = open_out file_name in
		
(*			(* Remove all non-parameter dimensions (the n highest) *)
		print_message Verbose_standard ("Removing the " ^ (string_of_int (model.nb_discrete + model.nb_clocks)) ^ " highest (clocks and discrete) dimensions in the constraint, to keep only the " ^ (string_of_int (model.nb_parameters)) ^ " lowest."); 
		(* Should be done already ?!! *)
		hide_assign model.clocks_and_discrete k;
		remove_dimensions (model.nb_discrete + model.nb_clocks) k ;*)
		
		(* find the points satisfying the constraint *)
		let s = LinearConstraint.plot_2d x_param y_param k !min_abs !min_ord !max_abs !max_ord in
		(* Get the points *)
		let the_points = snd s in
		(* print in the file the coordinates of the points *)
		output_string file_out the_points;

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			print_message Verbose_low ("  Points \n " ^ the_points ^ "");
		);
		
		(* Prepare the comments at the end of the file *)
		let comments =
			"\n# File automatically generated by " ^ Constants.program_name ^ ""
			^ "\n" ^"# Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build())
			^ "\n" ^"# Git      : " ^ (ImitatorUtilities.git_branch_and_hash)
			^ "\n" ^"# Model    : '" ^ options#model_input_file_name ^ "'"
			^ "\n" ^"# Command  : " ^ (OCamlUtilities.string_of_array_of_string_with_sep " " Sys.argv) ^ ""
			^ "\n" ^"# Generated: " ^ (now()) ^ ""
			(* This line is used by Giuseppe Lipari: do not change without prior agreement *)
			^ (if model.correctness_condition <> None then( 
				"\n# Tile nature: " ^ (StateSpace.string_of_statespace_nature statespace_nature) ^ ""
			) else "")
		in
		
		(* close the file and open it in a reading mode to read the first line *)
		close_out file_out;
		let file_in = open_in file_name in
		let s2 = input_line file_in in
		(* close the file and open it in a writting mode to copy the whole string in it and ensure that the polygon is closed*)
		close_in file_in;
		let file_out_bis = open_out file_name in
		output_string file_out_bis (the_points ^ s2 ^ comments);
		close_out file_out_bis;
		(* instructions to have the zones colored. If fst s = true then the zone is infinite *)
		if fst s
			(*** TODO : same color for one disjunctive tile ***)
			then script_line := !script_line ^ "-m " ^ (graph_color_of_int !tile_index statespace_nature true) ^ " -q 0.3 " ^ file_name ^ " "
			else script_line := !script_line ^ "-m " ^ (graph_color_of_int !tile_index statespace_nature false) ^ " -q 0.7 " ^ file_name ^ " "
		;
	in
	
	(* For all returned_constraint *)
(*	List.iter (function
		| Convex_constraint (k, tn) ->
			(*** WARNING: duplicate code ***)
			(* Test just in case ! (otherwise an exception arises *)
			if LinearConstraint.p_is_false k then(
				print_warning " Found a false constraint when computing the cartography. Ignored."
			)else(
				tile_index := !tile_index + 1;
				create_file_for_constraint k tn
			)
		| Union_of_constraints (list_of_k, tn) ->
			List.iter (fun k -> 
				(*** WARNING: duplicate code ***)
				(* Test just in case ! (otherwise an exception arises *)
				if LinearConstraint.p_is_false k then(
					print_warning " Found a false constraint when computing the cartography. Ignored."
				)else(
					tile_index := !tile_index + 1;
					create_file_for_constraint k tn
				)
			) list_of_k
		| NNCConstraint _ -> raise (InternalError ("NNCCs are not available everywhere yet."))
	) new_returned_constraint_list;*)
	List.iter (fun (p_linear_constraint, statespace_nature) ->
		(* Test just in case ! (otherwise an exception arises *)
		if LinearConstraint.p_is_false p_linear_constraint then(
			print_warning " Found a false constraint when computing the cartography. Ignored."
		)else(
			tile_index := !tile_index + 1;
			create_file_for_constraint p_linear_constraint statespace_nature
		)
	) returned_constraint_list;
	
	
	(* File in which the cartography will be printed *)
	let final_name = cartography_file_prefix ^ "." ^ cartography_extension in
	(* last part of the script *)	
	script_line := !script_line^" -C -m 2 -q -1 " ^ file_v0_name ^ " -L \"" ^ options#files_prefix ^ "\" > "^final_name;
	(* write the script into a file *)
	output_string script !script_line;
	(* Print some information *)
	(** TODO one day: change this string and update IMITATOR service in CosyVerif *)
	print_message Verbose_standard (
		"Plot cartography in 2D projected on parameters " ^ x_name ^ " and " ^ y_name
		^ " to file '" ^ final_name ^ "'."); 
	(* execute the script *)
	(*** TODO: Improve! Should perform an automatic detection of the model! ***)
	let execution = Sys.command !script_line in
	if execution != 0 then
		(print_error ("Something went wrong in the command. Exit code: " ^ (string_of_int execution) ^ ". Maybe you forgot to install the 'graph' utility (from the 'plotutils' package in Debian)."););
	
	(* Print some information *)
	print_message Verbose_high ("Result of the cartography execution: exit code " ^ (string_of_int execution));

	(* Remove files *)
	if not options#with_graphics_source then(
		print_message Verbose_medium ("Removing V0 file…");
		delete_file file_v0_name;
		print_message Verbose_medium ("Removing script file…");
		delete_file script_name;
		(* Removing all point files *)
		for i = 1 to !file_index do
			print_message Verbose_medium ("Removing points file #" ^ (string_of_int i) ^ "…");
			delete_file (make_file_name cartography_file_prefix i);
		done;
	);
	(* Statistics *)
	counter_graphics_cartography#stop;
	()

) with
	| CartographyError -> (print_error "Error while printing the cartography";
	(* Statistics *)
	counter_graphics_cartography#stop;
	()
	)



(************************************************************)
(* Dot Functions *)
(************************************************************)

let dot_colors = [
(* I ordered the first colors *)
(*"red" ; "green" ; *)"blue" ; "yellow" ; "cyan" ; "magenta" ;
(* The rest : random ! *)
"paleturquoise2"; "indianred1"; "goldenrod3"; "darkolivegreen4"; "slategray4"; "turquoise4"; "lightpink"; "salmon"; "pink3"; "chocolate4"; "lightslateblue"; "yellow3"; "red4"; "seashell3"; "cyan2"; "darkgoldenrod3"; "gainsboro"; "yellowgreen"; "peachpuff1"; "oldlace"; "khaki"; "deepskyblue"; "maroon3"; "gold3"; "tan"; "mediumblue"; "lightyellow"; "ivory"; "lightcyan"; "lightsalmon4"; "maroon2"; "maroon4"; "tan3"; "green2"; "ivory2"; "navyblue"; "wheat1"; "navajowhite3"; "darkkhaki"; "whitesmoke"; "goldenrod"; "gold1"; "sandybrown"; "springgreen3"; "magenta2"; "lightskyblue1"; "lightcyan3"; "khaki2"; "khaki3"; "lavender"; "orchid1"; "wheat"; "lavenderblush1"; "firebrick2"; "navajowhite4"; "darkslategray3"; "palegreen2"; "lavenderblush3"; "skyblue3"; "deepskyblue3"; "darkorange"; "magenta1"; "darkorange3"; "violetred1"; "lawngreen"; "deeppink3"; "darkolivegreen1"; "darkorange1"; "darkorchid1"; "limegreen"; "lightslategray"; "deeppink"; "red2"; "goldenrod1"; "mediumorchid4"; "cornsilk1"; 
"lemonchiffon3"; "gold"; "orchid"; "yellow2"; "lightpink4"; "violetred2"; "mediumpurple"; "lightslategrey"; "lightsalmon1"; "violetred"; "coral2"; "slategray"; "plum2"; "turquoise3"; "lightyellow3"; "green4"; "mediumorchid1"; "lightcyan1"; "lightsalmon3"; "green3"; "lightseagreen"; "mediumpurple1"; "lightskyblue"; "lightyellow2"; "firebrick"; "honeydew2"; "slateblue3"; "navajowhite"; "seagreen1"; "springgreen4"; "peru"; "springgreen2"; "mediumvioletred"; "ivory4"; "olivedrab3"; "lightyellow1"; "hotpink"; "sienna4"; "lightcyan4"; "chartreuse4"; "lemonchiffon4"; "indianred3"; "hotpink4"; "sienna1"; "slategray3"; "darkseagreen2"; "tomato3"; "honeydew3"; "mistyrose2"; "rosybrown1"; "pink2"; "powderblue"; "cornflowerblue"; "tan1"; "indianred4"; "slateblue2"; "palevioletred3"; "ivory1"; "honeydew4"; "white"; "wheat3"; "steelblue4"; "purple2"; "deeppink4"; "royalblue4"; "lightgrey"; "forestgreen"; "palegreen"; "darkorange4"; "lightsteelblue2"; "tomato4"; "royalblue1"; "hotpink1"; "hotpink3";
"palegoldenrod"; "orange3"; "yellow1"; "orange2"; "slateblue"; "lightblue"; "lavenderblush2"; "chartreuse3"; "hotpink2"; "lightblue1"; "coral1"; "orange1"; "gold2"; "lightcoral"; "mediumseagreen"; "darkgreen"; "dodgerblue1"; "khaki1"; "khaki4"; "lightblue4"; "lightyellow4"; "firebrick3"; "crimson"; "olivedrab2"; "mistyrose3"; "lightsteelblue4"; "mediumpurple3"; "maroon"; "purple1"; "mediumorchid3"; "lightblue3"; "snow4"; "pink4"; "lightgray"; "lightsteelblue1"; "mistyrose"; "lightgoldenrodyellow"; "slategray1"; "peachpuff4"; "lightsalmon2"; "lightgoldenrod4"; "linen"; "darkgoldenrod1"; "goldenrod4"; "navy"; "lightcyan2"; "darkgoldenrod"; "mediumorchid2"; "lightsalmon"; "sienna"; "lightgoldenrod"; "plum1"; "orangered4"; "mistyrose1"; "mediumorchid"; "salmon1"; "chocolate3"; "palevioletred"; "purple3"; "turquoise"; "snow"; "paleturquoise"; "darkolivegreen"; "deepskyblue2"; "honeydew1"; "midnightblue"; "steelblue2"; "darkturquoise"; "dimgray"; "mediumpurple4"; "darkorchid"; "seashell2"; "cyan2";
"olivedrab1"; "royalblue2"; "violet"; "seagreen2"; "thistle3"; "cornsilk3"; "moccasin"; "magenta3"; "mediumslateblue"; "cadetblue3"; "mediumaquamarine"; "magenta4"; "mintcream"; "orangered3"; "mistyrose4"; "darkseagreen4"; "orangered"; "palegreen4"; "mediumspringgreen"; "saddlebrown"; "plum3"; "palegreen3"; "darkviolet"; "violetred3"; "orange"; "seagreen"; "springgreen1"; "deeppink2"; "navajowhite1"; "paleturquoise4"; "tan4"; "slategrey"; "lightsteelblue"; "azure3"; "salmon4"; "olivedrab4"; "darkorchid2"; "rosybrown"; "peachpuff2"; "springgreen"; "thistle2"; "tan2"; "aquamarine2"; "rosybrown4"; "palevioletred2"; "slateblue4"; "cyan4"; "red1"; "slateblue1"; "cornsilk2"; "ivory3"; "lightpink2"; "mediumpurple2"; "sienna2"; "chocolate1"; "lightsteelblue3"; "lightgoldenrod3"; "blueviolet"; "sienna3"; "orangered1"; "lightpink3"; "mediumturquoise"; "darkorange2"; "skyblue1"; "steelblue"; "seashell4"; "salmon2"; "lightpink1"; "skyblue4"; "darkslategray4"; "palevioletred4"; "orchid2"; "blue2"; "orchid3";
"peachpuff3"; "transparent"; "lavenderblush4"; "darkslategray1"; "lemonchiffon"; "papayawhip"; "maroon1"; "skyblue"; "chocolate"; "snow2"; "pink1"; "peachpuff"; "tomato1"; "blue1"; "dodgerblue2"; "orchid4"; "plum"; "orange4"; "purple"; "royalblue3"; "pink"; "floralwhite"; "palegreen1"; "dodgerblue4"; "chartreuse"; "bisque4"; "plum4"; "darkseagreen3"; "lightskyblue3"; "darkseagreen1"; "lightblue2"; "royalblue"; "red3"; "salmon3"; "palevioletred1"; "purple4"; "burlywood1"; "chocolate2"; "darkolivegreen3"; "goldenrod2"; "seashell1"; "indianred"; "brown2"; "lemonchiffon1"; "steelblue1"; "thistle1"; "yellow4"; "lightskyblue4"; "skyblue2"; "lemonchiffon2"; "thistle4"; "tomato2"; "violetred4"; "green1"; "greenyellow"; "paleturquoise1"; "chartreuse2"; "darkseagreen"; "turquoise2"; "cyan3"; "olivedrab"; "darkslategrey"; "firebrick4"; "lightgoldenrod1"; "seagreen3"; "seagreen4"; "tomato"; "firebrick1"; "steelblue3"; "orangered2"; "lavenderblush"; "cyan1"; "snow1"; "dodgerblue3"; "rosybrown2";
"indianred2"; "blanchedalmond"; "gold4"; "paleturquoise3"; "honeydew"; "bisque2"; "bisque3"; "snow3"; "brown"; "deeppink1"; "dimgrey"; "lightgoldenrod2"; "lightskyblue2"; "navajowhite2"; "seashell"; "black"; "cadetblue1"; "cadetblue2"; "darkslategray"; "wheat2"; "burlywood"; "brown1"; "deepskyblue4"; "darkslateblue"; "deepskyblue1"; "slategray2"; "darksalmon"; "burlywood3"; "dodgerblue"; "turquoise1"; "grey"; "ghostwhite"; "thistle"; "blue4"; "cornsilk"; "azure"; "darkgoldenrod2"; "darkslategray2"; "beige"; "burlywood2"; "coral3"; "indigo"; "darkorchid4"; "coral"; "burlywood4"; "brown3"; "cornsilk4"; "wheat4"; "darkgoldenrod4"; "cadetblue4"; "brown4"; "cadetblue"; "azure4"; "darkolivegreen2"; "rosybrown3"; "coral4"; "azure2"; "blue3"; "chartreuse1"; "bisque1"; "aquamarine1"; "azure1"; "bisque"; "aquamarine4"; "antiquewhite3"; "antiquewhite2"; "darkorchid3"; "antiquewhite4"; "aquamarine3"; "aquamarine"; "antiquewhite"; "antiquewhite1"; "aliceblue"
]

(* Convert a graph to a dot file *)
let dot_of_statespace state_space algorithm_name (*~fancy*) =
	(* Retrieve the model *)
	let model = Input.get_model () in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Retrieve info from the graph *)
	let transitions = StateSpace.get_transitions_table state_space in
	let initial_state_index = get_initial_state_index state_space in
	
	(* Create the array of dot colors *)
	let dot_colors = Array.of_list dot_colors in
	
	let is_bad_location = fun global_location ->
		(* If BAD location: red *)
		match model.correctness_condition with
		(* If normal location: pick color from array *)
		| None -> false
		| Some (Unreachable unreachable_global_locations) ->
			(* Check whether the current location matches one of the unreachable global locations *)
			State.match_unreachable_global_locations unreachable_global_locations global_location
		| _ -> raise (InternalError("IMITATOR currently ony implements the non-reachability-like properties."))
	in
	
	(* Coloring function for each location *)
	let get_location_color = fun location_index is_bad ->
		try(
		(* If BAD location: red *)
		if is_bad then "red"
		else 
		(* If normal location: pick color from array *)
			dot_colors.(location_index)
		)
		(* If more colors than our array: white *)
		with Invalid_argument _ -> "white"
	in
	
	
	print_message Verbose_high "\n[dot_of_statespace] Starting to convert states to a graphics.";
	
	let header =
		(* Header *)
	"/************************************************************"
		^ "\n * File automatically generated by " ^ Constants.program_name ^ ""
		^ "\n" ^" * Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build())
		^ "\n" ^" * Model    : '" ^ options#model_input_file_name ^ "'"
		^ "\n" ^" * Algorithm: " ^ algorithm_name ^ ""
		^ "\n" ^" * Command  : " ^ (OCamlUtilities.string_of_array_of_string_with_sep " " Sys.argv) ^ ""
		^ "\n" ^" * Generated: " ^ (now()) ^ ""
(*		^ (
			match options#imitator_mode with
				| State_space_exploration -> "\n * State space exploration"
				| EF_synthesis -> "\n * EF-synthesis"
				(* Otherwise: IM / BC *)
				| _ -> 
					let pi0 = Input.get_pi0 () in
					"\n *"
					^ "\n * The following pi0 was considered:"
					^ "\n" ^ (ModelPrinter.string_of_pi0 model pi0)
		)*)
		^ "\n *"
		^ "\n * " ^ (string_of_int (nb_states state_space)) ^ " states and "
			^ (string_of_int (StateSpace.nb_transitions state_space)) ^ " transitions"
		^ "\n *" 
		^ "\n * " ^ Constants.program_name ^ " terminated " ^ (after_seconds ())
		^ "\n************************************************************/"
	in
	
	print_message Verbose_high "[dot_of_statespace] Header completed.";

	print_message Verbose_high "[dot_of_statespace] Retrieving states indexes…";

	(* Retrieve the states *)
	let state_indexes = StateSpace.all_state_indexes state_space in
	
	(* Sort the list (for better presentation in the file) *)
	let state_indexes = List.sort (fun a b -> if a = b then 0 else if a < b then -1 else 1) state_indexes in
	
	print_message Verbose_high "[dot_of_statespace] Starting to convert states…";

	let states_description =	
		(* Give the state indexes in comments *)
		  "\n"
		^ "\n  DESCRIPTION OF THE STATES"
		^
		(**** BAD PROG ****)
		(let string_states = ref "" in
			List.iter (fun state_index ->
			(* Retrieve location and constraint *)
			let global_location, linear_constraint = StateSpace.get_state state_space state_index in

			print_message Verbose_high ("[dot_of_statespace] Converting state " ^ (string_of_int state_index) ^ "");

			(* Eliminate clocks *)
			let parametric_constraint = LinearConstraint.px_hide_nonparameters_and_collapse linear_constraint in
			
			(* Construct the string *)
			string_states := !string_states
				(* Add the state *)
				^ "\n\n  /************************************************************/"
				^ (if initial_state_index = state_index then ("\n  INITIAL") else "")
				^ "\n  STATE " ^ (string_of_int state_index) ^ ":"
				^ "\n  " ^ (ModelPrinter.string_of_state model (global_location, linear_constraint))
				(* Add the projection of the constraint onto the parameters *)
				^ (
					"\n\n  Projection onto the parameters:"
					^ "\n  " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names parametric_constraint);
				)
				^
				(* Add the projection onto selected parameters, if any *)
				(
				match model.projection with
					| None -> ""
					| Some parameters ->
						(* Compute variables to eliminate *)
						(*** TODO: do only once for all… ***)
						let all_but_projectparameters = list_diff model.parameters parameters in
						(* Project *)
						let projected_constraint = LinearConstraint.p_hide all_but_projectparameters parametric_constraint in
						(* Print *)
						"\n\n  Projection onto selected parameters {" ^ (string_of_list_of_string_with_sep "," (List.map model.variable_names parameters)) ^ "}:"
						^ "\n  " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint);
				)
				;
			) state_indexes;
		!string_states)
		^ "\n"
	in
	
	print_message Verbose_high "[dot_of_statespace] Starting to convert transitions…";

	let transitions_description =
		(* Convert the transitions for humans *)
		"\n  DESCRIPTION OF THE TRANSITIONS"
		(* We iterate by updating the current string *)
		^ (Hashtbl.fold (fun source_index successors current_string ->
			current_string ^ (string_of_list_of_string (
				List.map (fun (combined_transition , target_index) ->
					(* Get the action of the combined_transition *)
					let action_index = StateSpace.get_action_from_combined_transition combined_transition in
					
					(*** HACK! ***)
					let is_nosync action_name =
						String.length action_name >= 7 &&
						String.sub action_name 0 7 = "nosync_" in
						
					let action_name = model.action_names action_index in
					let label = if is_nosync action_name then (
						""
					) else (
						" via \"" ^ action_name ^ "\""
					) in
					
					  "\n  "
					^ "s_" ^ (string_of_int source_index)
					^ " -> "
					^ "s_" ^ (string_of_int target_index)
					^ label
				) successors
			))
			
		) transitions ""
		)
		^ "\n"
	in
	
	print_message Verbose_high "[dot_of_statespace] Generating dot file…";
	
	let dot_file =
		"\n\ndigraph G { label=\"Trace set for
" ^ options#model_input_file_name ^ "\" labeljust=\"c\" labelloc=\"top\" "
			
		(* Generator information *)
(*		^ "\ninfos[shape=record, style=filled, fillcolor=\"0.99,0.98,0.99\", fontsize=18, label=\"{Trace set for|" ^ options#model_input_file_name ^ "|Generated by " ^ (ImitatorUtilities.program_name_and_version_and_build()) ^ "|Generation time: " ^ (now()) ^ "}\"];"*)
(*		^ "\nname[shape=none, style=bold, fontsize=24, label=\"\"];"
		^ "\ngenerator[shape=none, style=bold, fontsize=10, label=\"\"];"
		^ "\ndate[shape=none, style=bold, fontsize=10, label=\"\"];"*)
(*		(* To ensure the vertical ordering *)
		^ "\n traceset -> name [color=white];"
		^ "\n name -> generator [color=white];"
		^ "\n generator -> date [color=white];"*)
		
		(* Convert the transitions for dot *)
		(* We iterate by updating the current string *)
		^ (Hashtbl.fold (fun source_index successors current_string ->
			current_string ^ (string_of_list_of_string (
				List.map (fun (combined_transition , target_index) ->
					(* Get the action of the combined_transition *)
					let action_index = StateSpace.get_action_from_combined_transition combined_transition in
					
					(*** HACK! ***)
					let is_nosync action_name =
						String.length action_name >= 7 &&
						String.sub action_name 0 7 = "nosync_" in
						
					let action_name = model.action_names action_index in
					let label = if is_nosync action_name then (
						";"
					) else (
						" [label=\"" ^ action_name ^ "\"];"
					) in

					  "\n  "
					^ "s_" ^ (string_of_int source_index)
					^ " -> "
					^ "s_" ^ (string_of_int target_index)
					^ label
				) successors
			))
			) transitions ""
		)

		(** HANDLE INITIAL STATE *)
		^ "\n\n/* Initial state */"
		^ "\n  s_init [shape=none, label=\"init\"];"
		^ "\n  s_init -> s_" ^ (string_of_int initial_state_index) ^ ";"

		(** NOW HANDLE STATES *)
		^ "\n\n/* Colors */\n" ^
		(**** BAD PROG ****)
		(let states_encoding = ref "" in
			iterate_on_states (fun state_index (location_index, linear_constraint) ->
			
(*			(* Find the location index *)
			let location_index = try
				(**** BAD PROG: should be hashed ****)
				(* If the location index exists: return it *)
				DynArray.index_of (fun some_location -> some_location = location) locations
				(* Else add the location *)
				with Not_found -> (DynArray.add locations location; DynArray.length locations - 1)
			in*)
			
			(* Get the location *)
			let global_location = get_location state_space location_index in
			
			(* Check whether is bad *)
			let is_bad = is_bad_location global_location in
			
			(* Find the location color *)
			let location_color = get_location_color location_index is_bad in
			
			(* create node index *)
			let node_index = "s_" ^ (string_of_int state_index) in

			if options#graphical_state_space = Graphical_state_space_normal || options#graphical_state_space = Graphical_state_space_verbose then (
				(* create record label with location names *)
				let loc_names = List.map (fun aut_index -> 
					(* Print a structure of the form "automaton_name : location_name" *)
					let loc_index = Location.get_location global_location aut_index in
					let automaton_name = model.automata_names aut_index in
					let location_name = model.location_names aut_index loc_index in
					automaton_name ^ " : " ^ location_name
				) model.automata in
				let label = string_of_list_of_string_with_sep "|" loc_names in
				let label_discrete = 
					if model.nb_discrete > 0 then (
						"|" ^ (string_of_list_of_string_with_sep "|" (
							List.map (fun discrete_index -> 
	(* 						let loc_index = Location.get_location global_location aut_index in *)
								"v("
								(* Variable name *)
								^ (model.variable_names discrete_index)
								(* Equal *)
								^ ")="
								(* Variable value *)
								^ (NumConst.string_of_numconst (Location.get_discrete_value global_location discrete_index))
							) model.discrete
						))
					) else ""
				in
				
				(* Print constraint if requested *)
				let pxd_constraint =
					if options#graphical_state_space = Graphical_state_space_verbose then "|{" ^ (escape_string_for_dot (LinearConstraint.string_of_px_linear_constraint model.variable_names linear_constraint)) else ""
				in
				let p_constraint =
					if options#graphical_state_space = Graphical_state_space_verbose then (
					(* Eliminate clocks *)
					(*** WARNING: already done earlier; hence loss of efficiency ***)
					let parametric_constraint = LinearConstraint.px_hide_nonparameters_and_collapse linear_constraint in
					"|"
					^ (escape_string_for_dot (LinearConstraint.string_of_p_linear_constraint model.variable_names parametric_constraint))
					^
					(* Add the projection onto selected parameters, if any *)
					(
					match model.projection with
						| None -> ""
						| Some parameters ->
							(* Compute variables to eliminate *)
							(*** TODO: do only once for all… ***)
							let all_but_projectparameters = list_diff model.parameters parameters in
							(* Project *)
							(*** WARNING: already done earlier; hence loss of efficiency ***)
							let projected_constraint = LinearConstraint.p_hide all_but_projectparameters parametric_constraint in
							(* Print *)
							"|" ^ (escape_string_for_dot (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint));
					)
					^ "}"
					) else ""
				in
				
				(* Create the command *)
				states_encoding := !states_encoding
					^ "\n  " ^ node_index
					^ "[fillcolor=" ^ location_color
					^ ", style=filled, shape=Mrecord, label=\"" 
					^ node_index
					(* Add a smiley if bad location *)
					^ (if is_bad then "\n:-(" else "")
					^ "|{" 
					^ label ^ label_discrete ^ "}" ^ pxd_constraint ^ p_constraint ^ "\"];";
			) else (
				(* Create the command *)
				states_encoding := !states_encoding
					^ "\n  " ^ node_index
					^ " [color=" ^ location_color
					^ ", style=filled];";
			)
			) state_space;
		!states_encoding)
		
		(* Version and generation time infos *)
		^ "\ngeneration[rotation=90.0, shape=rectangle, fontsize=10, label=\"Generated by " ^ (ImitatorUtilities.program_name_and_version_and_build()) ^ "
Git hash: " ^ ImitatorUtilities.git_branch_and_hash ^ "
Generation time: " ^ (now()) ^ "\"];"
		
		^ "\n}"

	in
	print_message Verbose_high "[dot_of_statespace] Done.";

	(* Dot file *)
	header ^ dot_file,
	(* Description of the states (for human) *)
	header ^ states_description ^ transitions_description


(** Execute the 'dot' utility with as argument the image format, the radical, and the source file *)
let dot dot_image_extension radical dot_source_file =
	(* Retrieve the model *)
(* 	let model = Input.get_model () in *)
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Do not write if no dot AND no log *)
	
	(*** WARNING! that's bad programming… it shouldn't be the role of this function to test options ***)
	
	if options#graphical_state_space <> Graphical_state_space_none || options#with_log then (
		(* Get the file names *)
		let dot_file_name = (radical ^ "." ^ dot_file_extension) in
		let image_file_name = (radical ^ "." ^ dot_image_extension) in
		
		(* New line *)
		print_message Verbose_standard "";
		
		(* Create the input file *)
		print_message Verbose_medium ("Creating input file for dot…");

		if options#graphical_state_space <> Graphical_state_space_none then (
			(* Write dot file *)
			if options#with_graphics_source then(
				print_message Verbose_standard ("Creating source file for dot…");
			)else(
				print_message Verbose_medium ("Writing to dot file…");
			);
			write_to_file dot_file_name dot_source_file;

			(* Generate gif file using dot *)
			(*** WARNING: don't change this string (parsed by CosyVerif) **)
			(*** TODO: add CosyVerif mode with output of the form "key : value" **)
			print_message Verbose_standard ("Generating graphical output to '" ^ image_file_name ^ "'…");
			begin
			try (
				let command_result = Sys.command (dot_command ^ " -T" ^ dot_image_extension ^ " " ^ dot_file_name ^ " -o " ^ image_file_name ^ "") in
				print_message Verbose_medium ("Result of the 'dot' command: " ^ (string_of_int command_result));
				
				if command_result != 0 then
					print_error ("Something went wrong when calling 'dot'. Exit code: " ^ (string_of_int command_result) ^ ". Maybe you forgot to install the 'dot' utility (from the 'graphviz' package in Debian).");
			) with 
				| Sys_error error_message -> print_error ("System error while calling 'dot'. Error message: '" ^ error_message ^ "'.");
			end;
			
			(* Removing dot file (except if option) *)
			if not options#with_graphics_source then(
				print_message Verbose_medium ("Removing dot file…");
				delete_file dot_file_name;
			);
		); (* end if graphical_state_space *)
	)
	

(** Draw the state space using dot *)
let draw_statespace state_space algorithm_name radical =
	(* Preliminary check: the state space shall not be empty *)
	if (StateSpace.nb_states state_space) = 0 then (
		print_warning "State space is empty: not drawing";
	)else(
	
	(* Statistics *)
	counter_graphics_statespace#start;

	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Do not write if no dot AND no log *)
	if options#graphical_state_space <> Graphical_state_space_none || options#with_log then (
		let dot_model, states = dot_of_statespace state_space algorithm_name (*~fancy:options#fancy*) in
		
		dot state_space_image_format radical dot_model;
		
		(*(* Get the file names *)
		let dot_file_name = (radical ^ "." ^ dot_file_extension) in
		let states_file_name = (radical ^ "." ^ states_file_extension) in
		let gif_file_name = (radical ^ "." ^ dot_image_extension) in
		
		(* New line *)
		print_message Verbose_standard "";
		
		(* Create the input file *)
		print_message Verbose_medium ("Creating input file for dot…");

		if options#graphical_state_space then (
			(* Write dot file *)
			if options#graphical_state_space_source then(
				print_message Verbose_standard ("Creating source file for dot…");
			)else(
				print_message Verbose_medium ("Writing to dot file…");
			);
			write_to_file dot_file_name dot_model;

			(* Generate gif file using dot *)
			print_message Verbose_standard "Generating graphical output…";
			print_message Verbose_medium ("Calling dot…");
			let command_result = Sys.command (dot_command ^ " -T" ^ dot_image_extension ^ " " ^ dot_file_name ^ " -o " ^ gif_file_name ^ "") in
			print_message Verbose_medium ("Result of the 'dot' command: " ^ (string_of_int command_result));
			
			(* Removing dot file (except if option) *)
			if not options#graphical_state_space_source then(
				print_message Verbose_medium ("Removing dot file…");
				delete_file dot_file_name;
			);
		);*)
			
		(* Write states file *)
		if options#with_log then (
			let states_file_name = (radical ^ "." ^ states_file_extension) in
			print_message Verbose_standard ("Writing the states description to file '" ^ states_file_name ^ "'…");
			write_to_file states_file_name states;
		);
	);
	
	(* Statistics *)
	counter_graphics_statespace#stop;
	
	); (* end if state space not empty *)
	
	(* The end *)
	()





(************************************************************)
(* Meta programming (could have been written in Python for example) *)
(************************************************************)


(*let dot_colors = [

(* Then all the other one by alphabetic order *)
"aliceblue" ; "antiquewhite" ; "antiquewhite1" ; "antiquewhite2" ; "antiquewhite3" ;
"antiquewhite4" ; "aquamarine" ; "aquamarine1" ; "aquamarine2" ; "aquamarine3" ; 
"aquamarine4" ; "azure" ; "azure1" ; "azure2" ; "azure3" ; 
"azure4" ; "beige" ; "bisque" ; "bisque1" ; "bisque2" ; 
"bisque3" ; "bisque4" ; "black" ; "blanchedalmond" ; (*"blue" ;*) 
"blue1" ; "blue2" ; "blue3" ; "blue4" ; "blueviolet" ; 
"brown" ; "brown1" ; "brown2" ; "brown3" ; "brown4" ; 
"burlywood" ; "burlywood1" ; "burlywood2" ; "burlywood3" ; "burlywood4" ; 
"cadetblue" ; "cadetblue1" ; "cadetblue2" ; "cadetblue3" ; "cadetblue4" ; 
"chartreuse" ; "chartreuse1" ; "chartreuse2" ; "chartreuse3" ; "chartreuse4" ; 
"chocolate" ; "chocolate1" ; "chocolate2" ; "chocolate3" ; "chocolate4" ; 
"coral" ; "coral1" ; "coral2" ; "coral3" ; "coral4" ; 
"cornflowerblue" ; "cornsilk" ; "cornsilk1" ; "cornsilk2" ; "cornsilk3" ; 
"cornsilk4" ; "crimson" ; "cyan" ; "cyan1" ; "cyan2" ; 
"cyan3" ; "cyan4" ; "darkgoldenrod" ; "darkgoldenrod1" ; "darkgoldenrod2" ; 
"darkgoldenrod3" ; "darkgoldenrod4" ; "darkgreen" ; "darkkhaki" ; "darkolivegreen" ; 
"darkolivegreen1" ; "darkolivegreen2" ; "darkolivegreen3" ; "darkolivegreen4" ; "darkorange" ; 
"darkorange1" ; "darkorange2" ; "darkorange3" ; "darkorange4" ; "darkorchid" ; 
"darkorchid1" ; "darkorchid2" ; "darkorchid3" ; "darkorchid4" ; "darksalmon" ; 
"darkseagreen" ; "darkseagreen1" ; "darkseagreen2" ; "darkseagreen3" ; "darkseagreen4" ; 
"darkslateblue" ; "darkslategray" ; "darkslategray1" ; "darkslategray2" ; "darkslategray3" ; 
"darkslategray4" ; "darkslategrey" ; "darkturquoise" ; "darkviolet" ; "deeppink" ; 
"deeppink1" ; "deeppink2" ; "deeppink3" ; "deeppink4" ; "deepskyblue" ; 
"deepskyblue1" ; "deepskyblue2" ; "deepskyblue3" ; "deepskyblue4" ; "dimgray" ; 
"dimgrey" ; "dodgerblue" ; "dodgerblue1" ; "dodgerblue2" ; "dodgerblue3" ; 
"dodgerblue4" ; "firebrick" ; "firebrick1" ; "firebrick2" ; "firebrick3" ; 
"firebrick4" ; "floralwhite" ; "forestgreen" ; "gainsboro" ; "ghostwhite" ; 
"   gold   " ; "gold1" ; "gold2" ; "gold3" ; "gold4" ; 
"goldenrod" ; "goldenrod1" ; "goldenrod2" ; "goldenrod3" ; "goldenrod4" ; 
(*"   gray   " ; "gray0" ; "gray1" ; "gray2" ; "gray3" ; 
"gray4" ; "gray5" ; "gray6" ; "gray7" ; "gray8" ; 
"gray9" ; "gray10" ; "gray11" ; "gray12" ; "gray13" ; 
"gray14" ; "gray15" ; "gray16" ; "gray17" ; "gray18" ; 
"gray19" ; "gray20" ; "gray21" ; "gray22" ; "gray23" ; 
"gray24" ; "gray25" ; "gray26" ; "gray27" ; "gray28" ; 
"gray29" ; "gray30" ; "gray31" ; "gray32" ; "gray33" ; 
"gray34" ; "gray35" ; "gray36" ; "gray37" ; "gray38" ; 
"gray39" ; "gray40" ; "gray41" ; "gray42" ; "gray43" ; 
"gray44" ; "gray45" ; "gray46" ; "gray47" ; "gray48" ; 
"gray49" ; "gray50" ; "gray51" ; "gray52" ; "gray53" ; 
"gray54" ; "gray55" ; "gray56" ; "gray57" ; "gray58" ; 
"gray59" ; "gray60" ; "gray61" ; "gray62" ; "gray63" ; 
"gray64" ; "gray65" ; "gray66" ; "gray67" ; "gray68" ; 
"gray69" ; "gray70" ; "gray71" ; "gray72" ; "gray73" ; 
"gray74" ; "gray75" ; "gray76" ; "gray77" ; "gray78" ; 
"gray79" ; "gray80" ; "gray81" ; "gray82" ; "gray83" ; 
"gray84" ; "gray85" ; "gray86" ; "gray87" ; "gray88" ; 
"gray89" ; "gray90" ; "gray91" ; "gray92" ; "gray93" ; 
"gray94" ; "gray95" ; "gray96" ; "gray97" ; "gray98" ; 
"gray99" ; "gray100" ;*) (*"green" ;*) "green1" ; "green2" ; 
"green3" ; "green4" ; "greenyellow" ; "grey"; (* "grey0" ; 
"grey1" ; "grey2" ; "grey3" ; "grey4" ; "grey5" ; 
"grey6" ; "grey7" ; "grey8" ; "grey9" ; "grey10" ; 
"grey11" ; "grey12" ; "grey13" ; "grey14" ; "grey15" ; 
"grey16" ; "grey17" ; "grey18" ; "grey19" ; "grey20" ; 
"grey21" ; "grey22" ; "grey23" ; "grey24" ; "grey25" ; 
"grey26" ; "grey27" ; "grey28" ; "grey29" ; "grey30" ; 
"grey31" ; "grey32" ; "grey33" ; "grey34" ; "grey35" ; 
"grey36" ; "grey37" ; "grey38" ; "grey39" ; "grey40" ; 
"grey41" ; "grey42" ; "grey43" ; "grey44" ; "grey45" ; 
"grey46" ; "grey47" ; "grey48" ; "grey49" ; "grey50" ; 
"grey51" ; "grey52" ; "grey53" ; "grey54" ; "grey55" ; 
"grey56" ; "grey57" ; "grey58" ; "grey59" ; "grey60" ; 
"grey61" ; "grey62" ; "grey63" ; "grey64" ; "grey65" ; 
"grey66" ; "grey67" ; "grey68" ; "grey69" ; "grey70" ; 
"grey71" ; "grey72" ; "grey73" ; "grey74" ; "grey75" ; 
"grey76" ; "grey77" ; "grey78" ; "grey79" ; "grey80" ; 
"grey81" ; "grey82" ; "grey83" ; "grey84" ; "grey85" ; 
"grey86" ; "grey87" ; "grey88" ; "grey89" ; "grey90" ; 
"grey91" ; "grey92" ; "grey93" ; "grey94" ; "grey95" ; 
"grey96" ; "grey97" ; "grey98" ; "grey99" ; "grey100" ; *)
"honeydew" ; "honeydew1" ; "honeydew2" ; "honeydew3" ; "honeydew4" ; 
"hotpink" ; "hotpink1" ; "hotpink2" ; "hotpink3" ; "hotpink4" ; 
"indianred" ; "indianred1" ; "indianred2" ; "indianred3" ; "indianred4" ; 
"indigo" ; "ivory" ; "ivory1" ; "ivory2" ; "ivory3" ; 
"ivory4" ; "khaki" ; "khaki1" ; "khaki2" ; "khaki3" ; 
"khaki4" ; "lavender" ; "lavenderblush" ; "lavenderblush1" ; "lavenderblush2" ; 
"lavenderblush3" ; "lavenderblush4" ; "lawngreen" ; "lemonchiffon" ; "lemonchiffon1" ; 
"lemonchiffon2" ; "lemonchiffon3" ; "lemonchiffon4" ; "lightblue" ; "lightblue1" ; 
"lightblue2" ; "lightblue3" ; "lightblue4" ; "lightcoral" ; "lightcyan" ; 
"lightcyan1" ; "lightcyan2" ; "lightcyan3" ; "lightcyan4" ; "lightgoldenrod" ; 
"lightgoldenrod1" ; "lightgoldenrod2" ; "lightgoldenrod3" ; "lightgoldenrod4" ; "lightgoldenrodyellow" ; 
"lightgray" ; "lightgrey" ; "lightpink" ; "lightpink1" ; "lightpink2" ; 
"lightpink3" ; "lightpink4" ; "lightsalmon" ; "lightsalmon1" ; "lightsalmon2" ; 
"lightsalmon3" ; "lightsalmon4" ; "lightseagreen" ; "lightskyblue" ; "lightskyblue1" ; 
"lightskyblue2" ; "lightskyblue3" ; "lightskyblue4" ; "lightslateblue" ; "lightslategray" ; 
"lightslategrey" ; "lightsteelblue" ; "lightsteelblue1" ; "lightsteelblue2" ; "lightsteelblue3" ; 
"lightsteelblue4" ; "lightyellow" ; "lightyellow1" ; "lightyellow2" ; "lightyellow3" ; 
"lightyellow4" ; "limegreen" ; "linen" ; (*"magenta" ;*) "magenta1" ; 
"magenta2" ; "magenta3" ; "magenta4" ; "maroon" ; "maroon1" ; 
"maroon2" ; "maroon3" ; "maroon4" ; "mediumaquamarine" ; "mediumblue" ; 
"mediumorchid" ; "mediumorchid1" ; "mediumorchid2" ; "mediumorchid3" ; "mediumorchid4" ; 
"mediumpurple" ; "mediumpurple1" ; "mediumpurple2" ; "mediumpurple3" ; "mediumpurple4" ; 
"mediumseagreen" ; "mediumslateblue" ; "mediumspringgreen" ; "mediumturquoise" ; "mediumvioletred" ; 
"midnightblue" ; "mintcream" ; "mistyrose" ; "mistyrose1" ; "mistyrose2" ; 
"mistyrose3" ; "mistyrose4" ; "moccasin" ; "navajowhite" ; "navajowhite1" ; 
"navajowhite2" ; "navajowhite3" ; "navajowhite4" ; "   navy   " ; "navyblue" ; 
"oldlace" ; "olivedrab" ; "olivedrab1" ; "olivedrab2" ; "olivedrab3" ; 
"olivedrab4" ; "orange" ; "orange1" ; "orange2" ; "orange3" ; 
"orange4" ; "orangered" ; "orangered1" ; "orangered2" ; "orangered3" ; 
"orangered4" ; "orchid" ; "orchid1" ; "orchid2" ; "orchid3" ; 
"orchid4" ; "palegoldenrod" ; "palegreen" ; "palegreen1" ; "palegreen2" ; 
"palegreen3" ; "palegreen4" ; "paleturquoise" ; "paleturquoise1" ; "paleturquoise2" ; 
"paleturquoise3" ; "paleturquoise4" ; "palevioletred" ; "palevioletred1" ; "palevioletred2" ; 
"palevioletred3" ; "palevioletred4" ; "papayawhip" ; "peachpuff" ; "peachpuff1" ; 
"peachpuff2" ; "peachpuff3" ; "peachpuff4" ; "   peru   " ; "   pink   " ; 
"pink1" ; "pink2" ; "pink3" ; "pink4" ; "   plum   " ; 
"plum1" ; "plum2" ; "plum3" ; "plum4" ; "powderblue" ; 
"purple" ; "purple1" ; "purple2" ; "purple3" ; "purple4" ; 
(*"red" ;*) "red1" ; "   red2   " ; "   red3   " ; "   red4   " ; 
"rosybrown" ; "rosybrown1" ; "rosybrown2" ; "rosybrown3" ; "rosybrown4" ; 
"royalblue" ; "royalblue1" ; "royalblue2" ; "royalblue3" ; "royalblue4" ; 
"saddlebrown" ; "salmon" ; "salmon1" ; "salmon2" ; "salmon3" ; 
"salmon4" ; "sandybrown" ; "seagreen" ; "seagreen1" ; "seagreen2" ; 
"seagreen3" ; "seagreen4" ; "seashell" ; "seashell1" ; "seashell2" ; 
"seashell3" ; "seashell4" ; "sienna" ; "sienna1" ; "sienna2" ; 
"sienna3" ; "sienna4" ; "skyblue" ; "skyblue1" ; "skyblue2" ; 
"skyblue3" ; "skyblue4" ; "slateblue" ; "slateblue1" ; "slateblue2" ; 
"slateblue3" ; "slateblue4" ; "slategray" ; "slategray1" ; "slategray2" ; 
"slategray3" ; "slategray4" ; "slategrey" ; "   snow   " ; "snow1" ; 
"snow2" ; "snow3" ; "snow4" ; "springgreen" ; "springgreen1" ; 
"springgreen2" ; "springgreen3" ; "springgreen4" ; "steelblue" ; "steelblue1" ; 
"steelblue2" ; "steelblue3" ; "steelblue4" ; "   tan   " ; "   tan1   " ; 
"   tan2   " ; "   tan3   " ; "   tan4   " ; "thistle" ; "thistle1" ; 
"thistle2" ; "thistle3" ; "thistle4" ; "tomato" ; "tomato1" ; 
"tomato2" ; "tomato3" ; "tomato4" ; "transparent" ; "turquoise" ; 
"turquoise1" ; "turquoise2" ; "turquoise3" ; "turquoise4" ; "violet" ; 
"violetred" ; "violetred1" ; "violetred2" ; "violetred3" ; "violetred4" ; 
"wheat" ; "wheat1" ; "wheat2" ; "wheat3" ; "wheat4" ; 
"white" ; "whitesmoke" ; (*"yellow" ;*) "yellow1" ; "yellow2" ; 
"yellow3" ; "yellow4" ; "yellowgreen"
]*)


(*(* Shuffle dot colors: should be executed only once!! *)
let shuffle_dot_colors =
	let shuffle = 
		Array.sort (fun _ _ -> (Random.int 3) - 1)
	in
	let colors = Array.of_list dot_colors in
	shuffle colors;
	Array.iter (fun color ->
		print_string ("\"" ^ color ^ "\"; ");
	) colors;
	terminate_program();*)

