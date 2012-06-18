(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2012/06/18
 *
 ****************************************************************)

(**************************************************)
(* Modules *)
(**************************************************)

open Global
open LinearConstraint
module Ppl = Ppl_ocaml
open Ppl
open AbstractModel
open Reachability



(**************************************************)
(**************************************************)
(* GLOBAL CONSTANTS *)
(**************************************************)
(**************************************************)

let dot_command = "dot"
let dot_image_extension = "jpg"
let dot_file_extension = "dot"
let states_file_extension = "states"






(**************************************************)
(* Functions *)
(**************************************************)

(* transform a strict inequality into a not strict inequality *)
let strict_to_not_strict_inequality inequality =
	match inequality with
		|Less_Than (x,y) -> Less_Or_Equal (x,y)
		|Greater_Than (x,y) -> Greater_Or_Equal (x,y)
		|_ -> inequality


(* print the cartography which correspond to the list of constraint *)
let cartography program pi0cube returned_constraint_list cartography_name =
	(* Replace strict inequalities with large inequalities *)
(* 	let new_constraint_list = ref [] in  *)

	(* Local function to replace strict inequalities within a linear_constraint *)
	let replace_strict_inequalities_in_k k =
		(* Get the list of inequalities *)
		let inequality_list = ppl_Polyhedron_get_constraints k in 
		(* Replace inequelities and convert back to a linear_constraint *)
			make (List.map strict_to_not_strict_inequality inequality_list)
	in

	(* For all returned_constraint *)
	let new_returned_constraint_list = List.map (
		fun returned_constraint -> match returned_constraint with
			| Convex_constraint k -> Convex_constraint (replace_strict_inequalities_in_k k)
			| Union_of_constraints list_of_k -> Union_of_constraints (List.map replace_strict_inequalities_in_k list_of_k)
	) returned_constraint_list
	in


(*	(**** HORRIBLE IMPERATIVE (and not tail recursive) programming !!!!! ******)
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

	(* find indices of first two variables with a parameter range *)
	let range_params = ref [] in
	Array.iteri (fun index (a,b) -> 
		if a <> b then range_params := index :: !range_params
	) pi0cube;
	range_params := List.rev !range_params;

	if (List.length !range_params) < 2 then
		print_error "Could not plot cartography (region of interest has too few dimensions)"
	else ( 

		let x_param = List.nth !range_params 0 in
		let y_param = List.nth !range_params 1 in
	
		let x_name = program.variable_names x_param in
		let y_name = program.variable_names y_param in
		(* Create a script that will print the cartography *)
		let script_name = cartography_name ^ ".sh" in
		let script = open_out script_name in
		(* Find the V0 zone *)
		let file_v0_name = cartography_name^"_v0.txt" in
		let file_zone = open_out file_v0_name in
		let str_zone = ((string_of_float (float_of_int (fst (pi0cube.(x_param)))))^" "^(string_of_float (float_of_int (snd (pi0cube.(y_param)))))^"\n"^	(string_of_float (float_of_int (snd (pi0cube.(x_param)))))^" "^(string_of_float (float_of_int (snd (pi0cube.(y_param)))))^"\n"^	(string_of_float (float_of_int (snd (pi0cube.(x_param)))))^" "^(string_of_float (float_of_int (fst (pi0cube.(y_param)))))^"\n"^	(string_of_float (float_of_int (fst (pi0cube.(x_param)))))^" "^(string_of_float (float_of_int (fst (pi0cube.(y_param)))))^"\n"^	(string_of_float (float_of_int (fst (pi0cube.(x_param)))))^" "^(string_of_float (float_of_int (snd (pi0cube.(y_param)))))) in
		output_string file_zone str_zone;
		close_out file_zone;
		(* Beginning of the script *)
		let script_line = ref ("graph -T ps -C -X \"" ^ x_name ^ "\" -Y \"" ^ y_name ^ "\" ") in
		(* find the minimum and maximum abscissa and ordinate for each constraint and store them in a list *)
		
		(* get corners of v0 *)
		let init_min_abs, init_max_abs = pi0cube.(x_param) in
		let init_min_ord, init_max_ord = pi0cube.(y_param) in
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

		(* Find mininma and maxima for axes (version Etienne, who finds imperative here better ) *)
		let min_abs = ref (float_of_int init_min_abs) in
		let max_abs = ref (float_of_int init_max_abs) in
		let min_ord = ref (float_of_int init_min_ord) in
		let max_ord = ref (float_of_int init_max_ord) in
		(* Update min / max for ONE linear_constraint *)
		let update_min_max linear_constraint =
			let points, _ = shape_of_poly x_param y_param linear_constraint in
			List.iter (fun (x,y) ->
				min_abs := min !min_abs x;
				max_abs := max !max_abs x;
				min_ord := min !min_ord y;
				max_ord := max !max_ord y;
			) points;
		in
		(* Update min / max for all returned constraint *)
		List.iter (function
			| Convex_constraint k -> update_min_max k
			| Union_of_constraints list_of_k -> List.iter update_min_max list_of_k
		) new_returned_constraint_list;

		(* Add a margin of 1 unit *)
		let min_abs = !min_abs -. 1.0 in
		let max_abs = !max_abs +. 1.0 in
		let min_ord = !min_ord -. 1.0 in
		let max_ord = !max_ord +. 1.0 in
		
		(* print_message Debug_standard ((string_of_float !min_abs)^"  "^(string_of_float !min_ord)); *)
		(* Create a new file for each constraint *)
(*		for i=0 to List.length !new_constraint_list-1 do
			let file_name = cartography_name^"_points_"^(string_of_int i)^".txt" in
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

		(*** BAD PROG : bouh pas beau *)
		let file_index = ref 0 in
		let tile_index = ref 0 in
		(* Creation of files (Daphne wrote this?) *)
		let create_file_for_constraint k =
			let file_name = cartography_name^"_points_"^(string_of_int !file_index) ^ ".txt" in
			let file_out = open_out file_name in
			(* find the points satisfying the constraint *)
			let s=plot_2d (x_param) (y_param) k min_abs min_ord max_abs max_ord in
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
				(*** TO DO : same color for one disjunctive tile *)
				then script_line := !script_line^"-m "^(string_of_int(((!tile_index) mod 5)+1+20))^" -q 0.3 "^file_name^" "
				else script_line := !script_line^"-m "^(string_of_int(((!tile_index) mod 5)+1))^" -q 0.7 "^file_name^" "
			;
			(* Increment the file index *)
			file_index := !file_index + 1;
		in

		(* For all returned_constraint *)
		List.iter (function
			| Convex_constraint k -> tile_index := !tile_index + 1; create_file_for_constraint k
			| Union_of_constraints list_of_k ->
				List.iter (fun k -> tile_index := !tile_index + 1; create_file_for_constraint k) list_of_k
		) new_returned_constraint_list;

		
		(* File in which the cartography will be printed *)
		let final_name = cartography_name^".ps" in
		(* last part of the script *)	
		script_line := !script_line^" -C -m 2 -q -1 "^file_v0_name^" > "^final_name;
		(* write the script into a file *)
		output_string script !script_line;
		(* Debug output *)
		print_message Debug_standard (
			"Plot cartography projected on parameters " ^ x_name ^ ", " ^ y_name
			^ " to file '" ^ final_name ^ "'"); 
		(* execute the script *)
		let execution = Sys.command !script_line in 
		print_message Debug_high ("Result of the cartography execution: exit code "^(string_of_int execution))
	)




(**************************************************)
(* Function to interact with Dot *)
(**************************************************)
(* Create a gif or jpg graph using dot *)
let generate_graph program pi0 reachability_graph radical =
	(* Get the file names *)
	let dot_file_name = (radical ^ "." ^ dot_file_extension) in
	let states_file_name = (radical ^ "." ^ states_file_extension) in
	let gif_file_name = (radical ^ "." ^ dot_image_extension) in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Do not write if no dot AND no log *)
	if not (options#no_dot && options#no_log) then (
		
		(* Create the input file *)
		print_message Debug_medium ("Creating input file for dot...");
		let dot_program, states = Graph.dot_of_graph program pi0 reachability_graph ~fancy:options#fancy in

		if not options#no_dot then (
			(* Write dot file *)
			print_message Debug_medium ("Writing to dot file...");
			write_to_file dot_file_name dot_program;

			(* Generate gif file using dot *)
			print_message Debug_medium ("Calling dot...");
			let command_result = Sys.command (dot_command ^ " -T" ^ dot_image_extension ^ " " ^ dot_file_name ^ " -o " ^ gif_file_name ^ "") in
			print_message Debug_medium ("Result of the 'dot' command: " ^ (string_of_int command_result));
			(* Removing dot file *)
			print_message Debug_medium ("Removing dot file...");
			Sys.remove dot_file_name;
		);
			
		(* Write states file *)
		if not options#no_log then (
			print_message Debug_medium ("Writing to file for file description...");
			write_to_file states_file_name states;
		);
	)
