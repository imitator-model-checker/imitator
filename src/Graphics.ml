(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2010/07/13 
 *
 ****************************************************************)

(**************************************************)
(* Modules *)
(**************************************************)

open Global
open LinearConstraint
module Ppl = Ppl_ocaml
open Ppl
open AbstractImitatorFile

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
let cartography program pi0cube constraint_list cartography_name =
	(* replace strict inequalities *)
	let new_constraint_list = ref [] in 
	for i=0 to List.length constraint_list -1 do
		let inequality_list = ppl_Polyhedron_get_constraints (List.nth constraint_list i) in 
		let new_inequality_list = ref [] in
		for j=0 to List.length inequality_list -1 do 
			new_inequality_list := (strict_to_not_strict_inequality (List.nth inequality_list j))::!new_inequality_list;
		done;
		new_constraint_list := make !new_inequality_list::!new_constraint_list;
	done;

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
		(* convert to float *)
		let init_min_abs = float_of_int init_min_abs in
		let init_max_abs = float_of_int init_max_abs in
		let init_min_ord = float_of_int init_min_ord in
		let init_max_ord = float_of_int init_max_ord in
		
		(* find mininma and maxima for axes *)
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
		) (init_min_abs, init_max_abs, init_min_ord, init_max_ord) !new_constraint_list in
		(* add a margin of 1 unit *)
		let min_abs = min_abs -. 1.0 in
		let max_abs = max_abs +. 1.0 in
		let min_ord = min_ord -. 1.0 in
		let max_ord = max_ord +. 1.0 in
		
		(* print_message Debug_standard ((string_of_float !min_abs)^"  "^(string_of_float !min_ord)); *)
		(* Create a new file for each constraint *)
		for i=0 to List.length !new_constraint_list-1 do
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
		done;
		
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
