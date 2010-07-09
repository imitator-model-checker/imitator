(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2010/07/05 
 *
 ****************************************************************)

(**************************************************)
(* Modules *)
(**************************************************)

open Global
open LinearConstraint

(**************************************************)
(* Functions *)
(**************************************************)

let hello_world _ = print_message Debug_standard "Hello world!"

(* print the cartography which correspond to the list of constraint *)
let cartography constraint_list pi0cube nb_variables_projected cartography_name =
	(* Find indexes of the projected variables *)
	let idx = DynArray.create () in 
	let i = ref 0 in
	let nb = ref 0 in
	while !nb < nb_variables_projected && !i < (Array.length pi0cube) do
		match pi0cube.(!i) with 
			|(x,y) when x<>y -> (DynArray.insert idx !nb !i; nb := !nb + 1; i := !i + 1)
			|_-> i := !i + 1
	done;

	(* create an Array with all couple possible *)
	let couple_list = ref [] in
	for i=0 to DynArray.length idx -1 do
		for j=i to DynArray.length idx -1 do
			couple_list := (DynArray.get idx i,DynArray.get idx j)::!couple_list;
		done
	done;

	(* Keep only couple with diffrent values *)
	couple_list := List.filter ( fun (a,b) -> a<>b) !couple_list;
	for k=0 to List.length !couple_list -1 do 
		print_message Debug_standard ((string_of_int (fst (List.nth !couple_list k)))^" et "^(string_of_int (snd (List.nth !couple_list k)))^"\n");
	done;

	(* make a cartography for each element of the couple_list *)
	for k=0 to List.length !couple_list -1 do
		(* Create a script that will print the cartography *)
		let script_name = cartography_name^"_cart"^(string_of_int k)^".sh" in
		let script = open_out script_name in
		(* Beginning of the script *)
		let script_line = ref "graph -T ps -C "in
		(* Create a new file for each constraint *)
		for i=0 to List.length constraint_list-1 do
			let file_name = cartography_name^"_file_constraint_"^(string_of_int k)^"_"^(string_of_int i)^".txt" in
			let file_out = open_out file_name in
			(* find the points satisfying the constraint *)
			let s = plot_2d (fst (List.nth !couple_list k)) (snd (List.nth !couple_list k)) (List.nth constraint_list i) in
			(* print in the file the coordinates of the points *)
			output_string file_out s;
			(* close the file and open it in a reading mode *)
			close_out file_out;
			let file_in = open_in file_name in
			(* read the first line of the file *)
			let s2 = input_line file_in in
			(* close the file and open it in a writting mode *)
			let file_out_bis = open_out file_name in
			(* copy the whole string in it, to ensure that the polygon is closed *)
			output_string file_out_bis (s^s2);
			close_out file_out_bis;
			(* instructions to have the zones colored *)
			script_line := !script_line^"-m "^(string_of_int((i mod 5)+1))^" -q 0.5 "^file_name^" "
		done;
		
		(* File in which the cartography will be printed *)
		let final_name = cartography_name^"_"^(string_of_int k)^".ps" in
		let final_file = open_out final_name in
		(* last part of the script *)	
		script_line := !script_line^"> "^final_name;
		(* write the script into a file *)
		output_string script !script_line;
		(* execute the script *)
		let execution = Sys.command !script_line in 
		print_message Debug_standard ("Result of the cartography execution: exit code "^(string_of_int execution))
	done;
