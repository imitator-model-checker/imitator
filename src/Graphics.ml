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

(**************************************************)
(* Functions *)
(**************************************************)

let hello_world _ = print_message Debug_standard "Hello world!"

(* transform a strict inequality into a not strict inequality *)
let strict_to_not_strict_inequality inequality =
	match inequality with
		|Less_Than (x,y) -> Less_Or_Equal (x,y)
		|Greater_Than (x,y) -> Greater_Or_Equal (x,y)
		|_ -> inequality


(* print the cartography which correspond to the list of constraint *)
let cartography constraint_list pi0cube nb_variables_projected cartography_name =
	(* replace strict inequalities *)
	let new_constraint_list = ref [] in 
	for i=0 to List.length constraint_list -1 do
		let inequality_list = from_ppl_linear_constraint_list (ppl_Polyhedron_get_constraints (List.nth constraint_list i)) in 
		let new_inequality_list = ref [] in
		for j=0 to List.length inequality_list -1 do 
			new_inequality_list := (strict_to_not_strict_inequality (List.nth inequality_list j))::!new_inequality_list;
		done;
		new_constraint_list := make !new_inequality_list::!new_constraint_list;
	done;

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
		print_message Debug_standard ("Indices des variables projetees : "^(string_of_int (fst (List.nth !couple_list k)))^" et "^(string_of_int (snd (List.nth !couple_list k)))^"\n");
	done;

	(* make a cartography for each element of the couple_list *)
	for k=0 to List.length !couple_list -1 do
		(* Create a script that will print the cartography *)
		let script_name = cartography_name^"_"^(string_of_int k)^".sh" in
		let script = open_out script_name in
		(* Beginning of the script *)
		let script_line = ref "graph -T ps -C "in
		(* find the maximum abscissa and ordinate for each constraint and store them in a list *)
		let max_abs_list = ref [] in
		let max_ord_list = ref [] in
		for i=0 to List.length !new_constraint_list-1 do
			let max_abs = ref 0. in
			let max_ord = ref 0. in
			let (points,ray) = shape_of_poly (fst (List.nth !couple_list k)) (snd (List.nth !couple_list k)) (from_ppl_polyhedron (List.nth !new_constraint_list i)) in
			for j=0 to List.length points -1 do
				if !max_abs < fst (List.nth points j) then max_abs := fst (List.nth points j);
				if !max_ord < snd (List.nth points j) then max_ord := snd (List.nth points j);
			done;
			max_abs_list := !max_abs :: !max_abs_list;
			max_ord_list := !max_ord :: !max_ord_list;
		done;
		(* find the maximum abscissa and ordinate of the lists *)
		let max_abs = ref 0. in
		for i=0 to List.length !max_abs_list -1 do
			if !max_abs < List.nth !max_abs_list i then max_abs := List.nth !max_abs_list i;
		done;
		let max_ord = ref 0. in
		for i=0 to List.length !max_ord_list -1 do
			if !max_ord < List.nth !max_ord_list i then max_ord := List.nth !max_ord_list i;
		done;
		(* Create a new file for each constraint *)
		for i=0 to List.length !new_constraint_list-1 do
			let file_name = cartography_name^"_points_"^(string_of_int k)^"_"^(string_of_int i)^".txt" in
			let file_out = open_out file_name in
			(* find the points satisfying the constraint *)
			let s=plot_2d (fst (List.nth !couple_list k)) (snd (List.nth !couple_list k)) (from_ppl_polyhedron (List.nth !new_constraint_list i)) !max_abs !max_ord in
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
			(* instructions to have the zones colored *)
			if fst s = "p" 
				then script_line := !script_line^"-m "^(string_of_int((i mod 5)+1))^" -q 0.7 "^file_name^" "
				else script_line := !script_line^"-m "^(string_of_int((i mod 5)+1+20))^" -q 0.3 "^file_name^" "
		done;
		
		(* File in which the cartography will be printed *)
		let final_name = cartography_name^"_"^(string_of_int k)^".ps" in
		(* last part of the script *)	
		script_line := !script_line^"> "^final_name;
		(* write the script into a file *)
		output_string script !script_line;
		(* execute the script *)
		let execution = Sys.command !script_line in 
		print_message Debug_standard ("Result of the cartography execution: exit code "^(string_of_int execution))
	done;
