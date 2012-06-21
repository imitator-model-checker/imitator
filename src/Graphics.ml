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
(* Plot Functions *)
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
(* Dot Functions *)
(**************************************************)

let dot_colors = [
(* I ordered the first colors *)
"red" ; "green" ; "blue" ; "yellow" ; "cyan" ; "magenta" ;
(* The rest : random ! *)
"paleturquoise2"; "indianred1"; "goldenrod3"; "darkolivegreen4"; "slategray4"; "turquoise4"; "lightpink"; "salmon"; "pink3"; "chocolate4"; "lightslateblue"; "yellow3"; "red4"; "seashell3"; "cyan2"; "darkgoldenrod3"; "gainsboro"; "yellowgreen"; "peachpuff1"; "oldlace"; "khaki"; "deepskyblue"; "maroon3"; "gold3"; "tan"; "mediumblue"; "lightyellow"; "ivory"; "lightcyan"; "lightsalmon4"; "maroon2"; "maroon4"; "tan3"; "green2"; "ivory2"; "navyblue"; "wheat1"; "navajowhite3"; "darkkhaki"; "whitesmoke"; "goldenrod"; "gold1"; "sandybrown"; "springgreen3"; "magenta2"; "lightskyblue1"; "lightcyan3"; "khaki2"; "khaki3"; "lavender"; "orchid1"; "wheat"; "lavenderblush1"; "firebrick2"; "navajowhite4"; "darkslategray3"; "palegreen2"; "lavenderblush3"; "skyblue3"; "deepskyblue3"; "darkorange"; "magenta1"; "darkorange3"; "violetred1"; "lawngreen"; "deeppink3"; "darkolivegreen1"; "darkorange1"; "darkorchid1"; "limegreen"; "lightslategray"; "deeppink"; "red2"; "goldenrod1"; "mediumorchid4"; "cornsilk1"; 
"lemonchiffon3"; "gold"; "orchid"; "yellow2"; "lightpink4"; "violetred2"; "mediumpurple"; "lightslategrey"; "lightsalmon1"; "violetred"; "coral2"; "slategray"; "plum2"; "turquoise3"; "lightyellow3"; "green4"; "mediumorchid1"; "lightcyan1"; "lightsalmon3"; "green3"; "lightseagreen"; "mediumpurple1"; "lightskyblue"; "lightyellow2"; "firebrick"; "honeydew2"; "slateblue3"; "navajowhite"; "seagreen1"; "springgreen4"; "peru"; "springgreen2"; "mediumvioletred"; "ivory4"; "olivedrab3"; "lightyellow1"; "hotpink"; "sienna4"; "lightcyan4"; "chartreuse4"; "lemonchiffon4"; "indianred3"; "hotpink4"; "sienna1"; "slategray3"; "darkseagreen2"; "tomato3"; "honeydew3"; "mistyrose2"; "rosybrown1"; "pink2"; "powderblue"; "cornflowerblue"; "tan1"; "indianred4"; "slateblue2"; "palevioletred3"; "ivory1"; "honeydew4"; "white"; "wheat3"; "steelblue4"; "purple2"; "deeppink4"; "royalblue4"; "lightgrey"; "forestgreen"; "palegreen"; "darkorange4"; "lightsteelblue2"; "tomato4"; "royalblue1"; "hotpink1"; "hotpink3";
"palegoldenrod"; "orange3"; "yellow1"; "orange2"; "slateblue"; "lightblue"; "lavenderblush2"; "chartreuse3"; "hotpink2"; "lightblue1"; "coral1"; "orange1"; "gold2"; "lightcoral"; "mediumseagreen"; "darkgreen"; "dodgerblue1"; "khaki1"; "khaki4"; "lightblue4"; "lightyellow4"; "firebrick3"; "crimson"; "olivedrab2"; "mistyrose3"; "lightsteelblue4"; "mediumpurple3"; "maroon"; "purple1"; "mediumorchid3"; "lightblue3"; "snow4"; "pink4"; "lightgray"; "lightsteelblue1"; "mistyrose"; "lightgoldenrodyellow"; "slategray1"; "peachpuff4"; "lightsalmon2"; "lightgoldenrod4"; "linen"; "darkgoldenrod1"; "goldenrod4"; "navy"; "lightcyan2"; "darkgoldenrod"; "mediumorchid2"; "lightsalmon"; "sienna"; "lightgoldenrod"; "plum1"; "orangered4"; "mistyrose1"; "mediumorchid"; "salmon1"; "chocolate3"; "palevioletred"; "purple3"; "turquoise"; "snow"; "paleturquoise"; "darkolivegreen"; "deepskyblue2"; "honeydew1"; "midnightblue"; "steelblue2"; "darkturquoise"; "dimgray"; "mediumpurple4"; "darkorchid"; "seashell2"; "cyan2";
"olivedrab1"; "royalblue2"; "violet"; "seagreen2"; "thistle3"; "cornsilk3"; "moccasin"; "magenta3"; "mediumslateblue"; "cadetblue3"; "mediumaquamarine"; "magenta4"; "mintcream"; "orangered3"; "mistyrose4"; "darkseagreen4"; "orangered"; "palegreen4"; "mediumspringgreen"; "saddlebrown"; "plum3"; "palegreen3"; "darkviolet"; "violetred3"; "orange"; "seagreen"; "springgreen1"; "deeppink2"; "navajowhite1"; "paleturquoise4"; "tan4"; "slategrey"; "lightsteelblue"; "azure3"; "salmon4"; "olivedrab4"; "darkorchid2"; "rosybrown"; "peachpuff2"; "springgreen"; "thistle2"; "tan2"; "aquamarine2"; "rosybrown4"; "palevioletred2"; "slateblue4"; "cyan4"; "red1"; "slateblue1"; "cornsilk2"; "ivory3"; "lightpink2"; "mediumpurple2"; "sienna2"; "chocolate1"; "lightsteelblue3"; "lightgoldenrod3"; "blueviolet"; "sienna3"; "orangered1"; "lightpink3"; "mediumturquoise"; "darkorange2"; "skyblue1"; "steelblue"; "seashell4"; "salmon2"; "lightpink1"; "skyblue4"; "darkslategray4"; "palevioletred4"; "orchid2"; "blue2"; "orchid3";
"peachpuff3"; "transparent"; "lavenderblush4"; "darkslategray1"; "lemonchiffon"; "papayawhip"; "maroon1"; "skyblue"; "chocolate"; "snow2"; "pink1"; "peachpuff"; "tomato1"; "blue1"; "dodgerblue2"; "orchid4"; "plum"; "orange4"; "purple"; "royalblue3"; "pink"; "floralwhite"; "palegreen1"; "dodgerblue4"; "chartreuse"; "bisque4"; "plum4"; "darkseagreen3"; "lightskyblue3"; "darkseagreen1"; "lightblue2"; "royalblue"; "red3"; "salmon3"; "palevioletred1"; "purple4"; "burlywood1"; "chocolate2"; "darkolivegreen3"; "goldenrod2"; "seashell1"; "indianred"; "brown2"; "lemonchiffon1"; "steelblue1"; "thistle1"; "yellow4"; "lightskyblue4"; "skyblue2"; "lemonchiffon2"; "thistle4"; "tomato2"; "violetred4"; "green1"; "greenyellow"; "paleturquoise1"; "chartreuse2"; "darkseagreen"; "turquoise2"; "cyan3"; "olivedrab"; "darkslategrey"; "firebrick4"; "lightgoldenrod1"; "seagreen3"; "seagreen4"; "tomato"; "firebrick1"; "steelblue3"; "orangered2"; "lavenderblush"; "cyan1"; "snow1"; "dodgerblue3"; "rosybrown2";
"indianred2"; "blanchedalmond"; "gold4"; "paleturquoise3"; "honeydew"; "bisque2"; "bisque3"; "snow3"; "brown"; "deeppink1"; "dimgrey"; "lightgoldenrod2"; "lightskyblue2"; "navajowhite2"; "seashell"; "black"; "cadetblue1"; "cadetblue2"; "darkslategray"; "wheat2"; "burlywood"; "brown1"; "deepskyblue4"; "darkslateblue"; "deepskyblue1"; "slategray2"; "darksalmon"; "burlywood3"; "dodgerblue"; "turquoise1"; "grey"; "ghostwhite"; "thistle"; "blue4"; "cornsilk"; "azure"; "darkgoldenrod2"; "darkslategray2"; "beige"; "burlywood2"; "coral3"; "indigo"; "darkorchid4"; "coral"; "burlywood4"; "brown3"; "cornsilk4"; "wheat4"; "darkgoldenrod4"; "cadetblue4"; "brown4"; "cadetblue"; "azure4"; "darkolivegreen2"; "rosybrown3"; "coral4"; "azure2"; "blue3"; "chartreuse1"; "bisque1"; "aquamarine1"; "azure1"; "bisque"; "aquamarine4"; "antiquewhite3"; "antiquewhite2"; "darkorchid3"; "antiquewhite4"; "aquamarine3"; "aquamarine"; "antiquewhite"; "antiquewhite1"; "aliceblue"
]

open Graph

(* Convert a graph to a dot file *)
let dot_of_graph program reachability_graph ~fancy =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	let transitions = get_transitions reachability_graph in
	(* Create the array of dot colors *)
	let dot_colors = Array.of_list dot_colors in
	(* Coloring function for each location *)
	let color = fun location_index ->
		(* If more colors than our array: white *)
		try dot_colors.(location_index) with Invalid_argument _ -> "white"
	in
(*	(* Array location_index -> location *)
	let locations = DynArray.create () in*)
	
	let header =
		(* Header *)
		"/***************************************************"
		^ "\n * File automatically generated by " ^ program_name ^ " " ^ version_string ^ " for model '" ^ options#file ^ "'"
		^ (if options#imitator_mode = Reachability_analysis then "\n * Reachability analysis" else (
			let pi0 = Input.get_pi0 () in
			"\n * The following pi0 was considered:"
			^ "\n" ^ (ModelPrinter.string_of_pi0 program pi0)
		))
		^ "\n * " ^ (string_of_int (nb_states reachability_graph)) ^ " states and "
			^ (string_of_int (Hashtbl.length transitions)) ^ " transitions"
		^ "\n * Program terminated " ^ (after_seconds ())
		^ "\n***************************************************/"
	in
	
	(* Retrieve the states *)
	let state_indexes = Graph.all_state_indexes program reachability_graph in
	
	(* Sort the list (for better presentation in the file) *)
	let state_indexes = List.sort (fun a b -> if a = b then 0 else if a < b then -1 else 1) state_indexes in
	
	let states_description =	
		(* Give the state indexes in comments *)
		  "\n"
		^ "\n  DESCRIPTION OF THE STATES"
		^
		(**** BAD PROG ****)
		(let string_states = ref "" in
			List.iter (fun state_index ->
			(* Retrieve location and constraint *)
			let global_location, linear_constraint = Graph.get_state reachability_graph state_index in
			(* Construct the string *)
			string_states := !string_states
				(* Add the state *)
				^ "\n\n  STATE " ^ (string_of_int state_index) ^ ":"
				^ "\n  " ^ (ModelPrinter.string_of_state program (global_location, linear_constraint))
				(* Add the constraint with no clocks (option only) *)
				^ (if options#with_parametric_log then (
					(* Eliminate clocks *)
					let parametric_constraint = LinearConstraint.hide program.clocks linear_constraint in
					"\n\n  After clock elimination:"
					^ "\n  " ^ (LinearConstraint.string_of_linear_constraint program.variable_names parametric_constraint);
				) else "");
			) state_indexes;
		!string_states)
		^ "\n"
	in
	
	
	let transitions_description =
		(* Convert the transitions for human *)
		"\n  DESCRIPTION OF THE TRANSITIONS"
		^ (Hashtbl.fold (fun (orig_state_index, action_index) dest_state_index my_string ->
			let is_nosync action =
				String.length action >= 7 &&
				String.sub action 0 7 = "nosync_" in
			let action = program.action_names action_index in
			let label = if is_nosync action then (
				""
			) else (
				" via \"" ^ action ^ "\""
			) in
			my_string
			^ "\n  "
			^ "s_" ^ (string_of_int orig_state_index)
			^ " -> "
			^ "s_" ^ (string_of_int dest_state_index)
			^ label
		) transitions "")
		^ "\n"
	in
	
	
	let dot_file =
		"\n\ndigraph G {"
		(* Convert the transitions for dot *)
		^ (Hashtbl.fold (fun (orig_state_index, action_index) dest_state_index my_string ->
			let is_nosync action =
				String.length action >= 7 &&
				String.sub action 0 7 = "nosync_" in
			let action = program.action_names action_index in
			let label = if is_nosync action then (
				";"
			) else (
				" [label=\"" ^ action ^ "\"];"
			) in
			my_string
			^ "\n  "
			^ "s_" ^ (string_of_int orig_state_index)
			^ " -> "
			^ "s_" ^ (string_of_int dest_state_index)
			^ label
		) transitions "")

	(*	(* Add a nice color *)
		^ "\n\n  q_0 [color=red, style=filled];"
		^ "\n}"*)
		(* Add nice colors *)
		^ "\n/*Colors*/\n" ^
		(**** BAD PROG ****)
		(let string_colors = ref "" in
			iterate_on_states (fun state_index (location_index, _) ->
(*			(* Find the location index *)
			let location_index = try
				(**** BAD PROG: should be hashed ****)
				(* If the location index exists: return it *)
				DynArray.index_of (fun some_location -> some_location = location) locations
				(* Else add the location *)
				with Not_found -> (DynArray.add locations location; DynArray.length locations - 1)
			in*)
			(* Find the location color *)
			let location_color = color location_index in
			(* create node index *)
			let node_index = "s_" ^ (string_of_int state_index) in

			if fancy then (
				(* Get the location *)
				let global_location = get_location reachability_graph location_index in
				(* create record label with location names *)			
				let loc_names = List.map (fun aut_index -> 
					let loc_index = Automaton.get_location global_location aut_index in
					program.location_names aut_index loc_index
				) program.automata in
				let label = string_of_list_of_string_with_sep "|" loc_names in
				(* Create the command *)
				string_colors := !string_colors
					^ "\n  " ^ node_index
					^ "[fillcolor=" ^ location_color
					^ ", style=filled, shape=Mrecord, label=\"" 
					^ node_index ^ "|{" 
					^ label ^ "}\"];";
			) else (
				(* Create the command *)
				string_colors := !string_colors
					^ "\n  " ^ node_index
					^ " [color=" ^ location_color
					^ ", style=filled];";				
			)
			) reachability_graph;
		!string_colors)
		^ "\n}"

	in
	(* Dot file *)
	header ^ dot_file,
	(* Description of the states (for human) *)
	header ^ states_description ^ transitions_description





(* Create a gif or jpg graph using dot *)
let generate_graph program reachability_graph radical =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Do not write if no dot AND no log *)
	if options#with_dot || options#with_log then (
		(* Get the file names *)
		let dot_file_name = (radical ^ "." ^ dot_file_extension) in
		let states_file_name = (radical ^ "." ^ states_file_extension) in
		let gif_file_name = (radical ^ "." ^ dot_image_extension) in
		
		(* New line *)
		print_message Debug_standard "";
		
		(* Create the input file *)
		print_message Debug_medium ("Creating input file for dot...");
		let dot_program, states = dot_of_graph program reachability_graph ~fancy:options#fancy in

		if options#with_dot then (
			print_message Debug_standard "Generating graphical output...";
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
		if options#with_log then (
			print_message Debug_standard ("Writing to file the states description...");
			write_to_file states_file_name states;
		);
	)




(**************************************************)
(* Meta programming (could have been written in Python for example) *)
(**************************************************)


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

