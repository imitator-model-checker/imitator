open Global
open AbstractImitatorFile

type state_index = int
type 's graph_state = Automaton.location * 's
type 'l graph_transition = state_index * 'l * state_index

type ('s, 'l) t = {
	(** An Array 'state_index' -> 'state' *)
	mutable states : 's graph_state DynArray.t;
	
	(** A hashtable to quickly find identical states *)
	mutable hash_table : (int, state_index) Hashtbl.t;

	(** A hashtable '(state_index, action_index)' -> 'dest_state_index' *)
	mutable transitions_table : (state_index,  ('l * state_index)) Hashtbl.t;
	
	(** A predicate for testing the inclusion of constraints *)
	mutable included_in: 's -> 's -> bool
}

(** Type alias for symbolic reachabiliy graph *)
type reachability_graph = (LinearConstraint.linear_constraint, AbstractImitatorFile.action_index) t

(** Type alias for abstract reachability graph *)
type abstract_reachability_graph = (predicate list, abstract_label) t



let initial_size = 100

let make guessed_nb_transitions inclusion_predicate =
	{
		states = DynArray.make initial_size;
		hash_table = Hashtbl.create initial_size;
		transitions_table = Hashtbl.create guessed_nb_transitions;  
		included_in = inclusion_predicate;
	}
	

(** Return the number of states in a graph *)
let nb_states graph =
	DynArray.length graph.states
	
(** Return the number of transitions in a graph *)
let nb_transitions graph = 
	Hashtbl.length graph.transitions_table

(** Return the state of a state_index *)
let get_state graph state_index =
	DynArray.get graph.states state_index 

(** Iterate over the reachable states *)
let iter f graph =
	DynArray.iter f graph.states

(* state struct for constructing set type *)
module State = struct
	type t = int
	let compare = compare
end

(* set of states for efficient lookup *)
module StateSet = Set.Make(State)

(** find all "last" states on finite or infinite runs *)
(* Uses a depth first search on the reachability graph. The *)
(* prefix of the current DFS path is kept during the search *)
(* in order to detect cycles. *) 
let last_states graph =
	(* list to keep the resulting last states *)
	let last_states = ref [] in
	(* Table to keep all states already visited during DFS *)
	let dfs_table = ref StateSet.empty in
	(* functional version for lookup *)
	let already_seen node = StateSet.mem node !dfs_table in
	(* function to find all successors of a state *)
	let successors node = 
		let transitions = try (
			Hashtbl.find_all graph.transitions_table node
		) with Not_found -> [] in
		List.map (fun (_, dest) -> dest) transitions in 
	(* function to find all last states *)
	let rec cycle_detect node prefix =
		(* get all successors of current node *)
		let succs = successors node in
		if succs = [] then
			(* no successors -> last node on finite path *)
			last_states := node :: !last_states
		else (
			(* insert node in DFS table *)
			dfs_table := StateSet.add node !dfs_table;
			(* go on with successors *)
			List.iter (fun succ -> 
				(* successor in current path prefix (or self-cycle)? *)
				if succ = node || StateSet.mem succ prefix then
					(* found cycle *)
					last_states := succ :: !last_states
				else if not (already_seen succ) then
					(* go on recursively on newly found node *)
					cycle_detect succ (StateSet.add node prefix)					
			) succs;
		) in
	(* start cycle detection with initial state *)
	cycle_detect 0 StateSet.empty;
	(* return collected last states *)
	!last_states
		 
(** Iterate over the reachable states *)
let iter f graph =
	DynArray.iter f graph.states

exception Satisfied

(** Checks if a state exists that satisfies a given predicate *)
let exists_state p graph =
		try (
			iter (fun s -> 
				if p s then raise Satisfied
			) graph;
			false
		) with Satisfied -> true

(** Checks if all states satisfy a given predicate *)
let forall_state p graph =
		try (
			iter (fun s -> 
				if not (p s) then raise Satisfied
			) graph;
			true
		) with Satisfied -> false		

				
let hash_code = Automaton.hash_code

(* perform the insertion of a new state in a graph *)
let insert_state graph hash new_state =
	let new_state_index = DynArray.length graph.states in
	(* Add the state to the tables *)
	DynArray.add graph.states new_state;
	Hashtbl.add graph.hash_table hash new_state_index;
	(* Return state_index *)
	new_state_index

exception Found of state_index
				
(** Add a state to a graph, if it is not present yet *)
let add_state graph new_state =
	let program = Program.get_program () in
	let loc, c = new_state in
	(* compute hash value for the new state *)
	let hash = hash_code loc in
	if debug_mode_greater Debug_total then (
		print_message Debug_standard ("hash : " ^ (string_of_int hash));
	); 
	(* In acyclic mode: does not test anything *)
	if program.acyclic then (
		(* Since the state does NOT belong to the graph: find the state index *)
		let new_state_index = insert_state graph hash new_state in
		(* Return state_index, true *)
		new_state_index, true
	) else (		
		(* The check used for equality *)						
		try (
			(* use hash table to find states with same locations (modulo hash collisions) *)
			let old_states = Hashtbl.find_all graph.hash_table hash in
			if debug_mode_greater Debug_total then (
				let nb_old = List.length old_states in
				print_message Debug_standard ("hashed list of length " ^ (string_of_int nb_old));
			);
			List.iter (fun index -> 
				let _, c' = get_state graph index in
				if graph.included_in c c' then raise (Found index)
			) old_states;
			(* Not found -> insert state *)
			let new_state_index = insert_state graph hash new_state in
			(* Return state_index, true *)
			new_state_index, true				
		)	with Found state_index -> (								
				state_index, false
		)
	)
		
(** Add a transition to the graph *)
let add_transition graph (src, label, trg) =
	Hashtbl.add graph.transitions_table src (label, trg)

		
(** Return the list of all constraints on the parameters associated to the states of a graph *)
let all_p_constraints graph =
	let program = Program.get_program () in
	DynArray.fold_left
		(fun current_list (_, linear_constraint) ->
			let p_constraint = LinearConstraint.hide program.clocks_and_discrete linear_constraint in
			p_constraint :: current_list)
		[] graph.states


(** Compute the intersection of all parameter constraints, DESTRUCTIVE!!! *)
let compute_k0_destructive graph =
	let program = Program.get_program () in
	let k0 = LinearConstraint.true_constraint () in
	iter (fun (_, constr) -> 
		LinearConstraint.hide_assign program.clocks_and_discrete constr;
		LinearConstraint.intersection_assign k0 [constr]
	) graph;
	k0


(** Check if bad states are reachable *)
let is_bad graph =
	let program = Program.get_program () in
	(* get bad state pairs from program *)
	let bad_states = program.bad in
	(* if no bad state specified, then must be good *)
	if bad_states = [] then false else (
		let is_bad_state = fun (location, _) -> 
			List.for_all (fun (aut_index, loc_index) -> 
				loc_index = Automaton.get_location location aut_index
			) bad_states in
		exists_state is_bad_state graph
	)
	 

(** Add an inequality to all the states of the graph *)
let add_inequality_to_states graph inequality =
	let constraint_to_add = LinearConstraint.make [inequality] in
	(* For all state: *)
	for state_index = 0 to (DynArray.length graph.states) - 1 do
		 DynArray.set graph.states state_index (
			let (loc, const) = DynArray.get graph.states state_index in
			(* Perform the intersection *)
			loc, (LinearConstraint.intersection [constraint_to_add; const] )
		)
	done
	

let plot_graph x y graph = 
	DynArray.fold_left (
		fun s (_, constr) -> s ^ (Graphics.plot_2d x y constr) ^ "\n"
	) "" graph.states

let dot_colors = [
(* I ordered the first colors *)
"red" ; "green" ; "blue" ; "yellow" ; "cyan" ; "magenta" ;
(* The rest : random ! *)
"paleturquoise2"; "indianred1"; "goldenrod3"; "darkolivegreen4"; "slategray4"; "turquoise4"; "lightpink"; "salmon"; "pink3"; "chocolate4"; "lightslateblue"; "yellow3"; "   red4   "; "seashell3"; "cyan"; "darkgoldenrod3"; "gainsboro"; "yellowgreen"; "peachpuff1"; "oldlace"; "khaki"; "deepskyblue"; "maroon3"; "gold3"; "   tan   "; "mediumblue"; "lightyellow"; "ivory"; "lightcyan"; "lightsalmon4"; "maroon2"; "maroon4"; "   tan3   "; "green2"; "ivory2"; "navyblue"; "wheat1"; "navajowhite3"; "darkkhaki"; "whitesmoke"; "goldenrod"; "gold1"; "sandybrown"; "springgreen3"; "magenta2"; "lightskyblue1"; "lightcyan3"; "khaki2"; "khaki3"; "lavender"; "orchid1"; "wheat"; "lavenderblush1"; "firebrick2"; "navajowhite4"; "darkslategray3"; "palegreen2"; "lavenderblush3"; "skyblue3"; "deepskyblue3"; "darkorange"; "magenta1"; "darkorange3"; "violetred1"; "lawngreen"; "deeppink3"; "darkolivegreen1"; "darkorange1"; "darkorchid1"; "limegreen"; "lightslategray"; "deeppink"; "   red2   "; "goldenrod1"; "mediumorchid4"; "cornsilk1"; "lemonchiffon3"; "   gold   "; "orchid"; "yellow2"; "lightpink4"; "violetred2"; "mediumpurple"; "lightslategrey"; "lightsalmon1"; "violetred"; "coral2"; "slategray"; "plum2"; "turquoise3"; "lightyellow3"; "green4"; "mediumorchid1"; "lightcyan1"; "lightsalmon3"; "green3"; "lightseagreen"; "mediumpurple1"; "lightskyblue"; "lightyellow2"; "firebrick"; "honeydew2"; "slateblue3"; "navajowhite"; "seagreen1"; "springgreen4"; "   peru   "; "springgreen2"; "mediumvioletred"; "ivory4"; "olivedrab3"; "lightyellow1"; "hotpink"; "sienna4"; "lightcyan4"; "chartreuse4"; "lemonchiffon4"; "indianred3"; "hotpink4"; "sienna1"; "slategray3"; "darkseagreen2"; "tomato3"; "honeydew3"; "mistyrose2"; "rosybrown1"; "pink2"; "powderblue"; "cornflowerblue"; "   tan1   "; "indianred4"; "slateblue2"; "palevioletred3"; "ivory1"; "honeydew4"; "white"; "wheat3"; "steelblue4"; "purple2"; "deeppink4"; "royalblue4"; "lightgrey"; "forestgreen"; "palegreen"; "darkorange4"; "lightsteelblue2"; "tomato4"; "royalblue1"; "hotpink1"; "hotpink3"; "palegoldenrod"; "orange3"; "yellow1"; "orange2"; "slateblue"; "lightblue"; "lavenderblush2"; "chartreuse3"; "hotpink2"; "lightblue1"; "coral1"; "orange1"; "gold2"; "lightcoral"; "mediumseagreen"; "darkgreen"; "dodgerblue1"; "khaki1"; "khaki4"; "lightblue4"; "lightyellow4"; "firebrick3"; "crimson"; "olivedrab2"; "mistyrose3"; "lightsteelblue4"; "mediumpurple3"; "maroon"; "purple1"; "mediumorchid3"; "lightblue3"; "snow4"; "pink4"; "lightgray"; "lightsteelblue1"; "mistyrose"; "lightgoldenrodyellow"; "slategray1"; "peachpuff4"; "lightsalmon2"; "lightgoldenrod4"; "linen"; "darkgoldenrod1"; "goldenrod4"; "   navy   "; "lightcyan2"; "darkgoldenrod"; "mediumorchid2"; "lightsalmon"; "sienna"; "lightgoldenrod"; "plum1"; "orangered4"; "mistyrose1"; "mediumorchid"; "salmon1"; "chocolate3"; "palevioletred"; "purple3"; "turquoise"; "   snow   "; "paleturquoise"; "darkolivegreen"; "deepskyblue2"; "honeydew1"; "midnightblue"; "steelblue2"; "darkturquoise"; "dimgray"; "mediumpurple4"; "darkorchid"; "seashell2"; "cyan2"; "olivedrab1"; "royalblue2"; "violet"; "seagreen2"; "thistle3"; "cornsilk3"; "moccasin"; "magenta3"; "mediumslateblue"; "cadetblue3"; "mediumaquamarine"; "magenta4"; "mintcream"; "orangered3"; "mistyrose4"; "darkseagreen4"; "orangered"; "palegreen4"; "mediumspringgreen"; "saddlebrown"; "plum3"; "palegreen3"; "darkviolet"; "violetred3"; "orange"; "seagreen"; "springgreen1"; "deeppink2"; "navajowhite1"; "paleturquoise4"; "   tan4   "; "slategrey"; "lightsteelblue"; "azure3"; "salmon4"; "olivedrab4"; "darkorchid2"; "rosybrown"; "peachpuff2"; "springgreen"; "thistle2"; "   tan2   "; "aquamarine2"; "rosybrown4"; "palevioletred2"; "slateblue4"; "cyan4"; "red1"; "slateblue1"; "cornsilk2"; "ivory3"; "lightpink2"; "mediumpurple2"; "sienna2"; "chocolate1"; "lightsteelblue3"; "lightgoldenrod3"; "blueviolet"; "sienna3"; "orangered1"; "lightpink3"; "mediumturquoise"; "darkorange2"; "skyblue1"; "steelblue"; "seashell4"; "salmon2"; "lightpink1"; "skyblue4"; "darkslategray4"; "palevioletred4"; "orchid2"; "blue2"; "orchid3"; "peachpuff3"; "transparent"; "lavenderblush4"; "darkslategray1"; "lemonchiffon"; "papayawhip"; "maroon1"; "skyblue"; "chocolate"; "snow2"; "pink1"; "peachpuff"; "tomato1"; "blue1"; "dodgerblue2"; "orchid4"; "   plum   "; "orange4"; "purple"; "royalblue3"; "   pink   "; "floralwhite"; "palegreen1"; "dodgerblue4"; "chartreuse"; "bisque4"; "plum4"; "darkseagreen3"; "lightskyblue3"; "darkseagreen1"; "lightblue2"; "royalblue"; "   red3   "; "salmon3"; "palevioletred1"; "purple4"; "burlywood1"; "chocolate2"; "darkolivegreen3"; "goldenrod2"; "seashell1"; "indianred"; "brown2"; "lemonchiffon1"; "steelblue1"; "thistle1"; "yellow4"; "lightskyblue4"; "skyblue2"; "lemonchiffon2"; "thistle4"; "tomato2"; "violetred4"; "green1"; "greenyellow"; "paleturquoise1"; "chartreuse2"; "darkseagreen"; "turquoise2"; "cyan3"; "olivedrab"; "darkslategrey"; "firebrick4"; "lightgoldenrod1"; "seagreen3"; "seagreen4"; "tomato"; "firebrick1"; "steelblue3"; "orangered2"; "lavenderblush"; "cyan1"; "snow1"; "dodgerblue3"; "rosybrown2"; "indianred2"; "blanchedalmond"; "gold4"; "paleturquoise3"; "honeydew"; "bisque2"; "bisque3"; "snow3"; "brown"; "deeppink1"; "dimgrey"; "lightgoldenrod2"; "lightskyblue2"; "navajowhite2"; "seashell"; "black"; "cadetblue1"; "cadetblue2"; "darkslategray"; "wheat2"; "burlywood"; "brown1"; "deepskyblue4"; "darkslateblue"; "deepskyblue1"; "slategray2"; "darksalmon"; "burlywood3"; "dodgerblue"; "turquoise1"; "grey"; "ghostwhite"; "thistle"; "blue4"; "cornsilk"; "azure"; "darkgoldenrod2"; "darkslategray2"; "beige"; "burlywood2"; "coral3"; "indigo"; "darkorchid4"; "coral"; "burlywood4"; "brown3"; "cornsilk4"; "wheat4"; "darkgoldenrod4"; "cadetblue4"; "brown4"; "cadetblue"; "azure4"; "darkolivegreen2"; "rosybrown3"; "coral4"; "azure2"; "blue3"; "chartreuse1"; "bisque1"; "aquamarine1"; "azure1"; "bisque"; "aquamarine4"; "antiquewhite3"; "antiquewhite2"; "darkorchid3"; "antiquewhite4"; "aquamarine3"; "aquamarine"; "antiquewhite"; "antiquewhite1"; "aliceblue"
]


(* Convert a graph to a dot file *)
let dot_of_graph reachability_graph =
	let program = Program.get_program () in
	let pi0     = Program.get_pi0 () in
	let options = Program.get_options () in
	let fancy   = options#fancy in 
			
	let states = reachability_graph.states in
	let transitions = reachability_graph.transitions_table in
	(* Create the array of dot colors *)
	let dot_colors = Array.of_list dot_colors in
	(* Coloring function for each location *)
	let color = fun location_index ->
		(* If more colors than our array: white *)
		try dot_colors.(location_index) with Invalid_argument _ -> "white"
	in
	(* Array location_index -> location *)
	let locations = DynArray.create () in
	
	let header =
		(* Header *)
		"/***************************************************"
		^ "\n * File automatically generated for file '" ^ program.program_name ^ "'"
		^ (if program.imitator_mode = Reachability_analysis then "\n * Reachability analysis" else (
			"\n * The following pi0 was considered:"
			^ "\n" ^ (ImitatorPrinter.string_of_pi0 pi0)
		))
		^ "\n * " ^ (string_of_int (DynArray.length states)) ^ " states and "
			^ (string_of_int (Hashtbl.length transitions)) ^ " transitions"
		^ "\n * Program terminated " ^ (after_seconds ())
		^ "\n***************************************************/"
	in
	
	let states_description =	
		(* Give the state indexes in comments *)
		  "\n\n/*"
		^ "\n  DESCRIPTION OF THE STATES"
		^
		(**** BAD PROG ****)
		(let string_states = ref "" in
			DynArray.iteri (fun state_index state ->
			(* Construct the string *)
			string_states := !string_states
				(* Add the state *)
				^ "\n\n\n  STATE " ^ (string_of_int state_index) ^ ":"
				^ "\n  " ^ (ImitatorPrinter.string_of_state state)
				(* Add the constraint with no clocks (option only) *)
				^ (if program.with_parametric_log then (
					(* Get the constraint *)
					let _, linear_constraint = state in
					(* Eliminate clocks *)
					let parametric_constraint = LinearConstraint.hide program.clocks linear_constraint in
					"\n\n  After clock elimination:"
					^ "\n  " ^ (LinearConstraint.string_of_linear_constraint program.variable_names parametric_constraint);
				) else "");
			) states;
		!string_states)
		^ "\n*/"
	in
	
	let dot_file =
		"\n\ndigraph G {"
		(* Convert the transitions *)
		^ (Hashtbl.fold (fun orig_state_index (action_index, dest_state_index) my_string ->
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
			^ "s" ^ (string_of_int orig_state_index)
			^ " -> "
			^ "s" ^ (string_of_int dest_state_index)
			^ label
		) transitions "")

	(*	(* Add a nice color *)
		^ "\n\n  q_0 [color=red, style=filled];"
		^ "\n}"*)
		(* Add nice colors *)
		^ "\n" ^
		(**** BAD PROG ****)
		(let string_colors = ref "" in
			DynArray.iteri (fun state_index (location, _) ->
			(* Find the location index *)
			let location_index = try
				(**** BAD PROG: should be hashed ****)
				(* If the location index exists: return it *)
				DynArray.index_of (fun some_location -> some_location = location) locations
				(* Else add the location *)
				with Not_found -> (DynArray.add locations location; DynArray.length locations - 1)
			in
			(* Find the location color *)
			let location_color = color location_index in
			(* create node index *)
			let node_index = "s" ^ (string_of_int state_index) in

			if fancy then (
				(* create record label with location names *)			
				let loc_names = List.map (fun aut_index -> 
					let loc_index = Automaton.get_location location aut_index in
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
					^ "[color=" ^ location_color
					^ ", style=filled];";				
			)
			) states;
		!string_colors)
		^ "\n}"

	in
	(* Dot file *)
	header ^ dot_file,
	(* Description of the states *)
	header ^ states_description ^ dot_file
