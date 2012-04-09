open Global
open AbstractModel

type state_index = int
type 's graph_state = Automaton.location * 's
type 'l graph_transition = state_index * 'l * state_index
type 'l path = (state_index * 'l) list * state_index
type abstract_path = abstract_label path


type ('s, 'l) t = {
	(** Highest state index *)
	mutable curr_id : state_index;
	
	(** An Array 'state_index' -> 'state' *)
	mutable states : (state_index, 's graph_state) Hashtbl.t;
	
	(** A list of initial states (indices) *)
	mutable init_states : state_index list ref; 
	
	(** A hashtable to quickly find identical states *)
	mutable hash_table : (int, state_index) Hashtbl.t;

	(** A hashtable '(state_index, action_index)' -> 'dest_state_index' *)
	mutable transitions_table : (state_index,  ('l * state_index)) Hashtbl.t;
	
	(** A predicate for testing the inclusion of constraints *)
	mutable included_in: 's -> 's -> bool
}


(** Type alias for symbolic reachabiliy graph *)
type reachability_graph = (LinearConstraint.linear_constraint, AbstractModel.action_index) t

(** Type alias for abstract reachability graph *)
type abstract_reachability_graph = (bool list, abstract_label) t



let initial_size = 100

let make guessed_nb_transitions inclusion_predicate =
	{
		curr_id = 0;
		states = Hashtbl.create initial_size;
		init_states = ref [];
		hash_table = Hashtbl.create initial_size;
		transitions_table = Hashtbl.create guessed_nb_transitions;  
		included_in = inclusion_predicate;
	}
	

(** Return the number of states in a graph *)
let nb_states graph =
	Hashtbl.length graph.states
	
(** Return the number of transitions in a graph *)
let nb_transitions graph = 
	Hashtbl.length graph.transitions_table

(** Return the state of a state_index *)
let get_state graph state_index =
	Hashtbl.find graph.states state_index 

(** Iterate over the reachable states *)
let iter f graph =
	Hashtbl.iter (fun _ s -> f s) graph.states
	
(** Fold the reachable states *)
let fold f a graph =
	Hashtbl.fold (fun _ s x -> f x s) graph.states a

(** Return all states satisfying a predicate *)
let get_states graph p =
	let states = ref [] in 
	Hashtbl.iter (fun index state -> 
		if p state then	states := index :: !states
	) graph.states;
	!states

(** Return initial states *)
let initial_states graph =
	!(graph.init_states)


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


(* The hash code only depends on the location + discrete. *)
(* Note that merging / changing constraints will not change the hash code *)								
let hash_code = Automaton.hash_code

(* perform the insertion of a new state in a graph *)
let insert_state graph hash new_state =
	let new_state_index = graph.curr_id in
	graph.curr_id <- graph.curr_id + 1;
	(* Add the state to the tables *)
	Hashtbl.add graph.states new_state_index new_state;
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
				let loc', c' = get_state graph index in
				if Automaton.location_equal loc loc' && graph.included_in c c' then raise (Found index)
			) old_states;
			(* Not found -> insert state *)
			let new_state_index = insert_state graph hash new_state in
			(* Return state_index, true *)
			new_state_index, true				
		)	with Found state_index -> (								
				state_index, false
		)
	)
	
	
(** Add an initial state to the graph *)
let add_initial_state graph new_state =
	(* add state to the graph and get the new index *)
	let index, added = add_state graph new_state in
	(* register state as initial state *)
	if not (List.mem index !(graph.init_states)) then begin
		graph.init_states := index :: !(graph.init_states)
	end;
	(index, added)

	
let get_transitions graph src =
	try (
		Hashtbl.find_all graph.transitions_table src
	)	with Not_found -> []
	
		
(** Add a transition to the graph. Transitions are unique in that this 
    function will refuse to add two transitions with the same source and
		target index and the same label. *)
let add_transition graph (src, label, trg) =
	(* check if it already exists *)
	let transitions = get_transitions graph src in
	if not (List.mem (label, trg) transitions) then
		Hashtbl.add graph.transitions_table src (label, trg)

		
(** Return the list of all constraints on the parameters associated to the states of a graph *)
let all_p_constraints graph =
	let program = Program.get_program () in
	fold (fun current_list (_, linear_constraint) ->
			let p_constraint = LinearConstraint.hide program.clocks_and_discrete linear_constraint in
			p_constraint :: current_list)	[] graph


(** Compute the intersection of all parameter constraints, DESTRUCTIVE!!! *)
let compute_k0_destructive graph =
	let program = Program.get_program () in
	let k0 = LinearConstraint.true_constraint () in
	iter (fun (_, constr) -> 
		LinearConstraint.hide_assign program.clocks_and_discrete constr;
		LinearConstraint.intersection_assign k0 [constr]
	) graph;
	k0


(** Predicate which decides if a state is bad *)
let is_bad = fun (location, _) ->
	let program = Program.get_program () in
	let bad_states = program.bad in
	List.for_all (fun (aut_index, loc_index) -> 
		loc_index = Automaton.get_location location aut_index
	) bad_states

(** Check if bad states are reachable *)
let bad_states_reachable graph=
	 exists_state is_bad graph	
	 

(** Add an inequality to all the states of the graph *)
let add_inequality_to_states graph inequality =
	let constraint_to_add = LinearConstraint.make [inequality] in
	(* For all state: *)
	iter (fun (_,c) -> LinearConstraint.intersection_assign c [constraint_to_add]) graph

		
(** Merge two states by replacing the second one by the first one, in the whole graph structure (lists of states, and transitions) *)
let merge_states graph s mergers =
	print_message Debug_total ("update tables for " ^ (string_of_int s));
	
	(* rebuild transitions table *)
	print_message Debug_total "update transition table";
	let t' = Hashtbl.copy graph.transitions_table in
	Hashtbl.clear graph.transitions_table;
	Hashtbl.iter (fun src (a, trg) -> 
		let src' = if (List.mem src mergers) then s else src 
		and trg' = if (List.mem trg mergers) then s else trg in
		add_transition graph (src', a, trg')
	) t';

	(* Remove mergers from hash table *)
	print_message Debug_high "update hash table";
	let l, _ = get_state graph s in
	let h = hash_code l in
	(* Get all states with that hash *)
	print_message Debug_high ("get states with hash " ^ (string_of_int h));
	let bucket = Hashtbl.find_all graph.hash_table h in
	(* Remove them all *)
	while Hashtbl.mem graph.hash_table h do
		Hashtbl.remove graph.hash_table h;
	done;
	(* Add them back *)
	List.iter (fun y ->
		(* Only add if not state2 *)
		if not (List.mem y mergers) then Hashtbl.add graph.hash_table h y;
	) bucket;
	
	(* Remove mergers from state table *)
	print_message Debug_high "update state table";
	List.iter (fun s -> 
		print_message Debug_high ("remove state " ^ (string_of_int s));
(*		while Hashtbl.mem graph.states s do *)
			Hashtbl.remove graph.states s
(*		done*)
	) mergers
	


(* get states sharing the same location and discrete values from hash_table, excluding s *)
let get_siblings graph si =
	let s = get_state graph si in
	let l, _ = s in
	let h = hash_code l in
	let sibs = Hashtbl.find_all graph.hash_table h in
	(* check for exact correspondence (=> hash collisions!), and exclude si *)
	List.fold_left (fun siblings sj ->
		if sj = si then siblings else begin 
			let l', c' = get_state graph sj in
			if (Automaton.location_equal l l') then
				(sj, (l',c')) :: siblings
			else
				siblings
		end
	) [] sibs
	
	
(* Try to merge new states with existing ones. Returns updated list of new states *)
let merge graph new_states =
	let mergeable = LinearConstraint.hull_assign_if_exact in
	
	(* function for merging one state with its siblings *)
	let merge_state si =
		print_message Debug_total ("try to merge state " ^ (string_of_int si));
		let l, c = get_state graph si in
		(* get merge candidates as pairs (index, state) *)
		let candidates = get_siblings graph si in
		(* try to merge with siblings, restart if merge found, return eaten states *)
		let rec eat all_mc rest_mc = begin
			match rest_mc with
				| [] -> [] (* here, we are really done *)
				| m :: tail_mc -> begin
						let sj, (_, c') = m in
					  if mergeable c c' then begin
							print_message Debug_high ("merged with state " ^ (string_of_int sj));
							(* we ate sj, start over with new bigger state, removing sj *)
							let all_mc' = List.filter (fun (sk, _) -> sk <> sj) all_mc in
							sj :: eat all_mc' all_mc'  					
						end else begin
							(* try to eat the rest of them *)
							eat all_mc tail_mc
						end
					end
		end in
		eat candidates candidates in
	
	(* iterate list of new states and try to merge them, return eaten states *)
	let rec main_merger states =
		match states with
			| [] -> []
			| s :: ss -> begin
					let eaten = merge_state s in
					if eaten = [] then
						(* nothing merged -> go on with the rest *)
						main_merger ss
					else begin
						(* update transitions and state table *)
						merge_states graph s eaten;
						(* go on, skipping eaten states from the rest of the list *)
						eaten @ (main_merger (List.filter (fun s' -> not (List.mem s' eaten)) ss))
					end
				end in
	
	(* do it! *)
	let eaten = main_merger new_states in
	print_message Debug_standard ("  " ^ (string_of_int (List.length eaten)) ^ " states merged.");
	
	(* return non-eaten new states *)
	List.filter (fun s -> not (List.mem s eaten)) new_states 
				

let string_of_abstract_label = fun label -> 
	match label with 
		| Continuous -> "C"
		| Discrete (action_index, trans) -> (
			let program = Program.get_program () in
			let is_nosync action =
				String.length action >= 7 &&
				String.sub action 0 7 = "nosync_" in
			let action = program.action_names action_index in
			let label = if is_nosync action then (
					"D"
			) else (
				"D:" ^ action
			) in
			label
		)

let trans_compare t1 t2 = 
	if t1 = t2 then 0 else
		let l1, _ = t1 
		and l2, _ = t2 in
		let v_of_label = function
			| Continuous -> 1
			| _ -> -1 in
		(v_of_label l1) - (v_of_label l2)

	
(** Try to construct a path from an initial state to a state satisfying a predicate *)
let my_get_path graph initial_states index_predicate =
	(* Table to keep all states already visited during DFS *)
	let dfs_table = ref StateSet.empty in
	(* functional version for lookup *)
	let already_seen node = StateSet.mem node !dfs_table in
	(* function to find all successors of a state, sorting discrete transitions to the head *)
	let successors node = 
		let transitions = get_transitions graph node in
		let sorted_transitions = List.sort trans_compare transitions in
		sorted_transitions in
	(* reference to remember the actual target state *)
	let target = ref 0 in
	(* traversal function *)
	let rec dfs node was_continuous =
		(* if coming via a discrete transition, insert node in DFS table *)
		if not was_continuous then begin 
			dfs_table := StateSet.add node !dfs_table;
		end;		
		(* go on with successors *)
		let rec dfs_list transitions =			
			match transitions with
				| [] -> None
				| t :: tail -> (
					let label, succ = t in
					(* we are considering a continuous transition *)					
					let is_continuous = (label = Continuous) in
					(* delayed adding of this node, if a discrete transition was found *)
					if was_continuous && (not is_continuous) then begin
						dfs_table := StateSet.add node !dfs_table;
					end;
					if was_continuous && is_continuous then (
						(* do not consider consecutive continuous transitions *)
						dfs_list tail
					) else if index_predicate succ then (
						(* hit the target! *)
						target := succ;
						Some [(node,label)]
					) else if already_seen succ then (
						(* nothing to do here, go on with next *)
						dfs_list tail
					) else (						
						(* recursive call *)
						let my_path = dfs succ is_continuous in
						match my_path with 
							| None -> dfs_list tail
							| Some path -> Some ((node, label) :: path)
					)
				) in
		dfs_list (successors node) in
	(* Try for each initial state, keep first solution *)
	let path = List.fold_left (fun path init_state ->
		match path with
			| None -> dfs init_state false
			| Some path -> Some path
	) None initial_states in
	(* debug output *)
	if debug_mode_greater Debug_low then begin
		match path with
			| Some path -> 
					List.iter (fun (node, label) ->  
						print_message Debug_low ("s" ^ (string_of_int node) ^ 
							" --|" ^ (string_of_abstract_label label) ^ "|--> ") 
					) path;		
			| None -> ()
	end;						
	(* return path and target state index *)
	match path with
		| Some prefix -> Some (prefix, !target)
		| None -> None
	

let get_path graph initial_states index =
	my_get_path graph initial_states ((=) index)

let get_counterexample graph =
	let predicate = fun index -> is_bad (get_state graph index) in
	my_get_path graph (initial_states graph) predicate


let plot_graph x y graph = 
	fold (fun s (_, constr) -> s ^ (Graphics.plot_2d x y constr) ^ "\n") "" graph
	
let plot_abstract_graph x y = fun graph -> 
	let program = Program.get_program () in
	fold (fun s (l,b) ->
		let _,constr = PredicateAbstraction.concretize program.predicates (l,b) in 
		s ^ (Graphics.plot_2d x y constr) ^ "\n"
	) "" graph
	

let dot_colors = [
(* I ordered the first colors *)
"red" ; "green" ; "blue" ; "yellow" ; "cyan" ; "magenta" ;
(* The rest : random ! *)
"paleturquoise2"; "indianred1"; "goldenrod3"; "darkolivegreen4"; "slategray4"; "turquoise4"; "lightpink"; "salmon"; "pink3"; "chocolate4"; "lightslateblue"; "yellow3"; "   red4   "; "seashell3"; "cyan"; "darkgoldenrod3"; "gainsboro"; "yellowgreen"; "peachpuff1"; "oldlace"; "khaki"; "deepskyblue"; "maroon3"; "gold3"; "   tan   "; "mediumblue"; "lightyellow"; "ivory"; "lightcyan"; "lightsalmon4"; "maroon2"; "maroon4"; "   tan3   "; "green2"; "ivory2"; "navyblue"; "wheat1"; "navajowhite3"; "darkkhaki"; "whitesmoke"; "goldenrod"; "gold1"; "sandybrown"; "springgreen3"; "magenta2"; "lightskyblue1"; "lightcyan3"; "khaki2"; "khaki3"; "lavender"; "orchid1"; "wheat"; "lavenderblush1"; "firebrick2"; "navajowhite4"; "darkslategray3"; "palegreen2"; "lavenderblush3"; "skyblue3"; "deepskyblue3"; "darkorange"; "magenta1"; "darkorange3"; "violetred1"; "lawngreen"; "deeppink3"; "darkolivegreen1"; "darkorange1"; "darkorchid1"; "limegreen"; "lightslategray"; "deeppink"; "   red2   "; "goldenrod1"; "mediumorchid4"; "cornsilk1"; "lemonchiffon3"; "   gold   "; "orchid"; "yellow2"; "lightpink4"; "violetred2"; "mediumpurple"; "lightslategrey"; "lightsalmon1"; "violetred"; "coral2"; "slategray"; "plum2"; "turquoise3"; "lightyellow3"; "green4"; "mediumorchid1"; "lightcyan1"; "lightsalmon3"; "green3"; "lightseagreen"; "mediumpurple1"; "lightskyblue"; "lightyellow2"; "firebrick"; "honeydew2"; "slateblue3"; "navajowhite"; "seagreen1"; "springgreen4"; "   peru   "; "springgreen2"; "mediumvioletred"; "ivory4"; "olivedrab3"; "lightyellow1"; "hotpink"; "sienna4"; "lightcyan4"; "chartreuse4"; "lemonchiffon4"; "indianred3"; "hotpink4"; "sienna1"; "slategray3"; "darkseagreen2"; "tomato3"; "honeydew3"; "mistyrose2"; "rosybrown1"; "pink2"; "powderblue"; "cornflowerblue"; "   tan1   "; "indianred4"; "slateblue2"; "palevioletred3"; "ivory1"; "honeydew4"; "white"; "wheat3"; "steelblue4"; "purple2"; "deeppink4"; "royalblue4"; "lightgrey"; "forestgreen"; "palegreen"; "darkorange4"; "lightsteelblue2"; "tomato4"; "royalblue1"; "hotpink1"; "hotpink3"; "palegoldenrod"; "orange3"; "yellow1"; "orange2"; "slateblue"; "lightblue"; "lavenderblush2"; "chartreuse3"; "hotpink2"; "lightblue1"; "coral1"; "orange1"; "gold2"; "lightcoral"; "mediumseagreen"; "darkgreen"; "dodgerblue1"; "khaki1"; "khaki4"; "lightblue4"; "lightyellow4"; "firebrick3"; "crimson"; "olivedrab2"; "mistyrose3"; "lightsteelblue4"; "mediumpurple3"; "maroon"; "purple1"; "mediumorchid3"; "lightblue3"; "snow4"; "pink4"; "lightgray"; "lightsteelblue1"; "mistyrose"; "lightgoldenrodyellow"; "slategray1"; "peachpuff4"; "lightsalmon2"; "lightgoldenrod4"; "linen"; "darkgoldenrod1"; "goldenrod4"; "   navy   "; "lightcyan2"; "darkgoldenrod"; "mediumorchid2"; "lightsalmon"; "sienna"; "lightgoldenrod"; "plum1"; "orangered4"; "mistyrose1"; "mediumorchid"; "salmon1"; "chocolate3"; "palevioletred"; "purple3"; "turquoise"; "   snow   "; "paleturquoise"; "darkolivegreen"; "deepskyblue2"; "honeydew1"; "midnightblue"; "steelblue2"; "darkturquoise"; "dimgray"; "mediumpurple4"; "darkorchid"; "seashell2"; "cyan2"; "olivedrab1"; "royalblue2"; "violet"; "seagreen2"; "thistle3"; "cornsilk3"; "moccasin"; "magenta3"; "mediumslateblue"; "cadetblue3"; "mediumaquamarine"; "magenta4"; "mintcream"; "orangered3"; "mistyrose4"; "darkseagreen4"; "orangered"; "palegreen4"; "mediumspringgreen"; "saddlebrown"; "plum3"; "palegreen3"; "darkviolet"; "violetred3"; "orange"; "seagreen"; "springgreen1"; "deeppink2"; "navajowhite1"; "paleturquoise4"; "   tan4   "; "slategrey"; "lightsteelblue"; "azure3"; "salmon4"; "olivedrab4"; "darkorchid2"; "rosybrown"; "peachpuff2"; "springgreen"; "thistle2"; "   tan2   "; "aquamarine2"; "rosybrown4"; "palevioletred2"; "slateblue4"; "cyan4"; "red1"; "slateblue1"; "cornsilk2"; "ivory3"; "lightpink2"; "mediumpurple2"; "sienna2"; "chocolate1"; "lightsteelblue3"; "lightgoldenrod3"; "blueviolet"; "sienna3"; "orangered1"; "lightpink3"; "mediumturquoise"; "darkorange2"; "skyblue1"; "steelblue"; "seashell4"; "salmon2"; "lightpink1"; "skyblue4"; "darkslategray4"; "palevioletred4"; "orchid2"; "blue2"; "orchid3"; "peachpuff3"; "transparent"; "lavenderblush4"; "darkslategray1"; "lemonchiffon"; "papayawhip"; "maroon1"; "skyblue"; "chocolate"; "snow2"; "pink1"; "peachpuff"; "tomato1"; "blue1"; "dodgerblue2"; "orchid4"; "   plum   "; "orange4"; "purple"; "royalblue3"; "   pink   "; "floralwhite"; "palegreen1"; "dodgerblue4"; "chartreuse"; "bisque4"; "plum4"; "darkseagreen3"; "lightskyblue3"; "darkseagreen1"; "lightblue2"; "royalblue"; "   red3   "; "salmon3"; "palevioletred1"; "purple4"; "burlywood1"; "chocolate2"; "darkolivegreen3"; "goldenrod2"; "seashell1"; "indianred"; "brown2"; "lemonchiffon1"; "steelblue1"; "thistle1"; "yellow4"; "lightskyblue4"; "skyblue2"; "lemonchiffon2"; "thistle4"; "tomato2"; "violetred4"; "green1"; "greenyellow"; "paleturquoise1"; "chartreuse2"; "darkseagreen"; "turquoise2"; "cyan3"; "olivedrab"; "darkslategrey"; "firebrick4"; "lightgoldenrod1"; "seagreen3"; "seagreen4"; "tomato"; "firebrick1"; "steelblue3"; "orangered2"; "lavenderblush"; "cyan1"; "snow1"; "dodgerblue3"; "rosybrown2"; "indianred2"; "blanchedalmond"; "gold4"; "paleturquoise3"; "honeydew"; "bisque2"; "bisque3"; "snow3"; "brown"; "deeppink1"; "dimgrey"; "lightgoldenrod2"; "lightskyblue2"; "navajowhite2"; "seashell"; "black"; "cadetblue1"; "cadetblue2"; "darkslategray"; "wheat2"; "burlywood"; "brown1"; "deepskyblue4"; "darkslateblue"; "deepskyblue1"; "slategray2"; "darksalmon"; "burlywood3"; "dodgerblue"; "turquoise1"; "grey"; "ghostwhite"; "thistle"; "blue4"; "cornsilk"; "azure"; "darkgoldenrod2"; "darkslategray2"; "beige"; "burlywood2"; "coral3"; "indigo"; "darkorchid4"; "coral"; "burlywood4"; "brown3"; "cornsilk4"; "wheat4"; "darkgoldenrod4"; "cadetblue4"; "brown4"; "cadetblue"; "azure4"; "darkolivegreen2"; "rosybrown3"; "coral4"; "azure2"; "blue3"; "chartreuse1"; "bisque1"; "aquamarine1"; "azure1"; "bisque"; "aquamarine4"; "antiquewhite3"; "antiquewhite2"; "darkorchid3"; "antiquewhite4"; "aquamarine3"; "aquamarine"; "antiquewhite"; "antiquewhite1"; "aliceblue"
]


let concrete_state_printer =
	ModelPrinter.string_of_state 
	
let abstract_state_printer = fun state -> 
	let program = Program.get_program () in
	ModelPrinter.string_of_state (PredicateAbstraction.concretize program.predicates state)
	 
let concrete_label_printer action_index =
	let program = Program.get_program () in
	let is_nosync action =
	String.length action >= 7 &&
	String.sub action 0 7 = "nosync_" in
	let action = program.action_names action_index in
	let label = if is_nosync action then (
		";"
	) else (
		" [label=\"" ^ action ^ "\"];"
	) in
	label

let abstract_label_printer label =
	" [label=\"" ^ (string_of_abstract_label label) ^ "\"];"

let concrete_constraint_printer = fun constr -> 
	let program = Program.get_program () in
	LinearConstraint.string_of_linear_constraint program.variable_names constr
	
let abstract_constraint_printer =
	PredicateAbstraction.string_of_signature

let concrete_projection = fun constr -> 
	let program = Program.get_program () in
	LinearConstraint.hide program.clocks constr
	
let abstract_projection s = s



(* Convert a graph to a dot file *)
let make_dot_of_graph state_printer constraint_printer label_printer projection reachability_graph =
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
		^ (if program.imitator_mode = Reachability_analysis || program.imitator_mode = AbstractReachability then "\n * Reachability analysis" else (
			"\n * The following pi0 was considered:"
			^ "\n" ^ (ModelPrinter.string_of_pi0 pi0)
		))
		^ "\n * " ^ (string_of_int (Hashtbl.length states)) ^ " states and "
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
			Hashtbl.iter (fun state_index state ->
			(* Construct the string *)
			string_states := !string_states
				(* Add the state *)
				^ "\n\n\n  STATE " ^ (string_of_int state_index) ^ ":"
				^ "\n  " ^ (state_printer state)
				(* Add the constraint with no clocks (option only) *)
				^ (if program.with_parametric_log then (
					(* Get the constraint *)
					let _, linear_constraint = state in
					(* Eliminate clocks *)
					let parametric_constraint = projection linear_constraint in
					"\n\n  After clock elimination:"
					^ "\n  " ^ (constraint_printer parametric_constraint);
				) else "");
			) states;
		!string_states)
		^ "\n*/"
	in
	
	let dot_file =
		"\n\ndigraph G {"
		(* Convert the transitions *)
		^ (Hashtbl.fold (fun orig_state_index (label, dest_state_index) my_string ->			
			my_string
			^ "\n  "
			^ "s" ^ (string_of_int orig_state_index)
			^ " -> "
			^ "s" ^ (string_of_int dest_state_index)
			^ (label_printer label)
		) transitions "")

		(* Add nice colors *)
		^ "\n" ^
		(let string_colors = ref "" in
			Hashtbl.iter (fun state_index (location, _) ->
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


let dot_of_graph =
	make_dot_of_graph concrete_state_printer concrete_constraint_printer concrete_label_printer concrete_projection
	
let dot_of_abstract_graph =
	make_dot_of_graph abstract_state_printer abstract_constraint_printer abstract_label_printer abstract_projection 
 