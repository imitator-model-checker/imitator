(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2009/12/08
 * Last modified: 2012/06/15
 *
 ****************************************************************)

module Ppl = Ppl_ocaml
open Ppl
open Global
open AbstractModel


(****************************************************************)
(** Reachable states *)
(****************************************************************)
type state_index = int

(** Unique identifier for each different global location *)
type location_index = int

(** State: location and constraint *)
type state = Automaton.global_location * LinearConstraint.linear_constraint

type abstract_state = location_index * LinearConstraint.linear_constraint

(****************************************************************)
(** Graph structure *)
(****************************************************************)
type reachability_graph = {
	(** An Array 'state_index' -> 'abstract_state'; contains ALL states *)
	all_states : (state_index, abstract_state) Hashtbl.t;
	
	(** A hashtable location -> location_index *)
	index_of_locations : (Automaton.global_location, location_index) Hashtbl.t;

	(** A DynArray location_index -> location *)
	locations : Automaton.global_location DynArray.t;

	(** A hashtable to quickly find states with identical locations (? ; made by Ulrich); only for states to be compared *)
	states_for_comparison : (int, state_index) Hashtbl.t;

	(** A hashtable '(state_index, action_index)' -> 'dest_state_index' *)
	transitions_table : ((state_index * action_index), state_index) Hashtbl.t;

	(** An integer that remembers the next index of state_index (may not be equal to the number of states, if states are removed *)
	next_state_index : state_index ref;
}


(****************************************************************)
(** Constant *)
(****************************************************************)
(** Initial size of the array of states (will be updated automatically *)
let initial_size = 100


(* Debug *)
let nb_state_comparisons = ref 0
let nb_constraint_comparisons = ref 0


(****************************************************************)
(** Graph creation *)
(****************************************************************)

(** Create a fresh graph *)
let make guessed_nb_transitions = 
	(* Create a Hashtbl : state_index -> (location_index, linear_constraint) for the reachable states *)
	let states = Hashtbl.create initial_size in
	(* Create a hashtable : location -> location_index for the locations *)
	let index_of_locations = Hashtbl.create initial_size in
	(* Create a DynArray : location_index -> location for the locations *)
	let locations = DynArray.make initial_size in
	(* Create an empty lookup table : hash -> state_index *)
	let states_for_comparison = Hashtbl.create initial_size in
	(* Create a hashtable for the graph *)
	let transitions_table = Hashtbl.create guessed_nb_transitions in
	
	(* Create the graph *)
	{
		all_states = states;
		index_of_locations = index_of_locations;
		locations = locations;
		states_for_comparison = states_for_comparison;
		transitions_table = transitions_table;
		next_state_index = ref 0;
	}


(****************************************************************)
(** Interrogation on a graph *)
(****************************************************************)

(** Return the number of states in a graph *)
let nb_states graph =
	Hashtbl.length graph.all_states


(* Return the global_location corresponding to a location_index *)
let get_location graph location_index =
	DynArray.get graph.locations location_index


(** Return the state of a state_index *)
let get_state graph state_index =
	(* Find the couple (location_index, constraint) *)
	let location_index, linear_constraint =
		(* Exception just in case *)
		try (
			Hashtbl.find graph.all_states state_index
		) with Not_found -> raise (InternalError ("State of index '" ^ (string_of_int state_index) ^ "' was not found in graph (function: get_state)."))
	in
	(* Find the location *)
	let global_location = get_location graph location_index in
	(* Return the state *)
	(global_location, linear_constraint)


(** Return the list of all constraints on the parameters associated to the states of a graph *)
let all_p_constraints program graph =
	Hashtbl.fold
		(fun _ (_, linear_constraint) current_list ->
			let p_constraint = LinearConstraint.hide program.clocks_and_discrete linear_constraint in
			p_constraint :: current_list)
		graph.all_states []

	
(** Iterate over the reachable states *)
let iterate_on_states f graph =
	Hashtbl.iter f graph.all_states


(** Compute the intersection of all parameter constraints, DESTRUCTIVE!!! *)
(** HERE PROBLEM IF ONE WANTS TO COMPUTE THE states FILE AFTER **)
let compute_k0_destructive program graph =
	let k0 = LinearConstraint.true_constraint () in
	iterate_on_states (fun _ (_, constr) -> 
		LinearConstraint.hide_assign program.clocks_and_discrete constr;
		LinearConstraint.intersection_assign k0 [constr];

	) graph;
	k0


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
let last_states program graph =
	(* list to keep the resulting last states *)
	let last_states = ref [] in
	(* Table to keep all states already visited during DFS *)
	let dfs_table = ref StateSet.empty in
	(* functional version for lookup *)
	let already_seen node = StateSet.mem node !dfs_table in
	(* function to find all successors of a state *)
	let successors node = 
		List.fold_left (fun succs action_index -> 
			try (
				let succ = Hashtbl.find_all graph.transitions_table (node, action_index) in
				List.rev_append succ succs 
			) with Not_found -> succs
		) [] program.actions in
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


(*exception Satisfied

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
		) with Satisfied -> false	*)	



(*(** Check if bad states are reachable *)
let is_bad program graph =
	(* get bad state pairs from program *)
	let bad_states = program.bad in
	(* if no bad state specified, then must be good *)
	if bad_states = [] then false else (
		let is_bad_state = fun (location, _) -> 
			List.for_all (fun (aut_index, loc_index) -> 
				loc_index = Automaton.get_location location aut_index
			) bad_states in
		exists_state is_bad_state graph
	) *)


(****************************************************************)
(** Actions on a graph *)
(****************************************************************)
exception Found of state_index

(** compute a hash code for a state, depending only on the location *)
let hash_code (location, _) =
	Automaton.hash_code location


(** Check if two states are equal *)
let states_equal state1 state2 =
	let (loc1, constr1) = state1 in
	let (loc2, constr2) = state2 in
	if not (Automaton.location_equal loc1 loc2) then false else (
		(* Statistics *)
		print_message Debug_high ("About to compare equality between two constraints.");
		nb_constraint_comparisons := !nb_constraint_comparisons + 1;
		print_message Debug_high ("Already performed " ^ (string_of_int (!nb_constraint_comparisons)) ^ " constraint comparisons.");
		LinearConstraint.is_equal constr1 constr2
	)
	
(* Check dynamically if two states are equal*)
let states_equal_dyn state1 state2 constr =
	let (loc1, constr1) = state1 in
	let (loc2, constr2) = state2 in
	if not (Automaton.location_equal loc1 loc2) then false else (
		(* Statistics *)
		print_message Debug_high ("About to compare (dynamic) equality between two constraints.");
		nb_constraint_comparisons := !nb_constraint_comparisons + 1;
		print_message Debug_high ("Already performed " ^ (string_of_int (!nb_constraint_comparisons)) ^ " constraint comparisons.");
		(* WARNING!!! Really sure that one wants do MODIFY the constraints here?!!! *)
		LinearConstraint.intersection_assign constr1  [constr];
		LinearConstraint.intersection_assign constr2 [constr];
		LinearConstraint.is_equal constr1 constr2
	)


(** Check if a state is included in another one *)
(* (Despite the test based on the hash table, this is still necessary in case of hash collisions) *)
let state_included state1 state2 =
	let (loc1, constr1) = state1 in
	let (loc2, constr2) = state2 in
	if not (Automaton.location_equal loc1 loc2) then false else (
		(* Statistics *)
		print_message Debug_high ("About to compare inclusion between two constraints.");
		nb_constraint_comparisons := !nb_constraint_comparisons + 1;
		print_message Debug_high ("Already performed " ^ (string_of_int (!nb_constraint_comparisons)) ^ " constraint comparisons.");
		LinearConstraint.is_leq constr1 constr2
	)


(** Perform the insertion of a new state in a graph *)
let insert_state graph hash new_state =
	(* Compute the new state index *)
	let new_state_index = !(graph.next_state_index) in
	(* Retrieve the location and the constraint *)
	let location, linear_constraint = new_state in
	(* Try to find the location index *)
	let location_index = try (
		Hashtbl.find graph.index_of_locations location
	) with Not_found -> (
	(* If not found: add it *)
		(* Find new index *)
		let new_index = Hashtbl.length graph.index_of_locations in
		(* Add to hash table *)
		Hashtbl.add graph.index_of_locations location new_index;
		(* Add to Dyn Array *)
		DynArray.add graph.locations location;
		(* Check length (COULD BE REMOVED) *)
		if DynArray.length graph.locations != Hashtbl.length graph.index_of_locations then(
			raise (InternalError "Locations and index_of_locations seem not to be consistent anymore.");
		);
		(* Return new index *)
		new_index;
	) in
	(* Add the state to the tables *)
	Hashtbl.add graph.all_states new_state_index (location_index, linear_constraint);
	Hashtbl.add graph.states_for_comparison hash new_state_index;
	(* Update next state index *)
	graph.next_state_index := !(graph.next_state_index) + 1;
	(* Return state_index *)
	new_state_index


(**** TO DO: merge with add_state !!! *)
(** Add a state to a graph, if it is not present yet with the on-the-fly intersection *)
let add_state_dyn program graph new_state constr =
	(* compute hash value for the new state *)
	let hash = hash_code new_state in
	if debug_mode_greater Debug_total then (
		print_message Debug_standard ("hash : " ^ (string_of_int hash));
	); 
	(* In tree mode: does not test anything *)
	if program.options#tree then (
		(* Since the state does NOT belong to the graph: find the state index *)
		let new_state_index = insert_state graph hash new_state in
		(* Return state_index, true *)
		new_state_index, true
	) else (
		(* The check used for equality *)
		let check_states = states_equal_dyn in
		try (
			(* use hash table to find states with same locations (modulo hash collisions) *)
			let old_states = Hashtbl.find_all graph.states_for_comparison hash in
			if debug_mode_greater Debug_total then (
				let nb_old = List.length old_states in
				print_message Debug_total ("hashed list of length " ^ (string_of_int nb_old));
			);
			
			(* Statistics *)
			print_message Debug_medium ("About to compare new state with " ^ (string_of_int (List.length old_states)) ^ " state(s).");
			nb_state_comparisons := !nb_state_comparisons + (List.length old_states);
			print_message Debug_medium ("Already performed " ^ (string_of_int (!nb_state_comparisons)) ^ " comparisons.");
			
			List.iter (fun index -> 
				let state = get_state graph index in
				if check_states new_state state constr then raise (Found index)
			) old_states;
			(* Not found -> insert state *)
			let new_state_index = insert_state graph hash new_state in
			(* Return state_index, true *)
			new_state_index, true				
		)	with Found state_index -> (
				state_index, false
		)
	)


(** Add a state to a graph, if it is not present yet *)
let add_state program graph new_state =
	(* compute hash value for the new state *)
	let hash = hash_code new_state in
	if debug_mode_greater Debug_total then (
		print_message Debug_standard ("hash : " ^ (string_of_int hash));
	); 
	(* In tree mode: does not test anything *)
	if program.options#tree then (
		(* Since the state does NOT belong to the graph: find the state index *)
		let new_state_index = insert_state graph hash new_state in
		(* Return state_index, true *)
		new_state_index, true
	) else (
		(* The check used for equality *)
		let check_states = if program.options#inclusion then state_included else states_equal in				
		try (
			(* use hash table to find all states with same locations (modulo hash collisions) *)
			let old_states = Hashtbl.find_all graph.states_for_comparison hash in
			if debug_mode_greater Debug_total then (
				let nb_old = List.length old_states in
				print_message Debug_total ("hashed list of length " ^ (string_of_int nb_old));
			);

			(* Statistics *)
			print_message Debug_medium ("About to compare new state with " ^ (string_of_int (List.length old_states)) ^ " state(s).");
			nb_state_comparisons := !nb_state_comparisons + (List.length old_states);
			print_message Debug_medium ("Already performed " ^ (string_of_int (!nb_state_comparisons)) ^ " comparisons.");
			
			List.iter (fun index -> 
				let state = get_state graph index in
				if check_states new_state state then raise (Found index)
			) old_states;
			(* Not found -> insert state *)
			let new_state_index = insert_state graph hash new_state in
			(* Return state_index, true *)
			new_state_index, true
		)	with Found state_index -> (
				state_index, false
		)
	)
			

			
let get_transitions graph orig_state_index action_index =
	try (
		Hashtbl.find_all graph.transitions_table (orig_state_index, action_index)
	)	with Not_found -> []


(** Add a transition to the graph. Transitions are unique in that this 
    function will refuse to add two transitions with the same source and
		target index and the same label. *)
let add_transition graph (orig_state_index, action_index, dest_state_index) =
	(* check if it already exists *)
	let transitions = get_transitions graph orig_state_index action_index in
	if not (List.mem dest_state_index transitions) then
		Hashtbl.add graph.transitions_table (orig_state_index, action_index) dest_state_index


(*		
(** Add a transition to the graph *)
let add_transition reachability_graph (orig_state_index, action_index, dest_state_index) =
	Hashtbl.add reachability_graph.transitions_table (orig_state_index, action_index) dest_state_index*)


(** Add an inequality to all the states of the graph *)
let add_inequality_to_states graph inequality =
	let constraint_to_add = LinearConstraint.make [inequality] in
	(* For all state: *)
	iterate_on_states (fun _ (_, constr) ->
		 LinearConstraint.intersection_assign constr [constraint_to_add]
	) graph


(*
	CURRENTLY USELESS
(** Replace the constraint of a state in a graph by another one (the constraint is copied to avoid side-effects later) *)
let replace_constraint graph linear_constraint state_index =
	(* Copy to avoid side-effects *)
	let linear_constraint_copy = LinearConstraint.copy linear_constraint in
	try (
		(* Get the location index *)
		let location_index, _ = Hashtbl.find graph.all_states state_index in
		(* Replace with the new constraint *)
		Hashtbl.replace graph.all_states state_index (location_index, linear_constraint_copy);
	) with Not_found -> raise (InternalError ("Error when handling state '" ^ (string_of_int state_index) ^ "' in Graph:replace_constraint."))*)


(** Merge two states by replacing the second one with the first one, in the whole graph structure (lists of states, and transitions) *)
let merge_2_states graph state_index1 state_index2 =
	(* Retrieve state2 (for hash later *)
	let location_index2, constr2 = Hashtbl.find graph.all_states state_index2 in
	let state2 = get_location graph location_index2, constr2 in
	
	(*-------------------------------------------------------------*)
	(* Replace s2 with s1 in transition table *)
	(*-------------------------------------------------------------*)
	(* First copy the table (MEMORY CONSUMING! but necessary in order to avoid unexpected behaviors) *)
	let transitions_copy = Hashtbl.copy graph.transitions_table in
	Hashtbl.iter (fun (orig_state_index, action_index) dest_state_index ->
		(* Replace if source *)
		if orig_state_index = state_index2 then (
			(* Replace dest if needed *)
			let new_dest_state_index = if dest_state_index = state_index2 then state_index1 else dest_state_index in
			Hashtbl.remove graph.transitions_table (orig_state_index, action_index);
			if Hashtbl.mem graph.transitions_table (state_index1, action_index) && (Hashtbl.find graph.transitions_table (state_index1, action_index)) = new_dest_state_index then (
				
				(* TO RECHECK (exception disabled by Etienne, 16/03/2012 *)

				
				(* Unexpected case *)
(* 				raise (InternalError ("Error when merging states: a couple '(orig_state_index, action_index)' is already bound in the transitions table.")); *)
			)else
				Hashtbl.add graph.transitions_table (state_index1, action_index) new_dest_state_index;
		)
		(* Replace if destination *)
		else (if dest_state_index = state_index2 then (
			Hashtbl.remove graph.transitions_table (orig_state_index, action_index);
			if Hashtbl.mem graph.transitions_table (orig_state_index, action_index)  && (Hashtbl.find graph.transitions_table (orig_state_index, action_index)) = state_index1 then (

				(* TO RECHECK (exception disabled by Etienne, 16/03/2012 *)
			
				(* Unexpected case *)
(* 				raise (InternalError ("Error when merging states: a couple '(orig_state_index, action_index)' is already bound in the transitions table.")); *)
			)else
				Hashtbl.add graph.transitions_table (orig_state_index, action_index) state_index1;
		);
		(* Else do nothing *)
		)
	) transitions_copy;
	
	(*-------------------------------------------------------------*)
	(* Remove s2 from state hashtable *)
	(*-------------------------------------------------------------*)
	Hashtbl.remove graph.all_states state_index2;
	
	(*-------------------------------------------------------------*)
	(* Replace s2 with s1 in states_for_comparison *)
	(*-------------------------------------------------------------*)
	(* Find the hash *)
	let hash2 = hash_code state2 in
	(* Get all states with that hash *)
	let all_states_with_hash2 = Hashtbl.find_all graph.states_for_comparison hash2 in
	(* Remove them all *)
	while Hashtbl.mem graph.states_for_comparison hash2 do
		Hashtbl.remove graph.states_for_comparison hash2;
	done;
	(* Add them back *)
	List.iter (fun state_index ->
		(* Only add if not state2 *)
		if state_index != state_index2 then Hashtbl.add graph.states_for_comparison hash2 state_index;
	) all_states_with_hash2;
	
	
	(* TO DO: remove merged state from states_for_comparison *)
	
	
(*	(* First copy the table (MEMORY CONSUMING! but necessary in order to avoid unexpected behaviors) *)
	let states_for_comparison_copy = Hashtbl.copy graph.states_for_comparison in
	(* Empty the original hashtable *)
	Hashtbl.clear graph.states_for_comparison;
	(* Fill it again *)
	Hashtbl.iter (fun hash state_index ->
		(* Replace the state if needed *)
		let new_state_index = if state_index = state_index2 then state_index1 else state_index in
		(* Add it *)
		Hashtbl.add hash new_state_index;
	) states_for_comparison_copy; *)
	()



(** Merge two states by replacing the second one by the first one, in the whole graph structure (lists of states, and transitions) *)
let merge_states_ulrich graph s merged =
	(* NOTE: 'merged' is usually very small, e.g., 1 or 2, so no need to optimize functions using 'merged *) 
	print_message Debug_high ("Merging: update tables for state '" ^ (string_of_int s) ^ "' with " ^ (string_of_int (List.length merged)) ^ " merged.");
	
	(* Rebuild transitions table *)
	print_message Debug_high ("Merging: update transition table, containing " ^ (string_of_int (Hashtbl.length graph.transitions_table)) ^ " elements");
	let t' = Hashtbl.copy graph.transitions_table in
	Hashtbl.clear graph.transitions_table;
	Hashtbl.iter (fun (src, a) trg -> 
		let src' = if (List.mem src merged) then s else src 
		and trg' = if (List.mem trg merged) then s else trg in
		(* Add if not *)
		add_transition graph (src', a, trg')
	) t';

	(* Remove merged from hash table *)
	print_message Debug_high "Merging: update hash table";
	let the_state = get_state graph s in
	let h = hash_code the_state in
	(* Get all states with that hash *)
	let bucket = Hashtbl.find_all graph.states_for_comparison h in
	print_message Debug_high ("Merging: got " ^ (string_of_int (List.length bucket)) ^ " states with hash " ^ (string_of_int h));
	(* Remove them all *)
	while Hashtbl.mem graph.states_for_comparison h do
		Hashtbl.remove graph.states_for_comparison h;
	done;
	(* Add them back *)
	List.iter (fun y ->
		(* Only add if not to be merged *)
		if not (List.mem y merged) then Hashtbl.add graph.states_for_comparison h y;
	) bucket;
	
	(* Remove merged from state table *)
	print_message Debug_high "Merging: update state table";
	List.iter (fun s -> 
		print_message Debug_high ("Merging: remove state " ^ (string_of_int s));
(*		while Hashtbl.mem graph.states s do *)
			Hashtbl.remove graph.all_states s
(*		done*)
	) merged
	


(* Get states sharing the same location and discrete values from hash_table, excluding s *)
let get_siblings graph si =
	let s = get_state graph si in
	let l, _ = s in
	let h = hash_code s in
	let sibs = Hashtbl.find_all graph.states_for_comparison h in
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
	

module IntSet = Set.Make(
	struct
		type t = state_index
		let compare a b = compare a b
	end
)


(** Returns l1 minus l2, with assumption that all elements of l1 are different *)
let list_diff (l1 : int list) (l2 : int list) : int list =
(* 	print_message Debug_standard ("List diff : [" ^ (string_of_int (List.length l1)) ^ "] \ [" ^ (string_of_int (List.length l2)) ^ "]"); *)
	(* Optimize a little *)
	if l2 = [] then l1
	else (if l1 = [] then []
	else
		List.filter (fun elt -> not (List.mem elt l2)) l1
		(* NOTE: surprisingly much less efficient (some times 4 times slower!) to do the n log(n) solution below rather than the n2 solution above *)
(*		let set_of_list l =
			List.fold_left (fun set elt -> IntSet.add elt set) IntSet.empty l
		in
		(* Convert l1 *)
		let s1 = set_of_list l1 in
	(*	(* Convert l2 *)
		let s2 = set_of_list l2 in
		(* Performs set difference *)
		let set_diff = IntSet.diff s1 s2 in*)
		(* Remove elements from l2 *)
		let set_diff =
			List.fold_left (fun set elt -> IntSet.remove elt set) s1 l2
		in
		(* Return elements *)
		IntSet.elements set_diff
*)
	)



(* Try to merge new states with existing ones. Returns updated list of new states (ULRICH) *)
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
		end
		in
		eat candidates candidates
	in
	
	(* Iterate list of new states and try to merge them, return eaten states *)
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
						merge_states_ulrich graph s eaten;
(*						List.iter (fun state_to_be_eaten -> 
							merge_2_states graph s state_to_be_eaten
						) eaten;*)
						
						(* go on, skipping eaten states from the rest of the list *)
						(* Optimization: does not care of the order (otherwise use list_append *)
						List.rev_append eaten (main_merger (list_diff ss eaten))
					end
				end in
	
	(* Do it! *)
	let eaten = main_merger new_states in
	let nb_eaten = List.length eaten in
	let nb_orig = List.length new_states in
	if nb_eaten > 0 then
		print_message Debug_standard ("  " ^ (string_of_int nb_eaten) ^ " state" ^ (s_of_int nb_eaten) ^ " merged within " ^ (string_of_int nb_orig) ^ " state" ^ (s_of_int nb_orig) ^ ".");
	
	(* return non-eaten new states *)
	list_diff new_states eaten



(** Empties the hash table giving the set of states for a given location; optimization for the jobshop example, where one is not interested in comparing  a state of iteration n with states of iterations < n *)
let empty_states_for_comparison graph =
	Hashtbl.clear graph.states_for_comparison


(** Get statistics on the number of comparisons between states *)
let get_statistics () =
	(string_of_int !nb_state_comparisons) ^ " comparisons between states were performed."
	^ "\n" ^ (string_of_int !nb_constraint_comparisons) ^ " comparisons between constraints were performed."


(** Get statistics on the structure of the states: number of different locations, number of different constraints *)
let get_statistics_states graph =
	let nb_states = nb_states graph in
	(* Compute the number of constraints per location *)
	let nb_constraints_per_location_id = Hashtbl.create (DynArray.length graph.locations) in
	(* Compute the number of constraints equal to each other (list of couples (constraint, nb) )*)
	let nb_per_constraint = DynArray.make 0 in
	(* Iterate on all states *)
	iterate_on_states (fun _ (location_index, the_constraint) ->
		(* Find former nb of constraints for this location *)
		let former_nb = try
			Hashtbl.find nb_constraints_per_location_id location_index
		with Not_found -> 0 in
		(* Add +1 *)
		Hashtbl.replace nb_constraints_per_location_id location_index (former_nb + 1);
		
		(*(* Find former nb of constraints *)
		let _ =
		try(
			(* Iterate on the array *)
			DynArray.iteri (fun array_index (current_constraint, current_nb) ->
				if LinearConstraint.is_equal the_constraint current_constraint then(
					DynArray.set nb_per_constraint array_index (the_constraint, current_nb + 1);
					(* Found index *)
					raise (Found 0 (*(don't care) *));
				);
			) nb_per_constraint;
			(* If here: not found, i.e., new index *)
			DynArray.add nb_per_constraint (the_constraint, 1);
		)
		with Found _ -> ();
		in ();*)
	) graph;
	
	let nb_locations = Hashtbl.length nb_constraints_per_location_id in
	let nb_different_constraints = DynArray.length nb_per_constraint in
	let result_string = ref (
		(string_of_int nb_states) ^ " states, " ^ (string_of_int nb_locations) ^ " locations, " ^ (string_of_int nb_different_constraints) ^ " constraints"
		^ "\nNumber of constraints per locations:"
	) in
	
	(* Add number of constraints per location *)
	Hashtbl.iter (fun location nb_constraints ->
		result_string := !result_string ^ " - " ^ (string_of_int nb_constraints);
	) nb_constraints_per_location_id;
	(* Add average *)
	result_string := !result_string ^ "\nAverage: " ^ (string_of_float ((float_of_int nb_states) /. (float_of_int nb_locations)));
	(*
	(* Add number per constraint *)
	result_string := !result_string ^ "\nNumber of occurrence of constraints: ";
	DynArray.iter (fun (the_constraint , nb_constraints) ->
		result_string := !result_string ^ " - " ^ (string_of_int nb_constraints);
	) nb_per_constraint;
	(* Add average *)
	result_string := !result_string ^ "\nAverage: " ^ (string_of_float ((float_of_int nb_different_constraints) /. (float_of_int nb_states)));
	*)
	(* Return result *)
	!result_string
	
	
	
(*(** Get the number of comparisons between states (performance checking purpose) *)
let get_nb_state_comparisons () =
(*	print_message Debug_standard ("About to return the number of comparisons (" ^ (string_of_int !nb_state_comparisons) ^ ").");*)
	!nb_state_comparisons

(** Get the number of comparisons between constraints (performance checking purpose) *)
let get_nb_constraint_comparisons () =
	!nb_constraint_comparisons*)


(****************************************************************)
(** Interaction with dot *)
(****************************************************************)

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



(* Convert a graph to a dot file *)
let dot_of_graph program pi0 reachability_graph ~fancy =	
	let transitions = reachability_graph.transitions_table in
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
		^ "\n * File automatically generated by " ^ program_name ^ " " ^ version_string ^ " for model '" ^ program.options#file ^ "'"
		^ (if program.options#imitator_mode = Reachability_analysis then "\n * Reachability analysis" else (
			"\n * The following pi0 was considered:"
			^ "\n" ^ (ModelPrinter.string_of_pi0 program pi0)
		))
		^ "\n * " ^ (string_of_int (nb_states reachability_graph)) ^ " states and "
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
			iterate_on_states (fun state_index (location_index, linear_constraint) ->
			(* Get the location *)
			let global_location = get_location reachability_graph location_index in
			(* Construct the string *)
			string_states := !string_states
				(* Add the state *)
				^ "\n\n\n  STATE " ^ (string_of_int state_index) ^ ":"
				^ "\n  " ^ (ModelPrinter.string_of_state program (global_location, linear_constraint))
				(* Add the constraint with no clocks (option only) *)
				^ (if program.options#with_parametric_log then (
					(* Eliminate clocks *)
					let parametric_constraint = LinearConstraint.hide program.clocks linear_constraint in
					"\n\n  After clock elimination:"
					^ "\n  " ^ (LinearConstraint.string_of_linear_constraint program.variable_names parametric_constraint);
				) else "");
			) reachability_graph;
		!string_states)
		^ "\n*/"
	in
	
	let dot_file =
		"\n\ndigraph G {"
		(* Convert the transitions *)
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
	(* Description of the states *)
	header ^ states_description ^ dot_file
