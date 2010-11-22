(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/22
 * Last modified: 2010/07/22
 *
 **************************************************)

open Options
open Global
open AbstractImitatorFile
open ImitatorPrinter
open Graph

exception Unsat_exception


(* global constraint k *)
let global_k = ref (LinearConstraint.true_constraint ())

(* hash function for locations *)
let loc_hash locations =
	Array.fold_left (fun h l -> 
		7919 * h + l
	) 0 locations


(* Cache for computed invariants *)
let inv_cache = Cache.make loc_hash 200

(* Cache for flows *)
let flow_cache = Cache.make loc_hash 200


(* Print statistics for cache usage *)
let print_stats _ =
	print_message Debug_standard "invariant cache:"; 
	Cache.print_stats inv_cache;
	print_message Debug_standard "flow cache:";
	Cache.print_stats flow_cache


(*--------------------------------------------------*)
(* Compute the invariant associated to a location   *)
(*--------------------------------------------------*)
let compute_plain_invariant program location =
  (* construct invariant *)
	let invariants = List.map (fun automaton_index ->
		(* Get the current location *)
		let location_index = Automaton.get_location location automaton_index in
		(* Compute the invariant *)
		program.invariants automaton_index location_index
	) program.automata in
	(* Perform the intersection *)
	LinearConstraint.intersection invariants


(*--------------------------------------------------*)
(* Compute the invariant associated to a location,  *)
(* including renaming and time elapse. Uses cache.  *)
(*--------------------------------------------------*)
let compute_invariant program location =
	(* strip off discrete for caching scheme *)
	let locations = Automaton.get_locations location in
	(* check in cache *)
	let entry = Cache.find inv_cache locations in
	match entry with
		| Some inv -> inv
		| None -> (
			(* build plain invariant *)
			let invariant = compute_plain_invariant program location in
			(* Store in cache *)
			Cache.store inv_cache locations invariant;
			invariant
		)


(*--------------------------------------------------*)
(* Instantiate the flow for a given location        *)
(*--------------------------------------------------*)
let compute_flow program location =
	(* strip off discrete for caching scheme *)
	let locations = Automaton.get_locations location in
	(* check in cache *)
	let entry = Cache.find flow_cache locations in
	match entry with
		| Some flow -> flow
		| None -> (
	  (* build flow *)
		let init_flow = Rectangular (LinearConstraint.true_constraint ()) in
		let flow = List.fold_left (fun flow automaton_index ->  
			(* Get the current location *)
			let location_index = Automaton.get_location location automaton_index in
			(* Compute the flow *)
			let loc_flow = program.analog_flows automaton_index location_index in
			match flow with
				| Undefined -> raise (InternalError "found undefined flow")
				| Rectangular f -> (
						match loc_flow with
							| Undefined -> Rectangular f
							| Rectangular lf -> 
									LinearConstraint.intersection_assign f [lf];
									Rectangular f
							| Affine lf -> 
									LinearConstraint.intersection_assign f [lf];
									Affine f 
					)
				| Affine f -> (
						match loc_flow with
							| Undefined -> Affine f
							| Rectangular lf 
							| Affine lf -> 
									LinearConstraint.intersection_assign f [lf];
									Affine f 
					)					
		) init_flow program.automata in
		match flow with
			| Undefined -> raise (InternalError "undefined flow")
			| Affine f 
			| Rectangular f -> (
					(* Add standard flow for parameters, discrete and clocks *)
					LinearConstraint.intersection_assign f [program.standard_flow];					
					(* Store in cache *)
					Cache.store flow_cache locations flow;
					flow
				)
  )


(*--------------------------------------------------*)
(* Create a fresh constraint of the form 'D = d' for any discrete variable D with value d *)
(*--------------------------------------------------*)
let instantiate_discrete discrete_values =
	let inequalities = List.map (fun (discrete_index, discrete_value) ->
		(* Create a linear term 'D - d' *)
		let linear_term = LinearConstraint.make_linear_term
			[(NumConst.one, discrete_index)]
			(NumConst.neg discrete_value)
		in
		(* Create a linear equality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_eq
	) discrete_values in
	(* Create the linear constraint *)
	LinearConstraint.make inequalities


(*--------------------------------------------------*)
(* get the affine dimensions of a flow              *)
(*--------------------------------------------------*)
let affine_dimensions program invariant flow =
	(* find affine variables *)
	let support = LinearConstraint.support flow in
	let affine_vars = VariableSet.elements (
		VariableSet.inter support (program.continuous) 
	) in
	(* determine lower and upper bounds for those variables *)
	List.map (fun v ->
		let a, b = LinearConstraint.bounds invariant v in
		let min = match a with
			| None -> raise (InternalError "cannot compute affine time elapse due to unbounded invariant")
			| Some x -> x in
		let max = match b with
			| None -> raise (InternalError "cannot compute affine time elapse due to unbounded invariant")
			| Some x -> x in
		(v, min, max)
	) affine_vars


exception Last_item
exception Unbounded


let affine_time_elapse program invariant deriv constr = 
	(* find affine dimensions *)
	let affine_dim = affine_dimensions program invariant deriv in
	let affine_vars = List.map (fun (v,_,_) -> v) affine_dim in
	let n = List.length affine_dim in
	List.iter (fun (v, min, max) -> 
		print_message Debug_medium ("affine dimension: " ^ (program.variable_names v) ^ " in [" ^
			(NumConst.string_of_numconst min) ^ ", " ^ (NumConst.string_of_numconst max) ^ "]")
	) affine_dim;
	
	(* compute the spanned ranges for all dimensions *)
	let p = match n with
		| 1 -> 32
		| 2 -> 8
		| 3 -> 4
		| _ -> 2 in		
	let size_table = Hashtbl.create n in
	List.iter (fun (v, min, max) -> 
		let range = NumConst.sub max min in
		Hashtbl.add size_table v (NumConst.div range (NumConst.numconst_of_int p))
	) affine_dim; 

	(* functions to iterate over all combinations of ints < p *)
	let rec next_item = function
		| [] -> raise Last_item
		| i :: tail -> if i < p-1 then (i+1 :: tail) else (0 :: (next_item tail)) in
	let rec init_item p =
		if p <= 0 then [] else 0 :: (init_item (p-1)) in
		
	(* function to compute boundaries of a partition *)
	let boundaries = List.map2 (fun (v, min, _) i ->
		let p_size = Hashtbl.find size_table v in 
		let p_min = NumConst.add min (NumConst.mul (NumConst.numconst_of_int i) p_size) in
		let p_max = NumConst.add p_min p_size in
		(v, p_min, p_max)
	) affine_dim in
	
	(* compute the linearized/rectangularized flow for a partition *)
	let rectangular_flow bounds =
		(* bound affine dimensions for this partition *)
		let p_invariant = LinearConstraint.make_box bounds in
		(* add global invariant and affine flow *)
		LinearConstraint.intersection_assign p_invariant [invariant; deriv; program.standard_flow];
		(* existentially quantify affine dimensions *)
		LinearConstraint.hide_assign affine_vars p_invariant;
		(* map to X space *)
		LinearConstraint.rename_variables_assign program.unrenamed_clocks_couples p_invariant;
		p_invariant in

	(* compute admissible neighbors of a partition, wrt. rectangular flow *)
	let find_neighbors part flow =		
		let min_flow, max_flow = List.split (List.map (LinearConstraint.bounds flow) affine_vars) in
		let down = Array.of_list (List.map (fun x -> 
			match x with 
				| Some y -> NumConst.ge NumConst.zero y
				| None -> true
			) min_flow) in
		let up   = Array.of_list (List.map (fun x -> 
			match x with
				| Some y -> NumConst.le NumConst.zero y
				| None -> true
			) max_flow) in
		let nbs = ref [] in		
		let rec build_neighbors hd tl v =
			match tl with
				| [] -> ()
				| i :: rest ->
						if (i > 0 && down.(v)) then nbs := (List.rev_append hd ((i-1) :: List.tl tl)) :: !nbs;
						if (i < p-1 && up.(v)) then nbs := (List.rev_append hd ((i+1) :: List.tl tl)) :: !nbs;  
						build_neighbors ((List.hd tl) :: hd) (List.tl tl) (v+1) in
		build_neighbors [] part 0;
		!nbs in 
		
	print_message Debug_low "Building partitions...";
		
	(* build the partitions *)
	let rec power b ex = if ex <= 0 then 1 else b * (power b (ex-1)) in
	let partitions = Hashtbl.create (power p n) in
	let neighbors = Hashtbl.create (power p n) in
	let _ = try (
		(* start with [0; 0; ... 0] *)
		let current_partition = ref (init_item n) in
		while true do
			(* compute lower and upper bounds for affine dimensions *)
			let my_boundaries = boundaries !current_partition in
			(* turn them into a rectangular box *)
			let my_invariant = LinearConstraint.make_box my_boundaries in
			(* strip variables from boundaries *)
			let my_bounds = List.map (fun (_,x,y) -> (x,y)) my_boundaries in
			(* add global invariant (there might be other variables involved) *)
			LinearConstraint.intersection_assign my_invariant [invariant];
			(* build the approximated flow for this partition *)
			let my_flow = rectangular_flow my_boundaries in
			(* find admissible neighbors *)
			let nbs = find_neighbors !current_partition my_flow in
			(* store partition and neighbors *)
			Hashtbl.add partitions !current_partition (my_bounds, my_invariant, my_flow);
			Hashtbl.add neighbors !current_partition nbs;
			(* increase the index list *)
			current_partition := next_item !current_partition
	  done; ()
	) with Last_item -> () in

	(* queue with states to explore *)
	let q = Queue.create () in
	
  (* function to compute bounds for a region, None for unbounded *)
	let compute_bounds region =
		let deopt = function
			| Some x, Some y -> x, y
			| _ -> raise Unbounded in
		try (
			Some (List.map deopt (List.map (LinearConstraint.bounds region) affine_vars))
		) with Unbounded -> None in																					

	(* check if two bounding boxes intersect *)
	let boxes_intersect bounds1 bounds2 =
		let rec intersect bounds1 bounds2 =
			match bounds1 with
				| [] -> true
				| (min1, max1) :: r1 -> 
					let (min2, max2) = List.hd bounds2 in
					if (NumConst.l max2 min1) or (NumConst.l max1 min2) then (
						false
					) else (
						let r2 = List.tl bounds2 in
						intersect r1 r2
					) in
		intersect bounds1 bounds2 in  							

	print_message Debug_low "Determine initial partitions...";
	
	(* find initial partitions that intersect with the original constraint *)
	let orig_bounds = compute_bounds constr in 
	Hashtbl.iter (fun index (bounds, inv, _) ->
		let box_cut = match orig_bounds with
			| None -> true
			| Some o_bounds -> boxes_intersect o_bounds bounds in
		if box_cut then (
			let cut = LinearConstraint.intersection [inv; constr] in
			if (LinearConstraint.is_satisfiable cut) then (
				Queue.add (index, cut, index) q;
			)
		)
	) partitions;
	   		
	let string_of_partition p = List.fold_left (^) "" (List.map string_of_int p) in 
	
	print_message Debug_low "Compute locally reachable partitions...";
	
	(* reachable local states *)
	let states = Hashtbl.create (power p n) in
	
	(* function to check if a state s in partition p has already been visited *)
	let visited p s =
		try (
			let p_states = Hashtbl.find_all states p in
			List.exists (LinearConstraint.is_leq s) p_states		
		) with Not_found -> false in
						
	(* function to check if a region cuts a neighboring partition *)
	let rec is_cutting p1 bounds1 p2 bounds2 =
		match p1 with
			| [] -> false
			| i :: rest -> 
					let j = List.hd p2 in
					let min1, max1 = (List.hd bounds1) in
					let min2, max2 = (List.hd bounds2) in
					if i < j then (
						NumConst.ge max1 min2
					) else if i > j then (
						NumConst.le min1 max2
					) else (
						is_cutting rest (List.tl bounds1) (List.tl p2) (List.tl bounds2)
					)	in	
																																																																																																																																																																																																																																																																																																										
	(* compute locally reachable states *)
	while not (Queue.is_empty q) do
		print_message Debug_total ((string_of_int (Queue.length q)) ^ " partitions in queue");
		(* get top of the queue *)
		let trg, constr, src = Queue.take q in
		print_message Debug_total ("consider partition " ^ (string_of_partition trg));
		(* get infos for target partition *)
		let _, inv, flow = Hashtbl.find partitions trg in
		(* do the time elapse *)
		let elapsed = LinearConstraint.time_elapse constr flow in
		(* limit to current partition *)
		LinearConstraint.intersection_assign elapsed [inv];
		(* already seen this one? *)
		if not (visited trg elapsed) then (
			(* register new state *)
			Hashtbl.add states trg elapsed;
			(* compute bounds for fast intersection *)
			let trg_bounds = compute_bounds elapsed in 
			(* explore neighboring partitions *)
			let all_ns = Hashtbl.find neighbors trg in
			let ns = match trg_bounds with
				| Some bounds -> (
					(* consider only neighbors that are cut by the computed region *)
					List.filter (fun p -> 
						let p_bounds, _, _ = Hashtbl.find partitions p in
						is_cutting trg bounds p p_bounds 
					) all_ns)
				| None -> all_ns in
			List.iter (fun nb ->
				print_message Debug_total ("neighbor: " ^ (string_of_partition nb));
				(* don't look back in anger *)
				if nb <> src then (
					(* get info of neighbor *)
					let _, inv', _ = Hashtbl.find partitions nb in
					(* get the intersecting region on the bounds *) 
					let cut = LinearConstraint.intersection [elapsed; inv'] in
					if LinearConstraint.is_satisfiable cut then (
						(* add to queue *)
						Queue.add (nb, cut, trg) q
					)
				) 
			) ns;			
		)		
	done;
	
	(* build a coarse overapproximation by taking all affected partitions *)
(*	let affected_partitions = Hashtbl.fold (fun p _ ps -> *)
(*		if List.mem p ps then	ps else p :: ps              *)
(*	) states [] in                                        *)
(*	let affected_invariants = List.map (fun p ->          *)
(*		let _, inv, _ = Hashtbl.find partitions p in        *)
(*		inv) affected_partitions in                         *)
(*	print_message Debug_low ("Compute convex hull of " ^ (string_of_int (List.length affected_partitions)) ^ " partitions...");*)
(*	LinearConstraint.hull affected_invariants                                                                                  *)
			
	(* return convex hull *)
	let regions = Hashtbl.fold (fun _ s regions -> s :: regions) states [] in
(*	let boxed_regions = List.map (LinearConstraint.bounding_box (VariableSet.elements program.continuous)) regions in*)
	print_message Debug_low ("Compute convex hull of " ^ (string_of_int (List.length regions)) ^ " local states...");
	LinearConstraint.hull regions


	
	

(*--------------------------------------------------*)
(* Compute the time elapse for a location           *)
(*--------------------------------------------------*)
let compute_time_elapse program invariant flow constr =
	let elapsed =
	match flow with
		| Undefined -> raise (InternalError "undefined flow")
		| Rectangular deriv -> (
			(* rename variables to X space *)
			let x_deriv = LinearConstraint.rename_variables program.unrenamed_clocks_couples deriv in
			LinearConstraint.time_elapse constr x_deriv
			)
		| Affine deriv -> (			
			affine_time_elapse program invariant deriv constr
		) in
	elapsed
		
		

(*--------------------------------------------------*)
(* Compute the initial state with the initial invariants and time elapsing *)
(*--------------------------------------------------*)
let create_initial_state program =
	(* Get the declared init state with initial constraint C_0(X) *)
	let initial_location, init_constraint = program.init in
	
	(* Compute the invariants I_q0(X) for the initial locations *)
	print_message Debug_high ("\nComputing initial invariant I_q0(X)");
	(* Create the invariant *)
	let invariant = compute_invariant program initial_location in
	(* Debug *)
	if debug_mode_greater Debug_total then print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names invariant);
		
	(* Compute constraint for assigning a (constant) value to discrete variables *)
	print_message Debug_high ("Computing constraint for discrete variables");
	let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value initial_location discrete_index)) program.discrete in
	let discrete_constraint = instantiate_discrete discrete_values in
	(* Debug *)
	if debug_mode_greater Debug_total then print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names discrete_constraint);
		
	(* Perform intersection of all those constraints *)
	print_message Debug_high ("Performing intersection of C0(X) and I_q0(X)");
	let full_constraint = LinearConstraint.intersection [
		init_constraint ; 
		invariant ; 
    discrete_constraint
	] in
	(* Debug *)
	if debug_mode_greater Debug_total then print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names full_constraint);
	
	(* let time elapse *)
	print_message Debug_high ("Let time elapse");
	let deriv = compute_flow program initial_location in
	let full_constraint = compute_time_elapse program invariant deriv full_constraint in
	(* Debug *)
	if debug_mode_greater Debug_total then print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names full_constraint);	

	(* add invariant after time elapsing *)
	print_message Debug_high ("Intersect with invariant after time elapsing");
	LinearConstraint.intersection_assign full_constraint [invariant];
	
	(* Debug *)
	if debug_mode_greater Debug_total then print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names full_constraint);	
	
	(* Hide discrete *)
	print_message Debug_high ("Hide discrete");
	LinearConstraint.hide_assign program.discrete full_constraint;
				
	(* Debug *)
	if debug_mode_greater Debug_total then print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names full_constraint);
	(* Return the initial state *)
	initial_location, full_constraint



(*--------------------------------------------------*)
(* Compute a list of possible actions for a state   *)
(*--------------------------------------------------*)
let compute_possible_actions program original_location = 
	(* Create a boolean array for the possible actions *)
	let possible_actions = Array.make program.nb_actions false in
	(* Fill it with all the possible actions per location *)
	for automaton_index = 0 to program.nb_automata - 1 do
		(* Get the current location for automaton_index *)
		let location_index = Automaton.get_location original_location automaton_index in
		(* Debug print *)
		print_message Debug_total ("Considering automaton " ^ (program.automata_names automaton_index) ^ " with location " ^ (program.location_names automaton_index location_index) ^ ".");
		(* Get the possible actions for this location *)
		let possible_actions_for_this_automaton =
			program.actions_per_location automaton_index location_index
		in
		(* Add all the actions to our array *)
		List.iter (fun action_index ->
			(* Add the action *)
			possible_actions.(action_index) <- true;
			(* Debug print *)
			print_message Debug_total ("Adding action " ^ (program.action_names action_index) ^ " for automaton " ^ (program.automata_names automaton_index) ^ " with location " ^ (program.location_names automaton_index location_index) ^ ".");
		) possible_actions_for_this_automaton;
	done;
	(* Debug print *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("Possible actions for all the locations are:");
		Array.iteri (fun action_index possible ->
			if possible then (print_message Debug_total (" - " ^ (program.action_names action_index)));
		) possible_actions;
	);
	(* Remove every action where an automaton can not take this action in its *)
	(*  location although the action was declared for this automaton          *)
	let possible_actions = Array.mapi (fun action_index possible ->
		(* If this action is not possible, then false *)
		if not possible then false else(
		let automata_for_this_action = program.automata_per_action action_index in
		(* Check if the action is possible for all the automata for which it is defined *)
		(**** TO OPTIMIZE: no need to keep searching if not "still_possible" anymore ****)
		let action_possible =
			List.fold_left (fun still_possible automaton_index -> 
				still_possible
				&& (List.mem action_index (program.actions_per_location automaton_index (Automaton.get_location original_location automaton_index)))
			) possible automata_for_this_action in
		(* Debug print *)
		if not action_possible && (debug_mode_greater Debug_total) then (
			print_message Debug_total ("But action '" ^ (program.action_names action_index) ^ "' is not possible for all declared automata.")
		);
		(* Replace the previous value by the new one *)
		action_possible
		)
	) possible_actions in
	(* Make a list *)
	true_indexes possible_actions



(*------------------------------------------------------------------*)
(* Compute a new location for a given set of transitions            *)
(* aut_table         : indices of involved automata                 *)
(* trans_table       : indices of examined transition per automaton *)
(* action_index      : index of current action                      *)
(* original_location : the source location                          *)
(*------------------------------------------------------------------*)
(* returns the new location, the guards, the updates                *)
(*------------------------------------------------------------------*)
let compute_new_location program aut_table trans_table action_index original_location =
  (* copy location *)
	let location = Automaton.copy_location original_location in
	(* Create a temporary hashtbl for discrete values *)
	let updated_discrete = Hashtbl.create program.nb_discrete in
	(* Create a set to collect all local variables involved in the transition *)
	let updated_continuous = ref VariableSet.empty in
	(* Update the location for the automata synchronized with 'action_index'; return the list of guards and updates *)
	let guards_and_updates = Array.to_list (Array.mapi (fun local_index real_index ->
		(* Get the current location for this automaton *)
		let location_index = Automaton.get_location original_location real_index in
		(* Find the transitions for this automaton *)
		let transitions = program.transitions real_index location_index action_index in
  	(* Get the index of the examined transition for this automaton *)
		let current_index = trans_table.(local_index) in
		(* Keep the 'current_index'th transition *)
		let transition = List.nth transitions current_index in
		(* Keep only the dest location *)
		let guard, clock_updates, discrete_updates, dest_index = transition in			
		(* Register the updated local variables for this automaton *)
		let local_variables = program.continuous_per_automaton real_index in
		let _ = match clock_updates with
			  (* register variables to avoid adding stable constraints later *)
			| All_free -> (updated_continuous := VariableSet.union !updated_continuous local_variables)
			  (* all stable -> do nothing in order to add stable constraints later *)
			| All_stable -> ()
			  (* register only analog variables in order to add stable constraints for clocks later *)
			| Clocks_stable -> (
				  let clocks = VariableSet.filter program.is_analog local_variables in
			    updated_continuous := VariableSet.union !updated_continuous clocks )
				(* register variables to avoid adding stable constraints later *)
			| Update _ -> (updated_continuous := VariableSet.union !updated_continuous local_variables) in
		(* Update discrete *)
		List.iter (fun (discrete_index, linear_term) ->
			(* Compute its new value *)
			let new_value = LinearConstraint.evaluate_linear_term (Automaton.get_discrete_value original_location) linear_term in
			(* Check if already updated *)
			if Hashtbl.mem updated_discrete discrete_index then (
				(* Find its value *)
				let previous_new_value = Hashtbl.find updated_discrete discrete_index in
				(* Compare with the new one *)
				if NumConst.neq previous_new_value new_value then (
				(* If different: warning *)
					print_warning ("The discrete variable '" ^ (program.variable_names discrete_index) ^ "' is updated several times with different values for the same synchronized action '" ^ (program.action_names action_index) ^ "'. The behavior of the system is now unspecified.");
				);
			) else (
				(* Else keep it in memory for update *)
				Hashtbl.add updated_discrete discrete_index new_value;
			);
		) discrete_updates;
		(* Update the global location *)
		Automaton.update_location_with [real_index, dest_index] [] location;
		(* Keep the guard and updates *)
		guard, clock_updates;
	) aut_table) in
	(* split into guards and updates *)
	let guards, clock_updates = List.split guards_and_updates in
	(* Compute couples to update the discrete variables *)
	let updated_discrete_couples = ref [] in
	Hashtbl.iter (fun discrete_index discrete_value ->
		updated_discrete_couples := (discrete_index, discrete_value) :: !updated_discrete_couples;
	) updated_discrete;
	(* Update the global location *)
	Automaton.update_location_with [] !updated_discrete_couples location;
  (* return the new location, the guards, and the clock updates *)
	(location, guards, !updated_continuous, clock_updates)
	
	
	
(*------------------------------------------------------------*)
(* Compute the clock updates for a given transition           *)
(* clock_updates: list of lists of variables to be reset      *)
(*------------------------------------------------------------*)
(* returns a couple of constraints, representing the updated  *)
(* and the non-updated clocks                                 *)
(*------------------------------------------------------------*)
let compute_updates program updated_continuous clock_updates =
	(* Compute X' = rho(X) *)
	print_message Debug_total ("\nComputing clock updates mu(X,X')");
 	(* get non-involved (thus stable) local variables *)
	let non_updated_vars = VariableSet.elements (
		VariableSet.diff program.continuous updated_continuous
	) in
	(* get only the updates where something is updated *)
	let update_constrs = List.fold_left (fun updates update ->  
		match update with					
			| Update c -> c :: updates
			| _ -> updates
	) [] clock_updates in
	let stable_pairs = List.map (fun v -> (program.prime_of_variable v, v)) non_updated_vars in 
	let stable_constr = LinearConstraint.make_equalities stable_pairs in
	(* compute the updated valus after the transition *)
	LinearConstraint.intersection_assign stable_constr update_constrs;
	let updates = stable_constr in
	updates


	
(*--------------------------------------------------*)	
(* Compute the new constraint for a transition      *)
(* orig_constraint : contraint in source location   *)
(* orig_location   : source location                *)
(* dest_location   : target location                *)
(* guards          : guard constraints per automaton*)
(* clock_updates   : updated clock variables        *)
(*--------------------------------------------------*)
let compute_new_constraint program orig_constraint orig_location dest_location guards updated_continuous clock_updates =
	(* the constraint is checked on the fly for satisfyability -> exception mechanism *)
	try ( 
		print_message Debug_total ("\nComputing equalities for discrete variables (previous values)");
		(* Compute discrete values using the current (new) location *)
		let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value orig_location discrete_index)) program.discrete in
		(* Convert to a constraint *)
		let new_constraint = instantiate_discrete discrete_values in
		(* Debug *)
		if debug_mode_greater Debug_total then print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names new_constraint);
	
		(* Debug *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total ("\nComputing guards g(X)");
			List.iter (fun guard -> 
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names guard);
			) guards;
		);
		print_message Debug_total ("\nComputing the guards and discrete");
		(* Add the (old) value for discrete to the guards *)
	  LinearConstraint.intersection_assign new_constraint guards;
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names new_constraint);
		);
	
		(* check here for unsatisfiability *)
		if not (LinearConstraint.is_satisfiable new_constraint) then (
			print_message Debug_high "skip transition";
			raise Unsat_exception
		);
		
		print_message Debug_total ("\nEliminate the discrete variables in g(X)");
		(* Remove the discrete variables *)
		LinearConstraint.hide_assign program.discrete new_constraint;
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names new_constraint);
		);
		
		print_message Debug_total ("\nComputing clock updates X' = rho(X) + d");
		let updates = compute_updates program updated_continuous clock_updates in

		(* Compute the invariant in the destination location *)
		print_message Debug_total ("\nComputing invariant I_q(X) ");
		let invariant = compute_invariant program dest_location in
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names invariant);
			if not (LinearConstraint.is_satisfiable invariant) then
				print_message Debug_total ("This constraint is NOT satisfiable.");
		);
		
		(* Compute the invariant after time elapsing *)
		print_message Debug_total ("Computing invariant I_q(X') ");
		let renamed_invariant = LinearConstraint.rename_variables program.renamed_clocks_couples invariant in
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names renamed_invariant);
			if not (LinearConstraint.is_satisfiable renamed_invariant) then
				print_message Debug_total ("This constraint is NOT satisfiable.");
		);

		(* Compute the equalities for the discrete variables *)
		print_message Debug_total ("\nComputing equalities for discrete variables");
		(* Compute discrete values using the current (new) location *)
		let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value dest_location discrete_index)) program.discrete in
		(* Convert to a constraint *)
		let discrete_constraint = instantiate_discrete discrete_values in
		(* Debug *)
		if debug_mode_greater Debug_total then (
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names discrete_constraint)
		);
	
		(* Perform the intersection *)
		print_message Debug_total ("\nPerforming intersection of C(X) and g(X) and X' = rho(X) and I_q(X') ");
		LinearConstraint.intersection_assign new_constraint	[
			orig_constraint ();
			updates;				
			renamed_invariant;
			discrete_constraint
		];
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names new_constraint)
		);
		(* Check satisfiability *)
		if not (LinearConstraint.is_satisfiable new_constraint) then (
			print_message Debug_high "invariant before time elapse is unsat -> skip transition";
			raise Unsat_exception
		);				

		(* Hide 'X' and 'd' *)
		print_message Debug_total ("\nHide 'X' and discrete in C(X) ^ g(X) ^ X' = rho(X) ^ I_q(X')");
		LinearConstraint.hide_assign program.clocks_and_discrete new_constraint;
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names new_constraint);
			if not (LinearConstraint.is_satisfiable new_constraint) then
				print_message Debug_total ("This constraint is NOT satisfiable.");
		);

		(* Rename X' -> X *)
		print_message Debug_total ("\nRenaming X' into X:");			
		LinearConstraint.rename_variables_assign program.unrenamed_clocks_couples new_constraint;
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names new_constraint);
			if not (LinearConstraint.is_satisfiable new_constraint) then
				print_message Debug_total ("This constraint is NOT satisfiable.");
		);
		
		(* let time elapse *)
		print_message Debug_total ("\nLet time elapse");
		let deriv = compute_flow program dest_location in
		let new_constraint = compute_time_elapse program invariant deriv new_constraint in						 
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names new_constraint);
			if not (LinearConstraint.is_satisfiable new_constraint) then
				print_message Debug_total ("This constraint is NOT satisfiable.");
		);

		(* add invariant *)
		print_message Debug_total ("\nIntersect with invariant I(X)");
		LinearConstraint.intersection_assign new_constraint [invariant];
		(* Debug *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names new_constraint);
		);

		(* result of try-block *)
		Some new_constraint
	) with Unsat_exception -> None
		
		
	
(*------------------------------------------------*)
(* Computes next combination of indices           *)
(* current_indexes : combination                  *)
(* max_indexes     : maximum indices              *)
(*------------------------------------------------*)
(* returns a boolean, indicating that the         *)
(* new combination is valid (false if the old     *)
(* combination was the last one)                  *)
(*------------------------------------------------*)
let next_combination combination max_indexes =
	let len = Array.length combination in 
	let valid_combination = ref true in 
	let not_is_max = ref true in
	let local_index = ref 0 in
	while !not_is_max do
		(* Look for the first index to be incremented *)
		if combination.(!local_index) < max_indexes.(!local_index) then(
			(* Increment this index *)
			combination.(!local_index) <- combination.(!local_index) + 1;
			(* Reset the smaller indexes to 0 *)
			for i = 0 to !local_index - 1 do
				combination.(i) <- 0;
			done;
			(* Stop the loop *)
			not_is_max := false;
		) else (
			local_index := !local_index + 1;
			if !local_index >= len then(
				valid_combination := false;
				not_is_max := false;
			)
		)
	done; (* end while *)
	!valid_combination



(*-----------------------------------------------------*)
(* Checks a new state for pi0-compatibility and        *)
(* updates constraint K if incompatible state is found.*)
(* pi0               : reference valuation             *)
(* rechability_graph : current reachability graph      *)
(* constr            : new state constraint            *)
(*-----------------------------------------------------*)
(* returns true iff the state is pi0-compatible        *)
(*-----------------------------------------------------*)
let inverse_method_check_constraint program pi0 reachability_graph constr =			
	(* Hide non parameters (X) *)
	print_message Debug_high ("\nHiding non parameters:");
	let p_constraint = LinearConstraint.hide program.clocks_and_discrete constr in
	(* Debug print *)
	if debug_mode_greater Debug_high then(
		print_message Debug_high (LinearConstraint.string_of_linear_constraint program.variable_names p_constraint);
	);
	(* Check the pi0-compatibility *)
	let compatible, incompatible = LinearConstraint.partition_pi0_compatible pi0 p_constraint in
	let is_pi0_incompatible = incompatible != [] in
	
	(* If pi0-incompatible: select an inequality *)
	if is_pi0_incompatible then (
		print_message Debug_low ("\nFound a pi0-incompatible state.");
		(* Debug print *)
		if debug_mode_greater Debug_medium then(
			print_message Debug_medium ("\nThe following inequalities are pi0-incompatible:");
			List.iter (fun inequality -> print_message Debug_medium (LinearConstraint.string_of_linear_inequality program.variable_names inequality)) incompatible;
		);

		let inequality =
			(* If random selection: pick up a random inequality *)
			if program.random then random_element incompatible
			(* Else select the first one *)
			else List.nth incompatible 0
		in
		(* Debug print *)
		if debug_mode_greater  Debug_medium then(
			print_message Debug_medium ("\nSelecting the following pi0-incompatible inequality:");
			print_message Debug_medium (LinearConstraint.string_of_linear_inequality program.variable_names inequality);
		);

		(* Negate the inequality *)
		let negated_inequality = LinearConstraint.negate_wrt_pi0 pi0 inequality in
		(* Debug print *)
		let randomly = if program.random then "randomly " else "" in
		let among = if List.length incompatible > 1 then (" (" ^ randomly ^ "selected among " ^ (string_of_int (List.length incompatible)) ^ " inequalities)") else "" in
		print_message Debug_standard ("  Adding the following inequality" ^ among ^ ":");
		print_message Debug_standard ("  " ^ (LinearConstraint.string_of_linear_inequality program.variable_names negated_inequality));

		(* Update the previous states (including the 'new_states' and the 'orig_state') *)
		print_message Debug_medium ("\nUpdating all the previous states.\n");
		Graph.add_inequality_to_states reachability_graph negated_inequality;
		
		(* update global K *)
		LinearConstraint.intersection_assign !global_k [(LinearConstraint.make [negated_inequality])];
		
		(* If pi-incompatible *)
		false
		(* If pi-compatible *)
	) else true



(*-------------------------------------------------------*)
(* Computes all possible transition combinations for the *)
(* involved automata.                                    *)
(* constr               : current state constraint       *)
(* action index         : index of current action        *)
(* automata             : involved automata              *)
(* aut_table            : array of automata              *)
(* max_indexes          : array of maximal trans. indices*)
(* possible_transitions : array of transition indices    *)
(*-------------------------------------------------------*)
(* returns a bool, indicating iff at least one legal     *)
(* combination exists.                                   *)
(*-------------------------------------------------------*)
let compute_transitions program location constr action_index automata aut_table max_indexes possible_transitions  =
	let current_index = ref 0 in 
	(* Stop computation as soon as one automaton has no legal transition left. *)
	try (
		List.iter (fun automaton_index ->
			(* Tabulate the real index *)
			aut_table.(!current_index) <- automaton_index;
			(* Get the current location for this automaton *)
			let location_index = Automaton.get_location location automaton_index in
			(* Get transitions for this automaton *)
			let transitions = program.transitions automaton_index location_index action_index in
			(* Keep only possible transitions *)
			let is_possible = fun trans -> (
				let guard, _, _, _ = trans in
				let constr_and_guard = LinearConstraint.intersection [constr; guard] in
				let is_possible = LinearConstraint.is_satisfiable constr_and_guard in
				if not is_possible then (
					print_message Debug_medium "** early skip transition **"
				);
				is_possible
			) in
			let legal_transitions = ref [] in
			let trans_index = ref 0 in
			List.iter (fun trans -> 
				if is_possible trans then(
					legal_transitions := !trans_index :: !legal_transitions
				);
				trans_index := !trans_index + 1
			) transitions;
			(* Stop computation if no legal transition exists *)
			if !legal_transitions = [] then (
				print_message Debug_medium "*** early skip action ***";
				raise Unsat_exception
			);
			(* Store possible transitions *)
			possible_transitions.(!current_index) <- !legal_transitions;
			(* Tabulate the number of transitions for this location *)
			max_indexes.(!current_index) <-	List.length !legal_transitions - 1;
			(* Increment the index *)
			current_index := !current_index + 1;
		) automata;
		(* arrived here, so each automaton must have at least one legal transition *)
		true		
	) with Unsat_exception -> false



(*-----------------------------------------------------*)
(* Compute the list of possible destination states     *)
(* wrt. to a reachability_graph, and update this graph *)
(*-----------------------------------------------------*)
(* returns a list of (really) new states               *)
(*-----------------------------------------------------*)
let post program pi0 reachability_graph orig_state_index =
	(* Original location: static *)
	let original_location, _ = Graph.get_state reachability_graph orig_state_index in
	(* Dynamic version of the orig_constraint (can change!) *)
	let orig_constraint () =
		let _, orig_constraint = Graph.get_state reachability_graph orig_state_index in
		orig_constraint
	in

	(* Debug prints *)
	if debug_mode_greater Debug_high then(
		let orig_state = Graph.get_state reachability_graph orig_state_index in
		let _, orig_constraint = orig_state in
		let orig_constraint_projection = LinearConstraint.hide program.clocks_and_discrete orig_constraint in
		print_message Debug_high ("Performing post from state:");
		print_message Debug_high (string_of_state program orig_state);
		print_message Debug_high ("\nThe projection of this constraint onto the parameters is:");
		print_message Debug_high (LinearConstraint.string_of_linear_constraint program.variable_names orig_constraint_projection);
	);

	(* get possible actions originating from current state *)
	let list_of_possible_actions = compute_possible_actions program original_location in

	(* Debug print *)
	if debug_mode_greater Debug_high then (
		let actions_string = List.map (fun action_index -> program.action_names action_index) list_of_possible_actions in
		print_message Debug_high ("Possible synchronized actions are: " ^ (string_of_list_of_string_with_sep ", " actions_string));
	);

	(* build the list of new states *)
	let new_states = ref [] in

	(* FOR ALL ACTION DO: *)
	List.iter (fun action_index ->

		print_message Debug_medium ("\nComputing destination states for action '" ^ (program.action_names action_index) ^ "'");
		(* Get the automata declaring the action *)
		let automata_for_this_action = program.automata_per_action action_index in
		let nb_automata_for_this_action = List.length automata_for_this_action in
	
		(*------------------------------------------------*)
		(* Compute the reachable states on the fly: i.e., all the possible transitions for all the automata belonging to 'automata' *)
		(*------------------------------------------------*)
		
		(* add discrete values to constraint *)
		let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value original_location discrete_index)) program.discrete in
		(* Convert to a constraint *)
		let discrete_constr = instantiate_discrete discrete_values in
		(* compute conjunction with current constraint *)
		let orig_plus_discrete = LinearConstraint.intersection [orig_constraint (); discrete_constr] in
		
		(* Give a new index to those automata *)
		let real_indexes = Array.make nb_automata_for_this_action 0 in
		(* Keep an array of possible transition indices for each automaton *)
		let possible_transitions = Array.make nb_automata_for_this_action [] in
		(* Use an array of transition indices for the search (start with 0),*)
		(* indicating the current index within the possible transitions for*)
		(* each automaton *)
		let current_indexes = Array.make nb_automata_for_this_action 0 in
		(* Keep the maximum index of possible transitions for each automaton *)
		let max_indexes = Array.make nb_automata_for_this_action 0 in
		(* Array for the currently selected transition indices *)
		let current_transitions = Array.make nb_automata_for_this_action 0 in
		
		(* compute the possible combinations of transitions *)
		let legal_transitions_exist = compute_transitions program original_location orig_plus_discrete action_index automata_for_this_action real_indexes max_indexes possible_transitions in 
	
		(* Debug: compute the number of combinations *)
		if legal_transitions_exist && (debug_mode_greater Debug_medium) then(
			let nb_combinations = Array.fold_left (fun sum max -> sum * (max + 1)) 1 max_indexes in
			print_message Debug_medium ("I will consider " ^ (string_of_int nb_combinations) ^ " combination" ^ (s_of_int nb_combinations) ^ " for this state and this action\n");
		);
	
		(* Loop on all the transition combinations *)
		let more_combinations = ref legal_transitions_exist in
		while !more_combinations do
			(* Debug *)
			if debug_mode_greater Debug_total then (
			let local_indexes = string_of_array_of_string_with_sep "\n\t" (
			Array.mapi (fun local_index real_index ->
				(string_of_int local_index) ^ " -> " ^ (string_of_int real_index) ^ " : " ^ (string_of_int current_indexes.(local_index)) ^ "; ";
			) real_indexes) in
			print_message Debug_total ("--- Consider the combination \n\t" ^ local_indexes);
			);
	
			(* build the current combination of transitions *)
			for i=0 to Array.length current_transitions -1 do
				current_transitions.(i) <- List.nth (possible_transitions.(i)) (current_indexes.(i))
			done; 
	
			(* Compute the new location for the current combination of transitions *)
			let location, guards, updated_continuous, clock_updates = compute_new_location program real_indexes current_transitions action_index original_location in
			
			(* Compute the new constraint for the current transition *)
			let new_constraint = compute_new_constraint program orig_constraint original_location location guards updated_continuous clock_updates in
	
			(* Check the satisfiability *)
			match new_constraint with
				| None -> 
					print_message Debug_high ("\nThis constraint is not satisfiable.");	
				| Some final_constraint ->(					
					if not (LinearConstraint.is_satisfiable final_constraint) then(
						print_message Debug_high ("\nThis constraint is not satisfiable.");
					) else (
			
					let add_new_state =
					(* Branching between 2 algorithms here *)
					if program.imitator_mode = Reachability_analysis then ( 
						true
					) else (
						inverse_method_check_constraint program pi0 reachability_graph final_constraint
					) in
					
					if add_new_state then (				
						(* Create the state *)				
						let new_state = location, final_constraint in
		
						(* Debug print *)
						if debug_mode_greater Debug_total then(
							print_message Debug_total ("Consider the state \n" ^ (string_of_state program new_state));
						);
						
						(* Add this new state *)
						(* Try to add the state to the graph // with the p-constraint ????? *)
						let new_state_index, added = Graph.add_state program reachability_graph new_state in
						(* If this is really a new state *)
						if added then (
							(* Add the state_index to the list of new states *)
							new_states := new_state_index :: !new_states;
						);
						(* Update the transitions *)
						Graph.add_transition reachability_graph (orig_state_index, action_index, new_state_index);
						(* Debug print *)
						if debug_mode_greater Debug_high then (
							let beginning_message = (if added then "NEW STATE" else "Old state") in
							print_message Debug_high ("\n" ^ beginning_message ^ " reachable through action '" ^ (program.action_names action_index) ^ "': ");
							print_message Debug_high (string_of_state program new_state);
						);
					); (* end if pi0 incompatible *)
				); (* end if satisfiable *)
			);
		
			(* get the next combination *)
			more_combinations := next_combination current_indexes max_indexes 		
			
		done; (* while more new states *)
	) list_of_possible_actions;
	
	(* Return the list of (really) new states *)
	List.rev (!new_states)



(*---------------------------------------------------*)
(* Compute the reachability graph from a given state *)
(*---------------------------------------------------*)
let post_star program options pi0 init_state =	
	(* Time counter *)
	let counter = ref (Unix.gettimeofday()) in
	(* copy init state, as it might be destroyed later *)
	let init_loc, init_constr = init_state in
	let init_state = (init_loc, LinearConstraint.copy init_constr) in
	(* Get some variables *)
	let nb_actions = program.nb_actions in
	let nb_variables = program.nb_variables in
	let nb_automata = program.nb_automata in
	(* Debut prints *)
	print_message Debug_low ("Performing reachability analysis from state:");
	print_message Debug_low (string_of_state program init_state);
	(* Guess the number of reachable states *)
	let guessed_nb_states = 10 * (nb_actions + nb_automata + nb_variables) in 
	let guessed_nb_transitions = guessed_nb_states * nb_actions in 
	print_message Debug_total ("I guess I will reach about " ^ (string_of_int guessed_nb_states) ^ " states with " ^ (string_of_int guessed_nb_transitions) ^ " transitions.");
	(* Create the reachability graph *)
	let reachability_graph = Graph.make guessed_nb_transitions in
	
	(* Add the initial state to the reachable states *)
	let init_state_index, _ = Graph.add_state program reachability_graph init_state in

	(* initialize global constraint K *)
	global_k := LinearConstraint.true_constraint ();

	(*--------------------------------------------------*)
	(* Perform the post^* *)
	(*--------------------------------------------------*)
	let newly_found_new_states = ref [init_state_index] in
	let nb_iterations = ref 1 in
	let limit_reached = ref false in

	(* Check if the list of new states is empty *)
	while not (!limit_reached  || list_empty !newly_found_new_states) do
		if debug_mode_greater Debug_standard then (
			print_message Debug_low ("\n");
			print_message Debug_standard ("Computing post^" ^ (string_of_int (!nb_iterations)) ^ "");
			print_message Debug_low ("Number of recently found state" ^ (s_of_int (List.length !newly_found_new_states)) ^ ": " ^ (string_of_int (List.length !newly_found_new_states)) ^ ".");
		);
		(* Count the states for debug purpose: *)
		let num_state = ref 0 in
		(* Length of 'newly_found_new_states' for debug purpose *)
		let nb_states = List.length !newly_found_new_states in

		let new_newly_found_new_states =
		(* For each newly found state: *)
		List.fold_left (fun new_newly_found_new_states orig_state_index ->
			(* Count the states for debug purpose: *)
			num_state := !num_state + 1;
			(* Perform the post *)
			let post = post program pi0 reachability_graph orig_state_index in
			let new_states = post in
			(* Debug *)
			if debug_mode_greater Debug_medium then (
				let beginning_message = if list_empty new_states then "Found no new state" else ("Found " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
				print_message Debug_medium (beginning_message ^ " for the post of state " ^ (string_of_int !num_state) ^ " / " ^ (string_of_int nb_states) ^ " in post^" ^ (string_of_int (!nb_iterations)) ^ ".\n");
			);

			(* Return the concatenation of the new states *)
			(**** OPTIMIZED: do not care about order (else shoud consider 'list_append new_newly_found_new_states (List.rev new_states)') *)
			List.rev_append new_newly_found_new_states new_states
		) [] !newly_found_new_states in
		(* Update the newly_found_new_states *)
		newly_found_new_states := new_newly_found_new_states;
		(* Debug *)
		if debug_mode_greater Debug_medium then (
			let beginning_message = if list_empty !newly_found_new_states then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length !newly_found_new_states)) ^ " new state" ^ (s_of_int (List.length !newly_found_new_states)) ^ "") in
			print_message Debug_medium (beginning_message ^ " for post^" ^ (string_of_int (!nb_iterations)) ^ ".\n");
		);
		
		(* Clean up a little *)
		Gc.major ();
		
		(* Iterate *)
		nb_iterations := !nb_iterations + 1;
		(* Check if the limit has been reached *)
		match options#post_limit with
			| None -> ()
			| Some limit -> if !nb_iterations > limit then limit_reached := true;
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then limit_reached := true;
	done;

	if !limit_reached && not (list_empty !newly_found_new_states) then(
		match options#post_limit with
			| None -> ()
			| Some limit -> if !nb_iterations > limit then print_warning (
				"The limit number of iterations (" ^ (string_of_int limit) ^ ") has been reached. Post^* now stops, although there were still " ^ (string_of_int (List.length !newly_found_new_states)) ^ " state" ^ (s_of_int (List.length !newly_found_new_states)) ^ " to explore at this iteration."
			);
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. Post^* now stops, although there were still " ^ (string_of_int (List.length !newly_found_new_states)) ^ " state" ^ (s_of_int (List.length !newly_found_new_states)) ^ " to explore at this iteration."
			);
	);


	(*--------------------------------------------------*)
	(* Perform the intersection of all inequalities *)
	(*--------------------------------------------------*)
	if program.imitator_mode <> Reachability_analysis then (
		print_message Debug_standard ("\nFinal constraint K  :");
		print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names !global_k);
	);


	(*--------------------------------------------------*)
	(* Debug *)
	(*--------------------------------------------------*)
	print_message Debug_standard (
		"\nFixpoint reached after "
		^ (string_of_int (!nb_iterations)) ^ " iteration" ^ (s_of_int (!nb_iterations)) ^ ""
		^ " in " ^ (string_of_seconds (time_from !counter)) ^ ": "
		^ (string_of_int (Graph.nb_states reachability_graph)) ^ " reachable state" ^ (s_of_int (Graph.nb_states reachability_graph))
		^ " with "
		^ (string_of_int (Hashtbl.length (reachability_graph.transitions_table))) ^ " transition" ^ (s_of_int (Hashtbl.length (reachability_graph.transitions_table))) ^ ".");

	(*--------------------------------------------------*)
	(* Return the result *)
	(*--------------------------------------------------*)
	reachability_graph, !global_k, !nb_iterations, !counter
