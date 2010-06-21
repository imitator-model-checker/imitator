open Global
open AbstractImitatorFile
open Ppl_ocaml
open ImitatorPrinter

(* Invert a flow constraint *)
let invert_flow vars_to_invert flow =
	let negate_vars = fun v -> (
		if (List.mem v vars_to_invert) then
			Unary_Minus (Variable v)
		else
			Variable v
	) in
	LinearConstraint.substitute negate_vars flow 	


(* Invert the standard flow for clocks *)
let invert_standard_flow program =
	let flow = program.standard_flow in
	let clocks = program.clocks in
	invert_flow clocks flow


(* Invert the flows for analog variables *)
let invert_flows program = 
	(* get the variables to invert *)
	let analogs = List.filter (program.is_analog) program.clocks in
	if (analogs = []) then (
		(* always return the empty flow *)
		fun _ _ -> None
	) else (
		(* create an empty array *)
		let new_flow_array = Array.create program.nb_automata (Array.create 0 None) in
		(* For each automaton *) 
		for aut_index = 0 to (program.nb_automata - 1) do		
			let locations = program.locations_per_automaton aut_index in
			let nb_locations = List.length locations in
			let flows = Array.create nb_locations None in
			(* For each location *)
			List.iter (fun loc_index -> 
				let flow = program.analog_flows aut_index loc_index in
				let inv_flow = match flow with 
					| Some constr -> Some (invert_flow analogs constr) 
					| None -> None in
				flows.(loc_index) <- inv_flow;
			) locations;
			(* store array *)
			new_flow_array.(aut_index) <- flows
		done;
		(* functional representation *)
	  fun aut_index loc_index -> new_flow_array.(aut_index).(loc_index)
	)
	
(* invert a constraint and compute its update set *)
let invert_constraint program constr =
	(* swap X and X' in constraint *)
	let inv_constr = LinearConstraint.rename_variables program.renamed_clocks_couples constr in
	(* compute the support variables *)
	let support = LinearConstraint.support inv_constr in
	(* filter primed variables to obtain the update set *)
	let update_set = VariableSet.filter program.is_renamed_clock support in
	(* rename the variables in the update set *)
	let renamed_update_set = VariableSet.fold (fun v set -> 
		VariableSet.add (program.variable_of_prime v) set
	) update_set VariableSet.empty in
	(inv_constr, renamed_update_set)	
	
	
(* invert the update and guard for a transition *)
let invert_update program update =
	(* swap variables X,X' in jump relation and compute its update set *)
	match update with
		| None -> None
		| Some (_, mu) -> (
				(* invert mu and combine with inverted guard *)
				let inv_mu, update_set = invert_constraint program mu in
				Some (update_set, inv_mu)
	)
	
	
(* invert one transition, returns a pair (loc, trans) with the new source location *)
(* and the converted transition  *)
let invert_transition program new_dest transition =
	(* get components, drop destination location *)
	let guard, update, discr_updates, old_dest = transition in
	(* invert discrete updates *)
	print_message Debug_total "invert discrete updates";
	let inv_discr_updates =
		try (
			Matrix.invert_updates program.discrete discr_updates
		) with Matrix.Singular -> (
			raise (InternalError ("discrete updates not reversible (" ^	(string_of_discrete_updates program discr_updates) ^ ")"))
		) in 		
	(* invert updates *)
	print_message Debug_total "invert clock updates";
	let inv_update = invert_update program update in
	(* assemble new transition *)
	let inv_trans = (guard, inv_update, inv_discr_updates, new_dest) in
	(* return new source location and inverted transition *)
	(old_dest, inv_trans)
	
	
(* invert all transitions and create the index for actions *)
let invert_transitions program =	
	(* create empty array for transitions *)
	let trans_array = Array.make program.nb_automata (Array.make 0 (Array.make 0 [])) in
	let action_array = Array.make program.nb_automata (Array.make 0 []) in
	(* for each automaton *)
	let automata = program.automata in 
	List.iter (fun aut_index -> 
		(* create arrays for this automaton *)
		let locations = program.locations_per_automaton aut_index in
		let nb_locations = List.length locations in
		let nb_actions = program.nb_actions in	
		trans_array.(aut_index) <- Array.make_matrix nb_locations nb_actions [];
		action_array.(aut_index) <- Array.make nb_locations [];
		(* for each location in this automaton *)
		List.iter (fun loc_index ->
			(* for each possible action *)
			let actions = program.actions_per_location aut_index loc_index in
			List.iter (fun act_index -> 			
				let transitions = program.transitions aut_index loc_index act_index in
				(* invert transitions *)
				List.iter (fun transition -> 
					let new_source, inv_trans = invert_transition program loc_index transition in
					(* store new transition *)
					trans_array.(aut_index).(new_source).(act_index) <- inv_trans :: trans_array.(aut_index).(new_source).(act_index);
					(* register action *)
					action_array.(aut_index).(new_source) <- list_union [act_index] action_array.(aut_index).(new_source);
				) transitions;
			) actions;
		) locations;
	) automata;
	(* return functional representations *)
	let trans_fun = fun aut_i loc_i act_i -> trans_array.(aut_i).(loc_i).(act_i) in
	let action_fun = fun aut_i loc_i -> action_array.(aut_i).(loc_i) in
	(trans_fun, action_fun)
	

(* debug dump *)
let dump program = 
	for aut_i = 0 to (program.nb_automata - 1) do (
		let locations = program.locations_per_automaton aut_i in
		List.iter (fun loc_i -> 
			let actions = program.actions_per_location aut_i loc_i in
			let nb_actions = List.length actions in
			print_message Debug_standard (
				"location " ^ (program.automata_names aut_i) ^
				"." ^ (program.location_names aut_i loc_i) ^ 
				" has " ^ (string_of_int nb_actions) ^
				" registered actions:");
			List.iter (fun act_i -> 
				print_message Debug_standard (
					"action " ^ (program.action_names act_i));
				let transitions = program.transitions aut_i loc_i act_i in				
				List.iter (fun trans -> 
					let guard, update, discr_update, dest_i = trans in
					print_message Debug_standard (
						"  -> " ^ (program.location_names aut_i dest_i));
					print_message Debug_standard (
						"     guard: " ^ (LinearConstraint.string_of_linear_constraint program.variable_names guard));
					let upstr = match update with
						| None -> "empty"
						| Some (update_set, constr) -> (
								VariableSet.fold (fun v str -> str ^ " " ^ (program.variable_names v)) update_set ""
							)	in
					print_message Debug_standard (
						"     updated variables: " ^ upstr);
						
				) transitions
			) actions   
		) locations
	) done


(* invert all automata in a program *)
let invert program =
	print_message Debug_high "invert standard flow";
	let inv_standard_flow = invert_standard_flow program in
	(* invert flow conditions *)
	print_message Debug_high "invert analog flows";
	let inv_analog_flows = invert_flows program in
	(* invert transitions *)
	print_message Debug_high "invert transitions"; 
	let inv_transitions, new_actions_per_location = invert_transitions program in
	
	(* construct new structure *)
	let inv_program = { program with 
		standard_flow = inv_standard_flow;
		analog_flows = inv_analog_flows;
		transitions = inv_transitions;
		actions_per_location = new_actions_per_location
	} in
	inv_program
