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
	if analogs = [] then (
		(* no analogs, always return empty flow *)
		fun _ _  -> None
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


let invert program =
	let inv_standard_flow = invert_standard_flow program in
	let inv_analog_flows = invert_flows program in
	let inv_program = { program with 
		standard_flow = inv_standard_flow;
		analog_flows = inv_analog_flows
	} in
	print_message Debug_high (string_of_program inv_program );
	inv_program 
