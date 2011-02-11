(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/02
 * Last modified: 2010/03/10
 *
 **************************************************)


open Global
open AbstractImitatorFile


(**************************************************)
(** Program *)
(**************************************************)

(* Convert a var_type into a string *)
let string_of_var_type = function
	| Var_type_analog -> "analog"
	| Var_type_clock -> "clock"
	| Var_type_discrete -> "discrete"
	| Var_type_parameter -> "parameter"

(* Add a header to the program *)
let string_of_header program =
		"(**************************************************"
	^ "\n" ^" * Program " ^ program.program_name
	^ "\n" ^" **************************************************)"

(* Convert the initial variable declarations into a string *)
let string_of_declarations program =
	"var "
	^
	let string_of_variables list_of_variables =
		string_of_list_of_string_with_sep ", " (List.map program.variable_names list_of_variables) in
	(if program.nb_clocks > 0 then
		("\n\t" ^ (string_of_variables program.clocks) ^ "\n\t\t: clock;") else "")
	^
	(if program.nb_discrete > 0 then
		("\n\t" ^ (string_of_variables program.discrete) ^ "\n\t\t: discrete;") else "")
	^
	(if program.nb_parameters > 0 then
		("\n\t" ^ (string_of_variables program.parameters) ^ "\n\t\t: parameter;") else "")

(* Convert the synclabs of an automaton into a string *)
let string_of_synclabs program automaton_index =
	"synclabs: "
	^ (let synclabs, _ = (List.fold_left (fun (synclabs, first) action_index ->
		match program.action_types action_index with
		(* Case sync: declare *)
		| Action_type_sync ->
			synclabs
			^ (if not first then ", " else "")
			^ (program.action_names action_index)
			(* Not the first one anymore *)
			, false
		(* Case nosync: do not declare *)
		| Action_type_nosync -> (synclabs, first)
	) ("", true) (program.actions_per_automaton automaton_index)) in synclabs)
	^ ";"


(* Convert the initially of an automaton into a string *)
let string_of_initially program automaton_index =
	let inital_locations, _ = program.init in
	let initial_location = Automaton.get_location inital_locations automaton_index in
	"initally: "
	^ (program.location_names automaton_index initial_location)
	^ ";"


(* Convert the invariant of a location into a string *)
let string_of_invariant program automaton_index location_index =
	"while "
	^ (LinearConstraint.string_of_linear_constraint program.variable_names (program.invariants automaton_index location_index))
	^ " wait {"
	^ (LinearConstraint.string_of_linear_constraint program.variable_names (
			LinearConstraint.hide (List.append program.parameters program.discrete ) program.standard_flow		 
		))
	^ (
		let flow = program.analog_flows automaton_index location_index in
		match flow with
			| Affine f 
			| Rectangular f -> " & " ^ (LinearConstraint.string_of_linear_constraint program.variable_names f)
			| Undefined -> ""
		) 
	^ "}"


(* Convert a sync into a string *)
let string_of_sync program action_index =
	match program.action_types action_index with
	| Action_type_sync -> " sync " ^ (program.action_names action_index)
	| Action_type_nosync -> " (* sync " ^ (program.action_names action_index) ^ "*) "


(* Convert a list of discrete updates into a string *)
let string_of_discrete_updates updates =
	let program = Program.get_program () in
	string_of_list_of_string_with_sep ", " (List.map (fun (variable_index, linear_term) ->
		(* Convert the variable name *)
		(program.variable_names variable_index)
		^ "' = "
		(* Convert the linear_term *)
		^ (LinearConstraint.string_of_linear_term program.variable_names linear_term)
	) updates)

(* Convert an update into a string *)
let string_of_update program aut_index update =
		match update with
			| All_free -> ""
			| All_stable -> (
				let vars = program.continuous_per_automaton aut_index in
				let stable_strings =
					List.map (fun v -> 
						let var_name = program.variable_names v in
						var_name ^ "' = " ^ var_name 
					) (VariableSet.elements vars) in
				string_of_list_of_string_with_sep ", " stable_strings
			)
			| Clocks_stable -> (
				let vars = VariableSet.elements (program.continuous_per_automaton aut_index) in
				let clocks = List.filter program.is_clock vars in
				let stable_strings =
					List.map (fun v -> 
						let var_name = program.variable_names v in
						var_name ^ "' = " ^ var_name 
					) clocks in
				string_of_list_of_string_with_sep ", " stable_strings				
				)
			| Update update_constr ->  
					LinearConstraint.string_of_linear_constraint program.variable_names update_constr

(* Convert a transition of a location into a string *)
let string_of_transition program automaton_index action_index (guard, update, discrete_updates, destination_location) =
	"\n\t" ^ "when "
	(* Convert the guard *)
	^ (LinearConstraint.string_of_linear_constraint program.variable_names guard)
	(* Convert the updates *)
	^ " do {"
	^ (string_of_update program automaton_index update)
	^ (string_of_discrete_updates discrete_updates)
	^ "} "
	(* Convert the sync *)
	^ (string_of_sync program action_index)
	(* Convert the destination location *)
	^ " goto " ^ (program.location_names automaton_index destination_location)
	^ ";"


(* Convert the transitions of a location into a string *)
let string_of_transitions program automaton_index location_index =
	string_of_list_of_string (
	(* For each action *)
	List.map (fun action_index -> 
		(* Get the list of transitions *)
		let transitions = program.transitions automaton_index location_index action_index in
		(* Convert to string *)
		string_of_list_of_string (
			(* For each transition *)
			List.map (string_of_transition program automaton_index action_index) transitions
			)
		) (program.actions_per_location automaton_index location_index)
	)


(* Convert a location of an automaton into a string *)
let string_of_location program automaton_index location_index =
	"\n" ^ "loc: "
	^ (program.location_names automaton_index location_index)
	^ ": "
	^ (string_of_invariant program automaton_index location_index)
	^ (string_of_transitions program automaton_index location_index)


(* Convert the locations of an automaton into a string *)
let string_of_locations program automaton_index =
	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		string_of_location program automaton_index location_index
	) (program.locations_per_automaton automaton_index))


(* Convert an automaton into a string *)
let string_of_automaton program automaton_index =
	"\n(**************************************************)"
	^ "\n automaton " ^ (program.automata_names automaton_index)
	^ "\n(**************************************************)"
	^ "\n " ^ (string_of_synclabs program automaton_index)
	^ "\n " ^ (string_of_initially program automaton_index)
	^ "\n " ^ (string_of_locations program automaton_index)
	^ "\n end (* " ^ (program.automata_names automaton_index) ^ " *)"
	^ "\n(**************************************************)"


(* Convert the automata into a string *)
let string_of_automata program =
	string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton program automaton_index
	) program.automata)

(* Convert an automaton into a string *)
let string_of_program program =
	string_of_header program
	^  "\n" ^ string_of_declarations program
	^  "\n" ^ string_of_automata program


(**************************************************)
(** States *)
(**************************************************)

(* Convert a location name into a string *)
(*let string_of_location_name program location =
	let string_array = List.map (fun automaton_index location_index ->
		program.automata_names.(automaton_index) ^ ": " ^ (program.location_names automaton_index location_index)
	) location in
	string_of_array_of_string_with_sep ", " string_array*)

(* Convert a state into a string *)
let string_of_state (location, linear_constraint) =
	let program = Program.get_program () in
	"" ^ (Automaton.string_of_location program.automata_names program.location_names program.variable_names location) ^ " ==> \n" ^ (LinearConstraint.string_of_linear_constraint program.variable_names linear_constraint) ^ "" 


(**************************************************)
(** Pi0 *)
(**************************************************)
(* Convert a pi0 into a string *)
let string_of_pi0 pi0 =
	let program = Program.get_program () in
	"  " ^ (
	string_of_list_of_string_with_sep "\n& " (
		List.map (fun parameter ->
			(program.variable_names parameter)
			^ " = "
			^ (NumConst.string_of_numconst (pi0 parameter))
		) program.parameters
	)
	)
