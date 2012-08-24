(************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/08/24
 * Last modified: 2012/08/24
 *
 ************************************************************)


open Global
open AbstractModel


(************************************************************
 Global variable
************************************************************)
let id_transition = ref 0



(************************************************************
 Functions
************************************************************)

(* Create the array of dot colors *)
let dot_colors = Array.of_list Graphics.dot_colors

(* Coloring function for each thing *)
let color = fun index ->
	(* If more colors than our array: white *)
	try dot_colors.(index) with Invalid_argument _ -> "white"


let id_of_location automaton_index location_index =
	"s_" ^ (string_of_int automaton_index) ^ "_" ^ (string_of_int location_index)

let string_of_list_of_variables variable_names variables =
	let variables = List.map variable_names variables in
	string_of_list_of_string_with_sep "," variables

let escape_string_for_dot str =
	(** BUG: cannot work with global replace *)
(*		Str.global_substitute (Str.regexp ">\\|&") (fun s -> if s = ">" then "\\>" else if s = "&" then "\\&" else s)
			str*)
(* 		Str.global_replace (Str.regexp "\\(>\\|&\\)") ("\\" ^ "\\(\\1\\)") *)
	Str.global_replace (Str.regexp "\n") (" \\n ")
		(Str.global_replace (Str.regexp ">") ("\\>")
			(Str.global_replace (Str.regexp "&") ("\\&") str)
		)

	
	
	
(* Add a header to the program *)
let string_of_header program =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	          "/************************************************************"
	^ "\n" ^" * Program " ^ options#file
	^ "\n" ^" * Converted by " ^ program_name ^ " " ^ version_string
(* 	^ "\n" ^" * Generated at time " ^ time? *)
	^ "\n" ^" ************************************************************/"
	

(* Convert a sync into a string *)
let string_of_sync program action_index =
	match program.action_types action_index with
	| Action_type_sync -> (program.action_names action_index) ^ "\\n"
	| Action_type_nosync -> ""


let string_of_clock_updates program = function
	| No_update -> ""
	| Resets list_of_clocks -> 
		string_of_list_of_string_with_sep "\\n" (List.map (fun variable_index ->
			(program.variable_names variable_index)
			^ ":=0"
		) list_of_clocks)
	| Updates list_of_clocks_lt -> 
		string_of_list_of_string_with_sep "\\n" (List.map (fun (variable_index, linear_term) ->
			(program.variable_names variable_index)
			^ ":="
			^ (LinearConstraint.string_of_linear_term program.variable_names linear_term)
		) list_of_clocks_lt)

	
(* Convert a list of updates into a string *)
let string_of_updates program updates =
	string_of_list_of_string_with_sep "\\n" (List.map (fun (variable_index, linear_term) ->
		(* Convert the variable name *)
		(program.variable_names variable_index)
		^ ":="
		(* Convert the linear_term *)
		^ (LinearConstraint.string_of_linear_term program.variable_names linear_term)
	) updates)


(* Convert a transition of a location into a string *)
let string_of_transition program automaton_index source_location action_index (guard, clock_updates, discrete_updates, destination_location) =
(* s_12 -> s_5 [label="bUp"]; *)
	"\n\t"
	(* Source *)
	^ (id_of_location automaton_index source_location)
	(* Destination *)
	^ " -> "
	^ (id_of_location automaton_index destination_location)
	
	^ " ["
	(* Color for sync label *)
	(* Check if the label is shared *)
	^ (if List.length (program.automata_per_action action_index) > 1 then
		let color = color action_index in
		"style=bold, color=" ^ color ^ ", "
		else "")
	(* LABEL *)
	^ "label=\""
	(* Guard *)
	^ (
		if not (LinearConstraint.is_true guard) then
			(escape_string_for_dot (LinearConstraint.string_of_linear_constraint program.variable_names guard)) ^ "\\n"
		else ""
		)
	(* Sync *)
	^ (string_of_sync program action_index)
	(* Clock updates *)
	^ (string_of_clock_updates program clock_updates)
	(* Add a \n in case of both clocks and discrete *)
	^ (if clock_updates != No_update && discrete_updates != [] then "\\n" else "")
	(* Discrete updates *)
	^ (string_of_updates program discrete_updates)
	^ "\"];"
	
	(* Convert the guard *)
(* 	^ (LinearConstraint.string_of_linear_constraint program.variable_names guard) *)
	(* Convert the updates *)
(* 	^ " do {" *)
(* 	^ (string_of_clock_updates program clock_updates) *)
	
	
(*	^ (string_of_updates program discrete_updates)
	^ "} "
	(* Convert the sync *)
	^ (string_of_sync program action_index)
	(* Convert the destination location *)
	^ " goto " ^ (program.location_names automaton_index destination_location)
	^ ";"*)


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
			List.map (string_of_transition program automaton_index location_index action_index) transitions
			)
		) (program.actions_per_location automaton_index location_index)
	)


(* Convert a location of an automaton into a string *)
let string_of_location program automaton_index location_index =	
(* 	s_0[fillcolor=red, style=filled, shape=Mrecord, label="s_0|{InputInit|And111|Or111}"]; *)
	"\n"
	(* Id *)
	^ (id_of_location automaton_index location_index) ^ "["
	(* Color *)
	^ "fillcolor=" ^ "paleturquoise2" (*(color location_index)*) ^ ", style=filled, fontsize=16"
	(* Label: name *)
	^ ", label=\"" ^ (program.location_names automaton_index location_index)
	(* Label: invariant *)
	^ "|{" ^ (escape_string_for_dot (LinearConstraint.string_of_linear_constraint program.variable_names (program.invariants automaton_index location_index)))
	(* Label: stopwatches *)
	^ (if program.has_stopwatches then (
		let stopwatches = program.stopwatches automaton_index location_index in
		"|" ^
		(if stopwatches != [] then "stop " ^ string_of_list_of_variables program.variable_names stopwatches else "")
	) else "")
	(* The end *)
	^ "}\"];"
	
	(* Transitions *)
	^ (string_of_transitions program automaton_index location_index)
	
(*	
	^ 
	^ (match program.costs automaton_index location_index with
		| None -> ""
		| Some cost -> "[" ^ (LinearConstraint.string_of_linear_term program.variable_names cost) ^ "]"
	)
	^ ": "
	^ (string_of_invariant program automaton_index location_index)
	^ (string_of_transitions program automaton_index location_index)*)


(* Convert the locations of an automaton into a string *)
let string_of_locations program automaton_index =
	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		string_of_location program automaton_index location_index
	) (program.locations_per_automaton automaton_index))


(* Convert an automaton into a string *)
let string_of_automaton program automaton_index =
	(* Finding the initial location *)
	let inital_global_location  = program.initial_location in
	let initial_location = Automaton.get_location inital_global_location automaton_index in

	"\n/**************************************************/"
	^ "\n/* automaton " ^ (program.automata_names automaton_index) ^ " */"
	^ "\n/**************************************************/"
	
	(* Handling the initial arrow *)
	^ "\n init" ^ (string_of_int automaton_index) ^ "[shape=none, label=\"" ^ (program.automata_names automaton_index) ^ "\"];"
	^ "\n init" ^ (string_of_int automaton_index) ^ " -> " ^ (id_of_location automaton_index initial_location) ^ ";"

	(* Handling transitions *)
	^ "\n " ^ (string_of_locations program automaton_index)
	^ "\n/**************************************************/"


(* Convert the automata into a string *)
let string_of_automata program =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	let vertical_string_of_list_of_variables variables =
		let variables = List.map program.variable_names variables in
		string_of_list_of_string_with_sep "\\n" variables
	in

	
	"\n/**************************************************/"
	^ "\n/* Starting general graph */"
	^ "\n/**************************************************/"
	^ "\n digraph G {\n"
	^ "\n node [shape=Mrecord, fontsize=12];"
(* 	^ "\n rankdir=LR" *)
	^ "\n"
	(* General information *)
(* 	s_0[fillcolor=red, style=filled, shape=Mrecord, label="s_0|{InputInit|And111|Or111}"]; *)
	^ "\nname[shape=none, style=bold, fontsize=24, label=\"" ^ options#file ^ "\"];"
	^ "\ngeneral_info[shape=record, label=\"" (*Model|{*)
	^ "{Clocks|" ^ (vertical_string_of_list_of_variables program.clocks) ^ "}"
	^ "|{Parameters|" ^ (vertical_string_of_list_of_variables program.parameters) ^ "}"
	^ (if program.discrete != [] then
		"|{Discrete|" ^ (vertical_string_of_list_of_variables program.discrete) ^ "}"
		else "")
	^ "|{Initial|" ^ (escape_string_for_dot (LinearConstraint.string_of_linear_constraint program.variable_names program.initial_constraint)) ^ "}"
	^ "\"];" (*}*)
	(* To ensure that the name is above general info *)
	^ "\n name -> general_info [color=white];"

	
	^ (string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton program automaton_index
	) program.automata))
	^ "\n\n/**************************************************/"
	^ "\n/* Ending general graph */"
	^ "\n/**************************************************/"
	^ "\n}"

(* Convert an automaton into a string *)
let string_of_program program =
	string_of_header program
(* 	^  "\n" ^ string_of_declarations program *)
	^  "\n" ^ string_of_automata program


(**************************************************)
(** Pi0 *)
(**************************************************)
(* Convert a pi0 into a string *)
let string_of_pi0 program pi0 =
	"  " ^ (
	string_of_list_of_string_with_sep "\n& " (
		List.map (fun parameter ->
			(program.variable_names parameter)
			^ " = "
			^ (NumConst.string_of_numconst (pi0 parameter))
		) program.parameters
	)
	)



(**************************************************************)
(* Result *)
(**************************************************************)
(*(** Constraint returned by the inverse method *)
type returned_constraint =
	(** Constraint under convex form *)
	| Convex_constraint of LinearConstraint.linear_constraint
	(** Disjunction of constraints *)
	| Union_of_constraints of LinearConstraint.linear_constraint list*)
let string_of_returned_constraint variable_names = function 
	| Convex_constraint linear_constraint -> LinearConstraint.string_of_linear_constraint variable_names linear_constraint
	(** Disjunction of constraints *)
	| Union_of_constraints k_list -> string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_linear_constraint variable_names) k_list)

