(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Convert an IMITATOR model to a .jpg (or PNG, PDF…) file generated using the dot utility
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2012/08/24
 *
 ************************************************************)


open OCamlUtilities
open Exceptions
open AbstractModel
open Result
open ImitatorUtilities


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


(* Add a header to the model *)
let string_of_header model =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	          "/************************************************************"
	^ "\n" ^" * File automatically generated by " ^ Constants.program_name ^ ""
	^ "\n" ^" * Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build)
	^ "\n" ^" * Model    : " ^ options#model_file_name ^ ""
	^ "\n" ^" * Generated: " ^ (now()) ^ ""
	^ "\n" ^" ************************************************************/"


(* Convert a sync into a string *)
let string_of_sync model action_index =
	match model.action_types action_index with
	| Action_type_sync -> (model.action_names action_index) ^ "\\n"
	| Action_type_nosync -> ""


(** Convert clock updates into a string *)
let string_of_clock_updates variable_names clock_updates =
	let sep = "\\n" in
	let wrap_reset variable_index =  (variable_names variable_index) ^ " := 0" in
	let wrap_expr variable_index linear_term = (variable_names variable_index)
			^ " := "
			^ (LinearConstraint.string_of_pxd_linear_term variable_names linear_term) in
	ModelPrinter.string_of_clock_updates_template variable_names clock_updates wrap_reset wrap_expr sep

(* Convert a list of discrete updates into a string *)
let string_of_discrete_updates variable_names discrete_updates =
	ModelPrinter.string_of_discrete_updates ~sep:"\\n" variable_names discrete_updates

(** Converts a list of conditional updates into a string *)
let string_of_conditional_updates variable_names conditional_updates =
	let wrap_if boolean_expr  = "if (" ^ (ModelPrinter.string_of_boolean_expression variable_names boolean_expr) ^  ") then\\n" in
	let wrap_else = "\\nelse\\n" in
	let wrap_end = "\\nend" in
	let sep = "\\n" in
	ModelPrinter.string_of_conditional_updates_template variable_names conditional_updates string_of_clock_updates string_of_discrete_updates wrap_if wrap_else wrap_end sep

(* Convert a transition of a location into a string *)
let string_of_transition model automaton_index source_location transition =
(* s_12 -> s_5 [label="bUp"]; *)
	let first_separator, second_separator = ModelPrinter.separator_comma transition.updates in
	"\n\t"
	(* Source *)
	^ (id_of_location automaton_index source_location)
	(* Destination *)
	^ " -> "
	^ (id_of_location automaton_index transition.target)

	^ " ["
	(* Color and style for sync label *)
	(* Check if the label is shared *)
	^ (if List.length (model.automata_per_action transition.action) > 1 then
		let color = color transition.action in
		"style=bold, color=" ^ color ^ ", "
		(* Check if this is a Action_type_nosync action: in which case dotted *)
		else match model.action_types transition.action with
			| Action_type_sync -> ""
			| Action_type_nosync -> "style=dashed, "
		)

	(* LABEL *)
	^ "label=\""
	(* Guard *)
	^ (
		if transition.guard <> AbstractModel.True_guard then
			(*** HACK: also check that the result is not "True" ***)
			let guard_string = ModelPrinter.string_of_guard model.variable_names transition.guard in
			if guard_string = LinearConstraint.string_of_true then "" else
			(escape_string_for_dot guard_string) ^ "\\n"
		else ""
		)
	(* Sync *)
	^ (string_of_sync model transition.action)
	(* Clock updates *)
	^ (string_of_clock_updates model.variable_names transition.updates.clock)
	(* Add a \n in case of both clocks and discrete *)
	^ (if first_separator then "\\n" else "")
	(* Discrete updates *)
	^ (string_of_discrete_updates model.variable_names transition.updates.discrete)
	(* Add a \n in case of both discrete and conditional updates *)
	^ (if second_separator then "\\n" else "")
	^ (string_of_conditional_updates model.variable_names transition.updates.conditional)
	^ "\"];"


(* Convert the transitions of a location into a string *)
let string_of_transitions model automaton_index location_index =
	print_message Verbose_high ("\n Entering string_of_transitions(automaton_index = " ^ (string_of_int automaton_index) ^ " , location_index = " ^ (string_of_int location_index) ^ ")…");

	let result =
	string_of_list_of_string (
	(* For each action *)
	List.map (fun action_index ->
		(* Get the list of transitions *)
		let transitions = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in
		(* Convert to string *)
		string_of_list_of_string (
			(* For each transition *)
			List.map (string_of_transition model automaton_index location_index) transitions
			)
		) (model.actions_per_location automaton_index location_index)

	)
	in
	result


(* Convert a flow into a string (only <> 1 flows are printed) *)
let string_of_flow model automaton_index location_index =
	string_of_list_of_string_with_sep ", " (List.map (fun (clock_index, constant_value) -> 
		(model.variable_names clock_index)
		^ "' = "
		^ (NumConst.string_of_numconst constant_value)
		)
		(model.flow automaton_index location_index)
	)



(* Convert a location of an automaton into a string *)
let string_of_location model automaton_index location_index =
	print_message Verbose_high "\n Entering string_of_location…";
	
	let is_accepting = model.is_accepting automaton_index location_index in
	let is_urgent = model.is_urgent automaton_index location_index in
	
	let location_color =
		match is_accepting, is_urgent with
		(* Accepting AND urgent: orange *)
		| true, true -> "orange"
		(* Accepting: red *)
		| true, false -> "red"
		(* Urgent: yellow *)
		| false, true -> "yellow"
		(* Normal: standard *)
		| false, false -> "paleturquoise2"
	in

	let result =
(* 	s_0[fillcolor=red, style=filled, shape=Mrecord, label="s_0|{InputInit|And111|Or111}"]; *)
	"\n"
	(* Id *)
	^ (id_of_location automaton_index location_index) ^ "["
	(* Color *)
	^ "fillcolor=" ^ location_color (*(color location_index)*) ^ ", style=filled, fontsize=16"
	(* LP: shape MRecord inhibits the peripheries display *)
	^ (if is_accepting then ", peripheries=2" else "")
	(* Label: start *)
	^ ", label=\""
	(* Label: accepting *)
	^ (if is_accepting then "acc |" else "")
	(* Label: urgency *)
	^ (if is_urgent then "U |" else "")
	(* Label: name *)
	^ (model.location_names automaton_index location_index)
	(* Label: invariant *)
	^ "|{" ^ (escape_string_for_dot (ModelPrinter.string_of_guard model.variable_names (model.invariants automaton_index location_index)))
	(* Label: stopwatches *)
	^ (if model.has_non_1rate_clocks then (
		let stopwatches = model.stopwatches automaton_index location_index in
		""
		^ (if stopwatches <> [] then "| stop " ^ string_of_list_of_variables model.variable_names stopwatches ^ "" else "")
		(*** TODO: better delimiter? ***)
		^ ""
		^ (if (model.flow automaton_index location_index) <> [] then "|" ^ (string_of_flow model automaton_index location_index) else "")
	) else "")

	(* The end *)
	^ "}\"];"

	(* Transitions *)
	^ (string_of_transitions model automaton_index location_index) (*problem here*)

(*
	^
	^ (match model.costs automaton_index location_index with
		| None -> ""
		| Some cost -> "[" ^ (LinearConstraint.string_of_linear_term model.variable_names cost) ^ "]"
	)
	^ ": "
	^ (string_of_invariant model automaton_index location_index)
	^ (string_of_transitions model automaton_index location_index)*)

	in
	print_message Verbose_high "Exiting string_of_location…";
	result


(* Convert the locations of an automaton into a string *)
let string_of_locations model automaton_index =
	print_message Verbose_high "\n Entering string_of_locations…";
	print_message Verbose_high ("List length" ^ (string_of_int (List.length (model.locations_per_automaton automaton_index) )));

	let result =
	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
(* 		print_message Verbose_high "Entering string_of_locations…2.1"; *)
		print_message Verbose_high ("automaton_index: " ^ string_of_int automaton_index ^ " location_index: " ^ string_of_int location_index);
		string_of_location model automaton_index location_index;
		(* print_message Verbose_high "Entering string_of_locations…2.2"; *)
	) (model.locations_per_automaton automaton_index))
	in
	print_message Verbose_high "Exiting string_of_locations…";
	result


(* Convert an automaton into a string *)
let string_of_automaton model automaton_index =

	print_message Verbose_high "\n Entering string_of_automaton…";

	(* Finding the initial location *)
	let inital_global_location  = model.initial_location in
	let initial_location = DiscreteState.get_location inital_global_location automaton_index in

	let t1 = "\n init" ^ (string_of_int automaton_index) ^ "[shape=none, label=\"" ^ (model.automata_names automaton_index) ^ "\"];" in
	let t2 = "\n init" ^ (string_of_int automaton_index) ^ " -> " ^ (id_of_location automaton_index initial_location) ^ ";" in
	let t3 = "\n/* automaton " ^ (model.automata_names automaton_index) ^ " */" in
	let t4 = "\n " ^ (string_of_locations model automaton_index) in (*problem here!*)

	print_message Verbose_high "Exiting string_of_automaton…";

	let result =

	"\n/**************************************************/"
	(* ^ "\n/* automaton " ^ (model.automata_names automaton_index) ^ " */" *)
	^ t3
	^ "\n/**************************************************/"

	(* Handling the initial arrow *)
	(* ^ "\n init" ^ (string_of_int automaton_index) ^ "[shape=none, label=\"" ^ (model.automata_names automaton_index) ^ "\"];" *)
	^ t1
	(* ^ "\n init" ^ (string_of_int automaton_index) ^ " -> " ^ (id_of_location automaton_index initial_location) ^ ";" *)
	^ t2

	(* Handling transitions *)
	(* ^ "\n " ^ (string_of_locations model automaton_index) *)
	^ t4
	^ "\n/**************************************************/"
	in
	print_message Verbose_high "Exiting string_of_automaton…";
	result


(* Convert the automata into a string *)
let string_of_automata model =
	(* Print some information *)
 	print_message Verbose_high "Entering string_of_automata…";

	(* Retrieve the input options *)
	let options = Input.get_options () in

	let vertical_string_of_list_of_variables variables =
		let variables = List.map model.variable_names variables in
		string_of_list_of_string_with_sep "\\n" variables
	in

	print_message Verbose_high "Before gathering result in string_of_automata…";

	let result =

	"\n/**************************************************/"
	^ "\n/* Starting general graph */"
	^ "\n/**************************************************/"
	^ "\n digraph G {\n"
	^ "\n node [shape=Mrecord, fontsize=12];"
(* 	^ "\n rankdir=LR" *)
	^ "\n"
	(* General information *)
(* 	s_0[fillcolor=red, style=filled, shape=Mrecord, label="s_0|{InputInit|And111|Or111}"]; *)
^ "\nname[shape=none, style=bold, fontsize=24, label=\"" ^ options#model_local_file_name ^ "\"];"
	^ "\ngeneral_info[shape=record, label=\"" (*Model|{*)
	^ "{" ^ (string_of_int (List.length model.clocks_without_special_reset_clock)) ^ " clock" ^ (s_of_int (List.length model.clocks_without_special_reset_clock)) ^ "|" ^ (vertical_string_of_list_of_variables model.clocks_without_special_reset_clock) ^ "}"
		^ "|{" ^ (string_of_int (List.length model.parameters)) ^ " parameter" ^ (s_of_int (List.length model.parameters)) ^ "|" ^ (vertical_string_of_list_of_variables model.parameters) ^ "}"
	^ (if model.discrete <> [] then
		"|{" ^ (string_of_int (List.length model.discrete)) ^ " discrete|" ^ (vertical_string_of_list_of_variables model.discrete) ^ "}"
		else "")
	^ "|{Initial|" ^ (escape_string_for_dot (LinearConstraint.string_of_px_linear_constraint model.variable_names model.initial_constraint)) ^ "}"
	^ "\"];"
		(* Version and generation time infos *)
		^ "\ngeneration[rotation=90.0, shape=rectangle, fontsize=10, label=\"Generated by " ^ (OCamlUtilities.escape_string_for_dot (ImitatorUtilities.program_name_and_version_and_nickname)) ^ "
Build: " ^ ImitatorUtilities.git_branch_and_hash ^ "
Generation time: " ^ (now()) ^ "\"];"
		(* To ensure the vertical ordering *)
	^ "\n name -> generation [color=white];"
	^ "\n generation -> general_info [color=white];"


	^ (
(*		(* Do not print the observer *)
		let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
		in*)
		string_of_list_of_string_with_sep "\n\n" (
			List.map (fun automaton_index -> string_of_automaton model automaton_index
		) model.automata)
	)
	^ "\n\n/**************************************************/"
	^ "\n/* Ending general graph */"
	^ "\n/**************************************************/"
	^ "\n}"
	in
	print_message Verbose_high "Exiting string_of_automata…";
	result

(* Convert an automaton into a string *)
let string_of_model model =
	string_of_header model
(* 	^  "\n" ^ string_of_declarations model *)
	^  "\n" ^ string_of_automata model
