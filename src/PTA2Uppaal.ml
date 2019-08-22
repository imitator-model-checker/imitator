(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 *
 * Module description: Translater to Uppaal
 *
 * File contributors : Étienne André
 * Created           : 2019/03/01
 * Last modified     : 2019/05/29
 *
 ************************************************************)

open OCamlUtilities
open Result
open AbstractModel
open ImitatorUtilities
open LinearConstraint


(************************************************************)
(** Customized values for constraint conversion *)
(************************************************************)

let uppaal_strings : LinearConstraint.customized_string = {
	true_string   = "true";
	false_string  = "false";
	and_operator  = " &amp;&amp; ";
	or_operator   = " or "; (* useless *)
	l_operator    = " &lt; ";
	le_operator   = " &lt;= ";
	eq_operator   = " == ";
	ge_operator   = " &gt;= ";
	g_operator    = " &gt; ";
}

let uppaal_update_separator = ", "

let uppaal_assignment = " = "


(* Positining *)
let scaling_factor = 200



(************************************************************)
(** Header *)
(************************************************************)

(* Add a header to the model *)
let string_of_header model =
	"<nta>"



(************************************************************)
(** Footer *)
(************************************************************)

(* End of the file *)
let footer = ""
(*	^ "\n" ^ "/*************************************************************"
	^ "\n" ^ " * The end *"
	^ "\n" ^ " ************************************************************/"*)
	^ "</nta>"
	^ "\n" ^ ""


(************************************************************)
(** Variable declarations *)
(************************************************************)

(* Convert a var_type into a string *)
let string_of_var_type = function
	| Var_type_clock -> "clock"
	| Var_type_discrete -> "int"
	| Var_type_parameter -> "parameter"


(* Naming the discrete variables checking for strong broadcast *)
let string_of_nb_strongbroadcast model action_index =
	(* Get name *)
	let action_name = model.action_names action_index in
	(* Name it *)
	"nb__" ^ action_name


(* Create initial discrete declarations to encode strong broadcast *)
let string_of_discrete_nb_strongbroadcast model actions_and_nb_automata =
	(* Do not generate anything for empty list *)
	if actions_and_nb_automata = [] then ""
	else
		(* Comment *)
		"\n\n/* Discrete variable declarations needed to encode IMITATOR's strong broadcast into Uppaal */\n"
		^
		(* Initialize *)
		(string_of_list_of_string_with_sep "\n" (List.map (fun (action_index , nb_automata) ->
			let discrete_name = string_of_nb_strongbroadcast model action_index in
			"int " ^ discrete_name ^ " = " ^ (string_of_int nb_automata) ^ ";"
		) actions_and_nb_automata))



(* Convert the initial clocks declarations into a string *)
let string_of_clocks model =
	let string_of_variables list_of_variables =
		string_of_list_of_string_with_sep ", " (List.map model.variable_names list_of_variables)
	in
	if model.nb_clocks > 0 then
		("\n/* Clocks declarations */\nclock " ^ (string_of_variables model.clocks_without_special_reset_clock) ^ ";")
		else ""

(* Convert the initial discrete declarations into a string *)
let string_of_discrete model =
	(* Some comment *)
	if model.nb_discrete > 0 then(
		"\n\n/* Discrete variables declarations (WARNING: these variables can be rational-valued in IMITATOR, but they become integer-valued in Uppaal) */"
		^
		(string_of_list_of_string_with_sep "\n"
			(List.map (fun discrete_index ->
				(* Get the name *)
				let discrete_name = model.variable_names discrete_index in

				(* Get the initial value *)
				let inital_global_location  = model.initial_location in
				let initial_value = Location.get_discrete_value inital_global_location discrete_index in

				(* Assign *)
				"\nint " ^ discrete_name ^ " = " ^ (NumConst.string_of_numconst initial_value) ^ ";"
			) model.discrete
			)
		)

	) else ""


(* Convert the parameter declarations into a string *)
let string_of_parameters model =
	(* Some comment *)
	if model.nb_parameters > 0 then(
		"\n\n/* Parameters declarations */\n"
		^
		(string_of_list_of_string_with_sep "\n"
			(List.map (fun parameter_index ->
				(* Get the name *)
				let parameter_name = model.variable_names parameter_index in

				(* Assign *)
				(*** NOTE: assign arbitrarily to 0 to allow Uppaal compiling ***)
				"const int " ^ parameter_name ^ " = 0 /* TODO: add your favorite value here */;"
			) model.parameters
			)
		)

	) else ""

(* Convert the declared actions into a string *)
let string_of_declared_actions model =
	"\n\n/* Action declarations */\n"
	^
	(string_of_list_of_string_with_sep "\n"
		(List.map (fun action_index ->
			(* Do not declare silent actions *)
			match model.action_types action_index with
			| Action_type_nosync -> ""
			| Action_type_sync ->
			(* Get name *)
			let action_name = model.action_names action_index in

			(* Get number of automata *)
			let nb_automata = List.length (model.automata_per_action action_index) in

			(* Case action unused: drop *)
			if nb_automata = 0 then "/* action " ^ action_name ^ " is unused in the model */"

			(* For action in a single automaton, we use Uppaal "broadcast chan" system *)
			else if nb_automata = 1 then "broadcast chan " ^ action_name ^ ";"

			(* For action in exactly two automata, we use Uppaal standard "chan" system *)
			else if nb_automata = 2 then "chan " ^ action_name ^ ";"

			(* For action in exactly > 2 automata, we use broadcast with a special encoding *)
			else (
(*				(* Issue a warning *)
				print_warning ("Action '" ^ action_name ^ "' is used in " ^ (string_of_int nb_automata) ^ " automata: IMITATOR uses strong broadcast semantics, while Uppaal uses broadcast semantics; the behavior may differ!");*)
				"broadcast chan " ^ action_name ^ "; /* This action is used in " ^ (string_of_int nb_automata) ^ " automata: IMITATOR uses strong broadcast semantics, while Uppaal uses broadcast semantics; the correctness is ensured thanks to variable '" ^ (string_of_nb_strongbroadcast model action_index) ^ "' */"
			)

		) model.actions
		)
	)



(* Convert the initial variable declarations into a string *)
let string_of_declarations model actions_and_nb_automata =
	(* Header *)
	"<declaration>"

	^
	(* Some comments *)
	let options = Input.get_options () in
	          "\n/************************************************************"
	^ "\n" ^ " * File automatically generated by " ^ Constants.program_name ^ ""
	^ "\n" ^ " * Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build())
	^ "\n" ^ " * Git      : " ^ (ImitatorUtilities.git_branch_and_hash)
	^ "\n" ^ " * Model    : '" ^ options#model_input_file_name ^ "'"
	^ "\n" ^ " * Generated: " ^ (now()) ^ ""
	^ "\n" ^ " ************************************************************/"

	(* Declare clocks *)
	^ (string_of_clocks model)

	(* Declare discrete *)
	^ (string_of_discrete model)

	(* Declare discrete needed to encode strong broadcast *)
	^ (string_of_discrete_nb_strongbroadcast model actions_and_nb_automata)

	(* Declare parameters *)
	^ (string_of_parameters model)

	(* Declare actions *)
	^ (string_of_declared_actions model)


	(*** TODO: get the initial value of clocks from the initial constraint and, if not 0, then issue a warning ***)


	(* The initial constraint (in comment only) *)
	^ "\n" ^ ""
	^ "\n" ^ "\t/*------------------------------------------------------------*/"
	^ "\n" ^ "\t/* Initial constraint (not interpreted by Uppaal)             */"
	^ "\n" ^ "\t/*------------------------------------------------------------*/"
	^ "\n\t /* " ^ (LinearConstraint.customized_string_of_px_linear_constraint uppaal_strings model.variable_names model.initial_constraint) ^ " */"


	(* Footer *)
	^ "\n</declaration>"



(************************************************************)
(** Guard *)
(************************************************************)

(*** NOTE: special handling as we have a discrete and a continuous guard that must be handled homogeneously ***)

(** Convert a guard into a string *)
let string_of_guard actions_and_nb_automata variable_names x_coord_str y_coord_str = function
	(* True guard = no guard *)
	| True_guard -> ""

	(* False *)
	| False_guard ->
		"<label kind=\"guard\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ y_coord_str ^ "\">false</label>"


	(*** TODO: use the proper Uppaal syntax here ***)

	| Discrete_guard discrete_guard ->

		(*** NOTE/BUG: remove the true discrete guard! (not accepted by Uppaal) ***)

		"<label kind=\"guard\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ y_coord_str ^ "\">" ^ (LinearConstraint.customized_string_of_d_linear_constraint uppaal_strings variable_names discrete_guard) ^ "</label>"

	| Continuous_guard continuous_guard ->
		(* Remove true guard *)

		if LinearConstraint.pxd_is_true continuous_guard then "" else
		"<label kind=\"guard\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ y_coord_str ^ "\">" ^ (LinearConstraint.customized_string_of_pxd_linear_constraint uppaal_strings variable_names continuous_guard) ^ "</label>"

	| Discrete_continuous_guard discrete_continuous_guard ->
		"<label kind=\"guard\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ y_coord_str ^ "\">" ^ (
			(LinearConstraint.customized_string_of_d_linear_constraint uppaal_strings variable_names discrete_continuous_guard.discrete_guard)
			^
			(
				(* Remove true guard *)
				if LinearConstraint.pxd_is_true discrete_continuous_guard.continuous_guard then ""
				else uppaal_strings.and_operator ^ (LinearConstraint.customized_string_of_pxd_linear_constraint uppaal_strings variable_names discrete_continuous_guard.continuous_guard)
			)
		) ^ "</label>"




(************************************************************)
(** Automata *)
(************************************************************)


(* Creates a unique id for each location *)
let id_of_location model automaton_index location_index =
	(* Return an id of the form id_pta0_loc0 *)
	"id_pta" ^ (string_of_int automaton_index) ^ "_loc" ^ (string_of_int location_index ^ "")


(* Convert the invariant of a location into a string *)
let string_of_invariant model actions_and_nb_automata automaton_index location_index =
	(* In addition to the "real" invariant, we also constrain the variables corresponding to the strong broadcast actions to be equal to the number of automata that declare them *)
	(*** NOTE: of course, it would be better to only add these invariants to the locations target of such an action ***)
	(*** TODO: simplify some day ***)
	let strong_broadcast_invariant =
		(string_of_list_of_string_with_sep uppaal_strings.and_operator (List.map (fun (action_index , nb_automata) ->
			let discrete_name = string_of_nb_strongbroadcast model action_index in
			discrete_name ^ uppaal_strings.eq_operator ^ (string_of_int nb_automata)
		) actions_and_nb_automata))
	in

	(*** TODO: check well formed with constraints x <= … as requested by Uppaal, and issue a warning otherwise ***)

	let invariant = LinearConstraint.customized_string_of_pxd_linear_constraint uppaal_strings model.variable_names (model.invariants automaton_index location_index) in

	(* Avoid "true and …" *)
	let invariant_and_strong_broadcast_invariant =
		if invariant = uppaal_strings.true_string then strong_broadcast_invariant
		else if actions_and_nb_automata = [] then invariant
		else invariant ^ uppaal_strings.and_operator ^ strong_broadcast_invariant
	in

	(* Invariant *)
	(*** NOTE: arbitrary positioning (location_id * scaling_factor, +20%) ***)
	"\n\t<label kind=\"invariant\" x=\"" ^ (string_of_int (location_index * scaling_factor)) ^ "\" y=\"" ^ (string_of_int (scaling_factor / 5)) ^ "\">"
	^ invariant_and_strong_broadcast_invariant

	(* The end *)
	^ "</label>"





let string_of_clock_updates model = function
	| No_update -> ""
	| Resets list_of_clocks ->
		string_of_list_of_string_with_sep ", " (List.map (fun variable_index ->
			(model.variable_names variable_index)
			^ " = 0"
		) list_of_clocks)
	| Updates list_of_clocks_lt ->
		string_of_list_of_string_with_sep ", " (List.map (fun (variable_index, linear_term) ->
			(model.variable_names variable_index)
			^ " = "
			^ (LinearConstraint.string_of_pxd_linear_term model.variable_names linear_term)
		) list_of_clocks_lt)

(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
let string_of_arithmetic_expression variable_names =
	let rec string_of_arithmetic_expression = function
		(* Shortcut: Remove the "+0" / -"0" cases *)
		| DAE_plus (discrete_arithmetic_expression, DT_factor (DF_constant c))
		| DAE_minus (discrete_arithmetic_expression, DT_factor (DF_constant c)) when NumConst.equal c NumConst.zero ->
			string_of_arithmetic_expression discrete_arithmetic_expression

		| DAE_plus (discrete_arithmetic_expression, discrete_term) ->
			(string_of_arithmetic_expression discrete_arithmetic_expression)
			^ " + "
			^ (string_of_term discrete_term)

		| DAE_minus (discrete_arithmetic_expression, discrete_term) ->
			(string_of_arithmetic_expression discrete_arithmetic_expression)
			^ " - "
			^ (string_of_term discrete_term)

		| DAE_term discrete_term -> string_of_term discrete_term

	and string_of_term = function
		(* Eliminate the '1' coefficient *)
		| DT_mul (DT_factor (DF_constant c), discrete_factor) when NumConst.equal c NumConst.one ->
			string_of_factor discrete_factor
		(* No parentheses for constant * variable *)
		| DT_mul (DT_factor (DF_constant c), DF_variable v) ->
			(string_of_factor (DF_constant c))
			^ " * "
			^ (string_of_factor (DF_variable v))
		(*** TODO: No parentheses on the left for constant or variable * something ***)
		(* Otherwise: parentheses on the left *)
		| DT_mul (discrete_term, discrete_factor) ->
			"(" ^ (string_of_term discrete_term) ^ ")"
			^ " * "
			^ (string_of_factor discrete_factor)

		(*** TODO: No parentheses on the left for constant or variable / something ***)
		(*** TODO: No parentheses on the left for something / constant or variable ***)
		(* Otherwise: parentheses on the left *)
		| DT_div (discrete_term, discrete_factor) ->
			"(" ^ (string_of_term discrete_term) ^ ")"
			^ " / "
			^ (string_of_factor discrete_factor)

		| DT_factor discrete_factor -> string_of_factor discrete_factor

	and string_of_factor = function
		| DF_variable discrete_index -> variable_names discrete_index
		| DF_constant discrete_value -> NumConst.string_of_numconst discrete_value
		| DF_expression discrete_arithmetic_expression ->
			(*** TODO: simplify a bit? ***)
			"(" ^ (string_of_arithmetic_expression discrete_arithmetic_expression) ^ ")"
	(* Call top-level *)
	in string_of_arithmetic_expression



(* Convert a list of updates into a string *)
let string_of_discrete_updates model updates =
	string_of_list_of_string_with_sep uppaal_update_separator (List.map (fun (variable_index, arithmetic_expression) ->
		(* Convert the variable name *)
		(model.variable_names variable_index)
		^ uppaal_assignment
		(* Convert the arithmetic_expression *)
		^ (string_of_arithmetic_expression model.variable_names arithmetic_expression)
	) updates)


let string_of_updates model automaton_index action_index x_coord_str y_coord_str clock_updates discrete_updates =

	(* First add the update for the strong broadcast encoding *)

	(* Get number of automata *)
	let automata_for_this_action = model.automata_per_action action_index in
	let nb_automata = List.length automata_for_this_action in
	(* If strong broadcast encoding (i.e. >= 3) *)
	let update_strong_broadcast =
	if nb_automata >= 3 then
		(* Get discrete name *)
		let discrete_name = string_of_nb_strongbroadcast model action_index in
			discrete_name
			^
			(* Arbitrarily, the first automaton index in the list is "!" and therefore responsible for resetting the number, and the others are "?", and therefore they increment *)
			if automaton_index = List.nth automata_for_this_action 0
				(* := 1 *)
				then uppaal_assignment ^ (string_of_int 1)
				(* ++ *)
				else uppaal_assignment ^ discrete_name ^ " + 1"
	(* Otherwise, no update *)
	else ""
	in

	(* Check for emptiness of some updates *)
	let no_clock_updates =
		clock_updates = No_update || clock_updates = Resets [] || clock_updates = Updates []
	in
	let no_discrete_updates = discrete_updates = [] in

	(* If no update at all: empty string *)
	if no_clock_updates && no_discrete_updates && update_strong_broadcast = "" then ""

	else(
		(* Manage separator between clock updates and discrete updates *)
		let separator_clock_discrete =
			if no_clock_updates || no_discrete_updates then "" else uppaal_update_separator
		in

		(* Manage separator between the normal updates (clocks and discrete), and the strong broadcast updates *)
		let separator_clockdiscrete_strongbroadcast =
			if no_clock_updates && no_discrete_updates || update_strong_broadcast = "" then "" else uppaal_update_separator
		in

		"<label kind=\"assignment\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ y_coord_str ^ "\">"
		(* Clock updates *)
		^ (string_of_clock_updates model clock_updates)
		(* Add a separator in case of both clocks and discrete *)
		^ separator_clock_discrete
		(* Discrete updates *)
		^ (string_of_discrete_updates model discrete_updates)
		(* Add separator *)
		^ separator_clockdiscrete_strongbroadcast
		(* Special strong broadcast update *)
		^ update_strong_broadcast
		^ "</label>"
	)


(* Convert an action_index in automaton_index into a string; the automaton_index is important for "!" / "?" issues *)
let string_of_sync model automaton_index action_index =
	let action_name = model.action_names action_index in

	(* Different models of synchronization depending on the number of synchronizations *)

	(* Get number of automata *)
	let automata_for_this_action = model.automata_per_action action_index in
	let nb_automata = List.length automata_for_this_action in

	(* Case action unused: should not happen (but keep just in case) *)
	if nb_automata = 0 then(
		print_warning ("Action '" ^ action_name ^ "' seems to be unused in the model, so this transition is strange. This may be an error of the translator, in which case you may want to contact us.");
		(* Arbitrarily use it sending *)
		action_name ^ "!"
	)

	(* For action in a single automaton, we use Uppaal "broadcast chan" system => action "!" *)
	else if nb_automata = 1 then action_name ^ "!"

	(*** NOTE: duplicate code below; but since it is a bit a different framework, let us keep so, so far ***)

	(* For action in exactly two automata, we use Uppaal standard "chan" system *)
	else if nb_automata = 2 then(
		action_name
		^
		(* Arbitrarily, the first automaton index in the list is "!", and the other one is "?" *)
		if automaton_index = List.nth automata_for_this_action 0 then "!" else "?"
	)

	(* For action in >= 3 automata, we again use Uppaal "broadcast chan" system => action "!", with additional variables on updates/guard to ensure good behavior *)
	else
		action_name
		^
		(* Again, arbitrarily, the first automaton index in the list is "!", and the others are "?" *)
		if automaton_index = List.nth automata_for_this_action 0 then "!" else "?"



(* Convert a transition of a location into a string *)
(** TODO: Add conditions to the translation *)
let string_of_transition model actions_and_nb_automata automaton_index source_location transition =
	let clock_updates = transition.updates.clock in
	let discrete_updates = transition.updates.discrete in
	(* Arbitrary positioning: x = between source_location and target_location *)
	(*** NOTE: integer division here, so first multiplication, then division (otherwise result can be 0) ***)
	let x_coord_str = (string_of_int ((source_location + transition.target) * scaling_factor / 2)) in

	(* Header *)
	"\n\t<transition>"

	(* Source *)
	^ "\n\t\t<source ref=\"" ^ (id_of_location model automaton_index source_location) ^ "\"/>"

	(* Target *)
	^ "\n\t\t<target ref=\"" ^ (id_of_location model automaton_index transition.target) ^ "\"/>"

	(* Synchronisation label *)
	^ (
		match model.action_types transition.action with
			| Action_type_sync -> "\n\t\t<label kind=\"synchronisation\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ (string_of_int (scaling_factor * 2 / 5)) ^ "\">" ^ (string_of_sync model automaton_index transition.action) ^ "</label>"
			| Action_type_nosync -> ""
	)

	(* Guard *)
	^ (
		(* Quite arbitrary positioning *)
		let y_coord_str = (string_of_int (scaling_factor / 5)) in
		"\n\t\t" ^ (string_of_guard actions_and_nb_automata model.variable_names x_coord_str y_coord_str transition.guard)
	)

	(* Updates *)
	^ (
		(* Quite arbitrary positioning *)
		let y_coord_str = (string_of_int (- scaling_factor / 5)) in
		"\n\t\t" ^ (string_of_updates model automaton_index transition.action x_coord_str y_coord_str clock_updates discrete_updates)
	)

	(* Footer *)
	^ "\n\t</transition>"



(* Convert the transitions of an automaton into a string *)
let string_of_transitions model actions_and_nb_automata automaton_index =
	string_of_list_of_string (
	(* For each location *)
	List.map (fun location_index ->
		string_of_list_of_string (
		(* For each action *)
		List.map (fun action_index ->
			(* Get the list of transitions *)
			let transitions = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in
			(* Convert to string *)
			string_of_list_of_string (
				(* For each transition *)
				List.map (string_of_transition model actions_and_nb_automata automaton_index location_index) transitions
				)
			) (model.actions_per_location automaton_index location_index)
		)
	) (model.locations_per_automaton automaton_index))



(* Convert a location of an automaton into a string *)
let string_of_location model actions_and_nb_automata automaton_index location_index =
	"\n"

	(* Header *)
	^ "<location id=\"" ^ (id_of_location model automaton_index location_index) ^ "\" "
	(*** NOTE: arbitrary positioning at (location_id * scaling_factor, 0) ***)
	^ "x=\"" ^ (string_of_int (location_index * scaling_factor)) ^ "\" y=\"0\""
	(* Add yellow color if urgent :-) *)
	^ (if model.is_urgent automaton_index location_index then " color=\"#ffff00\"" else "")
	^ ">"

	(* Name *)
	(*** NOTE: arbitrary positioning at (location_id * scaling_factor, -20%) ***)
	^ "\n\t<name x=\"" ^ (string_of_int (location_index * scaling_factor)) ^ "\" y=\"" ^ (string_of_int (- scaling_factor / 5)) ^ "\">" ^ (model.location_names automaton_index location_index) ^ "</name>"

	(* Invariant *)
	^ (string_of_invariant model actions_and_nb_automata automaton_index location_index)

	(* Urgency *)
	^ (if model.is_urgent automaton_index location_index then "<urgent/>" else "")

	(* Stopwatches *)
	(*** TODO ***)

	(* Costs: dropped for now *)
	(*** TODO ***)
	 ^ (match model.costs automaton_index location_index with
		| None -> ""
		| Some cost ->
			(* Issue warning *)
			print_warning ("Cost '" ^ (LinearConstraint.string_of_p_linear_term model.variable_names cost) ^ "' is not supported in the translation to Uppaal.");
			"\n/* Cost '" ^ (LinearConstraint.string_of_p_linear_term model.variable_names cost) ^ "' is not supported in the translation to Uppaal. */\n"
	)

	(* Footer *)
	^ "</location>"


(* Convert the locations of an automaton into a string *)
let string_of_locations model actions_and_nb_automata automaton_index =
	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		string_of_location model actions_and_nb_automata automaton_index location_index
	) (model.locations_per_automaton automaton_index))

(* Convert the initial location of an automaton *)
let string_of_initial_location model automaton_index =
	(* Get initial location *)
	let inital_global_location  = model.initial_location in
	let initial_location = Location.get_location inital_global_location automaton_index in
	"<init ref=\"" ^ (id_of_location model automaton_index initial_location) ^ "\"/>"

(* Convert an automaton into a string *)
let string_of_automaton model actions_and_nb_automata automaton_index =
		(*** NOTE: arbitrary positioning at (automaton_index, automaton_index) ***)
(* 	"\n/************************************************************/" *)
	(*^*) "\n<template><name x=\"" ^ (string_of_int automaton_index) ^ "\" y=\"" ^ (string_of_int automaton_index) ^ "\">" ^ (model.automata_names automaton_index) ^ "</name><declaration>// No local declaration for automaton '" ^ (model.automata_names automaton_index) ^ "'
</declaration>"
	^ "\n " ^ (string_of_locations model actions_and_nb_automata automaton_index)
	^ "\n " ^ (string_of_initial_location model automaton_index)
	^ "\n " ^ (string_of_transitions model actions_and_nb_automata automaton_index)
	^ "\n </template>" (*/ * end " ^ (model.automata_names automaton_index) ^ " */ *)
(* 	^ "\n/************************************************************/" *)


(* Convert the automata into a string *)
let string_of_automata model actions_and_nb_automata =
	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in

	(* Print all (other) PTA *)
	string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton model actions_and_nb_automata automaton_index
	) pta_without_obs)



(************************************************************)
(** System *)
(************************************************************)
(* Create the system definition *)
let string_of_system model =
	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in
	(* Open *)
	"<system>"
	(* Comment *)
	^ "\n// List one or more processes to be composed into a system."
	(* System definition *)
	^ "\n\nsystem " ^ (string_of_list_of_string_with_sep ", " (List.map model.automata_names pta_without_obs)) ^ ";"
	(* Close *)
	^ "\n</system>"



(*** TODO: add properties, projection… as comments ***)



(************************************************************)
(** Model *)
(************************************************************)

(* Convert the model into a string *)
let string_of_model model =
	(* Create a list (action_index , nb_automata for this action), needed for strong broadcast encoding *)
	let actions_and_nb_automata = List.map (fun action_index ->
		(* Get number of automata *)
		let nb_automata = List.length (model.automata_per_action action_index) in
		(* Make it a pair *)
		action_index , nb_automata
	) model.actions
	in
	(* Strong broadcast is encoded to this scheme if the number of automata is >= 3, so filter *)
	let encoding_needed = List.filter (fun (_ , nb_automata) -> nb_automata >= 3) actions_and_nb_automata in

	(* The header *)
	string_of_header model

	(* The variable declarations *)
	^  "\n" ^ (string_of_declarations model encoding_needed)

	(* All automata *)
	^  "\n" ^ (string_of_automata model encoding_needed)

	(* The system definition *)
	^  "\n" ^ (string_of_system model)

	(*** TODO ***)
	(* The property *)
(*	^ property_header
	^  "\n" ^ string_of_property model model.user_property*)

	(*** TODO ***)
	(* The projection *)
(* 	^  "\n" ^ string_of_projection model *)

	(*** TODO ***)
	(* The optimization *)
(* 	^  "\n" ^ string_of_optimization model *)

	(* The footer *)
	^  footer
