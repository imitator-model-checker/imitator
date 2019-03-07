(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: Translater to Uppaal
 * 
 * File contributors : Étienne André
 * Created           : 2019/03/01
 * Last modified     : 2019/03/07
 *
 ************************************************************)

open OCamlUtilities
open Result
open AbstractModel
open ImitatorUtilities


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
let footer = "\n"
(*	^ "\n" ^ "/*************************************************************"
	^ "\n" ^ " * The end *"
	^ "\n" ^ " ************************************************************/"*)
	^ "\n" ^ "</nta>"
	^ "\n" ^ ""


(************************************************************)
(** Variable declarations *)
(************************************************************)

(* Convert a var_type into a string *)
let string_of_var_type = function
	| Var_type_clock -> "clock"
	| Var_type_discrete -> "int"
	| Var_type_parameter -> "parameter"



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
		
			(* For action in exactly > 2 automata, then it seems Uppaal cannot encode this situation: we use broadcast and issue a warning (semantics may be broken) *)
			else (
				(* Issue a warning *)
				print_warning ("Action '" ^ action_name ^ "' is used in " ^ (string_of_int nb_automata) ^ " automata: IMITATOR uses strong broadcast semantics, while Uppaal uses broadcast semantics; the behavior may differ!");
				"broadcast chan " ^ action_name ^ "; /* WARNING! This action is used in " ^ (string_of_int nb_automata) ^ " automata: IMITATOR uses strong broadcast semantics, while Uppaal uses broadcast semantics; the behavior may therefore differ */"
			)
		
		) model.actions
		)
	)



(* Convert the initial variable declarations into a string *)
let string_of_declarations model =
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
let string_of_guard variable_names x_coord_str y_coord_str = function
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
				else "&&" ^ (LinearConstraint.customized_string_of_pxd_linear_constraint uppaal_strings variable_names discrete_continuous_guard.continuous_guard)
			)
		) ^ "</guard>"
		




(************************************************************)
(** Automata *)
(************************************************************)


(* Creates a unique id for each location *)
let id_of_location model automaton_index location_index =
	(* Return an id of the form id_pta0_loc0 *)
	"id_pta" ^ (string_of_int automaton_index) ^ "_loc" ^ (string_of_int location_index ^ "")



(* Convert the invariant of a location into a string *)
let string_of_invariant model automaton_index location_index =
	(*** TODO: check well formed with constraints x <= … ***)

	(* Invariant *)
	(*** NOTE: arbitrary positioning (location_id * scaling_factor, +20%) ***)
	"\n\t<label kind=\"invariant\" x=\"" ^ (string_of_int (location_index * scaling_factor)) ^ "\" y=\"" ^ (string_of_int (scaling_factor / 5)) ^ "\">"
	^ (LinearConstraint.customized_string_of_pxd_linear_constraint uppaal_strings model.variable_names (model.invariants automaton_index location_index))
	
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
	string_of_list_of_string_with_sep ", " (List.map (fun (variable_index, arithmetic_expression) ->
		(* Convert the variable name *)
		(model.variable_names variable_index)
		^ " = "
		(* Convert the arithmetic_expression *)
		^ (string_of_arithmetic_expression model.variable_names arithmetic_expression)
	) updates)


let string_of_updates model x_coord_str y_coord_str clock_updates discrete_updates =
	(* Should we add a separating comma between clock updates and discrete updaes? *)
	let no_clock_updates =
		clock_updates = No_update || clock_updates = Resets [] || clock_updates = Updates []
	in
	let no_discrete_updates = discrete_updates = [] in
	let separator_comma =
		if no_clock_updates || no_discrete_updates then "" else ", "
	in

	(* Convert the updates *)
	if no_clock_updates && no_discrete_updates then ""
	else(
		(*** TODO ***)
		  "<label kind=\"assignment\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ y_coord_str ^ "\">"
		(* Clock updates *)
		^ (string_of_clock_updates model clock_updates)
		(* Add a coma in case of both clocks and discrete *)
		^ separator_comma
		(* Discrete updates *)
		^ (string_of_discrete_updates model discrete_updates)
		^ "</label>"
	)
	


(* Convert a transition of a location into a string *)
let string_of_transition model automaton_index action_index source_location (guard, clock_updates, discrete_updates, target_location) =
	(* Arbitrary positioning: x = between source_location and target_location *)
	(*** NOTE: integer division here, so first multiplication, then division (otherwise result can be 0) ***)
	let x_coord_str = (string_of_int ((source_location + target_location) * scaling_factor / 2)) in
	
	(* Header *)
	"\n\t<transition>"
	
	(* Source *)
	^ "\n\t\t<source ref=\"" ^ (id_of_location model automaton_index source_location) ^ "\"/>"
	
	(* Target *)
	^ "\n\t\t<target ref=\"" ^ (id_of_location model automaton_index target_location) ^ "\"/>"
	
	(* Synchronisation label *)
	^ (
		match model.action_types action_index with
			
			(*** TODO ***)
			
			(*** NOTE: temporarily set all actions are sending "!" ***)
			| Action_type_sync -> "\n\t\t<label kind=\"synchronisation\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ (string_of_int (scaling_factor * 2 / 5)) ^ "\">" ^ (model.action_names action_index) ^ "!</label>"
			| Action_type_nosync -> ""
	)
	
	(* Guard *)
	^ (
		(* Quite arbitrary positioning *)
		let y_coord_str = (string_of_int (scaling_factor / 5)) in
		"\n\t\t" ^ (string_of_guard model.variable_names x_coord_str y_coord_str guard)
	)

	(* Updates *)
	^ (
		(* Quite arbitrary positioning *)
		let y_coord_str = (string_of_int (- scaling_factor / 5)) in
		"\n\t\t" ^ (string_of_updates model x_coord_str y_coord_str clock_updates discrete_updates)
	)

	(* Footer *)
	^ "\n\t</transition>"



(* Convert the transitions of an automaton into a string *)
let string_of_transitions model automaton_index =
	string_of_list_of_string (
	(* For each location *)
	List.map (fun location_index -> 
		string_of_list_of_string (
		(* For each action *)
		List.map (fun action_index ->
			(* Get the list of transitions *)
			let transitions = model.transitions automaton_index location_index action_index in
			(* Convert to string *)
			string_of_list_of_string (
				(* For each transition *)
				List.map (string_of_transition model automaton_index action_index location_index) transitions
				)
			) (model.actions_per_location automaton_index location_index)
		)
	) (model.locations_per_automaton automaton_index))



(* Convert a location of an automaton into a string *)
let string_of_location model automaton_index location_index =
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
	^ (string_of_invariant model automaton_index location_index)
	
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
let string_of_locations model automaton_index =
	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		string_of_location model automaton_index location_index
	) (model.locations_per_automaton automaton_index))

(* Convert the initial location of an automaton *)
let string_of_initial_location model automaton_index =
	(* Get initial location *)
	let inital_global_location  = model.initial_location in
	let initial_location = Location.get_location inital_global_location automaton_index in
	"<init ref=\"" ^ (id_of_location model automaton_index initial_location) ^ "\"/>"

(* Convert an automaton into a string *)
let string_of_automaton model automaton_index =
		(*** NOTE: arbitrary positioning at (automaton_index, automaton_index) ***)
(* 	"\n/************************************************************/" *)
	(*^*) "\n<template><name x=\"" ^ (string_of_int automaton_index) ^ "\" y=\"" ^ (string_of_int automaton_index) ^ "\">" ^ (model.automata_names automaton_index) ^ "</name><declaration>// No local declaration for automaton '" ^ (model.automata_names automaton_index) ^ "'
</declaration>"
	^ "\n " ^ (string_of_locations model automaton_index)
	^ "\n " ^ (string_of_initial_location model automaton_index)
	^ "\n " ^ (string_of_transitions model automaton_index)
	^ "\n </template>" (*/ * end " ^ (model.automata_names automaton_index) ^ " */ *)
(* 	^ "\n/************************************************************/" *)


(* Convert the automata into a string *)
let string_of_automata model =
	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in

	(* Print all (other) PTA *)
	string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton model automaton_index
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





(************************************************************)
(** Property *)
(************************************************************)
let property_header =
	"\n"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ "(* Property specification *)"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ ""



let string_of_unreachable_location model unreachable_global_location =
	(* Convert locations *)
	string_of_list_of_string_with_sep " & " (List.map (fun (automaton_index, location_index) ->
			"loc[" ^ (model.automata_names automaton_index) ^ "]" ^ " = " ^ (model.location_names automaton_index location_index)
		) unreachable_global_location.unreachable_locations
	)
	^
	(* Separator *)
	(if unreachable_global_location.unreachable_locations <> [] && unreachable_global_location.discrete_constraints <> [] then " & " else "")
	^
	(* Convert discrete *)
	string_of_list_of_string_with_sep " & " (List.map (function
		| Discrete_l (discrete_index , discrete_value)
			-> (model.variable_names discrete_index) ^ " < " ^ (NumConst.string_of_numconst discrete_value)
		| Discrete_leq (discrete_index , discrete_value)
			-> (model.variable_names discrete_index) ^ " <= " ^ (NumConst.string_of_numconst discrete_value)
		| Discrete_equal (discrete_index , discrete_value)
			-> (model.variable_names discrete_index) ^ " = " ^ (NumConst.string_of_numconst discrete_value)
		| Discrete_neq (discrete_index , discrete_value)
			-> (model.variable_names discrete_index) ^ " <> " ^ (NumConst.string_of_numconst discrete_value)
		| Discrete_geq (discrete_index , discrete_value)
			-> (model.variable_names discrete_index) ^ " >= " ^ (NumConst.string_of_numconst discrete_value)
		| Discrete_g (discrete_index , discrete_value)
			-> (model.variable_names discrete_index) ^ " > " ^ (NumConst.string_of_numconst discrete_value)
		| Discrete_interval (discrete_index , min_discrete_value, max_discrete_value)
			-> (model.variable_names discrete_index) ^ " in [" ^ (NumConst.string_of_numconst min_discrete_value) ^ " , " ^ (NumConst.string_of_numconst max_discrete_value) ^ "]"
		) unreachable_global_location.discrete_constraints
	)


(** Convert the correctness property to a string *)
let string_of_property model property = 
	match property with
	(* An "OR" list of global locations *)
	| Unreachable_locations unreachable_global_location_list ->
		"property := unreachable " ^ (
			string_of_list_of_string_with_sep "\n or \n " (List.map (string_of_unreachable_location model) unreachable_global_location_list)
		)

	(* if a2 then a1 has happened before *)
	| Action_precedence_acyclic (a1 , a2) ->
		"property := if " ^ (model.action_names a2) ^ " then " ^ (model.action_names a1) ^ " has happened before;"
	(* everytime a2 then a1 has happened before *)
	| Action_precedence_cyclic (a1 , a2) ->
		"property := everytime " ^ (model.action_names a2) ^ " then " ^ (model.action_names a1) ^ " has happened before;"
	(* everytime a2 then a1 has happened exactly once before *)
	| Action_precedence_cyclicstrict (a1 , a2) ->
		"property := everytime " ^ (model.action_names a2) ^ " then " ^ (model.action_names a1) ^ " has happened exactly once before;"

	(*** NOTE: not implemented ***)
(*	(* if a1 then eventually a2 *)
	| Eventual_response_acyclic (a1 , a2) -> ""
	(* everytime a1 then eventually a2 *)
	| Eventual_response_cyclic (a1 , a2) -> ""
	(* everytime a1 then eventually a2 once before next *)
	| Eventual_response_cyclicstrict (a1 , a2) -> ""
	*)

	(* a no later than d *)
	| Action_deadline (a, d) ->
		"property := " ^ (model.action_names a) ^ " no later than " ^ (LinearConstraint.string_of_p_linear_term model.variable_names d) ^ ";"

	(* if a2 then a1 happened within d before *)
	| TB_Action_precedence_acyclic (a1 , a2, d) ->
		"property := if " ^ (model.action_names a2) ^ " then " ^ (model.action_names a1) ^ " has happened within " ^ (LinearConstraint.string_of_p_linear_term model.variable_names d) ^ " before;"
	(* everytime a2 then a1 happened within d before *)
	| TB_Action_precedence_cyclic (a1 , a2, d) ->
		"property := everytime " ^ (model.action_names a2) ^ " then " ^ (model.action_names a1) ^ " has happened within " ^ (LinearConstraint.string_of_p_linear_term model.variable_names d) ^ " before;"
	(* everytime a2 then a1 happened once within d before *)
	| TB_Action_precedence_cyclicstrict (a1 , a2, d) ->
		"property := everytime " ^ (model.action_names a2) ^ " then " ^ (model.action_names a1) ^ " has happened once within " ^ (LinearConstraint.string_of_p_linear_term model.variable_names d) ^ " before;"
	
	(* if a1 then eventually a2 within d *)
	| TB_response_acyclic (a1 , a2, d) ->
		"property := if " ^ (model.action_names a2) ^ " then eventually " ^ (model.action_names a1) ^ " within " ^ (LinearConstraint.string_of_p_linear_term model.variable_names d) ^ ";"
	(* everytime a1 then eventually a2 within d *)
	| TB_response_cyclic (a1 , a2, d) ->
		"property := everytime " ^ (model.action_names a2) ^ " then eventually " ^ (model.action_names a1) ^ " within " ^ (LinearConstraint.string_of_p_linear_term model.variable_names d) ^ ";"
	(* everytime a1 then eventually a2 within d once before next *)
	| TB_response_cyclicstrict (a1 , a2, d) ->
		"property := if " ^ (model.action_names a2) ^ " then eventually " ^ (model.action_names a1) ^ " within " ^ (LinearConstraint.string_of_p_linear_term model.variable_names d) ^ " once before next;"

	(* sequence a1, …, an *)
	| Sequence_acyclic action_index_list ->
		"property := sequence (" ^ (string_of_list_of_string_with_sep ", " (List.map model.action_names action_index_list)) ^ ");"
	(* always sequence a1, …, an *)
	| Sequence_cyclic action_index_list ->
		"property := always sequence (" ^ (string_of_list_of_string_with_sep ", " (List.map model.action_names action_index_list)) ^ ");"
	
	(*** NOTE: Would be better to have an "option" type ***)
	| Noproperty -> "(* no property *)"

(** Convert the projection to a string *)
let string_of_projection model =
	match model.projection with
	| None -> ""
	| Some parameter_index_list ->
		"\nprojectresult(" ^ (string_of_list_of_string_with_sep ", " (List.map model.variable_names parameter_index_list)) ^ ");"


(** Convert the optimization to a string *)
let string_of_optimization model =
	match model.optimized_parameter with
	| No_optimization -> ""
	| Minimize parameter_index ->
		"minimize(" ^ (model.variable_names parameter_index) ^ ");"
	| Maximize parameter_index ->
		"maximize(" ^ (model.variable_names parameter_index) ^ ");"


(************************************************************)
(** Model *)
(************************************************************)

(* Convert the model into a string *)
let string_of_model model =
	(* The header *)
	string_of_header model
	
	(* The variable declarations *)
	^  "\n" ^ (string_of_declarations model)
	
	(* All automata *)
	^  "\n" ^ (string_of_automata model)

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
	^  "\n" ^ footer


