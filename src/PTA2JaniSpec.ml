(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Translater to JaniSpec
 *
 * File contributors : Dylan Marinho
 * Created           : 2021/02/23
 * Last modified     : 2021/03/09
 *
 ************************************************************)
 
 (*
 TODO:
 - if..then..else
 - synchro
 - rate of clocks (on the model + continuous set to 1) -> doing
 - the not done strings (Discrete, ...)
 *)

open Constants
open OCamlUtilities
open ImitatorUtilities
open LinearConstraint
open DiscreteExpressions
open AbstractModel
open Result


(************************************************************)
(** Customized values for constraint conversion *)
(************************************************************)

let jani_strings : customized_string = {
	true_string   = "true";
	false_string  = "false";
	and_operator  = "∧";
	or_operator   = "∨"; (* useless *)
	l_operator    = "<";
	le_operator   = "≤";
	eq_operator   = "=";
	ge_operator   = ">";
	g_operator    = "≥";
}

let jani_separator = ", "

let jani_assignment = " = "

let jani_version = "1"
let jani_type = "sha"
let jani_features = "[\"derived-operators\"]"

(************************************************************)
(** Header *)
(************************************************************)

(* Add a header to the model *)
let string_of_header model =
  let options = Input.get_options () in
	   "\t\"jani-version\": " ^ jani_version ^ jani_separator ^ "\n"
  ^  "\t\"name\": " ^ "\"" ^ options#model_file_name ^ "\"" ^ jani_separator ^ "\n"
  ^  "\t\"type\": " ^ "\"" ^ jani_type ^ "\"" ^ jani_separator ^ "\n"
  ^  "\t\"features\": " ^ jani_features ^ jani_separator ^ "\n"

(************************************************************
 Declarations
************************************************************)

(* Declaration of actions *)
let string_of_actions model =
  "\t\"actions\": [\n"
  ^ (string_of_list_of_string_with_sep (jani_separator^"\n")
		(List.filter (fun string -> string<>"")
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
			  if nb_automata = 0 then ""

			  else "\t\t{\"name\":\"" ^ action_name ^ "\"}"
			) model.actions
		   )
		)
    )
  ^ "\n\t]" ^ jani_separator ^ "\n"

(* Convert the initial clocks declarations into a string *)
let string_of_clocks model =
  let multirate_bol = model.has_non_1rate_clocks in
  let string_of_variables list_of_variables =
    string_of_list_of_string_with_sep (jani_separator^"\n") (List.map
      (fun var ->
		let clocks_type = if multirate_bol then "continuous" else "clock" in
          "\t\t{\n"
        ^ "\t\t\t\"name\": \"" ^ model.variable_names var ^ "\"" ^ jani_separator ^ "\n"
        ^ "\t\t\t\"type\": \""^ clocks_type ^"\"" ^ jani_separator ^ "\n"
        ^ "\t\t\t\"initial_value\": 0" ^ "\n"
        ^ "\t\t}"
      )
      list_of_variables
    )
  in
  if model.nb_clocks > 0 then
    (string_of_variables model.clocks_without_special_reset_clock)
    else ""

(* Convert the initial discrete var declarations into a string *)
let string_of_discrete model =
	if model.nb_discrete > 0 then(
		(string_of_list_of_string_with_sep (jani_separator^"\n")
			(List.map (fun discrete_index ->
				(* Get the name *)
				let discrete_name = model.variable_names discrete_index in

				(* Get the initial value *)
				let inital_global_location  = model.initial_location in
				let initial_value = Location.get_discrete_value inital_global_location discrete_index in

				(* Assign *)
          "\t\t{\n"
        ^ "\t\t\t\"name\": \"" ^ discrete_name ^ "\"" ^ jani_separator ^ "\n"
        ^ "\t\t\t\"type\": \"int\"" ^ jani_separator ^ "\n"
        ^ "\t\t\t\"initial_value\": " ^ NumConst.jani_string_of_numconst initial_value ^ "\n"
        ^ "\t\t}"
      ) model.discrete
			)
		)
	) else ""

(* Convert the parameter declarations into a string *)
let string_of_parameters model =
	if model.nb_parameters > 0 then(
		(string_of_list_of_string_with_sep (jani_separator^"\n")
			(List.map (fun parameter_index ->
				(* Get the name *)
				let parameter_name = model.variable_names parameter_index in

				(* Assign *)
          "\t\t{\n"
        ^ "\t\t\t\"name\": \"" ^ parameter_name ^ "\"" ^ jani_separator ^ "\n"
        ^ "\t\t\t\"type\": \"real\"" ^ jani_separator ^ "\n"
        ^ "\t\t}"
			) model.parameters
			)
		)
	) else ""

(* Declaration of variables *)
let string_of_variables model =
  "\t\"variables\": [\n"
  ^ (string_of_clocks model) ^ jani_separator ^ "\n"
  ^ (string_of_discrete model) ^ jani_separator ^ "\n"
  ^ (string_of_parameters model) ^ "\n"
  ^ "\t]" ^ jani_separator ^ "\n"

(* Properties *)
let string_of_properties =
	"\t\"properties\": []" ^ jani_separator ^ "\n"

(************************************************************)
(** Automata *)
(************************************************************)

let rec string_of_strings_with_sep_and string_list =
	match string_list with
	| (elem::[]) -> elem
	| (elem::q) -> 
				  "\t\t\t\t\t\t\t\"op\": \"" ^ jani_strings.and_operator ^ "\"" ^ jani_separator ^ "\n"
				^ "\t\t\t\t\t\t\t\"left\": {\n" ^ elem ^ "\n\t\t\t\t\t\t\t}" ^ jani_separator ^ "\n"
				^ "\t\t\t\t\t\t\t\"right\": {\n" ^ string_of_strings_with_sep_and q ^ "\n\t\t\t\t\t\t\t}"

(** Convert a guard or an invariant into a string *)
(*TODO HERE FOR INVARIANT/GUARD DISPLAY*)
let string_of_guard_or_invariant actions_and_nb_automata variable_names = function
	(* True guard = no guard *)
	| True_guard -> ""

	(* False *)
	| False_guard -> "\t\t\t\t\t\t\"exp\": {" ^ jani_strings.false_string ^ "}" ^ "\n"

	| Discrete_guard discrete_guard -> (*DOING DYLAN*)

        let str_discrete_guard = (NonlinearConstraint.customized_string_of_nonlinear_constraint_for_jani jani_strings variable_names discrete_guard) in
        let str_discrete_guard_without_true = if str_discrete_guard = "true" then "" else str_discrete_guard in
        str_discrete_guard_without_true

	| Continuous_guard continuous_guard ->
		(* Remove true guard *)
		if LinearConstraint.pxd_is_true continuous_guard then "" else			
			let list_of_inequalities = LinearConstraint.pxd_get_inequalities continuous_guard in
			(string_of_strings_with_sep_and 
				(List.map (fun (inequality) ->
					let op = match (LinearConstraint.op_of_pxd_linear_inequality inequality) with
						| Op_l		-> jani_strings.l_operator
						| Op_le		-> jani_strings.le_operator
						| Op_eq		-> jani_strings.eq_operator
						| Op_ge 	-> jani_strings.ge_operator
						| Op_g		-> jani_strings.g_operator
					in
					let left = LinearConstraint.string_of_left_term_of_pxd_linear_inequality variable_names inequality in 
					let right = LinearConstraint.string_of_right_term_of_pxd_linear_inequality variable_names inequality in 
					  "\t\t\t\t\t\t\t\"op\": \"" ^ op ^ "\"" ^ jani_separator ^ "\n"
					^ "\t\t\t\t\t\t\t\"left\": \"" ^ left ^ "\"" ^ jani_separator ^ "\n"
					^ "\t\t\t\t\t\t\t\"right\": \"" ^ right ^ "\""
				) list_of_inequalities)
			)

	| Discrete_continuous_guard discrete_continuous_guard -> (*TODO DYLAN*)
	    let content = (
            (NonlinearConstraint.customized_string_of_nonlinear_constraint jani_strings variable_names discrete_continuous_guard.discrete_guard)
            ^
            (
                (* Remove true guard *)
                if LinearConstraint.pxd_is_true discrete_continuous_guard.continuous_guard then ""
                else jani_strings.and_operator ^ (LinearConstraint.customized_string_of_pxd_linear_constraint jani_strings variable_names discrete_continuous_guard.continuous_guard)
            )
        ) in
        content


(* Convert the invariant of a location into a string *)
let string_of_invariant model actions_and_nb_automata automaton_index location_index =
  let invariant = string_of_guard_or_invariant actions_and_nb_automata model.variable_names (model.invariants automaton_index location_index) in
	(* Invariant *)
	"\n" ^ invariant

(* Convert the guard of an edge into a string *)
let string_of_guard model actions_and_nb_automata model_variable_names transition_guard =
  let guard = string_of_guard_or_invariant actions_and_nb_automata model.variable_names (transition_guard) in
  (* Guard *)
  "\n" ^ guard

let string_of_clock_rate model actions_and_nb_automata automaton_index location_index = 
	let rec clock_is_1rate clock_index flow_list = 
		match flow_list with
		| [] -> true
		| ((var, _)::q) -> if clock_index = var then false else (clock_is_1rate clock_index q)
	in
	if not model.has_non_1rate_clocks then ""
	else (
	"\n" ^ string_of_strings_with_sep_and (List.append
		(*Step 1: explicit rates*)
		(
			List.map (
				fun (variable_index, flow_value) -> 
					let variable_name = (model.variable_names variable_index) in
					let value = (NumConst.jani_string_of_numconst flow_value) in 
					  "\t\t\t\t\t\t\t\t\"op\": \"=\"" ^ jani_separator 
					^ " \"left\": {\"op\": \"der\", \"var\": \"" ^ variable_name ^ "\"}" ^ jani_separator
					^ " \"right\": " ^ value ^ ""
			) (model.flow automaton_index location_index)
		)
		
		(*Step 2: set rate 1 to unspecified clocks*)
		(
			List.map (
				fun variable_index -> 
					let variable_name = (model.variable_names variable_index) in
					  "\t\t\t\t\t\t\t\t\"op\": \"=\"" ^ jani_separator 
					^ " \"left\": {\"op\": \"der\", \"var\": \"" ^ variable_name ^ "\"}" ^ jani_separator
					^ " \"right\": 1"
			) 
			(List.filter (fun clock_index -> clock_is_1rate clock_index (model.flow automaton_index location_index)) model.clocks)
		)
	)
	)

(* Convert a location of an automaton into a string *)
let string_of_location model actions_and_nb_automata automaton_index location_index =
	let invariant = string_of_invariant model actions_and_nb_automata automaton_index location_index in
	let der_clock = string_of_clock_rate model actions_and_nb_automata automaton_index location_index in
	let not_display_timeprogress = (invariant = "\n" && der_clock = "") in
	let twoparts = (invariant <> "\n" && der_clock <> "") in
	(* Header *)
	"\t\t\t\t{"

	(* Name *)
	^ "\n\t\t\t\t\t\"name\": \"" ^ (model.location_names automaton_index location_index) ^ "\""

	(* Invariant and stopwatches *)
	^ (if not_display_timeprogress then "" else (
		  jani_separator ^ "\n\t\t\t\t\t\"time-progress\": {\n\t\t\t\t\t\t\"exp\": {"
		^ (if twoparts then ("\n\t\t\t\t\t\t\t\t\"op\": \"" ^ jani_strings.and_operator ^ "\"" ^ jani_separator) else "")
		^ (if twoparts then "\n\t\t\t\t\t\t\t\t\"left\": {" else "") ^ invariant ^ (if twoparts then "\n\t\t\t\t\t\t\t}" ^ jani_separator else "")
		^ (if twoparts then "\n\t\t\t\t\t\t\t\t\"right\": {" else "") ^ der_clock ^ (if twoparts then "\n\t\t\t\t\t\t\t}" else "")
		^ "\n\t\t\t\t\t\t}\n\t\t\t\t\t}"
	))

	(* Footer *)
	^ "\n\t\t\t\t}"

(* Convert the locations of an automaton into a string *)
let string_of_locations model actions_and_nb_automata automaton_index =
	string_of_list_of_string_with_sep (jani_separator^"\n") (List.map (fun location_index ->
		string_of_location model actions_and_nb_automata automaton_index location_index
	) (model.locations_per_automaton automaton_index))

(* Convert the initial location of an automaton *)
let string_of_initial_location model automaton_index =
	let inital_global_location  = model.initial_location in
	let initial_location = Location.get_location inital_global_location automaton_index in
	"\"" ^ (model.location_names automaton_index initial_location) ^ "\""

let string_of_clock_updates model = function
	| No_update -> ""
	| Resets list_of_clocks ->
		string_of_list_of_string_with_sep (jani_separator^"\n") (List.map (fun variable_index ->
			"\t\t\t\t\t\t\t{\"ref\": \""
			^ (model.variable_names variable_index)
			^ "\"" ^ jani_separator ^ " \"value\" : 0}"
		) list_of_clocks)
	| Updates list_of_clocks_lt ->
		string_of_list_of_string_with_sep (jani_separator^"\n") (List.map (fun (variable_index, linear_term) ->
			"\t\t\t\t\t\t\t{\"ref\": \""
			^ (model.variable_names variable_index)
			^ "\"" ^ jani_separator ^ " \"value\" : "
			^ (LinearConstraint.string_of_pxd_linear_term_for_jani model.variable_names linear_term)
			^ "}"
		) list_of_clocks_lt)

(* Convert a list of updates into a string *)
let string_of_discrete_updates model updates =
	string_of_list_of_string_with_sep (jani_separator^"\n") (List.map (fun (variable_index, arithmetic_expression) ->
		"\n\t\t\t\t\t\t\t{\"ref\": \""
		^ (model.variable_names variable_index)
		^ "\"" ^ jani_separator ^ " \"value\" : "
		^ (DiscreteExpressions.string_of_arithmetic_expression_for_jani model.variable_names arithmetic_expression)
		^ "}"
	) updates)

let string_of_updates model automaton_index action_index clock_updates discrete_updates =
	(* Check for emptiness of some updates *)
	let no_clock_updates =
		clock_updates = No_update || clock_updates = Resets [] || clock_updates = Updates []
	in
	let no_discrete_updates = discrete_updates = [] in
	(* If no update at all: empty string *)
	if no_clock_updates && no_discrete_updates then ""

	else(
		"\n"
		^ (string_of_clock_updates model clock_updates) 
		^ (if (not no_clock_updates) && (not no_discrete_updates) then jani_separator else "")
		^ (string_of_discrete_updates model discrete_updates)
		^ "\n"
	)

(* Convert a transition of a location into a string *)
let string_of_transition model actions_and_nb_automata automaton_index source_location transition =
	let clock_updates = transition.updates.clock in
	let discrete_updates = transition.updates.discrete in
	let guard = (string_of_guard model actions_and_nb_automata model.variable_names transition.guard) in
	let assignments = (string_of_updates model automaton_index transition.action clock_updates discrete_updates) in
	(* Header *)
	"\t\t\t\t{"

	(* Source *)
	^ "\n\t\t\t\t\t\"location\": \"" ^ (model.location_names automaton_index source_location) ^ "\"" ^ jani_separator

	(* Guard *)
	^ (if guard = "\n" then "" else 
		((
			"\n\t\t\t\t\t\"guard\": {" ^ guard ^ "\n\t\t\t\t\t}"
		) ^ jani_separator))

	(* Target *)
	^ "\n\t\t\t\t\t\"destinations\": [{"
	^ "\n\t\t\t\t\t\t\"location\": \"" ^ (model.location_names automaton_index transition.target) ^ "\"" ^ jani_separator
	^  (if assignments = "" then "" else ("\n\t\t\t\t\t\t\"assignments\": [" ^ assignments ^ "\t\t\t\t\t\t]"))
	^ "\n\t\t\t\t\t}]"

	(* Footer *)
	^ "\n\t\t\t\t}"

(* Convert the transitions of an automaton into a string *)
let string_of_transitions model actions_and_nb_automata automaton_index =
	string_of_list_of_string_with_sep (jani_separator^"\n") (
	(* For each location *)
	List.map (fun location_index ->
		string_of_list_of_string_with_sep (jani_separator^"\n") (
		(* For each action *)
		List.map (fun action_index ->
			(* Get the list of transitions *)
			let transitions = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in
			(* Convert to string *)
			string_of_list_of_string_with_sep (jani_separator^"\n") (
				(* For each transition *)
				List.map (string_of_transition model actions_and_nb_automata automaton_index location_index) transitions
				)
			) (model.actions_per_location automaton_index location_index)
		)
	) (model.locations_per_automaton automaton_index))

(* Convert an automaton into a string *)
let string_of_automaton model actions_and_nb_automata automaton_index =
  	"\n\t\t{\n"
    ^ "\t\t\t\"name\": \"" ^ (model.automata_names automaton_index) ^ "\"" ^ jani_separator
  	^ "\n\t\t\t\"locations\": [\n" ^ (string_of_locations model actions_and_nb_automata automaton_index) ^ "\n\t\t\t]" ^ jani_separator
  	^ "\n\t\t\t\"initial_locations\": [" ^ (string_of_initial_location model automaton_index) ^ "]" ^ jani_separator
  	^ "\n\t\t\t\"edges\": [\n" ^ (string_of_transitions model actions_and_nb_automata automaton_index) ^ "\n\t\t\t]"
    ^ "\n\t\t}"

(* Convert the automata into a string *)
let string_of_automata model actions_and_nb_automata =
	(*** WARNING: Do not print the observer TODO ? ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in
	(* Print all (other) PTA *)
    "\t\"automata\": ["
  ^ string_of_list_of_string_with_sep (jani_separator^"\n") (
  		List.map (fun automaton_index ->
        string_of_automaton model actions_and_nb_automata automaton_index
  	) pta_without_obs)
  ^ "\n\t]" ^ jani_separator ^ "\n"


(************************************************************)
(** System *)
(************************************************************)
(* Create the system definition *)
let string_of_system model =
	let is_in_automaton action_index automaton_index = (List.mem action_index (model.actions_per_automaton automaton_index)) in
	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in
	(* Open *)
	"\t\"system\": {\n"
	^ "\t\t\"elements\": ["
		(* System definition *)
		^ "\n" ^ (string_of_list_of_string_with_sep (jani_separator^"\n") (
					List.map (fun automaton_index ->
						let automaton = model.automata_names automaton_index in
						"\t\t\t{\"automaton\": \"" ^ automaton ^ "\"}"
					 )
					 (pta_without_obs)
				  ))
		^ "\n"
		(* Close *)
		^ "\t\t]" ^ jani_separator ^ "\n"
	^ "\t\t\"syncs\": ["
		(* Actions *)
		^ "\n" ^ (string_of_list_of_string_with_sep (jani_separator^"\n") (
					List.map (fun action_index ->
						let action_name = model.action_names action_index in
						  "\t\t\t{\"synchronise\": ["
						^ (
							string_of_list_of_string_with_sep (jani_separator^" ") (
							List.map (fun automaton_index ->
								if (is_in_automaton action_index automaton_index)
								then ("\"" ^ action_name ^ "\"")
								else "null"
							) (pta_without_obs))
						  )
						^ "]"
						^ jani_separator ^ " "
						^ "\"result\": \"" ^ action_name ^ "\" }"
					 )
					 (List.filter (fun action_index -> model.action_types action_index = Action_type_sync) model.actions)
				  ))
		^ "\n"
		(* Close *)
		^ "\t\t]\n"
	^ "\t}\n"

(************************************************************)
(** Model *)
(************************************************************)

(* Convert the model into a string *)
let string_of_model model =
  let actions_and_nb_automata = List.map (fun action_index ->
      (* Get number of automata *)
      let nb_automata = List.length (model.automata_per_action action_index) in
      (* Make it a pair *)
      action_index , nb_automata
    ) model.actions
    in

  "{\n"
  (*Header*)
  ^ string_of_header model
  ^ string_of_actions model
  ^ string_of_variables model
  ^ string_of_properties
  ^ string_of_automata model actions_and_nb_automata
  ^ string_of_system model
  ^ "}"
