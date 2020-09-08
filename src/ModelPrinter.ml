(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Convert an abstract model to the input syntax of IMITATOR
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2009/12/02
 * Last modified     : 2020/09/08
 *
 ************************************************************)

open OCamlUtilities
open Exceptions
open Result
open DiscreteExpressions
open AbstractModel
open AbstractProperty
open ImitatorUtilities
open State
open StateSpace



(************************************************************)
(** Constants *)
(************************************************************)
let string_of_true	= "True"
let string_of_false	= "False"



(************************************************************)
(** Parameter valuation (PVal.pval) *)
(************************************************************)
(* Convert a parameter valuation (PVal.pval) into a string *)
let string_of_pval model pval =
	"  " ^ (
	string_of_list_of_string_with_sep "\n& " (
		List.map (fun parameter ->
			(model.variable_names parameter)
			^ " = "
			^ (NumConst.string_of_numconst (pval#get_value parameter))
		) model.parameters
	)
	)


(************************************************************)
(** V0 *)
(************************************************************)
(* Convert a V0 into a string *)
let string_of_v0 model v0 =
	"  " ^ (
	string_of_list_of_string_with_sep "\n& " (
		List.map (fun parameter ->
			(model.variable_names parameter)
			^ " = "
			^ (
(* 				let min_bound, max_bound = v0.(parameter) in *)
				let min_bound = v0#get_min parameter in
				let max_bound = v0#get_max parameter in
				if min_bound = max_bound
					then NumConst.string_of_numconst min_bound
					else (NumConst.string_of_numconst min_bound) ^ ".." ^ (NumConst.string_of_numconst max_bound)
			)
		) model.parameters
	)
	)



(************************************************************)
(** Header *)
(************************************************************)

let common_header =
	          "(************************************************************"
	^ "\n" ^ " * File automatically generated by " ^ Constants.program_name ^ ""
	^ "\n" ^ " * Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build)
	^ "\n" ^ " * Git      : " ^ (ImitatorUtilities.git_branch_and_hash)



(* Add a header to the model *)
let model_header() =
	let options = Input.get_options () in
	common_header
	^ "\n" ^ " * Model    : '" ^ options#model_file_name ^ "'"
	^ "\n" ^ " * Generated: " ^ (now()) ^ ""
	^ "\n" ^ " ************************************************************)"

(* Add a header to the property *)
let property_header() =
	let options = Input.get_options () in
	common_header
	^ "\n" ^ " * Model    : '" ^ (match options#property_file_name with Some property_file_name -> property_file_name | None -> raise (InternalError "Property file name not found in `ModelPrinter.property_header()`")) ^ "'"
	^ "\n" ^ " * Generated: " ^ (now()) ^ ""
	^ "\n" ^ " ************************************************************)"
	^ "\n"



(************************************************************)
(** Footer *)
(************************************************************)

(* End of the file *)
let footer = "\n"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ "(* The end *)"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ "end"
	^ "\n" ^ ""


(************************************************************)
(** Variable declarations *)
(************************************************************)

(* Convert a var_type into a string *)
let string_of_var_type = function
	| Var_type_clock -> "clock"
	| Var_type_discrete -> "discrete"
	| Var_type_parameter -> "parameter"



(* Convert the initial variable declarations into a string *)
let string_of_declarations model =
	(* Print some information *)
(* 	print_message Verbose_total "Entering `ModelPrinter.string_of_declarations`…"; *)

	let string_of_variables list_of_variables =
		string_of_list_of_string_with_sep ", " (List.map model.variable_names list_of_variables) in

		"var "
	^
	(if model.nb_clocks > 0 then
		("\n\t" ^ (string_of_variables model.clocks_without_special_reset_clock) ^ "\n\t\t: clock;\n") else "")
	^
	(if model.nb_discrete > 0 then
		("\n\t" ^ (string_of_variables model.discrete) ^ "\n\t\t: discrete;\n") else "")
	^
	(if model.nb_parameters > 0 then
		("\n\t" ^ (string_of_variables model.parameters) ^ "\n\t\t: parameter;\n") else "")

(************************************************************)
(** Guard *)
(************************************************************)

(*** NOTE: special handling as we have a discrete and a continuous guard that must be handled homogeneously ***)

(** Convert a guard into a string *)
let string_of_guard variable_names = function
	| True_guard -> LinearConstraint.string_of_true
	| False_guard -> LinearConstraint.string_of_false
	| Discrete_guard discrete_guard -> LinearConstraint.string_of_d_linear_constraint variable_names discrete_guard
	| Continuous_guard continuous_guard -> LinearConstraint.string_of_pxd_linear_constraint variable_names continuous_guard
	| Discrete_continuous_guard discrete_continuous_guard ->
		(LinearConstraint.string_of_d_linear_constraint variable_names discrete_continuous_guard.discrete_guard)
		^ LinearConstraint.string_of_and ^
		(LinearConstraint.string_of_pxd_linear_constraint variable_names discrete_continuous_guard.continuous_guard)




(************************************************************)
(** Automata *)
(************************************************************)

(* Convert the synclabs of an automaton into a string *)
let string_of_synclabs model automaton_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_synclabs(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

	"synclabs: "
	^ (let synclabs, _ = (List.fold_left (fun (synclabs, first) action_index ->
		match model.action_types action_index with
		(* Case sync: declare *)
		| Action_type_sync ->
			synclabs
			^ (if not first then ", " else "")
			^ (model.action_names action_index)
			(* Not the first one anymore *)
			, false
		(* Case nosync: do not declare *)
		| Action_type_nosync -> (synclabs, first)
	) ("", true) (model.actions_per_automaton automaton_index)) in synclabs)
	^ ";"


(* Convert the invariant of a location into a string *)
let string_of_invariant model automaton_index location_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_invariant(" ^ (model.automata_names automaton_index) ^ ", " ^ (model.location_names automaton_index location_index) ^ ")`…"); *)
	
	(* Invariant *)
	"invariant "
	^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names (model.invariants automaton_index location_index))

	(* Handle stopwatches *)
	^
	let stopped = model.stopwatches automaton_index location_index in
	(* Case 1: no stopwatches *)
	if stopped = [] then " "
	(* Case 2: some clocks stopped *)
	else
	let stopped_str = string_of_list_of_string_with_sep "," (List.map model.variable_names stopped) in
	" stop{" ^ stopped_str ^ "}"


(* Convert a sync into a string *)
let string_of_sync model action_index =
	match model.action_types action_index with
	| Action_type_sync -> " sync " ^ (model.action_names action_index)
	| Action_type_nosync -> " (* sync " ^ (model.action_names action_index) ^ "*) "

(** generic template for converting clock updates into string *)
let string_of_clock_updates_template model clock_updates wrap_reset wrap_expr sep =
	match clock_updates with
		| No_update -> ""
		| Resets list_of_clocks ->
			string_of_list_of_string_with_sep sep (List.map (fun variable_index ->
				wrap_reset variable_index
			) list_of_clocks)
		| Updates list_of_clocks_lt ->
			string_of_list_of_string_with_sep sep (List.map (fun (variable_index, linear_term) ->
				wrap_expr variable_index linear_term
			) list_of_clocks_lt)

(** Convert a clock update into a string *)
let string_of_clock_updates model clock_updates =
	let sep = ", " in
	let wrap_reset variable_index =  (model.variable_names variable_index) ^ " := 0" in
	let wrap_expr variable_index linear_term = (model.variable_names variable_index)
			^ " := "
			^ (LinearConstraint.string_of_pxd_linear_term model.variable_names linear_term) in
	string_of_clock_updates_template model clock_updates wrap_reset wrap_expr sep

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
		| DF_unary_min discrete_factor -> "-" ^ (string_of_factor discrete_factor)
		| DF_expression discrete_arithmetic_expression ->
			(*** TODO: simplify a bit? ***)
			"(" ^ (string_of_arithmetic_expression discrete_arithmetic_expression) ^ ")"
	(* Call top-level *)
	in string_of_arithmetic_expression



(* Convert a list of discrete updates into a string *)
let string_of_discrete_updates ?(sep=", ") model updates =
	string_of_list_of_string_with_sep sep (List.map (fun (variable_index, arithmetic_expression) ->
		(* Convert the variable name *)
		(model.variable_names variable_index)
		^ " := "
		(* Convert the arithmetic_expression *)
		^ (string_of_arithmetic_expression model.variable_names arithmetic_expression)
	) updates)


let string_of_boolean_operations = function
	| OP_L		-> "<"
	| OP_LEQ	-> "<="
	| OP_EQ		-> "="
	| OP_NEQ	-> "<>"
	| OP_GEQ	-> ">="
	| OP_G		-> ">"


(** Convert a discrete_boolean_expression into a string *)
let string_of_discrete_boolean_expression variable_names = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression (discrete_arithmetic_expression1, relop, discrete_arithmetic_expression2) ->
		(string_of_arithmetic_expression variable_names discrete_arithmetic_expression1)
		^ " "
		^ (string_of_boolean_operations relop)
		^ " "
		^ (string_of_arithmetic_expression variable_names discrete_arithmetic_expression2)
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in (discrete_arithmetic_expression1, discrete_arithmetic_expression2, discrete_arithmetic_expression3) ->
		(string_of_arithmetic_expression variable_names discrete_arithmetic_expression1)
		^ " in ["
		^ (string_of_arithmetic_expression variable_names discrete_arithmetic_expression2)
		^ " , "
		^ (string_of_arithmetic_expression variable_names discrete_arithmetic_expression3)
		^ "]"

(** Convert a Boolean expression into a string *)
let rec string_of_boolean variable_names = function
	| True_bool -> string_of_true
	| False_bool -> string_of_false
	| Not_bool b -> "<> (" ^ (string_of_boolean variable_names b) ^ ")"
	| And_bool (b1, b2) ->
		(string_of_boolean variable_names b1)
		^ " && "
		^ (string_of_boolean variable_names b2)
	| Or_bool (b1, b2) ->
		(string_of_boolean variable_names b1)
		^ " || "
		^ (string_of_boolean variable_names b2)
	| Discrete_boolean_expression discrete_boolean_expression ->
		string_of_discrete_boolean_expression variable_names discrete_boolean_expression



(** Return if there is no clock updates *)
let no_clock_updates clock_updates =
	clock_updates = No_update || clock_updates = Resets [] || clock_updates = Updates []

(** Returns when add comma separators between clock and discrete updates and
between discrete and conditional updates *)
let separator_comma updates =
	let no_clock_updates_ = no_clock_updates updates.clock in
	let no_discrete_updates = updates.discrete = [] in
	let no_conditional_updates = updates.conditional = [] in

	let first_separator = not (no_clock_updates_ || no_discrete_updates) in
	let second_separator = not (no_conditional_updates || (no_clock_updates_ && no_discrete_updates)) in
	(first_separator, second_separator)

(** Generic template to convert conditional updates into a string *)
let string_of_conditional_updates_template model conditional_updates string_of_clock_updates string_of_discrete_updates wrap_if wrap_else wrap_end sep =
	string_of_list_of_string_with_sep sep (List.map (fun (boolean_expr, if_updates, else_updates) ->
		let if_separator, _ = separator_comma if_updates in
		let empty_else = no_clock_updates else_updates.clock && else_updates.discrete = [] && else_updates.conditional = [] in
		(** Convert the Boolean expression *)
		(wrap_if boolean_expr)
		(** Convert the if updates *)
		^ (string_of_clock_updates model if_updates.clock)
		^ (if if_separator then sep else "")
		^ (string_of_discrete_updates model if_updates.discrete)
		(** Convert the else updates *)
		^ (if empty_else then "" else
			let else_separator, _ = separator_comma else_updates in
			wrap_else
			^ (string_of_clock_updates model else_updates.clock)
			^ (if else_separator then sep else "")
			^ (string_of_discrete_updates model else_updates.discrete))
		^ wrap_end
	) conditional_updates)

(** Convert a list of conditional updates into a string *)
let string_of_conditional_updates model conditional_updates =
	let wrap_if boolean_expr  = "if (" ^ (string_of_boolean model.variable_names boolean_expr) ^  ") then " in
	let wrap_else = " else " in
	let wrap_end = " end" in
	let sep = ", " in
	string_of_conditional_updates_template model conditional_updates string_of_clock_updates string_of_discrete_updates wrap_if wrap_else wrap_end sep

(* Convert a transition into a string *)
let string_of_transition model automaton_index (transition : transition) =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_transition(" ^ (model.automata_names automaton_index) ^ ")` with target `" ^ (model.location_names automaton_index transition.target) ^ "` via action `" ^ (string_of_sync model transition.action) ^ "`…"); *)

	let clock_updates = transition.updates.clock in
	let discrete_updates = transition.updates.discrete in
	let conditional_updates = transition.updates.conditional in
	let first_separator, second_separator = separator_comma transition.updates in

	(* Print some information *)
(* 	print_message Verbose_total ("Updates retrieved…"); *)

	"\n\t" ^ "when "
	(* Convert the guard *)
	^ (string_of_guard model.variable_names transition.guard)

	(* Convert the updates *)
	^ " do {"
	(* Clock updates *)
	^ (string_of_clock_updates model clock_updates)
	(* Add a coma in case of both clocks and discrete *)
	^ (if first_separator then ", " else "")
	(* Discrete updates *)
	^ (string_of_discrete_updates model discrete_updates)
	(* Add a coma in case of both clocks and discrete and conditions *)
	^ (if second_separator then ", " else "")
	(* Conditional updates *)
	^ (string_of_conditional_updates model conditional_updates)
	^ "} "
	
	(* Convert the sync *)
	^ (string_of_sync model transition.action)
	(* Convert the destination location *)
	^ " goto " ^ (model.location_names automaton_index transition.target)
	^ ";"


(* Convert a transition into a string: compact version for debugging/pretty-printing *)
let debug_string_of_transition model automaton_index (transition : transition) =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.debug_string_of_transition(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

	let clock_updates = transition.updates.clock in
	let discrete_updates = transition.updates.discrete in
	let conditional_updates = transition.updates.conditional in
	let first_separator, second_separator = separator_comma transition.updates in

	"[PTA " ^ (model.automata_names automaton_index) ^ ": guard{"
	(* Convert the guard *)
	^ (string_of_guard model.variable_names transition.guard)

	(* Convert the updates *)
	^ "} updates{"
	(* Clock updates *)
	^ (string_of_clock_updates model clock_updates)
	(* Add a coma in case of both clocks and discrete *)
	^ (if first_separator then ", " else "")
	(* Discrete updates *)
	^ (string_of_discrete_updates model discrete_updates)
	(* Add a coma in case of both clocks and discrete and conditions *)
	^ (if second_separator then ", " else "")
	(* Conditional updates *)
	^ (string_of_conditional_updates model conditional_updates)
	^ "} "

	(* Convert the sync *)
	^ (string_of_sync model transition.action)
	(* Convert the destination location *)
	^ " Target " ^ (model.location_names automaton_index transition.target)
	^ "] "




(* Convert the transitions of a location into a string *)
let string_of_transitions model automaton_index location_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_transitions(" ^ (model.automata_names automaton_index) ^ ": " ^ (model.location_names automaton_index location_index) ^ ")`…"); *)

	string_of_list_of_string (
	(* For each action *)
	List.map (fun action_index ->
		(* Print some information *)
(* 		print_message Verbose_total ("Retrieving transitions via `" ^ (string_of_sync model action_index) ^ "`…"); *)

		(* Get the list of transitions *)
		let transitions = model.transitions automaton_index location_index action_index in
		
		(* Print some information *)
(* 		print_message Verbose_total ("Transitions retrieved."); *)

		(* Convert to string *)
		string_of_list_of_string (
			(* For each transition *)
			List.map (string_of_transition model automaton_index) (List.map model.transitions_description transitions)
			)
		) (model.actions_per_location automaton_index location_index)
	)


(* Convert a location of an automaton into a string *)
let string_of_location model automaton_index location_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_location(" ^ (model.automata_names automaton_index) ^ ": " ^ (model.location_names automaton_index location_index) ^ ")`…"); *)

	"\n"
	^ (if model.is_urgent automaton_index location_index then "urgent " else "")
	^ (if model.is_accepting automaton_index location_index then "accepting " else "")
	^ "loc "
	^ (model.location_names automaton_index location_index)
	 ^ (match model.costs automaton_index location_index with
		| None -> ""
		| Some cost -> "[" ^ (LinearConstraint.string_of_p_linear_term model.variable_names cost) ^ "]"
	)
	^ ": "
	^ (string_of_invariant model automaton_index location_index) (* bug here! *)
	^ (string_of_transitions model automaton_index location_index)


(* Convert the locations of an automaton into a string *)
let string_of_locations model automaton_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_locations(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		(* Print some information *)
(* 		print_message Verbose_total ("Location_index : " ^ (string_of_int location_index)); *)
		
		string_of_location model automaton_index location_index
	) (model.locations_per_automaton automaton_index))


(* Convert an automaton into a string *)
let string_of_automaton model automaton_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_automaton(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

	"\n(************************************************************)"
	^ "\n automaton " ^ (model.automata_names automaton_index)
	^ "\n(************************************************************)"
	^ "\n " ^ (string_of_synclabs model automaton_index)
	^ "\n " ^ (string_of_locations model automaton_index)
	^ "\n end (* " ^ (model.automata_names automaton_index) ^ " *)"
	^ "\n(************************************************************)"


(* Convert the automata into a string *)
let string_of_automata model =
	(* Print some information *)
(* 	print_message Verbose_total "Entering `ModelPrinter.string_of_automata`…"; *)

	(*	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in*)

	(* Print all (other) PTA *)
	string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton model automaton_index
	) model.automata)




(************************************************************)
(** Initial state *)
(************************************************************)
let string_of_initial_state model =
	(* Print some information *)
(* 	print_message Verbose_total "Entering `ModelPrinter.string_of_initial_state`…"; *)

	(* Header of initial state *)
	"\n"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ "(* Initial state *)"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ ""
	^ "\n" ^ "init := True"

	(* Initial location *)
	^ "\n" ^ "\t(*------------------------------------------------------------*)"
	^ "\n" ^ "\t(* Initial location *)"
	^ "\n" ^ "\t(*------------------------------------------------------------*)"
	^
	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in

	(* Handle all (other) PTA *)
	let inital_global_location  = model.initial_location in
	let initial_automata = List.map
	(fun automaton_index ->
		(* Finding the initial location for this automaton *)
		let initial_location = Location.get_location inital_global_location automaton_index in
		(* '& loc[pta] = location' *)
		"\n\t& loc[" ^ (model.automata_names automaton_index) ^ "] = " ^ (model.location_names automaton_index initial_location)
	) pta_without_obs
	in string_of_list_of_string initial_automata

	(* Initial discrete assignments *)
	^ "\n" ^ ""
	^ "\n" ^ "\t(*------------------------------------------------------------*)"
	^ "\n" ^ "\t(* Initial discrete assignments *)"
	^ "\n" ^ "\t(*------------------------------------------------------------*)"
	^
	let initial_discrete = List.map
	(fun discrete_index ->
		(* Finding the initial value for this discrete *)
		let initial_value = Location.get_discrete_value inital_global_location discrete_index in
		(* '& var = val' *)
		"\n\t& " ^ (model.variable_names discrete_index) ^ " = " ^ (NumConst.string_of_numconst initial_value)
	) model.discrete
	in string_of_list_of_string initial_discrete

	(* Initial constraint *)
	^ "\n" ^ ""
	^ "\n" ^ "\t(*------------------------------------------------------------*)"
	^ "\n" ^ "\t(* Initial constraint *)"
	^ "\n" ^ "\t(*------------------------------------------------------------*)"
	^ "\n\t & " ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names model.initial_constraint)

	(* Footer of initial state *)
	^ "\n" ^ ""
	^ "\n" ^ ";"



(************************************************************)
(** Property *)
(************************************************************)



(** Convert a state_predicate to a string *)

let string_of_loc_predicate (model : AbstractModel.abstract_model) = function
	| Loc_predicate_EQ (automaton_index , location_index) ->
		"loc[" ^ (model.automata_names automaton_index) ^ "] = " ^ (model.location_names automaton_index location_index)
	| Loc_predicate_NEQ (automaton_index , location_index) ->
		"loc[" ^ (model.automata_names automaton_index) ^ "] =/= " ^ (model.location_names automaton_index location_index)


let string_of_simple_predicate (model : AbstractModel.abstract_model) = function
	| Discrete_boolean_expression discrete_boolean_expression ->
		string_of_discrete_boolean_expression model.variable_names discrete_boolean_expression
	| Loc_predicate loc_predicate ->
		string_of_loc_predicate model loc_predicate


	
let rec string_of_state_predicate_factor model = function
	| State_predicate_factor_NOT state_predicate_factor ->
		"not(" ^ (string_of_state_predicate_factor model state_predicate_factor) ^ ")"
	| Simple_predicate simple_predicate ->
		string_of_simple_predicate model simple_predicate
	| State_predicate state_predicate ->
		string_of_state_predicate model state_predicate

and string_of_state_predicate_term model = function
	| State_predicate_term_AND (state_predicate_term_1 , state_predicate_term_2) ->
		(string_of_state_predicate_term model state_predicate_term_1)
		^
		" && "
		^
		(string_of_state_predicate_term model state_predicate_term_2)
	| State_predicate_factor state_predicate_factor ->
		string_of_state_predicate_factor model state_predicate_factor


and string_of_state_predicate model = function
	| State_predicate_OR (state_predicate_1 , state_predicate_2) ->
		(string_of_state_predicate model state_predicate_1)
		^
		" || "
		^
		(string_of_state_predicate model state_predicate_2)
		
	| State_predicate_term state_predicate_term ->
		string_of_state_predicate_term model state_predicate_term
		
	| State_predicate_true -> string_of_true
	
	| State_predicate_false -> string_of_false



(** Convert the projection to a string, if any *)
let string_of_projection model property =
	match property.projection with
	| None -> ""
	| Some parameter_index_list ->
		"\nprojectresult(" ^ (string_of_list_of_string_with_sep ", " (List.map model.variable_names parameter_index_list)) ^ ");"


(** Convert a property to a string *)
let string_of_abstract_property model property =
	let header = property_header() in
	let prefix = "property := " in
	
	header
	^
	prefix
	
	^
	
	(* Handle synthesis_type *)
	(
	match property.synthesis_type with
	| Witness -> "#exhibit"
	| Synthesis -> "#synth"
	)
	
	^
	
	(
	(* Handle the actual property *)
	match property.property with
		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| EF state_predicate -> "EF(" ^ (string_of_state_predicate model state_predicate) ^ ")"
		(* Safety *)
		| AGnot state_predicate ->
			(* Print some information *)
(* 			print_message Verbose_high "Converting an AGnot property to a string…"; *)
			
			(* Convert the state predicate *)
			let state_predicate_str = string_of_state_predicate model state_predicate in
			
			(* Print some information *)
(* 			print_message Verbose_total "State predicate converted"; *)

			"AGnot(" ^ state_predicate_str ^ ")"

		
		(*------------------------------------------------------------*)
		(* Reachability and specification illustration *)
		(*------------------------------------------------------------*)
		
		(** EF-synthesis with examples of (un)safe words *)
		| EFexemplify state_predicate -> "EFexemplify(" ^ (string_of_state_predicate model state_predicate) ^ ")"


		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)
		
		(* Reachability with minimization of a parameter valuation *)
		| EFpmin (state_predicate , parameter_index) ->
			"EFpmin(" ^ (string_of_state_predicate model state_predicate) ^ ", " ^ (model.variable_names parameter_index) ^ ")"
		
		(* Reachability with maximization of a parameter valuation *)
		| EFpmax (state_predicate , parameter_index) ->
			"EFpmax(" ^ (string_of_state_predicate model state_predicate) ^ ", " ^ (model.variable_names parameter_index) ^ ")"
		
		(* Reachability with minimal-time *)
		| EFtmin state_predicate ->
			"EFpmin(" ^ (string_of_state_predicate model state_predicate) ^ ")"
		

		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)
		
		(** Infinite-run (cycle) *)
		| Cycle -> "inf_cycle"

		(** Accepting infinite-run (cycle) via an accepting keyword *)
		| Accepting_cycle -> "inf_acc_cycle"

		(** Accepting infinite-run (cycle) through a state predicate *)
		| Cycle_through state_predicate ->
		"inf_cycle_through(" ^ (string_of_state_predicate model state_predicate) ^ ")"
		
		(** Infinite-run (cycle) with non-Zeno assumption: method by checking whether the PTA is already a CUB-PTA for some valuation *)
		| NZCycle_check -> "NZ_inf_cycle_check"
		
		(** Infinite-run (cycle) with non-Zeno assumption: method by transforming the PTA into a CUB-PTA *)
		| NZCycle_transform -> "NZ_inf_cycle_transform"
		
		(** Infinite-run (cycle) with non-Zeno assumption: method assuming the PTA is already a CUB-PTA *)
		| NZCycle_CUB -> "NZ_inf_cycle_CUB"

		
		(*------------------------------------------------------------*)
		(* Deadlock-freeness *)
		(*------------------------------------------------------------*)
		
		(* Deadlock-free synthesis *)
		| Deadlock_Freeness -> "deadlockfree"
		
		
		(*------------------------------------------------------------*)
		(* Inverse method, trace preservation, robustness *)
		(*------------------------------------------------------------*)
		
		(* Inverse method with complete, non-convex result *)
		| IM pval -> "tracepreservation(" ^ (string_of_pval model pval) ^ ")"

		(* Non-complete, non-deterministic inverse method with convex result *)
		| ConvexIM pval -> "IMconvex(" ^ (string_of_pval model pval) ^ ")"

		| PRP (state_predicate, pval) -> "PRP(" ^ (string_of_state_predicate model state_predicate) ^ " , " ^ (string_of_pval model pval) ^ ")"

		| IMK pval -> "IMK(" ^ (string_of_pval model pval) ^ ")"

		| IMunion pval -> "IMunion(" ^ (string_of_pval model pval) ^ ")"

		
		(*------------------------------------------------------------*)
		(* Cartography algorithms *)
		(*------------------------------------------------------------*)
		
		(* Cartography *)
		| Cover_cartography (hyper_rectangle, step) ->
			"BCcover(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"

		(** Cover the whole cartography using learning-based abstractions *)
		| Learning_cartography (state_predicate, hyper_rectangle, step) ->
			"BClearn(" ^ (string_of_state_predicate model state_predicate) ^ " , " ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
		| Shuffle_cartography (hyper_rectangle, step) ->
			"BCshuffle(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		(** Look for the border using the cartography*)
		| Border_cartography (hyper_rectangle, step) ->
			"BCborder(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		(** Randomly pick up values for a given number of iterations *)
		| Random_cartography (hyper_rectangle, nb, step) ->
			"BCrandom(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (string_of_int nb)  ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
		| RandomSeq_cartography (hyper_rectangle, nb, step) ->
			"BCrandomseq(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (string_of_int nb)  ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"

		(* Parametric reachability preservation *)
		| PRPC (state_predicate , hyper_rectangle, step) ->
			"PRPC(" ^ (string_of_state_predicate model state_predicate) ^ " , " ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		

		(*** TODO ***)
(* 		| _ -> raise (NotImplemented "ModelPrinter.string_of_property for any other algorithm") *)
	)
	
	^
	
	(* Final (optional) semi-colon *)
	";"
	
	^
	
	(* Add the projection, if any *)
	(string_of_projection model property)


(************************************************************)
(** Model *)
(************************************************************)

(* Convert the model into a string *)
let string_of_model model =
	(* Print some information *)
(* 	print_message Verbose_total "Entering `ModelPrinter.string_of_model`…"; *)
	
	(* The header *)
	model_header ()
	(* The variable declarations *)
	^  "\n" ^ string_of_declarations model
	(* All automata *)
	^  "\n" ^ string_of_automata model
	(* The initial state *)
	^ "\n" ^ string_of_initial_state model
	(* The footer *)
	^  "\n" ^ footer



(************************************************************)
(** PX-valuation *)
(************************************************************)
(* Convert a valuation into a string *)
let string_of_valuation variables variable_names valuation =
	string_of_list_of_string_with_sep " & " (
		List.map (fun variable ->
			(variable_names variable)
			^ " = "
			^ (NumConst.string_of_numconst (valuation variable))
		) variables
	)

(* Convert a px-valuation into a string *)
let string_of_px_valuation model = string_of_valuation model.parameters_and_clocks model.variable_names

(* Convert an x-valuation into a string *)
let string_of_x_valuation model = string_of_valuation model.clocks model.variable_names






(************************************************************)
(** States *)
(************************************************************)

(* Convert a state into a string *)
let string_of_state model (state : state) =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	"" ^ (Location.string_of_location model.automata_names model.location_names model.variable_names options#output_float state.global_location) ^ " ==> \n&" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names state.px_constraint) ^ ""

let string_of_concrete_state model (state : State.concrete_state) =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	"" ^ (Location.string_of_location model.automata_names model.location_names model.variable_names options#output_float state.global_location) ^ " ==> \n" ^ (string_of_px_valuation model state.px_valuation) ^ ""
	


(************************************************************)
(** Debug-print for runs *)
(************************************************************)

(* Function to pretty-print combined transitions *)
let debug_string_of_combined_transition model combined_transition = string_of_list_of_string_with_sep ", " (
	List.map (fun transition_index ->
		(* Get automaton index *)
		let automaton_index = model.automaton_of_transition transition_index in
		(* Get actual transition *)
		let transition = model.transitions_description transition_index in
		(* Print *)
		debug_string_of_transition model automaton_index transition
	) combined_transition
)

(** Convert a symbolic run to a string (for debug-purpose) *)
let debug_string_of_symbolic_run model state_space (symbolic_run : StateSpace.symbolic_run) =
	(* Iterate *)
	let steps_string = string_of_list_of_string_with_sep "\n" (List.map (fun (symbolic_step : StateSpace.symbolic_step)  ->
		(* Get actual state *)
		let state = StateSpace.get_state state_space symbolic_step.source in
	
		  (" " ^ (string_of_state model state))
		^ ("\n | ")
		^ ("\n | via combined transition " ^ (debug_string_of_combined_transition model symbolic_step.transition))
		^ ("\n | ")
		^ ("\n v ")
	) symbolic_run.symbolic_steps) in
	
	(* Get the state *)
	let target_state = StateSpace.get_state state_space symbolic_run.final_state in
	
	(* Add target and return *)
	steps_string ^ (" " ^ (string_of_state model target_state))



let string_of_concrete_steps model concrete_steps =
	(* Iterate on following steps *)
	(string_of_list_of_string_with_sep "\n" (List.map (fun (concrete_step : StateSpace.concrete_step)  ->
		  ("\n | ")
		^ ("\n | via d = " ^ (NumConst.string_of_numconst concrete_step.time))
		^ ("\n | followed by combined transition " ^ (debug_string_of_combined_transition model concrete_step.transition))
		^ ("\n | ")
		^ ("\n v ")
		^ (" " ^ (string_of_concrete_state model concrete_step.target))
	) concrete_steps))


let string_of_impossible_concrete_steps model impossible_concrete_steps =
	(* Iterate on following steps *)
	(string_of_list_of_string_with_sep "\n" (List.map (fun (impossible_concrete_step : StateSpace.impossible_concrete_step)  ->
		  ("\n | ")
		^ ("\n | via d = " ^ (NumConst.string_of_numconst impossible_concrete_step.time))
		^ ("\n | followed by impossible transition labeled with " ^ (model.action_names impossible_concrete_step.action))
		^ ("\n | ")
		^ ("\n v ")
		^ (" " ^ (string_of_concrete_state model impossible_concrete_step.target))
	) impossible_concrete_steps))



(** Convert a concrete run to a string (for debug-purpose) *)
let debug_string_of_concrete_run model (concrete_run : StateSpace.concrete_run) =
	(* First recall the parameter valuation *)
	"Concrete run for parameter valuation:"
	^ "\n" ^ (string_of_pval model concrete_run.p_valuation)
	
	^ "\n"
	
	(* Then print the initial state *)
	^ "\n" ^ (string_of_concrete_state model concrete_run.initial_state)
	
	(* Iterate on following steps *)
	^ (string_of_concrete_steps model concrete_run.steps)
	

	
(** Convert an impossible_concrete_run to a string (for debug-purpose) *)
let debug_string_of_impossible_concrete_run model (impossible_concrete_run : StateSpace.impossible_concrete_run) =
	(* First recall the parameter valuation *)
	"Impossible concrete run for parameter valuation:"
	^ "\n" ^ (string_of_pval model impossible_concrete_run.p_valuation)
	
	^ "\n"
	
	(* Then print the initial state *)
	^ "\n" ^ (string_of_concrete_state model impossible_concrete_run.initial_state)
	
	(* Iterate on following concrete steps *)
	^ (string_of_concrete_steps model impossible_concrete_run.steps)
	
	(* Iterate on following impossible steps *)
	^ (string_of_impossible_concrete_steps model impossible_concrete_run.impossible_steps)
	
