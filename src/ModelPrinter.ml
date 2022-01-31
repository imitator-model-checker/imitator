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
 * Last modified     : 2022/02/09
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
(** Getting the flows of a location *)
(************************************************************)

(*** BADPROG: very, very bad programming: this function should be in AlgoStateBased BUT ModelPrinter doesn't have access to AlgoStateBased (but the other way is possible); and it is called from both modules, so defined here (ÉA, 2021/11/02) ***)

(*------------------------------------------------------------*)
(* Compute a hashtable clock => flow in a Location.global_location *)
(* Also returns a Boolean being true iff there is any non-1 flow *)
(* Raises a warning whenever a clock is assigned to TWO different flows *)
(*------------------------------------------------------------*)
let compute_flows_gen (location : Location.global_location) : (((Automaton.clock_index, NumConst.t) Hashtbl.t) * bool) =
	(* Retrieve the model *)
	let model = Input.get_model() in

	(* Hashtbl clock_id --> flow *)
	let flows_hashtable = Hashtbl.create (List.length model.clocks) in
	
	(* Maintain a Boolean to see if any clock has a rate different from 1 *)
	let flow_mode = ref false in
	
	(* Update hash table *)
	List.iter (fun automaton_index ->
		(* Get the current location *)
		let location_index = Location.get_location location automaton_index in
		
		(* 1. Manage the list of stopped clocks *)
		let stopped = model.stopwatches automaton_index location_index in
		(* If list non null: we have flows <> 1 *)
		if stopped <> [] then flow_mode := true;
		(* Add each clock *)
		List.iter (fun stopwatch_id ->
			Hashtbl.replace flows_hashtable stopwatch_id NumConst.zero
		) stopped;
		
		(* 2. Manage the explicit flows *)
		let flows = model.flow automaton_index location_index in
		(* Add each clock *)
		List.iter (fun (clock_id, flow_value) ->
			(* If flow <> 1, update Boolean *)
			if NumConst.neq flow_value NumConst.one then flow_mode := true;

			(* Compare with previous value *)
			try(
				(* Get former value *)
				let former_flow_value = Hashtbl.find flows_hashtable clock_id in
				(* Compare *)
				if NumConst.neq former_flow_value flow_value then(
					
					(*** TODO: a flag should be raised somewhere so that the result is said to be perhaps wrong! (or unspecified) ***)
					
					print_warning ("Clock `" ^ (model.variable_names clock_id) ^ "` is assigned to two different flow values at the same time (`" ^ (NumConst.string_of_numconst flow_value) ^ "` in location `" ^ (model.location_names automaton_index location_index) ^ "`, as well as `" ^ (NumConst.string_of_numconst former_flow_value) ^ "`). The behavior becomes unspecified!");
				);
				(* Do not add *)
				()
			) with Not_found ->(
			(* Not found: not yet defined => add *)
				Hashtbl.add flows_hashtable clock_id flow_value
			);
			
		) flows;
		
	) model.automata;
	
	(* Return the hash table and the Boolean *)
	flows_hashtable , !flow_mode


(*------------------------------------------------------------*)
(* Compute the list of clocks with their flow in a Location.global_location *)
(* Returns a list of pairs (clock_index, flow)                *)
(* Raises a warning whenever a clock is assigned to TWO different flows *)
(*------------------------------------------------------------*)
let compute_flows_list (location : Location.global_location) : ((Automaton.clock_index * NumConst.t) list) =
	(* Call generic function *)
	let flows_hashtable, non_1_flow = compute_flows_gen location in

	(* Retrieve the model *)
	let model = Input.get_model() in

	(* If there are no explicit flows then just return the set of clocks with flow 1 *)
	if (not non_1_flow) then (List.map (fun clock_id -> clock_id, NumConst.one) model.clocks) else (
		(* Computing the list of clocks with their flow *)
		List.map (fun clock_id ->
			(* Try to get the clock explicit flow *)
			try(
				(* Get value *)
				let flow_value = Hashtbl.find flows_hashtable clock_id in
				(* Return *)
				clock_id, flow_value
			) with Not_found ->
				(* Absent: flow is 1 *)
				clock_id, NumConst.one
		) model.clocks
	) (* if no explicit flow for this location *)



(*------------------------------------------------------------*)
(* Compute the functional flow function in a Location.global_location *)
(* Returns a function clock_index -> flow                     *)
(* Raises a warning whenever a clock is assigned to TWO different flows *)
(*------------------------------------------------------------*)
let compute_flows_fun (location : Location.global_location) : (Automaton.clock_index -> NumConst.t) =
	(* Call generic function *)
	let flows_hashtable, non_1_flow = compute_flows_gen location in

	(* If there are no explicit flows then just return the set of clocks with flow 1 *)
	if (not non_1_flow) then (fun clock_id -> NumConst.one) else (
		(* Return the functional view *)
		(fun clock_id ->
			(* Try to get the clock explicit flow *)
			try(
				(* Try to get value *)
				Hashtbl.find flows_hashtable clock_id
			) with Not_found ->
				(* Absent: flow is 1 *)
				NumConst.one
		)
	) (* if no explicit flow for this location *)


(************************************************************)
(** Constants *)
(************************************************************)
let string_of_true		= "True"
let string_of_false		= "False"
let string_of_accepting	= "accepting"



(************************************************************)
(** JSON *)
(************************************************************)
let json_NULL	= "null" (*** NOTE: no quotes ***)
let json_TRUE	= "true"
let json_FALSE	= "false"

let json_of_string s = "\"" ^ s ^ "\""

let json_escape_ampersand str =
	(Str.global_replace (Str.regexp "\n") (" ")
		(Str.global_replace (Str.regexp "&") ("AND") str)
	)

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

(* Convert a parameter valuation (PVal.pval) into a JSON-like string *)
let json_of_pval model pval =
	(* Hack for empty model *)
	if model.nb_parameters = 0 then json_NULL
	else(
		"{" ^ (
		string_of_list_of_string_with_sep "," (
			List.map (fun parameter ->
				"\n\t\t\t"
				^ (json_of_string (model.variable_names parameter))
				^ ": "
				^ (json_of_string (NumConst.string_of_numconst (pval#get_value parameter)))
			) model.parameters
		)
		)
		^ "\n\t\t\t}"
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

(* Convert a var_type_discrete into a string *)
let string_of_var_type_discrete = DiscreteType.string_of_var_type_discrete

(* Convert a var_type into a string *)
let string_of_var_type = DiscreteType.string_of_var_type

(* Convert discrete variable declarations group (by type) into a string *)
let string_of_discrete_variables_by_type var_type variable_names =
	if List.length variable_names > 0 then (
	    (* Get string of all variable names of the group *)
	    let string_of_variable_names = string_of_list_of_string_with_sep ", " variable_names in
	    (* Get string of declaration group *)
		("\n\t" ^ string_of_variable_names ^ "\n\t\t: " ^ (string_of_var_type var_type) ^ ";\n")
    )
    else ""

(* Convert discrete variable declarations into a string *)
let string_of_discrete_variables model =
    (* Get string of each variable groups (by type) *)
    let str = List.map (fun (var_type, variable_names) -> string_of_discrete_variables_by_type var_type variable_names) model.discrete_names_by_type_group in
    (* Join all strings *)
    string_of_list_of_string str

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
	(string_of_discrete_variables model)
	^
	(if model.nb_parameters > 0 then
		("\n\t" ^ (string_of_variables model.parameters) ^ "\n\t\t: parameter;\n") else "")

(* Get string of a global expression *)
let string_of_global_expression = DiscreteExpressions.string_of_global_expression
(* Get string of an arithmetic expression *)
let string_of_arithmetic_expression = DiscreteExpressions.string_of_arithmetic_expression
(* Get string of a boolean expression *)
let string_of_boolean_expression = DiscreteExpressions.string_of_boolean_expression
(* Get string of a discrete boolean expression *)
let string_of_discrete_boolean_expression = DiscreteExpressions.string_of_discrete_boolean_expression


(************************************************************)
(** Guard *)
(************************************************************)

(*** NOTE: special handling as we have a discrete and a continuous guard that must be handled homogeneously ***)

let customized_string_of_guard customized_boolean_string variable_names = function
	| True_guard -> LinearConstraint.string_of_true
	| False_guard -> LinearConstraint.string_of_false
	| Discrete_guard discrete_guard -> NonlinearConstraint.customized_string_of_nonlinear_constraint customized_boolean_string variable_names discrete_guard
	| Continuous_guard continuous_guard -> LinearConstraint.string_of_pxd_linear_constraint variable_names continuous_guard
	| Discrete_continuous_guard discrete_continuous_guard ->
		(NonlinearConstraint.customized_string_of_nonlinear_constraint customized_boolean_string variable_names discrete_continuous_guard.discrete_guard)
		^ LinearConstraint.string_of_and ^
		(LinearConstraint.string_of_pxd_linear_constraint variable_names discrete_continuous_guard.continuous_guard)

(** Convert a guard into a string *)
let string_of_guard = customized_string_of_guard Constants.global_default_string
(*
let string_of_guard variable_names = function
	| True_guard -> LinearConstraint.string_of_true
	| False_guard -> LinearConstraint.string_of_false
	| Discrete_guard discrete_guard -> NonlinearConstraint.string_of_nonlinear_constraint variable_names discrete_guard
	| Continuous_guard continuous_guard -> LinearConstraint.string_of_pxd_linear_constraint variable_names continuous_guard
	| Discrete_continuous_guard discrete_continuous_guard ->
		(NonlinearConstraint.string_of_nonlinear_constraint variable_names discrete_continuous_guard.discrete_guard)
		^ LinearConstraint.string_of_and ^
		(LinearConstraint.string_of_pxd_linear_constraint variable_names discrete_continuous_guard.continuous_guard)
*)


(** Convert a guard into a JSON-style string *)
let json_of_guard variable_names guard =
	let customized_boolean_string = Constants.global_default_string in
	match guard with
	| True_guard -> json_TRUE
	| False_guard -> json_FALSE
	| Discrete_guard discrete_guard -> json_escape_ampersand (NonlinearConstraint.customized_string_of_nonlinear_constraint customized_boolean_string variable_names discrete_guard)
	| Continuous_guard continuous_guard -> json_escape_ampersand (LinearConstraint.string_of_pxd_linear_constraint variable_names continuous_guard)
	| Discrete_continuous_guard discrete_continuous_guard ->
		(*** HACK for now (2021/12/09): just replace the " & " with some suited string ***)
		(json_escape_ampersand (NonlinearConstraint.customized_string_of_nonlinear_constraint customized_boolean_string variable_names discrete_continuous_guard.discrete_guard))
		^ " AND " ^
		(json_escape_ampersand (LinearConstraint.string_of_pxd_linear_constraint variable_names discrete_continuous_guard.continuous_guard))


(************************************************************)
(** Automata *)
(************************************************************)

(* Convert the actions declaration of an automaton into a string *)
let string_of_actions_declaration model automaton_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_actions_declaration(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

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
	^ (string_of_guard model.variable_names (model.invariants automaton_index location_index))

	(* Handle stopwatches *)
	^
	let stopped = model.stopwatches automaton_index location_index in
	(* Case 1: no stopwatches *)
	if stopped = [] then ""
	(* Case 2: some clocks stopped *)
	else
	let stopped_str = string_of_list_of_string_with_sep "," (List.map model.variable_names stopped) in
	" stop{" ^ stopped_str ^ "}"

	(* Handle flow *)
	^
	let flow = model.flow automaton_index location_index in
	(* Case 1: no explicit flow *)
	if flow = [] then ""
	(* Case 2: some flow *)
	else
	let flow_str = string_of_list_of_string_with_sep "," (List.map (fun (variable_index, flow_value) -> (model.variable_names variable_index) ^ "' = " ^ (NumConst.string_of_numconst flow_value) ) flow) in
	" flow{" ^ flow_str ^ "}"


(* Convert an action into a string *)
let string_of_action model (action_index : Automaton.action_index) =
	match model.action_types action_index with
	| Action_type_sync -> " sync " ^ (model.action_names action_index)
	| Action_type_nosync -> " (* sync " ^ (model.action_names action_index) ^ "*) "

(* Convert an action into a JSON-like string *)
let json_of_action model (action_index : Automaton.action_index) =
	match model.action_types action_index with
	| Action_type_sync -> (model.action_names action_index)
	| Action_type_nosync -> "(silent)"


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

(** Convert a clock update into a JSON-like string *)
let json_of_clock_updates model clock_updates =
	let sep = "," in
	let wrap_reset variable_index = "\n\t\t\t\t\t\t\t" ^ (json_of_string (model.variable_names variable_index)) ^ ": " ^ (json_of_string "0") ^ "" in
	let wrap_expr variable_index linear_term = "\n\t\t\t\t\t\t\t" ^ (json_of_string (model.variable_names variable_index)) ^ ": " ^ (json_of_string (LinearConstraint.string_of_pxd_linear_term model.variable_names linear_term)) ^ "" in
	string_of_clock_updates_template model clock_updates wrap_reset wrap_expr sep




(* Access to either variable or array *)
let rec string_of_variable_update_type model = function
	| Variable_update (variable_index) ->
		model.variable_names variable_index
	| Indexed_update (variable_access, index_expr) ->
		string_of_variable_update_type model variable_access
		^ "[" ^ DiscreteExpressions.string_of_int_arithmetic_expression model.variable_names index_expr  ^ "]"
    | Void_update -> ""

(* Convert a list of discrete updates into a string *)
let string_of_discrete_updates ?(sep=", ") model updates =
	string_of_list_of_string_with_sep sep (List.rev_map (fun (variable_access, arithmetic_expression) ->
		(* Convert the variable name *)
		string_of_variable_update_type model variable_access
		^ " := "
		(* Convert the arithmetic_expression *)
		^ (DiscreteExpressions.string_of_global_expression model.variable_names arithmetic_expression)
	) updates)

(* Convert a list of discrete updates into a JSON-like string *)
let json_of_discrete_updates ?(sep=", ") model updates =
	string_of_list_of_string_with_sep sep (List.rev_map (fun (variable_access, arithmetic_expression) ->
		(* Convert the variable name *)
		"" ^ (json_of_string (string_of_variable_update_type model variable_access)) ^ ""
		^ ": "
		(* Convert the arithmetic_expression *)
		^ "" ^ (json_of_string (DiscreteExpressions.string_of_global_expression model.variable_names arithmetic_expression)) ^ ""
	) updates)

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
	let wrap_if boolean_expr  = "if (" ^ (DiscreteExpressions.string_of_boolean_expression model.variable_names boolean_expr) ^  ") then " in
	let wrap_else = " else " in
	let wrap_end = " end" in
	let sep = ", " in
	string_of_conditional_updates_template model conditional_updates string_of_clock_updates string_of_discrete_updates wrap_if wrap_else wrap_end sep

(** Convert a list of conditional updates into a JSON-like string *)
(*** WARNING: not really supported ***)
let json_of_conditional_updates model conditional_updates =
	if conditional_updates <> [] then(
		print_warning "Conditional updates not (really) supported in the JSON export!";
		(* Do our best to still export something *)
		""
		^ "\n\t\t\t\t\t\t\t" ^ (json_of_string "conditional") ^ ": {"
		^ "\n\t\t\t\t\t\t\t\t" ^ (json_of_string "update")  ^ ": " ^ (json_of_string (string_of_conditional_updates model conditional_updates)) ^ ""
		^ "\n\t\t\t\t\t\t\t}"
	)else ""

(* Convert a transition into a string *)
let string_of_transition model automaton_index (transition : transition) =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_transition(" ^ (model.automata_names automaton_index) ^ ")` with target `" ^ (model.location_names automaton_index transition.target) ^ "` via action `" ^ (string_of_action model transition.action) ^ "`…"); *)

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
	^ (string_of_action model transition.action)
	(* Convert the target location *)
	^ " goto " ^ (model.location_names automaton_index transition.target)
	^ ";"


(* Convert a transition into a string: version for runs *)
let string_of_transition_for_runs model automaton_index (transition : transition) =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_transition(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

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
	^ (string_of_action model transition.action)
	(* Convert the target location *)
	^ " Target " ^ (model.location_names automaton_index transition.target)
	^ "] "

(* Convert a transition into a JSON-like string *)
let json_of_transition model automaton_index (transition : transition) =
	let clock_updates = transition.updates.clock in
	let discrete_updates = transition.updates.discrete in
	let conditional_updates = transition.updates.conditional in

	""
	(* Begin transition *)
	^ "\n\t\t\t\t\t{"
	^ "\n\t\t\t\t\t" ^ (json_of_string "transition") ^ ": {"
	
	(* PTA name *)
	^ "\n\t\t\t\t\t\t" ^ (json_of_string "PTA") ^ ": " ^ (json_of_string (model.automata_names automaton_index)) ^ ","
	
	(* Guard *)
	^ "\n\t\t\t\t\t\t" ^ (json_of_string "guard") ^ ": " ^ (json_of_string (json_of_guard model.variable_names transition.guard)) ^ ","
	
	(* Updates *)
	^ "\n\t\t\t\t\t\t" ^ (json_of_string "updates") ^ ": {"
	(* Clock updates *)
	^ (json_of_clock_updates model clock_updates)
	(* Discrete updates *)
	^ (json_of_discrete_updates model discrete_updates)
	(* Conditional updates *)
	^ (json_of_conditional_updates model conditional_updates)
	^ "\n\t\t\t\t\t\t}"
	
(* 	(* Convert the target location *) *)
(* 	^ " Target " ^ (model.location_names automaton_index transition.target) *)

	(* end transition *)
	^ "\n\t\t\t\t\t}"
	^ "\n\t\t\t\t\t}"



(* Convert the transitions of a location into a string *)
let string_of_transitions model automaton_index location_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_transitions(" ^ (model.automata_names automaton_index) ^ ": " ^ (model.location_names automaton_index location_index) ^ ")`…"); *)

	string_of_list_of_string (
	(* For each action *)
	List.map (fun action_index ->
		(* Print some information *)
(* 		print_message Verbose_total ("Retrieving transitions via `" ^ (string_of_action model action_index) ^ "`…"); *)

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
	^ "\n " ^ (string_of_actions_declaration model automaton_index)
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


let rec customized_string_of_variable_access customized_string model = function
    | Variable_update variable_index -> model.variable_names variable_index
    | Indexed_update (variable_access, index_expr) ->
        customized_string_of_variable_access customized_string model variable_access ^ "[" ^ DiscreteExpressions.customized_string_of_int_arithmetic_expression customized_string model.variable_names index_expr ^ "]"
    | Void_update -> ""

let string_of_variable_access = customized_string_of_variable_access Constants.global_default_string

(* Convert initial state of locations to string *)
let string_of_new_initial_locations ?indent_level:(i=1) model =
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
		let tabulations = string_n_times i "\t" in
		tabulations ^ "loc[" ^ (model.automata_names automaton_index) ^ "] := " ^ (model.location_names automaton_index initial_location)
	) pta_without_obs
	in string_of_list_of_string_with_sep ", \n" initial_automata

(* Convert initial state of discrete variables to string *)
let string_of_new_initial_discretes ?indent_level:(i=1) model =
	let initial_discrete = List.map
	(fun discrete_index ->
		(* Finding the initial value for this discrete *)
		let initial_value = Location.get_discrete_value model.initial_location discrete_index in
		(* '& var = val' *)
		let tabulations = string_n_times i "\t" in
		tabulations ^ (model.variable_names discrete_index) ^ " := " ^ (DiscreteValue.string_of_value initial_value)
	) model.discrete
	in string_of_list_of_string_with_sep ", \n" initial_discrete

(************************************************************)
(** Initial state *)
(************************************************************)
let string_of_old_initial_state model =
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
		"\n\t& " ^ (model.variable_names discrete_index) ^ " = " ^ (DiscreteValue.string_of_value initial_value)
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
(** New initial state since version 3.1 *)
(************************************************************)
let string_of_initial_state model =
	(* Header of initial state *)
	"\n"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ "(* Initial state *)"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ ""
	^ "\n" ^ "init := {"
    ^ "\n"
	(* Discrete zone *)
    ^ "\n" ^ "\tdiscrete = "
	^ "\n" ^ "\t\t(*------------------------------------------------------------*)"
    ^ "\n" ^ "\t\t(* Initial location *)"
	^ "\n" ^ "\t\t(*------------------------------------------------------------*)"
    ^ "\n"
    ^ (string_of_new_initial_locations ~indent_level:2 model) ^ ","
	^ "\n" ^ "\t\t(*------------------------------------------------------------*)"
    ^ "\n" ^ "\t\t(* Initial discrete variables assignments *)"
	^ "\n" ^ "\t\t(*------------------------------------------------------------*)"
    ^ "\n"
    ^ (string_of_new_initial_discretes ~indent_level:2 model)
    ^ "\n" ^ "\t;"
    ^ "\n"
	(* Continuous zone *)
	^ "\n" ^ "\t(*------------------------------------------------------------*)"
	^ "\n" ^ "\t(* Initial continuous constraint *)"
	^ "\n" ^ "\t(*------------------------------------------------------------*)"

    ^ "\n" ^ "\tcontinuous = "
    ^ "\n\t\t& " ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names model.initial_constraint)
    ^ "\n" ^ "\t;"
    ^ "\n"
	^ "\n" ^ "}"

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

	| State_predicate_true -> string_of_true
	
	| State_predicate_false -> string_of_false
	
	| State_predicate_accepting -> string_of_accepting

	
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
	| Exemplification	-> "#exemplify"
	| Synthesis			-> "#synth"
	| Witness			-> "#exhibit"
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
		
		(** Accepting infinite-run (cycle) through a state predicate *)
		| Cycle_through state_predicate ->
			if state_predicate = (State_predicate_term (State_predicate_factor (Simple_predicate State_predicate_true))) then "Cycle"
			else "CycleThrough(" ^ (string_of_state_predicate model state_predicate) ^ ")"
		
		(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
		| Cycle_through_generalized state_predicate_list ->
			"CycleThrough(" ^ (string_of_list_of_string_with_sep " , " (List.map (string_of_state_predicate model) state_predicate_list)) ^ ")"
		
		(** Infinite-run (cycle) with non-Zeno assumption *)
		| NZ_Cycle -> "NZCycle"
		
		
		(*------------------------------------------------------------*)
		(* Deadlock-freeness *)
		(*------------------------------------------------------------*)
		
		(* Deadlock-free synthesis *)
		| Deadlock_Freeness -> "DeadlockFree"
		
		
		(*------------------------------------------------------------*)
		(* Inverse method, trace preservation, robustness *)
		(*------------------------------------------------------------*)
		
		(* Inverse method with complete, non-convex result *)
		| IM pval -> "TracePreservation(" ^ (string_of_pval model pval) ^ ")"

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

(* Convert a valuation into a JSON-like string *)
let json_of_valuation variables variable_names valuation =
	string_of_list_of_string_with_sep "," (
		List.map (fun variable ->
			"\n\t\t\t\t\t"
			^ (json_of_string (variable_names variable)) ^ ""
			^ ": "
			^ "" ^ (json_of_string (NumConst.string_of_numconst (valuation variable))) ^ ""
		) variables
	)

(* Convert a px-valuation into a string *)
let string_of_px_valuation model = string_of_valuation model.parameters_and_clocks model.variable_names

(* Convert an x-valuation into a string *)
let string_of_x_valuation model = string_of_valuation model.clocks model.variable_names

(* Convert a px-valuation into a JSON-like string *)
let json_of_px_valuation model = json_of_valuation model.parameters_and_clocks model.variable_names






(************************************************************)
(** States *)
(************************************************************)

(* Convert a state into a string *)
let string_of_state model (state : state) =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	"" ^ (Location.string_of_location model.automata_names model.location_names model.variable_names (if options#output_float then Location.Float_display else Location.Exact_display) state.global_location) ^ " ==> \n&" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names state.px_constraint) ^ ""


(* Convert a concrete state (locations, discrete variables valuations, continuous variables valuations, current flows for continuous variables) *)
let string_of_concrete_state model (state : State.concrete_state) =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	""
	(* Convert location *)
	^ (Location.string_of_location model.automata_names model.location_names model.variable_names (if options#output_float then Location.Float_display else Location.Exact_display) state.global_location)
	(* Convert variables valuations *)
	^ " ==> \n" ^ (string_of_px_valuation model state.px_valuation)
	(* Convert rates *)
	^ " flows["
	^ (
		let global_location : Location.global_location = state.global_location in
		let flows : (Automaton.clock_index * NumConst.t) list = compute_flows_list global_location in
		(* Iterate *)
		string_of_list_of_string_with_sep ", " (
			List.map (fun (variable_index, flow) -> (model.variable_names variable_index ) ^ "' = "  ^ (NumConst.string_of_numconst flow) ) flows
		)
	)
	^ "]"

(* Convert a global location into JSON-style string (locations, NO discrete variables valuations) *)
let json_of_global_location model (global_location : Location.global_location) =
	string_of_list_of_string_with_sep ", " (
		List.map (fun automaton_index ->
			(* Retrieve location for `automaton_index` *)
			let location_index = Location.get_location global_location automaton_index in
			
			(* Get names *)
			let automaton_name = model.automata_names automaton_index in
			let location_name = model.location_names automaton_index location_index in
			
			(* Convert *)
			"\n\t\t\t\t\t" ^ (json_of_string automaton_name) ^ ": " ^ (json_of_string location_name) ^ ""
		) model.automata
	)

(* Convert the values of the discrete variables in a global location into JSON-style string *)
let json_of_discrete_values model (global_location : Location.global_location) =
	string_of_list_of_string_with_sep ", " (
		List.map (fun discrete_index ->
			(* Retrieve valuation for `discrete_index` *)
			let variable_value = Location.get_discrete_value global_location discrete_index in
			
			(* Convert to strings *)
			let variable_name = model.variable_names discrete_index in
			let variable_valuation = DiscreteValue.string_of_value variable_value in
			
			(* Convert *)
			"\n\t\t\t\t\t" ^ (json_of_string variable_name) ^ ": " ^ (json_of_string variable_valuation) ^ ""
		) model.discrete
	)



(* Convert a concrete state into JSON-style string (locations, discrete variables valuations, continuous variables valuations, current flows for continuous variables) *)
let json_of_concrete_state model (state : State.concrete_state) =
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)
	
	""
	
	(* Begin state *)
	^ "\n\t\t\t{"
	^ "\n\t\t\t" ^ (json_of_string "state") ^ ": {"

	(* Convert location *)
	^ "\n\t\t\t\t" ^ (json_of_string "location") ^ ": {" ^ (json_of_global_location model state.global_location)
	^ "\n\t\t\t\t}," (* end locations *)
	
	(* Convert discrete variables *)
	^ "\n\t\t\t\t" ^ (json_of_string "discrete_variables") ^ ": {" ^ (json_of_discrete_values model state.global_location) (*** TODO: float? ***)
	^ "\n\t\t\t\t}," (* end discrete *)
	
	(* Convert continuous variables valuations *)
	^ "\n\t\t\t\t" ^ (json_of_string "continuous_variables") ^ ": {" ^ (json_of_px_valuation model state.px_valuation)
	^ "\n\t\t\t\t}," (* end continuous variables *)

	(* Convert rates *)
	^ "\n\t\t\t\t" ^ (json_of_string "flows") ^ ": {"
	^ (
		let global_location : Location.global_location = state.global_location in
		let flows : (Automaton.clock_index * NumConst.t) list = compute_flows_list global_location in
		(* Iterate *)
		string_of_list_of_string_with_sep ", " (
			List.map (fun (variable_index, flow) ->
				"\n\t\t\t\t\t" ^ (json_of_string (model.variable_names variable_index )) ^ ": " ^ (json_of_string (NumConst.string_of_numconst flow)) ^ ""
			) flows
		)
	)
	^ "\n\t\t\t\t}" (* end flows *)

	(* End state *)
	^ "\n\t\t\t}"
	^ "\n\t\t\t}"


(************************************************************)
(** Runs conversion to strings *)
(************************************************************)


(* Function to pretty-print combined transitions *)
let debug_string_of_combined_transition model combined_transition = string_of_list_of_string_with_sep ", " (
	List.map (fun transition_index ->
		(* Get automaton index *)
		let automaton_index = model.automaton_of_transition transition_index in
		(* Get actual transition *)
		let transition = model.transitions_description transition_index in
		(* Convert *)
		string_of_transition_for_runs model automaton_index transition
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




let debug_string_of_concrete_steps model concrete_steps =
	(* Iterate on following steps *)
	(string_of_list_of_string_with_sep "\n" (List.map (fun (concrete_step : StateSpace.concrete_step)  ->
		  ("\n | ")
		^ ("\n | via d = " ^ (NumConst.string_of_numconst concrete_step.time))
		^ ("\n | followed by combined transition " ^ (debug_string_of_combined_transition model concrete_step.transition))
		^ ("\n | ")
		^ ("\n v ")
		^ (" " ^ (string_of_concrete_state model concrete_step.target))
	) concrete_steps))


let debug_string_of_arbitrary_or_impossible_concrete_step_gen (step_description : string) model (impossible_concrete_step : StateSpace.impossible_concrete_step) =
		("\n | ")
	^ ("\n | via d = " ^ (NumConst.string_of_numconst impossible_concrete_step.time))
	^ ("\n | followed by " ^ step_description ^ " transition labeled with " ^ (model.action_names impossible_concrete_step.action))
	^ ("\n | ")
	^ ("\n v ")
	^ (" " ^ (string_of_concrete_state model impossible_concrete_step.target))

let debug_string_of_impossible_concrete_step = debug_string_of_arbitrary_or_impossible_concrete_step_gen "impossible"

let debug_string_of_arbitrary_concrete_steps model impossible_concrete_steps =
	(* Iterate on following steps *)
	(string_of_list_of_string_with_sep "\n" (List.map (fun (impossible_concrete_step : StateSpace.impossible_concrete_step)  ->
		debug_string_of_arbitrary_or_impossible_concrete_step_gen "arbitrary" model impossible_concrete_step
	) impossible_concrete_steps))



(** Convert a concrete run to a string (for debug-purpose) *)
let debug_string_of_concrete_run model (concrete_run : StateSpace.concrete_run) =
	(* First recall the parameter valuation *)
	"Concrete run for parameter valuation:"
	^ "\n" ^ (string_of_pval model concrete_run.p_valuation)
	
	^ "\n"
	
	(* Then convert the initial state *)
	^ "\n" ^ (string_of_concrete_state model concrete_run.initial_state)
	
	(* Iterate on following steps *)
	^ (debug_string_of_concrete_steps model concrete_run.steps)



(** Convert an impossible_concrete_run to a string (for debug-purpose) *)
let debug_string_of_impossible_concrete_run model (impossible_concrete_run : StateSpace.impossible_concrete_run) =
	(* First recall the parameter valuation *)
	"Impossible concrete run for parameter valuation:"
	^ "\n" ^ (string_of_pval model impossible_concrete_run.p_valuation)
	
	^ "\n"
	
	(* Then print the initial state *)
	^ "\n" ^ (string_of_concrete_state model impossible_concrete_run.initial_state)
	
	(* Iterate on following concrete steps *)
	^ (debug_string_of_concrete_steps model impossible_concrete_run.steps)
	
	(*** NOTE: only the first step is impossible; others are "arbitrary" ***)
	^ (match impossible_concrete_run.impossible_steps with
	| [] -> ""
	| first_step :: following_steps ->
		(* Convert the first impossible step *)
		(debug_string_of_impossible_concrete_step model first_step)

		(* Iterate on following impossible steps *)
		^ (debug_string_of_arbitrary_concrete_steps model following_steps)
	)


(************************************************************)
(** Runs conversion to JSON *)
(************************************************************)


(* Function to pretty-print combined transitions *)
let json_of_combined_transition model combined_transition =
	""
	^ "\n\t\t\t\t" ^ (json_of_string "transitions") ^ ": ["

	^ (
		string_of_list_of_string_with_sep ", " (
			List.map (fun transition_index ->
				(* Get automaton index *)
				let automaton_index = model.automaton_of_transition transition_index in
				
				(* Get actual transition *)
				let transition = model.transitions_description transition_index in
				
				(* Convert *)
				json_of_transition model automaton_index transition
			) combined_transition
		)
	)
	^ "\n\t\t\t\t]"



let json_of_concrete_steps model concrete_steps =
	(* Iterate on following steps *)
	(string_of_list_of_string_with_sep ", " (List.map (fun (concrete_step : StateSpace.concrete_step)  ->
		(* Get the action: a little tricky, as it is stored in *each* of transition of the combined transition *)
		(*** NOTE: we get it arbitrarily from the first transition of the combined transition ***)
		let first_transition_index : AbstractModel.transition_index = (List.hd (concrete_step.transition)) in
		let first_transition : AbstractModel.transition = model.transitions_description first_transition_index in
		let action_index : Automaton.action_index = first_transition.action in
		(* Convert action to string *)
		let action_name = json_of_action model action_index in
		
		""
		
		(* Begin transition *)
		^ "\n\t\t\t{"
		^ "\n\t\t\t" ^ (json_of_string "transition") ^ ": {"
		^ "\n\t\t\t\t" ^ (json_of_string "nature") ^ ": " ^ (json_of_string ("concrete")) ^ ","
		^ "\n\t\t\t\t" ^ (json_of_string "duration") ^ ": " ^ (json_of_string (NumConst.string_of_numconst concrete_step.time)) ^ ","
		^ "\n\t\t\t\t" ^ (json_of_string "action") ^ ": " ^ (json_of_string action_name) ^ ","
		^ (json_of_combined_transition model concrete_step.transition) ^ ""
		(* End transition *)
		^ "\n\t\t\t}"
		^ "\n\t\t\t}"
		
		^ ","
		
		(* Target state *)
		^ (json_of_concrete_state model concrete_step.target)
	) concrete_steps))

let json_of_arbitrary_or_impossible_concrete_step_gen (step_description : string) model (impossible_concrete_step : StateSpace.impossible_concrete_step) =
	(* Convert action to string *)
	let action_name = json_of_action model impossible_concrete_step.action in
	
	""
	
	(* Begin transition *)
	^ "\n\t\t\t{"
	^ "\n\t\t\t" ^ (json_of_string "transition") ^ ": {"
	^ "\n\t\t\t\t" ^ (json_of_string "nature") ^ ": " ^ (json_of_string step_description) ^ ","
	^ "\n\t\t\t\t" ^ (json_of_string "duration") ^ ": " ^ (json_of_string (NumConst.string_of_numconst impossible_concrete_step.time)) ^ ","
	^ "\n\t\t\t\t" ^ (json_of_string "action") ^ ": " ^ (json_of_string action_name)
	(* End transition *)
	^ "\n\t\t\t}"
	^ "\n\t\t\t}"
	
	^ ","
	
	(* Target state *)
	^ (json_of_concrete_state model impossible_concrete_step.target)

let json_of_impossible_concrete_step = json_of_arbitrary_or_impossible_concrete_step_gen "impossible"

let json_of_arbitrary_concrete_steps model impossible_concrete_steps =
	(* Iterate on following steps *)
	(string_of_list_of_string_with_sep "\n" (List.map (fun (impossible_concrete_step : StateSpace.impossible_concrete_step)  ->
		json_of_arbitrary_or_impossible_concrete_step_gen "arbitrary" model impossible_concrete_step
	) impossible_concrete_steps))



(** Convert a concrete run to a JSON-style string *)
let json_of_concrete_run model (concrete_run : StateSpace.concrete_run) =
	(* First recall the parameter valuation *)
	"{"
	^ "\n\t" ^ (json_of_string "run") ^ ": {"
	^ "\n\t\t" ^ (json_of_string "nature") ^ ": " ^ (json_of_string "concrete") ^ ","
	^ "\n\t\t" ^ (json_of_string "valuation") ^ ": " ^ (json_of_pval model concrete_run.p_valuation) ^ ","
	
(* 	^ "\n" *)
	^ "\n\t\t" ^ (json_of_string "steps") ^ ": ["
	
	(* Then convert the initial state *)
	^ "" ^ (json_of_concrete_state model concrete_run.initial_state) ^ ","
	
	(* Iterate on following steps *)
	^ (json_of_concrete_steps model concrete_run.steps)
	
	^ "\n\t\t]" (* end steps *)
	^ "\n\t}" (* end run *)
	^ "\n}" (* end *)
	


(** Convert an impossible_concrete_run to a JSON-style string *)
let json_of_impossible_concrete_run model (impossible_concrete_run : StateSpace.impossible_concrete_run) =
	(* First recall the parameter valuation *)
	"{"
	^ "\n\t" ^ (json_of_string "run") ^ ": {"
	^ "\n\t\t" ^ (json_of_string "nature") ^ ": " ^ (json_of_string "negative") ^ ","
	^ "\n\t\t" ^ (json_of_string "valuation") ^ ": " ^ (json_of_pval model impossible_concrete_run.p_valuation) ^ ","
	
	^ "\n\t\t" ^ (json_of_string "steps") ^ ": ["
	
	(* Then convert the initial state *)
	^ "" ^ (json_of_concrete_state model impossible_concrete_run.initial_state) ^ ","
	
	(* Iterate on following concrete steps *)
	^ (json_of_concrete_steps model impossible_concrete_run.steps)
	
	(*** NOTE: only the first step is impossible; others are "arbitrary" ***)
	^ (match impossible_concrete_run.impossible_steps with
	| [] -> ""
	| first_step :: following_steps ->
		(* Convert the first impossible step *)
		(json_of_impossible_concrete_step model first_step)

		(* Iterate on following impossible steps *)
		^ (json_of_arbitrary_concrete_steps model following_steps)
	)

	^ "\n\t\t]" (* end steps *)
	^ "\n\t}" (* end run *)
	^ "\n}" (* end *)
	
