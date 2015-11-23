(************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/02
 * Last modified: 2015/11/23
 *
 ************************************************************)


open CamlUtilities
open Result
open AbstractModel


(************************************************************)
(** Model *)
(************************************************************)

(* Convert a var_type into a string *)
let string_of_var_type = function
	| Var_type_clock -> "clock"
	| Var_type_discrete -> "discrete"
	| Var_type_parameter -> "parameter"

(* Add a header to the model *)
let string_of_header model =
	let options = Input.get_options () in
		"(**************************************************"
	^ "\n" ^" * Model " ^ options#file
(* 	^ "\n" ^" * Generated at time " ^ time? *)
	^ "\n" ^" **************************************************)"

(* Convert the initial variable declarations into a string *)
let string_of_declarations model =
	"var "
	^
	let string_of_variables list_of_variables =
		string_of_list_of_string_with_sep ", " (List.map model.variable_names list_of_variables) in
	(if model.nb_clocks > 0 then
		("\n\t" ^ (string_of_variables model.clocks) ^ "\n\t\t: clock;") else "")
	^
	(if model.nb_discrete > 0 then
		("\n\t" ^ (string_of_variables model.discrete) ^ "\n\t\t: discrete;") else "")
	^
	(if model.nb_parameters > 0 then
		("\n\t" ^ (string_of_variables model.parameters) ^ "\n\t\t: parameter;") else "")

(* Convert the synclabs of an automaton into a string *)
let string_of_synclabs model automaton_index =
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


(* Convert the initially of an automaton into a string *)
let string_of_initially model automaton_index =
	let inital_global_location  = model.initial_location in
	let initial_location = Location.get_location inital_global_location automaton_index in
	"initally: "
	^ (model.location_names automaton_index initial_location)
	^ ";"


(* Convert the invariant of a location into a string *)
let string_of_invariant model automaton_index location_index =
	"while "
	^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names (model.invariants automaton_index location_index))
	^ " wait"


(* Convert a sync into a string *)
let string_of_sync model action_index =
	match model.action_types action_index with
	| Action_type_sync -> " sync " ^ (model.action_names action_index)
	| Action_type_nosync -> " (* sync " ^ (model.action_names action_index) ^ "*) "



let string_of_clock_updates model = function
	| No_update -> ""
	| Resets list_of_clocks -> 
		string_of_list_of_string_with_sep ", " (List.map (fun variable_index ->
			(model.variable_names variable_index)
			^ "' = 0"
		) list_of_clocks)
	| Updates list_of_clocks_lt -> 
		string_of_list_of_string_with_sep ", " (List.map (fun (variable_index, linear_term) ->
			(model.variable_names variable_index)
			^ "' = "
			^ (LinearConstraint.string_of_pxd_linear_term model.variable_names linear_term)
		) list_of_clocks_lt)

	
	
(* Convert a list of updates into a string *)
let string_of_updates model updates =
	string_of_list_of_string_with_sep ", " (List.map (fun (variable_index, linear_term) ->
		(* Convert the variable name *)
		(model.variable_names variable_index)
		^ "' = "
		(* Convert the linear_term *)
		^ (LinearConstraint.string_of_pxd_linear_term model.variable_names linear_term)
	) updates)


(* Convert a transition of a location into a string *)
let string_of_transition model automaton_index action_index (guard, clock_updates, discrete_updates, destination_location) =
	"\n\t" ^ "when "
	(* Convert the guard *)
	^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard)

	(* Convert the updates *)
	^ " do {"
	(* Clock updates *)
	^ (string_of_clock_updates model clock_updates)
	(* Add a coma in case of both clocks and discrete *)
	^ (if clock_updates != No_update && discrete_updates != [] then ", " else "")
	(* Discrete updates *)
	^ (string_of_updates model discrete_updates)
	^ "} "
	
	(* Convert the sync *)
	^ (string_of_sync model action_index)
	(* Convert the destination location *)
	^ " goto " ^ (model.location_names automaton_index destination_location)
	^ ";"


(* Convert the transitions of a location into a string *)
let string_of_transitions model automaton_index location_index =
	string_of_list_of_string (
	(* For each action *)
	List.map (fun action_index -> 
		(* Get the list of transitions *)
		let transitions = model.transitions automaton_index location_index action_index in
		(* Convert to string *)
		string_of_list_of_string (
			(* For each transition *)
			List.map (string_of_transition model automaton_index action_index) transitions
			)
		) (model.actions_per_location automaton_index location_index)
	)


(* Convert a location of an automaton into a string *)
let string_of_location model automaton_index location_index =
	"\n"
	^ (if model.is_urgent automaton_index location_index then "urgent loc " else "loc ")
	^ (model.location_names automaton_index location_index)
	^ (match model.costs automaton_index location_index with
		| None -> ""
		| Some cost -> "[" ^ (LinearConstraint.string_of_p_linear_term model.variable_names cost) ^ "]"
	)
	^ ": "
	^ (string_of_invariant model automaton_index location_index)
	^ (string_of_transitions model automaton_index location_index)


(* Convert the locations of an automaton into a string *)
let string_of_locations model automaton_index =
	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		string_of_location model automaton_index location_index
	) (model.locations_per_automaton automaton_index))


(* Convert an automaton into a string *)
let string_of_automaton model automaton_index =
	"\n(************************************************************)"
	^ "\n automaton " ^ (model.automata_names automaton_index)
	^ "\n(************************************************************)"
	^ "\n " ^ (string_of_synclabs model automaton_index)
	^ "\n " ^ (string_of_initially model automaton_index)
	^ "\n " ^ (string_of_locations model automaton_index)
	^ "\n end (* " ^ (model.automata_names automaton_index) ^ " *)"
	^ "\n(************************************************************)"


(* Convert the automata into a string *)
let string_of_automata model =
	string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton model automaton_index
	) model.automata)

(* Convert the model into a string *)
let string_of_model model =
	string_of_header model
	^  "\n" ^ string_of_declarations model
	^  "\n" ^ string_of_automata model
	(*** TODO: the initial constraint !! ***)
	(*** TODO: the property !! ***)


(************************************************************)
(** Property *)
(************************************************************)
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
		| Discrete_geq (discrete_index , discrete_value)
			-> (model.variable_names discrete_index) ^ " >= " ^ (NumConst.string_of_numconst discrete_value)
		| Discrete_g (discrete_index , discrete_value)
			-> (model.variable_names discrete_index) ^ " > " ^ (NumConst.string_of_numconst discrete_value)
		| Discrete_interval (discrete_index , min_discrete_value, max_discrete_value)
			-> (model.variable_names discrete_index) ^ " in [" ^ (NumConst.string_of_numconst min_discrete_value) ^ " , " ^ (NumConst.string_of_numconst max_discrete_value) ^ "]"
		) unreachable_global_location.discrete_constraints
	)


(** Convert the correctness property to a string *)
let string_of_property model = function
	(* An "OR" list of global locations *)
	| Unreachable_locations unreachable_global_location_list ->
		string_of_list_of_string_with_sep "\n or \n " (List.map (string_of_unreachable_location model) unreachable_global_location_list)

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

	(* sequence a1, ..., an *)
	| Sequence_acyclic action_index_list ->
		"property := sequence (" ^ (string_of_list_of_string_with_sep ", " (List.map model.action_names action_index_list)) ^ ");"
	(* always sequence a1, ..., an *)
	| Sequence_cyclic action_index_list ->
		"property := always sequence (" ^ (string_of_list_of_string_with_sep ", " (List.map model.action_names action_index_list)) ^ ");"
	
	(* Would be better to have an "option" type *)
	| Noproperty -> "(* no property *)"


(************************************************************)
(** States *)
(************************************************************)

(* Convert a location name into a string *)
(*let string_of_location_name model location =
	let string_array = List.map (fun automaton_index location_index ->
		model.automata_names.(automaton_index) ^ ": " ^ (model.location_names automaton_index location_index)
	) location in
	string_of_array_of_string_with_sep ", " string_array*)

(* Convert a state into a string *)
let string_of_state model (global_location, linear_constraint) =
	"" ^ (Location.string_of_location model.automata_names model.location_names model.variable_names global_location) ^ " ==> \n&" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names linear_constraint) ^ "" 


(************************************************************)
(** Pi0 *)
(************************************************************)
(* Convert a pi0 into a string *)
let string_of_pi0 model pi0 =
	"  " ^ (
	string_of_list_of_string_with_sep "\n& " (
		List.map (fun parameter ->
			(model.variable_names parameter)
			^ " = "
			^ (NumConst.string_of_numconst (pi0#get_value parameter))
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
(* Result *)
(************************************************************)
let string_of_returned_constraint variable_names = function 
	| Convex_constraint (linear_constraint, _) -> LinearConstraint.string_of_p_linear_constraint variable_names linear_constraint
	
	(** Disjunction of constraints *)
	| Union_of_constraints (k_list, _) -> string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_p_linear_constraint variable_names) k_list)
	
	| NNCConstraint (k_good, k_bad, _) -> 
	(
		string_of_list_of_string_with_sep "\n AND \n" (List.map (LinearConstraint.string_of_p_linear_constraint variable_names) k_good)
	) ^ (
		string_of_list_of_string_with_sep "\n AND NOT \n" (List.map (LinearConstraint.string_of_p_linear_constraint variable_names) k_bad)
	)
