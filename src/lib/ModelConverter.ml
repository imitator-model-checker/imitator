(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Convert a parsing structure into an abstract model
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci, Benjamin Loillier
 * Created           : 2009/09/09
 *
 ************************************************************)

(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Automaton
open DiscreteExpressions
open AbstractModel
open ParsingStructure
open VariableInfo
open AbstractProperty
open ParsingStructureGraph
open DiscreteType
open CustomModules
open JsonFormatter
open Templates


(************************************************************)
(************************************************************)
(* Exceptions *)
(************************************************************)
(************************************************************)

(** Exception for detecting strongly deterministic PTAs *)
exception Not_strongly_deterministic

exception InvalidProperty

(*------------------------------------------------------------*)
(* Find the clocks in a linear_constraint *)
(*------------------------------------------------------------*)

(*let get_clocks_in_linear_constraint clocks =
  (* Get a long list with duplicates, and then simplify *)
  (*	(* Should not be too inefficient, because our linear constraints are relatively small *)
    	let list_of_clocks = List.fold_left (fun current_list_of_clocks linear_inequality ->
    		list_append current_list_of_clocks (get_clocks_in_linear_inequality is_clock linear_inequality)
    	) [] linear_constraint
    	in
    	(* Simplify *)
    	list_only_once list_of_clocks*)
  LinearConstraint.pxd_find_variables clocks*)

(************************************************************)
(* Print error messages *)
(************************************************************)

(** Print variable not declared in linear constraint *)
let undeclared_variable_in_linear_constraint_message variable_name =
    print_error ("The variable `" ^ variable_name ^ "` used in a linear constraint was not declared.")

(** Print variable not declared in bool expression *)
let undeclared_variable_in_boolean_expression_message variable_name =
    print_error ("The variable `" ^ variable_name ^ "` used in a boolean expression was not declared.")

(************************************************************)
(* Getting variables *)
(************************************************************)

(*------------------------------------------------------------*)
(** Get all (possibly identical) names of variables in the header *)
(*------------------------------------------------------------*)
let get_variables_and_constants var_type =
    List.fold_left (fun (current_list, constants) (name, possible_value) ->
        match possible_value with
        (* If no value: add to names *)
        | None -> (name :: current_list , constants)
        (* Otherwise: add to constants : name * value * var_type *)
        | Some value -> (current_list , constants @ [(name, value, var_type)])
      ) ([], [])

let get_declared_variable_names variable_declarations =
  (* Get all (possibly identical) names of variables in one variable declaration and add it to the computed n-uple *)
  let get_variables_in_variable_declaration (clocks, discrete_rational, parameters, constants, unassigned_constants) (var_type, list_of_names) =
    let new_list, new_constants = get_variables_and_constants var_type list_of_names in
    match var_type with
    | Var_type_clock ->
      (List.rev_append new_list clocks, discrete_rational, parameters, List.rev_append new_constants constants, unassigned_constants)
(*    | ParsingStructure.Var_type_constant ->*)
(*      (clocks, discrete_rational, parameters, List.rev_append new_constants constants, List.rev_append new_list unassigned_constants)*)
    | Var_type_discrete _ ->
      (clocks, List.rev_append new_list discrete_rational, parameters, List.rev_append new_constants constants, unassigned_constants)
    | Var_type_parameter ->
      (clocks, discrete_rational, List.rev_append new_list parameters, List.rev_append new_constants constants, unassigned_constants)
  in
  let (clocks, discrete_rational, parameters, constants, unassigned_constants) = List.fold_left get_variables_in_variable_declaration ([], [], [], [], []) variable_declarations in
  (* Do not reverse lists *)
  (clocks, discrete_rational, parameters, constants, unassigned_constants)

(** Only get declared discrete variables with their specific types *)
let get_declared_discrete_variables_by_type variable_declarations =
    let get_discrete_variables_in_variable_declaration discretes_by_type (var_type, list_of_names) =
        let new_list, _ = get_variables_and_constants var_type list_of_names in
        match var_type with
            | Var_type_discrete _ ->
                let new_list_discretes_by_type = List.map (fun variable_names -> var_type, variable_names) new_list in
                List.rev_append new_list_discretes_by_type discretes_by_type
            | _ ->
                discretes_by_type
        in
        List.fold_left get_discrete_variables_in_variable_declaration [] variable_declarations

(*------------------------------------------------------------*)
(** Get all (possibly identical) names of automata *)
(*------------------------------------------------------------*)
let get_declared_automata_names =
  List.map (fun (automaton_name, _, _) -> automaton_name)

(*------------------------------------------------------------*)
(** Get all (all different) names of synclabs *)
(*------------------------------------------------------------*)
let get_declared_synclabs_names =
  List.fold_left (fun action_names (_, synclabs, _) -> list_union action_names synclabs) []

(************************************************************)
(* Checking the model *)
(************************************************************)

(*------------------------------------------------------------*)
(** Check that all controllable actions were declared; also warn for multiply declared names *)
(*------------------------------------------------------------*)
let check_controllable_actions (controllable_actions : string list) (all_actions : string list) : bool =
	(* 1. Check duplicates (no big deal) *)
	(* Compute the multiply defined variables *)
	let multiply_defined_names = elements_existing_several_times controllable_actions in
	(* Print a warning for each of them *)
	match multiply_defined_names with
		| [] -> true
		| _ -> List.iter (fun variable_name -> print_warning ("Action `" ^ variable_name ^ "` is listed several times in the list of controllable actions.")) multiply_defined_names;

	(* 2. Check for undefined actions *)
	(*** NOTE: using fold_left instead of for_all to get ALL errors *)
	let all_valid = List.fold_left (fun current_valid action_name ->
		if not (List.mem action_name all_actions) then (
			print_error ("The action `" ^ action_name ^ "` declared as controllable was not declared for any automaton."); false)
		else current_valid
	) true controllable_actions in
	all_valid


(*------------------------------------------------------------*)
(** Check that variable names are all different, return false otherwise; warns if a variable is defined twice as the same type *)
(*------------------------------------------------------------*)
let check_variable_names clock_names discrete_names parameters_names constants =
	(* Warn if a variable is defined twice as the same type *)
	let warn_for_multiply_defined_variables list_of_variables =
		(* Compute the multiply defined variables *)
		let multiply_defined_variables = elements_existing_several_times list_of_variables in
		(* Print a warning for each of them *)
		List.iter (fun variable_name -> print_warning ("Multiply-declared variable `" ^ variable_name ^"`")) multiply_defined_variables;
	in
	warn_for_multiply_defined_variables clock_names;
	warn_for_multiply_defined_variables discrete_names;
	warn_for_multiply_defined_variables parameters_names;
	(* Check different from constants *)
	let different_from_constants l =
		try(
		List.iter (fun name ->
			if Hashtbl.mem constants name then (
				print_error ("Constant `" ^ name ^ "` is also defined as a variable.");
				raise False_exception;
			)
			) l;
		true
		) with False_exception -> false
	in
	(* Error for variables defined as different types *)
	let error_for_multiply_defined_variables l1 l2 =
		let inter = list_inter l1 l2 in
		match inter with
		| [] -> true
		| _ -> List.iter (fun variable_name -> print_error ("The variable `" ^ variable_name ^ "` is defined twice as two different types.")) inter; false
	in
	let check1 = error_for_multiply_defined_variables clock_names discrete_names in
	let check2 = error_for_multiply_defined_variables clock_names parameters_names in
	let check3 = error_for_multiply_defined_variables discrete_names parameters_names in
	let check4 = different_from_constants clock_names in
	let check5 = different_from_constants discrete_names in
	let check6 = different_from_constants parameters_names in
	check1 && check2 && check3 && check4 && check5 && check6


(*------------------------------------------------------------*)
(** Check that the names of automata are all different; return false otherwise *)
(*------------------------------------------------------------*)
let check_declared_automata_names automata_names =
	(* Compute the multiply defined variables *)
	let multiply_defined_names = elements_existing_several_times automata_names in
	(* Print an error for each of them *)
	match multiply_defined_names with
	| [] -> true
	| _ -> List.iter (fun variable_name -> print_error ("Several automata have name `" ^ variable_name ^ "`.")) multiply_defined_names; false


(*------------------------------------------------------------*)
(** Check that all locations of a given automaton are different *)
(*------------------------------------------------------------*)
let all_locations_different =
	(* Check for every automaton *)
	List.fold_left
		(fun all_different (automaton_name, _, locations) ->
		(* Get all the location names *)
		let locations =
			List.map (fun (location : parsed_location) -> location.name) locations in
		(* Look for multiply declared locations *)
		let multiply_declared_locations = elements_existing_several_times locations in
		List.iter (fun location_name -> print_error ("Several locations have name `" ^ location_name ^ "` in automaton `" ^ automaton_name ^ "`.")) multiply_declared_locations;
		if multiply_declared_locations = [] then all_different else false
		)
		true

(*------------------------------------------------------------*)
(** Check that a normal update is well formed *)
(*------------------------------------------------------------*)
(*let check_normal_update variable_infos automaton_name normal_update =

    (* Extract update expression *)
    let _, update_expr = normal_update in

    (* Prepare callback function that print error message when undeclared variable is found *)
    let print_variable_in_update_not_declared variable_name =
        print_error ("Variable `" ^ variable_name ^ "` used in update `" ^ ParsingStructureUtilities.string_of_parsed_normal_update variable_infos normal_update ^ "` in automaton `" ^ automaton_name ^ "` was not declared.")
    in
    let print_variable_in_update_not_declared_opt = Some print_variable_in_update_not_declared in
    (* Prepare print error function for trying to update a constant *)
    let print_update_constant_error variable_name =
        print_error ("Trying to update constant `" ^ variable_name ^ "` in `" ^ ParsingStructureUtilities.string_of_parsed_normal_update variable_infos normal_update ^ "` in automaton `" ^ automaton_name ^ "`.")
    in
    (* Prepare print error function for trying to update a parameter *)
    let print_update_parameter_error parameter_name =
        print_error ("Trying to update parameter `" ^ parameter_name ^ "` in `" ^ ParsingStructureUtilities.string_of_parsed_normal_update variable_infos normal_update ^ "` in automaton `" ^ automaton_name ^ "`.")
    in

    (* Check that all variables in update are declared, and call print function if it's not the case *)
    let all_variables_declared = ParsingStructureMeta.all_variables_defined_in_parsed_normal_update variable_infos print_variable_in_update_not_declared_opt normal_update in
    (* Get an updated variable in normal update *)
    let updated_variable_ref = (
        ParsingStructureUtilities.fold_parsed_normal_update
            (@)
            []
            (function Leaf_update_variable (variable_ref, _, _) -> [variable_ref])
            (fun _ -> []) normal_update
            |> List.nth
        ) 0
    in

    if VariableInfo.is_variable_or_constant_defined variable_infos updated_variable_ref then
        (* Get kind (variable or constant ?) of updated variable *)
        let variable_kind = VariableInfo.variable_kind_of_variable_name variable_infos updated_variable_ref in
        (* Get var type of updated variable *)
        let var_type = VariableInfo.var_type_of_variable_or_constant variable_infos updated_variable_ref in

        (* Check if variable is a constant *)
        let is_constant = match variable_kind with Constant_kind _ -> true | Variable_kind -> false in
        (* Check if variable is a parameter *)
        let is_parameter = match var_type with DiscreteType.Var_type_parameter -> true | _ -> false in
        (* Check if variable is a discrete type *)
        let is_discrete = match var_type with DiscreteType.Var_type_discrete _ -> true | _ -> false in

        (* Get variable name from ref *)
        let updated_variable_name, _ = updated_variable_ref in

        (* Eventually print error messages *)
        if is_constant then print_update_constant_error updated_variable_name;
        if is_parameter then print_update_parameter_error updated_variable_name;

        let is_trying_to_assign_a_clock_or_param =
            if is_discrete then (
                let is_only_discrete = ParsingStructureMeta.only_discrete_in_parsed_boolean_expression variable_infos None update_expr in
                if not is_only_discrete then (
                    print_error ("Trying to update variable `" ^ updated_variable_name ^ "` with clock(s) or parameter(s) in `" ^ ParsingStructureUtilities.string_of_parsed_normal_update variable_infos normal_update ^ "`.");
                    true
                )
                else
                    false
            ) else
                false
        in

        all_variables_declared && not (is_constant || is_parameter || is_trying_to_assign_a_clock_or_param)
    else (
        all_variables_declared
    )*)

(*------------------------------------------------------------*)
(** Check that a sync is well formed *)
(*------------------------------------------------------------*)
let check_sync sync_name_list automaton_name = function
	| Sync sync_name ->  if not (List.mem sync_name sync_name_list) then (
		print_error ("The action `" ^ sync_name ^ "` used in automaton `" ^ automaton_name ^ "` was not declared for this automaton."); false)
		else true
	| NoSync -> true

(*------------------------------------------------------------*)
(** Check that a sync is used in all the automata where it is declared *)
(*------------------------------------------------------------*)
let synclab_used_everywhere automata synclab_name =
	(* Try to find the synclab in all the automaton where it is declared *)
	try(
		(* Check each automaton *)
		List.iter (fun (automaton_name, sync_name_list, locations) ->
			(* Only check if the synclab is declared here *)
			if List.mem synclab_name sync_name_list then(
			(* Check that at least one location contains the synclab *)
			if not (List.exists (fun (location : parsed_location) ->
				(* Check that at least one transition contains the synclab *)
				List.exists (fun (_, _, sync, _) -> sync = (Sync synclab_name)) location.transitions
				) locations ) then (
				(* No location contains the synclab: warning and exception (to save a bit of time) *)
				(*** TODO: perform exhaustive search, i.e., remove the exception mechanism ***)
				print_warning ("Action `" ^ synclab_name ^ "` is not used in (at least) the automaton `" ^ automaton_name ^ "` where it is declared: it will thus be removed from the whole model.");
				raise Not_found;
			);
			);
		) automata;
		(* The synclab was found everywhere: true *)
		true
		(* At least one automata does not use the synclab : false *)
	) with Not_found -> false


(*------------------------------------------------------------*)
(** Check that all variables mentioned in a list of stopwatches exist and are clocks *)
(*------------------------------------------------------------*)
let check_stopwatches variable_infos location_name stopwatches =

    List.map (fun stopwatch_name ->

            let var_type = VariableInfo.var_type_of_global_variable_or_constant_opt variable_infos stopwatch_name in
            match var_type with
            (* Ok *)
            | Some Var_type_clock -> true
            (* Not defined *)
            | None -> print_error ("The variable `" ^ stopwatch_name ^ "` that should be stopped in location `" ^ location_name ^ "` is not defined."); false
            (* Not a clock *)
            | _ -> print_error ("The variable `" ^ stopwatch_name ^ "` that should be stopped in location `" ^ location_name ^ "` is not defined as a clock."); false
        ) stopwatches
    (* Map before make for_all in order to avoid short-circuit evaluation (quit loop when an element is false) *)
    |> List.for_all identity


(*------------------------------------------------------------*)
(** Check that all variables mentioned in a list of flows exist and are clocks *)
(*------------------------------------------------------------*)
let check_flows_2 variable_infos location_name flows =

    (* Check clocks are declared and well-typed *)
    let clock_names = List.map first_of_tuple flows in
    let is_clocks_declared = check_stopwatches variable_infos location_name clock_names in

    (* Group flow values by clock name *)
    let clock_names_by_flow_values = OCamlUtilities.group_by (fun (clock_name, _) -> clock_name) flows in

    (* For each clocks *)
    let is_no_flow_value_discrepancies = List.map (fun (clock_name, flow_values) ->
        (* Check whether a value was defined multiple times *)
        if List.length flow_values > 1 then (

            let flow_values_without_duplicates = OCamlUtilities.list_only_once flow_values in

            (* Check whether the value is the same or not *)
            if List.length flow_values_without_duplicates > 1 then (
                (* If different value: error *)
                print_error ("Multiple and different clock flow values for variable `" ^ clock_name ^ "` in location `" ^ location_name ^ "`.");
                false
            ) else (
				(* If same value: warn *)
                print_warning ("Duplicate clock flow value for variable `" ^ clock_name ^ "` in location `" ^ location_name ^ "`.");
                false
            )
        )
        else
            true
    ) clock_names_by_flow_values
    (* Map before make for_all in order to avoid short-circuit evaluation (quit loop when an element is false) *)
    |> List.for_all identity
    in

    is_clocks_declared && is_no_flow_value_discrepancies

(*let check_flows nb_clocks index_of_variables type_of_variables location_name flows =
	(* Create a hash table variable_index => flow value *)
	let temp_flow_hashtable : (variable_index, NumConst.t) Hashtbl.t = Hashtbl.create nb_clocks in

	(* Flag *)
	let ok = ref true in
	List.iter (fun (clock_name, flow_value) ->
		(* Get variable name *)
		try (
			let variable_index = Hashtbl.find index_of_variables clock_name in

			(* Check variable type *)
			if type_of_variables variable_index <> DiscreteType.Var_type_clock then (
				print_error ("The variable `" ^ clock_name ^ "` used in a flow in location `" ^ location_name ^ "` is not defined as a clock.");
				ok := false;
			);

			(* Check whether a value was already defined *)
			if Hashtbl.mem temp_flow_hashtable variable_index then(
				(* Check whether the value is the same or not *)
				let previous_value = Hashtbl.find temp_flow_hashtable variable_index in
				(* If same value: warn *)
				if NumConst.equal flow_value previous_value then(
					print_warning("Duplicate clock flow value for variable `" ^ clock_name ^ "` in location `" ^ location_name ^ "`.");
				)else(
				(* If different value: error *)
					print_error("Multiple and different clock flow values for variable `" ^ clock_name ^ "` in location `" ^ location_name ^ "`.");
					ok := false;
				);
			)else(
				(* Add the value to the table *)
				Hashtbl.add temp_flow_hashtable variable_index flow_value;
			);


		) with Not_found -> (
			print_error ("The variable `" ^ clock_name ^ "` used in a flow in location `" ^ location_name ^ "` is not defined.");
			ok := false;
			);
		) flows;
	!ok*)

(*------------------------------------------------------------*)
(** Check that the automata are well-formed *)
(*------------------------------------------------------------*)
let check_automata (useful_parsing_model_information : useful_parsing_model_information) automata =

	let index_of_automata		= useful_parsing_model_information.index_of_automata in
	let array_of_location_names	= useful_parsing_model_information.array_of_location_names in

    let variable_infos = useful_parsing_model_information.variable_infos in

	let well_formed = ref true in

	(* Check each automaton *)
	List.iter (fun (automaton_name, sync_name_list, locations) ->
		print_message Verbose_total ("      Checking automaton `" ^ automaton_name ^ "`");
		(* Get the index of the automaton *)
		let index = try (Hashtbl.find index_of_automata automaton_name) with
			Not_found -> raise (InternalError ("Impossible to find the index of automaton `" ^ automaton_name ^ "`."))
		in
		(* Check each location *)
		List.iter (fun (location : parsed_location) ->
			print_message Verbose_total ("        Checking location " ^ location.name);
			(* Check that the location_name exists (which is obvious) *)
			if not (in_array location.name array_of_location_names.(index)) then(
				print_error ("The location `" ^ location.name ^ "` declared in automaton `" ^ automaton_name ^ "` does not exist.");
				well_formed := false);

			(* Check the cost *)
			begin
				match location.cost with
				| Some cost ->
				print_message Verbose_total ("          Checking cost");
				if not (ParsingStructureMeta.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message cost) then well_formed := false;
				| None -> ()
			end;

			(* Check the stopwatches *)
			print_message Verbose_total ("          Checking stopwatches");
			if not (check_stopwatches variable_infos location.name location.stopped) then well_formed := false;

			(* Check the flows *)
			print_message Verbose_total ("          Checking flows");
(*			if not (check_flows useful_parsing_model_information.nb_clocks index_of_variables type_of_variables location.name location.flow) then well_formed := false;*)
			if not (check_flows_2 variable_infos location.name location.flow) then well_formed := false;


			(* Check the convex predicate *)

			(*** TODO: preciser quel automate et quelle location en cas d'erreur ***)

			print_message Verbose_total ("          Checking convex predicate");
			if not (ParsingStructureMeta.all_variables_defined_in_nonlinear_convex_predicate variable_infos (Some undeclared_variable_in_boolean_expression_message) location.invariant) then well_formed := false;


			(* Check transitions *)
			print_message Verbose_total ("          Checking transitions");
			List.iter (fun (convex_predicate, _, sync, target_location_name) ->
				(* Check the convex predicate *)
				print_message Verbose_total ("            Checking convex predicate");
				if not (ParsingStructureMeta.all_variables_defined_in_nonlinear_convex_predicate variable_infos (Some undeclared_variable_in_boolean_expression_message) convex_predicate) then well_formed := false;
				(* Check the sync *)
				print_message Verbose_total ("            Checking sync name ");
				if not (check_sync sync_name_list automaton_name sync) then well_formed := false;
				(* Check that the target location exists for this automaton *)
				if not (in_array target_location_name array_of_location_names.(index)) then (
					print_error ("The target location `" ^ target_location_name ^ "` used in automaton `" ^ automaton_name ^ "` does not exist.");
					well_formed := false);
				) location.transitions;
			) locations;
		) automata;

	(* Return whether the automata passed the tests *)
	!well_formed

(* CHECK INIT *)

(** Check whether an automaton has exactly one initial location *)
let has_one_loc_per_automaton initial_locations parsed_model observer_automaton_index_option =

	(* Check that every automaton is given at most one initial location *)
	let init_locations_for_automata = Hashtbl.create (List.length parsed_model.automata) in

	let at_most_one_loc_per_automaton = List.for_all (fun (automaton_name, location_name) ->
		(* Check if this automaton was already given an initial location before *)
		if Hashtbl.mem init_locations_for_automata automaton_name then (
			(* Get the initial location already declared previously *)
			let previous_location = Hashtbl.find init_locations_for_automata automaton_name in
			(* If identical : only warns *)
			if location_name = previous_location then (
                print_warning ("The automaton `" ^ automaton_name ^ "` is assigned twice the initial location `" ^ location_name ^ "` in the init definition.");
                (* If different : error *)
                true
			) else (
                print_error ("The automaton `" ^ automaton_name ^ "` is assigned several different locations in the init definition.");
                false
			)
			(* If not already given : add it *)
		) else (
			Hashtbl.add init_locations_for_automata automaton_name location_name;
			true
		)
    ) initial_locations
    in

	(* Check that every automaton is given at least one initial location *)
	let at_least_one_loc_per_automaton = List.for_all (fun automaton_index ->
		let is_observer i = match observer_automaton_index_option with
			| None -> false
			| Some observer_id -> i = observer_id
		in
		(* No check for the observer (will be added later) *)
		if not (is_observer automaton_index) then (
			(* Get the name *)
			let automaton_name = parsed_model.automata_names automaton_index in
			(* Look for it in the hash table *)
			if not (Hashtbl.mem init_locations_for_automata automaton_name) then (
                (* Error *)
                print_error ("The automaton `" ^ automaton_name ^ "` is not given any initial location in the init definition.");
                false
			)
			else
			    true
		)
		else
		    true

    ) parsed_model.automata
    in

    at_most_one_loc_per_automaton && at_least_one_loc_per_automaton


let check_init_definition parsed_model =

    let variable_infos = parsed_model.variable_infos in

    let rec check_init_predicate = function
        | Parsed_discrete_predicate (variable_name, expr) ->
            (* Check that l-value variable exist *)
            if not (is_global_variable_or_constant_declared variable_infos variable_name) then (
                print_error ("Variable `" ^ variable_name ^ "` in discrete init is not declared");
                false
            )
            (* And that all variables in expr are defined *)
            else if not (ParsingStructureMeta.all_variables_defined_in_parsed_boolean_expression_without_callback variable_infos expr) then (
                print_error ("Expression \"" ^ variable_name ^ " := " ^ ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos expr ^ "\" uses undeclared variable(s)");
                false
            )
            else
                true
		| Parsed_linear_predicate linear_constraint -> check_init_constraint linear_constraint
		| Parsed_loc_assignment (automaton_name, location_name) ->
			(* Check that the automaton_name exists *)
			let index, exists = try (Hashtbl.find parsed_model.index_of_automata automaton_name, true) with
				Not_found -> (print_error ("The automaton `" ^ automaton_name ^ "` mentioned in the init definition does not exist."); 0, false) in

			if not exists then
                false
			(* Check that the location_name exists (only if the automaton_name exists) *)
			else if not (in_array location_name parsed_model.array_of_location_names.(index)) then (
			    print_error ("The location `" ^ location_name ^ "` mentioned in the init definition does not exist in automaton `" ^ automaton_name ^ "`.");
			    false
			)
			else
                true

    and check_init_constraint = function
        (*** NOTE: do not check linear constraints made of a variable to be removed compared to a linear term ***)
        | Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , linear_expression) as linear_constraint  ->

            if (VariableInfo.is_global_variable_removed variable_infos variable_name) then
                print_message Verbose_total ("Variable `" ^ variable_name ^ "` is compared to a linear term, but will be removed: no check." );

            if not (VariableInfo.is_global_variable_declared variable_infos variable_name) then (
                print_error ("Variable `" ^ variable_name ^ "` in continuous init is not declared");
                false
            )
            (* Still check the second term *)
            else if not (ParsingStructureMeta.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message linear_expression) then (
                print_error ("Linear constraint \"" ^ ParsingStructureUtilities.string_of_parsed_linear_constraint variable_infos linear_constraint ^ "\" uses undeclared variable(s).");
                false
            )
            else
                true
        (* General case: check *)
        | Parsed_linear_constraint _ as linear_constraint ->
            if not (ParsingStructureMeta.all_variables_defined_in_linear_constraint variable_infos undeclared_variable_in_linear_constraint_message linear_constraint) then (
                print_error ("Linear constraint \"" ^ ParsingStructureUtilities.string_of_parsed_linear_constraint variable_infos linear_constraint ^ "\" uses undeclared variable(s).");
                false
            )
            else
                true
        | _ -> true
    in

    check_init_predicate

let is_inequality_has_left_hand_removed_variable removed_variable_names = function
    | Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , _))
    | Parsed_discrete_predicate (variable_name, _) ->
        (* Filter out if the left-hand is in the removed variable names *)
        List.mem variable_name removed_variable_names
    (* Any other combination is OK *)
    | _ ->
        false



(** Convert discrete linear constraint predicate to a discrete init (tuple variable_name * parsed_boolean_expression) *)
let discrete_init_of_discrete_linear_predicate variable_infos = function
    | Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (coeff, updated_variable_name)), op , expression))
    when VariableInfo.is_discrete_global_variable variable_infos updated_variable_name ->

        (* Check *)
        if NumConst.neq coeff NumConst.one then (
            print_error ("The discrete variable `" ^ updated_variable_name ^ "` must have a coeff 1 in the init definition.");
            raise InvalidModel;
        )
        else (

            match (op, expression) with
            (* Simple constant: OK *)
            | (PARSED_OP_EQ, Linear_term (Constant c)) ->
                let rational_value = ParsedValue.Weak_number_value c in
                My_left (
                    updated_variable_name,
                    Parsed_discrete_bool_expr (
                    Parsed_arithmetic_expr (
                    Parsed_term (
                    Parsed_factor (
                    Parsed_constant rational_value))))
                )
            (* Constant: OK *)
            | (PARSED_OP_EQ, Linear_term (Variable (coef, variable_name))) ->
                let coef_rational_value = ParsedValue.Weak_number_value coef in
                My_left (
                    updated_variable_name,
                    Parsed_discrete_bool_expr (
                    Parsed_arithmetic_expr (
                    Parsed_term (
                    Parsed_product_quotient (
                    Parsed_factor (Parsed_constant coef_rational_value),
                    Parsed_variable (variable_name, 0),
                    Parsed_mul))))
                )

            | _ ->
                print_error ("The initial value for discrete variable `" ^ updated_variable_name ^ "` must be given in the form `" ^ updated_variable_name ^ " = c`, where `c` is an integer, a rational or a constant.");
                raise InvalidModel;
        )
    | Parsed_discrete_predicate (updated_variable_name, expr) -> My_left (updated_variable_name, expr)
    | Parsed_loc_assignment _ -> raise (InternalError "Trying to partition between discrete and continuous inits in list containing location inits.")
    | continous_init_predicate -> My_right continous_init_predicate


let check_discrete_inits functions_table variable_infos init_values_for_discrete (variable_name, expr) =

        (* Get kind of variable *)
        let variable_kind = VariableInfo.variable_kind_of_global_variable_name variable_infos variable_name in

        (match variable_kind with
        | Constant_kind _ ->
            print_error ("Initialize `" ^ variable_name ^ "` constant is forbidden");
            false
        | Variable_kind ->

            (* Get the variable index *)
            let discrete_index = VariableInfo.index_of_variable_name variable_infos variable_name in
            (* Convert init to abstract model *)
            let converted_expr = DiscreteExpressionConverter.convert_discrete_init variable_infos variable_name expr in

            (* Check if it was already declared *)
            if Hashtbl.mem init_values_for_discrete discrete_index then (
                print_error (
                    "The discrete variable `"
                    ^ variable_name
                    ^ "` is given an initial value several times in the init definition."
                );
                false
            )
            (* Try to reduce expression to a value *)
            else if not (DiscreteExpressionEvaluator.is_global_expression_constant (Some functions_table) converted_expr) then (

                print_error (
                    "Init variable `"
                    ^ variable_name
                    ^ "` with a non constant expression `"
                    ^ ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos expr
                    ^ "` is forbidden."
                );
                false
            ) else (
                let value = DiscreteExpressionEvaluator.try_eval_constant_global_expression (Some functions_table) converted_expr in
                Hashtbl.add init_values_for_discrete discrete_index value;
                true
            )
        )


(*------------------------------------------------------------*)
(** Check that the init_definition are well-formed *)
(*------------------------------------------------------------*)
let check_init dependency_graph functions_table (useful_parsing_model_information : useful_parsing_model_information) init_definition observer_automaton_index_option variable_remove =

    let partition_and_map_loc_init_and_variable_inits = function
        (* Make pairs (automaton_name, location_name) *)
        | Parsed_loc_assignment (automaton_name, location_name) -> My_left (automaton_name, location_name)
        | Parsed_linear_predicate _
        | Parsed_discrete_predicate _ as predicate -> My_right predicate
    in

    let variable_infos = useful_parsing_model_information.variable_infos in

    (* Partition init predicates between initial location and inequalities *)
    let initial_locations, init_inequalities = OCamlUtilities.partition_map partition_and_map_loc_init_and_variable_inits init_definition in


    (* For all definitions : *)
    (* Check that automaton names and location names exist *)
    (* Check that continuous variables used in continuous init exist *)
    (* Check that discrete variables used in discrete init exist *)
    let definitions_well_formed = List.for_all (check_init_definition useful_parsing_model_information) init_definition in

    (* Here if not well formed we can raise an error *)
    if not definitions_well_formed then
        raise InvalidModel;

	(* Get pairs for the initialisation of the discrete variables, and check the init definition *)
    let init_definition =
        (* If no auto remove, keep all inits *)
        if variable_remove then
            init_definition
        (* If auto remove, remove all unused variable inits *)
        else
            ParsingStructureGraph.remove_unused_inits dependency_graph init_definition
    in

    (* Check there is only one initial location per automaton *)
    let one_loc_per_automaton = has_one_loc_per_automaton initial_locations useful_parsing_model_information observer_automaton_index_option in

	(* Remove the inequalities of which the left-hand term is a removed variable *)
	let filtered_init_inequalities = List.filter (fun x -> not (is_inequality_has_left_hand_removed_variable variable_infos.removed_variable_names x)) init_inequalities in

	(* Partition and map the init inequalities between the discrete init assignments, and other inequalities *)
    let discrete_inits, other_inequalities = OCamlUtilities.partition_map (discrete_init_of_discrete_linear_predicate variable_infos) filtered_init_inequalities in

(*    let well_formed = definitions_well_formed && one_loc_per_automaton in*)

    (* Here if not well formed we can raise an error *)
    if not one_loc_per_automaton then
        raise InvalidModel;

    (* Check init discrete section : discrete *)
	(* Check that every discrete variable is given only one (rational) initial value *)
	let init_values_for_discrete = Hashtbl.create (List.length variable_infos.discrete) in

    (* Compute discrete init values and add to init hash table *)
    let discrete_initialization_well_formed = List.for_all (check_discrete_inits functions_table variable_infos init_values_for_discrete) discrete_inits in

	(* Check that every discrete variable is given at least one initial value (if not: warns) *)
	List.iter (fun discrete_index ->
		if not (Hashtbl.mem init_values_for_discrete discrete_index) then (
		    let variable_name = VariableInfo.variable_name_of_index variable_infos discrete_index in
		    let variable_type = VariableInfo.var_type_of_global_variable_name variable_infos variable_name in
		    let default_value = AbstractValue.default_value variable_type in

			print_warning ("The discrete variable `" ^ variable_name ^ "` was not given an initial value in the init definition: it will be assigned to " ^ AbstractValue.string_of_value default_value ^ ".");
			Hashtbl.add init_values_for_discrete discrete_index default_value
		);
    ) variable_infos.discrete;

	(* Convert the Hashtbl to pairs (discrete_index, init_value) *)
	let discrete_values_pairs =
		List.map (fun discrete_index ->
			discrete_index, Hashtbl.find init_values_for_discrete discrete_index
		) variable_infos.discrete
	in

    (* Check init constraints section : continuous *)

	(* Check variable types in other_inequalities *)
	(* As other_inequalities represent init constraints of clocks, parameters, or only rational discrete variables *)
    (* we raise an exception if there is one discrete variable of another type than discrete rational *)

    let continuous_init_error = ref false in
	List.iter (fun lp ->

        (* Search variables used in linear predicate *)
	    let variable_names = ParsingStructureMeta.get_variables_in_init_state_predicate lp in

        (* Gathering all variables that are non rational *)
        let non_rational_variable_names = StringSet.filter (fun variable_name ->
            let discrete_type = VariableInfo.discrete_type_of_global_variable_or_constant variable_infos variable_name in
            not (DiscreteType.is_discrete_type_rational_type discrete_type)
        ) variable_names
        in

        (* Print error message for all non rational variables *)
        StringSet.iter(fun variable_name ->
            print_error (
                "Variable `"
                ^ variable_name
                ^ "` used in init constraint \""
                ^ ParsingStructureUtilities.string_of_parsed_init_state_predicate variable_infos lp
                ^ "\" should be "
                ^ DiscreteType.string_of_var_type_discrete (DiscreteType.Dt_number DiscreteType.Dt_rat)
            );
            continuous_init_error := true;
        ) non_rational_variable_names

	) other_inequalities;

    (* There are errors in the constraints init section *)
    if !continuous_init_error then
        raise (InvalidExpression ("There are errors in the continuous init section"));

	(* Return whether the init declaration passed the tests *)
	init_definition, discrete_values_pairs, discrete_initialization_well_formed

(* Check if a constant or a variable is typed as a void, print error when one found *)
let has_void_constant_or_variable str_var_kind name var_type =

    let str_var_kind_capitalized = String.capitalize_ascii str_var_kind in

    if is_var_type_holding_void_type var_type then (
        print_error (str_var_kind_capitalized ^ " `" ^ name ^ "` was declared as `void`. A " ^ str_var_kind ^ " cannot be declared as `void`.");
        true
    ) else
        false

(************************************************************)
(** Converting the model *)
(************************************************************)

(*------------------------------------------------------------*)
(** Create the hash table of constants ; check the validity on-the-fly *)
(*------------------------------------------------------------*)
let make_constants constants =
  (* Create hash table *)
  let constants_hashtable : (string, AbstractValue.abstract_value) Hashtbl.t = Hashtbl.create (List.length constants) in
  (* Manage Boolean for checking errors *)
  let correct = ref true in
  List.iter (fun (name, value(*, discrete_type *)) ->
      if (Hashtbl.mem constants_hashtable name) then (
        let old_value = Hashtbl.find constants_hashtable name in
        (* If same: warning *)
        if(AbstractValue.equal old_value value) then(
          print_warning ("Constant `" ^ name ^ "` is defined twice.");
        )else(
          (* If different: error *)
          print_error ("Constant `" ^ name ^ "` is given different values.");
          correct := false;
        );
      )else(
        Hashtbl.add constants_hashtable name value;
      );
    ) constants;
  (* Return hash table *)
  constants_hashtable, !correct


(*------------------------------------------------------------*)
(** Get all the declared actions for every automaton *)
(*------------------------------------------------------------*)
(*let make_actions_per_automaton index_of_actions index_of_automata automata =
  (* Create an empty array for every automaton *)
  let actions_per_automaton = Array.make (List.length automata) [] in
  (* Fill it *)
  List.iter (fun (automaton_name, sync_name_list, _) ->
      (* Get the index of the automaton *)
      let automaton_index = Hashtbl.find index_of_automata automaton_name in
      (* Update the array *)
      actions_per_automaton.(automaton_index) <-
        List.map (fun sync_name ->
            (* Get the index of the action *)
            let action_index = Hashtbl.find index_of_actions sync_name in
            (* Return the action index *)
            action_index
          ) sync_name_list;
    ) automata;
  (* Return the array *)
  actions_per_automaton*)


(*------------------------------------------------------------*)
(** Creating the list of controllable actions indexes *)
(*------------------------------------------------------------*)
(* Convert the list of controllable action names into a list of (unique) controllable action indices *)
let make_controllable_actions (controllable_actions_names : action_name list) index_of_actions : action_index list =
	(* Print some information *)
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("Non-necessarily unique controllable action names: " ^ (string_of_list_of_string_with_sep ", " controllable_actions_names) ^ ".");
	);

	(* Remove duplicates *)
	(*** NOTE (ÉA, 2022/11): sort-of duplicate operation from check_controllable_actions, but let us assume the number of actions remains reasonable *)
	let unique_controllable_actions_names : action_name list = list_only_once controllable_actions_names in

	(* Print some information *)
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("Unique controllable action names: " ^ (string_of_list_of_string_with_sep ", " unique_controllable_actions_names) ^ ".");
	);

	(* Print some information *)
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("Index of actions: " ^ (Hashtbl.fold (fun action_name action_index current_string -> current_string ^ "; " ^ action_name ^ "=>" ^ (string_of_int action_index)) index_of_actions "") ^ ".");
	);

	(* Convert *)
	List.map (Hashtbl.find index_of_actions) unique_controllable_actions_names


(*------------------------------------------------------------*)
(** Get all the locations for every automaton *)
(*------------------------------------------------------------*)
let make_locations_per_automaton index_of_automata parsed_automata nb_automata =
  (* Create an empty array for every automaton *)
  let locations_per_automaton = Array.make nb_automata (Array.make 0 "") in
  (* For each automaton: *)
  List.iter
    (fun (automaton_name, _, transitions) ->
       (* Get the index of the automaton *)
       let index = try(Hashtbl.find index_of_automata automaton_name)
         with Not_found -> raise (InternalError ("Automaton name `" ^ automaton_name ^ "` not found in function 'make_locations_per_automaton' although this had been checked before."))
       in
       (* Get the location names *)
       let location_names = List.map (fun (location : parsed_location) -> location.name) transitions in
       (* Update the array *)
       locations_per_automaton.(index) <- Array.of_list location_names
    )
    parsed_automata;
  (* Return the array *)
  locations_per_automaton


(*------------------------------------------------------------*)
(** Get all the possible actions for every location of every automaton *)
(*------------------------------------------------------------*)
let make_automata (useful_parsing_model_information : useful_parsing_model_information) parsed_automata (with_observer_action : bool) =

	let index_of_actions		= useful_parsing_model_information.index_of_actions in
	let index_of_automata		= useful_parsing_model_information.index_of_automata in
	let index_of_locations		= useful_parsing_model_information.index_of_locations in
	let actions					= useful_parsing_model_information.actions in
	let removed_action_names	= useful_parsing_model_information.removed_action_names in

    let variable_infos = useful_parsing_model_information.variable_infos in

	(* Number of automata *)
	let nb_automata = Hashtbl.length index_of_automata in
	(* Create an empty array for the actions of every automaton *)
	let actions_per_automaton = Array.make nb_automata [] in
	(* Create an empty array for the actions of every location of every automaton *)
	let actions_per_location = Array.make nb_automata (Array.make 0 []) in
	(* Create an empty array for the actions of every location of every automaton *)
	let location_urgency = Array.make nb_automata (Array.make 0 Location_nonurgent) in
	(* Create an empty array for the actions of every location of every automaton *)
	let location_acceptance = Array.make nb_automata (Array.make 0 Location_nonaccepting) in
	(* Create an empty array for the costs *)
	let costs = Array.make nb_automata (Array.make 0 None) in
	(* Create an empty array for the transitions *)
	let transitions = Array.make nb_automata (Array.make 0 []) in
	(* Create an empty array for the invariants *)
	let invariants = Array.make nb_automata (Array.make 0 (False_guard)) in
	(* Create an empty array for the stopwatches *)
	let stopwatches_array = Array.make nb_automata (Array.make 0 []) in
	(* Create an empty array for the flows *)
	let flow_array = Array.make nb_automata (Array.make 0 []) in
	
	(* Does the model has any clock with a rate <>1? *)
	let has_non_1rate_clocks = ref false in
	(* Maintain the index of no_sync *)
	let no_sync_index = ref (Array.length actions) in

	(* For each automaton (except the observer, if any): *)
	List.iter
		(fun (automaton_name, _, locations) ->
		(* Get the index of the automaton *)
		print_message Verbose_total ("    - Building automaton " ^ automaton_name);
		let automaton_index = try (Hashtbl.find index_of_automata automaton_name) with Not_found -> raise (InternalError ("Impossible to find the index of automaton `" ^ automaton_name ^ "`.")) in
		(* Get the number of locations *)
		let nb_locations = List.length locations in
		(* Create the array of lists of actions for this automaton *)
		actions_per_location.(automaton_index) <- Array.make nb_locations [];
		(* Create the array of urgent locations for this automaton (default: non-urgent) *)
		location_urgency.(automaton_index) <- Array.make nb_locations Location_nonurgent;
		(* Create the array of accepting locations for this automaton (default: non-accepting) *)
		location_acceptance.(automaton_index) <- Array.make nb_locations Location_nonaccepting;
		(* Create the array of costs for this automaton *)
		costs.(automaton_index) <- Array.make nb_locations None;
		(* Create the array of list of transitions for this automaton *)
		transitions.(automaton_index) <- Array.make nb_locations [];
		(* Create the array of invariants for this automaton *)
		invariants.(automaton_index) <- Array.make nb_locations (False_guard);
		(* Create the array of stopwatches for this automaton *)
		stopwatches_array.(automaton_index) <- Array.make nb_locations [];
		(* Create the array of flows for this automaton *)
		flow_array.(automaton_index) <- Array.make nb_locations [];

		(* For each location: *)
		List.iter
			(fun (location : parsed_location) ->
				(* Get the index of the location *)
				let location_index = try (Hashtbl.find index_of_locations.(automaton_index) location.name) with Not_found -> raise (InternalError ("Impossible to find the index of location `" ^ location.name ^ "`.")) in

				(* Create the list of actions for this location, by iterating on parsed_transitions *)
				let list_of_actions, list_of_transitions =  List.fold_left (fun (current_list_of_actions, current_list_of_transitions) (guard, updates, sync, target_location_name) ->
					(* Get the index of the target location *)
					let target_location_index = try (Hashtbl.find index_of_locations.(automaton_index) target_location_name) with Not_found -> raise (InternalError ("Impossible to find the index of location `" ^ target_location_name ^ "`.")) in
					(* Depend on the action type *)
					match sync with
					| ParsingStructure.Sync action_name ->
					(* If the 'sync' is within the removed actions, do nothing *)
					if List.mem action_name removed_action_names then (
						current_list_of_actions, current_list_of_transitions
						(* Else : *)
					) else (
						(* Get the action index *)
						let action_index =
						try (Hashtbl.find index_of_actions action_name) with Not_found -> raise (InternalError ("Impossible to find the index of action `" ^ action_name ^ "`."))
						in
						(* Compute the list of actions *)
						(action_index :: current_list_of_actions)
						,
						(* Compute the list of transitions *)
						((action_index, guard, updates, target_location_index) :: current_list_of_transitions)
					)
					| ParsingStructure.NoSync ->
					(* Get the action index *)
					let action_index = !no_sync_index in
					(* Increment the number of nosync indexes *)
					no_sync_index := !no_sync_index + 1;
					(* Compute the list of actions *)
					(action_index :: current_list_of_actions)
					,
					(* Compute the list of transitions *)
					((action_index, guard, updates, target_location_index) :: current_list_of_transitions)
				) ([], []) location.transitions in

				(* Update the array of actions per location *)
				actions_per_location.(automaton_index).(location_index) <- (List.rev (list_only_once list_of_actions));

				(* Update the array of costs per location *)
				begin
				match location.cost with
				| Some cost ->
					costs.(automaton_index).(location_index) <- Some (
						LinearConstraint.cast_p_of_pxd_linear_term
						(ExpressionConverter.Convert.linear_term_of_linear_expression variable_infos cost)
						true
					);
				| None -> ()
				end;

				(* Update the array of urgency *)
				let urgency =
				match location.urgency with
				| Parsed_location_urgent -> Location_urgent
				| Parsed_location_nonurgent -> Location_nonurgent
				in
				location_urgency.(automaton_index).(location_index) <- urgency;

				(* Update the array of acceptance *)
				let acceptance =
				match location.acceptance with
				| Parsed_location_accepting -> Location_accepting
				| Parsed_location_nonaccepting -> Location_nonaccepting
				in
				location_acceptance.(automaton_index).(location_index) <- acceptance;

				(* Update the array of transitions per location *)
				transitions.(automaton_index).(location_index) <- (List.rev list_of_transitions);

				(* Update the array of invariants *)
				invariants.(automaton_index).(location_index) <- DiscreteExpressionConverter.convert_guard variable_infos location.invariant;

				(* Does the model has stopwatches? *)
				if location.stopped <> [] then has_non_1rate_clocks := true;
				(* Convert the stopwatches names into variables *)
				let list_of_stopwatch_names = list_only_once location.stopped in
				(* Update the array of stopwatches *)
				stopwatches_array.(automaton_index).(location_index) <- List.map (index_of_variable_name variable_infos) list_of_stopwatch_names;

				(* Does the model has clocks with <> rate? *)
				(*** NOTE: technically, we should update the flag only whenever the rate is <> 1… ***)
				if location.flow <> [] then has_non_1rate_clocks := true;
				(* Convert the flow names into variables *)
				(* Update the array of flows *)
				flow_array.(automaton_index).(location_index) <-
					(* Sort the list and remove duplicates, just to potentially speed up a bit *)
					List.sort_uniq compare
					(
						List.map (fun (clock_name, flow_value) ->
							(index_of_variable_name variable_infos clock_name), flow_value
						) location.flow
					);

			) locations;
		(* Update the array of actions per automaton *)
		let all_actions_for_this_automaton = Array.fold_left (fun list_of_all_actions list_of_actions ->
			list_union list_of_all_actions list_of_actions
			) [] actions_per_location.(automaton_index) in
		actions_per_automaton.(automaton_index) <- all_actions_for_this_automaton
		) parsed_automata;


	(* Create the array of action names; add 1 no_sync for the observer, if any *)
	let nb_actions = !no_sync_index + (if with_observer_action then 1 else 0) in
	let array_of_action_names = Array.make nb_actions "" in
	(* Create the array of action types (sync / no_sync) *)
	let array_of_action_types = Array.make nb_actions Action_type_sync in
	(* Fill the sync actions *)
	for i = 0 to (Array.length actions) - 1 do
		array_of_action_names.(i) <- actions.(i);
	done;
	(* Fill the no sync actions *)
	for i = Array.length actions to nb_actions - 1 - (if with_observer_action then 1 else 0) do
		array_of_action_names.(i) <- ("nosync_" ^ (string_of_int (i - (Array.length actions) + 1)));
		array_of_action_types.(i) <- Action_type_nosync;
	done;
	(* Fill the array for the observer no_sync *)
	if with_observer_action then(
		array_of_action_names.(nb_actions - 1) <- Constants.observer_nosync_name;
		array_of_action_types.(nb_actions - 1) <- Action_type_nosync;
	);

	(* Create the action list *)
	let actions = list_of_interval 0 (nb_actions - 1) in

	(* Return all the structures in a functional representation *)
	actions, array_of_action_names, array_of_action_types, actions_per_automaton, actions_per_location, location_acceptance, location_urgency, costs, invariants, stopwatches_array, !has_non_1rate_clocks, flow_array, transitions, (if with_observer_action then Some (nb_actions - 1) else None)



(*------------------------------------------------------------*)
(** Get the automata for every action *)
(*------------------------------------------------------------*)
let make_automata_per_action actions_per_automaton nb_automata nb_actions =
  (* Create an empty array for actions *)
  let automata_per_action = Array.make nb_actions [] in
  (* For all automaton *)
  for automaton_index = 0 to nb_automata - 1 do
    (* For all action *)
    List.iter (fun action_index ->
        (* Add the automaton_index to the action_index *)
        automata_per_action.(action_index) <- automaton_index :: automata_per_action.(action_index);
      ) (actions_per_automaton automaton_index);
  done;
  (* Reverse it (not so important…) and return it *)
  let automata_per_action = Array.map List.rev automata_per_action in
  (* Return a functional representation *)
  fun automaton_index -> automata_per_action.(automaton_index)

(*------------------------------------------------------------*)
(** Convert the transitions *)
(*------------------------------------------------------------*)

(* TODO factorise parameters ! so many parameters ! maybe we can remove some, or use structure, etc *)
let convert_transitions options nb_transitions nb_actions declarations_info variable_infos dependency_graph user_function_definitions_table transitions
	: (((AbstractModel.transition_index list) array) array) array * (AbstractModel.transition array) * (Automaton.automaton_index array)
	=

  (* Create the empty array of transitions automaton_index -> location_index -> action_index -> list of (transition_index) *)
  
  (*** NOTE/TODO: why (Array.length transitions) ?! ***)
  
  let array_of_transitions : (((AbstractModel.transition_index list) array) array) array = Array.make (Array.length transitions) (Array.make 0 (Array.make 0 [])) in
  (* Create the empty array transition_index -> transition *)
  let dummy_transition = {
	guard		= True_guard;
	action		= -1;
	updates = No_potential_update, [];
	target		= -1;
	} in
  let transitions_description : AbstractModel.transition array = Array.make nb_transitions dummy_transition in
  let automaton_of_transition : Automaton.automaton_index array = Array.make nb_transitions (-1) in
  
  (* Maintain an index for the next transition *)
  let transition_index = ref 0 in

  (* Iterate on automata *)
  Array.iteri (fun automaton_index transitions_for_this_automaton ->
      let nb_locations = Array.length transitions_for_this_automaton in

      (* Set the array for this automaton *)
      array_of_transitions.(automaton_index) <- Array.make nb_locations (Array.make 0 []);

      (* Iterate on locations *)
      Array.iteri (fun location_index transitions_for_this_location ->

          (*** WARNING !!! Here, a BIG array is created (as big as the number of actions !!) ***)
          (*** TODO: convert to HashTbl ? (test efficiency?) ***)

          (* Set the array for this location *)
          array_of_transitions.(automaton_index).(location_index) <- Array.make nb_actions [];

          (* Iterate on transitions *)
          List.iter (fun (action_index, guard, parsed_seq_code_bloc_update, target_location_index) ->

              (* Convert the guard *)
              let converted_guard = DiscreteExpressionConverter.convert_guard variable_infos guard in

              (* Filter instruction in update code bloc according to option -no-var-autoremove *)
              let filtered_parsed_seq_code_bloc_updates =
                if options#no_variable_autoremove then
                    (* No variable auto remove, keep all instructions in update code bloc *)
                    parsed_seq_code_bloc_update
                else
                    (* If variable auto remove, remove unused clock assignments *)
                    ParsingStructureGraph.remove_unused_assignments_in_updates declarations_info dependency_graph parsed_seq_code_bloc_update
              in

              (* translate parsed updates into their abstract model *)
              let converted_updates = DiscreteExpressionConverter.convert_seq_code_bloc variable_infos user_function_definitions_table filtered_parsed_seq_code_bloc_updates in

              (* Update the transition array *)
              array_of_transitions.(automaton_index).(location_index).(action_index) <- !transition_index :: array_of_transitions.(automaton_index).(location_index).(action_index);
              
              (* Add the transition to the description *)
              transitions_description.(!transition_index) <- {
					guard   = converted_guard;
					action  = action_index;
					updates = converted_updates;
					target  = target_location_index;
				};
              (* Add the automaton *)
              automaton_of_transition.(!transition_index) <- automaton_index;
              
              (* Increment the index *)
              transition_index := !transition_index + 1;

            ) transitions_for_this_location;
        ) transitions_for_this_automaton;
    ) transitions;

  (* Return transitions and the arrays transition_index -> transition and transition_index -> automaton_index *)
  array_of_transitions, transitions_description, automaton_of_transition




(*------------------------------------------------------------*)
(** Create the initial state *)
(*------------------------------------------------------------*)
let make_initial_state variable_infos index_of_automata index_of_locations parameters removed_variable_names variable_names init_discrete_pairs init_definition =
	(* Get the location initialisations and the constraint *)
	let loc_assignments, linear_predicates = List.partition (function
		| Parsed_loc_assignment _ -> true
		| _ -> false
		) init_definition in
	(* Make pairs (automaton_name, location_name) *)
	let initial_locations = List.map (function
		| Parsed_loc_assignment (automaton_name, location_name) -> (automaton_name, location_name)
		| _ -> raise (InternalError "Something else than a Parsed_loc_assignment was found in a Parsed_loc_assignment list")
		) loc_assignments in
	(* Convert the pairs to automaton_index, location_index *)
	let locations = List.map (fun (automaton_name, location_name) ->
		(* Find the automaton index *)
		let automaton_index = Hashtbl.find index_of_automata automaton_name in
		(* Find the location index *)
		automaton_index,
		Hashtbl.find index_of_locations.(automaton_index) location_name
		) initial_locations in

	(* Construct the initial location *)
	let initial_location = DiscreteState.make_location locations init_discrete_pairs in

	(* Remove the init definitions for discrete variables *)
	let other_inequalities = List.filter (function
		(* Check if the left part is only a variable name *)
		| Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , _)) ->
			(* First check whether it was removed *)
			if List.mem variable_name removed_variable_names then
			    false
			else
			    let is_discrete =
                    (* variable should be global because we search variable ref with id=0 (variable_name, id=0) *)
                    let variable_ref = variable_name, 0 in
                    (* Get kind of variable *)
                    let variable_kind_opt = VariableInfo.variable_kind_of_variable_name_opt variable_infos variable_ref in
                    (* If a variable, it should be discrete, otherwise, not discrete, if None => variable not found *)
                    match variable_kind_opt with
                    | Some Variable_kind -> VariableInfo.is_discrete_variable variable_infos variable_ref
                    | Some Constant_kind _ -> false
                    | None ->
                        (* Otherwise: problem! *)
                        raise (InternalError ("The variable `" ^ variable_name ^ "` mentioned in the init definition does not exist, although this should have been checked before."));
			    in
			    not is_discrete
        (* Do not care about discrete boolean init for constraint initialization ! *)
        | Parsed_discrete_predicate _ -> false
		| _ -> true
		) linear_predicates in
	(* Convert the inequalities *)
	let convex_predicate = List.map (function
		| Parsed_linear_predicate lp -> lp
		| _ -> raise (InternalError "Something else than a Parsed_linear_predicate was found in a Parsed_linear_predicate list.")
    ) other_inequalities in

	let initial_constraint : LinearConstraint.px_linear_constraint =

		(* Create pairs of (index , value) for discrete variables *)
		(* 		let discrete_values = List.map (fun discrete_index -> discrete_index, (DiscreteState.get_discrete_value initial_location discrete_index)) model.discrete in *)

        (* Get only rational discrete for constraint encoding *)
        let init_discrete_rational_pairs = List.filter (fun (_, discrete_value) -> AbstractValue.is_rational_value discrete_value) init_discrete_pairs in
        (* map to num const *)
        let init_discrete_rational_numconst_pairs = List.map (fun (discrete_index, discrete_value) -> discrete_index, AbstractValue.numconst_value discrete_value) init_discrete_rational_pairs in

		(* Create a constraint encoding the value of the discretes *)
		let discretes = LinearConstraint.pxd_constraint_of_discrete_values init_discrete_rational_numconst_pairs in

		(* Create initial constraint (through parsing) *)
		let initial_constraint = (ExpressionConverter.Convert.linear_constraint_of_convex_predicate variable_infos convex_predicate) in

		(* Intersects initial constraint with discretes *)
		LinearConstraint.pxd_intersection_assign initial_constraint [discretes];

		(* Remove discretes *)
		LinearConstraint.pxd_hide_discrete_and_collapse initial_constraint
	in

	(* PERFORM VERIFICATIONS *)
	(* Check that all parameters are bound to be >= 0 *)
	List.iter (fun parameter_id ->
		(* Print some information *)
		print_message Verbose_low ("Checking that parameter `" ^ (variable_names parameter_id) ^ "` is >= 0 in the initial constraint…");

		(* Check *)
		if not (LinearConstraint.px_is_positive_in parameter_id initial_constraint) then
			print_warning ("Parameter `" ^ (variable_names parameter_id) ^ "` is not necessarily positive in the initial constraint. The behavior of " ^ Constants.program_name ^ " is unspecified in this case. You are advised to add inequality `" ^ (variable_names parameter_id) ^ " >= 0` to the initial state of the model.");
		) parameters;

	(* Return the initial state *)
	initial_location, initial_constraint




(************************************************************)
(************************************************************)
(** Checking and converting property *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Checking the property *)
(************************************************************)


(*

(*------------------------------------------------------------*)
(* Local functions checking existence of a name in property specifications
 * May print warnings or errors
*)
(*------------------------------------------------------------*)
let check_automaton_name index_of_automata automaton_name =
  if not (Hashtbl.mem index_of_automata automaton_name)
  then (
    print_error ("The automaton name `" ^ automaton_name ^ "` used in the correctness property does not exist.");
    false
  )
  else true

(*------------------------------------------------------------*)
let check_location_name index_of_locations automaton_index automaton_name location_name =
  if not (Hashtbl.mem index_of_locations.(automaton_index) location_name)
  then (
    print_error ("The location name `" ^ location_name ^ "` used in the correctness property does not exist in automaton `" ^ automaton_name ^ "`.");
    false
  )
  else true

*)
(*------------------------------------------------------------*)
let check_action_name index_of_actions action_name =
	if not (Hashtbl.mem index_of_actions action_name)
	then (
		print_error ("The action `" ^ action_name ^ "` used in the property does not exist in this model.");
		false
	)
	else true

(*
(*------------------------------------------------------------*)
(* Check a list of local location declaration (i.e., of the form `loc[automaton] = location`)
 * Return a list of unreachable_location, and a Boolean encoding whether all checks passed
 * May print warnings or errors
*)
(*------------------------------------------------------------*)
let check_and_convert_unreachable_local_locations index_of_automata index_of_locations parsed_unreachable_locations =
  (* Create a hash table to check for double declarations *)
  let hashtable = Hashtbl.create (Hashtbl.length index_of_automata) in

  (* Global check *)
  let checks_passed = ref true in

  (* Iterate on parsed_unreachable_locations and check for names *)
  List.iter(fun parsed_unreachable_predicate ->
      let automaton_name , location_name =
        match parsed_unreachable_predicate with
        | Parsed_unreachable_loc (automaton_name , location_name) -> automaton_name , location_name
        | _ -> raise (InternalError("Expecting a Parsed_unreachable_loc, which should have been checked earlier."))
      in
      (* Check automaton name *)
      let check1 = check_automaton_name index_of_automata automaton_name in

      (* Check location name *)
      let check2 =
        (* Only perform check2 if check1 passed *)
        if not check1 then false else(
          (* Retrieve automaton index *)
          let automaton_index = Hashtbl.find index_of_automata automaton_name in
          (* Check location name for this automaton *)
          check_location_name index_of_locations automaton_index automaton_name location_name
        )
      in

      (* Check whether another location was declared for this automaton and, if not, store the location *)
      let check3 = if Hashtbl.mem hashtable automaton_name then(
          (* Retrieve former location name *)
          let old_name = Hashtbl.find hashtable automaton_name in
          (* If same name: just warning *)
          if old_name = location_name then(
            (* Warning *)
            print_warning ("Automaton `" ^ automaton_name ^ "` is assigned several times to location name `" ^ location_name ^ "` in the correctness property.");
            (* No problem *)
            true
          )else(
            (* Otherwise: error *)
            print_warning ("Automaton `" ^ automaton_name ^ "` is assigned to several different location names (e.g., `" ^ location_name ^ "` and `" ^ old_name ^ "`) in the correctness property.");
            (* Problem *)
            false
          )
        ) else (
          (* Add to hash table *)
          Hashtbl.add hashtable automaton_name location_name;
          (* No problem *)
          true
        )
      in

      (* Update checks *)
      checks_passed := !checks_passed && check1 && check2 && check3;

    ) parsed_unreachable_locations;

  (* If problem: returns dummy result *)
  if not !checks_passed then(
    [] , false
  )else(
    (* Return all pairs `loc[automaton] = location` *)
    let pairs = Hashtbl.fold (fun automaton_name location_name former_pairs ->
        (* Retrieve automaton index (no test because was tested earlier) *)
        let automaton_index = Hashtbl.find index_of_automata automaton_name in
        (* Retrieve location index (no test because was tested earlier) *)
        let location_index = Hashtbl.find index_of_locations.(automaton_index) location_name in
        (* Add new pair *)
        (automaton_index, location_index) :: former_pairs
      ) hashtable [] in
    pairs , true
  )


(*------------------------------------------------------------*)
(* Check a list of discrete constraints declaration (i.e., of the form `d ~ constant(s)`)
 * Return a list of discrete_constraint, and a Boolean encoding whether all checks passed
 * May print warnings or errors
*)
(*------------------------------------------------------------*)
let check_and_convert_unreachable_discrete_constraints index_of_variables type_of_variables variable_names discrete parsed_unreachable_locations =
  (* Create a hash table to check for double declarations *)
  let hashtable = Hashtbl.create (List.length discrete) in

  (*(* Horrible hack: store not the constants, but their string representation (to allow for a quick representation of single constants and pairs of constants in intervals) *)
    	let string_of_parsed_discrete_constraint = function
    		| Parsed_discrete_l (_ , v)
    			-> "<" ^ (NumConst.string_of_numconst v)
    		| Parsed_discrete_leq (_ , v)
    			-> "<=" ^ (NumConst.string_of_numconst v)
    		| Parsed_discrete_equal (_ , v)
    			-> "=" ^ (NumConst.string_of_numconst v)
    		| Parsed_discrete_geq (_ , v)
    			-> ">=" ^ (NumConst.string_of_numconst v)
    		| Parsed_discrete_g (_ , v)
    			-> ">" ^ (NumConst.string_of_numconst v)
    		| Parsed_discrete_interval (_ , min_bound, max_bound)
    			-> " in [" ^ (NumConst.string_of_numconst min_bound)
    					^ " .. "
    					^ (NumConst.string_of_numconst max_bound)
    					^ "]"
    	in*)

  (* Local function to convert a parsed constraint into an abstract discrete constraint *)
  (*** NOTE: will only be called once the discrete variable name has been checked for name and type ***)
  let convert_discrete_constraint = function
    | Parsed_discrete_l (discrete_name , v)
      -> Discrete_l (Hashtbl.find index_of_variables discrete_name , v)
    | Parsed_discrete_leq (discrete_name , v)
      -> Discrete_leq (Hashtbl.find index_of_variables discrete_name , v)
    | Parsed_discrete_equal (discrete_name , v)
      -> Discrete_equal (Hashtbl.find index_of_variables discrete_name , v)
    | Parsed_discrete_geq (discrete_name , v)
      -> Discrete_geq (Hashtbl.find index_of_variables discrete_name , v)
    | Parsed_discrete_g (discrete_name , v)
      -> Discrete_g (Hashtbl.find index_of_variables discrete_name , v)
    | Parsed_discrete_interval (discrete_name , min_bound, max_bound)
      -> Discrete_interval (Hashtbl.find index_of_variables discrete_name , min_bound, max_bound)
  in

  (* Global check *)
  let checks_passed = ref true in

  (* Iterate on parsed_unreachable_predicate and check for names and values *)
  List.iter(fun parsed_unreachable_predicate ->
      let parsed_discrete_constraint =
        match parsed_unreachable_predicate with
        | Parsed_unreachable_discrete parsed_discrete_constraint -> parsed_discrete_constraint
        | _ -> raise (InternalError("Expecting a Parsed_unreachable_discrete, which should have been checked earlier."))
      in

      (* Get discrete variable name *)
      let discrete_name = match parsed_discrete_constraint with
        | Parsed_discrete_l (variable_name , _)
        | Parsed_discrete_leq (variable_name , _)
        | Parsed_discrete_equal (variable_name , _)
        | Parsed_discrete_geq (variable_name , _)
        | Parsed_discrete_g (variable_name , _)
          -> variable_name
        | Parsed_discrete_interval (variable_name , _, _)
          -> variable_name
      in

      (* Check discrete name and type *)
      let check1 =
        (* 1a. Check for name *)
        if not (Hashtbl.mem index_of_variables discrete_name) then(
          (* Print error *)
          print_error ("The discrete variable `" ^ discrete_name ^ "` used in the correctness property does not exist in this model.");
          (* Problem *)
          false
          (* 1b. Check for type *)
        )else(
          (* Get variable index *)
          (*** NOTE: safe because Hashtbl.mem was checked in test 1a ***)
          let variable_index = Hashtbl.find index_of_variables discrete_name in
          (* Check type *)
          let variable_type = type_of_variables variable_index in
          if variable_type <> Var_type_discrete then(
            (* Print error *)
            print_error ("The variable `" ^ discrete_name ^ "` used in the correctness property must be a discrete variable (clocks and parameters are not allowed at this stage).");
            (* Problem *)
            false
          )else true
        )
      in

      (* Check value1 <= value2 *)
      let check2 = match parsed_discrete_constraint with
        | Parsed_discrete_l _
        | Parsed_discrete_leq _
        | Parsed_discrete_equal _
        | Parsed_discrete_geq _
        | Parsed_discrete_g _
          -> true
        | Parsed_discrete_interval (_ , min_bound , max_bound )
          -> if NumConst.g min_bound max_bound then(
            print_error ("The interval `[" ^ (NumConst.string_of_numconst min_bound) ^ " , " ^ (NumConst.string_of_numconst max_bound) ^ "]` used in the correctness property is not well-formed.");
            (* Problem *)
            false
          ) else true
      in

      (* Check whether another value was declared for this discrete and, if not, store the value *)
      let check3 =
        (* Only perform this if further checks passed *)
        if not (check1 && check2) then false else(
          (* Compute abstract constraint *)
          let abstract_constraint = convert_discrete_constraint parsed_discrete_constraint in
          (* Check whether another value was declared for this discrete *)
          if Hashtbl.mem hashtable discrete_name then(
            (* Retrieve former value *)
            let old_constraint = Hashtbl.find hashtable discrete_name in
            (* If same name: just warning *)
            if old_constraint = abstract_constraint then(
              (* Warning *)
              print_warning ("Discrete variable `" ^ discrete_name ^ "` is compared several times to the same constant in the correctness property.");
              (* No problem *)
              true
            )else(
              (* Otherwise: error *)
              print_warning ("Discrete variable `" ^ discrete_name ^ "` is compared several times to different constants in the correctness property.");
              (* Problem *)
              false
            )
          ) else (
            (* Add to hash table *)
            Hashtbl.add hashtable discrete_name abstract_constraint;
            (* No problem *)
            true
          )
        )
      in

      (* Update checks *)
      checks_passed := !checks_passed && check1 && check2 && check3;

    ) parsed_unreachable_locations;

  (* If problem: returns dummy result *)
  if not !checks_passed then(
    [] , false
  )else(
    (* Return all abstract constraints *)
    let abstract_constraints = Hashtbl.fold (fun _ abstract_constraint former_abstract_constraints ->
        (*			(* Retrieve automaton index (no test because was tested earlier) *)
          			let discrete_index = Hashtbl.find index_of_variables discrete_name in*)
        (* Add new abstract_constraint *)
        abstract_constraint :: former_abstract_constraints
      ) hashtable [] in
    abstract_constraints , true
  )


(*------------------------------------------------------------*)
(* Check one global location declaration (i.e., a list of location predicates and/or of discrete constraints)
 * Return a converted global location, and a Boolean encoding whether all checks passed
 * May print warnings or errors
*)
(*------------------------------------------------------------*)
let check_and_convert_unreachable_global_location index_of_variables type_of_variables discrete variable_names index_of_automata index_of_locations parsed_unreachable_global_location =
  (* Split the predicates in location and discrete *)
  let parsed_unreachable_locations, parsed_discrete_constraints = List.partition (function
      | Parsed_unreachable_loc _ -> true
      | _ -> false
    ) parsed_unreachable_global_location
  in

  (* Call dedicated functions *)
  let unreachable_locations, check1 = check_and_convert_unreachable_local_locations index_of_automata index_of_locations parsed_unreachable_locations in
  let discrete_constraints, check2 = check_and_convert_unreachable_discrete_constraints index_of_variables type_of_variables variable_names discrete parsed_discrete_constraints in

  (* Return *)
  {
    unreachable_locations = unreachable_locations;
    discrete_constraints  = discrete_constraints;
  }
  ,
  check1 && check2

*)

(*------------------------------------------------------------*)
(** Check a state predicate *)
(*------------------------------------------------------------*)

let check_parsed_state_predicate parsing_infos expr =
    let variable_infos = parsing_infos.variable_infos in
    ParsingStructureMeta.all_variables_defined_in_parsed_state_predicate
        parsing_infos
        variable_infos
        (* Undefined variable name callback, triggered if an undefined variable is found *)
        (Some (fun variable_name -> print_error ("Undefined variable name `" ^ variable_name ^ "` in the property")))
        (* Undefined automaton name callback, triggered if an undefined automaton is found *)
        (Some (fun automaton_name -> print_error ("Unknown automaton name `" ^ automaton_name ^ "` in the property.")))
        (* Undefined location name callback, triggered if an undefined location is found *)
        (Some (fun automaton_name loc_name -> print_error ("Unknown location name `" ^ loc_name ^ "` in automaton `" ^ automaton_name ^ "` in the property.")))
        expr
    &&
    ParsingStructureMeta.only_discrete_in_parsed_state_predicate variable_infos (Some (fun var_type variable_name -> print_error ("Variable `" ^ variable_name ^ "` is a " ^ DiscreteType.string_of_var_type var_type ^ ". Only discrete can be used in property."))) expr

(*------------------------------------------------------------*)
(** Generic function checking whether a name is a valid parameter name *)
(*------------------------------------------------------------*)
let check_parameter_name suffix_explanation_string variable_infos parameter_name =
	(* First check it is a variable *)
	if not (VariableInfo.is_global_variable_is_defined variable_infos parameter_name) then(
		print_error ("Parameter " ^ parameter_name ^ " is not a defined variable" ^ suffix_explanation_string);
		false
	) else(
	    (* Note: By using VariableInfo.is_param, we consider that global variable parameter_name exist ! *)
	    (* It should be check before *)
		if not (VariableInfo.is_param variable_infos (parameter_name, 0)) then (
			print_error ("Variable " ^ parameter_name ^ " is not a parameter" ^ suffix_explanation_string);
			false
		) else true
	)

(*------------------------------------------------------------*)
(** Check that all parameters in the projection definition are valid *)
(*------------------------------------------------------------*)
let check_projection_definition variable_infos = function
	| None -> true
	| Some parsed_parameters -> (
		let well_formed = ref true in
		List.iter (fun parsed_parameter ->
			if not (check_parameter_name " in the projection definition" variable_infos parsed_parameter) then
				well_formed := false
			) parsed_parameters;
		!well_formed
		)

(*------------------------------------------------------------*)
(** Check that the optimization definition is valid *)
(*------------------------------------------------------------*)
(*let check_optimization parameters_names = function
	| No_parsed_optimization -> true
	
	| Parsed_minimize parameter_name | Parsed_maximize parameter_name ->
		if not (List.mem parameter_name parameters_names) then(
		print_error ("Parameter " ^ parameter_name  ^ " is not a valid parameter in the optimization definition.");
		false
		) else true*)


(*------------------------------------------------------------*)
(** Check the parsed_pval w.r.t. the model parameters *)
(*------------------------------------------------------------*)
let check_parsed_pval useful_parsing_model_information (parsed_pval : ParsingStructure.parsed_pval) =
	(* Compute the list of variable names *)
	let list_of_variables, _ = List.split parsed_pval in

	(* Compute the multiply defined variables *)
	let multiply_defined_variables = elements_existing_several_times list_of_variables in
	(* Print an error for each of them *)
	List.iter (fun variable_name -> print_error ("The parameter `" ^ variable_name ^ "` was assigned several times a valuation in the reference valuation.")) multiply_defined_variables;

	(*** TODO: only warns if it is always defined to the same value ***)

		(* Check if the variables are all defined *)
		let all_defined = List.fold_left
				(fun all_defined variable_name ->
					if List.mem variable_name list_of_variables then all_defined
					else (
					print_error ("The parameter `" ^ variable_name ^ "` was not assigned a valuation in the reference valuation.");
					false
					)
				)
				true
				useful_parsing_model_information.parameter_names
		in

	(* Check if some defined variables are not parameters (and warn) *)
	List.iter
		(fun variable_name ->
		if not (List.mem variable_name useful_parsing_model_information.parameter_names) then (
			print_warning ("`" ^ variable_name ^ "`, which is assigned a valuation in the reference valuation, is not a valid parameter name.")
		)
		)
		list_of_variables
	;
	
	(* If something went wrong: raise an error *)
	multiply_defined_variables = [] && all_defined


(*------------------------------------------------------------*)
(** Check the parsed_pval w.r.t. the model parameters *)
(*------------------------------------------------------------*)
let check_parsed_hyper_rectangle useful_parsing_model_information (parsed_hyper_rectangle : ParsingStructure.parsed_pdomain) =
	(* Compute the list of variable names *)
	let list_of_variables = List.map (fun (parameter_name, _, _) -> parameter_name) parsed_hyper_rectangle in

	(* Compute the multiply defined variables *)
	let multiply_defined_variables = elements_existing_several_times list_of_variables in
	(* Print an error for each of them *)
	List.iter (fun variable_name -> print_error ("The parameter `" ^ variable_name ^ "` was assigned several times a valuation in the reference parameter domain.")) multiply_defined_variables;

	(*** TODO: only warns if it is always defined to the same value ***)

		(* Check if the variables are all defined *)
		let all_defined = List.fold_left
				(fun all_defined variable_name ->
					if List.mem variable_name list_of_variables then all_defined
					else (
					print_error ("The parameter `" ^ variable_name ^ "` was not assigned a valuation in the reference parameter domain.");
					false
					)
				)
				true
				useful_parsing_model_information.parameter_names
		in

	(* Check that the intervals are not null *)
	let all_intervals_ok = List.fold_left
		(fun all_intervals_ok (variable_name, a, b) ->
			if NumConst.le a b then all_intervals_ok
			else (
			print_error ("The interval [" ^ (NumConst.string_of_numconst a) ^ ", " ^ (NumConst.string_of_numconst b) ^ "] is empty for parameter `" ^ variable_name ^ "` in the reference parameter domain.");
			false
			)
		)
		true
		parsed_hyper_rectangle
	in

	(* Check if some defined variables are not parameters (and warn) *)
	List.iter
		(fun variable_name ->
		if not (List.mem variable_name useful_parsing_model_information.parameter_names) then (
			print_warning ("`" ^ variable_name ^ "`, which is assigned a valuation in the reference parameter domain, is not a valid parameter name.")
		)
		)
		list_of_variables
	;
	
	(* If something went wrong: raise an error *)
	multiply_defined_variables = [] && all_defined && all_intervals_ok


(*------------------------------------------------------------*)
(** Check a parsed interval *)
(*------------------------------------------------------------*)
let check_parsed_interval (useful_parsing_model_information : useful_parsing_model_information) = function
	| Parsed_closed_closed_interval (parsed_duration_1, parsed_duration_2)
	| Parsed_closed_open_interval (parsed_duration_1, parsed_duration_2)
	| Parsed_open_closed_interval (parsed_duration_1, parsed_duration_2)
	| Parsed_open_open_interval (parsed_duration_1, parsed_duration_2)
		->
		let variable_infos = useful_parsing_model_information.variable_infos in
			let check1 = ParsingStructureMeta.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message parsed_duration_1 in
			let check2 = (if ParsingStructureMeta.no_variables_in_linear_expression variable_infos parsed_duration_1
						then true
						else (print_error("No variable is allowed in the left-side bound of the interval of a TCTL operator in the property definition (only constants and parameters are allowed)."); false))
			in
			let check3 = ParsingStructureMeta.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message parsed_duration_2 in
			let check4 = (if ParsingStructureMeta.no_variables_in_linear_expression variable_infos parsed_duration_2
						then true
						else (print_error("No variable is allowed in the right-side bound of the interval of a TCTL operator in the property definition (only constants and parameters are allowed)."); false))
			in
			check1 && check2 && check3 && check4

	| Parsed_zero_closed_interval parsed_duration
	| Parsed_zero_open_interval parsed_duration
	| Parsed_closed_infinity_interval parsed_duration
	| Parsed_open_infinity_interval parsed_duration
		->
		let variable_infos = useful_parsing_model_information.variable_infos in
			let check1 = ParsingStructureMeta.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message parsed_duration in
			let check2 = (if ParsingStructureMeta.no_variables_in_linear_expression variable_infos parsed_duration
						then true
						else (print_error("No variable is allowed in the left-side bound of an infinite interval of a TCTL operator in the property definition (only constants and parameters are allowed)."); false))
			in
			check1 && check2



(*------------------------------------------------------------*)
(** Check the correctness property declaration       *)
(*------------------------------------------------------------*)
let check_property_option (useful_parsing_model_information : useful_parsing_model_information) (parsed_property_option : ParsingStructure.parsed_property option) =

	let index_of_actions	= useful_parsing_model_information.index_of_actions in

    let variable_infos = useful_parsing_model_information.variable_infos in
	(* Check *)
	match parsed_property_option with
	| None -> true
	| Some parsed_property ->
		begin
		match parsed_property.property with

		(*------------------------------------------------------------*)
		(* Basic properties *)
		(*------------------------------------------------------------*)

		(* Validity *)
		| Parsed_Valid -> true


		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Parsed_EF parsed_state_predicate
		
		(* Safety *)
		| Parsed_AGnot parsed_state_predicate

		(* Global invariant *)
		| Parsed_AG parsed_state_predicate

		(* Unavoidability *)
		| Parsed_AF parsed_state_predicate

		(* Exists globally *)
		| Parsed_EG parsed_state_predicate

			->
			check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
		
		(* Until and variants *)
		| Parsed_ER (parsed_state_predicate_phi, parsed_state_predicate_psi)
		| Parsed_EU (parsed_state_predicate_phi, parsed_state_predicate_psi)
		| Parsed_EW (parsed_state_predicate_phi, parsed_state_predicate_psi)
		| Parsed_AR (parsed_state_predicate_phi, parsed_state_predicate_psi)
		| Parsed_AU (parsed_state_predicate_phi, parsed_state_predicate_psi)
		| Parsed_AW (parsed_state_predicate_phi, parsed_state_predicate_psi)
			->
			evaluate_and
				(check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate_phi)
				(check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate_psi)


		(*------------------------------------------------------------*)
		(* Non-nested CTL: timed version *)
		(*------------------------------------------------------------*)
		(* Reachability with timing constraint *)
		| Parsed_EF_timed (parsed_interval, parsed_state_predicate)
		(* Unavoidability with timing constraint *)
		| Parsed_AF_timed (parsed_interval, parsed_state_predicate)
			->
			evaluate_and
				(check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate)
				(check_parsed_interval useful_parsing_model_information parsed_interval)

		(* Exists release with timing constraint *)
		| Parsed_ER_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi)
		(* Exists until with timing constraint *)
		| Parsed_EU_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi)
		(* Exists weak until with timing constraint *)
		| Parsed_EW_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi)
		(* Always release with timing constraint *)
		| Parsed_AR_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi)
		(* Always until with timing constraint *)
		| Parsed_AU_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi)
		(* Always weak until with timing constraint *)
		| Parsed_AW_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi)
			->
			evaluate_all [
				check_parsed_interval useful_parsing_model_information parsed_interval
				;
				check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				;
				check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate_psi
				]


		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)
		
		(* Reachability with minimization of a parameter valuation *)
		| Parsed_EFpmin (parsed_state_predicate , parameter_name)
		| Parsed_EFpmax (parsed_state_predicate , parameter_name)
			->
			(*** NOTE: two checks to allow to check both side of the equality whatever happens ***)
			evaluate_and
				(check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate)
				(check_parameter_name " in the optimization definition" variable_infos parameter_name)

		
		(* Reachability with minimal-time *)
		| Parsed_EFtmin parsed_state_predicate ->
			check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
		
		
		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)
		
		(* Accepting infinite-run (cycle) through a state predicate *)
		| Parsed_Cycle_Through parsed_state_predicate ->
			check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
		
		(* Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
		| Parsed_Cycle_Through_generalized parsed_state_predicate_list ->
			(* Do a fold_left to check everything even in case of failure *)
			List.fold_left (fun current_result parsed_state_predicate ->
				(* Make sure we do evaluate this part even if current_result is false *)
				let check = check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate in
				current_result && check
				) true parsed_state_predicate_list

		(* Infinite-run (cycle) with non-Zeno assumption *)
		| Parsed_NZ_Cycle -> true


		(*------------------------------------------------------------*)
		(* Deadlock-freeness *)
		(*------------------------------------------------------------*)
		
		(* Deadlock-free synthesis *)
		| Parsed_Deadlock_Freeness -> true
		
		
		(*------------------------------------------------------------*)
		(* Inverse method, trace preservation, robustness *)
		(*------------------------------------------------------------*)
		
		(* Inverse method with complete, non-convex result *)
		| Parsed_IM parsed_pval
		(* Non-complete, non-deterministic inverse method with convex result *)
		| Parsed_ConvexIM parsed_pval
		(* IMK *)
		| Parsed_IMK parsed_pval
		(* IMunion *)
		| Parsed_IMunion parsed_pval
			->
			check_parsed_pval useful_parsing_model_information parsed_pval
		
		(* PRP *)
		| Parsed_PRP (parsed_state_predicate, parsed_pval) ->
			(*** NOTE: two checks to allow to check both side of the equality whatever happens ***)
			evaluate_and
				(check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate)
				(check_parsed_pval useful_parsing_model_information parsed_pval)
		
		
		(*------------------------------------------------------------*)
		(* Cartography algorithms *)
		(*------------------------------------------------------------*)
		
		(* Cartography *)
		| Parsed_Cover_cartography (parsed_hyper_rectangle, step)
		(* Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
		| Parsed_Shuffle_cartography (parsed_hyper_rectangle, step)
		(* Look for the border using the cartography*)
		| Parsed_Border_cartography (parsed_hyper_rectangle, step)
		(* Randomly pick up values for a given number of iterations *)
		| Parsed_Random_cartography (parsed_hyper_rectangle, _, step)
		(* Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
		| Parsed_RandomSeq_cartography (parsed_hyper_rectangle, _, step)
			->
			evaluate_and
				(* The step has to be > 0 *)
				(NumConst.g step NumConst.zero)
				(check_parsed_hyper_rectangle useful_parsing_model_information parsed_hyper_rectangle)
	
		(* Cover the whole cartography using learning-based abstractions *)
		| Parsed_Learning_cartography (parsed_state_predicate, parsed_hyper_rectangle, step)
		(* Parametric reachability preservation *)
		| Parsed_PRPC (parsed_state_predicate, parsed_hyper_rectangle, step)
		->
			(* The step has to be > 0 *)
			let check1 = NumConst.g step NumConst.zero in
			let check2 = check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate in
			let check3 = check_parsed_hyper_rectangle useful_parsing_model_information parsed_hyper_rectangle in
			check1 && check2 && check3


		(*------------------------------------------------------------*)
		(* Observer patterns *)
		(*------------------------------------------------------------*)
		
		(* CASE TWO ACTIONS *)
		
		(* if a2 then a1 has happened before *)
		| Parsed_pattern (ParsingStructure.Parsed_action_precedence_acyclic ( a1 , a2 ))
		(* everytime a2 then a1 has happened before *)
		| Parsed_pattern (ParsingStructure.Parsed_action_precedence_cyclic ( a1 , a2 ))
		(* everytime a2 then a1 has happened once before *)
		| Parsed_pattern (ParsingStructure.Parsed_action_precedence_cyclicstrict ( a1 , a2 ))
			->
			(* Check action names (perform 2 even if one fails) *)
			evaluate_and
				(check_action_name index_of_actions a1)
				(check_action_name index_of_actions a2)
		

		(* CASE ACTION + DEADLINE *)
		
		(* a within d *)
		| Parsed_pattern (ParsingStructure.Parsed_action_deadline ( a , d ))
			->
			(* Check action name and deadline (perform all even if one fails) *)
			let check1 = check_action_name index_of_actions a in
			let check2 = ParsingStructureMeta.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message d in
			let check3 = (if ParsingStructureMeta.no_variables_in_linear_expression variable_infos d
						then true
						else (print_error("No variable is allowed in the property definition (only constants and parameters)."); false))
			in
			check1 && check2 && check3


		(* CASE 2 ACTIONS + DEADLINE *)
		
		(* if a2 then a1 happened within d before *)
		| Parsed_pattern (ParsingStructure.Parsed_TB_Action_precedence_acyclic (a1, a2, d))
		(* everytime a2 then a1 happened within d before *)
		| Parsed_pattern (ParsingStructure.Parsed_TB_Action_precedence_cyclic (a1, a2, d))
		(* everytime a2 then a1 happened once within d before *)
		| Parsed_pattern (ParsingStructure.Parsed_TB_Action_precedence_cyclicstrict (a1, a2, d))
		
		(* if a1 then eventually a2 within d *)
		| Parsed_pattern (ParsingStructure.Parsed_TB_response_acyclic (a1, a2, d))
		(* everytime a1 then eventually a2 within d *)
		| Parsed_pattern (ParsingStructure.Parsed_TB_response_cyclic (a1, a2, d))
		(* everytime a1 then eventually a2 within d once before next *)
		| Parsed_pattern (ParsingStructure.Parsed_TB_response_cyclicstrict (a1, a2, d))
			->
			(* Check action names and deadline (perform 3 even if one fails) *)
			let check1 = check_action_name index_of_actions a1 in
			let check2 = check_action_name index_of_actions a2 in
			let check3 = ParsingStructureMeta.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message d in
			let check4 = (if ParsingStructureMeta.no_variables_in_linear_expression variable_infos d
						then true
						else (print_error("No variable is allowed in the property definition (only constants and parameters)."); false))
			in
			check1 && check2 && check3 && check4


		(* CASE SEQUENCES (list of actions) *)

		(* sequence a1, …, an *)
		| Parsed_pattern (ParsingStructure.Parsed_Sequence_acyclic (actions_list))
		(* always sequence a1, …, an *)
		| Parsed_pattern (ParsingStructure.Parsed_Sequence_cyclic (actions_list))
			->
			(* Check action names (use a fold_left instead of forall to ensure that all actions will be checked) *)
			List.fold_left (fun current_result a ->
				(* Make sure we do evaluate this part even if current_result is false *)
				let check = check_action_name index_of_actions a in
				current_result && check
				) true actions_list


		(*------------------------------------------------------------*)
		(* Games *)
		(*------------------------------------------------------------*)

		(* Parametric timed game: reachability condition *)
		| Parsed_Win parsed_state_predicate -> check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate

		end



(************************************************************)
(** Converting the property  *)
(************************************************************)


(*------------------------------------------------------------*)
(** Convert the parsed parsed_pval into a valid parsed_pval *)
(*------------------------------------------------------------*)
let convert_parsed_pval useful_parsing_model_information (parsed_pval : ParsingStructure.parsed_pval) : PVal.pval =
	let pval = new PVal.pval in
	for i = 0 to useful_parsing_model_information.nb_parameters - 1 do
		let parameter_name = useful_parsing_model_information.variable_infos.variables.(i) in
		let valuation = try(
			List.assoc parameter_name parsed_pval
		) with Not_found ->
			raise (InternalError ("The parameter name `" ^ parameter_name ^ "` was not found in parsed_pval although checks should have been performed before."))
		in
		pval#set_value i valuation
	done;
	(* Return the parameter valuation *)
	pval


(*------------------------------------------------------------*)
(** Convert the parsed hyper_rectangle into a valid hyper_rectangle *)
(*------------------------------------------------------------*)
let convert_parsed_hyper_rectangle variable_infos (parsed_hyper_rectangle : ParsingStructure.parsed_pdomain) : HyperRectangle.hyper_rectangle =
	
	let hyper_rectangle = new HyperRectangle.hyper_rectangle in
	
	List.iter (fun (variable_name, a, b) ->
		try
			(* Get the variable index *)
			let variable_index = index_of_variable_name variable_infos variable_name in
			(* Update the variable value *)
			hyper_rectangle#set_min variable_index a;
			hyper_rectangle#set_max variable_index b;
		with Not_found ->
			(* No problem: this must be an invalid parameter name (which is ignored) *)
			()
			(* 			raise (InternalError ("The variable name `" ^ variable_name ^ "` was not found in the list of variables although checks should have been performed before.")) *)
		) parsed_hyper_rectangle;
	hyper_rectangle


let convert_synthesis_type = function
	| Parsed_exemplify	-> Exemplification
	| Parsed_synthesis	-> Synthesis
	| Parsed_witness	-> Witness



(*------------------------------------------------------------*)
(** Convert a list of parsed parameters into a list of variable_index *)
(*------------------------------------------------------------*)
let convert_projection_definition (index_of_variables : (Automaton.variable_name, Automaton.variable_index) Hashtbl.t)  = function
	(* No property *)
	| None -> None
	(* Some property *)
	| Some (parsed_property : parsed_property) ->
	begin
		match parsed_property.projection with
		| None -> None
		| Some (parsed_parameters : string list) -> Some (List.map (fun (parsed_parameter_name : string) ->
			(* No check because this was checked before *)
			Hashtbl.find index_of_variables parsed_parameter_name
			) parsed_parameters)
	end



type converted_observer_structure = {
	(*  observer_actions, observer_actions_per_location, observer_location_urgency, observer_invariants, observer_transitions *)
	observer_structure					: Automaton.action_index list * (Automaton.action_index list) array * AbstractModel.location_urgency array * AbstractModel.guard array * AbstractModel.transition list array array;
	
	nb_transitions_for_observer			: int;
	
	initial_observer_constraint_option	: LinearConstraint.px_linear_constraint option;
}



(* Convert ParsingStructure.parsed_property into AbstractProperty.property *)
let convert_property_option (useful_parsing_model_information : useful_parsing_model_information) (nb_actions : int) (observer_automaton_index_option : automaton_index option) (observer_nosync_index_option : action_index option) (parsed_property_option : ParsingStructure.parsed_property option) : (AbstractProperty.abstract_property option * converted_observer_structure option) =

	let index_of_actions	= useful_parsing_model_information.index_of_actions in
	let index_of_variables	= useful_parsing_model_information.variable_infos.index_of_variables in

    let variable_infos = useful_parsing_model_information.variable_infos in

	(* Convert *)
	match parsed_property_option with
	(* No property, no observer *)
	| None -> None, None
	
	(* Some property *)
	| Some parsed_property ->
		let property , converted_observer_structure_option =
		match parsed_property.property with

		(*------------------------------------------------------------*)
		(* Basic properties *)
		(*------------------------------------------------------------*)

		(* Validity *)
		| Parsed_Valid -> Valid, None


		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Parsed_EF parsed_state_predicate ->
			(* Return a property and no observer *)
			EF (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
			
		(* Safety *)
		| Parsed_AGnot parsed_state_predicate ->
			(* Return a property and no observer *)
			AGnot (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
		
		(* Global invariant *)
		| Parsed_AG parsed_state_predicate ->
			(* Return a property and no observer *)
			AG (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None

		(* Exists globally *)
		| Parsed_EG parsed_state_predicate ->
			(* Return a property and no observer *)
			EG (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None

		(* Exists release *)
		| Parsed_ER (parsed_state_predicate_phi, parsed_state_predicate_psi)
			->
			(* Return a property and no observer *)
			ER
				(PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Exists until *)
		| Parsed_EU (parsed_state_predicate_phi, parsed_state_predicate_psi)
			->
			(* Return a property and no observer *)
			EU
				(PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Exists weak until *)
		| Parsed_EW (parsed_state_predicate_phi, parsed_state_predicate_psi)
			->
			(* Return a property and no observer *)
			EW
				(PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Unavoidability *)
		| Parsed_AF parsed_state_predicate ->
			(* Return a property and no observer *)
			AF (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None

		(* Always release *)
		| Parsed_AR (parsed_state_predicate_phi, parsed_state_predicate_psi)
			->
			(* Return a property and no observer *)
			AR
				(PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Always until *)
		| Parsed_AU (parsed_state_predicate_phi, parsed_state_predicate_psi)
			->
			(* Return a property and no observer *)
			AU
				(PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Always until *)
		| Parsed_AW (parsed_state_predicate_phi, parsed_state_predicate_psi)
			->
			(* Return a property and no observer *)
			AW
				(PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None


		(*------------------------------------------------------------*)
		(* Non-nested CTL (timed version) *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Parsed_EF_timed (parsed_interval, parsed_state_predicate) ->
			(* Return a property and no observer *)
			EF_timed
				(PropertyConverter.timed_interval_of_parsed_interval useful_parsing_model_information parsed_interval
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None


		(* Unavoidability with timing constraint *)
		| Parsed_AF_timed (parsed_interval, parsed_state_predicate) ->
			(* Return a property and no observer *)
			AF_timed
				(PropertyConverter.timed_interval_of_parsed_interval useful_parsing_model_information parsed_interval
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None

		(* Exists release with timing constraint *)
		| Parsed_ER_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi) ->
			(* Return a property and no observer *)
			ER_timed
				(PropertyConverter.timed_interval_of_parsed_interval useful_parsing_model_information parsed_interval
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Exists until with timing constraint *)
		| Parsed_EU_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi) ->
			(* Return a property and no observer *)
			EU_timed
				(PropertyConverter.timed_interval_of_parsed_interval useful_parsing_model_information parsed_interval
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Exists weak until with timing constraint *)
		| Parsed_EW_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi) ->
			(* Return a property and no observer *)
			EW_timed
				(PropertyConverter.timed_interval_of_parsed_interval useful_parsing_model_information parsed_interval
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Always release with timing constraint *)
		| Parsed_AR_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi) ->
			(* Return a property and no observer *)
			AR_timed
				(PropertyConverter.timed_interval_of_parsed_interval useful_parsing_model_information parsed_interval
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Always until with timing constraint *)
		| Parsed_AU_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi) ->
			(* Return a property and no observer *)
			AU_timed
				(PropertyConverter.timed_interval_of_parsed_interval useful_parsing_model_information parsed_interval
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None

		(* Always weak until with timing constraint *)
		| Parsed_AW_timed (parsed_interval, parsed_state_predicate_phi, parsed_state_predicate_psi) ->
			(* Return a property and no observer *)
			AW_timed
				(PropertyConverter.timed_interval_of_parsed_interval useful_parsing_model_information parsed_interval
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_phi
				,
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate_psi)
			,
			None


		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)
		
		(* Reachability with minimization of a parameter valuation *)
		| Parsed_EFpmin (parsed_state_predicate , parameter_name) ->
			EFpmin (
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate
				,
				Hashtbl.find useful_parsing_model_information.variable_infos.index_of_variables parameter_name
			)
			,
			None
		
		(* Reachability with maximization of a parameter valuation *)
		| Parsed_EFpmax (parsed_state_predicate , parameter_name) ->
			EFpmax (
				PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate
				,
				Hashtbl.find useful_parsing_model_information.variable_infos.index_of_variables parameter_name
			)
			,
			None
		
		(* Reachability with minimal-time *)
		| Parsed_EFtmin parsed_state_predicate ->
			EFtmin (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
		
		
		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)
		
		(* Accepting infinite-run (cycle) through a state predicate *)
		| Parsed_Cycle_Through parsed_state_predicate ->
			Cycle_through (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
		
		(* Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
		| Parsed_Cycle_Through_generalized parsed_state_predicate_list ->
			Cycle_through_generalized (List.map (PropertyConverter.convert_state_predicate useful_parsing_model_information) parsed_state_predicate_list)
			,
			None

		
		(* Infinite-run (cycle) with non-Zeno assumption *)
		| Parsed_NZ_Cycle -> NZ_Cycle, None
		

		(*------------------------------------------------------------*)
		(* Deadlock-freeness *)
		(*------------------------------------------------------------*)
		
		(* Deadlock-free synthesis *)
		| Parsed_Deadlock_Freeness -> Deadlock_Freeness, None
		
		
		(*------------------------------------------------------------*)
		(* Inverse method, trace preservation, robustness *)
		(*------------------------------------------------------------*)
		
		(* Inverse method with complete, non-convex result *)
		| Parsed_IM parsed_pval ->
			IM (convert_parsed_pval useful_parsing_model_information parsed_pval)
			,
			None
			
		(* Non-complete, non-deterministic inverse method with convex result *)
		| Parsed_ConvexIM parsed_pval ->
			ConvexIM (convert_parsed_pval useful_parsing_model_information parsed_pval)
			,
			None

		(* PRP *)
		| Parsed_PRP (parsed_state_predicate , parsed_pval) ->
			PRP (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate , convert_parsed_pval useful_parsing_model_information parsed_pval)
			,
			None

		(* IMK *)
		| Parsed_IMK parsed_pval ->
			IMK (convert_parsed_pval useful_parsing_model_information parsed_pval)
			,
			None

		(* IMunion *)
		| Parsed_IMunion parsed_pval ->
			IMunion (convert_parsed_pval useful_parsing_model_information parsed_pval)
			,
			None

			
		(*------------------------------------------------------------*)
		(* Cartography algorithms *)
		(*------------------------------------------------------------*)
		
		(* Cartography *)
		| Parsed_Cover_cartography (parsed_hyper_rectangle, step) ->
			Cover_cartography ((convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle) , step)
			,
			None
		
		(* Cover the whole cartography using learning-based abstractions *)
		| Parsed_Learning_cartography (parsed_state_predicate, parsed_hyper_rectangle, step) ->
			Learning_cartography ((PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate , convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , step))
			,
			None
		
		(* Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
		| Parsed_Shuffle_cartography (parsed_hyper_rectangle, step) ->
			Shuffle_cartography (convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , step)
			,
			None
		
		(* Look for the border using the cartography*)
		| Parsed_Border_cartography (parsed_hyper_rectangle, step) ->
			Border_cartography (convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , step)
			,
			None
		
		(* Randomly pick up values for a given number of iterations *)
		| Parsed_Random_cartography (parsed_hyper_rectangle, nb, step) ->
			Random_cartography (convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , nb , step)
			,
			None
		
		(* Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
		| Parsed_RandomSeq_cartography (parsed_hyper_rectangle, nb, step) ->
			RandomSeq_cartography (convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , nb , step)
			,
			None
	
		(* Parametric reachability preservation *)
		| Parsed_PRPC (parsed_state_predicate, parsed_hyper_rectangle, step) ->
			PRPC (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate , convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , step)
			,
			None


		(*------------------------------------------------------------*)
		(* Observer patterns *)
		(*------------------------------------------------------------*)
		
		(* CASE TWO ACTIONS *)
		
		(* if a2 then a1 has happened before *)
		| Parsed_pattern (ParsingStructure.Parsed_action_precedence_acyclic _)
		(* everytime a2 then a1 has happened before *)
		| Parsed_pattern (ParsingStructure.Parsed_action_precedence_cyclic _)
		(* everytime a2 then a1 has happened once before *)
		| Parsed_pattern (ParsingStructure.Parsed_action_precedence_cyclicstrict _)

		(* CASE ACTION + DEADLINE *)
		
		(* a within d *)
		| Parsed_pattern (ParsingStructure.Parsed_action_deadline _)
		
		(* CASE 2 ACTIONS + DEADLINE *)
		
		(* if a2 then a1 happened within d before *)
		| Parsed_pattern (ParsingStructure.Parsed_TB_Action_precedence_acyclic _)
		(* everytime a2 then a1 happened within d before *)
		| Parsed_pattern (ParsingStructure.Parsed_TB_Action_precedence_cyclic _)
		(* everytime a2 then a1 happened once within d before *)
		| Parsed_pattern (ParsingStructure.Parsed_TB_Action_precedence_cyclicstrict _)
		
		(* if a1 then eventually a2 within d *)
		| Parsed_pattern (Parsed_TB_response_acyclic _)
		(* everytime a1 then eventually a2 within d *)
		| Parsed_pattern (Parsed_TB_response_cyclic _)
		(* everytime a1 then eventually a2 within d once before next *)
		| Parsed_pattern (Parsed_TB_response_cyclicstrict _)
		
		(* CASE SEQUENCES *)
		
		(* sequence a1, …, an *)
		| Parsed_pattern (Parsed_Sequence_acyclic _)
		(* always sequence a1, …, an *)
		| Parsed_pattern (Parsed_Sequence_cyclic _)
		
			->
			
			(* Print some information *)
			print_message Verbose_low ("*** The property is an observer pattern. Generating the observer…");
			
			(* First, let us retrieve some useful information *)
			
			(* Get the silent action index for the observer *)
			let observer_nosync_index = match observer_nosync_index_option with
				| Some action_index -> action_index
				| None -> raise (InternalError ("An observer action should have been defined."))
			in
			
			(* Get the observer automaton index *)
			let observer_automaton_index = match observer_automaton_index_option with
				| Some automaton_index -> automaton_index
				| None -> raise (InternalError ("An observer automaton index should have been defined."))
			in
			
			(* Print some information *)
			print_message Verbose_total ("*** Retrieved the observer automaton index: `" ^ (string_of_int observer_automaton_index) ^ "`");
			
			(* Get the local clock for the observer *)
			(*** WARNING: quite a HACK, here ***)
			let clock_obs = useful_parsing_model_information.nb_parameters + useful_parsing_model_information.nb_clocks - 1 in
			
			(* Print some information *)
			print_message Verbose_total ("*** Retrieved the observer's clock index (if any): `" ^ (string_of_int clock_obs) ^ "`");
			
			(* Create the function action index -> action name *)
			let action_index_of_action_name action_name = try (Hashtbl.find index_of_actions action_name) with Not_found -> raise (InternalError ("Action `" ^ action_name ^ "` not found in HashTable `index_of_actions` when defining function `action_index_of_action_name`, althoug this should have been checked before.")) in
			
			(* Create the function converting a ParsingStructure.parsed_duration into a LinearConstraint.p_linear_term *)
			let p_linear_term_of_parsed_duration (parsed_duration : ParsingStructure.parsed_duration) : LinearConstraint.p_linear_term = LinearConstraint.cast_p_of_pxd_linear_term (ExpressionConverter.Convert.linear_term_of_linear_expression variable_infos parsed_duration) true in
			
			(* Get the info from the observer pattern *)
			let observer_actions, observer_actions_per_location, observer_location_urgency, observer_invariants, observer_transitions, initial_observer_constraint, abstract_property =
				ObserverPatterns.get_observer_automaton action_index_of_action_name p_linear_term_of_parsed_duration nb_actions observer_automaton_index observer_nosync_index clock_obs parsed_property in

			(* Count transitions *)
			let nb_transitions_for_observer =
			(* Iterate on locations *)
				Array.fold_left (fun nb_transitions_for_locations transitions_for_this_location ->
					Array.fold_left (fun nb_transitions_for_actions transitions_for_this_action ->
					nb_transitions_for_actions + (List.length transitions_for_this_action)
					) nb_transitions_for_locations transitions_for_this_location
				) 0 observer_transitions
			in
			
			(* Create the structure *)
			let converted_observer_structure = {
				observer_structure					= observer_actions , observer_actions_per_location , observer_location_urgency , observer_invariants , observer_transitions;
				
				nb_transitions_for_observer			= nb_transitions_for_observer;
				
				initial_observer_constraint_option	= initial_observer_constraint;
			}
			in
			
			(* Return the property and the structure *)
			abstract_property
			,
			Some converted_observer_structure
	
		(*------------------------------------------------------------*)
		(* Games *)
		(*------------------------------------------------------------*)

		(* Parametric timed game: reachability condition *)
		| Parsed_Win parsed_state_predicate ->
			(* Return a property and no observer *)
			Win (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
		in

		(* Get the synthesis or emptiness type *)
		let synthesis_type = convert_synthesis_type parsed_property.synthesis_type in
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Convert the projection definition *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		print_message Verbose_total ("*** Building the projection definition…");
		let projection = convert_projection_definition index_of_variables parsed_property_option in

		(* Return the property *)
		Some {
			(* Emptiness or synthesis *)
			synthesis_type	= synthesis_type;
			(* Property *)
			property		= property;
			(* Projection of the result *)
			projection		= projection;
		}
		,
		converted_observer_structure_option

(************************************************************)
(************************************************************)
(** MODEL AND PROPERTY CONVERSION *)
(************************************************************)
(************************************************************)



(*------------------------------------------------------------*)
(** Convert the parsed model and the parsed property into an abstract model and an abstract property *)
(*------------------------------------------------------------*)
let abstract_structures_of_parsing_structures options (parsed_model : ParsingStructure.unexpanded_parsed_model) (parsed_property_option : ParsingStructure.parsed_property option) : AbstractModel.abstract_model * (AbstractProperty.abstract_property option) =

  (* Instantiate the template calls and expand syntatic variables *)
  let parsed_model = expand_model parsed_model in

  print_message Verbose_high ("\n*** Link variables to declarations.");
  (* Recompute model to link variables to their declarations, and return all variables declarations *)
  let parsed_model, variable_refs = ParsingStructureUtilities.link_variables_in_parsed_model parsed_model in

  print_message Verbose_high ("\n*** Linking variables finished.");

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug functions *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug print function for arrays *)
	let debug_print_array verbose_level =
		Array.iteri (fun i e ->
			print_message verbose_level ((string_of_int i) ^ " -> " ^ e)
		)
	in

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get names *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get the declared variable names *)
	let
	possibly_multiply_defined_clock_names,
	possibly_multiply_defined_discrete_names,
	possibly_multiply_defined_parameter_names,
	constants,
	unassigned_constants
	= get_declared_variable_names parsed_model.variable_declarations in
	(* Get the declared discrete variable names by type as a tuple of var_type * string list *)
	let possibly_multiply_defined_discrete_names_by_type = get_declared_discrete_variables_by_type parsed_model.variable_declarations in
	(* Get the declared automata names *)
	let declared_automata_names = get_declared_automata_names parsed_model.automata in
	(* Get the declared synclabs names *)
	let action_names = get_declared_synclabs_names parsed_model.automata in



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the synclabs declarations *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let action_names, removed_action_names = if options#sync_auto_detection then action_names, [] else (
		(* Keep only the synclabs which are used in ALL the automata where they are declared *)
		List.partition (synclab_used_everywhere parsed_model.automata) (*(fun synclab_name -> if synclab_used_everywhere parsed_model.automata synclab_name then
			(* If it is used everywhere: keep *)
			true
			(* If there exists an automaton where it is not used : warns and remove *)
			else (print_warning ("The synclab `" ^ synclab_name ^ "` is not used in some of the automata where it is declared: it will thus be removed."); false)
		)*) action_names
	) in




	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create a hash table that will contain previously initialized constants *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
    (* It allow to initialize a constant with an other one, previously defined *)
    let initialized_constants = (Hashtbl.create 0) in
    (* Create variable infos template *)
    let variable_infos = {
        constants = initialized_constants;
        variables = [||];
        variable_names = [];
        index_of_variables = Hashtbl.create 0;
        removed_variable_names = [];
        discrete = [];
        variable_refs = Hashtbl.create 0;
        fun_meta = Hashtbl.create 0;
        type_of_variables = fun _ -> DiscreteType.Var_type_discrete (DiscreteType.Dt_number DiscreteType.Dt_rat);
    }
    in

    (* Check that constants are not void *)
    let has_void_constants = List.exists (fun (name, _, var_type) ->
        has_void_constant_or_variable "constant" name var_type
    ) constants in

    if has_void_constants then
        raise InvalidModel;

    (* Evaluate the constants init expressions *)
    let evaluated_constants = List.map (fun (name, expr, var_type) ->

        (* Create variable infos containing only initialized constants *)
        let current_variable_infos = { variable_infos with constants = initialized_constants } in
        (* Check all constants used are defined *)
        let all_variable_defined = ParsingStructureMeta.all_variables_defined_in_parsed_boolean_expression_without_callback current_variable_infos expr in
        if not all_variable_defined then (
            print_error (
                "Expression \""
                ^ ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos expr
                ^ "\" uses undeclared variable or constant"
            );
            raise InvalidModel;
        );

        (* TYPE CHECKING *)
        let constant = name, expr, var_type in
        let typed_expr(*, expr_type *) = DiscreteExpressionConverter.convert_constant_init initialized_constants constant in

        (* Note: If we want use functions for constant initialization, we has to replace None by the function table *)
        (* It meant that function table should be initialized before constant, if possible. *)
        let value = DiscreteExpressionEvaluator.try_eval_constant_global_expression None (* function table *) typed_expr in
        (* Add evaluated constant to hash table *)
        Hashtbl.add initialized_constants name value;
        (* Return *)
        name, value(*, expr_type *)
    ) (List.rev constants) in



    (* TYPE CHECKING : CONSTANTS IN DECLARATION *)
    let constant_tuples = evaluated_constants in

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Make the array of constants *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let (constants : (Automaton.variable_name , AbstractValue.abstract_value) Hashtbl.t), constants_consistent = make_constants constant_tuples in



	if verbose_mode_greater Verbose_high then(
		(* Constants *)
		print_message Verbose_high ("\n*** Constants:");
		Hashtbl.iter (fun key value ->
			print_message Verbose_high (key ^ " = " ^ (AbstractValue.string_of_value value) ^ "")
		) constants;
	);



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the variable_declarations *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that all variable names are different (and print warnings for multiply-defined variables if same type) *)
	let all_variables_different = check_variable_names possibly_multiply_defined_clock_names possibly_multiply_defined_discrete_names possibly_multiply_defined_parameter_names constants in
	(* Check that all automata names are different *)
	let all_automata_different = check_declared_automata_names declared_automata_names in


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Errors if unassigned constants *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let check_no_unassigned_constants =
		if unassigned_constants = [] then true
		else
		(
			List.iter (fun unassigned_constant ->
				print_error("Constant `" ^ unassigned_constant ^ "` is not assigned a value in the variable declarations.");
			) unassigned_constants;
			(* Check failed *)
			false;
		)
	in

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the controllable actions *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE (ÉA, 2022/11): will fail if some controllable actions use action names defined but not used in at least one automaton: modify behavior? ***)
	let controllable_actions_checked : bool = match parsed_model.controllable_actions with
	| Parsed_controllable_actions controllable_action_names
	| Parsed_uncontrollable_actions controllable_action_names
		-> check_controllable_actions controllable_action_names action_names
	| Parsed_no_controllable_actions -> true
	in

	if not controllable_actions_checked then(
		print_error("There were errors in the definition of controllable actions.");
	);


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Exit if not well formed *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that at least one automaton is defined *)
	let at_least_one_automaton =
		if List.length declared_automata_names = 0 then (
			print_error ("At least one automaton should be declared."); false
		) else true in

	(* Stop here if model not well formed *)
 	if not (constants_consistent && all_variables_different && all_automata_different && controllable_actions_checked && at_least_one_automaton) then raise InvalidModel;
 	
 	
 	
 	
 	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add clock and automaton for the observer *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Note that the observer has not been checked yet, but it doesn't matter *)
	let observer_automaton, observer_clock_option = match parsed_property_option with
		| None -> None, None
		| Some parsed_property -> ObserverPatterns.new_elements parsed_property
	in

	(* Print some information *)
	if verbose_mode_greater Verbose_high then(
		begin
			match observer_automaton with
			| None -> ()
			| Some observer_automaton -> print_message Verbose_high ("Adding extra automaton `" ^ observer_automaton ^ "` for the observer.");
		end;
		begin
			match observer_clock_option with
			| None -> ()
			| Some observer_clock_name -> print_message Verbose_high ("Adding extra clock `" ^ observer_clock_name ^ "` for the observer.");
		end;
	);

	
	(* Print some information *)
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("Automata names : " ^ (string_of_list_of_string_with_sep ", " declared_automata_names));
	);


	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Start building variable lists *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* First remove multiply defined variable names *)
	let single_clock_names = list_only_once possibly_multiply_defined_clock_names in
	let single_discrete_names = list_only_once possibly_multiply_defined_discrete_names in
	let single_parameter_names = list_only_once possibly_multiply_defined_parameter_names in
	let single_discrete_names_by_type = list_only_once possibly_multiply_defined_discrete_names_by_type in

    let declarations_info = {
        clock_names = single_clock_names;
        parameter_names = single_parameter_names;
        discrete_names = single_discrete_names;
        variable_refs = variable_refs;
    }
    in
	(*------------------------------------------------------------*)
	(* Resolve dependencies between variables and functions *)
	(*------------------------------------------------------------*)

    ImitatorUtilities.print_message_lazy Verbose_high (lazy "\n*** Compute dependency graph.\n");

    (* Resolve dependency graph of the model *)
    let dependency_graph = ParsingStructureGraph.dependency_graph ~no_var_autoremove:options#no_variable_autoremove declarations_info parsed_model parsed_property_option in
    (* Get dependency graph as dot format *)
    let str_dependency_graph = lazy (ParsingStructureGraph.string_of_dependency_graph dependency_graph) in
    (* Print dependency graph, if verbose mode >= high *)
    ImitatorUtilities.print_message_lazy Verbose_high str_dependency_graph;
    ImitatorUtilities.print_message_lazy Verbose_high (lazy "\n*** Dependency graph computed. (copy/paste in new .dot file and use `dot -Tpng file.dot file.png` command to see it).\n");

    (* Get unused components and print warnings *)
    let unused_components = ParsingStructureGraph.unused_components_of_model dependency_graph in

    (* Iter on unused components and print warnings *)
    ComponentSet.iter (function
        | Fun_component function_name ->
(*            print_warning ("Function `" ^ function_name ^ "` is declared but never used in the model; it is therefore removed from the model.")*)
            print_warning ("Function `" ^ function_name ^ "` is declared but never used.")
        | Variable_component ((variable_name, _) as variable_ref) when VariableInfo.is_local variable_ref ->
            print_warning ("Local variable `" ^ variable_name ^ "` is declared but never used.")
(*        | Param_component (param_name, function_name) ->*)
(*            print_warning ("Formal parameter `" ^ param_name ^ "` in `" ^ function_name ^ "` is declared but never used.")*)
        | _ -> ()
    ) unused_components;


	(*------------------------------------------------------------*)
	(* Remove unused variables *)
	(*------------------------------------------------------------*)

	(* Unless a specific option is activated, we first remove all variables declared but unused *)
	let clock_names, _, parameter_names, discrete_names_by_type, removed_variable_names =
	if options#no_variable_autoremove then(
		(* Nothing to do *)
		single_clock_names, single_discrete_names, single_parameter_names, single_discrete_names_by_type, []
	)else (

		(* Gather all variables used *)
		let all_variables_used_in_model = ParsingStructureGraph.used_global_variables_of_model dependency_graph in

		(* Remove variable unused *)
		let remove_unused_variables_gen variable_type_name = List.partition (fun variable_name ->
			(* The variable is kept if… *)

            (* Either it is used somewhere *)
            let is_variable_used_somewhere = StringSet.mem variable_name all_variables_used_in_model in
            (* Or it is a clock with the special global_time name *)
            let is_special_global_time = variable_name = Constants.global_time_clock_name && List.mem variable_name single_clock_names in

			if is_variable_used_somewhere || is_special_global_time then (
			    true
            )
			else (
				(* First print a warning *)
				print_warning ("The " ^ variable_type_name ^ " `" ^ variable_name ^ "` is declared but never used in the model; it is therefore removed from the model. Use option -no-var-autoremove to keep it.");
				(* Filter out *)
				false
			)
		) in
		(* Create all three lists *)
		let single_clock_names, removed_clock_names = remove_unused_variables_gen "clock" single_clock_names in
		let single_discrete_names, removed_discrete_names = remove_unused_variables_gen "discrete" single_discrete_names in
		let single_parameter_names, removed_parameter_names = remove_unused_variables_gen "parameter" single_parameter_names in
		(* Remove unused variable names by type in function of single_discrete_names content after auto remove *)
		let single_discrete_names_by_type = List.filter (fun (_, variable_name) -> List.mem variable_name single_discrete_names) single_discrete_names_by_type in
		(* Return and append removed variable names *)
		let removed_variable_names = List.rev_append removed_clock_names (List.rev_append removed_discrete_names removed_parameter_names) in
		single_clock_names, single_discrete_names, single_parameter_names, single_discrete_names_by_type, removed_variable_names
	)
	in

    (* Check that variables are not void *)
    let has_void_variables = List.exists (fun (var_type, name) ->
        has_void_constant_or_variable "variable" name var_type
    ) discrete_names_by_type in

    if has_void_variables then
        raise InvalidModel;

    (* Hack!!! Sort rational at first, for PPL *)
    let discrete_names_by_type, discrete_names =
        let sorted_discrete_names_by_type = List.stable_sort (fun (var_type_a, _) (var_type_b, _) ->
            match var_type_a, var_type_b with
            | Var_type_discrete (Dt_number Dt_rat), Var_type_discrete (Dt_number Dt_rat) -> 0
            | Var_type_discrete (Dt_number Dt_rat), _ -> -1
            | _, Var_type_discrete (Dt_number Dt_rat) -> 1
            | _, _ -> 0
        ) discrete_names_by_type
        in
        sorted_discrete_names_by_type, List.map OCamlUtilities.second_of_tuple sorted_discrete_names_by_type
    in

    (* Group variable names by types *)
	let discrete_names_by_type_group = OCamlUtilities.group_by_and_map (fun (var_type, _) -> var_type) (fun (_, var_name) -> var_name) discrete_names_by_type in

	(*------------------------------------------------------------*)
	(* Special clocks *)
	(*------------------------------------------------------------*)

	(* First handle the observer clock if any *)
	let observer_clock_list = match observer_clock_option with
		| None -> []
		| Some observer_clock_name -> [observer_clock_name]
	in
	
	(* Second handle the special_reset_clock *)
	let with_special_reset_clock = match parsed_property_option with
	| Some parsed_property ->
		begin
		match parsed_property.property with
		| Parsed_NZ_Cycle
			->
			print_message Verbose_high ("\nDefining special reset clock…");
			true
			
		| _ -> false
		end
	
	| None -> false
	in
	
	let special_reset_clock_list = if with_special_reset_clock then [Constants.special_reset_clock_name] else [] in
	

	(*------------------------------------------------------------*)
	(* Create lists *)
	(*------------------------------------------------------------*)
	
	let clock_names = list_append (list_append clock_names observer_clock_list) special_reset_clock_list in

	(* Make only one list for all variables *)
    (* Keep order (parameters, clocks, discretes) *)
	let variable_names = discrete_names |> list_append clock_names |> list_append parameter_names in

	(* Update automata names with the observer automaton *)
	let declared_automata_names = match observer_automaton with
		| None -> declared_automata_names
		| Some automaton_name -> list_append declared_automata_names [automaton_name]
	in

	(* Numbers *)
	let nb_automata		    = List.length declared_automata_names in
	let nb_actions		    = List.length action_names in
	let nb_clocks		    = List.length clock_names in
	let nb_discrete		    = List.length discrete_names in
	let nb_rationals        = discrete_names_by_type |> List.filter (fun (var_type, _) -> match var_type with Var_type_discrete (Dt_number Dt_rat) -> true | _ -> false) |> List.length in
	let nb_parameters	    = List.length parameter_names in
	let nb_variables	    = List.length variable_names in
	let nb_ppl_variables = nb_clocks + nb_parameters + nb_rationals in

	(* Compute the index for the observer automaton *)
	let observer_automaton_index_option = match observer_automaton with
		| None -> None
		| Some _ ->
			(* Print some information *)
			print_message Verbose_high ("\nObserver automaton index is: " ^ (string_of_int (nb_automata - 1)));
			
			Some (nb_automata - 1)
	in



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the LinearConstraint dimensions *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Print some information *)
	print_message Verbose_high ("\nSetting dimensions…");
	LinearConstraint.set_dimensions nb_parameters nb_clocks nb_rationals;
	
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the parameter dimensions *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*** NOTE: must be done one and exactly one time ***)

	(* Set dimensions for hyper rectangles *)
	HyperRectangle.set_dimensions nb_parameters;

	(* Set dimensions for parameter valuations *)
	PVal.set_dimensions nb_parameters;




	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the arrays of automata, variables and actions *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* The list of automata *)
	let automata = list_of_interval 0 (nb_automata - 1) in

	(* The array of automata names ; index -> automaton name *)
	let array_of_automata_names = Array.of_list declared_automata_names in
	(* A (constant) hash table `automaton name -> index` *)
	let index_of_automata : (Automaton.automaton_name, Automaton.automaton_index) Hashtbl.t = Hashtbl.create nb_automata in
	for i = 0 to nb_automata - 1 do
		Hashtbl.add index_of_automata array_of_automata_names.(i) i;
	done;

	(* Functional version *)
	let (automata_names : automaton_index -> automaton_name) = fun automaton_index ->
		(* Add a safety mechanism *)
		try(
			array_of_automata_names.(automaton_index)
		) with Invalid_argument msg -> raise (InternalError ("Automaton name of index `" ^ (string_of_int automaton_index) ^ "` not found in `automata_names` function. Additional details: `" ^ msg ^ "`"))
	in

	(* The array of actions ; index -> action name *)
	let actions : action_name array = Array.of_list action_names in
	(* A (constant) hash table `action name -> index` *)
	let index_of_actions = Hashtbl.create nb_actions in
	for i = 0 to nb_actions - 1 do
		Hashtbl.add index_of_actions actions.(i) i;
	done;

	(* The array of variables names ; index -> variable name *)
	let variables = Array.of_list variable_names in
	(* A (constant) hash table `variable name -> index` *)
	let index_of_variables : (Automaton.variable_name, Automaton.variable_index) Hashtbl.t = Hashtbl.create nb_variables in
	for i = 0 to nb_variables - 1 do
		Hashtbl.add index_of_variables variables.(i) i;
	done;

	let first_parameter_index = 0 in
	let first_clock_index    = first_parameter_index + nb_parameters in
	let first_discrete_index  = first_clock_index + nb_clocks in

	(* An array `variable index -> AbstractModel.var_type` *)
	let type_of_variables = Array.make nb_variables DiscreteType.Var_type_parameter in
	for i = first_clock_index to first_discrete_index - 1 do
		type_of_variables.(i) <- DiscreteType.Var_type_clock;
	done;

    (* Print some information *)
    print_message Verbose_high ("\nSetting discretes variables types…\n");

	for i = first_discrete_index to nb_variables - 1 do
	    (* Get specific var_type of discrete variable *)
	    (* Remove offset, because array of var_type * variable_name for discrete start from 0 *)
        let var_type, v = List.nth discrete_names_by_type (i - first_discrete_index) in
        (* Convert var_type from ParsingStructure to AbstractModel *)
		type_of_variables.(i) <- var_type;
        (* Print type infos *)
		print_message Verbose_high ("  set type variable " ^ v ^ " : " ^ (DiscreteType.string_of_var_type type_of_variables.(i)))
	done;

	(* Functional representation *)
	let type_of_variables = fun variable_index -> type_of_variables.(variable_index) in

	(* Create the lists of different variables *)
	let (parameters : parameter_index list)	= list_of_interval first_parameter_index (first_clock_index - 1) in
	let (clocks : clock_index list)			= list_of_interval first_clock_index (first_discrete_index - 1) in
	let (discrete : discrete_index list)	= list_of_interval first_discrete_index (nb_variables - 1) in
    let discrete_rationals                  = list_of_interval first_discrete_index (nb_ppl_variables - 1) in

	(* Create the type check functions *)
	let is_clock = (fun variable_index -> try (type_of_variables variable_index = DiscreteType.Var_type_clock) with Invalid_argument _ ->  false) in
	let is_discrete = (fun variable_index -> try (DiscreteType.is_discrete_type (type_of_variables variable_index)) with Invalid_argument _ ->  false) in


	(* Detect the clock with a special global time name, if any *)
	let global_time_clock =
		if List.mem Constants.global_time_clock_name clock_names then Some (Hashtbl.find index_of_variables Constants.global_time_clock_name)
		else None
	in


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug prints *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Automata *)
	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("\n*** Array of automata names:");
		debug_print_array Verbose_high array_of_automata_names;

		(* Actions *)
		print_message Verbose_high ("\n*** Array of declared synchronization action names:");
		debug_print_array Verbose_high actions;

		(* Variables *)
		print_message Verbose_total ("\n*** Variable names:");
		Array.iteri (fun i e ->
			print_message Verbose_total ((string_of_int i) ^ " -> " ^ e ^ " : " ^ (ModelPrinter.string_of_var_type (type_of_variables i)))
		) variables;
	);


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get all the locations *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that all the location names of an automaton are different *)
	if not (all_locations_different parsed_model.automata) then raise InvalidModel;

	(* Get all the locations for each automaton: automaton_index -> location_index -> location_name *)
	let (array_of_location_names : location_name array array) = make_locations_per_automaton index_of_automata parsed_model.automata nb_automata in
	(* Add the observer locations *)
	begin
	match observer_automaton_index_option with
		| None -> ()
			(*** WARNING: we assume here that observer automaton is the last one ! ***)
		| Some automaton_index ->
			(* Get the property *)
			let parsed_property = match parsed_property_option with
				| None -> raise (InternalError "A property must be defined at this point since we have an observer automaton")
				| Some parsed_property -> parsed_property
			in
			print_message Verbose_high ("Adding the observer locations.");
			array_of_location_names.(automaton_index) <- ObserverPatterns.get_locations parsed_property
	end;

	(* A (constant) array of hash tables `automaton_index -> location_name -> location_index` *)
	let index_of_locations = Array.make nb_automata (Hashtbl.create 0) in
	for automaton_index = 0 to nb_automata - 1 do
		let nb_locations = Array.length (array_of_location_names.(automaton_index)) in
		let location_index_table = Hashtbl.create nb_locations in
		for location_index = 0 to nb_locations - 1 do
			Hashtbl.add location_index_table array_of_location_names.(automaton_index).(location_index) location_index;
		done;
		index_of_locations.(automaton_index) <- location_index_table;
	done;
	(* Create the access function returning a list of locations*)
	let array_of_locations_per_automaton = Array.make nb_automata [] in
	for automaton_index = 0 to nb_automata - 1 do
		array_of_locations_per_automaton.(automaton_index) <-
		Array.to_list (Array.mapi (fun location_index _ -> location_index) array_of_location_names.(automaton_index));
	done;
	let (locations_per_automaton : automaton_index -> location_index list) = fun automaton_index -> array_of_locations_per_automaton.(automaton_index) in
	
	(* Create the access function returning a location name *)
	let location_names = fun automaton_index location_index ->
		(* Add a safety mechanism *)
		try(
			array_of_location_names.(automaton_index).(location_index)
		) with Invalid_argument msg -> raise (InternalError ("Location name of index `" ^ (string_of_int location_index) ^ "` in automaton of index `" ^ (string_of_int automaton_index) ^ "` not found in `location_names` function. Additional details: `" ^ msg ^ "`"))
	in


	(* Debug print *)
	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("\n*** Locations per automaton:");
		List.iter (fun automaton_index ->
			print_message Verbose_high ((automata_names automaton_index) ^ " : ");
			List.iter (fun location_index ->
				print_message Verbose_high ("    " ^ (string_of_int location_index) ^ " -> " ^ (location_names automaton_index location_index) ^ "");
			)
			(locations_per_automaton automaton_index);
		) automata;
	);

    (* Gather all functions metadata in a table *)

    (* Get user functions metadata from parsed functions *)
    (* Get only used user functions definition *)
    let used_function_names = ParsingStructureGraph.used_functions_of_model dependency_graph in
    let used_function_definitions =
        (*
        if options#no_variable_autoremove then
            parsed_model.fun_definitions
        else
        *)
            List.filter (fun (fun_def : parsed_fun_definition) -> StringSet.mem fun_def.name used_function_names) parsed_model.fun_definitions
    in

(*    let used_function_definitions = parsed_model.fun_definitions in*)

    let used_function_definitions =
        if options#no_variable_autoremove then
            (* No variable auto remove, keep all instructions in function *)
            used_function_definitions
        else
            (* If variable auto remove, remove unused clock assignments *)
            List.map (ParsingStructureGraph.remove_unused_assignments_in_fun_def declarations_info dependency_graph) used_function_definitions
    in


    (* Check for function cycles *)

    let cycle_infos = ParsingStructureGraph.model_cycle_infos dependency_graph in
    let model_has_cycle = List.exists first_of_tuple cycle_infos in
    let cycle_paths = List.filter_map (fun (has_cycle, path) -> if has_cycle then Some path else None) cycle_infos in

    List.iter (fun cycle_path ->
        print_error (
            "Cycle found: `"
            ^ cycle_path
            ^ "`. Cycle dependencies are forbidden."
        )
    ) cycle_paths;

    if model_has_cycle then (
        (* Prepare json for res results *)
        let json_cycle_paths = Json_array (List.map (fun cycle_path -> Json_string cycle_path) cycle_paths) in
        (* Add cycle section *)
        Logger.add_custom_detail_property "cycles" json_cycle_paths;
        (* Raise error *)
        raise InvalidModel
    );


    (* Create table of user function definitions *)
    let user_function_definitions_table = List.map (fun (fun_def : parsed_fun_definition) -> fun_def.name, fun_def) used_function_definitions |> OCamlUtilities.hashtbl_of_tuples in

    (* Get metadata of these functions *)
    let user_functions_metadata = List.map (Functions.metadata_of_parsed_function_definition Functions.builtin_functions_metadata_table user_function_definitions_table) used_function_definitions in
    (* Concat builtin & user functions *)
    let all_functions_metadata = user_functions_metadata @ Functions.builtin_functions_metadata in
    (* Create function table that associate function name to function metadata *)
    let functions_metadata_table = (List.map (fun (fun_def : ParsingStructure.function_metadata) -> fun_def.name, fun_def) all_functions_metadata) |> OCamlUtilities.hashtbl_of_tuples in

    (* Print some info on side effects resolution *)
    let str_fun_side_effects = lazy (
        let str_fun_side_effects_info_list = List.map (fun (fm : function_metadata) ->
            "  `" ^ fm.name ^ "`: " ^ string_of_bool fm.side_effect
        ) all_functions_metadata
        in
        "\n*** Functions side effects:\n\n"
        ^ OCamlUtilities.string_of_list_of_string_with_sep "\n" str_fun_side_effects_info_list
    ) in

    print_message_lazy Verbose_high str_fun_side_effects;

    (* Function metas to json *)
    let json_function_metas = List.map ParsingStructureUtilities.json_of_function_metadata all_functions_metadata in
    (* Create json array *)
    let json_array_function_metas = Json_array json_function_metas in
    (* Add new property to details *)
    Logger.add_custom_detail_property "function_metas" json_array_function_metas;

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create useful parsing structure, used in subsequent functions *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	let variable_infos = {
        constants					= constants;
        discrete					= discrete;
        index_of_variables			= index_of_variables;
        type_of_variables			= type_of_variables;
        variables					= variables;
        variable_names				= variable_names;
        removed_variable_names		= removed_variable_names;
        variable_refs             = variable_refs;
        fun_meta                   = functions_metadata_table;
    }
    in

	let useful_parsing_model_information : ParsingStructure.useful_parsing_model_information = {
		actions						= actions;
		array_of_location_names		= array_of_location_names;
		automata_names				= automata_names;
		automata					= automata;
		index_of_actions			= index_of_actions;
		index_of_automata			= index_of_automata;
		index_of_locations			= index_of_locations;
		nb_clocks					= nb_clocks;
		nb_parameters				= nb_parameters;
		parameter_names				= parameter_names;
		removed_action_names		= removed_action_names;
		variable_infos              = variable_infos
	}
	in

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the user function definitions *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	print_message Verbose_high ("*** Checking user functions definitions…");

    (* Try to convert (only used) function definition from parsing structure to abstract model into sequence of tuple (name * fun_def) *)
    let user_functions_list = List.map (fun (parsed_fun_def : parsed_fun_definition) ->
        (* Convert fun def from parsing structure to abstract model *)
        let fun_def = DiscreteExpressionConverter.convert_fun_definition variable_infos parsed_fun_def in
        fun_def.name, fun_def
    ) used_function_definitions
    in
    (* Get builtin functions implementations as associative list *)
    let builtin_functions_list = List.map (fun (fun_def : fun_definition) -> fun_def.name, fun_def) Functions.builtin_function_bodies in
    (* Concat all functions *)
    let functions_list = user_functions_list @ builtin_functions_list in
    (* Convert to table *)
    let functions_table = OCamlUtilities.hashtbl_of_tuples functions_list in

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the automata *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("*** Checking automata…");
	let well_formed_automata = check_automata useful_parsing_model_information parsed_model.automata in


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* exit if not well formed *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: might be a problem in the init checking if the check_automata test fails, hence test first ***)
	if not (check_no_unassigned_constants && well_formed_automata)
		then raise InvalidModel;


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the init_definition *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("*** Checking init definition…");

	let init_definition, init_discrete_pairs, well_formed_init = check_init dependency_graph functions_table useful_parsing_model_information parsed_model.init_definition observer_automaton_index_option options#no_variable_autoremove in

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the constants inits *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
    print_message Verbose_high ("*** Checking constant inits…");

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check projection definition *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let well_formed_projection = match parsed_property_option with
		| None -> true
		| Some parsed_property -> check_projection_definition variable_infos parsed_property.projection
	in



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* exit if not well formed *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	if not (well_formed_projection && well_formed_init)
		then raise InvalidModel;

	print_message Verbose_medium ("Model syntax successfully checked.");
	

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the property *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	if not (check_property_option useful_parsing_model_information parsed_property_option)
		then raise InvalidProperty;

	print_message Verbose_medium ("Property syntax successfully checked.");
	

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the automata without the observer, and with the transitions in a non-finalized form *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("*** Building automata…");
	(* Get all the possible actions for every location of every automaton *)
	let (actions : action_index list), array_of_action_names, action_types, actions_per_automaton, actions_per_location, location_acceptance, location_urgency, costs, invariants, stopwatches_array, has_non_1rate_clocks, flow_array, transitions, observer_nosync_index_option = make_automata useful_parsing_model_information parsed_model.automata (observer_automaton_index_option <> None) in
	
	let nb_actions = List.length actions in
	
	(* Print some information *)
	print_message Verbose_high ("The model contains " ^ (string_of_int nb_actions) ^ " action" ^ (s_of_int nb_actions) ^ ".");


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create the abstract property from the parsed property *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	(* We may need to create additional structures for the observer, if any *)
	
	let abstract_property_option, converted_observer_structure_option = convert_property_option useful_parsing_model_information nb_actions observer_automaton_index_option observer_nosync_index_option parsed_property_option in
	
	(* Convert some variables to catch up with older code below *)
	let observer_structure_option, nb_transitions_for_observer, initial_observer_constraint_option = match converted_observer_structure_option with
		| None -> None, 0, None
		| Some converted_observer_structure ->
			Some converted_observer_structure.observer_structure,
			converted_observer_structure.nb_transitions_for_observer,
			converted_observer_structure.initial_observer_constraint_option
	in
	
	



	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Convert the transitions to their final form *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	print_message Verbose_high ("*** Building transitions…");
	(* Count the number of transitions *)
	let nb_transitions_without_observer =
		(* Iterate on automata *)
		Array.fold_left (fun nb_transitions_for_automata transitions_for_this_automaton ->
			(* Iterate on locations *)
			Array.fold_left (fun nb_transitions_for_locations transitions_for_this_location ->
				nb_transitions_for_locations + (List.length transitions_for_this_location)
			) nb_transitions_for_automata transitions_for_this_automaton
		) 0 transitions
	in
	let nb_transitions = nb_transitions_without_observer + nb_transitions_for_observer in
	
	(* Print some information *)
	print_message Verbose_total ("" ^ (string_of_int nb_transitions_without_observer) ^ " transition" ^ (s_of_int nb_transitions_without_observer) ^ " in the model, and " ^ (string_of_int nb_transitions_for_observer) ^ " additional transition" ^ (s_of_int nb_transitions_for_observer) ^ " for the observer");

	(* Convert transitions *)
	(*** TODO: integrate inside `make_automata` (?) ***)
	let transitions, transitions_description, automaton_of_transition = convert_transitions options nb_transitions nb_actions declarations_info variable_infos dependency_graph user_function_definitions_table transitions in


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add the observer structure to the automata *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	begin
	match observer_automaton_index_option with
	| None -> ()
	| Some observer_id ->
		(* Get the info from the observer pattern *)
		begin
		match observer_structure_option with
		| None -> raise (InternalError ("No observer structure saved although it should have been set at that point"))
		
		(*** TODO: create a structure !!! ***)
		
		| Some (observer_actions, observer_actions_per_location, observer_location_urgency, observer_invariants, observer_transitions) ->
			print_message Verbose_high ("*** Adding observer data to automata…");
			(* Retrieve the number of locations of the observer *)
			let nb_locations = Array.length observer_actions_per_location in
			(* Update actions per automaton *)
			actions_per_automaton.(observer_id)	<- observer_actions;
			(* Update actions per location *)
			actions_per_location.(observer_id)	<- observer_actions_per_location;
			(* Update urgency *)
			location_urgency.(observer_id)		<- observer_location_urgency;
			(* Create the array of accepting locations for this automaton (for now: all non accepting) *)
			location_acceptance.(observer_id)	<- Array.make nb_locations Location_nonaccepting;
			(* Update invariants *)
			invariants.(observer_id)			<- observer_invariants;
			(* Update costs (no costs in observers) *)
			costs.(observer_id)					<- Array.make nb_locations None;
			(* Update stopwatches (no stopwatches in observers) *)
			stopwatches_array.(observer_id)		<- Array.make nb_locations [];
			(* Update stopwatches (no stopwatches in observers) *)
			flow_array.(observer_id)			<- Array.make nb_locations [];
			
			(* Update transitions *)
			
			(* First convert the transitions to transition_index, and update the transitions_description *)

			(* Maintain an index for the next transition *)
			let transition_index = ref nb_transitions_without_observer in
			let observer_transitions =
			(* Iterate on locations *)
			Array.map (fun actions_for_this_location ->
				(* Iterate on actions *)
				Array.map (fun transitions_for_this_action ->
					(* Iterate on transitions for this action *)
					List.map (fun transition ->
						let current_transition_index = !transition_index in
						(* Add the transition to the description *)
						begin
						try(
							(* Update transition description *)
							transitions_description.(current_transition_index) <- transition;
							(* Update automaton of transition *)
							automaton_of_transition.(current_transition_index) <- observer_id;
						) with
							| Invalid_argument e -> raise (InternalError ("Invalid argument `" ^ e ^ "` when updating observer transitions (current index: " ^ (string_of_int current_transition_index) ^ " max size: " ^ (string_of_int (Array.length transitions_description)) ^ ")"))
						;
						end;
						
						(* Increment the index *)
						transition_index := !transition_index + 1;

						(* Update the transition array *)
						current_transition_index
					) transitions_for_this_action;
				) actions_for_this_location;
			) observer_transitions
			in
			
			(* Then update transitions *)
			transitions.(observer_id) <- observer_transitions;
			
		end;
	end;
	
	(*** TODO : perform init for observer (location) ***)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Convert the controllable actions *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	print_message Verbose_high ("*** Converting the controllable actions…");

	let controllable_actions_indices : action_index list = match parsed_model.controllable_actions with
		| Parsed_controllable_actions controllable_action_names ->
			print_message Verbose_high ("      Controllable actions detected");
			(* Convert to a list of controllable actions *)
			make_controllable_actions controllable_action_names index_of_actions

		| Parsed_uncontrollable_actions controllable_action_names ->
			print_message Verbose_high ("      Uncontrollable actions detected");
			(* Convert to a list of UNcontrollable actions *)
			let uncontrollable_actions_indices = make_controllable_actions controllable_action_names index_of_actions in
			(* Take complement *)
			list_diff actions uncontrollable_actions_indices

		| Parsed_no_controllable_actions ->
			print_message Verbose_high ("      No controllable actions detected");
			[]
	 in

	(* Is an action controllable? *)
	let is_controllable_action action_index = List.mem action_index controllable_actions_indices in





	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Convert to functional view *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	print_message Verbose_high ("*** Converting to functional view…");

	let array_of_variable_names = Array.make nb_variables "" in
	(* Add normal names *)
	for variable_index = 0 to nb_variables - 1 do
		array_of_variable_names.(variable_index) <- variables.(variable_index);
	done;

	(* Create the functional representation *)
	let variable_names = fun variable_index ->
		(* Add a safety mechanism *)
		try(
			array_of_variable_names.(variable_index) 
		) with Invalid_argument msg -> raise (InternalError ("Could not find name of variable `" ^ (string_of_int variable_index) ^ "` in function `variable_names`. Additional details: `" ^ msg ^ "`"))
	in

	(* Create the functional representation for action types *)
	let action_types = fun action_index ->
		(* Add a safety mechanism *)
		try(
			action_types.(action_index)
		) with Invalid_argument msg -> raise (InternalError ("Could not find type of action `" ^ (string_of_int action_index) ^ "` in function `action_types`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Create the functional representation for the actions of every automaton *)
	let actions_per_automaton = fun automaton_index ->
		(* Add a safety mechanism *)
		try(
			actions_per_automaton.(automaton_index)
		) with Invalid_argument msg -> raise (InternalError ("Could not find list of actions for automaton of index `" ^ (string_of_int automaton_index) ^ "` in function `actions_per_automaton`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Create the functional representation for the actions of every location of every automaton *)
	let actions_per_location = fun automaton_index location_index ->
		(* Add a safety mechanism *)
		try(
			actions_per_location.(automaton_index).(location_index)
		) with Invalid_argument msg -> raise (InternalError ("Could not find list of actions for automaton of index `" ^ (string_of_int automaton_index) ^ "` and location of index `" ^ (string_of_int location_index) ^ "` in function `actions_per_location`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Invariants *)
	let invariants = fun automaton_index location_index ->
		(* Add a safety mechanism *)
		try(
			invariants.(automaton_index).(location_index)
		) with Invalid_argument msg -> raise (InternalError ("Could not find invariant for automaton of index `" ^ (string_of_int automaton_index) ^ "` and location of index `" ^ (string_of_int location_index) ^ "` in function `invariants`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Accepting locations *)
	let is_accepting = fun automaton_index location_index ->
		(* Add a safety mechanism *)
		try(
			location_acceptance.(automaton_index).(location_index) = Location_accepting
		) with Invalid_argument msg -> raise (InternalError ("Acceptance of location of index `" ^ (string_of_int location_index) ^ "` in automaton of index `" ^ (string_of_int automaton_index) ^ "` not found in function `is_accepting`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Urgency *)
	let is_urgent = fun automaton_index location_index ->
		(* Add a safety mechanism *)
		try(
			location_urgency.(automaton_index).(location_index) = Location_urgent
		) with Invalid_argument msg -> raise (InternalError ("Urgency of location of index `" ^ (string_of_int location_index) ^ "` in automaton of index `" ^ (string_of_int automaton_index) ^ "` not found in function `is_urgent`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Costs *)
	let costs = fun automaton_index location_index ->
		(* Add a safety mechanism *)
		try(
			costs.(automaton_index).(location_index)
		) with Invalid_argument msg -> raise (InternalError ("Cost of location of index `" ^ (string_of_int location_index) ^ "` in automaton of index `" ^ (string_of_int automaton_index) ^ "` not found in function `costs`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Stopwatches *)
	let stopwatches = fun automaton_index location_index ->
		(* Add a safety mechanism *)
		try(
			stopwatches_array.(automaton_index).(location_index)
		) with Invalid_argument msg -> raise (InternalError ("Clocks stopped at location of index `" ^ (string_of_int location_index) ^ "` in automaton of index `" ^ (string_of_int automaton_index) ^ "` not found in function `stopwatches`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Flow *)
	let flow = fun automaton_index location_index ->
		(* Add a safety mechanism *)
		try(
			flow_array.(automaton_index).(location_index)
		) with Invalid_argument msg -> raise (InternalError ("List of flows at location of index `" ^ (string_of_int location_index) ^ "` in automaton of index `" ^ (string_of_int automaton_index) ^ "` not found in function `flow`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Transitions *)
	let transitions = fun automaton_index location_index action_index ->
		(* Add a safety mechanism *)
		try(
			transitions.(automaton_index).(location_index).(action_index)
		) with Invalid_argument msg -> raise (InternalError ("Transitions of location of index `" ^ (string_of_int location_index) ^ "` in automaton of index `" ^ (string_of_int automaton_index) ^ "` via action of index `" ^ (string_of_int action_index) ^ "` not found in function `transitions`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Transition description *)
	let transitions_description = fun transition_index ->
		(* Add a safety mechanism *)
		try(
			transitions_description.(transition_index)
		) with Invalid_argument msg -> raise (InternalError ("Description of transition of index `" ^ (string_of_int transition_index) ^ "` not found in function `transitions_description`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Automaton of transition *)
	let automaton_of_transition = fun transition_index ->
		(* Add a safety mechanism *)
		try(
			automaton_of_transition.(transition_index)
		) with Invalid_argument msg -> raise (InternalError ("Automaton of transition of index `" ^ (string_of_int transition_index) ^ "` not found in function `automaton_of_transition`. Additional details: `" ^ msg ^ "`"))
	in
	
	(* Actions *)
	let action_names = fun action_index ->
		try (array_of_action_names.(action_index))
		with _ -> raise (InternalError ("Action index " ^ (string_of_int action_index) ^ " does not exist in the model."))
	in
	
	let nb_locations = List.fold_left (fun current_nb automaton -> current_nb + (List.length (locations_per_automaton automaton))) 0 automata in


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the number of discrete variables *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let min_discrete_index = first_discrete_index in
	let max_discrete_index = nb_variables - 1 in
	DiscreteState.initialize nb_automata min_discrete_index max_discrete_index;


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the automata per action *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* List of automata for every action *)
	print_message Verbose_high ("*** Building automata per action…");
	let automata_per_action = make_automata_per_action actions_per_automaton nb_automata nb_actions in

(*	(* Convert the costs *)
	print_message Verbose_total ("*** Building costs (if any)…");
	let costs = convert_costs index_of_variables constants costs in*)

(*	(* Convert the invariants *)
	print_message Verbose_total ("*** Building invariants…");
	let invariants = convert_invariants index_of_variables constants invariants in*)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Handling the special reset clock *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*** NOTE: if it is defined, then it is the last clock in the list of clocks ***)
	let special_reset_clock : clock_index option = if with_special_reset_clock then
		Some (List.nth clocks (nb_clocks - 1))
	else None
	in
	(* The list of clock indexes except the reset clock (used, e.g., to print the model *)
	let clocks_without_special_reset_clock = match special_reset_clock with
		| None -> clocks
		| Some clock_index -> list_remove_first_occurence clock_index clocks
	in

	(* Print metrics if verbose low or in any case in mode translation *)
	if verbose_mode_greater Verbose_low || (match options#imitator_mode with AbstractAlgorithm.Translation _ -> true | _ -> false) then(
		print_message Verbose_standard (
				let nb_declared_actions = List.length (List.filter (fun a -> action_types a = Action_type_sync) actions) in
			(string_of_int nb_automata) ^ " automat" ^ (if nb_automata > 1 then "a" else "on")
			^ ", "
			^ (string_of_int nb_locations) ^ " location" ^ (s_of_int nb_locations) ^ ", "
			^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ ", "
			^ (string_of_int nb_declared_actions) ^ " declared synchronization action" ^ (s_of_int nb_declared_actions) ^ ", "
			^ (string_of_int nb_actions) ^ " action" ^ (s_of_int nb_actions) ^ " (synchronized or not), "
			^ (string_of_int nb_clocks) ^ " clock variable" ^ (s_of_int nb_clocks) ^ ", "
			^ (string_of_int nb_rationals) ^ " rational variable" ^ (s_of_int nb_rationals) ^ ", "
			^ (string_of_int nb_parameters) ^ " parameter" ^ (s_of_int nb_parameters) ^ ", "
			^ (string_of_int nb_discrete) ^ " discrete variable" ^ (s_of_int nb_discrete) ^ ", "
			^ (string_of_int nb_variables) ^ " variable" ^ (s_of_int nb_variables) ^ ", "
			^ (string_of_int nb_ppl_variables) ^ " ppl variable" ^ (s_of_int nb_ppl_variables) ^ ", "
			^ (string_of_int (Hashtbl.length constants)) ^ " constant" ^ (s_of_int (Hashtbl.length constants)) ^ "."
		);
	);


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect the strongly deterministic nature of the PTA *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: strongly deterministic = for each PTA, for each location, for each VISIBLE (non-silent) action action, at most one outgoing transition with this action action ***)

	(* Print some information *)
	print_message Verbose_high ("*** Detecting the strongly-deterministic nature of the model…");

	(* By default: yes, it is strongly deterministic *)
	let strongly_deterministic = ref true in
	
	(* And now check for any counterexample *)
	(* For all PTA *)
	begin
	try(
	List.iter (fun automaton_index ->
		let locations_for_this_automaton = locations_per_automaton automaton_index in
		(* For all locations *)
		List.iter (fun location_index ->

			let actions_for_this_location = actions_per_location automaton_index location_index in
			(* For all actions *)
			List.iter (fun action_index ->
				let transitions_for_this_location = transitions automaton_index location_index action_index in
				(* If more than one transition with this action: not strongly deterministic *)
				(*** NOTE: we don't care about silent actions as, by construction, all silent actions are different and therefore their cardinality can only be 1 ***)
				if List.length transitions_for_this_location > 1 then(
					(* Write a message *)
					if verbose_mode_greater Verbose_high then(
						print_message Verbose_high ("This network of PTAs is not strongly deterministic: in automaton `" ^ (automata_names automaton_index) ^ "`, in location `" ^ (location_names automaton_index location_index) ^ "`, there are " ^ (string_of_int (List.length transitions_for_this_location)) ^ " outgoing transitions labeled with action `" ^ (action_names action_index) ^ "`.");
					);

					(* Update flag *)
					strongly_deterministic := false;
					
					(* Raise exception to not test further *)
					raise Not_strongly_deterministic;
				);
			) actions_for_this_location;
		) locations_for_this_automaton;
	) automata;
	) with
		(* Just to accelerate exit from the loops *)
		Not_strongly_deterministic -> ();
	;
	end;
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect the presence of silent transitions *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Print some information *)
	print_message Verbose_high ("*** Detecting the presence of silent transitions in the model…");

	(*** NOTE: instead of enumerating all transitions, we just check for the presence of a silent action; if an action was created but unused, it should have been deleted ***)
	let has_silent_actions = List.exists (fun action_index ->
		action_types action_index = Action_type_nosync
	) actions in
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect the L/U nature of the PTA *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*** NOTE/HACK: duplicate function in StateSpace ***)
	let continuous_part_of_guard (*: LinearConstraint.pxd_linear_constraint*) = function
		| True_guard -> LinearConstraint.pxd_true_constraint()
		| False_guard -> LinearConstraint.pxd_false_constraint()
		| Discrete_guard _ -> LinearConstraint.pxd_true_constraint()
		| Continuous_guard continuous_guard -> continuous_guard
		| Discrete_continuous_guard discrete_continuous_guard -> discrete_continuous_guard.continuous_guard
	in

	(* 1) Get ALL constraints of guards and invariants *)
	
	(* Print some information *)
	print_message Verbose_total ("*** Retrieving all constraints to detect the L/U nature of the model…");
	
	(*** BADPROG ***)
	let all_constraints = ref [] in
	(* For all PTA *)
	List.iter (fun automaton_index ->
		let locations_for_this_automaton = locations_per_automaton automaton_index in
		(* For all locations *)
		List.iter (fun location_index ->
			let invariant = invariants automaton_index location_index in
			(* Get only continuous part of invariant *)
            let continuous_part_of_invariant = (continuous_part_of_guard invariant) in
			(* Add invariant *)
			all_constraints := continuous_part_of_invariant :: !all_constraints;

			let actions_for_this_location = actions_per_location automaton_index location_index in
			(* For all actions *)
			List.iter (fun action_index ->
				let transitions_for_this_location = List.map transitions_description (transitions automaton_index location_index action_index) in
				(* For all transitions *)
				List.iter (fun transition ->

					(* Add guard *)
					(*** NOTE: quite inefficient as we create a lot of pxd_true_constraint() although we just want to know whether they are L/U or not (but OK because prior to model analysis) ***)
					all_constraints := (continuous_part_of_guard transition.guard) :: !all_constraints;
				) transitions_for_this_location;
			) actions_for_this_location;
		) locations_for_this_automaton;
	) automata;

	(* 2) Try to partition parameters as L- and U- *)

	(* Print some information *)
	print_message Verbose_high ("*** Detecting the L/U nature of the model…");
	
	let lu_status =
	try(
		let l_parameters, u_parameters = LinearConstraint.partition_lu parameters !all_constraints in
		(*** TODO: check that all parameters are indeed partitioned (if some parameters appear nowhere, that might be a problem) ***)
		match l_parameters, u_parameters with
		| [] , [] -> PTA_notLU
		| _ , [] -> PTA_L
		| [] , _ -> PTA_U
		| l_parameters, u_parameters -> PTA_LU (l_parameters, u_parameters)
	) with Not_LU -> PTA_notLU
	in

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check existence of invariants *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let has_invariants = 
	(* For all PTA *)
	List.exists (fun automaton_index ->
		let locations_for_this_automaton = locations_per_automaton automaton_index in
		(* For all locations *)
		List.exists (fun location_index ->
			let invariant = invariants automaton_index location_index in
			match invariant with
			    | True_guard -> false
                | False_guard -> false
			    | Continuous_guard continuous_invariant ->
                    (* Costly test! But inherent to the invariants structure *)
                    not (LinearConstraint.pxd_is_true continuous_invariant)
                | Discrete_continuous_guard discrete_continuous_invariant ->
                    (* Costly test! But inherent to the invariants structure *)
                    not (LinearConstraint.pxd_is_true discrete_continuous_invariant.continuous_guard)
                (* We assume that an exclusively discrete invariant does not count as an invariant *)
			    | Discrete_guard _ -> false
		) locations_for_this_automaton
	) automata in


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check existence of complex updates *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	let transition_contains_complex_update (transition : AbstractModel.transition) : bool =
		let is_clock_update_complex = function
			| No_potential_update
			| Potential_resets _ -> false
			| Potential_updates _ -> true
		in

		let is_updates_complex (clock_update, _) = is_clock_update_complex clock_update in
		is_updates_complex transition.updates
	in
	
	let has_complex_updates : bool = 
	(* For all PTA *)
	List.exists (fun automaton_index ->
		let locations_for_this_automaton = locations_per_automaton automaton_index in
		(* For all locations *)
		List.exists (fun location_index ->
			let actions_for_this_location = actions_per_location automaton_index location_index in
			(* For all actions *)
			List.exists (fun action_index ->
				let transitions_for_this_location = List.map transitions_description (transitions automaton_index location_index action_index) in
				(* For all transitions *)
				List.exists (fun transition ->
					transition_contains_complex_update transition
				) transitions_for_this_location
			) actions_for_this_location
		) locations_for_this_automaton
	) automata in

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the initial state *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Print some information *)
	print_message Verbose_high ("*** Building initial state…");

    (* Extract only local variable references and init values *)
    let variable_refs_list = variable_refs |> Hashtbl.to_seq |> List.of_seq in
    let local_variables_list =
        List.filter_map (fun ((variable_name, id), var_type) ->
            if id <> 0 then Some ((variable_name, id), AbstractValue.default_value var_type) else None
        ) variable_refs_list
    in

    let local_variables_table = local_variables_list |> List.to_seq |> Hashtbl.of_seq in


	let (initial_location, initial_constraint) =
		make_initial_state variable_infos index_of_automata index_of_locations parameters removed_variable_names variable_names init_discrete_pairs init_definition in

	(* Add the observer initial constraint *)
	begin
	match initial_observer_constraint_option with
		| None -> ()
		| Some c -> LinearConstraint.px_intersection_assign initial_constraint [c];
	end;



	(* Build the K0 constraint *)
	let initial_p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse initial_constraint in

	(* Print some information *)
	if verbose_mode_greater Verbose_low then(
		print_message Verbose_low ("Initial parameter constraint:");
		print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint variable_names initial_p_constraint);
	);


	(* Build the X >= 0 constraint *)
	let px_clocks_non_negative = LinearConstraint.px_constraint_of_nonnegative_variables clocks in

	(* Print some information *)
	if verbose_mode_greater Verbose_medium then(
		print_message Verbose_medium ("Constraint X >= 0:");
		print_message Verbose_medium (LinearConstraint.string_of_px_linear_constraint variable_names px_clocks_non_negative);
	);


	(* Build the X >= 0 ^ K0 constraint *)
	let px_clocks_non_negative_and_initial_p_constraint = LinearConstraint.px_intersection [px_clocks_non_negative; (LinearConstraint.px_of_p_constraint initial_p_constraint)] in

	(* Print some information *)
	if verbose_mode_greater Verbose_medium then(
		print_message Verbose_medium ("Constraint X >= 0 ^ K0:");
		print_message Verbose_medium (LinearConstraint.string_of_px_linear_constraint variable_names px_clocks_non_negative_and_initial_p_constraint);
	);


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect bounded parameters *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create an array parameter_index -> bounds *)
	let parameter_bounds_array : AbstractModel.bounds array = Array.make nb_parameters {upper = Unbounded; lower = Unbounded} in
	(* Fill it *)
	List.iter (fun parameter_index ->
		let lower_bound, upper_bound = LinearConstraint.p_compute_bounds initial_p_constraint parameter_index in
		let bounds : AbstractModel.bounds =
		{
			lower	= (match lower_bound with None -> Unbounded | Some (bound, is_closed) -> Bounded (bound, is_closed));
			upper	= (match upper_bound with None -> Unbounded | Some (bound, is_closed) -> Bounded (bound, is_closed));
		}
		in
		(* Update array *)
		parameter_bounds_array.(parameter_index) <- bounds;

	) parameters;
	
	(* Detect unbounded model, i.e., at least one parameter is unbounded in at least one direction *)
	let unbounded_parameters = List.exists (fun parameter_index ->
		let lower_bound = parameter_bounds_array.(parameter_index).lower in
		let upper_bound = parameter_bounds_array.(parameter_index).upper in
		lower_bound = Unbounded || upper_bound = Unbounded
	) parameters in
	
	(* Build functional view *)
	let parameter_bounds parameter_index = parameter_bounds_array.(parameter_index) in



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug prints *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variables *)
	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("\n*** All variables:");
		for i = 0 to nb_variables - 1 do
			print_message Verbose_high ("  "
				^ (string_of_int i) ^ " : " ^ (variable_names i)
	(* 			^ (if is_renamed_clock i then " (renamed clock)" else "") *)
			);
		done;
	);

	if verbose_mode_greater Verbose_total then(
		(* Urgency of locations *)
		print_message Verbose_total ("\n*** Urgency and acceptance of locations:");
		(* For each automaton *)
		List.iter (fun automaton_index ->
			(* Print the automaton name *)
			print_message Verbose_total ("" ^ (automata_names automaton_index) ^ " :");
			(* For each location *)
			List.iter (fun location_index ->
				(* Get the urgency *)
				let urgency_string =
					if is_urgent automaton_index location_index then "URGENT" else "non-urgent"
				in
				(* Get the acceptance *)
				let acceptance_string =
					if is_accepting automaton_index location_index then "ACCEPTING" else "non-accepting"
				in
				print_message Verbose_total (" - " ^ (location_names automaton_index location_index) ^ " :" ^ urgency_string ^ "/" ^ acceptance_string);
			) (locations_per_automaton automaton_index);
		) automata;

		(* All action names *)
		print_message Verbose_total ("\n*** All action names:");
		(* For each action *)
		List.iter (fun action_index ->
			print_message Verbose_total ((string_of_int action_index) ^ " -> " ^ (action_names action_index));
		) actions;

		(* All controllable action names *)
		print_message Verbose_total ("\n*** All controllable actions:");
		(* For each action *)
		List.iter (fun action_index ->
			print_message Verbose_total ((string_of_int action_index) ^ " -> " ^ (action_names action_index));
		) controllable_actions_indices;

		(* Debug print: actions per automaton *)
		print_message Verbose_total ("\n*** Actions per automaton:");
		(* For each automaton *)
		List.iter (fun automaton_index ->
			(* Get the actions *)
			let actions = actions_per_automaton automaton_index in
			(* Print it *)
			let actions_string = string_of_list_of_string_with_sep ", " (List.map action_names actions) in
			print_message Verbose_total ((automata_names automaton_index) ^ " : " ^ actions_string)
		) automata;

		(* Debug print: automata per action *)
		print_message Verbose_total ("\n*** Automata per action:");
		(* For each action *)
		List.iter (fun action_index ->
			(* Get the automata *)
			let automata = automata_per_action action_index in
			(* Print it *)
			let automata_string = string_of_list_of_string_with_sep ", " (List.map automata_names automata) in
			print_message Verbose_total ((action_names action_index) ^ " : " ^ automata_string)
		) actions;


		(* Possible actions per location *)
		print_message Verbose_total ("\n*** Possible actions per location:");
		(* For each automaton *)
		List.iter (fun automaton_index ->
			(* Print the automaton name *)
			print_message Verbose_total ("" ^ (automata_names automaton_index) ^ " :");
			(* For each location *)
			List.iter (fun location_index ->
				(* Get the actions *)
				let actions = actions_per_location automaton_index location_index in
				(* Print it *)
				let my_string = string_of_list_of_string_with_sep ", " (List.map action_names actions) in
				print_message Verbose_total (" - " ^ (location_names automaton_index location_index) ^ " :" ^ my_string);
			) (locations_per_automaton automaton_index);
		) automata;

		(* Debug print: transition indexes *)
		print_message Verbose_total ("\n*** Transitions:");
		(* For each transition *)
		for transition_index = 0 to nb_transitions - 1 do
			(* Get the automaton *)
			let automaton_index = automaton_of_transition transition_index in
			(* Get the transition *)
			let transition = transitions_description transition_index in
			(* Print automaton + action *)
				(*** TODO: print source too (and guard, and reset?!) ***)
			print_message Verbose_total ("Transition " ^ (string_of_int transition_index) ^ ": in automaton `" ^ (automata_names automaton_index) ^ "` via action `" ^ (action_names (transition.action)) ^ "` to location `" ^ (location_names automaton_index (transition.target)) ^ "`")
		done;
		
		print_message Verbose_total ("");
	);

	(* Debug print: L/U *)
	begin
	match lu_status with
	| PTA_notLU ->
		print_message Verbose_low ("This model is not an L/U-PTA.");

	| PTA_LU (l_parameters, u_parameters) ->
		print_message Verbose_standard ("This model is an L/U-PTA:");
		print_message Verbose_standard ("- lower-bound parameters {" ^ (string_of_list_of_string_with_sep ", " (List.map variable_names l_parameters)) ^ "}");
		print_message Verbose_standard ("- upper-bound parameters {" ^ (string_of_list_of_string_with_sep ", " (List.map variable_names u_parameters)) ^ "}");

	| PTA_L ->
		print_message Verbose_standard ("This model is an L-PTA.");

	| PTA_U ->
		print_message Verbose_standard ("This model is a U-PTA.");

	end;

	(* Debug print: bounded parameters *)
	if nb_parameters > 0 then(
		if unbounded_parameters then
			print_message Verbose_low ("Parameters are not (all) bounded.")
		else
			print_message Verbose_standard ("Parameters are all bounded.")
		;
	);
	if verbose_mode_greater Verbose_low then(
		List.iter (fun parameter_index ->
			let bounds = parameter_bounds parameter_index in
			print_message Verbose_low ("Bounds for parameter " ^ (variable_names parameter_index) ^ ": "
				^ (match bounds.lower with Unbounded -> "(-infinity" | Bounded (bound, is_closed) -> (if is_closed then "[" else "(") ^ (NumConst.string_of_numconst bound))
				^ ", "
				^ (match bounds.upper with Unbounded -> "+infinity)" | Bounded (bound, is_closed) -> (NumConst.string_of_numconst bound) ^ (if is_closed then "]" else ")"))
				^ "");
		) parameters;
	);

	(* Debug print: special global clock *)
	begin
	match global_time_clock with
		| Some _ -> print_message Verbose_standard ("A global time clock `" ^ Constants.global_time_clock_name ^ "` has been detected.");
		| None -> print_message Verbose_medium ("No global time clock `" ^ Constants.global_time_clock_name ^ "` detected.");
	end;

	
	(* Debug print: strong determinism *)
	if !strongly_deterministic then
		print_message Verbose_low ("This PTA is strongly deterministic.")
	else
		print_message Verbose_medium ("This PTA is not strongly deterministic.")
	;

	
	(* Debug print: silent transitions *)
	if has_silent_actions then
		print_message Verbose_medium ("Silent actions detected.")
	else
		print_message Verbose_low ("No silent action detected.")
	;



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Build the final structure *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	{
	(* Cardinality *)
	nb_automata    = nb_automata;
	nb_actions     = nb_actions;
	nb_clocks      = nb_clocks;
	nb_discrete    = nb_discrete;
	nb_rationals   = nb_rationals;
	nb_parameters  = nb_parameters;
	nb_variables   = nb_variables;
	nb_ppl_variables = nb_ppl_variables;
	nb_locations   = nb_locations;
	nb_transitions = nb_transitions;

	(* Is there any invariant in the model? *)
	has_invariants = has_invariants;
	(* Is there any clock going at a rate <> 1 in the model? *)
	has_non_1rate_clocks = has_non_1rate_clocks;
	(* Is there any clock reset of another form than x := 0? *)
	has_complex_updates = has_complex_updates;
	(* Is the model an L/U-PTA? *)
	lu_status = lu_status;
	(* Is the model a strongly deterministic PTA? *)
	strongly_deterministic = !strongly_deterministic;
	(* Does the model contain any transition labeled by a silent, non-observable action? *)
	has_silent_actions = has_silent_actions;

	(* Are all parameters bounded in the initial state? *)
	bounded_parameters = not unbounded_parameters;
	(* Function returning the bounds of each parameter *)
	parameters_bounds = parameter_bounds;

	(* The observer *)
	observer_pta = observer_automaton_index_option;
	is_observer = (fun automaton_index ->
		match observer_automaton_index_option with
		| Some pta -> pta = automaton_index
		| None -> false
	);

	(* The list of clock indexes *)
	clocks = clocks;
	(* True for clocks, false otherwise *)
	is_clock = is_clock;
	(* Index of the special clock to be reset at each transition to measure time elapsing (only used in NZ checking) *)
	special_reset_clock = special_reset_clock;
	(* Index of a special clock meant to measure the global time (how this clock is actually used is up to the model designer *)
	global_time_clock = global_time_clock;
	(* The list of clock indexes except the reset clock (used, e.g., to print the model *)
	clocks_without_special_reset_clock = clocks_without_special_reset_clock;
	(* The list of discrete indexes *)
	discrete = discrete;
	(* The list of rational indexes *)
	discrete_rationals = discrete_rationals;
	(* True for discrete, false otherwise *)
	is_discrete = is_discrete;
	(* The list of parameter indexes *)
	parameters = parameters;
	(* The non parameters (clocks and discrete) *)
	clocks_and_discrete = list_append clocks discrete;
	(* The non clocks (parameters and discrete) *)
	parameters_and_discrete = list_append parameters discrete_rationals;
	(* The non discrete (clocks and parameters) *)
	parameters_and_clocks = list_append parameters clocks;
	(* The function : variable_index -> variable name *)
	variable_names = variable_names;
	(* All discrete variable names group by types *)
    discrete_names_by_type_group = discrete_names_by_type_group;
	(* The type of variables *)
	type_of_variables = type_of_variables;

	(* The automata *)
	automata = automata;
	(* The automata names *)
	automata_names = automata_names;

	(* The locations for each automaton *)
	locations_per_automaton = locations_per_automaton;
	(* The location names for each automaton *)
	location_names = location_names;
	(* The location attributes for each automaton *)
	is_accepting = is_accepting;
	is_urgent = is_urgent;

	(* All action indexes *)
	actions = actions;
	(* Only controllable action indexes *)
	controllable_actions = controllable_actions_indices;
	(* Action names *)
	action_names = action_names;
	(* The type of actions *)
	action_types = action_types;
	(* The list of actions for each automaton *)
	actions_per_automaton = actions_per_automaton;
	(* The list of automatons for each action *)
	automata_per_action = automata_per_action;
	(* The list of actions for each automaton for each location *)
	actions_per_location = actions_per_location;
	(* Is an action controllable *)
	is_controllable_action = is_controllable_action;

	(* The cost for each automaton and each location *)
	costs = costs;

	(* The invariant for each automaton and each location *)
	invariants = invariants;
	(* The transitions for each automaton and each location and each action *)
	transitions = transitions;
	(* The list of clocks stopped for each automaton and each location *)
	stopwatches = stopwatches;
	(* The list of pairs (clock, NumConst.t) defining the flow of some clocks at each automaton and each location *)
	flow = flow;
	(* An array transition_index -> transition *)
	transitions_description = transitions_description;
	(* An array transition_index -> automaton_index *)
	automaton_of_transition = automaton_of_transition;

    (* The list of declared functions *)
    functions_table = functions_table;

    (* Local variables table type *)
    (*** WARNING (ÉA, 2023/04/14): probably has nothing to do here ***)
    local_variables_table = local_variables_table;

	(* All clocks non-negative *)
	px_clocks_non_negative = px_clocks_non_negative;
	(* Initial location of the model *)
	initial_location = initial_location;
	(* Initial constraint of the model *)
	initial_constraint = initial_constraint;
	(* Initial constraint of the model projected onto P *)
	initial_p_constraint = initial_p_constraint;
	(* Initial constraint of the model projected onto P and all clocks non-negative *)
	px_clocks_non_negative_and_initial_p_constraint = px_clocks_non_negative_and_initial_p_constraint;

	}

	,
	
	abstract_property_option
