(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Convert a parsing structure into an abstract model
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci, Benjamin Loillier
 * Created           : 2009/09/09
 *
 ************************************************************)

(************************************************************)
(************************************************************)
(** Modules *)
(************************************************************)
(************************************************************)
open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Options
open Automaton
open DiscreteExpressions
open DiscreteExpressionEvaluator
open AbstractModel
open ParsingStructure
open VariableInfo
open AbstractProperty
open ParsingStructureUtilities
open DiscreteType
open CustomModules

(************************************************************)
(************************************************************)
(** Exceptions *)
(************************************************************)
(************************************************************)

(* For detecting strongly deterministic PTAs *)
exception Not_strongly_deterministic
exception InvalidProperty

(*------------------------------------------------------------*)
(* Find the clocks in a linear_constraint *)
(*------------------------------------------------------------*)

let get_clocks_in_linear_constraint clocks =
  (* Get a long list with duplicates, and then simplify *)
  (*	(* Should not be too inefficient, because our linear constraints are relatively small *)
    	let list_of_clocks = List.fold_left (fun current_list_of_clocks linear_inequality ->
    		list_append current_list_of_clocks (get_clocks_in_linear_inequality is_clock linear_inequality)
    	) [] linear_constraint
    	in
    	(* Simplify *)
    	list_only_once list_of_clocks*)
  LinearConstraint.pxd_find_variables clocks


(*** WARNING: duplicate function in ClockElimination ***)
let rec get_clocks_in_updates updates : clock_index list =
  let get_clocks: clock_updates -> clock_index list = function
    (* No update at all *)
    | No_update -> []
    (* Reset to 0 only *)
    | Resets clock_reset_list -> clock_reset_list
    (* Reset to arbitrary value (including discrete, parameters and clocks) *)
    | Updates clock_update_list ->
      let result, _ = List.split clock_update_list in result
  in
  let clocks_in_conditons = List.flatten (List.map
    (fun (b, u1, u2) -> (get_clocks_in_updates u1) @ (get_clocks_in_updates u2) )
    updates.conditional)
  in
  (get_clocks updates.clock) @ clocks_in_conditons

(************************************************************)
(** Print error messages *)
(************************************************************)

(* Print variable not declared in linear constraint *)
let undeclared_variable_in_linear_constraint_message variable_name =
    print_error ("The variable `" ^ variable_name ^ "` used in a linear constraint was not declared.")

(* Print variable not declared in bool expression *)
let undeclared_variable_in_boolean_expression_message variable_name =
    print_error ("The variable `" ^ variable_name ^ "` used in a boolean expression was not declared.")

(************************************************************)
(** Converting linear constraints *)
(************************************************************)

(* TODO benjamin CLEAN remove comments, it was moved to ExpressionConverter *)
(*------------------------------------------------------------*)
(* Convert a ParsingStructure.linear_expression into an array of coef and constant *)
(*------------------------------------------------------------*)
(*
let array_of_coef_of_linear_expression index_of_variables constants linear_expression =
  (* Create an array of coef *)
  let array_of_coef = Array.make (Hashtbl.length index_of_variables) NumConst.zero in
  (* Create a zero constant *)
  let constant = ref NumConst.zero in

  (* Internal function to update the array for a linear term *)
  let update_array_linear_term mul_coef = function
    (* Case constant -> update the constant with the coef *)
    | Constant c -> constant := NumConst.add !constant (NumConst.mul c mul_coef);
      (* Case variables -> update the array with the coef  *)
    | Variable (coef, variable_name) ->
      (* Try to find the variable_index *)
      if Hashtbl.mem index_of_variables variable_name then (
        let variable_index = Hashtbl.find index_of_variables variable_name in
        (* Update the variable with its coef *)
        array_of_coef.(variable_index) <- NumConst.add array_of_coef.(variable_index) (NumConst.mul coef mul_coef);
        (* Try to find a constant *)
      ) else (
        if Hashtbl.mem constants variable_name then (
          (* Retrieve the value of the global constant *)
          let value = Hashtbl.find constants variable_name in
          let numconst_value = DiscreteValue.numconst_value value in
          (* Update the NumConst *)
          constant := NumConst.add !constant (NumConst.mul (NumConst.mul numconst_value coef) mul_coef);
        ) else (
          raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))
        )
      );
  in

  (* Internal function to update the array for a linear expression *)
  let rec update_array_linear_expression = function
    | Linear_term lt -> update_array_linear_term NumConst.one lt
    | Linear_plus_expression (le, lt) ->
      (* Fill the array with le *)
      update_array_linear_expression le;
      (* Fill the array with lt *)
      update_array_linear_term NumConst.one lt;
    | Linear_minus_expression (le, lt) ->
      (* Fill the array with le *)
      update_array_linear_expression le;
      (* Fill the array with lt *)
      update_array_linear_term NumConst.minus_one lt;
  in
  (* Call the recursive function *)
  update_array_linear_expression linear_expression;
  (* Return the array of coef and the constant *)
  array_of_coef, !constant
*)

(************************************************************)
(************************************************************)
(** Checking and converting model *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Getting variables *)
(************************************************************)

(*------------------------------------------------------------*)
(* Get all (possibly identical) names of variables in the header *)
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

(* Only get declared discrete variables with their specific types *)
let get_declared_discrete_variables_by_type variable_declarations =
    let get_discrete_variables_in_variable_declaration discretes_by_type (var_type, list_of_names) =
        let new_list, new_constants = get_variables_and_constants var_type list_of_names in
        match var_type with
            | Var_type_discrete var_type_discrete ->
                let new_list_discretes_by_type = List.map (fun variable_names -> var_type, variable_names) new_list in
                List.rev_append new_list_discretes_by_type discretes_by_type
            | _ ->
                discretes_by_type
        in
        List.fold_left get_discrete_variables_in_variable_declaration [] variable_declarations

(*------------------------------------------------------------*)
(* Get all (possibly identical) names of automata *)
(*------------------------------------------------------------*)
let get_declared_automata_names =
  List.map (fun (automaton_name, _, _) -> automaton_name)

(*------------------------------------------------------------*)
(* Get all (all different) names of synclabs *)
(*------------------------------------------------------------*)
let get_declared_synclabs_names =
  List.fold_left (fun action_names (_, synclabs, _) -> list_union action_names synclabs) []

(************************************************************)
(** Checking the model *)
(************************************************************)

(*------------------------------------------------------------*)
(* Check that variable names are all different, return false otherwise; warns if a variable is defined twice as the same type *)
(*------------------------------------------------------------*)
(* TODO benjamin refactor because of new types ! *)
let check_variable_names clock_names discrete_names parameters_names constants =
	(* Warn if a variable is defined twice as the same type *)
	let warn_for_multiply_defined_variables list_of_variables =
		(* Compute the multiply defined variables *)
		let multiply_defined_variables = elements_existing_several_times list_of_variables in
		(* Print a warning for each of them *)
		List.iter (fun variable_name -> print_warning ("Multiply-declared variable '" ^ variable_name ^"'")) multiply_defined_variables;
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
(* Check that the names of automata are all different; return false otherwise *)
(*------------------------------------------------------------*)
let check_declared_automata_names automata_names =
	(* Compute the multiply defined variables *)
	let multiply_defined_names = elements_existing_several_times automata_names in
	(* Print an error for each of them *)
	match multiply_defined_names with
	| [] -> true
	| _ -> List.iter (fun variable_name -> print_error ("Several automata have name " ^ variable_name ^ ".")) multiply_defined_names; false


(*------------------------------------------------------------*)
(* Check that all locations of a given automaton are different *)
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
(* Check that a normal update is well formed *)
(*------------------------------------------------------------*)
let check_normal_update variable_infos automaton_name normal_update =

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
    let all_variables_declared = ParsingStructureUtilities.all_variables_defined_in_parsed_normal_update variable_infos print_variable_in_update_not_declared_opt print_variable_in_update_not_declared_opt normal_update in
    (* Get all updated variables (can have many updated variables for one update, in conditional update for example) *)
    let updated_variable_name_opt = (ParsingStructureUtilities.fold_map_parsed_normal_update (^) "" (function _ -> "") (function Leaf_update_updated_variable variable_name -> variable_name) normal_update |> List.filter (fun x -> x <> "") |> List.nth_opt) 0 in

    let updated_variable_name = match updated_variable_name_opt with Some updated_variable_name -> updated_variable_name | None -> "_" in

    if updated_variable_name <> "_" && is_variable_is_defined variable_infos updated_variable_name then
        (* Get kind (variable or constant ?) of updated variable *)
        let variable_kind = variable_kind_of_variable_name variable_infos updated_variable_name in
        (* Get var type of updated variable *)
        let var_type = ExpressionConverter.TypeChecker.get_type_of_variable_by_name variable_infos updated_variable_name in

        (* Check if variable is a constant *)
        let is_constant = match variable_kind with Constant_kind _ -> true | Variable_kind _ -> false in
        (* Check if variable is a parameter *)
        let is_parameter = match var_type with DiscreteType.Var_type_parameter -> true | _ -> false in
        (* Check if variable is a discrete type *)
        let is_discrete = match var_type with DiscreteType.Var_type_discrete _ -> true | _ -> false in

        (* Eventually print error messages *)
        if is_constant then print_update_constant_error updated_variable_name;
        if is_parameter then print_update_parameter_error updated_variable_name;

        if is_discrete then (
            let is_only_discrete = ParsingStructureUtilities.only_discrete_in_parsed_global_expression variable_infos None update_expr in
            if not is_only_discrete then
                print_error ("Trying to update variable `" ^ updated_variable_name ^ "` with clock(s) or parameter(s) in `" ^ ParsingStructureUtilities.string_of_parsed_normal_update variable_infos normal_update ^ "`.");
        );

        all_variables_declared && not (is_constant || is_parameter)
    else (
        all_variables_declared
    )

(*------------------------------------------------------------*)
(* Check that an update is well formed *)
(*------------------------------------------------------------*)
let check_update variable_infos automaton_name = function
	| Normal normal_update ->
	    check_normal_update variable_infos automaton_name normal_update

	| Condition (bool_expr, update_list_if, update_list_else) ->

	    (* Concatenate updates*)
	    let all_updates = update_list_if @ update_list_else in

        (* Prepare print error function for conditional containing clock or parameter *)
	    let print_conditional_update_contain_clock_or_param_error var_type variable_name =
            print_error (
                "Condition update `"
                ^ ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos bool_expr
                ^ "` contains "
                ^ DiscreteType.string_of_var_type var_type
                ^ " `"
                ^ variable_name
                ^ "` in automaton `"
                ^ automaton_name
                ^ "`."
            )
	    in

        (* Prepare callback function that print error message when undeclared variable is found *)
        let print_variable_in_update_condition_not_declared variable_name =
            print_error (
                "Variable `"
                ^ variable_name
                ^ "` used in update condition `"
                ^ ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos bool_expr
                ^ "` in automaton `"
                ^ automaton_name
                ^ "` was not declared."
            )
        in

	    (* Check that all variables in update condition are declared *)
        let all_declared_in_condition = ParsingStructureUtilities.all_variables_defined_in_parsed_boolean_expression variable_infos (Some print_variable_in_update_condition_not_declared) bool_expr in

	    (* Check that boolean condition expression doesn't contains any clock(s) or parameter(s) *)
	    let is_condition_use_only_discrete =
	        ParsingStructureUtilities.only_discrete_in_parsed_boolean_expression variable_infos (Some print_conditional_update_contain_clock_or_param_error) bool_expr
        in

	    (* Check all normal updates are valid (make a map for avoid short-circuit eval with for_all) *)
	    let is_valid_normal_updates =
	        List.map (check_normal_update variable_infos automaton_name) all_updates
	        |> List.for_all identity
        in

	    (* If all normal updates and condition are valid, update is valid *)
	    all_declared_in_condition && is_valid_normal_updates && is_condition_use_only_discrete

(* TODO benjamin CLEAN remove comments *)
(*------------------------------------------------------------*)
(* Check that an update is well formed *)
(*------------------------------------------------------------*)
(*
let check_update variable_infos automaton_name update =

    let check_update_normal (parsed_update_type, global_expression) =

        (* Function that check that variable kind of an update is not a constant *)
        let check_variable_kind_of_update variable_name =

            (* Get kind of variable (variable / constant) *)
            let variable_kind = ParsingStructureUtilities.variable_kind_of_variable_name variable_infos variable_name in

            (* Get variable type, if possible *)
            match variable_kind with
            | ParsingStructureUtilities.Variable_kind _ ->
                ExpressionConverter.TypeChecker.get_type_of_variable_by_name_opt variable_infos variable_name

            | ParsingStructureUtilities.Constant_kind _ ->
                    print_error (
                        "Trying to update a constant: `"
                        ^ ParsingStructureUtilities.string_of_parsed_update_type variable_infos parsed_update_type
                        ^ " := "
                        ^ ParsingStructureUtilities.string_of_parsed_global_expression variable_infos global_expression
                        ^ "`"
                    );
                    raise InvalidModel
        in

        (* Function that check that variable type of an update is correct *)
        let check_variable_type_of_update variable_name = function

            (* Type clock: allow any linear term in updates: so just check that variables have been declared *)
            | Some DiscreteType.Var_type_clock ->
                print_message Verbose_total ("                A clock!");
                true

            (* Case of a discrete var.: allow only an arithmetic expression of constants and discrete *)
            | Some DiscreteType.Var_type_discrete var_type_discrete ->
                let string_of_var_type = DiscreteType.string_of_var_type_discrete var_type_discrete in
                print_message Verbose_total ("                A " ^ string_of_var_type ^ "!");

                let is_only_discrete = ParsingStructureUtilities.only_discrete_in_parsed_global_expression variable_infos global_expression in

                if not is_only_discrete then (
                    print_error ("The variable `" ^ variable_name ^ "` is a discrete and its update can only be an arithmetic expression over constants and discrete variables in automaton `" ^ automaton_name ^ "`."); false
                )
                else (
                    print_message Verbose_total ("                Check passed.");
                    true
                )

            (* Case of a parameter: forbidden! *)
            | Some DiscreteType.Var_type_parameter ->
                print_error ("The variable `" ^ variable_name ^ "` is a parameter and cannot be updated in automaton `" ^ automaton_name ^ "`."); false

            | None ->
                print_error ("The variable `" ^ variable_name ^ "` used in an update in automaton `" ^ automaton_name ^ "` was not declared."); false
        in

        (* Prepare callback function that will be called if an undeclared variable is found in an update *)
        let print_variable_in_update_not_declared str_left_member variable_name =
            print_error ("Variable `" ^ variable_name ^ "` used in update `" ^ str_left_member ^ " := " ^ ParsingStructureUtilities.string_of_parsed_global_expression variable_infos global_expression ^ "` in automaton `" ^ automaton_name ^ "` was not declared.")
        in

        (* Function that check if all variables are defined in update *)
        let check_all_variables_defined_in_update updated_variable_name =
            ParsingStructureUtilities.all_variables_defined_in_parsed_global_expression variable_infos (Some (print_variable_in_update_not_declared updated_variable_name)) global_expression
        in

        (* Function that check update on variable or variable access *)
        let rec check_parsed_update_type = function
            | Parsed_void_update ->
                check_all_variables_defined_in_update "_"

            | Parsed_indexed_update (inner_update_type, index_expr) as update_type ->

                (* String representation of the current indexed update *)
                let str_indexed_update = ParsingStructureUtilities.string_of_parsed_update_type variable_infos update_type in

                (* Check that all variable in index expression was declared *)
                let all_variable_declared_in_index_expr = ParsingStructureUtilities.all_variables_defined_in_parsed_discrete_arithmetic_expression variable_infos (Some (print_variable_in_update_not_declared str_indexed_update)) index_expr in
                (* Update is valid only if all variables are declared and inner update type is valid *)
                all_variable_declared_in_index_expr && check_parsed_update_type inner_update_type

            | Parsed_scalar_update variable_name ->

                (* Check whether this variable is to be removed because unused elsewhere than in resets *)
                let to_be_removed = List.mem variable_name variable_infos.removed_variable_names in

                if to_be_removed then
                    true
                else (

                    (* Check if updated variable is declared *)
                    let is_updated_variable_defined = List.mem variable_name variable_infos.variable_names in
                    (* If not declared, print error message *)
                    if not is_updated_variable_defined then
                        print_variable_in_update_not_declared variable_name variable_name;

                    (* Check that variables contained in update expression are all declared *)
                    let all_variables_defined = check_all_variables_defined_in_update variable_name in

                    let is_variable_not_a_constant = lazy (
                        (* Check that updated variable is not a constant *)
                        let variable_type_opt = check_variable_kind_of_update variable_name in
                        (* Check type of updated variable *)
                        check_variable_type_of_update variable_name variable_type_opt
                    )
                    in

                    (* Short-circuit eval *)
                    all_variables_defined && is_updated_variable_defined && Lazy.force is_variable_not_a_constant
                )
        in
        check_parsed_update_type parsed_update_type

    in

    (* Function that check the condition of the conditional update *)
	let check_update_condition_elements = function
        | Leaf_constant _ -> true
        | Leaf_variable variable_name ->

            (* Get variable type, if possible *)
            let variable_type =
                if List.mem variable_name variable_infos.variable_names then (
                    let index = index_of_variable_name variable_infos variable_name in
                    Some (variable_infos.type_of_variables index)
                ) else if is_constant_is_defined variable_infos variable_name then (
                    let value = Hashtbl.find variable_infos.constants variable_name in
                    Some (DiscreteValue.var_type_of_value value)
                ) else
                    None
            in
            match variable_type with
            | None -> print_error ("Variable or constant \"" ^ variable_name ^ "\" in the condition of a conditional update is not declared."); false
            | Some DiscreteType.Var_type_clock -> print_error ("The variable " ^ variable_name ^ " is a clock and cannot be used in the condition of a conditional update."); false
            | Some DiscreteType.Var_type_parameter -> print_error ("The variable " ^ variable_name ^ " is a parameter and cannot be used in the condition of a conditional update."); false
            | _ -> print_message Verbose_total ("                Check passed."); true

    in
	(* Print some information *)
	print_message Verbose_total ("              Checking one update");

	match update with
	| Normal update -> check_update_normal update
	| Condition (update, updates_if, updates_else) ->
	    (* Check if condition is well formed *)
	    let is_well_formed_condition = ParsingStructureUtilities.for_all_in_parsed_boolean_expression check_update_condition_elements update in
	    (* Check if updates are well formed *)
	    let is_well_formed_updates = List.fold_left (fun acc u ->
            (check_update_normal u) && acc
        ) true (updates_if@updates_else)
        in
        (* If condition not well formed display specific message *)
        if not (is_well_formed_condition) then (
            print_error (
                "Condition \""
                ^ (ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos update)
                ^ "\" is ill-formed"
            );
            false
        )
        else
            is_well_formed_condition && is_well_formed_updates
*)

(*------------------------------------------------------------*)
(* Check that a sync is well formed *)
(*------------------------------------------------------------*)
let check_sync sync_name_list automaton_name = function
	| Sync sync_name ->  if not (List.mem sync_name sync_name_list) then (
		print_error ("The sync action '" ^ sync_name ^ "' used in automaton `" ^ automaton_name ^ "` was not declared for this automaton."); false)
		else true
	| NoSync -> true

(*------------------------------------------------------------*)
(* Check that a sync is used in all the automata where it is declared *)
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
				print_warning ("The synclab '" ^ synclab_name ^ "' is not used in (at least) the automaton `" ^ automaton_name ^ "` where it is declared: it will thus be removed from the whole model.");
				raise Not_found;
			);
			);
		) automata;
		(* The synclab was found everywhere: true *)
		true
		(* At least one automata does not use the synclab : false *)
	) with Not_found -> false


(*------------------------------------------------------------*)
(* Check that all variables mentioned in a list of stopwatches exist and are clocks *)
(*------------------------------------------------------------*)
let check_stopwatches variable_infos location_name stopwatches =

    List.map (fun stopwatch_name ->
            let var_type = ExpressionConverter.TypeChecker.get_type_of_variable_by_name_opt variable_infos stopwatch_name in
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
(* Check that all variables mentioned in a list of flows exist and are clocks *)
(*------------------------------------------------------------*)
let check_flows_2 variable_infos location_name flows =

    (* Check clocks are declared and well-typed *)
    let clock_names = List.map first_of_tuple flows in
    let is_clocks_declared = check_stopwatches variable_infos location_name clock_names in

    (* Group flow values by clock name *)
    let clock_names_by_flow_values = OCamlUtilities.group_by (fun (clock_name, flow_value) -> clock_name) flows in

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

let check_flows nb_clocks index_of_variables type_of_variables location_name flows =
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
	!ok

(* Check if user function definition is well formed *)
(* - check that all variables used in user function are declared *)
let check_fun_definition variable_infos (fun_def : parsed_fun_definition) =

    (* Check if there is duplicate parameter with inconsistent types *)
    let is_not_consistent_duplicate_parameters =

        (* Message to display when duplicate parameters found *)
        let duplicate_parameter_message parameter_name =
            "Duplicate parameter `"
            ^ parameter_name
            ^ "` in function `"
            ^ fun_def.name
            ^ "`"
        in

        (* Check that each parameter have different name *)
        (* Group parameters by their names *)
        let parameters_by_names = OCamlUtilities.group_by first_of_tuple fun_def.parameters in
        (* If for one parameter name, their is more than one parameter, there is duplicates *)
        let duplicate_parameters = List.filter (fun (parameter_name, group) -> List.length group > 1) parameters_by_names in

        (* For each parameter get if duplicate definitions are consistent or not *)
        (* Ex: for fn f (a : int, a : rat), duplicate definition of `a` isn't consistent *)
        let not_consistent_duplicate_parameters = List.map (fun (parameter_name, group) ->
                (* Remove parameter duplicates *)
                let group_without_duplicates = OCamlUtilities.list_only_once group in
                (* Prepare message *)
                let current_duplicate_parameter_message = duplicate_parameter_message parameter_name in
                (* If duplicates remain greater than 1, there is inconsistent definitions *)
                if List.length group_without_duplicates = 1 then (
                    print_warning (current_duplicate_parameter_message ^ ".");
                    false
                ) else (
                    let str_parameters_list = List.map (fun (parameter_name, discrete_type) -> parameter_name ^ " : " ^ DiscreteType.string_of_var_type_discrete discrete_type) group_without_duplicates in
                    let str_parameters = OCamlUtilities.string_of_list_of_string_with_sep ", " str_parameters_list in
                    print_error (current_duplicate_parameter_message ^ "` doesn't have consistent definitions: `" ^ str_parameters ^ "`.");
                    true
                )
            ) duplicate_parameters
        in
        (* Check if it exist a non consistent duplicate definition *)
        List.exists identity not_consistent_duplicate_parameters
    in

    (* Check if all variables in function definition are defined *)
    let is_all_variables_defined =

        (* Prepare callback function that print error message when undeclared variable is found *)
        let print_variable_in_fun_not_declared variable_name =
            print_error (
                "Variable `"
                ^ variable_name
                ^ "` used in function `"
                ^ fun_def.name
                ^ "` was not declared."
            )
        in

        let print_variable_in_fun_not_declared_opt = Some print_variable_in_fun_not_declared in
        ParsingStructureUtilities.all_variables_defined_in_parsed_fun_def variable_infos print_variable_in_fun_not_declared_opt print_variable_in_fun_not_declared_opt fun_def
    in

    (* TODO benjamin IMPLEMENT check that a local variable is not updated *)

    (* Return *)
    not is_not_consistent_duplicate_parameters && is_all_variables_defined

(*------------------------------------------------------------*)
(* Check that the automata are well-formed *)
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
				if not (ParsingStructureUtilities.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message cost) then well_formed := false;
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
			if not (ParsingStructureUtilities.all_variables_defined_in_nonlinear_convex_predicate variable_infos (Some undeclared_variable_in_boolean_expression_message) location.invariant) then well_formed := false;


			(* Check transitions *)
			print_message Verbose_total ("          Checking transitions");
			List.iter (fun (convex_predicate, update_section, sync, target_location_name) ->
				(* Check the convex predicate *)
				print_message Verbose_total ("            Checking convex predicate");
				if not (ParsingStructureUtilities.all_variables_defined_in_nonlinear_convex_predicate variable_infos (Some undeclared_variable_in_boolean_expression_message) convex_predicate) then well_formed := false;
				(* Check the updates *)
				print_message Verbose_total ("            Checking updates");
				let updates = ParsingStructureUtilities.updates_of_update_section update_section in
				List.iter (fun update -> if not (check_update variable_infos automaton_name update) then well_formed := false) updates;
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

(* Partition between initial localisations and initial inequalities *)
let partition_loc_init_and_variable_inits init_definition =
	(* Get all the Parsed_loc_assignment *)
	let loc_assignments, init_inequalities = List.partition (function
		| Parsed_loc_assignment _ -> true
		| Parsed_linear_predicate _
		| Parsed_discrete_predicate _ -> false
    ) init_definition
    in

	(* Make pairs (automaton_name, location_name) *)
	let initial_locations = List.map (function
		| Parsed_loc_assignment (automaton_name, location_name) -> (automaton_name, location_name)
		| _ -> raise (InternalError "Something else than a Parsed_loc_assignment was found in a Parsed_loc_assignment list")
    ) loc_assignments
    in

    (* Return initial locations, initial inequalities *)
    initial_locations, init_inequalities

(* Check whether an automaton has exactly one initial location *)
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
            if not (is_variable_or_constant_declared variable_infos variable_name) then (
                print_error ("Variable `" ^ variable_name ^ "` in discrete init is not declared");
                false
            )
            (* And that all variables in expr are defined *)
            else if not (ParsingStructureUtilities.all_variables_defined_in_parsed_global_expression_without_callback variable_infos expr) then (
                print_error ("Expression \"" ^ variable_name ^ " := " ^ ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr ^ "\" use undeclared variable(s)");
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
        | Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , linear_expression) as linear_constraint when is_variable_removed variable_infos variable_name ->
            print_message Verbose_total ("Variable `" ^ variable_name ^ "` is compared to a linear term, but will be removed: no check." );
            (* Still check the second term *)
            if not (ParsingStructureUtilities.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message linear_expression) then (
                print_error ("Linear constraint \"" ^ ParsingStructureUtilities.string_of_parsed_linear_constraint variable_infos linear_constraint ^ "\" use undeclared variable(s)");
                false
            )
            else
                true
        (* General case: check *)
        | Parsed_linear_constraint _ as linear_constraint ->
            if not (ParsingStructureUtilities.all_variables_defined_in_linear_constraint variable_infos undeclared_variable_in_linear_constraint_message linear_constraint) then (
                print_error ("Linear constraint \"" ^ ParsingStructureUtilities.string_of_parsed_linear_constraint variable_infos linear_constraint ^ "\" use undeclared variable(s)");
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

(*
let is_inequality_has_left_hand_removed_variable_used_in_init removed_variable_names only_used_in_init_variable_names = function
    | Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , _))
    | Parsed_discrete_predicate (variable_name, _) ->
        (* Filter out if the left-hand is in the removed variable names *)
        List.mem variable_name removed_variable_names
        &&
        not (List.mem variable_name only_used_in_init_variable_names)
    (* Any other combination is OK *)
    | _ ->
        false
*)

let partition_discrete_continuous variable_infos filtered_init_inequalities =
    List.partition (function
		(* Check if the left part is only a variable name *)
		| Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , _)) ->
			let is_discrete =
			(* TODO benjamin REFACTOR, can refactor with VariableInfo *)
			(* Try to get the variable index *)
			if (is_variable_is_defined variable_infos variable_name) then (
				let variable_index = index_of_variable_name variable_infos variable_name in
				(* Keep if this is a discrete *)
				DiscreteType.is_discrete_type (variable_infos.type_of_variables variable_index)
			) else if (is_constant_is_defined variable_infos variable_name) then
			    false
            else (
                (* Otherwise: problem! *)
                raise (InternalError ("The variable `" ^ variable_name ^ "` mentioned in the init definition does not exist."));
            )
			in is_discrete
	    (* All parsed boolean predicate are for discrete variables *)
        | Parsed_discrete_predicate _ -> true
		(* Otherwise false *)
		| _ -> false
    ) filtered_init_inequalities

(* Convert discrete linear constraint predicate to discrete predicate, more simple *)
let discrete_predicate_of_discrete_linear_predicate = function
    | Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (coeff, discrete_name)), op , expression)) ->

        (* Check *)
        if NumConst.neq coeff NumConst.one then (
            print_error ("The discrete variable `" ^ discrete_name ^ "` must have a coeff 1 in the init definition.");
            None
        )
        else (

            match (op, expression) with
            (* Simple constant: OK *)
            | (PARSED_OP_EQ, Linear_term (Constant c)) ->
                let rational_value = DiscreteValue.Number_value c in
                let discrete_predicate = Parsed_discrete_predicate (
                    discrete_name,
                    Parsed_global_expression (
                    Parsed_Discrete_boolean_expression (
                    Parsed_arithmetic_expression (
                    Parsed_DAE_term (
                    Parsed_DT_factor (
                    Parsed_DF_constant rational_value)))))
                )
                in
                Some discrete_predicate
            (* Constant: OK *)
            | (PARSED_OP_EQ, Linear_term (Variable (coef, variable_name))) ->
                let coef_rational_value = DiscreteValue.Number_value coef in
                let discrete_predicate = Parsed_discrete_predicate (
                    discrete_name,
                    Parsed_global_expression (
                    Parsed_Discrete_boolean_expression (
                    Parsed_arithmetic_expression (
                    Parsed_DAE_term (
                    Parsed_product_quotient (
                    Parsed_DT_factor (Parsed_DF_constant coef_rational_value),
                    Parsed_DF_variable variable_name,
                    Parsed_mul)))))
                )
                in
                Some discrete_predicate
            | _ ->
                print_error ("The initial value for discrete variable `" ^ discrete_name ^ "` must be given in the form `" ^ discrete_name ^ " = c`, where `c` is an integer, a rational or a constant.");
                None
        )
    | Parsed_discrete_predicate _ as discrete_predicate -> Some discrete_predicate
    | _ ->
        raise (InternalError "Trying to convert a non discrete linear constraint to discrete predicate")

let check_discrete_predicate_and_init variable_infos init_values_for_discrete = function
    | Parsed_discrete_predicate (variable_name, expr) ->

        (* TODO benjamin REFACTOR with kind_of *)
        (* Check that initialized variable of name 'variable_name' is not a constant *)
        if is_constant_is_defined variable_infos variable_name then (
            print_error ("Initialize '" ^ variable_name ^ "' constant is forbidden");
            false
        )
        else (


            (* Get the variable index *)
            let discrete_index = index_of_variable_name variable_infos variable_name in
            (* TYPE CHECKING *)
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
            else if not (DiscreteExpressionEvaluator.is_global_expression_constant converted_expr) then (

                print_error (
                    "Init variable \""
                    ^ variable_name
                    ^ "\" with a non constant expression \""
                    ^ ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr
                    ^ "\" is forbidden."
                );
                false
            ) else (
                let value = DiscreteExpressionEvaluator.try_eval_constant_global_expression converted_expr in
                Hashtbl.add init_values_for_discrete discrete_index value;
                true
            )

        )

    | _ -> raise (InternalError ("Must have this form since it was checked before."))


(*------------------------------------------------------------*)
(* Check that the init_definition are well-formed *)
(*------------------------------------------------------------*)
let check_init (useful_parsing_model_information : useful_parsing_model_information) init_definition observer_automaton_index_option =

    let variable_infos = useful_parsing_model_information.variable_infos in

	let well_formed = ref true in

    (* Partition init predicates between initial location and inequalities *)
    let initial_locations, init_inequalities = partition_loc_init_and_variable_inits init_definition in

    (* For all definitions : *)
    (* Check that automaton names and location names exist *)
    (* Check that continuous variables used in continuous init exist *)
    (* Check that discrete variables used in discrete init exist *)
    let definitions_well_formed = List.for_all (check_init_definition useful_parsing_model_information) init_definition in

    (* Check there is only one initial location per automaton *)
    let one_loc_per_automaton = has_one_loc_per_automaton initial_locations useful_parsing_model_information observer_automaton_index_option in

	(* Remove the inequalities of which the left-hand term is a removed variable *)
	let filtered_init_inequalities = List.filter (fun x -> not (is_inequality_has_left_hand_removed_variable variable_infos.removed_variable_names x)) init_inequalities in

	(* Partition the init inequalities between the discrete init assignments, and other inequalities *)
	let discrete_init, other_inequalities = partition_discrete_continuous variable_infos filtered_init_inequalities in

    (* Convert discrete linear constraint to discrete predicate *)
    let some_discrete_predicates = List.map discrete_predicate_of_discrete_linear_predicate discrete_init in

    let discrete_predicate_well_formed = List.for_all (function | None -> false | _ -> true) some_discrete_predicates in

    well_formed := definitions_well_formed && one_loc_per_automaton && discrete_predicate_well_formed;

    (* Here if not well formed we can raise an error *)
    if not (!well_formed) then
        raise InvalidModel;

    let discrete_predicates = List.map OCamlUtilities.a_of_a_option some_discrete_predicates in

    (* Check init discrete section : discrete *)
	(* Check that every discrete variable is given only one (rational) initial value *)
	let init_values_for_discrete = Hashtbl.create (List.length variable_infos.discrete) in

    (* Compute discrete init values and add to init hash table *)
    let discrete_initialization_well_formed = List.for_all (check_discrete_predicate_and_init variable_infos init_values_for_discrete) discrete_predicates in
    (* If some value is None, there is errors *)
    well_formed := !well_formed && discrete_initialization_well_formed;

	(* Check that every discrete variable is given at least one initial value (if not: warns) *)
	List.iter (fun discrete_index ->
		if not (Hashtbl.mem init_values_for_discrete discrete_index) then (
		    let variable_name = variable_name_of_index variable_infos discrete_index in
		    let variable_type = var_type_of_variable_name variable_infos variable_name in
		    let default_value = DiscreteValue.default_value variable_type in

			print_warning ("The discrete variable '" ^ variable_name ^ "' was not given an initial value in the init definition: it will be assigned to " ^ DiscreteValue.string_of_value default_value ^ ".");
			Hashtbl.add init_values_for_discrete discrete_index default_value
		);
    ) variable_infos.discrete;

	(* Convert the Hashtbl to pairs (discrete_index, init_value) *)
	let discrete_values_pairs =
		List.map (fun discrete_index ->
			discrete_index, Hashtbl.find init_values_for_discrete discrete_index
		) variable_infos.discrete
	in

(*    let other_inequalities = List.map (replace_unused_discrete_variable_by_constant variable_infos init_values_for_discrete) other_inequalities in*)

    (* Check init constraints section : continuous *)

	(* Check variable types in other_inequalities *)
	(* As other_inequalities represent init constraints of clocks, parameters, or only rational discrete variables *)
    (* we raise an exception if there is one discrete variable of another type than discrete rational *)

    let continuous_init_error = ref false in
	List.iter (fun lp ->

        (* Search variables used in linear predicate *)
	    let variable_names = ParsingStructureUtilities.get_variables_in_init_state_predicate lp in

        (* Gathering all variables that are non rational *)
        let non_rational_variable_names = StringSet.filter (fun variable_name ->
            let discrete_type = discrete_type_of_variable_or_constant variable_infos variable_name in
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
                ^ DiscreteType.string_of_var_type_discrete (DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rat)
            );
            continuous_init_error := true;
        ) non_rational_variable_names

	) other_inequalities;

    (* There are errors in the constraints init section *)
    if !continuous_init_error then
        raise (InvalidExpression ("There are errors in the continuous init section"));

	(* Return whether the init declaration passed the tests *)
	discrete_values_pairs, !well_formed



(************************************************************)
(** Converting the model *)
(************************************************************)

(*------------------------------------------------------------*)
(* Create the hash table of constants ; check the validity on-the-fly *)
(*------------------------------------------------------------*)
let make_constants constants =
  (* Create hash table *)
  let constants_hashtable : (string, DiscreteValue.discrete_value) Hashtbl.t = Hashtbl.create (List.length constants) in
  (* Manage Boolean for checking errors *)
  let correct = ref true in
  List.iter (fun (name, value(*, discrete_type *)) ->
      if (Hashtbl.mem constants_hashtable name) then (
        let old_value = Hashtbl.find constants_hashtable name in
        (* If same: warning *)
        if(DiscreteValue.equal old_value value) then(
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
(* Get all the declared actions for every automaton *)
(*------------------------------------------------------------*)
let make_actions_per_automaton index_of_actions index_of_automata automata =
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
  actions_per_automaton


(*------------------------------------------------------------*)
(* Get all the locations for every automaton *)
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
(* Get all the possible actions for every location of every automaton *)
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
(* Get the automata for every action *)
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
(* Convert updates *)
(*------------------------------------------------------------*)

(** Checks if a update is a normal update *)
let is_normal_update = function
  | Normal _ -> true
  | _ -> false

(** Returns the value of a normal update *)
let get_normal_update_value = function
  | Normal u -> u
  | _ -> assert false

(** Returns the value of a conditonal update *)
let get_conditional_update_value = function
  | Condition u -> u
  | _ -> assert false




(* Filter the updates that should assign some variable name to be removed to any expression *)
let filter_updates removed_variable_names updates =
  let not_removed_variable (parsed_update_type, _) =
    let variable_name_opt = ParsingStructureUtilities.variable_name_of_parsed_update_type_opt parsed_update_type in
    match variable_name_opt with
    | Some variable_name ->
        not (List.mem variable_name removed_variable_names)
    | None -> true
  in
  List.fold_left (fun acc u ->
      match u with
      | Normal update ->
        if (not_removed_variable update) then u::acc else acc
      | Condition (bool, updates_if, updates_else) ->
        let filtered_if = List.filter (not_removed_variable) updates_if in
        let filtered_else = List.filter (not_removed_variable) updates_else in
        Condition (bool, filtered_if, filtered_else)::acc
    ) [] updates


(** Translate a parsed clock update into its abstract model *)
let to_abstract_clock_update variable_infos only_resets updates_list =

    (** Translate parsed clock update into the tuple clock_index, linear_term *)
    let to_intermediate_abstract_clock_update clock_update =

        let parsed_update_type, update_expr = clock_update in

        (* Check that clock update is a linear expression *)
        let is_linear = ParsingStructureUtilities.is_linear_parsed_global_expression variable_infos update_expr in
        if not is_linear then
            raise (InvalidExpression (
                "Clock update `"
                ^ ParsingStructureUtilities.string_of_parsed_normal_update variable_infos clock_update
                ^ "` is not a linear expression. A linear expression is expected for clock update."
            ));

        let variable_name_opt = ParsingStructureUtilities.variable_name_of_parsed_update_type_opt parsed_update_type in
        match variable_name_opt with
        | Some variable_name ->
            let variable_index = index_of_variable_name variable_infos variable_name in
            let _, converted_update = DiscreteExpressionConverter.convert_continuous_update variable_infos parsed_update_type update_expr in
            (variable_index, converted_update)
        (* TODO benjamin REFACTOR it never can happen, should pass variable_name, update_expr instead of parsed_update_variable_type, update_expr *)
        | None -> raise (InternalError "Try to convert a unit expression value to a rational-valued clock.")
    in

    let converted_clock_updates = List.map to_intermediate_abstract_clock_update updates_list in

    (* Differentiate between different kinds of clock updates *)
    let clock_updates : clock_updates =
    (* Case 1: no update *)
    if converted_clock_updates = [] then
        No_update
    else (
        (* Case 2: resets only *)
        if only_resets then (
            (* Keep only the clock ids, not the linear terms *)
            let clocks_to_reset, _ = List.split converted_clock_updates in
            Resets (List.rev clocks_to_reset)
        ) else
            (* Case 3: complex with linear terms *)
            Updates (List.rev converted_clock_updates)
    )
    in

    (** abstract clock updates *)
    clock_updates

(* Check if there is only resets in an update list *)
let is_only_resets updates =
    List.for_all (fun (_, update) ->
        ParsingStructureUtilities.exists_in_parsed_global_expression (function
            | Leaf_variable _
            | Leaf_fun _ -> false
            | Leaf_constant value -> DiscreteValue.is_zero value
        ) update
    ) updates

(** Split normal updates into clock, discrete updates *)
let split_to_clock_discrete_updates variable_infos updates =

    (** Function that check if a normal update is a clock update *)
    let is_clock_update (parsed_update_type, _) =

        (* Get updated variable name *)
        let variable_name_opt = ParsingStructureUtilities.variable_name_of_parsed_update_type_opt parsed_update_type in

        match variable_name_opt with
        | Some variable_name ->
            (* Retrieve variable type *)
            variable_infos.type_of_variables (index_of_variable_name variable_infos variable_name) = DiscreteType.Var_type_clock
        (* Unit update, so it's not a clock *)
        | None -> false
    in

    List.partition is_clock_update updates


(** Translate a normal parsed update into its abstract model *)
let convert_normal_updates variable_infos updates_type updates_list =

	(* Flag to check if there are clock resets only to 0 *)
    let only_resets = is_only_resets updates_list in

	(** Split clocks and discrete updates *)
	let parsed_clock_updates, parsed_discrete_updates = split_to_clock_discrete_updates variable_infos updates_list in

    (* Check that pre and post updates not updating clocks ! It's only for discrete variables *)
    (match updates_type with
    | Parsed_seq_updates when List.length parsed_clock_updates > 0 ->
        print_error "`do` bloc is reserved for sequential updates on discrete variables. This bloc cannot be used for updating clock(s).";
        raise InvalidModel
    | _ -> ()
    );

    (* Convert discrete udpates *)
    let converted_discrete_updates = List.map (fun (parsed_update_type, expr) -> DiscreteExpressionConverter.convert_update variable_infos updates_type parsed_update_type expr) parsed_discrete_updates in
    (* Convert continuous udpates *)
    let converted_clock_updates = to_abstract_clock_update variable_infos only_resets parsed_clock_updates in

	(** update abstract model *)
	{
		clock = converted_clock_updates;
		discrete = converted_discrete_updates;
		conditional = [];
	}


(** convert normal and conditional updates *)
let convert_updates variable_infos updates_type updates : updates =

    (** split normal and conditional updates *)
    let normal_updates, conditional_updates = List.partition is_normal_update updates in

    (** convert normal parsed updates *)
    let converted_updates = convert_normal_updates variable_infos updates_type (List.map get_normal_update_value normal_updates) in

    (** convert normal parsed updates inside conditional updates *)
    let conditional_updates_values : conditional_update list = List.map (fun u ->
        let boolean_value, if_updates, else_updates = get_conditional_update_value u in

        let convert_boolean_expr = DiscreteExpressionConverter.convert_conditional variable_infos boolean_value in

        let convert_if_updates = convert_normal_updates variable_infos updates_type if_updates in
        let convert_else_updates = convert_normal_updates variable_infos updates_type  else_updates in

        (convert_boolean_expr, convert_if_updates, convert_else_updates)
    ) conditional_updates in

    (** updates abstract model *)
    { converted_updates with conditional = conditional_updates_values }


(*------------------------------------------------------------*)
(* Convert the transitions *)
(*------------------------------------------------------------*)
(* Convert the structure: 'automaton_index -> location_index -> list of (action_index, guard, resets, target_state)'
	into a structure:
	'automaton_index -> location_index -> action_index -> list of (transition_index)'
	and creates a structure transition_index -> (guard, action_index, resets, target_state)
	and creates a structure transition_index -> automaton_index
*)
let convert_transitions nb_transitions nb_actions (useful_parsing_model_information : useful_parsing_model_information) transitions
	: (((AbstractModel.transition_index list) array) array) array * (AbstractModel.transition array) * (Automaton.automaton_index array)
	=
  (* Extract values from model parsing info *)
  let index_of_variables, constants, removed_variable_names, type_of_variables =
    useful_parsing_model_information.variable_infos.index_of_variables, useful_parsing_model_information.variable_infos.constants,
    useful_parsing_model_information.variable_infos.removed_variable_names, useful_parsing_model_information.variable_infos.type_of_variables in

    let variable_infos = useful_parsing_model_information.variable_infos in

  (* Create the empty array of transitions automaton_index -> location_index -> action_index -> list of (transition_index) *)
  
  (*** NOTE/TODO: why (Array.length transitions) ?! ***)
  
  let array_of_transitions : (((AbstractModel.transition_index list) array) array) array = Array.make (Array.length transitions) (Array.make 0 (Array.make 0 [])) in
  (* Create the empty array transition_index -> transition *)
  let dummy_transition = {
	guard		= True_guard;
	action		= -1;
	seq_updates	= { clock = No_update; discrete = [] ; conditional = []};
	updates		= { clock = No_update; discrete = [] ; conditional = []};
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
          List.iter (fun (action_index, guard, update_section, target_location_index) ->

              (* Convert the guard *)
              let converted_guard = DiscreteExpressionConverter.convert_guard variable_infos guard in

              (* Filter the updates that should assign some variable name to be removed to any expression *)
              (* let filtered_updates = List.filter (fun (variable_name, (*linear_expression*)_) ->
                 					not (List.mem variable_name removed_variable_names)
                 				) updates
                 				in *)
              let seq_updates, updates = update_section in

              let filtered_seq_updates = filter_updates removed_variable_names seq_updates in
              let filtered_updates = filter_updates removed_variable_names updates in

              (* Flag to check if there are clock resets only to 0 *)
              (* let only_resets = ref true in *)

              (* Split between the clock and discrete updates *)
              (* let clock_updates, discrete_updates = List.partition (fun (variable_index, linear_term) ->
                 					if type_of_variables variable_index = Var_type_clock then(
                 						(* Update flag *)
                 						if linear_term <> (LinearConstraint.make_pxd_linear_term [] NumConst.zero) then(
                 							only_resets := false;
                 						);
                 						true
                 					)else
                 						false
                 				) converted_updates
                 				in *)

              (* Split between the clock and discrete updates *)
              (* let parsed_clock_updates, parsed_discrete_updates = List.partition (is_clock_update index_of_variables only_resets) filtered_updates
                 				in *)

              (* translate parsed updates into their abstract model *)
              let converted_seq_updates = convert_updates variable_infos Parsed_seq_updates filtered_seq_updates in
              let converted_updates = convert_updates variable_infos Parsed_std_updates filtered_updates in

              (* Convert the updates *)
              (* let converted_updates = List.map (fun (variable_name, parsed_update_arithmetic_expression) ->
                 					let variable_index = Hashtbl.find index_of_variables variable_name in
                 					let linear_term = linear_term_of_parsed_update_arithmetic_expression index_of_variables constants parsed_update_arithmetic_expression in
                 					(variable_index, linear_term)
                 				) filtered_updates in*)

              (* Differentiate between different kinds of clock updates *)
              (* let clock_updates : clock_updates =
                 					(* Case 1: no update *)
                 					if converted_clock_updates = [] then No_update
                 					else (
                 						(* Case 2: resets only *)
                 						if !only_resets then (
                 							(* Keep only the clock ids, not the linear terms *)
                 							let clocks_to_reset, _ = List.split converted_clock_updates in
                 							Resets clocks_to_reset
                 						) else
                 						(* Case 3: complex with linear terms *)
                 							Updates converted_clock_updates
                 					)
                 				in *)

              (* Update the transition array *)
              array_of_transitions.(automaton_index).(location_index).(action_index) <- !transition_index :: array_of_transitions.(automaton_index).(location_index).(action_index);
              
              (* Add the transition to the description *)
              transitions_description.(!transition_index) <- {
					guard   = converted_guard;
					action  = action_index;
					seq_updates = converted_seq_updates;
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
(* Create the initial state *)
(*------------------------------------------------------------*)
let make_initial_state variable_infos index_of_automata locations_per_automaton index_of_locations index_of_variables parameters removed_variable_names constants type_of_variables variable_names init_discrete_pairs init_definition =
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
	let initial_location = Location.make_location locations init_discrete_pairs in

	(* Remove the init definitions for discrete variables *)
	let other_inequalities = List.filter (function
		(* Check if the left part is only a variable name *)
		| Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , _)) ->
			(* First check whether it was removed *)
			if List.mem variable_name removed_variable_names then false
			else
			let is_discrete =
				(* Try to get the variable index *)
				if (Hashtbl.mem index_of_variables variable_name) then (
				let variable_index =  Hashtbl.find index_of_variables variable_name in
				(* Keep if this is a discrete *)
				DiscreteType.is_discrete_type (type_of_variables variable_index)
				) else (
				(* Case constant *)
				if (Hashtbl.mem constants variable_name) then false
				else (
					(* Otherwise: problem! *)
					raise (InternalError ("The variable `" ^ variable_name ^ "` mentioned in the init definition does not exist, although this should have been checked before."));
				))
			in not is_discrete
        (* Doesn't care about discrete boolean inits for constraint initalization ! *)
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
		(* 		let discrete_values = List.map (fun discrete_index -> discrete_index, (Location.get_discrete_value initial_location discrete_index)) model.discrete in *)

        (* Get only rational discrete for constraint encoding *)
        let init_discrete_rational_pairs = List.filter (fun (discrete_index, discrete_value) -> DiscreteValue.is_rational_value discrete_value) init_discrete_pairs in
        (* map to num const *)
        let init_discrete_rational_numconst_pairs = List.map (fun (discrete_index, discrete_value) -> discrete_index, DiscreteValue.numconst_value discrete_value) init_discrete_rational_pairs in

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
(** Getting variables *)
(************************************************************)

(*------------------------------------------------------------*)
(* Gather the set of all variable names used in a parsed reference valuation *)
(*------------------------------------------------------------*)
let get_variables_in_parsed_pval (parsed_pval : ParsingStructure.parsed_pval) : variable_name list =
	(* Return the left part of all pairs *)
	let left, _ = List.split parsed_pval in
	left

(*------------------------------------------------------------*)
(* Gather the set of all variable names used in a parsed reference valuation *)
(*------------------------------------------------------------*)
let get_variables_in_parsed_hyper_rectangle (parsed_hyper_rectangle : ParsingStructure.parsed_pdomain) : variable_name list =
	(* Return the left part of all triples *)
	List.map (fun (parameter_name, _, _) -> parameter_name) parsed_hyper_rectangle




(*------------------------------------------------------------*)
(* Gather the set of all variable names used in the parsed property *)
(*------------------------------------------------------------*)

let all_variables_in_property_option (parsed_property_option : ParsingStructure.parsed_property option) =
	(* First create the set *)
	let variables_used_ref = ref StringSet.empty in
	
	(* Gather variables to the set, passed by reference *)
	begin
	match parsed_property_option with
	| None -> ()
	| Some parsed_property ->
		begin
		match parsed_property.property with
	
		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Parsed_EF parsed_state_predicate
		(* Safety *)
		| Parsed_AGnot parsed_state_predicate
			-> ParsingStructureUtilities.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate
			
		
		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)
		
		(* Reachability with minimization of a parameter valuation *)
		| Parsed_EFpmin (parsed_state_predicate , parameter_name)
		| Parsed_EFpmax (parsed_state_predicate , parameter_name)
			(* First get the variables in the state predicate *)
			-> ParsingStructureUtilities.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate;
			(* Then add the parameter name *)
			variables_used_ref := StringSet.add parameter_name !variables_used_ref
		
		(* Reachability with minimal-time *)
		| Parsed_EFtmin parsed_state_predicate
			-> ParsingStructureUtilities.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate

		
		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)
		
		(** Accepting infinite-run (cycle) through a state predicate *)
		| Parsed_Cycle_Through parsed_state_predicate
			-> ParsingStructureUtilities.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate

		(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
		| Parsed_Cycle_Through_generalized parsed_state_predicate_list
			-> List.iter (ParsingStructureUtilities.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref) parsed_state_predicate_list

		(** Infinite-run (cycle) with non-Zeno assumption *)
		| Parsed_NZ_Cycle -> ()
		

		(*------------------------------------------------------------*)
		(* Deadlock-freeness *)
		(*------------------------------------------------------------*)
		
		(* Deadlock-free synthesis *)
		| Parsed_Deadlock_Freeness -> ()
		
		
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
			variables_used_ref := StringSet.of_list (get_variables_in_parsed_pval parsed_pval);
		
		(* Non-complete, non-deterministic inverse method with convex result *)
		| Parsed_PRP (parsed_state_predicate , parsed_pval) ->
			(* First get the variables in the state predicate *)
			ParsingStructureUtilities.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate;
			(* Then add the pval *)
			variables_used_ref := StringSet.union !variables_used_ref (StringSet.of_list (get_variables_in_parsed_pval parsed_pval))
		
		
		(*------------------------------------------------------------*)
		(* Cartography algorithms *)
		(*------------------------------------------------------------*)
		
		(* Cartography *)
		| Parsed_Cover_cartography (parsed_hyper_rectangle, _)
		(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
		| Parsed_Shuffle_cartography (parsed_hyper_rectangle, _)
		(** Look for the border using the cartography*)
		| Parsed_Border_cartography (parsed_hyper_rectangle, _)
		(** Randomly pick up values for a given number of iterations *)
		| Parsed_Random_cartography (parsed_hyper_rectangle, _, _)
		(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
		| Parsed_RandomSeq_cartography (parsed_hyper_rectangle, _, _)
			->
			variables_used_ref := StringSet.of_list (get_variables_in_parsed_hyper_rectangle parsed_hyper_rectangle);
		

		(** Cover the whole cartography using learning-based abstractions *)
		| Parsed_Learning_cartography (parsed_state_predicate, parsed_hyper_rectangle, _)
		(* Parametric reachability preservation *)
		| Parsed_PRPC (parsed_state_predicate, parsed_hyper_rectangle, _)
			->
			(* First get the variables in the state predicate *)
			ParsingStructureUtilities.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate;
			(* Then add the HyperRectangle *)
			variables_used_ref := StringSet.union !variables_used_ref (StringSet.of_list (get_variables_in_parsed_hyper_rectangle parsed_hyper_rectangle));


		(*------------------------------------------------------------*)
		(* Observer patterns *)
		(*------------------------------------------------------------*)
		(* if a2 then a1 has happened before *)
		| Parsed_pattern (Parsed_action_precedence_acyclic _)
		(* everytime a2 then a1 has happened before *)
		| Parsed_pattern (Parsed_action_precedence_cyclic _)
		(* everytime a2 then a1 has happened once before *)
		| Parsed_pattern (Parsed_action_precedence_cyclicstrict _)
			-> ()

		(* a within d *)
		| Parsed_pattern (Parsed_action_deadline (_ , duration)) ->
(*			get_variables_in_linear_expression variables_used_ref duration*)
            variables_used_ref := ParsingStructureUtilities.get_variables_in_linear_expression duration


		(* if a2 then a1 happened within d before *)
		| Parsed_pattern (Parsed_TB_Action_precedence_acyclic ((*sync_name*)_, (*sync_name*)_, duration))
		(* everytime a2 then a1 happened within d before *)
		| Parsed_pattern (Parsed_TB_Action_precedence_cyclic ((*sync_name*)_, (*sync_name*)_, duration))
		(* everytime a2 then a1 happened once within d before *)
		| Parsed_pattern (Parsed_TB_Action_precedence_cyclicstrict ((*sync_name*)_, (*sync_name*)_, duration)) ->
(*			get_variables_in_linear_expression variables_used_ref duration*)
				variables_used_ref := ParsingStructureUtilities.get_variables_in_linear_expression duration

		(* if a1 then eventually a2 within d *)
		| Parsed_pattern (Parsed_TB_response_acyclic (_, _, parsed_duration))
		(* everytime a1 then eventually a2 within d *)
		| Parsed_pattern (Parsed_TB_response_cyclic (_, _, parsed_duration))
		(* everytime a1 then eventually a2 within d once before next *)
		| Parsed_pattern (Parsed_TB_response_cyclicstrict (_, _, parsed_duration)) ->
(*		    get_variables_in_linear_expression variables_used_ref parsed_duration*)
            variables_used_ref := ParsingStructureUtilities.get_variables_in_linear_expression parsed_duration;

		(* sequence a1, …, an *)
		| Parsed_pattern (Parsed_Sequence_acyclic _)
		(* always sequence a1, …, an *)
		| Parsed_pattern (Parsed_Sequence_cyclic _)
			-> ()

	
		end;
	end;
	(* Return the set *)
	!variables_used_ref


    
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
    ParsingStructureUtilities.all_variable_in_parsed_state_predicate
        parsing_infos
        variable_infos
        (* Undefined variable name callback, triggered if an undefined variable is found *)
        (Some (fun variable_name -> print_error ("Undefined variable name `" ^ variable_name ^ "` in the property")))
        (* Undefined automaton name callback, triggered if an undefined automaton is found *)
        (Some (fun automaton_name -> print_error ("Unknown automaton name `" ^ automaton_name ^ "` in the property.")))
        (* Undefined location name callback, triggered if an undefined location is found *)
        (Some (fun automaton_name loc_name -> print_error ("Unknown location name `" ^ loc_name ^ "` in automaton `" ^ automaton_name ^ "` in the property.")))
        expr

(*------------------------------------------------------------*)
(** Generic function checking whether a name is a valid parameter name *)
(*------------------------------------------------------------*)
let check_parameter_name suffix_explanation_string variable_infos parameter_name =
	(* First check it is a variable *)
	if not (is_variable_is_defined variable_infos parameter_name) then(
		print_error ("Parameter " ^ parameter_name ^ " is not a defined variable" ^ suffix_explanation_string);
		false
	) else(
		let parameter_index = index_of_variable_name variable_infos parameter_name in
		if not(variable_infos.type_of_variables parameter_index = DiscreteType.Var_type_parameter) then(
			print_error ("Variable " ^ parameter_name ^ " is not a parameter" ^ suffix_explanation_string);
			false
		)else true
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
(* Check the parsed_pval w.r.t. the model parameters *)
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
(* Check the parsed_pval w.r.t. the model parameters *)
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
(* Check the correctness property declaration       *)
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
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Parsed_EF parsed_state_predicate
		
		(* Safety *)
		| Parsed_AGnot parsed_state_predicate
			->
			check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
		
		
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
		
		(** Accepting infinite-run (cycle) through a state predicate *)
		| Parsed_Cycle_Through parsed_state_predicate ->
			check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
		
		(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
		| Parsed_Cycle_Through_generalized parsed_state_predicate_list ->
			(* Do a fold_left to check everything even in case of failure *)
			List.fold_left (fun current_result parsed_state_predicate ->
				(* Make sure we do evaluate this part even if current_result is false *)
				let check = check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate in
				current_result && check
				) true parsed_state_predicate_list

		(** Infinite-run (cycle) with non-Zeno assumption *)
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
		(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
		| Parsed_Shuffle_cartography (parsed_hyper_rectangle, step)
		(** Look for the border using the cartography*)
		| Parsed_Border_cartography (parsed_hyper_rectangle, step)
		(** Randomly pick up values for a given number of iterations *)
		| Parsed_Random_cartography (parsed_hyper_rectangle, _, step)
		(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
		| Parsed_RandomSeq_cartography (parsed_hyper_rectangle, _, step)
			->
			evaluate_and
				(* The step has to be > 0 *)
				(NumConst.g step NumConst.zero)
				(check_parsed_hyper_rectangle useful_parsing_model_information parsed_hyper_rectangle)
	
		(** Cover the whole cartography using learning-based abstractions *)
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
			let check2 = ParsingStructureUtilities.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message d in
			let check3 = (if ParsingStructureUtilities.no_variables_in_linear_expression variable_infos d
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
			let check3 = ParsingStructureUtilities.all_variables_defined_in_linear_expression variable_infos undeclared_variable_in_linear_constraint_message d in
			let check4 = (if ParsingStructureUtilities.no_variables_in_linear_expression variable_infos d
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

		end



(************************************************************)
(** Converting the property  *)
(************************************************************)


(*------------------------------------------------------------*)
(* Convert the parsed parsed_pval into a valid parsed_pval *)
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
(* Convert the parsed hyper_rectangle into a valid hyper_rectangle *)
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
		
		(** Accepting infinite-run (cycle) through a state predicate *)
		| Parsed_Cycle_Through parsed_state_predicate ->
			Cycle_through (PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
		
		(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
		| Parsed_Cycle_Through_generalized parsed_state_predicate_list ->
			Cycle_through_generalized (List.map (PropertyConverter.convert_state_predicate useful_parsing_model_information) parsed_state_predicate_list)
			,
			None

		
		(** Infinite-run (cycle) with non-Zeno assumption *)
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
		
		(** Cover the whole cartography using learning-based abstractions *)
		| Parsed_Learning_cartography (parsed_state_predicate, parsed_hyper_rectangle, step) ->
			Learning_cartography ((PropertyConverter.convert_state_predicate useful_parsing_model_information parsed_state_predicate , convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , step))
			,
			None
		
		(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
		| Parsed_Shuffle_cartography (parsed_hyper_rectangle, step) ->
			Shuffle_cartography (convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , step)
			,
			None
		
		(** Look for the border using the cartography*)
		| Parsed_Border_cartography (parsed_hyper_rectangle, step) ->
			Border_cartography (convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , step)
			,
			None
		
		(** Randomly pick up values for a given number of iterations *)
		| Parsed_Random_cartography (parsed_hyper_rectangle, nb, step) ->
			Random_cartography (convert_parsed_hyper_rectangle variable_infos parsed_hyper_rectangle , nb , step)
			,
			None
		
		(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
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
	
		in
		
		(* Get the synthesis or emptiness type *)
		let synthesis_type = convert_synthesis_type parsed_property.synthesis_type in
		
		(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Convert the projection definition *)
		(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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
(* Convert the parsed model and the parsed property into an abstract model and an abstract property *)
(*------------------------------------------------------------*)
let abstract_structures_of_parsing_structures options (parsed_model : ParsingStructure.parsed_model) (parsed_property_option : ParsingStructure.parsed_property option) : AbstractModel.abstract_model * (AbstractProperty.abstract_property option) =
	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug functions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug print function for arrays *)
	let debug_print_array verbose_level =
		Array.iteri (fun i e ->
			print_message verbose_level ((string_of_int i) ^ " -> " ^ e)
		)
	in

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get names *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the synclabs declarations *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let action_names, removed_action_names = if options#sync_auto_detection then action_names, [] else (
		(* Keep only the synclabs which are used in ALL the automata where they are declared *)
		List.partition (synclab_used_everywhere parsed_model.automata) (*(fun synclab_name -> if synclab_used_everywhere parsed_model.automata synclab_name then
			(* If it is used everywhere: keep *)
			true
			(* If there exists an automaton where it is not used : warns and remove *)
			else (print_warning ("The synclab `" ^ synclab_name ^ "` is not used in some of the automata where it is declared: it will thus be removed."); false)
		)*) action_names
	) in


    (* Create an hash table that will contain previously initialized constants *)
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
        functions = Hashtbl.create 0;
        type_of_variables = fun i -> DiscreteType.Var_type_discrete (DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rat);
    }
    in

    (* Evaluate the constants init expressions *)
    let evaluated_constants = List.map (fun (name, expr, var_type) ->

        (* Create variable infos containing only initialized constants *)
        let current_variable_infos = { variable_infos with constants = initialized_constants } in
        (* Check all constants used are defined *)
        let all_variable_defined = ParsingStructureUtilities.all_variables_defined_in_parsed_global_expression_without_callback current_variable_infos expr in
        if not all_variable_defined then (
            print_error (
                "Expression \""
                ^ ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr
                ^ "\" use undeclared variable or constant"
            );
            raise (InvalidModel);
        );


        (* TYPE CHECKING *)
        let constant = name, expr, var_type in

        let typed_expr(*, expr_type *) = DiscreteExpressionConverter.convert_discrete_constant initialized_constants constant in
        let value = DiscreteExpressionEvaluator.try_eval_constant_global_expression typed_expr in
        (* Add evaluated constant to hash table *)
        Hashtbl.add initialized_constants name value;
        (* Return *)
        name, value(*, expr_type *)
    ) (List.rev constants) in



    (* TYPE CHECKING : CONSTANTS IN DECLARATION *)
    let constant_tuples = evaluated_constants in

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Make the array of constants *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let (constants : (Automaton.variable_name , DiscreteValue.discrete_value) Hashtbl.t), constants_consistent = make_constants constant_tuples in



	if verbose_mode_greater Verbose_high then(
		(* Constants *)
		print_message Verbose_high ("\n*** Constants:");
		Hashtbl.iter (fun key value ->
			print_message Verbose_high (key ^ " = " ^ (DiscreteValue.string_of_value value) ^ "")
		) constants;
	);



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the variable_declarations *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that all variable names are different (and print warnings for multiply-defined variables if same type) *)
	let all_variables_different = check_variable_names possibly_multiply_defined_clock_names possibly_multiply_defined_discrete_names possibly_multiply_defined_parameter_names constants in
	(* Check that all automata names are different *)
	let all_automata_different = check_declared_automata_names declared_automata_names in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Errors if unassigned constants *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Exit if not well formed *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that at least one automaton is defined *)
	let at_least_one_automaton =
		if List.length declared_automata_names = 0 then (
			print_error ("At least one automaton should be declared."); false
		) else true in

	(* Stop here if model not well formed *)
 	if not (constants_consistent && all_variables_different && all_automata_different && at_least_one_automaton) then raise InvalidModel;
 	
 	
 	
 	
 	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add clock and automaton for the observer *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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


	

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Start building variable lists *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* First remove multiply defined variable names *)
	let single_clock_names = list_only_once possibly_multiply_defined_clock_names in
	let single_discrete_names = list_only_once possibly_multiply_defined_discrete_names in
	let single_parameter_names = list_only_once possibly_multiply_defined_parameter_names in
	let single_discrete_names_by_type = list_only_once possibly_multiply_defined_discrete_names_by_type in

	(*------------------------------------------------------------*)
	(* Resolve dependencies between variables and functions *)
	(*------------------------------------------------------------*)

    (* Resolve dependency graph of the model *)
    let dependency_graph = ParsedModelMetadata.dependency_graph parsed_model in
    (* Get dependency graph as dot format *)
    let str_dependency_graph = ParsedModelMetadata.string_of_dependency_graph dependency_graph in
    (* Print dependency graph *)
    ImitatorUtilities.print_message Verbose_high str_dependency_graph;

    (* Get unused components and print warnings *)
    let unused_components = ParsedModelMetadata.unused_components_of_model dependency_graph in

    (* Iter on unused components and print warnings *)
    ParsedModelMetadata.ComponentSet.iter (function
        | Fun_ref function_name ->
            print_warning ("Function `" ^ function_name ^ "` is declared but never used in the model.")
        | Local_variable_ref (variable_name, function_name, _) ->
            print_warning ("Local variable `" ^ variable_name ^ "` in `" ^ function_name ^ "` is declared but never used.")
        | Param_ref (param_name, function_name) ->
            print_warning ("Parameter `" ^ param_name ^ "` in `" ^ function_name ^ "` is declared but never used.")
        | _ -> ()
    ) unused_components;


	(*------------------------------------------------------------*)
	(* Remove unused variables *)
	(*------------------------------------------------------------*)

	(* Unless a specific option is activated, we first remove all variables declared but unused *)
	let clock_names, discrete_names, parameter_names, discrete_names_by_type, removed_variable_names =
	if options#no_variable_autoremove then(
		(* Nothing to do *)
		single_clock_names, single_discrete_names, single_parameter_names, single_discrete_names_by_type, []
	)else (

		(* Gather all variables used *)
		let all_variables_used_in_model = ParsedModelMetadata.used_variables_of_model dependency_graph in
		let all_variables_used_in_property = all_variables_in_property_option parsed_property_option in
		let all_variable_used = StringSet.union all_variables_used_in_model all_variables_used_in_property in

		(* Remove variable unused *)
		let remove_unused_variables_gen variable_type_name = List.partition (fun variable_name ->
			(* The variable is kept if… *)
			if
				(* Either it is used somewhere *)
				(StringSet.mem variable_name all_variable_used)
				(* Or it is a clock with the special global_time name *)
				||
				(variable_name = Constants.global_time_clock_name && List.mem variable_name single_clock_names)
			then true
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
		let single_discrete_names_by_type = List.filter (fun (var_type, variable_name) -> List.mem variable_name single_discrete_names) single_discrete_names_by_type in
		(* Return and append removed variable names *)
		let removed_variable_names = List.rev_append removed_clock_names (List.rev_append removed_discrete_names removed_parameter_names) in
		single_clock_names, single_discrete_names, single_parameter_names, single_discrete_names_by_type, removed_variable_names
	)
	in

    (* Group variable names by types *)
	let discrete_names_by_type_group = OCamlUtilities.group_by_and_map (fun (var_type, var_name) -> var_type) (fun (var_type, var_name) -> var_name) discrete_names_by_type in

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
	let discrete_names = discrete_names in

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
	let nb_parameters	    = List.length parameter_names in
	let nb_variables	    = List.length variable_names in

	(* Compute the index for the observer automaton *)
	let observer_automaton_index_option = match observer_automaton with
		| None -> None
		| Some _ ->
			(* Print some information *)
			print_message Verbose_high ("\nObserver automaton index is: " ^ (string_of_int (nb_automata - 1)));
			
			Some (nb_automata - 1)
	in



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the LinearConstraint dimensions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Print some information *)
	print_message Verbose_high ("\nSetting dimensions…");
	LinearConstraint.set_dimensions nb_parameters nb_clocks nb_discrete;
	
	
	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the parameter dimensions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*** NOTE: must be done one and exactly one time ***)

	(* Set dimensions for hyper rectangles *)
	HyperRectangle.set_dimensions nb_parameters;

	(* Set dimensions for parameter valuations *)
	PVal.set_dimensions nb_parameters;




	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the arrays of automata, variables and actions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

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
    print_message Verbose_high ("\nSetting discretes variables types…");

	for i = first_discrete_index to nb_variables - 1 do
	    (* Get specific var_type of discrete variable *)
	    (* Remove offset, because array of var_type * variable_name for discrete start from 0 *)
        let var_type, v = List.nth discrete_names_by_type (i - first_discrete_index) in
        (* Convert var_type from ParsingStructure to AbstractModel *)
		type_of_variables.(i) <- var_type;
        (* Print type infos *)
		print_message Verbose_high ("variable " ^ v ^ " : " ^ (DiscreteType.string_of_var_type type_of_variables.(i)))
	done;

	(* Functional representation *)
	let type_of_variables = fun variable_index -> type_of_variables.(variable_index) in

	(* Create the lists of different variables *)
	let (parameters : parameter_index list)	= list_of_interval first_parameter_index (first_clock_index - 1) in
	let (clocks : clock_index list)			= list_of_interval first_clock_index (first_discrete_index - 1) in
	let (discrete : discrete_index list)	= list_of_interval first_discrete_index (nb_variables - 1) in


	(* Create the type check functions *)
	let is_clock = (fun variable_index -> try (type_of_variables variable_index = DiscreteType.Var_type_clock) with Invalid_argument _ ->  false) in
	let is_discrete = (fun variable_index -> try (DiscreteType.is_discrete_type (type_of_variables variable_index)) with Invalid_argument _ ->  false) in


	(* Detect the clock with a special global time name, if any *)
	let global_time_clock =
		if List.mem Constants.global_time_clock_name clock_names then Some (Hashtbl.find index_of_variables Constants.global_time_clock_name)
		else None
	in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug prints *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get all the locations *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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

    (* Get builtin functions metadata *)
    let builtin_functions_metadata = Functions.builtin_functions in
    (* Get user functions metadata from parsed functions *)
    let used_function_names = ParsedModelMetadata.used_functions_of_model dependency_graph in
    (* Get only used user functions definition *)
    let used_function_definitions = List.filter (fun (fun_def : parsed_fun_definition) -> StringSet.mem fun_def.name used_function_names) parsed_model.fun_definitions in
    (* Get metadata of these functions *)
    let user_functions_metadata = List.map Functions.metadata_of_function_definition used_function_definitions in
    (* Concat builtin & user functions *)
    let all_functions_metadata = user_functions_metadata @ builtin_functions_metadata in
    (* Create function table that associate function name to function metadata *)
    let functions_metadata_table = (List.map (fun (fun_def : ParsingStructure.function_metadata) -> fun_def.name, fun_def) all_functions_metadata) |> List.to_seq |> Hashtbl.of_seq in

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create useful parsing structure, used in subsequent functions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	let variable_infos = {
        constants					= constants;
        discrete					= discrete;
        index_of_variables			= index_of_variables;
        type_of_variables			= type_of_variables;
        variables					= variables;
        variable_names				= variable_names;
        removed_variable_names		= removed_variable_names;
        functions                   = functions_metadata_table;
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

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the user function definitions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	print_message Verbose_high ("*** Checking user functions definitions…");

    (* Check user functions are well formed *)
	let well_formed_user_functions_list = List.map (check_fun_definition variable_infos) parsed_model.fun_definitions in
	let well_formed_user_functions = List.for_all identity well_formed_user_functions_list in

    if not well_formed_user_functions then
        raise InvalidModel;

    (* Convert (only used) function definition from parsing structure to abstract model into sequence of tuple (name * fun_def) *)
    List.iter (fun (parsed_fun_def : parsed_fun_definition) ->
        (* Convert fun def from parsing structure to abstract model *)
        let fun_def = DiscreteExpressionConverter.convert_fun_definition variable_infos parsed_fun_def in
        (* Add fun def to functions table *)
        Hashtbl.add Functions.fun_definitions_table fun_def.name fun_def
    ) used_function_definitions;

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the automata *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("*** Checking automata…");
	let well_formed_automata = check_automata useful_parsing_model_information parsed_model.automata in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* exit if not well formed *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: might be a problem in the init checking if the check_automata test fails, hence test first ***)
	if not (check_no_unassigned_constants && well_formed_automata)
		then raise InvalidModel;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the init_definition *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("*** Checking init definition…");
	(* Get pairs for the initialisation of the discrete variables, and check the init definition *)

	let init_discrete_pairs, well_formed_init = check_init useful_parsing_model_information parsed_model.init_definition observer_automaton_index_option in

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the constants inits *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
    print_message Verbose_high ("*** Checking constant inits…");

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check projection definition *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let well_formed_projection = match parsed_property_option with
		| None -> true
		| Some parsed_property -> check_projection_definition variable_infos parsed_property.projection
	in



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* exit if not well formed *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	if not (well_formed_projection && well_formed_init)
		then raise InvalidModel;

	print_message Verbose_medium ("Model syntax successfully checked.");
	

	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the property *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	if not (check_property_option useful_parsing_model_information parsed_property_option)
		then raise InvalidProperty;

	print_message Verbose_medium ("Property syntax successfully checked.");
	

	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the automata without the observer, and with the transitions in a non-finalized form *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("*** Building automata…");
	(* Get all the possible actions for every location of every automaton *)
	let actions, array_of_action_names, action_types, actions_per_automaton, actions_per_location, location_acceptance, location_urgency, costs, invariants, stopwatches_array, has_non_1rate_clocks, flow_array, transitions, observer_nosync_index_option = make_automata useful_parsing_model_information parsed_model.automata (observer_automaton_index_option <> None) in
	
	let nb_actions = List.length actions in
	
	(* Print some information *)
	print_message Verbose_high ("The model contains " ^ (string_of_int nb_actions) ^ " action" ^ (s_of_int nb_actions) ^ ".");


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create the abstract property from the parsed property *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
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
	
	



	

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Convert the transitions to their final form *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
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
	let transitions, transitions_description, automaton_of_transition = convert_transitions nb_transitions nb_actions useful_parsing_model_information transitions in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add the observer structure to the automata *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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









	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Convert to functional view *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

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


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the number of discrete variables *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let min_discrete_index = first_discrete_index in
	let max_discrete_index = nb_variables - 1 in
	Location.initialize nb_automata min_discrete_index max_discrete_index;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the automata per action *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* List of automata for every action *)
	print_message Verbose_high ("*** Building automata per action…");
	let automata_per_action = make_automata_per_action actions_per_automaton nb_automata nb_actions in

(*	(* Convert the costs *)
	print_message Verbose_total ("*** Building costs (if any)…");
	let costs = convert_costs index_of_variables constants costs in*)

(*	(* Convert the invariants *)
	print_message Verbose_total ("*** Building invariants…");
	let invariants = convert_invariants index_of_variables constants invariants in*)


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Handling the special reset clock *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

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
			(string_of_int nb_automata) ^ " automat" ^ (if nb_automata > 1 then "a" else "on")
			^ ", "
			^ (string_of_int nb_locations) ^ " location" ^ (s_of_int nb_locations) ^ ", "
			^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ ", "
			^ (string_of_int nb_actions) ^ " declared synchronization action" ^ (s_of_int nb_actions) ^ ", "
			^ (string_of_int nb_clocks) ^ " clock variable" ^ (s_of_int nb_clocks) ^ ", "
			^ (string_of_int nb_discrete) ^ " discrete variable" ^ (s_of_int nb_discrete) ^ ", "
			^ (string_of_int nb_parameters) ^ " parameter" ^ (s_of_int nb_parameters) ^ ", "
			^ (string_of_int nb_variables) ^ " variable" ^ (s_of_int nb_variables) ^ ", "
			^ (string_of_int (Hashtbl.length constants)) ^ " constant" ^ (s_of_int (Hashtbl.length constants)) ^ "."
		);
	);


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect the strongly deterministic nature of the PTA *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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
						print_message Verbose_high ("This network of PTAs is not strongly deterministic: in automaton '" ^ (automata_names automaton_index) ^ "', in location '" ^ (location_names automaton_index location_index) ^ "', there are " ^ (string_of_int (List.length transitions_for_this_location)) ^ " outgoing transitions labeled with action '" ^ (action_names action_index) ^ "'.");
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
	
	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect the presence of silent transitions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Print some information *)
	print_message Verbose_high ("*** Detecting the presence of silent transitions in the model…");

	(*** NOTE: instead of enumerating all transitions, we just check for the presence of a silent action; if an action was created but unused, it should have been deleted ***)
	let has_silent_actions = List.exists (fun action_index ->
		action_types action_index = Action_type_nosync
	) actions in
	

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect the L/U nature of the PTA *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*** NOTE/HACK: duplicate function in StateSpace ***)
	let continuous_part_of_guard (*: LinearConstraint.pxd_linear_constraint*) = function
		| True_guard -> LinearConstraint.pxd_true_constraint()
		| False_guard -> LinearConstraint.pxd_false_constraint()
		| Discrete_guard discrete_guard -> LinearConstraint.pxd_true_constraint()
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

	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check existence of invariants *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let has_invariants = 
	(* For all PTA *)
	List.exists (fun automaton_index ->
		let locations_for_this_automaton = locations_per_automaton automaton_index in
		(* For all locations *)
		List.exists (fun location_index ->
			let invariant = invariants automaton_index location_index in
			(* TODO benjamin duplicate in PTA2TikZ, maybe we should create general Guard module ? *)
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



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the initial state *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Print some information *)
	print_message Verbose_high ("*** Building initial state…");

	let (initial_location, initial_constraint) =
		make_initial_state variable_infos index_of_automata array_of_location_names index_of_locations index_of_variables parameters removed_variable_names constants type_of_variables variable_names init_discrete_pairs parsed_model.init_definition in

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


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect bounded parameters *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug prints *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
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
			print_message Verbose_total ("Transition " ^ (string_of_int transition_index) ^ ": in automaton '" ^ (automata_names automaton_index) ^ "' via action '" ^ (action_names (transition.action)) ^ "' to location '" ^ (location_names automaton_index (transition.target)) ^ "'")
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
		| Some name -> print_message Verbose_standard ("A global time clock `" ^ Constants.global_time_clock_name ^ "` has been detected.");
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



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Build the final structure *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	{
	(* Cardinality *)
	nb_automata    = nb_automata;
	nb_actions     = nb_actions;
	nb_clocks      = nb_clocks;
	nb_discrete    = nb_discrete;
	nb_parameters  = nb_parameters;
	nb_variables   = nb_variables;
	nb_locations   = nb_locations;
	nb_transitions = nb_transitions;

	(* Is there any invariant in the model? *)
	has_invariants = has_invariants;
	(* Is there any clock going at a rate <> 1 in the model? *)
	has_non_1rate_clocks = has_non_1rate_clocks;
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
	(* True for discrete, false otherwise *)
	is_discrete = is_discrete;
	(* The list of parameter indexes *)
	parameters = parameters;
	(* The non parameters (clocks and discrete) *)
	clocks_and_discrete = list_append clocks discrete;
	(* The non clocks (parameters and discrete) *)
	parameters_and_discrete = list_append parameters discrete;
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
    fun_definitions = Functions.fun_definitions_table;

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
