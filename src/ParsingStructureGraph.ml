(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Dependency graph of the parsed model (variables / functions, etc.)
 *
 * File contributors : Benjamin L.
 * Created           : 2022/09/07
 *
 ************************************************************)

open ParsingStructure
open ParsingStructureUtilities
open ParsingStructureMeta
open CustomModules
open ImitatorUtilities
open OCamlUtilities



type automaton_name = string
type variable_name = string
type param_name = string
type fun_name = string
type id = int

type local_variable_ref = variable_name * fun_name * id
type param_ref = param_name * fun_name

(* Reference to a program component *)
type component =
    | System_component
    | Automaton_component of automaton_name
    | Variable_component of variable_ref
    | Param_component of param_ref
    | Fun_component of fun_name

(* Relation between two components a -> b mean a use b *)
type relation = component * component
(* Dependency graph as a list of relations between the components *)
type dependency_graph = component list (* declared components *) * relation list

(* A relations set *)
module RelationSet = Set.Make(struct
    type t = relation
    let compare a b = if a = b then 0 else (if a < b then -1 else 1)
end)

(* A components set *)
module ComponentSet = Set.Make(struct
    type t = component
    let compare a b = if a = b then 0 else (if a < b then -1 else 1)
end)


(* String of component *)
let string_of_component = function
    | System_component -> "sys"
    | Automaton_component x -> "auto_" ^ x
    | Variable_component (variable_name, id) -> if id = 0 then variable_name else variable_name ^ "_" ^ string_of_int id
    | Fun_component x -> "fun_" ^ x
    | Param_component (x, function_name) -> "param_" ^ x ^ "_of_" ^ function_name

(* String of component with attributes for DOT format *)
let string_of_component_with_attr = function
    | System_component -> "sys [color=gray]"
    | Automaton_component x -> "auto_" ^ x ^ "[color=red]"
    | Variable_component (variable_name, id) -> if id = 0 then variable_name else variable_name ^ "_" ^ string_of_int id ^ "[color=darkseagreen2]"
    | Fun_component x -> "fun_" ^ x ^ "[color=darkseagreen][label=\"" ^ x ^ ":fun\"]"
    | Param_component (x, function_name) -> "param_" ^ x ^ "_of_" ^ function_name ^ "[color=darkseagreen2][label=\"" ^ x ^ ":param\"]"

(* String of component name only *)
let string_of_component_name = function
    | System_component -> ""
    | Automaton_component x -> x
    | Variable_component (variable_name, _) -> variable_name
    | Fun_component x -> x
    | Param_component (x, _) -> x

(* Functions that return component of a variable *)
let get_variable_component variable_ref = Variable_component variable_ref

(* Function that return component reference found in a parsed global expression *)
let components_in_parsed_boolean_expression expr =
    (* Get variables used in the local init expression of the variable *)
    let variables_used = var_set_to_list (get_variable_refs_in_parsed_boolean_expression expr) in
    let variables_used_components = List.map get_variable_component variables_used in
    (* Get functions used in the local init expression of the variable *)
    let functions_used = string_set_to_list (get_functions_in_parsed_boolean_expression expr) in
    let functions_used_components = List.map (fun function_name -> Fun_component function_name) functions_used in
    (* Get refs *)
    variables_used_components @ functions_used_components

(* Function that return component reference found in a parsed arithmetic expression *)
let components_in_parsed_arithmetic_expression expr =
    (* Get variables used in the local init expression of the variable *)
    let variables_used = var_set_to_list (get_variable_refs_in_parsed_discrete_arithmetic_expression expr) in
    let variables_used_components = List.map get_variable_component variables_used in
    (* Get functions used in the local init expression of the variable *)
    let functions_used = string_set_to_list (get_functions_in_parsed_discrete_arithmetic_expression expr) in
    let functions_used_components = List.map (fun function_name -> Fun_component function_name) functions_used in
    (* Get refs *)
    variables_used_components @ functions_used_components

(* TODO benjamin IMPORTANT Check if clock_names can be a constant (ex : x = 1 : clock) *)
(* Check whether a variable is a clock, given it's name (search in declarations_info) *)
let is_clock declarations_info variable_ref =
    let variable_name, _ = variable_ref in
    VariableInfo.is_global variable_ref && List.mem variable_name declarations_info.clock_names

(* Check whether a discrete boolean expression is a reset *)
let is_reset = function
    | Parsed_discrete_bool_expr (Parsed_arithmetic_expr (Parsed_term (Parsed_factor (Parsed_constant value)))) ->
        ParsedValue.is_zero value
    | _ -> false

(* Check whether an update is a clock reset *)
let is_clock_reset declarations_info variable_ref expr =
    let variable_name, _ = variable_ref in
    is_clock declarations_info variable_ref && is_reset expr && variable_name <> Constants.global_time_clock_name

(* All relations found in a sequential code bloc *)
let rec relations_in_parsed_seq_code_bloc declarations_info code_bloc_name bloc_component (* parsed_seq_code_bloc *) =

    let rec relations_in_parsed_seq_code_bloc_rec parsed_seq_code_bloc =
        let relations_nested = List.map relations_in_parsed_instruction parsed_seq_code_bloc in
        List.concat relations_nested

    and relations_in_parsed_instruction = function
        | Parsed_local_decl (variable_name, _, init_expr, id) ->

            (* Create local variable ref representing a unique variable ref *)
            let variable_component = Variable_component (variable_name, id) in

            (* Get references to variables and functions in the local init expression *)
            let all_components = components_in_parsed_boolean_expression init_expr in

            let relations = List.map (fun c -> (variable_component, c)) all_components in

            (* Create relation between current code bloc and declared variable *)
            let bloc_relation = bloc_component, variable_component in

            (* Concat relations *)
            relations @ [bloc_relation]

        | Parsed_assignment (parsed_scalar_or_index_update_type, expr) ->

            let rec contains_function_call =
                let has_fun_call = ParsingStructureMeta.has_fun_call_parsed_boolean_expression expr in
                let rec has_indexes_fun_call = function
                    | Parsed_scalar_update _ -> false
                    | Parsed_indexed_update (inner_scalar_or_index_update_type, index_expr) ->
                        let index_contains_fun_call = ParsingStructureMeta.has_fun_call_parsed_discrete_arithmetic_expression index_expr in
                         index_contains_fun_call || has_indexes_fun_call inner_scalar_or_index_update_type
                in
                has_fun_call || has_indexes_fun_call parsed_scalar_or_index_update_type
            in


            let rec relations_of_scalar_or_index_update_type = function
                | Parsed_scalar_update variable_ref ->

                    (* Is only a clock reset ? We consider not use *)
                    if is_clock_reset declarations_info variable_ref expr then
                        []
                    else (

                        (* Updated variable use all variables found in expression *)
                        (* For example x := a + b, x use a, b *)
                        (* and current function use x *)

                        (* Create local variable ref representing a unique variable ref *)
                        let variable_component = get_variable_component variable_ref in

                        (* Get references to variables and functions in the update expression *)
                        let all_components = components_in_parsed_boolean_expression expr in
                        let relations = List.map (fun c -> (variable_component, c)) all_components in
                        relations

                    )

                | Parsed_indexed_update (inner_scalar_or_index_update_type, index_expr) ->
                    (* Updated variable use all variables found in expression, and all variables found in index *)
                    (* For example x[y + z] = a + b, x use y, z, a, b *)
                    (* and current function use x *)
                    let variable_ref = variable_ref_of_parsed_scalar_or_index_update_type inner_scalar_or_index_update_type in
                    (* Create local variable ref representing a unique variable ref *)
                    let variable_component = get_variable_component variable_ref in
                    (* Get variables / functions used in the indexed expression of the variable *)
                    let all_components = components_in_parsed_arithmetic_expression index_expr in
                    let relations = List.map (fun c -> (variable_component, c)) all_components in

                    let variable_use_variables_relations = relations_of_scalar_or_index_update_type inner_scalar_or_index_update_type in
                    relations @ variable_use_variables_relations
            in

            let variable_ref = variable_ref_of_parsed_scalar_or_index_update_type parsed_scalar_or_index_update_type in
            (* Create local variable ref representing a unique variable ref *)
            let variable_component = get_variable_component variable_ref in

            let relations = relations_of_scalar_or_index_update_type parsed_scalar_or_index_update_type in

            (* Check if assigned expression contains function call(s) *)
            (* If there are any function call, it mean that it may have side effects in expression *)
            (* In this case, we have to consider assigned variable as used in order to NOT delete this assignment instruction on auto-remove :) *)
            let has_fun_call = contains_function_call in

            if has_fun_call then (
                let assigned_variable_relation = bloc_component, variable_component in
                assigned_variable_relation :: relations
            )
            else
                relations






        | Parsed_instruction expr ->
            let all_components = components_in_parsed_boolean_expression expr in
            let relations = List.map (fun c -> (bloc_component, c)) all_components in
            relations

        | Parsed_for_loop (variable_name, from_expr, to_expr, _, inner_bloc, id) ->
            (* Create local variable ref representing a unique variable ref *)
            let variable_component = Variable_component (variable_name, id) in

            (* Get variable and function refs used in the from expression *)
            let from_all_components = components_in_parsed_arithmetic_expression from_expr in
            (* Get variable and function refs used in the to expression *)
            let to_all_components = components_in_parsed_arithmetic_expression to_expr in
            let all_components = from_all_components @ to_all_components in

            (* variable of for loop (for i, i) use variables found in from_expr and to_expr  *)
            let relations = List.map (fun c -> (variable_component, c)) all_components in

            (* Create relation between current bloc and variable declared in for loop *)
            let bloc_relation = bloc_component, variable_component in

            (* Get list of relations for the inner expressions *)
            let inner_declaration_relations = relations_in_parsed_seq_code_bloc_rec inner_bloc in

            (* Concat relations *)
            inner_declaration_relations @ relations @ [bloc_relation]

        | Parsed_while_loop (condition_expr, inner_bloc) ->

            (* Get references to variables and functions in the condition expression *)
            let all_components = components_in_parsed_boolean_expression condition_expr in

            (* Make relations between variable used in condition expression and current function *)
            let relations = List.map (fun c -> (bloc_component, c)) all_components in

            (* Get list of relations for the inner expressions *)
            let inner_declaration_relations = relations_in_parsed_seq_code_bloc_rec inner_bloc in

            (* Concat relations *)
            inner_declaration_relations @ relations

        | Parsed_if (condition_expr, then_bloc, else_bloc_opt) ->

            (* Get references to variables and functions in the condition expression *)
            let all_components = components_in_parsed_boolean_expression condition_expr in

            (* Make relations between variable used and current function *)
            let relations = List.map (fun c -> (bloc_component, c)) all_components in

            (* Get list of relations for the then bloc expressions *)
            let then_bloc_declaration_relations = relations_in_parsed_seq_code_bloc_rec then_bloc in

            (* Get list of relations for the else bloc expressions *)
            let else_bloc_declaration_relations =
                match else_bloc_opt with
                | Some else_bloc ->
                    relations_in_parsed_seq_code_bloc_rec else_bloc
                | None -> []
            in

            (* Concat relations *)
            then_bloc_declaration_relations @ else_bloc_declaration_relations @ relations

    in
    relations_in_parsed_seq_code_bloc_rec (* parsed_seq_code_bloc *)

(* Get the set of all variable names used in the parsed model *)
let all_components_used_in_automatons declarations_info (parsed_model : ParsingStructure.parsed_model) =
	(* Create a set for components *)
	let all_relations = ref RelationSet.empty in

	(*** NOTE: we pass this set by reference ***)

	(* Gather in each automaton *)
	List.iter (fun (automaton_name, sync_name_list, locations) ->
		print_message Verbose_total ("      Gathering variables used in automaton " ^ automaton_name);
        let automaton_component = Automaton_component automaton_name in
        all_relations := RelationSet.add (System_component, automaton_component) !all_relations;
		(* Gather in each location *)
		List.iter (fun (location : parsed_location) ->
			print_message Verbose_total ("        Gathering variables used in location " ^ location.name);

			(* Gather in the cost *)
			begin
				match location.cost with
				| Some cost ->
				print_message Verbose_total ("          Gathering variables in used cost");
				ParsingStructureUtilities.iterate_parsed_linear_expression (function
                    | Leaf_linear_variable (_, variable_name) ->
                        all_relations := RelationSet.add (automaton_component, Variable_component (variable_name, 0)) !all_relations
                    | Leaf_linear_constant _
                    | Leaf_false_linear_constraint
                    | Leaf_true_linear_constraint -> ()

				) cost;

				| None -> ()
			end;

			(* Gather in the stopwatches *)
			print_message Verbose_total ("          Gathering variables used in possible stopwatches");
			List.iter (fun stopwatch_name ->
				all_relations := RelationSet.add (automaton_component, Variable_component (stopwatch_name, 0)) !all_relations
            ) location.stopped;

			(* Gather in the flows *)
			print_message Verbose_total ("          Gathering variables used in possible flows");
			List.iter (fun (clock_name, _) ->
				all_relations := RelationSet.add (automaton_component, Variable_component (clock_name, 0)) !all_relations
            ) location.flow;

			(* Gather in the convex predicate *)
			print_message Verbose_total ("          Gathering variables in convex predicate");
			ParsingStructureUtilities.iterate_parsed_nonlinear_convex_predicate (function
                | Leaf_variable ((variable_name, id) as variable_ref) ->
                    if VariableInfo.is_global variable_ref then
                        all_relations := RelationSet.add (automaton_component, Variable_component (variable_name, 0)) !all_relations
                    else
                        all_relations := RelationSet.add (automaton_component, Variable_component (variable_name, id)) !all_relations
                | Leaf_fun function_name ->
                    all_relations := RelationSet.add (automaton_component, Fun_component function_name) !all_relations
                | Leaf_constant _ -> ()
			) location.invariant;

			(* Gather in transitions *)
			print_message Verbose_total ("          Gathering variables in transitions");
			List.iter (fun (convex_predicate, updates, (*sync*)_, (*target_location_name*)_) ->
				(* Gather in the convex predicate (guard) *)
				print_message Verbose_total ("            Gathering variables in convex predicate");
				ParsingStructureUtilities.iterate_parsed_nonlinear_convex_predicate (function

                    | Leaf_variable ((variable_name, id) as variable_ref) ->
                        if VariableInfo.is_global variable_ref then
                            all_relations := RelationSet.add (automaton_component, Variable_component (variable_name, 0)) !all_relations
                        else
                            all_relations := RelationSet.add (automaton_component, Variable_component (variable_name, id)) !all_relations
                    | Leaf_fun function_name ->
                        all_relations := RelationSet.add (automaton_component, Fun_component function_name) !all_relations
                    | Leaf_constant _ -> ()
                ) convex_predicate;

                let mixin_updates_relations = relations_in_parsed_seq_code_bloc declarations_info "" automaton_component updates in
                all_relations := List.fold_left (fun acc r -> RelationSet.add r acc) !all_relations mixin_updates_relations;

            ) location.transitions;
        ) locations;

    ) parsed_model.automata;

    RelationSet.to_seq !all_relations |> List.of_seq


(* Gather the set of all variable names used in the parsed property *)
let all_components_used_in_property_option parsed_property_option =

    let get_variables_in_parsed_pval parsed_pval =
        let left, _ = List.split parsed_pval in left
    in

    let get_variables_in_parsed_hyper_rectangle parsed_hyper_rectangle =
        (* Return the left part of all triples *)
        List.map (fun (parameter_name, _, _) -> parameter_name) parsed_hyper_rectangle
    in

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
		(* Basic properties *)
		(*------------------------------------------------------------*)

		(* Validity *)
		| Parsed_Valid -> ()


		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Parsed_EF parsed_state_predicate
		(* Safety *)
		| Parsed_AGnot parsed_state_predicate ->
		    ParsingStructureMeta.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate


		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)

		(* Reachability with minimization of a parameter valuation *)
		| Parsed_EFpmin (parsed_state_predicate , parameter_name)
		| Parsed_EFpmax (parsed_state_predicate , parameter_name) ->
			(* First get the variables in the state predicate *)
			ParsingStructureMeta.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate;
			(* Then add the parameter name *)
			variables_used_ref := StringSet.add parameter_name !variables_used_ref

		(* Reachability with minimal-time *)
		| Parsed_EFtmin parsed_state_predicate ->
            ParsingStructureMeta.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate


		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)

		(** Accepting infinite-run (cycle) through a state predicate *)
		| Parsed_Cycle_Through parsed_state_predicate
        | Parsed_Win parsed_state_predicate
			-> ParsingStructureMeta.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate

		(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
		| Parsed_Cycle_Through_generalized parsed_state_predicate_list
			-> List.iter (ParsingStructureMeta.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref) parsed_state_predicate_list

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
		| Parsed_IMunion parsed_pval ->
			variables_used_ref := StringSet.of_list (get_variables_in_parsed_pval parsed_pval);

		(* Non-complete, non-deterministic inverse method with convex result *)
		| Parsed_PRP (parsed_state_predicate , parsed_pval) ->
			(* First get the variables in the state predicate *)
			ParsingStructureMeta.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate;
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
			ParsingStructureMeta.get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref parsed_state_predicate;
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
            variables_used_ref := ParsingStructureMeta.get_variables_in_linear_expression duration


		(* if a2 then a1 happened within d before *)
		| Parsed_pattern (Parsed_TB_Action_precedence_acyclic ((*sync_name*)_, (*sync_name*)_, duration))
		(* everytime a2 then a1 happened within d before *)
		| Parsed_pattern (Parsed_TB_Action_precedence_cyclic ((*sync_name*)_, (*sync_name*)_, duration))
		(* everytime a2 then a1 happened once within d before *)
		| Parsed_pattern (Parsed_TB_Action_precedence_cyclicstrict ((*sync_name*)_, (*sync_name*)_, duration)) ->
(*			get_variables_in_linear_expression variables_used_ref duration*)
				variables_used_ref := ParsingStructureMeta.get_variables_in_linear_expression duration

		(* if a1 then eventually a2 within d *)
		| Parsed_pattern (Parsed_TB_response_acyclic (_, _, parsed_duration))
		(* everytime a1 then eventually a2 within d *)
		| Parsed_pattern (Parsed_TB_response_cyclic (_, _, parsed_duration))
		(* everytime a1 then eventually a2 within d once before next *)
		| Parsed_pattern (Parsed_TB_response_cyclicstrict (_, _, parsed_duration)) ->
(*		    get_variables_in_linear_expression variables_used_ref parsed_duration*)
            variables_used_ref := ParsingStructureMeta.get_variables_in_linear_expression parsed_duration;

		(* sequence a1, …, an *)
		| Parsed_pattern (Parsed_Sequence_acyclic _)
		(* always sequence a1, …, an *)
		| Parsed_pattern (Parsed_Sequence_cyclic _)
			-> ()


		end;
	end;
	(* Return the set *)
	let variables_used = string_set_to_list !variables_used_ref in
	List.map (fun variable_name -> Variable_component (variable_name, 0)) variables_used


(* All declared components found in the parsed model *)
let declared_components_of_model parsed_model =

    (* Get all declared variables in model *)
    let all_declared_variables_in_model =
        List.map (fun (_, variables_list) ->
            List.map (fun (variable_name, _) -> Variable_component (variable_name, 0)) variables_list
        ) parsed_model.variable_declarations
        (* Flatten list of list of variable components *)
        |> List.flatten
    in

    (* Get all declared functions in model *)
    let all_declared_functions_in_model =
        List.map (fun (fun_def : parsed_fun_definition) -> Fun_component fun_def.name) parsed_model.fun_definitions
    in

    (* Get all declared local variables in model *)
    let all_declared_local_variables_in_model =

        (* Get all declared local variables in a given function definition *)
        let all_declared_local_variables_in_fun_def (fun_def : parsed_fun_definition) =
            let local_variables = ParsingStructureMeta.local_variables_of_parsed_fun_def fun_def in
            (* Trick, we eliminate local variables with id = -1 (means that it's a parameter) *)
            let local_variables_without_parameters = List.filter (fun (_, _, id) -> id <> -1) local_variables in
            List.map (fun (variable_name, _, id) -> Variable_component (variable_name, id)) local_variables_without_parameters
        in

        List.fold_left (fun acc fun_def -> all_declared_local_variables_in_fun_def fun_def @ acc) [] parsed_model.fun_definitions
    in

    (* Get all declared formal parameters in model *)
    let all_declared_params_in_model =

        (* Get all declared parameters in a given function definition *)
        let all_declared_params_in_fun_def (fun_def : parsed_fun_definition) =
            List.fold_left (fun acc (variable_name, _ (* id *), _) -> Param_component (variable_name, fun_def.name) :: acc) [] fun_def.parameters
        in
        List.fold_left (fun acc fun_def -> all_declared_params_in_fun_def fun_def @ acc) [] parsed_model.fun_definitions
    in

    all_declared_variables_in_model @ all_declared_functions_in_model @ all_declared_local_variables_in_model @ all_declared_params_in_model

(* Get a dependency graph as a list of relations between variables and functions *)
(* Each relation is a pair representing a ref to a variable / function using another variable / function *)
let dependency_graph ?(no_var_autoremove=false) declarations_info parsed_model parsed_property_opt =

    (* Function that return all component relations of a given function definition *)
    let function_relations (fun_def : parsed_fun_definition) =

        (* Ref to function *)
        let fun_component = Fun_component fun_def.name in
        (* Get code bloc and return expr *)
        let code_bloc, return_expr_opt = fun_def.body in

        (* Get all component relations of current function body *)
        let code_bloc_relations = relations_in_parsed_seq_code_bloc declarations_info fun_def.name fun_component code_bloc in

        let return_expr_relations =
            match return_expr_opt with
            | Some return_expr ->
                (* Get references to variables and functions in the expression *)
                let all_components = components_in_parsed_boolean_expression return_expr in
                List.map (fun c -> (fun_component, c)) all_components
            | None -> []
        in
        code_bloc_relations @ return_expr_relations
    in


    (* Get variables and functions used by automatons *)
    let automatons_relations =
        all_components_used_in_automatons declarations_info parsed_model
    in
    (* Get variables and functions used in all declared functions *)
    let system_functions_relations =
        List.fold_left (fun acc fun_def -> function_relations fun_def @ acc) [] parsed_model.fun_definitions
    in

    (* Get components used in property and create relations between them and system *)
    let property_relations =
        let all_components_used_in_property = all_components_used_in_property_option parsed_property_opt in
        List.map (fun c -> System_component, c) all_components_used_in_property
    in

    (* Create relations found between components in init *)
    let init_relations =
        List.fold_left (fun acc init ->
            match init with
            (* `loc[automaton] = location`: no variable => nothing to do *)
            | Parsed_loc_assignment _
            (* Linear predicate are true or false constraint => nothing to do*)
            | Parsed_linear_predicate Parsed_true_constraint
            | Parsed_linear_predicate Parsed_false_constraint
            (* Special form `variable ~ constant` => in this case we assume NOT used *)
            | Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable _), _, Linear_term (Constant _))) -> acc
            (* *)
            | Parsed_discrete_predicate (variable_name, expr) ->
                let used_variable_names = get_variables_in_parsed_boolean_expression expr in
                let used_variable_names_list = string_set_to_list used_variable_names in
                List.map (fun used_variable_name -> Variable_component (variable_name, 0), Variable_component (used_variable_name, 0)) used_variable_names_list
            (* Linear constraint: get variables *)
            | Parsed_linear_predicate (Parsed_linear_constraint (l_expr, _, r_expr)) ->
                let left_hand_variables = string_set_to_list (get_variables_in_linear_expression l_expr) in
                let right_hand_variables = string_set_to_list (get_variables_in_linear_expression r_expr) in
                let left_and_right_variables = left_hand_variables @ right_hand_variables in
                let combination = OCamlUtilities.list_combination left_and_right_variables left_and_right_variables in
                List.map (fun (l_variable_name, r_variable_name) -> Variable_component (l_variable_name, 0), Variable_component (r_variable_name, 0)) combination @ acc
        ) [] parsed_model.init_definition
    in

    (* Concat all relations, to get overall relations of the model *)
    let all_model_relations = automatons_relations @ system_functions_relations @ init_relations @ property_relations in

    (* Remove variable to variable relations when it's an auto reference *)
    let all_model_relations_without_variable_autoref = List.filter (function
        | (Variable_component _ as a, (Variable_component _ as b)) -> a <> b
        | _ -> true
    ) all_model_relations in

    (* If no var auto remove active, we make relations between system and every components *)


    (* Return dependency graph of the model *)
    declared_components_of_model parsed_model, all_model_relations_without_variable_autoref

(* Get all components that are effectively used by automatons of the model *)
(* It mean all components that are reachable starting from the system reference *)
let used_components_of_model_list (_, component_relations) =

    (* A set that will contain already processed references, to avoid circular *)
    let already_processed_component = ref RelationSet.empty in

    (* Function that compute all reachable refs given a source ref *)
    let rec all_reachable_component relation =

        (* Decompose relation *)
        let _, used_variable_component = relation in

        let already_processed = !already_processed_component in

        (* Check if relation was already processed *)
        (* Avoid that circular references make an infinite loop *)
        if RelationSet.mem relation already_processed then [] else (
            already_processed_component := RelationSet.add relation already_processed;
            (* Get destination refs as new source starting from the current ref *)
            let source_refs = List.filter (fun (a, b) -> a = used_variable_component) component_relations in
            (* Compute destination refs *)
            let dest_refs = List.fold_left (fun acc s -> all_reachable_component s @ acc) [] source_refs in
            (* Add current ref with computed destination refs *)
            used_variable_component :: dest_refs
        )
    in

    (* Get system refs *)
    let system_refs = List.filter (fun (s, d) -> match s with System_component -> true | _ -> false) component_relations in
    (* Find all reachable refs (variables / functions) from system refs... *)
    List.fold_left (fun acc system_ptr -> all_reachable_component system_ptr @ acc) [] system_refs

(* Get all components that are effectively used by automatons of the model *)
(* It mean all components that are reachable starting from the system reference *)
let used_components_of_model dependency_graph = used_components_of_model_list dependency_graph |> List.to_seq |> ComponentSet.of_seq

(* Get all declared components of model *)
let components_of_model (components, _) = components |> List.to_seq |> ComponentSet.of_seq

(* Get all components that are not used by automatons of the model *)
(* It mean all components that are not reachable starting from the system reference *)
let unused_components_of_model dependency_graph =
    (* Get declared components from dependency graph *)
    let declared_components, _ = dependency_graph in
    (* Compute used components from dependency graph *)
    let used_components = used_components_of_model_list dependency_graph in
    (* Convert to sets *)
    let declared_components_set = declared_components |> List.to_seq |> ComponentSet.of_seq in
    let used_components_set = used_components |> List.to_seq |> ComponentSet.of_seq in
    (* Make the diff *)
    let unused_components = ComponentSet.diff
        (declared_components_set)
        (used_components_set)
    in
    unused_components


let unused_components_of_model_list dependency_graph =
    unused_components_of_model dependency_graph |> ComponentSet.to_seq |> List.of_seq

(* General function to filter and map components used in model *)
let filter_map_components_from component_function dependency_graph filter_map =
    (* Get all used components *)
    let used_components = component_function dependency_graph in
    (* Extract something from all used components *)
    let used_component_names = List.filter_map filter_map used_components in
    (* Convert to StringSet, remove duplicates *)
    list_to_string_set used_component_names

(* General function to filter and map components used in model *)
let filter_map_components_used_in_model = filter_map_components_from used_components_of_model_list
(* General function to filter and map components unused in model *)
let filter_map_components_unused_in_model = filter_map_components_from unused_components_of_model_list

(* Get the names of all used global variable *)
let used_global_variables_of_model dependency_graph =
    filter_map_components_used_in_model dependency_graph (function Variable_component (variable_name, id) when id = 0 -> Some variable_name | _ -> None)

(* Get the names of all unused global variable *)
let unused_global_variables_of_model dependency_graph =
    filter_map_components_unused_in_model dependency_graph (function Variable_component (variable_name, id) when id = 0 -> Some variable_name | _ -> None)

(* Get the names of all function used *)
let used_functions_of_model dependency_graph =
    filter_map_components_used_in_model dependency_graph (function Fun_component name -> Some name | _ -> None)

(* Get the names of all function unused *)
let unused_functions_of_model dependency_graph =
    filter_map_components_unused_in_model dependency_graph (function Fun_component name -> Some name | _ -> None)


(* Remove all unused clock assignments in sequential code bloc *)
let remove_unused_assignments_in_parsed_seq_code_bloc declarations_info dependency_graph code_bloc_name (* parsed_seq_code_bloc *) =

    (* Get global variables used in model *)
    let used_global_variables = used_global_variables_of_model dependency_graph in
    let used_global_variables_list = string_set_to_list used_global_variables in

    let rec remove_unused_assignments_in_parsed_seq_code_bloc_rec parsed_seq_code_bloc =
        List.filter_map remove_unused_assignment_instruction parsed_seq_code_bloc

    and remove_unused_assignment_instruction = function
        | Parsed_assignment (parsed_scalar_or_index_update_type, expr) as instruction ->
            let variable_name, id as variable_ref = variable_ref_of_parsed_scalar_or_index_update_type parsed_scalar_or_index_update_type in

            let is_not_used = not (List.mem variable_name used_global_variables_list) && VariableInfo.is_global variable_ref in

            if is_not_used then (
                None
            )
            else
                Some instruction

        | Parsed_instruction expr as instruction ->
            Some instruction

        | Parsed_local_decl (variable_name, discrete_type, init_expr, id) as instruction ->
            Some instruction

        (* These type of instruction are always been considered as used by the bloc ! *)
        | Parsed_for_loop (variable_name, from_expr, to_expr, loop_dir, inner_bloc, id) ->
            let filtered_inner_bloc = remove_unused_assignments_in_parsed_seq_code_bloc_rec inner_bloc in
            Some (Parsed_for_loop (variable_name, from_expr, to_expr, loop_dir, filtered_inner_bloc, id))

        | Parsed_while_loop (cond_expr, inner_bloc) ->
            let filtered_inner_bloc = remove_unused_assignments_in_parsed_seq_code_bloc_rec inner_bloc in
            Some (Parsed_while_loop (cond_expr, filtered_inner_bloc))

        | Parsed_if (cond_expr, then_bloc, else_bloc_opt) ->
            let filtered_then_bloc = remove_unused_assignments_in_parsed_seq_code_bloc_rec then_bloc in

            let filtered_else_bloc =
                match else_bloc_opt with
                | Some else_bloc ->
                    Some (remove_unused_assignments_in_parsed_seq_code_bloc_rec else_bloc)
                | None -> None
            in

            Some (Parsed_if (cond_expr, filtered_then_bloc, filtered_else_bloc))
    in
    remove_unused_assignments_in_parsed_seq_code_bloc_rec (* parsed_seq_code_bloc *)


(* Remove all unused assignments in sequential code bloc *)
let remove_unused_assignments_in_updates declarations_info dependency_graph (* parsed_seq_code_bloc *) =
    remove_unused_assignments_in_parsed_seq_code_bloc declarations_info dependency_graph "" (* parsed_seq_code_bloc *)

(* Remove all unused assignments in function definition *)
let remove_unused_assignments_in_fun_def declarations_info dependency_graph (fun_def : parsed_fun_definition) =
    (* Get code bloc and return expr *)
    let code_bloc, return_expr_opt = fun_def.body in
    { fun_def with body = remove_unused_assignments_in_parsed_seq_code_bloc declarations_info dependency_graph fun_def.name code_bloc, return_expr_opt }

(* Check whether a init state predicate is used *)
let is_init_state_predicate_used used_global_variables (* init_state_predicate *) =

    (* Check whether linear constraint is used *)
    let is_linear_constraint_used = function
        | Parsed_true_constraint
        | Parsed_false_constraint -> true
        | Parsed_linear_constraint (l_expr, _ ,r_expr) ->
            (* If any variable in constraint are used, then linear constraint in init is used *)
            let variable_names =
                StringSet.union (get_variables_in_linear_expression l_expr) (get_variables_in_linear_expression r_expr)
                |> OCamlUtilities.string_set_to_list
            in
            List.exists (fun variable_name -> StringSet.mem variable_name used_global_variables || variable_name = Constants.global_time_clock_name) variable_names
    in
    (* Check whether init state predicate is used *)
    let is_init_state_predicate_used = function
        | Parsed_discrete_predicate (variable_name, expr) ->
            StringSet.mem variable_name used_global_variables
        | Parsed_linear_predicate linear_constraint ->
            is_linear_constraint_used linear_constraint
        | _ -> true
    in
    is_init_state_predicate_used (* init_state_predicate *)

(* Filter a init_definition (init_state_predicate list) to keep only init definition of used variables *)
let remove_unused_inits dependency_graph (* init_definition *) =
    let used_global_variables = used_global_variables_of_model dependency_graph in
    List.filter (is_init_state_predicate_used used_global_variables) (* init_definition *)


let model_cycle_infos (_, model_relations) =

    let rec is_cycle_in already_seen c =

        let is_fun_component = function
            | Fun_component _ -> true
            | _ -> false
        in

        if List.mem c already_seen then (
            if is_fun_component c then (
                [true, c :: already_seen]
            )
            else (
                []
            )
        )
        else (
            let next_components = List.filter_map (function (src, dst) when src = c -> Some dst | _ -> None) model_relations in
            List.map (is_cycle_in (c :: already_seen)) next_components |> List.flatten
        )
    in

    let system_components = List.filter_map (function (System_component, dst) -> Some dst | _ -> None) model_relations in
    let cycle_infos = List.map (is_cycle_in []) system_components |> List.flatten in
    (* Transform path list to str path *)
    List.map (fun (has_cycle, path) ->
        let str_path_list = List.map string_of_component_name path |> List.rev in
        let str_path = OCamlUtilities.string_of_list_of_string_with_sep " -> " str_path_list in
        has_cycle, str_path
    ) cycle_infos

(* Get dependency graph as string (dot graphviz format) (generate png of this graph with the cmd: dot -Tpng file.dot file.png) *)
let string_of_dependency_graph (components, component_relations) =
    (* String of each components *)
    let str_components_list = List.map string_of_component_with_attr components in
    let str_components = OCamlUtilities.string_of_list_of_string_with_sep ";" str_components_list in
    (* String of relation between two components *)
    let string_of_component_relation (a, b) = string_of_component a ^ " -> " ^ string_of_component b in
    (* Map all relations to strings *)
    let str_relations_list = List.map string_of_component_relation component_relations in
    (* Concat to string *)
    let str_component_relations = OCamlUtilities.string_of_list_of_string_with_sep "; " str_relations_list in
    (* Dependency graph as dot format *)
    "digraph dependency_graph {" ^ str_components ^ ";" ^ str_component_relations ^ "}"