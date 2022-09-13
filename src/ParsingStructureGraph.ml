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
    | System_ref
    | Automaton_ref of automaton_name
    | Global_variable_ref of variable_name
    | Local_variable_ref of local_variable_ref
    | Param_ref of param_ref
    | Fun_ref of fun_name

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
    | System_ref -> "sys"
    | Automaton_ref x -> "auto_" ^ x
    | Global_variable_ref x -> x
    | Local_variable_ref (x, _, id) -> x ^ "_" ^ string_of_int id
    | Fun_ref x -> "fun_" ^ x
    | Param_ref (x, function_name) -> "param_" ^ x ^ "_of_" ^ function_name

(* String of component with attributes for DOT format *)
let string_of_component_with_attr = function
    | System_ref -> "sys [color=gray]"
    | Automaton_ref x -> "auto_" ^ x ^ "[color=red]"
    | Global_variable_ref x -> x
    | Local_variable_ref (x, _, id) -> x ^ "_" ^ string_of_int id ^ "[color=darkseagreen2]"
    | Fun_ref x -> "fun_" ^ x ^ "[color=darkseagreen][label=\"" ^ x ^ ":fun\"]"
    | Param_ref (x, function_name) -> "param_" ^ x ^ "_of_" ^ function_name ^ "[color=darkseagreen2][label=\"" ^ x ^ ":param\"]"

(* String of component name only *)
let string_of_component_name = function
    | System_ref -> ""
    | Automaton_ref x -> x
    | Global_variable_ref x -> x
    | Local_variable_ref (x, _, _) -> x
    | Fun_ref x -> x
    | Param_ref (x, _) -> x

(* Get the set of all variable names used in the parsed model *)
let all_components_used_in_automatons (parsed_model : ParsingStructure.parsed_model) =
	(* Create a set for components *)
	let all_relations = ref RelationSet.empty in

	(*** NOTE: we pass this set by reference ***)

	(* Gather in each automaton *)
	List.iter (fun (automaton_name, sync_name_list, locations) ->
		print_message Verbose_total ("      Gathering variables used in automaton " ^ automaton_name);
        let automaton_ref = Automaton_ref automaton_name in
        all_relations := RelationSet.add (System_ref, automaton_ref) !all_relations;
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
                        all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                    | Leaf_linear_constant _
                    | Leaf_false_linear_constraint
                    | Leaf_true_linear_constraint -> ()

				) cost;

				| None -> ()
			end;

			(* Gather in the stopwatches *)
			print_message Verbose_total ("          Gathering variables used in possible stopwatches");
			List.iter (fun stopwatch_name ->
				all_relations := RelationSet.add (automaton_ref, Global_variable_ref stopwatch_name) !all_relations
            ) location.stopped;

			(* Gather in the flows *)
			print_message Verbose_total ("          Gathering variables used in possible flows");
			List.iter (fun (clock_name, _) ->
				all_relations := RelationSet.add (automaton_ref, Global_variable_ref clock_name) !all_relations
            ) location.flow;

			(* Gather in the convex predicate *)
			print_message Verbose_total ("          Gathering variables in convex predicate");
			ParsingStructureUtilities.iterate_parsed_nonlinear_convex_predicate (function
                | Leaf_variable variable_name ->
                    all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                | Leaf_fun function_name ->
                    all_relations := RelationSet.add (automaton_ref, Fun_ref function_name) !all_relations
                | Leaf_constant _
                | Leaf_update_variable _ -> ()
			) location.invariant;

			(* Gather in transitions *)
			print_message Verbose_total ("          Gathering variables in transitions");
			List.iter (fun (convex_predicate, update_section, (*sync*)_, (*target_location_name*)_) ->
				(* Gather in the convex predicate (guard) *)
				print_message Verbose_total ("            Gathering variables in convex predicate");
				ParsingStructureUtilities.iterate_parsed_nonlinear_convex_predicate (function
                    | Leaf_variable variable_name ->
                        all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                    | Leaf_fun function_name ->
                        all_relations := RelationSet.add (automaton_ref, Fun_ref function_name) !all_relations
                    | Leaf_constant _
                    | Leaf_update_variable _ -> ()
                ) convex_predicate;

				(* Gather in the updates *)
				print_message Verbose_total ("            Gathering variables in updates");
				let updates = updates_of_update_section update_section in

				List.iter (fun update_expression ->
					(*** NOTE: let us NOT consider that a reset is a 'use' of a variable; it must still be used in a guard, an invariant, in the right-hand side term of a reset, or a property, to be considered 'used' in the model ***)
					ParsingStructureUtilities.iterate_parsed_update (function
                        | Leaf_variable variable_name ->
                            all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                        | Leaf_fun function_name ->
                            all_relations := RelationSet.add (automaton_ref, Fun_ref function_name) !all_relations
                        | Leaf_constant _
                        | Leaf_update_variable _ -> ()
					) update_expression;

                ) updates;

                (* Only for seq updates *)
                let seq_updates, _ = update_section in

                (* In seq update an assigned variable is considered as used (even if not used in any guard / invariant) *)
                (* For example r := <anything>, r is considered as used *)
                (* We make this choice for sake of simplicity, because seq updates can contains side effect functions that should be executed *)
                (* eg: r := stack_pop(s) *)
				List.iter (fun update_expression ->
					ParsingStructureUtilities.iterate_parsed_update
					    (function
                            | Leaf_variable _
                            | Leaf_constant _
                            | Leaf_fun _ -> ()
					        | Leaf_update_variable variable_name ->
                                all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                        )
                    update_expression;
                ) seq_updates;

            ) location.transitions;
        ) locations;

    ) parsed_model.automata;

    RelationSet.to_seq !all_relations |> List.of_seq

(**)
let declared_components_of_model parsed_model =

    (* Get all declared variables in model *)
    let all_declared_variables_in_model =
        List.map (fun (_, variables_list) ->
            List.map (fun (variable_name, _) -> Global_variable_ref variable_name) variables_list
        ) parsed_model.variable_declarations
        (* Flatten list of list of variable components *)
        |> List.flatten
    in

    (* Get all declared function in model *)
    let all_declared_functions_in_model =
        List.map (fun (fun_def : parsed_fun_definition) -> Fun_ref fun_def.name) parsed_model.fun_definitions
    in

    (* Get all declared local variables in a given function definition *)
    let all_declared_local_variables_in_fun_def (fun_def : parsed_fun_definition) =
        ParsingStructureUtilities.fold_parsed_function_definition
            (@) (* operator concat list *)
            [] (* base *)
            (function Leaf_decl_variable (variable_name, _, id) -> [Local_variable_ref (variable_name, fun_def.name, id)])
            (function _ -> [])
            fun_def
    in

    (* Get all declared parameters in a given function definition *)
    let all_declared_params_in_fun_def (fun_def : parsed_fun_definition) =
        List.fold_left (fun acc (variable_name, _) -> Param_ref (variable_name, fun_def.name) :: acc) [] fun_def.parameters
    in

    let all_declared_local_variables_in_model =
        List.fold_left (fun acc fun_def -> all_declared_local_variables_in_fun_def fun_def @ acc) [] parsed_model.fun_definitions
    in

    let all_declared_params_in_model =
        List.fold_left (fun acc fun_def -> all_declared_params_in_fun_def fun_def @ acc) [] parsed_model.fun_definitions
    in

    all_declared_variables_in_model @ all_declared_functions_in_model @ all_declared_local_variables_in_model @ all_declared_params_in_model

(* Get a dependency graph as a list of relations between variables and functions *)
(* Each relation is a pair representing a ref to a variable / function using another variable / function *)
let dependency_graph ?(no_var_autoremove=false) parsed_model =

    (* Function that return all component relations of a given function definition *)
    let function_relations fun_def =

        (* Get parameter names *)
        let parameter_names = List.map first_of_tuple fun_def.parameters in
        (* Add parameter names to local variables of function *)
        let local_variables = List.fold_right (fun parameter_name acc -> StringMap.add parameter_name (Param_ref (parameter_name, fun_def.name)) acc) parameter_names StringMap.empty in
        (* Ref to function *)
        let fun_ref = Fun_ref fun_def.name in

        (* Functions that return ref of a variable, if variable is found in local variable table *)
        (* It return a Local_variable_ref, else a Global_variable_ref *)
        let get_variable_ref local_variables used_variable_name =
            let used_variable_ref_opt = StringMap.find_opt used_variable_name local_variables in
            match used_variable_ref_opt with
            | Some used_variable_ref -> used_variable_ref
            | None -> Global_variable_ref used_variable_name
        in

        (* Create relations between a set of variables used by another variable reference *)
        let variable_to_variable_relations local_variables variable_ref variables_used =
            StringSet.fold (fun used_variable_name acc ->
                let used_variable_ref = get_variable_ref local_variables used_variable_name in
                (variable_ref, used_variable_ref) :: acc
            ) variables_used []
        in

        (* Function that return component reference found in a parsed global expression *)
        let get_variable_and_function_refs_in_parsed_boolean_expression local_variables expr =
            (* Get variables used in the local init expression of the variable *)
            let variables_used = string_set_to_list (get_variables_in_parsed_boolean_expression expr) in
            let variables_used_refs = List.map (get_variable_ref local_variables) variables_used in
            (* Get functions used in the local init expression of the variable *)
            let functions_used = string_set_to_list (get_functions_in_parsed_boolean_expression expr) in
            let functions_used_refs = List.map (fun function_name -> Fun_ref function_name) functions_used in
            (* Get refs *)
            variables_used_refs, functions_used_refs
        in

        (* Function that return component reference found in a parsed arithmetic expression *)
        let get_variable_and_function_refs_in_parsed_arithmetic_expression local_variables expr =
            (* Get variables used in the local init expression of the variable *)
            let variables_used = string_set_to_list (get_variables_in_parsed_discrete_arithmetic_expression expr) in
            let variables_used_refs = List.map (get_variable_ref local_variables) variables_used in
            (* Get functions used in the local init expression of the variable *)
            let functions_used = string_set_to_list (get_functions_in_parsed_discrete_arithmetic_expression expr) in
            let functions_used_refs = List.map (fun function_name -> Fun_ref function_name) functions_used in
            (* Get refs *)
            variables_used_refs, functions_used_refs
        in

        (* Function that return all component relations of a given function expression *)
        let rec function_relations_in_parsed_seq_code_bloc_rec local_variables = function
            | Parsed_local_decl (variable_name, _, init_expr, next_expr, id) ->

                (* Create local variable ref representing a unique variable ref *)
                let variable_ref = Local_variable_ref (variable_name, fun_def.name, id) in
                (* Add the new declared local variable (or update if the new declaration shadows a previous one) *)
                let local_variables = StringMap.update variable_name (function None -> Some variable_ref | Some _ -> Some variable_ref) local_variables in

                (* Get variable and function refs used in the local init expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_boolean_expression local_variables init_expr in
                let all_refs = variables_used_refs @ functions_used_refs in
                let relations = List.map (fun _ref -> (variable_ref, _ref)) all_refs in

                (* Add a relation between current function and declared variable *)
                (* The declared variable will always be considered as used here *)
                let relations = (fun_ref, variable_ref) :: relations in
                (* Old behavior (deprecated) *)
                (*
                (* Add a relation between current function and declared variable, if -no-var-autoremove option is set *)
                let relations =
                    if no_var_autoremove then
                        (fun_ref, variable_ref) :: relations
                    else
                        relations
                in
                *)

                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables next_expr in
                (* Concat current relations with next relations *)
                next_declaration_relations @ relations

            | Parsed_assignment ((parsed_update_type, expr), next_expr) ->

                let rec relations_of_scalar_or_index_update_type = function
                    | Parsed_scalar_update variable_name ->
                        (* Updated variable use all variables found in expression *)
                        (* For example x := a + b, x use a, b *)
                        (* and current function use x *)

                        (* Create local variable ref representing a unique variable ref *)
                        let variable_ref = get_variable_ref local_variables variable_name in

                        (* Get variable and function refs used in the update expression *)
                        let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_boolean_expression local_variables expr in
                        let all_refs = variables_used_refs @ functions_used_refs in
                        let relations = List.map (fun _ref -> (variable_ref, _ref)) all_refs in

                        (* Here for sake of simplicity, we consider that all global variables modified in function are used (it's not always the case) *)

                        (* Create relation between current function and assigned variable *)
                        let cur_fun_used_variable_relation = fun_ref, variable_ref in
                        (* Concat all relations *)
                        cur_fun_used_variable_relation :: relations

                    | Parsed_indexed_update (inner_scalar_or_index_update_type, index_expr) ->
                        (* Updated variable use all variables found in expression, and all variables found in index *)
                        (* For example x[y + z] = a + b, x use y, z, a, b *)
                        (* and current function use x *)
                        let variable_name = variable_name_of_parsed_scalar_or_index_update_type inner_scalar_or_index_update_type in

                        (* Create local variable ref representing a unique variable ref *)
                        let variable_ref = get_variable_ref local_variables variable_name in

                        (* Get variables / functions used in the indexed expression of the variable *)
                        let variables_used = string_set_to_list (get_variables_in_parsed_discrete_arithmetic_expression index_expr) in
                        (* Get functions used in the local init expression of the variable *)
                        let functions_used = string_set_to_list (get_functions_in_parsed_discrete_arithmetic_expression index_expr) in

                        let variables_used_refs = List.map (get_variable_ref local_variables) variables_used in
                        let functions_used_refs = List.map (fun function_name -> Fun_ref function_name) functions_used in
                        let all_refs = variables_used_refs @ functions_used_refs in
                        let relations = List.map (fun _ref -> (variable_ref, _ref)) all_refs in

                        let variable_use_variables_relations = relations_of_scalar_or_index_update_type inner_scalar_or_index_update_type in
                        relations @ variable_use_variables_relations


                in
                let relations_of_update_type = function
                    | Parsed_variable_update parsed_scalar_or_index_update_type ->
                        relations_of_scalar_or_index_update_type parsed_scalar_or_index_update_type
                    | Parsed_void_update ->
                        (* All variables found in expression are used by current function *)
                        (* For example: stack_pop(s) + x + y *)
                        (* Current function 'f' use s, x, y *)

                        (* Get variables used in update expression *)
                        let variables_used = get_variables_in_parsed_boolean_expression expr in
                        let functions_used = get_functions_in_parsed_boolean_expression expr in

                        let fun_use_variables_relations = variable_to_variable_relations local_variables fun_ref variables_used in

                        let fun_use_fun_relations = StringSet.fold (fun used_function_name acc ->
                            (fun_ref, Fun_ref used_function_name) :: acc
                        ) functions_used []
                        in
                        fun_use_variables_relations @ fun_use_fun_relations

                in
                let relations = relations_of_update_type parsed_update_type in

                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables next_expr in
                (* Concat current relations with next relations *)
                relations @ next_declaration_relations

            | Parsed_for_loop (variable_name, from_expr, to_expr, _, inner_bloc, next_expr, id) ->
                (* Create local variable ref representing a unique variable ref *)
                let variable_ref = Local_variable_ref (variable_name, fun_def.name, id) in
                (* Add the new declared local variable (or update if the new declaration shadows a previous one) *)
                let local_variables_of_loop = StringMap.update variable_name (function None -> Some variable_ref | Some _ -> Some variable_ref) local_variables in

                (* Get variable and function refs used in the from expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_arithmetic_expression local_variables from_expr in
                let all_refs = variables_used_refs @ functions_used_refs in
                (* Get variable and function refs used in the to expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_arithmetic_expression local_variables to_expr in
                let all_refs = all_refs @ variables_used_refs @ functions_used_refs in

                (* variable of for loop (for i, i) use variables found in from_expr and to_expr  *)
                let relations = List.map (fun _ref -> (variable_ref, _ref)) all_refs in

                (* Add a relation between current function and variable of for loop *)
                let relations = (fun_ref, variable_ref) :: relations in

                (* Get list of relations for the inner expressions *)
                let inner_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables_of_loop inner_bloc in
                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables next_expr in
                (* Concat current relations with next relations *)
                inner_declaration_relations @ next_declaration_relations @ relations

            | Parsed_while_loop (condition_expr, inner_bloc, next_expr) ->

                (* Get variable and function refs used in the condition expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_boolean_expression local_variables condition_expr in
                let all_refs = variables_used_refs @ functions_used_refs in

                (* Make relations between variable used in condition expression and current function *)
                let relations = List.map (fun _ref -> (fun_ref, _ref)) all_refs in

                (* Get list of relations for the inner expressions *)
                let inner_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables inner_bloc in
                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables next_expr in

                (* Concat current relations with next relations *)
                inner_declaration_relations @ next_declaration_relations @ relations

            | Parsed_if (condition_expr, then_bloc, else_bloc_opt, next_expr) ->

                (* Get variable and function refs used in the condition expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_boolean_expression local_variables condition_expr in
                let all_refs = variables_used_refs @ functions_used_refs in

                (* Make relations between variable used and current function *)
                let relations = List.map (fun _ref -> (fun_ref, _ref)) all_refs in

                (* Get list of relations for the then bloc expressions *)
                let then_bloc_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables then_bloc in

                (* Get list of relations for the else bloc expressions *)
                let else_bloc_declaration_relations =
                    match else_bloc_opt with
                    | Some else_bloc ->
                        function_relations_in_parsed_seq_code_bloc_rec local_variables else_bloc
                    | None -> []
                in

                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables next_expr in

                (* Concat current relations with next relations *)
                then_bloc_declaration_relations @ else_bloc_declaration_relations @ next_declaration_relations @ relations

            | Parsed_bloc_expr expr ->
                (* Get variable and function refs used in the expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_boolean_expression local_variables expr in
                let all_refs = variables_used_refs @ functions_used_refs in
                List.map (fun _ref -> (fun_ref, _ref)) all_refs

            | Parsed_bloc_void -> []
        in
        (* Get all component relations of current function body *)
        function_relations_in_parsed_seq_code_bloc_rec local_variables fun_def.body
    in
    (* Get variables and functions used by automatons *)
    let automatons_relations =
        all_components_used_in_automatons parsed_model
    in
    (* Get variables and functions used in all declared functions *)
    let system_functions_relations =
        List.fold_left (fun acc fun_def -> function_relations fun_def @ acc) [] parsed_model.fun_definitions
    in

    (* Get variables and functions dependencies in init *)
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
                List.map (fun used_variable_name -> Global_variable_ref variable_name, Global_variable_ref used_variable_name) used_variable_names_list
            (* Linear constraint: get variables *)
            | Parsed_linear_predicate (Parsed_linear_constraint (l_expr, _, r_expr)) ->
                let left_hand_variables = string_set_to_list (get_variables_in_linear_expression l_expr) in
                let right_hand_variables = string_set_to_list (get_variables_in_linear_expression r_expr) in
                let left_and_right_variables = left_hand_variables @ right_hand_variables in
                let combination = OCamlUtilities.list_combination left_and_right_variables left_and_right_variables in
                List.map (fun (l_variable_name, r_variable_name) -> Global_variable_ref l_variable_name, Global_variable_ref r_variable_name) combination @ acc
        ) [] parsed_model.init_definition
    in
    (* Concat all relations, to get overall relations of the model *)
    let all_model_relations = automatons_relations @ system_functions_relations @ init_relations in
    (* Remove variable to variable relations when it's an auto reference *)
    let all_model_relations_without_variable_autoref = List.filter (function
        | (Global_variable_ref _ as a, (Global_variable_ref _ as b)) -> a <> b
        | (Local_variable_ref _ as a, (Local_variable_ref _ as b)) -> a <> b
        | _ -> true
    ) all_model_relations in
    (* Return dependency graph of the model *)
    declared_components_of_model parsed_model, all_model_relations_without_variable_autoref

(* Get all components that are effectively used by automatons of the model *)
(* It mean all components that are reachable starting from the system reference *)
let used_components_of_model_list (_, component_relations) =

    (* A set that will contain already processed references, to avoid circular *)
    let already_processed_ref = ref RelationSet.empty in

    (* Function that compute all reachable refs given a source ref *)
    let rec all_reachable_ref relation =

        (* Decompose relation *)
        let variable_ref, used_variable_ref = relation in

        let already_processed = !already_processed_ref in

        (* Check if relation was already processed *)
        (* Avoid that circular references make an infinite loop *)
        if RelationSet.mem relation already_processed then [] else (
            already_processed_ref := RelationSet.add relation already_processed;
            (* Get destination refs as new source starting from the current ref *)
            let source_refs = List.filter (fun (a, b) -> a = used_variable_ref) component_relations in
            (* Compute destination refs *)
            let dest_refs = List.fold_left (fun acc s -> all_reachable_ref s @ acc) [] source_refs in
            (* Add current ref with computed destination refs *)
            used_variable_ref :: dest_refs
        )
    in

    (* Get system refs *)
    let system_refs = List.filter (fun (s, d) -> match s with Automaton_ref _ -> true | _ -> false) component_relations in
    (* Find all reachable refs (variables / functions) from system refs... *)
    List.fold_left (fun acc system_ptr -> all_reachable_ref system_ptr @ acc) [] system_refs

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
    (* Remove parameters and local variables from unused components if the function to which they belong is unused *)
    (* Note that it's useless to print message `param x of f` is unused if the function `f` is unused *)
    ComponentSet.filter (function
        | Param_ref (_, function_name)
        | Local_variable_ref (_, function_name, _) ->
            not (ComponentSet.mem (Fun_ref function_name) unused_components)
        | _ -> true
    ) unused_components


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
let used_variables_of_model dependency_graph =
    filter_map_components_used_in_model dependency_graph (function Global_variable_ref variable_name -> Some variable_name | _ -> None)

(* Get the names of all unused global variable *)
let unused_variables_of_model dependency_graph =
    filter_map_components_unused_in_model dependency_graph (function Global_variable_ref variable_name -> Some variable_name | _ -> None)

(* Get the names of all function used *)
let used_functions_of_model dependency_graph =
    filter_map_components_used_in_model dependency_graph (function Fun_ref name -> Some name | _ -> None)

(* Get the names of all function unused *)
let unused_functions_of_model dependency_graph =
    filter_map_components_unused_in_model dependency_graph (function Fun_ref name -> Some name | _ -> None)



let model_cycle_infos (_, model_relations) =

    let rec is_cycle_in already_seen c =

        let is_fun_ref = function
            | Fun_ref _ -> true
            | _ -> false
        in

        if List.mem c already_seen then (
            if is_fun_ref c then (
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

    let system_components = List.filter_map (function (System_ref, dst) -> Some dst | _ -> None) model_relations in
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

(* Utils function for traversing a parsed_fun_definition *)
let traverse_function operator f base (fun_def : parsed_fun_definition) =
    (* Add parameters as local variables *)
    let parameter_refs = List.map (fun (param_name, _) -> Param_ref (param_name, fun_def.name)) fun_def.parameters in
    let local_variable_components = List.fold_right ComponentSet.add parameter_refs ComponentSet.empty in
    (* Create ref of local variable components set *)
    let local_variable_components_ref = ref local_variable_components in

    (* TODO benjamin see if can replace by general function *)
    (* Function that traverse function body expression *)
    let rec traverse_parsed_seq_code_bloc = function
        | Parsed_local_decl (variable_name, _, _, next_expr, id) as expr ->
            (* Add the new declared local variable to set *)
            let local_variable_ref = Local_variable_ref (variable_name, fun_def.name, id) in
            local_variable_components_ref := ComponentSet.add local_variable_ref !local_variable_components_ref;

            operator
                (f !local_variable_components_ref expr)
                (traverse_parsed_seq_code_bloc next_expr)

        | Parsed_for_loop (variable_name, _, _, _, inner_bloc, next_expr, id) as expr ->
            (* Add the new declared local variable to set *)
            let local_variable_ref = Local_variable_ref (variable_name, fun_def.name, id) in
            local_variable_components_ref := ComponentSet.add local_variable_ref !local_variable_components_ref;

            operator
                (operator
                    (f !local_variable_components_ref expr)
                    (traverse_parsed_seq_code_bloc inner_bloc)
                )
                (traverse_parsed_seq_code_bloc next_expr)

        | Parsed_while_loop (_, inner_bloc, next_expr) as expr ->
            operator
                (operator
                    (f !local_variable_components_ref expr)
                    (traverse_parsed_seq_code_bloc inner_bloc)
                )
                (traverse_parsed_seq_code_bloc next_expr)

        | Parsed_if (_, then_bloc, else_bloc_opt, next_expr) as expr ->
            (f !local_variable_components_ref expr)
            |> operator (traverse_parsed_seq_code_bloc then_bloc)
            |> (match else_bloc_opt with Some else_bloc -> operator (traverse_parsed_seq_code_bloc else_bloc) | None -> operator base)
            |> operator (traverse_parsed_seq_code_bloc next_expr)

        | Parsed_assignment (_, next_expr) as expr ->
            operator
                (f !local_variable_components_ref expr)
                (traverse_parsed_seq_code_bloc next_expr)

        | Parsed_bloc_expr _ as expr ->
            f !local_variable_components_ref expr
        | Parsed_bloc_void -> base
    in
    traverse_parsed_seq_code_bloc fun_def.body


(* TODO benjamin REFACT look at this function, very ugly *)
(* Get all variables (local and global) at the left side of an assignment in a function body implementation *)
let left_variables_of_assignments_in (fun_def : parsed_fun_definition) =

    (* Create set of assigned variables *)
    let components_ref = ref ComponentSet.empty in

    (* Function that get assigned variable in function body expression *)
    let rec left_variables_of_assignments_in_parsed_seq_code_bloc local_variable_components = function
        | Parsed_for_loop _
        | Parsed_while_loop _
        | Parsed_if _
        | Parsed_local_decl _ -> ()

        | Parsed_assignment ((parsed_update_type, _), next_expr) ->

            let variable_name_opt = variable_name_of_parsed_update_type_opt parsed_update_type in
            (match variable_name_opt with
            | Some variable_name ->
                (* Check existence of any local variable / parameter that may shadow global variable of the same name *)
                let local_variable_component_opt = ComponentSet.find_first_opt (function
                    | Param_ref (v, _)
                    | Local_variable_ref (v, _, _) -> variable_name = v
                    | _ -> false
                ) local_variable_components
                in

                let component =
                    match local_variable_component_opt with
                    (* If any local variable shadow a global variable, get it's reference *)
                    | Some local_variable_component -> local_variable_component
                    (* Else it's possibly an update of a global variable (or an nonexistent variable) *)
                    | None -> Global_variable_ref variable_name
                in
                components_ref := ComponentSet.add component !components_ref

            | None -> ()
            );
        | Parsed_bloc_expr _
        | Parsed_bloc_void -> ()
    in
    traverse_function bin_unit left_variables_of_assignments_in_parsed_seq_code_bloc () fun_def;
    !components_ref

let variable_ref_of local_variable_components variable_name =
    (* Check existence of any local variable / parameter that may shadow global variable of the same name *)
    let local_variable_component_opt = ComponentSet.find_first_opt (function
        | Param_ref (v, _)
        | Local_variable_ref (v, _, _) -> variable_name = v
        | _ -> false
    ) local_variable_components
    in

    match local_variable_component_opt with
    (* If any local variable shadow a global variable, get it's reference *)
    | Some local_variable_component -> local_variable_component
    (* Else it's possibly an update of a global variable (or an nonexistent variable) *)
    | None -> Global_variable_ref variable_name

(* TODO benjamin REFACT look at this function, very ugly *)
(* Get all variables (local and global) at the right side of an assignment in a function body implementation *)
let right_variables_of_assignments_in (fun_def : parsed_fun_definition) =

    (* Create set of assigned variables *)
    let component_refs = ref ComponentSet.empty in

    (* Function that get assigned variable in function body expression *)
    let right_variables_of_assignments_in_parsed_seq_code_bloc local_variable_components = function
        | Parsed_local_decl (_, _, expr, _, _)
        | Parsed_assignment ((_, expr), _) ->
            let variable_names = string_set_to_list (get_variables_in_parsed_boolean_expression expr) in
            let variable_refs = List.map (variable_ref_of local_variable_components) variable_names in
            component_refs := List.fold_left (Fun.flip ComponentSet.add) !component_refs variable_refs

        | Parsed_for_loop _
        | Parsed_while_loop _
        | Parsed_bloc_expr _
        | Parsed_if _
        | Parsed_bloc_void -> ()
    in
    traverse_function bin_unit right_variables_of_assignments_in_parsed_seq_code_bloc () fun_def;
    !component_refs
