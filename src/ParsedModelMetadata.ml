(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Functions that extract information on parsed model (dependency graph of variables / functions, etc.)
 *
 * File contributors : Benjamin L.
 * Created           : 2022/05/18
 *
 ************************************************************)

open ParsingStructure
open ParsingStructureUtilities
open OCamlUtilities
open ImitatorUtilities
open CustomModules

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

let string_of_component_with_attr = function
    | System_ref -> "sys [color=gray]"
    | Automaton_ref x -> "auto_" ^ x ^ "[color=red]"
    | Global_variable_ref x -> x
    | Local_variable_ref (x, _, id) -> x ^ "_" ^ string_of_int id ^ "[color=darkseagreen2]"
    | Fun_ref x -> "fun_" ^ x ^ "[color=darkseagreen][label=\"" ^ x ^ ":fun\"]"
    | Param_ref (x, function_name) -> "param_" ^ x ^ "_of_" ^ function_name ^ "[color=darkseagreen2][label=\"" ^ x ^ ":param\"]"

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
                    | Leaf_linear_constant _ -> ()
                    | Leaf_linear_variable (_, variable_name) ->
                        all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
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
                | Leaf_constant _ -> ()
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
                    | Leaf_constant _ -> ()
                ) convex_predicate;

				(* Gather in the updates *)
				print_message Verbose_total ("            Gathering variables in updates");
				let updates = ParsingStructureUtilities.updates_of_update_section update_section in

				List.iter (fun update_expression ->
					(*** NOTE: let us NOT consider that a reset is a 'use' of a variable; it must still be used in a guard, an invariant, in the right-hand side term of a reset, or a property, to be considered 'used' in the model ***)
					ParsingStructureUtilities.iterate_parsed_update (function
                        | Leaf_variable variable_name ->
                            all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                        | Leaf_fun function_name ->
                            all_relations := RelationSet.add (automaton_ref, Fun_ref function_name) !all_relations
                        | Leaf_constant _ -> ()
					) (function _ -> ()) update_expression;

                ) updates;

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

        let rec all_local_variable_of_fun_body = function
            | Parsed_fun_local_decl (variable_name, _, _, fun_body, id) ->
                    Local_variable_ref (variable_name, fun_def.name, id) ::
                    all_local_variable_of_fun_body fun_body
            | Parsed_fun_instruction _
            | Parsed_fun_expr _ -> []
        in
        all_local_variable_of_fun_body fun_def.body
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
let dependency_graph parsed_model =

    (* Function that return dependency graph of a given function definition *)
    let dependency_graph_of_function fun_def =

        (* Get parameter names *)
        let parameter_names = List.map first_of_tuple fun_def.parameters in
        (* Add parameter names to local variables of function *)
        let local_variables = List.fold_right (fun parameter_name acc -> StringMap.add parameter_name (Param_ref (parameter_name, fun_def.name)) acc) parameter_names StringMap.empty in

        (**)
        let get_variable_ref local_variables used_variable_name =
            let used_variable_ref_opt = StringMap.find_opt used_variable_name local_variables in
            match used_variable_ref_opt with
            | Some used_variable_ref -> used_variable_ref
            | None -> Global_variable_ref used_variable_name
        in

        (* TODO benjamin CLEAN comment *)
        (**)
        let variable_to_variable_relations variable_ref variables_used =
            StringSet.fold (fun used_variable_name acc ->
                let used_variable_ref = get_variable_ref local_variables used_variable_name in
                (variable_ref, used_variable_ref) :: acc
            ) variables_used []
        in

        (* TODO benjamin CLEAN comment *)
        (**)
        let variable_to_fun_relations variable_ref functions_used =
            StringSet.fold (fun used_function_name acc ->
                (variable_ref, Fun_ref used_function_name) :: acc
            ) functions_used []
        in

        (* Function that return dependency graph of a given function expression *)
        let rec dependency_graph_of_function_in_parsed_next_expr_rec local_variables = function
            | Parsed_fun_local_decl (variable_name, _, init_expr, next_expr, id) ->

                (* Create local variable ref representing a unique variable ref *)
                let variable_ref = Local_variable_ref (variable_name, fun_def.name, id) in

                (* Get variables used in the local init expression of the variable *)
                let variables_used = get_variables_in_parsed_global_expression init_expr in
                (* Get functions used in the local init expression of the variable *)
                let functions_used = get_functions_in_parsed_global_expression init_expr in

                (* For each variable used in init expression get *)
                let variable_to_variable_relations = variable_to_variable_relations variable_ref variables_used in



                let variable_to_fun_relations = StringSet.fold (fun used_function_name acc ->
                    (variable_ref, Fun_ref used_function_name) :: acc
                ) functions_used [] in

                (* Add the new declared local variable (or update if the new declaration shadows a previous one) *)
                let local_variables = StringMap.update variable_name (function None -> Some variable_ref | Some _ -> Some variable_ref) local_variables in
                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = dependency_graph_of_function_in_parsed_next_expr_rec local_variables next_expr in
                (* Concat current relations with next relations *)
                next_declaration_relations @ variable_to_variable_relations @ variable_to_fun_relations

            | Parsed_fun_instruction ((parsed_variable_update_type, expr), next_expr) ->

                let rec relations_of_variable_update = function
                    | Parsed_variable_update variable_name ->
                        (* Updated variable use all variables found in expression *)
                        (* For example x := a + b, x use a, b *)
                        (* and current function use x *)

                        (* Create local variable ref representing a unique variable ref *)
                        let variable_ref = get_variable_ref local_variables variable_name in

                        (* Get variables used in the local init expression of the variable *)
                        let variables_used = get_variables_in_parsed_global_expression expr in
                        (* Get functions used in the local init expression of the variable *)
                        let functions_used = get_functions_in_parsed_global_expression expr in

                        (* For each variable used in expression *)
                        let variable_to_variable_relations = variable_to_variable_relations variable_ref variables_used in
                        (* For each function used in expression *)
                        let variable_to_fun_relations = StringSet.fold (fun used_function_name acc ->
                            (variable_ref, Fun_ref used_function_name) :: acc
                        ) functions_used [] in

                        (* TODO benjamin REFACTOR, remove that, an instruction that use global variable that is not used elsewhere should be removed *)
                        (* Create relation between current function and assigned variable *)
                        let cur_fun_used_variable_relation = Fun_ref fun_def.name, variable_ref in
                        (* Concat all relations *)
                        cur_fun_used_variable_relation :: (variable_to_variable_relations @ variable_to_fun_relations)

                    | Parsed_indexed_update (inner_variable_update_type, index_expr) ->
                        (* Updated variable use all variables found in expression, and all variables found in index *)
                        (* For example x[y + z] = a + b, x use y, z, a, b *)
                        (* and current function use x *)
                        let variable_name = ParsingStructureUtilities.variable_name_of_parsed_variable_update_type inner_variable_update_type in

                        (* Create local variable ref representing a unique variable ref *)
                        let variable_ref = get_variable_ref local_variables variable_name in

                        (* Get variables / functions used in the indexed expression of the variable *)
                        let variables_used = get_variables_in_parsed_discrete_arithmetic_expression index_expr in
                        let functions_used = get_functions_in_parsed_discrete_arithmetic_expression index_expr in

                        (* Get relations between current variable and variables contained in indexed expression *)
                        let variable_to_variable_relations = variable_to_variable_relations variable_ref variables_used in
                        (* Get relations between current variable and functions contained in indexed expression *)
                        let variable_to_fun_relations = variable_to_fun_relations variable_ref functions_used in

                        let variable_use_indexed_variables_functions_relations = variable_to_variable_relations @ variable_to_fun_relations in
                        let variable_use_variables_relations = relations_of_variable_update inner_variable_update_type in
                        variable_use_indexed_variables_functions_relations @ variable_use_variables_relations

                    | Parsed_void_update ->
                        (* TODO benjamin IMPLEMENT *)
                        (* All variables found in expression are used by current function *)
                        (* For example: stack_pop(s) + x + y *)
                        (* Current function 'f' use x, y *)
                        []
                in
                let relations = relations_of_variable_update parsed_variable_update_type in

                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = dependency_graph_of_function_in_parsed_next_expr_rec local_variables next_expr in
                (* Concat current relations with next relations *)
                relations @ next_declaration_relations

            | Parsed_fun_expr expr ->
                (* Get variables used in the local init expression of the variable *)
                let variables_used = get_variables_in_parsed_global_expression expr in
                (* TODO benjamin REFACTOR simplify creating a function in ParsingStructureUtilities that return refs *)
                (* For each variable used in init expression get *)
                let fun_to_variable_relations = StringSet.fold (fun used_variable_name acc ->
                    let used_variable_ref = get_variable_ref local_variables used_variable_name in
                    (Fun_ref fun_def.name, used_variable_ref) :: acc
                ) variables_used []
                in
                (* Get functions used in the local init expression of the variable *)
                let functions_used = ParsingStructureUtilities.get_functions_in_parsed_global_expression expr in

                let fun_to_fun_relations = StringSet.fold (fun used_function_name acc ->
                    (Fun_ref fun_def.name, Fun_ref used_function_name) :: acc
                ) functions_used [] in

                fun_to_variable_relations @ fun_to_fun_relations
        in
        (* Get dependency graph of current function body *)
        dependency_graph_of_function_in_parsed_next_expr_rec local_variables fun_def.body
    in
    (* Get variables and functions used by automatons *)
    let dependency_graph_of_automatons =
        all_components_used_in_automatons parsed_model
    in
    (* Get variables and functions used in all declared functions *)
    let dependency_graph_of_system_functions =
        List.fold_left (fun acc fun_def -> dependency_graph_of_function fun_def @ acc) [] parsed_model.fun_definitions
    in

    (* Get variables and functions dependencies in init *)
    let dependency_graph_of_init =
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
                let used_variable_names = ParsingStructureUtilities.get_variables_in_parsed_global_expression expr in
                let used_variable_names_list = string_set_to_list used_variable_names in
                List.map (fun used_variable_name -> Global_variable_ref variable_name, Global_variable_ref used_variable_name) used_variable_names_list
            (* Linear constraint: get variables *)
            | Parsed_linear_predicate (Parsed_linear_constraint (l_expr, _, r_expr)) ->
                let left_hand_variables = string_set_to_list (ParsingStructureUtilities.get_variables_in_linear_expression l_expr) in
                let right_hand_variables = string_set_to_list (ParsingStructureUtilities.get_variables_in_linear_expression r_expr) in
                let left_and_right_variables = left_hand_variables @ right_hand_variables in
                let combination = OCamlUtilities.list_combination left_and_right_variables left_and_right_variables in
                List.map (fun (l_variable_name, r_variable_name) -> Global_variable_ref l_variable_name, Global_variable_ref r_variable_name) combination @ acc
        ) [] parsed_model.init_definition
    in
    (* Concat all dependency graphs, to get overall dependency graph of the model *)
    declared_components_of_model parsed_model, dependency_graph_of_automatons @ dependency_graph_of_system_functions @ dependency_graph_of_init

(* Get all components that are effectively used by automatons of the model *)
(* It mean all components that are reachable starting from the system reference *)
let used_components_of_model_list (_, dependency_graph) =

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
            let source_refs = List.filter (fun (a, b) -> a = used_variable_ref) dependency_graph in
            (* Compute destination refs *)
            let dest_refs = List.fold_left (fun acc s -> all_reachable_ref s @ acc) [] source_refs in
            (* Add current ref with computed destination refs *)
            used_variable_ref :: dest_refs
        )
    in

    (* Get system refs *)
    let system_refs = List.filter (fun (s, d) -> match s with Automaton_ref _ -> true | _ -> false) dependency_graph in
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
    ComponentSet.diff
        (declared_components_set)
        (used_components_set)

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