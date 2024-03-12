open ParsingStructure;;
open ParsingStructureUtilities;;

type var_map = (variable_name, parsed_template_arg) Hashtbl.t

let instantiate_leaf (param_map : var_map) : parsing_structure_leaf_modifier =
  fun leaf -> match leaf with
    | Leaf_variable (name, id) -> begin
        match Hashtbl.find_opt param_map name with
           | None                  -> leaf
           | Some (Arg_name name') -> Leaf_variable (name', id)
           | Some (Arg_int i)      -> Leaf_constant (ParsedValue.Weak_number_value i)
           | Some (Arg_float f)    -> Leaf_constant (ParsedValue.Rat_value f)
           | Some (Arg_bool b)     -> Leaf_constant (Bool_value b)
    end
    | _ -> leaf

let instantiate_discrete_boolean_expression (param_map : var_map) : parsed_discrete_boolean_expression -> parsed_discrete_boolean_expression =
  map_parsed_discrete_boolean_expression (instantiate_leaf param_map)

let instantiate_boolean_expression (param_map : var_map) : parsed_boolean_expression -> parsed_boolean_expression =
  map_parsed_boolean_expression (instantiate_leaf param_map)

let instantiate_discrete_arithmetic_expression (param_map : var_map) : parsed_discrete_arithmetic_expression -> parsed_discrete_arithmetic_expression =
  map_parsed_discrete_arithmetic_expression (instantiate_leaf param_map)

let instantiate_convex_predicate (param_map : var_map) (inv : convex_predicate) : convex_predicate =
  List.map (instantiate_discrete_boolean_expression param_map) inv

let instantiate_stopped (param_map : var_map) (clocks : variable_name list) : variable_name list =
  List.map (fun clock ->
    match Hashtbl.find_opt param_map clock with
    | None                 -> clock
    | Some (Arg_name name) -> name
    | Some _               -> failwith "[instantiate_stopped]: unexpected argument for template (expecting name)"
    (* This last case would be catched by type checking *)
  ) clocks

let instantiate_flows (param_map : var_map) (flows : parsed_flow) : parsed_flow =
  let instantiate_flow (clock, rate) =
    let clock' =
      match Hashtbl.find_opt param_map clock with
      | None                 -> clock
      | Some (Arg_name name) -> name
      | Some _               -> failwith "[instantiate_flows]: unexpected argument for template (expecting name)"
    in
    let rate' =
      match rate with
        | VarName name -> begin
            match Hashtbl.find_opt param_map name with
              | None -> rate
              | Some (Arg_int i)   -> NumLiteral i
              | Some (Arg_float f) -> NumLiteral f
              | Some _ -> failwith "[instantiate_flows]: unexpected argument for template (expecting name)"
            end
        | NumLiteral _ -> rate
    in
    (clock', rate')
  in
  List.map instantiate_flow flows

let rec instantiate_instructions (param_map : var_map) : parsed_seq_code_bloc -> parsed_seq_code_bloc =
  function
    | [] -> []
    | Parsed_local_decl ((name, id), tp, init) :: tl -> begin
        match Hashtbl.find_opt param_map name with
          | None ->
              let tl' = instantiate_instructions param_map tl in
              let init' = instantiate_boolean_expression param_map init in
              Parsed_local_decl ((name, id), tp, init') :: tl'
          | Some v ->
              Hashtbl.remove param_map name;
              let tl' = instantiate_instructions param_map tl in
              let init' = instantiate_boolean_expression param_map init in
              Hashtbl.add param_map name v;
              Parsed_local_decl ((name, id), tp, init') :: tl'
    end
    | inst :: tl ->
        let tl' = instantiate_instructions param_map tl in
        match inst with
          | Parsed_assignment (Parsed_scalar_update (name, id), rhs) -> begin
              let rhs' = instantiate_boolean_expression param_map rhs in
              match Hashtbl.find_opt param_map name with
                | None -> Parsed_assignment (Parsed_scalar_update (name, id), rhs') :: tl'
                | Some (Arg_name name') -> Parsed_assignment (Parsed_scalar_update (name', id), rhs') :: tl'
                | Some _ ->
                        failwith "[instantiate_instructions]: unexpected argument for template (expecting name)"
          end
          | Parsed_assignment (Parsed_indexed_update (name, index), rhs) ->
              let rhs' = instantiate_boolean_expression param_map rhs in
              Parsed_assignment (Parsed_indexed_update (name, index), rhs') :: tl'
          | Parsed_instruction expr ->
              let expr' = instantiate_boolean_expression param_map expr in
              Parsed_instruction expr' :: tl'
          | Parsed_for_loop ((name, id), left_expr, right_expr, up_down, body) -> begin
              match Hashtbl.find_opt param_map name with
                | None ->
                    let left_expr' = instantiate_discrete_arithmetic_expression param_map left_expr in
                    let right_expr' = instantiate_discrete_arithmetic_expression param_map right_expr in
                    let body' = instantiate_instructions param_map body in
                    Parsed_for_loop ((name, id), left_expr', right_expr', up_down, body') :: tl'
                | Some v ->
                    (* No need to isolate this one since the local variable is only scoped at `body` *)
                    let left_expr' = instantiate_discrete_arithmetic_expression param_map left_expr in
                    let right_expr' = instantiate_discrete_arithmetic_expression param_map right_expr in
                    Hashtbl.remove param_map name;
                    let body' = instantiate_instructions param_map body in
                    Hashtbl.add param_map name v;
                    Parsed_for_loop ((name, id), left_expr', right_expr', up_down, body') :: tl'
          end
          | Parsed_while_loop (cond, body) ->
             let cond' = instantiate_boolean_expression param_map cond in
             let body' = instantiate_instructions param_map body in
             Parsed_while_loop (cond', body') :: tl'
          | Parsed_if (cond, then_branch, else_branch_opt) ->
              let cond' = instantiate_boolean_expression param_map cond in
              let then_branch' = instantiate_instructions param_map then_branch in
              let else_branch_opt' = Option.map (instantiate_instructions param_map) else_branch_opt in
              Parsed_if (cond', then_branch', else_branch_opt') :: tl'
          | Parsed_local_decl _ ->
              raise (Exceptions.InternalError "[instantiate_instructions]: This point of code is unreachable") 

let instantiate_transition (param_map : var_map) ((guard, bloc, sync, loc_name) : unexpanded_transition) : unexpanded_transition =
  let guard' = instantiate_convex_predicate param_map guard in
  let bloc' = instantiate_instructions param_map bloc in
  (guard', bloc', sync, loc_name)

let instantiate_transitions (param_map : var_map) : unexpanded_transition list -> unexpanded_transition list =
  List.map (instantiate_transition param_map)

let instantiate_loc (param_map : var_map) (loc : unexpanded_parsed_location) : unexpanded_parsed_location =
  { loc with unexpanded_invariant   = instantiate_convex_predicate param_map loc.unexpanded_invariant;
             unexpanded_stopped     = instantiate_stopped param_map loc.unexpanded_stopped;
             unexpanded_flow        = instantiate_flows param_map loc.unexpanded_flow;
             unexpanded_transitions = instantiate_transitions param_map loc.unexpanded_transitions
  }

let rename_action_decls (param_map : var_map) (automaton_name : variable_name) (action_decls : parsed_action list) : parsed_action list =
  let rename_action_decl parsed_action =
    match parsed_action with
      | Action_name action_name -> begin
          match Hashtbl.find_opt param_map action_name with
            | None -> Action_name (action_name ^ "_" ^ automaton_name)
            | Some (Arg_name action_name') -> Action_name action_name'
            | Some _ -> failwith "[rename_action_decl]: unexpected argument for template (expecting name)"
      end
      (* NOTE: We do not allow instantiation of the array of actions *)
      | Action_array_access (array_name, VarName id_name) -> begin
          match Hashtbl.find_opt param_map id_name with
            | None | Some (Arg_name _) -> failwith "[rename_action_decl]: forbidden variable index in syntatic action array."
            | Some (Arg_int c) | Some (Arg_float c) -> Action_array_access (array_name, NumLiteral c)
            | Some (Arg_bool _) -> failwith "[rename_action_decl]: Not allowed to access an array with a boolean."
      end
      | _ -> parsed_action
  in
  List.map rename_action_decl action_decls

let var_index_err_msg arr_name var_name =
  "Invalid usage of variable " ^ var_name ^ " to access array of actions " ^ arr_name ^ "."

(* Convert a syntatic array access (`x[i]`) into the identifier following our convention (`x__i`) *)
let gen_access_id (arr_name : string) (index : int) : string = arr_name ^ "__" ^ (Int.to_string index)

let rename_action_locs (param_map : var_map) (automaton_name : variable_name) (locs : unexpanded_parsed_location list) : unexpanded_parsed_location list =
  let rename_action_sync sync =
    match sync with
    | UnexpandedNoSync -> UnexpandedNoSync
    | UnexpandedSync (Action_name action_name) -> begin
        match Hashtbl.find_opt param_map action_name with
          | None -> UnexpandedSync (Action_name (action_name ^ "_" ^ automaton_name))
          | Some (Arg_name action_name') -> UnexpandedSync (Action_name action_name')
          | Some _ -> failwith "[rename_action_sync]: unexpected argument for template (expecting name)"
    end
    | UnexpandedSync (Action_array_access (arr_name, (NumLiteral i))) ->
        UnexpandedSync (Action_name (gen_access_id arr_name (NumConst.to_bounded_int i)))
    | UnexpandedSync (Action_array_access (arr_name, (VarName var_name))) ->
        match Hashtbl.find_opt param_map var_name with
          | Some (Arg_int i)         -> UnexpandedSync (Action_name (gen_access_id arr_name (NumConst.to_bounded_int i)))
          | Some (Arg_name var_name) -> failwith ("[rename_action_sync]: " ^ var_index_err_msg arr_name var_name)
          | Some (Arg_float f)       -> failwith ("[rename_action_sync]: Cannot access array " ^ arr_name ^ " with float value " ^ (NumConst.to_string f))
          | Some (Arg_bool b)        -> failwith ("[rename_action_sync]: Cannot access array " ^ arr_name ^ " with bool value " ^ (Bool.to_string b))
          | None                     -> failwith ("[rename_action_sync]: Unknown variable " ^ var_name)
  in
  let rename_action_transition (guard, bloc, sync, loc_name) =
    let sync' = rename_action_sync sync in
    (guard, bloc, sync', loc_name)
  in
  let rename_action_transitions loc =
    { loc with unexpanded_transitions = List.map rename_action_transition loc.unexpanded_transitions }
  in
  List.map rename_action_transitions locs

let rename_actions (param_map : var_map) (automaton_name : variable_name) (template : parsed_template_definition) : parsed_template_definition =
  let action_decls, locs = template.template_body in
  let action_decls'      = rename_action_decls param_map automaton_name action_decls in
  let locs'              = rename_action_locs param_map automaton_name locs in
  { template with template_body = (action_decls', locs') }

(* This is just instantiation, we do type checking somewhere else *)
let instantiate_automaton (templates : parsed_template_definition list) (parsed_template_call : parsed_template_call) : unexpanded_parsed_automaton =
  let automaton_name, template_name, args = parsed_template_call in
  let template                       = List.find (fun t -> t.template_name = template_name) templates in
  if List.length template.template_parameters <> List.length args then
    failwith ("[instantiate_automaton]: The number of arguments provided for " ^ automaton_name ^ " is incorrect.");
  let param_names                    = List.map fst template.template_parameters in
  let param_map                      = Hashtbl.of_seq (List.to_seq (List.combine param_names args)) in
  (* Replace action names *)
  let template'                      = rename_actions param_map automaton_name template in
  let renamed_actions, renamed_locs  = template'.template_body in
  (* Instantiate other parameters *)
  let instantiated_locs              = List.map (instantiate_loc param_map) renamed_locs in
  (automaton_name, renamed_actions, instantiated_locs)

let instantiate_automata (templates : parsed_template_definition list) (insts : parsed_template_call list) : unexpanded_parsed_automaton list =
  List.map (instantiate_automaton templates) insts

let expand_synt_decls (synt_decls : synt_var_decl list) : variable_declarations =
  let aux ((len, kind), names) =
    let ids = List.init len Fun.id in
    let expand_name name =
      List.fold_left (fun acc i -> gen_access_id name i :: acc) [] ids in
    let expanded_names = List.concat_map expand_name names in
    let packed_expanded_names = List.map (fun name -> (name, None)) expanded_names in
    match kind with
      | Clock_synt_array ->
          Some (DiscreteType.Var_type_clock, packed_expanded_names)
      | Action_synt_array -> None
  in
  List.filter_map aux synt_decls

let expand_action = function
  | Action_name act_name -> act_name
  | Action_array_access (arr_name, NumLiteral i) -> gen_access_id arr_name (NumConst.to_bounded_int i)
  | Action_array_access (arr_name, VarName var_name) ->
      failwith "[expand_synt_arrays_automaton]: " ^ (var_index_err_msg arr_name var_name)

let expand_sync : unexpanded_sync -> sync = function
  | UnexpandedSync (Action_name act_name) -> Sync act_name
  | UnexpandedSync (Action_array_access (arr_name, NumLiteral id)) ->
      Sync (gen_access_id arr_name (NumConst.to_bounded_int id))
  | UnexpandedSync (Action_array_access (arr_name, VarName var_name)) ->
      failwith ("[expand_sync]: " ^ var_index_err_msg arr_name var_name)
  | UnexpandedNoSync -> NoSync

let expand_loc (loc : unexpanded_parsed_location) : parsed_location =
  let expanded_transitions =
    List.map (fun (guard, bloc, sync, loc_name) -> guard, bloc, expand_sync sync, loc_name) loc.unexpanded_transitions
  in
  {
    name        = loc.unexpanded_name;
    urgency     = loc.unexpanded_urgency;
    cost        = loc.unexpanded_cost;
    acceptance  = loc.unexpanded_acceptance;
    invariant   = loc.unexpanded_invariant;
    stopped     = loc.unexpanded_stopped;
    flow        = loc.unexpanded_flow;
    transitions = expanded_transitions;
  }

let expand_synt_arrays_automaton (automaton : unexpanded_parsed_automaton) : parsed_automaton =
  let name, unexpanded_actions, unexpanded_locs = automaton in
  let expanded_actions = List.map expand_action unexpanded_actions in
  let expanded_locs = List.map expand_loc unexpanded_locs in
  name, expanded_actions, expanded_locs

let expand_synt_arrays_automata : unexpanded_parsed_automaton list -> parsed_automaton list =
  List.map expand_synt_arrays_automaton

let expand_model unexpanded_parsed_model =
  let instantiated_automata = instantiate_automata unexpanded_parsed_model.template_definitions unexpanded_parsed_model.template_calls in
  let all_automata = unexpanded_parsed_model.unexpanded_automata @ instantiated_automata in
  (* NOTE: at this point `parsed_action` calls must have an integer as argument, not a name *)
  let expanded_automata = expand_synt_arrays_automata all_automata in
  let expanded_decls = expand_synt_decls unexpanded_parsed_model.synt_declarations in
  { controllable_actions = unexpanded_parsed_model.unexpanded_controllable_actions;
    variable_declarations = (unexpanded_parsed_model.unexpanded_variable_declarations @ expanded_decls);
    fun_definitions = unexpanded_parsed_model.unexpanded_fun_definitions;
    automata = expanded_automata;
    init_definition = unexpanded_parsed_model.unexpanded_init_definition;
  }
