open ParsingStructure;;
open ParsingStructureUtilities;;

(* Instantiation of parameters *)

type var_map = (variable_name, parsed_template_arg) Hashtbl.t

let instantiate_leaf (param_map : var_map) (leaf : parsing_structure_leaf) : parsing_structure_leaf =
  match leaf with
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

let instantiate_flows (param_map : var_map) (flows : (variable_name * NumConst.t) list) : (variable_name * NumConst.t) list =
  let instantiate_flow (clock, rate) =
    match Hashtbl.find_opt param_map clock with
    | None                 -> (clock, rate)
    | Some (Arg_name name) -> (name, rate)
    | Some _               -> failwith "[instantiate_flows]: unexpected argument for template (expecting name)"
  in
  List.map instantiate_flow flows

let rec instantiate_instructions (param_map : var_map) : parsed_instruction list -> parsed_instruction list = function
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
              | Some _ -> failwith "[instantiate_instructions]: unexpected argument for template (expecting name)"
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

let instantiate_transition (param_map : var_map) ((guard, bloc, sync, loc_name) : transition) : transition =
  let guard' = instantiate_convex_predicate param_map guard in
  let bloc' = instantiate_instructions param_map bloc in
  (guard', bloc', sync, loc_name)

let instantiate_transitions (param_map : var_map) : transition list -> transition list =
  List.map (instantiate_transition param_map)

let instantiate_loc (param_map : var_map) (loc : parsed_location) : parsed_location =
  { loc with invariant   = instantiate_convex_predicate param_map loc.invariant;
             stopped     = instantiate_stopped param_map loc.stopped;
             flow        = instantiate_flows param_map loc.flow;
             transitions = instantiate_transitions param_map loc.transitions
  }

let rename_action_decls (param_map : var_map) (automaton_name : variable_name) (action_decls : variable_name list) : variable_name list =
  let rename_action_decl action_name =
    match Hashtbl.find_opt param_map action_name with
      | None -> action_name ^ "_" ^ automaton_name
      | Some (Arg_name action_name') -> action_name'
      | Some _ -> failwith "[rename_action_decl]: unexpected argument for template (expecting name)"
  in
  List.map rename_action_decl action_decls

let rename_action_locs (param_map : var_map) (automaton_name : variable_name) (locs : parsed_location list) : parsed_location list =
  let rename_action_sync sync =
    match sync with
    | NoSync -> NoSync
    | Sync action_name ->
        match Hashtbl.find_opt param_map action_name with
          | None -> Sync (action_name ^ "_" ^ automaton_name)
          | Some (Arg_name action_name') -> Sync (action_name')
          | Some _ -> failwith "[rename_action_sync]: unexpected argument for template (expecting name)"
  in
  let rename_action_transition (guard, bloc, sync, loc_name) =
    let sync' = rename_action_sync sync in
    (guard, bloc, sync', loc_name)
  in
  let rename_action_transitions loc =
    { loc with transitions = List.map rename_action_transition loc.transitions }
  in
  List.map rename_action_transitions locs

let rename_actions (param_map : var_map) (automaton_name : variable_name) (template : parsed_template_definition) : parsed_template_definition =
  let action_decls, locs = template.template_body in
  let action_decls'      = rename_action_decls param_map automaton_name action_decls in
  let locs'              = rename_action_locs param_map automaton_name locs in
  { template with template_body = (action_decls', locs') }

(* This is just instantiation, we do type checking somewhere else *)
let instantiate_automaton (templates : parsed_template_definition list) (parsed_template_call : parsed_template_call) : parsed_automaton =
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

let instantiate_automata (templates : parsed_template_definition list) (insts : parsed_template_call list) : parsed_automaton list =
    List.map (instantiate_automaton templates) insts


