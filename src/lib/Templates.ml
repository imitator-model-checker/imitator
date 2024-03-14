open ParsingStructure;;
open ParsingStructureUtilities;;

type var_map = (variable_name, parsed_template_arg) Hashtbl.t

(* Convert a syntatic array access (`x[i]`) into the identifier following our convention (`x__i`) *)
let gen_access_id (arr_name : string) (index : int) : string = arr_name ^ "__" ^ (Int.to_string index)

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

let instantiate_array_index (param_map : var_map) (arr : variable_name) (id : variable_name) : name_or_access =
  match Hashtbl.find_opt param_map id with
    | None | Some (Arg_name _) -> failwith "[instantiate_array_index]: Not allowed to access a syntatic array with a variable."
    | Some (Arg_int i) -> Var_array_access (arr, Index_literal i)
    | Some (Arg_float _) -> failwith "[instantiate_array_index]: Not allowed to access a syntatic array with a float"
    | Some (Arg_bool _) -> failwith "[instantiate_array_index]: Not allowed to access a syntatic array with a boolean."

let instantiate_stopped (param_map : var_map) (clocks : name_or_access list) : name_or_access list =
  List.map (function
    | Var_name clock_name -> begin
      match Hashtbl.find_opt param_map clock_name with
        | None                 -> Var_name clock_name
        | Some (Arg_name name) -> Var_name name
        | Some _               ->
            failwith "[instantiate_stopped]: unexpected argument for template (expecting name)"
      (* This last case would be catched by type checking *)
    end
    | Var_array_access (arr_name, Index_literal id) -> Var_array_access (arr_name, Index_literal id)
    | Var_array_access (arr_name, Index_name id) -> instantiate_array_index param_map arr_name id
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
        | Index_name name -> begin
            match Hashtbl.find_opt param_map name with
              | None -> rate
              | Some (Arg_int i)   -> Index_literal i
              | Some (Arg_float f) -> Index_literal f
              | Some _ -> failwith "[instantiate_flows]: unexpected argument for template (expecting name)"
            end
        | Index_literal _ -> rate
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

let rename_action_decls (param_map : var_map) (automaton_name : variable_name) (action_decls : name_or_access list) : name_or_access list =
  let rename_action_decl parsed_action =
    match parsed_action with
      | Var_name action_name -> begin
          match Hashtbl.find_opt param_map action_name with
            | None -> Var_name (action_name ^ "_" ^ automaton_name)
            | Some (Arg_name action_name') -> Var_name action_name'
            | Some _ -> failwith "[rename_action_decl]: unexpected argument for template (expecting name)"
      end
      (* NOTE: We do not allow instantiation of the array of actions *)
      | Var_array_access (array_name, Index_name id_name) -> instantiate_array_index param_map array_name id_name
      | _ -> parsed_action
  in
  List.map rename_action_decl action_decls

let var_index_err_msg arr_name var_name =
  "Invalid usage of variable " ^ var_name ^ " to access array of actions " ^ arr_name ^ "."

let rename_action_locs (param_map : var_map) (automaton_name : variable_name) (locs : unexpanded_parsed_location list) : unexpanded_parsed_location list =
  let rename_action_sync sync =
    match sync with
    | UnexpandedNoSync -> UnexpandedNoSync
    | UnexpandedSync (Var_name action_name) -> begin
        match Hashtbl.find_opt param_map action_name with
          | None -> UnexpandedSync (Var_name (action_name ^ "_" ^ automaton_name))
          | Some (Arg_name action_name') -> UnexpandedSync (Var_name action_name')
          | Some _ -> failwith "[rename_action_sync]: unexpected argument for template (expecting name)"
    end
    | UnexpandedSync (Var_array_access (arr_name, (Index_literal i))) ->
        UnexpandedSync (Var_name (gen_access_id arr_name (NumConst.to_bounded_int i)))
    | UnexpandedSync (Var_array_access (arr_name, (Index_name var_name))) ->
        UnexpandedSync (instantiate_array_index param_map arr_name var_name)
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

(* The names and the corresponding array lengths of all syntatic variables in the problem *)
type synt_vars_data = (variable_name * int) list

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

let rec get_const_disc_arith_expr = function
  | Parsed_sum_diff (lhs, rhs, Parsed_plus) -> NumConst.add (get_const_disc_arith_expr lhs) (get_const_disc_term rhs) 
  | Parsed_sum_diff (lhs, rhs, Parsed_minus) -> NumConst.sub (get_const_disc_arith_expr lhs) (get_const_disc_term rhs) 
  | Parsed_term term -> get_const_disc_term term

and get_const_disc_term = function
  | Parsed_product_quotient (lhs, rhs, Parsed_mul) -> NumConst.mul (get_const_disc_term lhs) (get_const_disc_factor rhs) 
  | Parsed_product_quotient (lhs, rhs, Parsed_div) -> NumConst.div (get_const_disc_term lhs) (get_const_disc_factor rhs) 
  | Parsed_factor factor -> get_const_disc_factor factor

and get_const_disc_factor = function
  | Parsed_constant (ParsedValue.Weak_number_value value) -> value
  | Parsed_unary_min factor -> NumConst.neg (get_const_disc_factor factor)
  | Parsed_nested_expr expr -> get_const_disc_arith_expr expr
  | _ -> failwith "[get_const_disc_factor]: argument is not an integer constant"

(* Expand syntatic arrays - unfortunatelly this can't be implemented just with a map_parsed_boolean_expression *)
let rec expand_parsed_boolean_expression synt_arrays = function
  | Parsed_conj_dis (e1, e2, c) ->
      let e1' = expand_parsed_boolean_expression synt_arrays e1 in
      let e2' = expand_parsed_boolean_expression synt_arrays e2 in
      Parsed_conj_dis (e1', e2', c)
  | Parsed_discrete_bool_expr e ->
    Parsed_discrete_bool_expr (expand_parsed_discrete_boolean_expression synt_arrays e)

and expand_parsed_discrete_boolean_expression synt_arrays = function
  | Parsed_arithmetic_expr e -> Parsed_arithmetic_expr (expand_parsed_discrete_arithmetic_expression synt_arrays e)
  | Parsed_comparison (e1, op, e2) ->
      let e1' = expand_parsed_discrete_boolean_expression synt_arrays e1 in
      let e2' = expand_parsed_discrete_boolean_expression synt_arrays e2 in
      Parsed_comparison (e1', op, e2')
  | Parsed_comparison_in (e1, e2, e3) ->
      let e1' = expand_parsed_discrete_arithmetic_expression synt_arrays e1 in
      let e2' = expand_parsed_discrete_arithmetic_expression synt_arrays e2 in
      let e3' = expand_parsed_discrete_arithmetic_expression synt_arrays e3 in
      Parsed_comparison_in (e1', e2', e3')
  | Parsed_nested_bool_expr e -> Parsed_nested_bool_expr (expand_parsed_boolean_expression synt_arrays e)
  | Parsed_not e -> Parsed_not (expand_parsed_boolean_expression synt_arrays e)

and expand_parsed_discrete_arithmetic_expression synt_arrays = function
  | Parsed_sum_diff (e, t, sum_diff) ->
      let e' = expand_parsed_discrete_arithmetic_expression synt_arrays e in
      let t' = expand_parsed_discrete_term synt_arrays t in
      Parsed_sum_diff (e', t', sum_diff)
  | Parsed_term t -> Parsed_term (expand_parsed_discrete_term synt_arrays t)

and expand_parsed_discrete_term synt_arrays = function
  | Parsed_product_quotient (t, f, product_quotient) ->
      let t' = expand_parsed_discrete_term synt_arrays t in
      let f' = expand_parsed_discrete_factor synt_arrays f in
      Parsed_product_quotient (t', f', product_quotient)
  | Parsed_factor f -> Parsed_factor (expand_parsed_discrete_factor synt_arrays f)

and expand_parsed_discrete_factor synt_arrays = fun factor ->
  let get_name_of_factor = function
    | Parsed_variable (name, _) -> name
    | _ -> failwith "[expand_parsed_discrete_factor]: Name of syntatic array was not a variable name."
  in
  match factor with
    | Parsed_access (factor', index) -> begin
        let arr_name = get_name_of_factor factor' in
        match List.assoc_opt arr_name synt_arrays with
          | None -> factor
          | Some len ->
              let index_c = NumConst.to_bounded_int (get_const_disc_arith_expr index) in
              if index_c < len then
                let var_name = gen_access_id arr_name index_c in
                Parsed_variable (var_name, 0)
              else failwith "[expand_parsed_discrete_factor]: Index is greater or equal to length of syntatic array."
    end
    | _ -> factor

let expand_name_or_access = function
  | Var_name act_name -> act_name
  | Var_array_access (arr_name, Index_literal i) -> gen_access_id arr_name (NumConst.to_bounded_int i)
  | Var_array_access (arr_name, Index_name var_name) ->
      failwith "[expand_synt_arrays_automaton]: " ^ (var_index_err_msg arr_name var_name)

let expand_name_or_access_list = List.map expand_name_or_access

let expand_sync : unexpanded_sync -> sync = function
  | UnexpandedSync (Var_name act_name) -> Sync act_name
  | UnexpandedSync (Var_array_access (arr_name, Index_literal id)) ->
      Sync (gen_access_id arr_name (NumConst.to_bounded_int id))
  | UnexpandedSync (Var_array_access (arr_name, Index_name var_name)) ->
      failwith ("[expand_sync]: " ^ var_index_err_msg arr_name var_name)
  | UnexpandedNoSync -> NoSync

let expand_loc (synt_vars : synt_vars_data) (loc : unexpanded_parsed_location) : parsed_location =
  let expanded_transitions =
    List.map (fun (guard, bloc, sync, loc_name) -> guard, bloc, expand_sync sync, loc_name) loc.unexpanded_transitions
  in
  let expanded_invariant = List.map (expand_parsed_discrete_boolean_expression synt_vars) loc.unexpanded_invariant in
  let expanded_stopped = expand_name_or_access_list loc.unexpanded_stopped in
  {
    name        = loc.unexpanded_name;
    urgency     = loc.unexpanded_urgency;
    cost        = loc.unexpanded_cost;
    acceptance  = loc.unexpanded_acceptance;
    invariant   = expanded_invariant;
    stopped     = expanded_stopped;
    flow        = loc.unexpanded_flow;
    transitions = expanded_transitions;
  }

let expand_synt_arrays_automaton (synt_vars : synt_vars_data) (automaton : unexpanded_parsed_automaton) : parsed_automaton =
  let name, unexpanded_actions, unexpanded_locs = automaton in
  let expanded_actions = expand_name_or_access_list unexpanded_actions in
  let expanded_locs = List.map (expand_loc synt_vars) unexpanded_locs in
  name, expanded_actions, expanded_locs

let expand_synt_arrays_automata (synt_vars : synt_vars_data) : unexpanded_parsed_automaton list -> parsed_automaton list =
  List.map (expand_synt_arrays_automaton synt_vars)

let expand_model unexpanded_parsed_model =
  let instantiated_automata = instantiate_automata unexpanded_parsed_model.template_definitions unexpanded_parsed_model.template_calls in
  let all_automata = unexpanded_parsed_model.unexpanded_automata @ instantiated_automata in
  (* NOTE: at this point `parsed_action` calls must have an integer as argument, not a name *)
  let synt_vars =
    List.concat_map
      (fun ((len, _), names) -> List.map (fun name -> (name, len)) names)
      unexpanded_parsed_model.synt_declarations
  in
  let expanded_automata = expand_synt_arrays_automata synt_vars all_automata in
  let expanded_decls = expand_synt_decls unexpanded_parsed_model.synt_declarations in
  { controllable_actions = unexpanded_parsed_model.unexpanded_controllable_actions;
    variable_declarations = (unexpanded_parsed_model.unexpanded_variable_declarations @ expanded_decls);
    fun_definitions = unexpanded_parsed_model.unexpanded_fun_definitions;
    automata = expanded_automata;
    init_definition = unexpanded_parsed_model.unexpanded_init_definition;
  }
