open ParsingStructure;;
open ParsingStructureUtilities;;

(* Utilities *)

(* The names and the corresponding array lengths of all syntatic variables in the problem *)
type synt_vars_data = (variable_name * synt_var_kind * int) list

type var_map = (variable_name, parsed_template_arg) Hashtbl.t

let eval_expr_err_msg = "[eval_boolean_expression]: Trying to evaluate an expression whose value is not known at compile time."

let rec eval_parsed_boolean_expression g_decls = function
  | Parsed_discrete_bool_expr e -> eval_parsed_discrete_bool_expr g_decls e
  | Parsed_conj_dis _ -> failwith eval_expr_err_msg

and eval_parsed_discrete_bool_expr g_decls = function
  | Parsed_arithmetic_expr e -> eval_parsed_arithmetic_expr g_decls e
  | _ -> failwith eval_expr_err_msg

and eval_parsed_arithmetic_expr g_decls = function
  | Parsed_sum_diff (arith_expr, term, Parsed_plus) -> NumConst.add (eval_parsed_arithmetic_expr g_decls arith_expr) (eval_parsed_term g_decls term)
  | Parsed_sum_diff (arith_expr, term, Parsed_minus) -> NumConst.sub (eval_parsed_arithmetic_expr g_decls arith_expr) (eval_parsed_term g_decls term)
  | Parsed_term t -> eval_parsed_term g_decls t

and eval_parsed_term g_decls = function
  | Parsed_product_quotient (term, factor, Parsed_mul) -> NumConst.mul (eval_parsed_term g_decls term) (eval_parsed_factor g_decls factor)
  | Parsed_product_quotient (term, factor, Parsed_div) -> NumConst.div (eval_parsed_term g_decls term) (eval_parsed_factor g_decls factor)
  | Parsed_factor factor -> eval_parsed_factor g_decls factor

and eval_parsed_factor g_decls = function
  | Parsed_constant v -> ParsedValue.to_numconst_value v
  | Parsed_variable (name, _) -> expand_const_var g_decls name
  | Parsed_nested_expr expr -> eval_parsed_arithmetic_expr g_decls expr
  | Parsed_unary_min f -> NumConst.neg (eval_parsed_factor g_decls f)
  | _ -> failwith eval_expr_err_msg

and expand_const_var g_decls name =
  let inspect_decl (name', expr_opt) = if name = name' then expr_opt else None in
  let inspect_decls_of_type (_, decls_of_type) = List.find_map inspect_decl decls_of_type in
  let inspect_all_decls decls =
    List.find_map Fun.id (List.map inspect_decls_of_type decls)
  in
  match inspect_all_decls g_decls with
    | None -> failwith eval_expr_err_msg
    | Some expr -> eval_parsed_boolean_expression g_decls expr

let find_arr_len_opt arr_name =
  List.find_map (fun (name, _, len) -> if arr_name = name then Some len else None)

let gen_var_from_access g_decls def gen_var_from_name arr_name index synt_arrays =
  let arr_len_opt = find_arr_len_opt arr_name synt_arrays in
  match arr_len_opt with
    | None -> def
    | Some len ->
        let index_c = eval_parsed_arithmetic_expr g_decls index |> NumConst.to_bounded_int in
        if index_c < len then
          let var_name = gen_access_id arr_name index_c in
          gen_var_from_name var_name
        else failwith "[expand_parsed_discrete_factor]: Index is greater or equal to length of syntatic array."

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

let rec instantiate_linear_term (param_map : var_map) (term : unexpanded_linear_term) : unexpanded_linear_term =
  match term with
    | Unexpanded_constant _ -> term
    | Unexpanded_variable (c, Var_name name) -> begin
        match Hashtbl.find_opt param_map name with
          | None ->  Unexpanded_variable (c, Var_name name)
          | Some (Arg_name name') -> Unexpanded_variable (c, Var_name name')
          | Some (Arg_int i_val) -> Unexpanded_constant i_val
          | Some (Arg_float f_val) -> Unexpanded_constant f_val
          | Some (Arg_bool _) -> failwith "[instantiate_linear_term]: boolean variable in linear constraint."
    end
    | Unexpanded_variable (c, Var_array_access (arr_name, index)) ->
        let index' = instantiate_discrete_arithmetic_expression param_map index in
        Unexpanded_variable (c, Var_array_access (arr_name, index'))

and instantiate_linear_expression (param_map : var_map) (expr : unexpanded_linear_expression) : unexpanded_linear_expression =
  match expr with
    | Unexpanded_linear_term t -> Unexpanded_linear_term (instantiate_linear_term param_map t)
    | Unexpanded_linear_plus_expression (e, t) ->
        let e' = instantiate_linear_expression param_map e in
        let t' = instantiate_linear_term param_map t in
        Unexpanded_linear_plus_expression (e', t')
    | Unexpanded_linear_minus_expression (e, t) ->
        let e' = instantiate_linear_expression param_map e in
        let t' = instantiate_linear_term param_map t in
        Unexpanded_linear_minus_expression (e', t')

and instantiate_linear_constraint (param_map : var_map) (constr : unexpanded_linear_constraint) : unexpanded_linear_constraint =
  match constr with
    | Unexpanded_parsed_true_constraint -> Unexpanded_parsed_true_constraint
    | Unexpanded_parsed_false_constraint -> Unexpanded_parsed_false_constraint
    | Unexpanded_parsed_linear_constraint (e1, relop, e2) ->
        let e1' = instantiate_linear_expression param_map e1 in
        let e2' = instantiate_linear_expression param_map e2 in
        Unexpanded_parsed_linear_constraint (e1', relop, e2')

(* Returns a list with all indices inside the forall range *)
let indices_from_forall_index_data g_decls forall_index_data =
  let { forall_index_name = _; forall_lb; forall_ub } = forall_index_data in
  let forall_lb_val = eval_parsed_arithmetic_expr g_decls forall_lb |> NumConst.to_bounded_int in
  let forall_ub_val = eval_parsed_arithmetic_expr g_decls forall_ub |> NumConst.to_bounded_int in
  List.init (forall_ub_val - forall_lb_val + 1) (fun i -> i + forall_lb_val)

let gen_aux_var_tbl i index_data =
  Hashtbl.of_seq (List.to_seq [(index_data.forall_index_name, Arg_int (NumConst.numconst_of_int i))])

(*****************************************************************************)
(* Instantiation of templates *)
(*****************************************************************************)

let instantiate_stopped_clock (param_map : var_map) : name_or_access -> name_or_access =
  function
    | Var_name clock_name -> begin
      match Hashtbl.find_opt param_map clock_name with
        | None                 -> Var_name clock_name
        | Some (Arg_name name) -> Var_name name
        | Some _               ->
            failwith "[instantiate_stopped_clock]: unexpected argument for template (expecting name)"
      (* This last case would be catched by type checking *)
    end
    | Var_array_access (arr, index) -> Var_array_access (arr, instantiate_discrete_arithmetic_expression param_map index)

let instantiate_stopped_clocks (param_map : var_map) : name_or_access list -> name_or_access list =
  List.map (instantiate_stopped_clock param_map)

let instantiate_flows (param_map : var_map) (flows : unexpanded_parsed_flow) : unexpanded_parsed_flow =
  let instantiate_flow (clock, rate) =
    let clock' =
      match clock with
        | Var_name clock_name -> begin
          match Hashtbl.find_opt param_map clock_name with
            | None                 -> clock
            | Some (Arg_name clock_name') -> Var_name clock_name'
            | Some _               -> failwith "[instantiate_flows]: unexpected argument for template (expecting name)"
        end
        | Var_array_access (arr, index) -> Var_array_access (arr, instantiate_discrete_arithmetic_expression param_map index)
    in
    let rate' = instantiate_discrete_arithmetic_expression param_map rate in
    (clock', rate')
  in
  List.map instantiate_flow flows

let rec instantiate_indexed_update param_map =
  function
    | Parsed_scalar_update (name, id) -> begin
        match Hashtbl.find_opt param_map name with
          | None -> Parsed_scalar_update (name, id)
          | Some (Arg_name name') -> Parsed_scalar_update (name', id)
          | Some _ ->
              failwith "[instantiate_instructions]: unexpected argument for template (expecting name)"
    end
    | Parsed_indexed_update (arr, index) ->
        let index' = instantiate_discrete_arithmetic_expression param_map index in
        let arr' = instantiate_indexed_update param_map arr in
        Parsed_indexed_update (arr', index')

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
          | Parsed_assignment (indexed_update, rhs) ->
              let indexed_update' = instantiate_indexed_update param_map indexed_update in
              let rhs' = instantiate_boolean_expression param_map rhs in
              Parsed_assignment (indexed_update', rhs') :: tl'
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

let instantiate_action (param_map : var_map) : name_or_access -> name_or_access = fun action ->
  match action with
    | Var_name name -> begin
        match Hashtbl.find_opt param_map name with
          | Some (Arg_name name') -> Var_name name'
          | None -> Var_name name
          | _ -> failwith "[instantiate_action]: Argument of type action must be a name."
    end
    | Var_array_access (arr, index) -> Var_array_access (arr, instantiate_discrete_arithmetic_expression param_map index)

let instantiate_actions (param_map : var_map) : name_or_access list -> name_or_access list =
  List.map (instantiate_action param_map)

let instantiate_sync (param_map : var_map) : unexpanded_sync -> unexpanded_sync =
  function
    | UnexpandedSync action -> UnexpandedSync (instantiate_action param_map action)
    | UnexpandedNoSync -> UnexpandedNoSync

let instantiate_transition (param_map : var_map) ((guard, bloc, sync, loc_name) : unexpanded_transition) : unexpanded_transition =
  let guard' = instantiate_convex_predicate param_map guard in
  let bloc' = instantiate_instructions param_map bloc in
  let sync' = instantiate_sync param_map sync in
  (guard', bloc', sync', loc_name)

let instantiate_transitions (param_map : var_map) : unexpanded_transition list -> unexpanded_transition list =
  List.map (instantiate_transition param_map)

let instantiate_loc (param_map : var_map) (loc : unexpanded_parsed_location) : unexpanded_parsed_location =
  { loc with unexpanded_invariant   = instantiate_convex_predicate param_map loc.unexpanded_invariant;
             unexpanded_stopped     = instantiate_stopped_clocks param_map loc.unexpanded_stopped;
             unexpanded_flow        = instantiate_flows param_map loc.unexpanded_flow;
             unexpanded_transitions = instantiate_transitions param_map loc.unexpanded_transitions
  }

(* This is just instantiation, we do type checking somewhere else *)
let instantiate_automaton (templates : parsed_template_definition list) (parsed_template_call : parsed_template_call) : unexpanded_parsed_automaton =
  let automaton_name, template_name, args = parsed_template_call in
  let template                       = List.find (fun t -> t.template_name = template_name) templates in
  if List.length template.template_parameters <> List.length args then
    failwith ("[instantiate_automaton]: The number of arguments provided for " ^ automaton_name ^ " is incorrect.");
  let param_names                    = List.map fst template.template_parameters in
  let param_map                      = Hashtbl.of_seq (List.to_seq (List.combine param_names args)) in
  let actions, locs                  = template.template_body in
  let instantiated_actions           = instantiate_actions param_map actions in
  (* Instantiate other parameters *)
  let instantiated_locs              = List.map (instantiate_loc param_map) locs in
  (automaton_name, instantiated_actions, instantiated_locs)

let instantiate_automata (templates : parsed_template_definition list) (insts : parsed_template_call list) : unexpanded_parsed_automaton list =
  List.map (instantiate_automaton templates) insts

(*****************************************************************************)
(* Expansion of syntatic arrays *)
(*****************************************************************************)

let expand_synt_decls (synt_decls : synt_vars_data) : variable_declarations =
  let aux (name, kind, len) =
    if len < 0 then
      raise (Failure "Length of array should be a positive integer.")
    else
      let ids = List.init len Fun.id in
      let access_ids = List.map (fun i -> (gen_access_id name i, None)) ids in
      match kind with
        | Clock_synt_array -> Some (DiscreteType.Var_type_clock, access_ids)
        | Param_synt_array -> Some (DiscreteType.Var_type_parameter, access_ids)
        | Action_synt_array -> None
  in
  List.filter_map aux synt_decls

let expand_name_or_access g_decls = function
  | Var_name name -> name
  | Var_array_access (arr, index) -> gen_access_id arr (NumConst.to_bounded_int (eval_parsed_arithmetic_expr g_decls index))

let expand_name_or_access_list g_decls = List.map (expand_name_or_access g_decls)

let rec expand_linear_term g_decls = function
  | Unexpanded_constant c -> Constant c
  | Unexpanded_variable (c, name_or_access) ->
      Variable (c, expand_name_or_access g_decls name_or_access)

and expand_linear_expression g_decls = function
  | Unexpanded_linear_term t -> Linear_term (expand_linear_term g_decls t)
  | Unexpanded_linear_plus_expression (e, t) ->
      let e' = expand_linear_expression g_decls e in
      let t' = expand_linear_term g_decls t in
      Linear_plus_expression (e', t')
  | Unexpanded_linear_minus_expression (e, t) ->
      let e' = expand_linear_expression g_decls e in
      let t' = expand_linear_term g_decls t in
      Linear_minus_expression (e', t')

and expand_linear_constraint g_decls = function
  | Unexpanded_parsed_true_constraint -> Parsed_true_constraint
  | Unexpanded_parsed_false_constraint -> Parsed_false_constraint
  | Unexpanded_parsed_linear_constraint (e1, relop, e2) ->
      let e1' = expand_linear_expression g_decls e1 in
      let e2' = expand_linear_expression g_decls e2 in
      Parsed_linear_constraint (e1', relop, e2')

(* Expand syntatic arrays - unfortunatelly this can't be implemented just with a map_parsed_boolean_expression *)
let rec expand_parsed_boolean_expression g_decls synt_arrays = function
  | Parsed_conj_dis (e1, e2, c) ->
      let e1' = expand_parsed_boolean_expression g_decls synt_arrays e1 in
      let e2' = expand_parsed_boolean_expression g_decls synt_arrays e2 in
      Parsed_conj_dis (e1', e2', c)
  | Parsed_discrete_bool_expr e ->
    Parsed_discrete_bool_expr (expand_parsed_discrete_boolean_expression g_decls synt_arrays e)

and expand_parsed_discrete_boolean_expression g_decls synt_arrays = function
  | Parsed_arithmetic_expr e -> Parsed_arithmetic_expr (expand_parsed_discrete_arithmetic_expression g_decls synt_arrays e)
  | Parsed_comparison (e1, op, e2) ->
      let e1' = expand_parsed_discrete_boolean_expression g_decls synt_arrays e1 in
      let e2' = expand_parsed_discrete_boolean_expression g_decls synt_arrays e2 in
      Parsed_comparison (e1', op, e2')
  | Parsed_comparison_in (e1, e2, e3) ->
      let e1' = expand_parsed_discrete_arithmetic_expression g_decls synt_arrays e1 in
      let e2' = expand_parsed_discrete_arithmetic_expression g_decls synt_arrays e2 in
      let e3' = expand_parsed_discrete_arithmetic_expression g_decls synt_arrays e3 in
      Parsed_comparison_in (e1', e2', e3')
  | Parsed_nested_bool_expr e -> Parsed_nested_bool_expr (expand_parsed_boolean_expression g_decls synt_arrays e)
  | Parsed_not e -> Parsed_not (expand_parsed_boolean_expression g_decls synt_arrays e)

and expand_parsed_discrete_arithmetic_expression g_decls synt_arrays = function
  | Parsed_sum_diff (e, t, sum_diff) ->
      let e' = expand_parsed_discrete_arithmetic_expression g_decls synt_arrays e in
      let t' = expand_parsed_discrete_term g_decls synt_arrays t in
      Parsed_sum_diff (e', t', sum_diff)
  | Parsed_term t -> Parsed_term (expand_parsed_discrete_term g_decls synt_arrays t)

and expand_parsed_discrete_term g_decls synt_arrays = function
  | Parsed_product_quotient (t, f, product_quotient) ->
      let t' = expand_parsed_discrete_term g_decls synt_arrays t in
      let f' = expand_parsed_discrete_factor g_decls synt_arrays f in
      Parsed_product_quotient (t', f', product_quotient)
  | Parsed_factor f -> Parsed_factor (expand_parsed_discrete_factor g_decls synt_arrays f)

and expand_parsed_discrete_factor g_decls synt_arrays = fun factor ->
  let get_name_of_factor = function
    | Parsed_variable (name, _) -> name
    | _ -> failwith "[expand_parsed_discrete_factor]: Name of syntatic array was not a variable name."
  in
  match factor with
    | Parsed_access (factor', index) ->
        let arr_name = get_name_of_factor factor' in
        let gen_var_from_name name = Parsed_variable (name, 0) in
        gen_var_from_access g_decls factor gen_var_from_name arr_name index synt_arrays
    | _ -> factor

let expand_sync (g_decls : variable_declarations) : unexpanded_sync -> sync = function
  | UnexpandedSync action -> Sync (expand_name_or_access g_decls action)
  | UnexpandedNoSync -> NoSync

let expand_indexed_update (g_decls : variable_declarations) (synt_arrays : synt_vars_data) : parsed_scalar_or_index_update_type -> parsed_scalar_or_index_update_type =
  fun e ->
    match e with
      | Parsed_indexed_update (Parsed_scalar_update (name, _), index) -> begin
          let gen_var_from_name name = Parsed_scalar_update (name, 0) in
          gen_var_from_access g_decls e gen_var_from_name name index synt_arrays
      end
      | _ -> e

let rec expand_instruction (g_decls : variable_declarations) (synt_vars : synt_vars_data) : parsed_instruction -> parsed_instruction = function
  | Parsed_local_decl (name, tp, init) ->
      let init' = expand_parsed_boolean_expression g_decls synt_vars init in
      Parsed_local_decl (name, tp, init')
  | Parsed_assignment (upd, rhs) ->
      let rhs' = expand_parsed_boolean_expression g_decls synt_vars rhs in
      let upd' = expand_indexed_update g_decls synt_vars upd in
      Parsed_assignment (upd', rhs')
  | Parsed_instruction expr -> Parsed_instruction (expand_parsed_boolean_expression g_decls synt_vars expr)
  | Parsed_for_loop (var_ref, left_expr, right_expr, dir, bloc) ->
      let left_expr' = expand_parsed_discrete_arithmetic_expression g_decls synt_vars left_expr in
      let right_expr' = expand_parsed_discrete_arithmetic_expression g_decls synt_vars right_expr in
      let bloc' = expand_code_bloc g_decls synt_vars bloc in
      Parsed_for_loop (var_ref, left_expr', right_expr', dir, bloc')
  | Parsed_while_loop (cond, bloc) ->
      let cond' = expand_parsed_boolean_expression g_decls synt_vars cond in
      let bloc' = expand_code_bloc g_decls synt_vars bloc in
      Parsed_while_loop (cond', bloc')
  | Parsed_if (cond, then_branch, else_branch_opt) ->
      let cond' = expand_parsed_boolean_expression g_decls synt_vars cond in
      let then_branch' = expand_code_bloc g_decls synt_vars then_branch in
      let else_branch_opt' = Option.map (fun expr -> expand_code_bloc g_decls synt_vars expr) else_branch_opt in
      Parsed_if (cond', then_branch', else_branch_opt')

and expand_code_bloc (g_decls : variable_declarations) (synt_vars : synt_vars_data) : parsed_seq_code_bloc -> parsed_seq_code_bloc =
  List.map (expand_instruction g_decls synt_vars)

let expand_loc (g_decls : variable_declarations) (synt_vars : synt_vars_data) (loc : unexpanded_parsed_location) : parsed_location =
  let expand_transition (guard, bloc, sync, loc_name) =
    let sync' = expand_sync g_decls sync in
    let guard' = List.map (expand_parsed_discrete_boolean_expression g_decls synt_vars) guard in
    let bloc' = expand_code_bloc g_decls synt_vars bloc in
    (guard', bloc', sync', loc_name)
  in
  let expanded_transitions = List.map expand_transition loc.unexpanded_transitions in
  let expanded_invariant = List.map (expand_parsed_discrete_boolean_expression g_decls synt_vars) loc.unexpanded_invariant in
  let expanded_stopped = expand_name_or_access_list g_decls loc.unexpanded_stopped in
  let expand_flow (clock, rate) =
    (* Rate must be a constant expression at this point, since we already instantiated template vars *)
    let expanded_rate = eval_parsed_arithmetic_expr g_decls rate in
    match clock with
      | Var_name name -> (name, expanded_rate)
      | Var_array_access (arr, index) -> (gen_access_id arr (NumConst.to_bounded_int (eval_parsed_arithmetic_expr g_decls index)), expanded_rate)
  in
  let expanded_flow = List.map expand_flow loc.unexpanded_flow in
  {
    name        = loc.unexpanded_name;
    urgency     = loc.unexpanded_urgency;
    cost        = Option.map (expand_linear_expression g_decls) loc.unexpanded_cost;
    acceptance  = loc.unexpanded_acceptance;
    invariant   = expanded_invariant;
    stopped     = expanded_stopped;
    flow        = expanded_flow;
    transitions = expanded_transitions;
  }

let expand_synt_arrays_automaton (g_decls : variable_declarations) (synt_vars : synt_vars_data) (automaton : unexpanded_parsed_automaton) : parsed_automaton =
  let name, unexpanded_actions, unexpanded_locs = automaton in
  let expanded_actions = expand_name_or_access_list g_decls unexpanded_actions in
  let expanded_locs = List.map (expand_loc g_decls synt_vars) unexpanded_locs in
  name, expanded_actions, expanded_locs

let expand_synt_arrays_automata (g_decls : variable_declarations) (synt_vars : synt_vars_data) : unexpanded_parsed_automaton list -> parsed_automaton list =
  List.map (expand_synt_arrays_automaton g_decls synt_vars)

let expand_forall_call (g_decls: variable_declarations) (call_data: parsed_forall_template_call) =
  let { forall_index_data; forall_template; forall_aut_name; forall_args } = call_data in
  let indices = indices_from_forall_index_data g_decls forall_index_data in
  let instantiate_arg idx = fun arg ->
    match arg with
      | Arg_name name ->
          if name = forall_index_data.forall_index_name then
            Arg_int (NumConst.numconst_of_int idx)
          else arg
      | _ -> arg
  in
  let build_call idx =
    let aut_name = gen_access_id forall_aut_name idx in
    let args = List.map (instantiate_arg idx) forall_args in
    (aut_name, forall_template, args)
  in
  List.map build_call indices

let expand_init_state_predicate (g_decls: variable_declarations) (pred: unexpanded_parsed_init_state_predicate): parsed_init_state_predicate list =
  match pred with
    | Unexpanded_parsed_forall_loc_assignment (index_data, arr_name, arr_idx, loc_name) ->
        let instantiate_arr_idx i arr_idx =
          let aux_var_tbl = gen_aux_var_tbl i index_data in
          (* instantiate index expression with respect to the forall variable *)
          instantiate_discrete_arithmetic_expression aux_var_tbl arr_idx |>
          (* ... then evaluate it, to obtain a concrete number *)
          eval_parsed_arithmetic_expr g_decls |>
          NumConst.to_bounded_int
        in
        indices_from_forall_index_data g_decls index_data |>
        List.map (fun i -> Parsed_loc_assignment (gen_access_id arr_name (instantiate_arr_idx i arr_idx), loc_name))
    | Unexpanded_parsed_loc_assignment (aut, loc) -> [Parsed_loc_assignment (aut, loc)]
    | Unexpanded_parsed_linear_predicate constr -> [Parsed_linear_predicate (expand_linear_constraint g_decls constr)]
    | Unexpanded_parsed_forall_linear_predicate (index_data, constr) ->
        let instantiate_constr i constr =
          let aux_var_tbl = gen_aux_var_tbl i index_data in
          instantiate_linear_constraint aux_var_tbl constr |>
          expand_linear_constraint g_decls
        in
        indices_from_forall_index_data g_decls index_data |>
        List.map (fun i -> Parsed_linear_predicate (instantiate_constr i constr))
    | Unexpanded_parsed_discrete_predicate (name, bool_expr) ->
        [Parsed_discrete_predicate (name, bool_expr)]

let expand_init_definition (g_decls: variable_declarations): unexpanded_parsed_init_state_predicate list -> parsed_init_state_predicate list =
  List.concat_map (expand_init_state_predicate g_decls)

let expand_model (model : unexpanded_parsed_model) : parsed_model =
  let g_decls = model.unexpanded_variable_declarations in

  (* Expand foralls *)
  let forall_calls =
    model.forall_template_calls |>
    List.concat_map (expand_forall_call g_decls)
  in
  let all_calls = model.template_calls @ forall_calls in
  let instantiated_automata = instantiate_automata model.template_definitions all_calls in
  let all_automata = model.unexpanded_automata @ instantiated_automata in

  let synt_vars =
    List.concat_map
      (fun ((len, kind), names) -> List.map (fun name -> (name, kind, NumConst.to_bounded_int (eval_parsed_arithmetic_expr g_decls len))) names)
      model.synt_declarations
  in
  (* NOTE: at this point `parsed_action` calls must have an integer as argument, not an arbitrary expression *)
  let expanded_automata = expand_synt_arrays_automata g_decls synt_vars all_automata in
  let expanded_decls = expand_synt_decls synt_vars in
  let expanded_controllable_actions =
    match model.unexpanded_controllable_actions with
      | Unexpanded_parsed_controllable_actions actions ->
          Parsed_controllable_actions (expand_name_or_access_list g_decls actions)
      | Unexpanded_parsed_uncontrollable_actions actions ->
          Parsed_uncontrollable_actions (expand_name_or_access_list g_decls actions)
      | Unexpanded_parsed_no_controllable_actions -> Parsed_no_controllable_actions
  in

  let expanded_init_definition =
    expand_init_definition g_decls model.unexpanded_init_definition
  in

  { controllable_actions  = expanded_controllable_actions;
    variable_declarations = model.unexpanded_variable_declarations @ expanded_decls;
    fun_definitions       = model.unexpanded_fun_definitions;
    automata              = expanded_automata;
    init_definition       = expanded_init_definition;
  }

let instantiate_name_or_access (param_map: var_map) (aut: name_or_access): name_or_access =
  match aut with
    | Var_name _ -> aut
    | Var_array_access (arr, idx) ->
        let instantiated_idx = instantiate_discrete_arithmetic_expression param_map idx in
        Var_array_access (arr, instantiated_idx)

let instantiate_simple_predicate (param_map: var_map) (spred: unexpanded_parsed_simple_predicate): unexpanded_parsed_simple_predicate =
  match spred with
    | Unexpanded_Parsed_discrete_boolean_expression expr ->
        Unexpanded_Parsed_discrete_boolean_expression (instantiate_discrete_boolean_expression param_map expr)
    | Unexpanded_Parsed_loc_predicate (Unexpanded_Parsed_loc_predicate_EQ (aut, loc)) ->
        let instantiated_aut = instantiate_name_or_access param_map aut in
        Unexpanded_Parsed_loc_predicate (Unexpanded_Parsed_loc_predicate_EQ (instantiated_aut, loc))
    | Unexpanded_Parsed_loc_predicate (Unexpanded_Parsed_loc_predicate_NEQ (aut, loc)) ->
        let instantiated_aut = instantiate_name_or_access param_map aut in
        Unexpanded_Parsed_loc_predicate (Unexpanded_Parsed_loc_predicate_NEQ (instantiated_aut, loc))
    | _ -> spred

let rec instantiate_state_predicate (param_map: var_map): unexpanded_parsed_state_predicate -> unexpanded_parsed_state_predicate = function
  | Unexpanded_Parsed_state_predicate_OR (pred1, pred2) ->
      let instantiated_pred1 = instantiate_state_predicate param_map pred1 in
      let instantiated_pred2 = instantiate_state_predicate param_map pred2 in
      Unexpanded_Parsed_state_predicate_OR (instantiated_pred1, instantiated_pred2)
  | Unexpanded_Parsed_state_predicate_term t ->
      Unexpanded_Parsed_state_predicate_term (instantiate_state_term param_map t)

and instantiate_state_term (param_map: var_map): unexpanded_parsed_state_predicate_term -> unexpanded_parsed_state_predicate_term = function
  | Unexpanded_Parsed_state_predicate_term_AND (t1, t2) ->
      let instantiated_t1 = instantiate_state_term param_map t1 in
      let instantiated_t2 = instantiate_state_term param_map t2 in
      Unexpanded_Parsed_state_predicate_term_AND (instantiated_t1, instantiated_t2)
  | Unexpanded_Parsed_state_predicate_factor f ->
      Unexpanded_Parsed_state_predicate_factor (instantiate_state_factor param_map f)
  (* TODO: Refactor the next two branches *)
and instantiate_state_factor (param_map: var_map): unexpanded_parsed_state_predicate_factor -> unexpanded_parsed_state_predicate_factor = function
  | Unexpanded_Parsed_state_predicate_factor_NOT f -> Unexpanded_Parsed_state_predicate_factor_NOT (instantiate_state_factor param_map f)
  | Unexpanded_Parsed_simple_predicate spred -> Unexpanded_Parsed_simple_predicate (instantiate_simple_predicate param_map spred)
  | Unexpanded_Parsed_state_predicate pred -> Unexpanded_Parsed_state_predicate (instantiate_state_predicate param_map pred)
  (* TODO: merge next 4 branches *)
  | Unexpanded_Parsed_forall_state_predicate (index_data, pred) ->
      let binded_name = index_data.forall_index_name in
      let prev_binded_value_opt = Hashtbl.find_opt param_map binded_name in
      (* Here we remove the binde to allow the inner forall to rebind it. *)
      (* This means that if the user had something like `forall i in [0, 1]: forall i in [0, 1]: (p(i)) && q(i)`, the inner one will be
         substituted first, and the outer one will substitute only `q(i)` *)
      Hashtbl.remove param_map binded_name;
      let r = Unexpanded_Parsed_forall_state_predicate (index_data, instantiate_state_predicate param_map pred) in
      begin match prev_binded_value_opt with
        | Some v -> Hashtbl.add param_map binded_name v; r
        | None -> r
      end
  | Unexpanded_Parsed_forall_simple_predicate (index_data, spred) ->
      let binded_name = index_data.forall_index_name in
      let prev_binded_value_opt = Hashtbl.find_opt param_map binded_name in
      Hashtbl.remove param_map binded_name;
      let r = Unexpanded_Parsed_forall_simple_predicate (index_data, instantiate_simple_predicate param_map spred) in
      begin match prev_binded_value_opt with
        | Some v -> Hashtbl.add param_map binded_name v; r
        | None -> r
      end
  | Unexpanded_Parsed_exists_state_predicate (index_data, pred) ->
      let binded_name = index_data.forall_index_name in
      let prev_binded_value_opt = Hashtbl.find_opt param_map binded_name in
      Hashtbl.remove param_map binded_name;
      let r = Unexpanded_Parsed_exists_state_predicate (index_data, instantiate_state_predicate param_map pred) in
      begin match prev_binded_value_opt with
        | Some v -> Hashtbl.add param_map binded_name v; r
        | None -> r
      end
  | Unexpanded_Parsed_exists_simple_predicate (index_data, spred) ->
      let binded_name = index_data.forall_index_name in
      let prev_binded_value_opt = Hashtbl.find_opt param_map binded_name in
      Hashtbl.remove param_map binded_name;
      let r = Unexpanded_Parsed_exists_simple_predicate (index_data, instantiate_simple_predicate param_map spred) in
      begin match prev_binded_value_opt with
        | Some v -> Hashtbl.add param_map binded_name v; r
        | None -> r
      end

let expand_loc_pred (g_decls: variable_declarations): unexpanded_parsed_loc_predicate -> parsed_loc_predicate = function
  | Unexpanded_Parsed_loc_predicate_EQ (aut_name, loc) -> Parsed_loc_predicate_EQ (expand_name_or_access g_decls aut_name, loc)
  | Unexpanded_Parsed_loc_predicate_NEQ (aut_name, loc) -> Parsed_loc_predicate_NEQ (expand_name_or_access g_decls aut_name, loc)

let expand_simple_predicate (g_decls: variable_declarations): unexpanded_parsed_simple_predicate -> parsed_simple_predicate = function
  | Unexpanded_Parsed_discrete_boolean_expression expr -> Parsed_discrete_boolean_expression expr
  | Unexpanded_Parsed_loc_predicate loc_pred -> Parsed_loc_predicate (expand_loc_pred g_decls loc_pred)
  | Unexpanded_Parsed_state_predicate_true -> Parsed_state_predicate_true
  | Unexpanded_Parsed_state_predicate_false -> Parsed_state_predicate_false
  | Unexpanded_Parsed_state_predicate_accepting -> Parsed_state_predicate_accepting

let rec expand_state_predicate (g_decls: variable_declarations): unexpanded_parsed_state_predicate -> parsed_state_predicate = function
  | Unexpanded_Parsed_state_predicate_OR (pred1, pred2) ->
      Parsed_state_predicate_OR (expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_state_predicate_term t -> Parsed_state_predicate_term (expand_state_term g_decls t)

and expand_state_term (g_decls: variable_declarations): unexpanded_parsed_state_predicate_term -> parsed_state_predicate_term = function
  | Unexpanded_Parsed_state_predicate_term_AND (t1, t2) ->
      Parsed_state_predicate_term_AND (expand_state_term g_decls t1, expand_state_term g_decls t2)
  | Unexpanded_Parsed_state_predicate_factor f -> Parsed_state_predicate_factor (expand_state_factor g_decls f)

and expand_state_factor (g_decls: variable_declarations) (factor: unexpanded_parsed_state_predicate_factor): parsed_state_predicate_factor =
  let instantiate_spred_with index_data spred idx =
    let param_map_idx = gen_aux_var_tbl idx index_data in
    instantiate_simple_predicate param_map_idx spred |>
    expand_simple_predicate g_decls
  in
  let instantiate_pred_with index_data pred idx =
    let param_map_idx = gen_aux_var_tbl idx index_data in
    instantiate_state_predicate param_map_idx pred |>
    expand_state_predicate g_decls
  in
  match factor with
  (* TODO: merge next 4 branches *)
  | Unexpanded_Parsed_forall_simple_predicate (index_data, spred) -> begin
      let indices = indices_from_forall_index_data g_decls index_data in
      (* Here we reverse the list to preserve the order of the index after the fold *)
      match List.rev indices with
      | [] -> failwith "error or vacuity?"
      | i :: is ->
            (* Compute the conjunction of spred instantiated with each idx in (i::is) *)
            let term_of_simple_pred p = Parsed_state_predicate_factor (Parsed_simple_predicate p) in
            let spred_i = instantiate_spred_with index_data spred i in
            let fold_fun acc idx =
                let spred_idx = instantiate_spred_with index_data spred idx in
                let spred_idx_term = term_of_simple_pred spred_idx in
                match acc with
                  | Parsed_state_predicate (Parsed_state_predicate_term t) ->
                      Parsed_state_predicate (Parsed_state_predicate_term (Parsed_state_predicate_term_AND (spred_idx_term, t)))
                  | Parsed_simple_predicate p -> (* Only first iteration of fold, after that I know for sure is a term *)
                      let p_term = term_of_simple_pred p in
                      Parsed_state_predicate (Parsed_state_predicate_term (Parsed_state_predicate_term_AND (spred_idx_term, p_term)))
                  | _ -> failwith "[expand_state_factor]: unreachable"
                in
            List.fold_left fold_fun (Parsed_simple_predicate spred_i) is
  end
  | Unexpanded_Parsed_forall_state_predicate (index_data, pred) -> begin
      let indices = indices_from_forall_index_data g_decls index_data in
      match List.rev indices with
        | [] -> failwith "error or vacuity?"
        | i :: is ->
            let pred_i = instantiate_pred_with index_data pred i in
            let term_of_state_pred p = Parsed_state_predicate_factor (Parsed_state_predicate p) in
            let fold_fun acc idx =
              let pred_idx = instantiate_pred_with index_data pred idx in
              let pred_idx_term = term_of_state_pred pred_idx in
              match acc with
                | Parsed_state_predicate (Parsed_state_predicate_term t) ->
                    Parsed_state_predicate (Parsed_state_predicate_term (Parsed_state_predicate_term_AND (pred_idx_term, t)))
                | Parsed_state_predicate p -> (* Only first iteration of fold, after that I know for sure is an and *)
                    let p_term = term_of_state_pred p in
                    Parsed_state_predicate (Parsed_state_predicate_term (Parsed_state_predicate_term_AND (pred_idx_term, p_term)))
                | _ -> failwith "[expand_state_factor]: unreachable"
            in
            List.fold_left fold_fun (Parsed_state_predicate pred_i) is
  end
  | Unexpanded_Parsed_exists_simple_predicate (index_data, spred) -> begin
      let indices = indices_from_forall_index_data g_decls index_data in
      match List.rev indices with
      | [] -> failwith "error or vacuity?"
      | i :: is ->
            (* Compute the disjunction of spred instantiated with each idx in (i::is) *)
            let spred_i = instantiate_spred_with index_data spred i in
            let state_pred_of_simple_pred p =
              Parsed_state_predicate_term (Parsed_state_predicate_factor (Parsed_simple_predicate p))
            in
            let fold_fun acc idx =
                let spred_idx = instantiate_spred_with index_data spred idx in
                let spred_idx_state = state_pred_of_simple_pred spred_idx in
                match acc with
                  | Parsed_state_predicate p ->
                      Parsed_state_predicate (Parsed_state_predicate_OR (spred_idx_state, p))
                  | Parsed_simple_predicate p ->
                      let p_state= state_pred_of_simple_pred p in
                      Parsed_state_predicate (Parsed_state_predicate_OR (spred_idx_state, p_state))
                  | _ -> failwith "[expand_state_factor]: unreachable"
                in
            List.fold_left fold_fun (Parsed_simple_predicate spred_i) is
  end
  | Unexpanded_Parsed_exists_state_predicate (index_data, pred) -> begin
      let indices = indices_from_forall_index_data g_decls index_data in
      match List.rev indices with
        | [] -> failwith "error or vacuity?"
        | i :: is ->
            let pred_i = instantiate_pred_with index_data pred i in
            let fold_fun acc idx =
              let pred_idx = instantiate_pred_with index_data pred idx in
              match acc with
                | Parsed_state_predicate p -> Parsed_state_predicate (Parsed_state_predicate_OR (pred_idx, p))
                | _ -> failwith "[expand_state_factor]: unreachable"
            in
            List.fold_left fold_fun (Parsed_state_predicate pred_i) is
  end
  | Unexpanded_Parsed_state_predicate_factor_NOT f -> Parsed_state_predicate_factor_NOT (expand_state_factor g_decls f)
  | Unexpanded_Parsed_simple_predicate spred -> Parsed_simple_predicate (expand_simple_predicate g_decls spred)
  | Unexpanded_Parsed_state_predicate pred -> Parsed_state_predicate (expand_state_predicate g_decls pred)


let expand_property_type (g_decls: variable_declarations) : unexpanded_parsed_property_type -> parsed_property_type = function
  | Unexpanded_Parsed_Valid -> Parsed_Valid
  | Unexpanded_Parsed_EF pred1 -> Parsed_EF (expand_state_predicate g_decls pred1)
  | Unexpanded_Parsed_AGnot pred1 -> Parsed_AGnot (expand_state_predicate g_decls pred1)
  | Unexpanded_Parsed_AG pred1 -> Parsed_AG (expand_state_predicate g_decls pred1)
  | Unexpanded_Parsed_EG pred1 -> Parsed_EG (expand_state_predicate g_decls pred1)
  | Unexpanded_Parsed_ER (pred1, pred2) -> Parsed_ER (expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_EU (pred1, pred2) -> Parsed_EU (expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_EW (pred1, pred2) -> Parsed_EW (expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_AF pred1 -> Parsed_AF (expand_state_predicate g_decls pred1)
  | Unexpanded_Parsed_AR (pred1, pred2) -> Parsed_AR (expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_AU (pred1, pred2) -> Parsed_AU (expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_AW (pred1, pred2) -> Parsed_AW (expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_EF_timed (inter, pred1) -> Parsed_EF_timed (inter, expand_state_predicate g_decls pred1)
  | Unexpanded_Parsed_ER_timed (inter, pred1, pred2) -> Parsed_ER_timed (inter, expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_EU_timed (inter, pred1, pred2) -> Parsed_EU_timed (inter, expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_EW_timed (inter, pred1, pred2) -> Parsed_EW_timed (inter, expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_AF_timed (inter, pred1) -> Parsed_AF_timed (inter, expand_state_predicate g_decls pred1)
  | Unexpanded_Parsed_AR_timed (inter, pred1, pred2) -> Parsed_AR_timed (inter, expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_AU_timed (inter, pred1, pred2) -> Parsed_AU_timed (inter, expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_AW_timed (inter, pred1, pred2) -> Parsed_AW_timed (inter, expand_state_predicate g_decls pred1, expand_state_predicate g_decls pred2)
  | Unexpanded_Parsed_EFpmin (pred1, name) -> Parsed_EFpmin (expand_state_predicate g_decls pred1, name)
  | Unexpanded_Parsed_EFpmax (pred1, name) -> Parsed_EFpmax (expand_state_predicate g_decls pred1, name)
  | Unexpanded_Parsed_EFtmin pred1 -> Parsed_EFtmin (expand_state_predicate g_decls pred1)
  | Unexpanded_Parsed_Cycle_Through pred1 -> Parsed_Cycle_Through (expand_state_predicate g_decls pred1)
  | Unexpanded_Parsed_Cycle_Through_generalized pred_list -> Parsed_Cycle_Through_generalized (List.map (expand_state_predicate g_decls) pred_list)
  | Unexpanded_Parsed_NZ_Cycle -> Parsed_NZ_Cycle
  | Unexpanded_Parsed_Deadlock_Freeness -> Parsed_Deadlock_Freeness
  | Unexpanded_Parsed_IM pval -> Parsed_IM pval
  | Unexpanded_Parsed_ConvexIM pval -> Parsed_ConvexIM pval
  | Unexpanded_Parsed_PRP (pred1, pval) -> Parsed_PRP (expand_state_predicate g_decls pred1, pval)
  | Unexpanded_Parsed_IMK pval -> Parsed_IMK pval
  | Unexpanded_Parsed_IMunion pval -> Parsed_IMunion pval
  | Unexpanded_Parsed_Cover_cartography (pdomain, n) -> Parsed_Cover_cartography (pdomain, n)
  | Unexpanded_Parsed_Learning_cartography (pred1, pdomain, n) -> Parsed_Learning_cartography (expand_state_predicate g_decls pred1, pdomain, n)
  | Unexpanded_Parsed_Shuffle_cartography (pdomain, n) -> Parsed_Shuffle_cartography (pdomain, n)
  | Unexpanded_Parsed_Border_cartography (pdomain, n) -> Parsed_Border_cartography (pdomain, n)
  | Unexpanded_Parsed_Random_cartography (pdomain, i, n) -> Parsed_Random_cartography (pdomain, i, n)
  | Unexpanded_Parsed_RandomSeq_cartography (pdomain, i, n) -> Parsed_RandomSeq_cartography (pdomain, i, n)
  | Unexpanded_Parsed_PRPC (pred1, pdomain, n) -> Parsed_PRPC (expand_state_predicate g_decls pred1, pdomain, n)
  | Unexpanded_Parsed_pattern pattern -> Parsed_pattern pattern
  | Unexpanded_Parsed_Win pred1 -> Parsed_Win (expand_state_predicate g_decls pred1)

let expand_property (g_decls: variable_declarations) (property: unexpanded_parsed_property): parsed_property =
  { synthesis_type = property.unexpanded_synthesis_type
  ; property = expand_property_type g_decls property.unexpanded_property
  ; projection = property.unexpanded_projection
  }
