open ImitatorUtilities;;
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

let instantiate_array_index (param_map : var_map) (arr : variable_name) (id : variable_name) : name_or_access =
  match Hashtbl.find_opt param_map id with
    | None | Some (Arg_name _) -> failwith "[instantiate_array_index]: Not allowed to access a syntatic array with a variable."
    | Some (Arg_int i)         -> Var_array_access (arr, Literal i)
    | Some (Arg_float _)       -> failwith "[instantiate_array_index]: Not allowed to access a syntatic array with a float"
    | Some (Arg_bool _)        -> failwith "[instantiate_array_index]: Not allowed to access a syntatic array with a boolean."

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
    | Var_array_access (arr_name, Literal id) -> Var_array_access (arr_name, Literal id)
    | Var_array_access (arr_name, Const_var id) -> instantiate_array_index param_map arr_name id
  ) clocks


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
        | Var_array_access (arr_name, Const_var id) -> instantiate_array_index param_map arr_name id
        | Var_array_access (_, Literal _) -> clock
    in
    let rate' =
      match rate with
        | Const_var name -> begin
            match Hashtbl.find_opt param_map name with
              | None -> rate
              | Some (Arg_int i)   -> Literal i
              | Some (Arg_float f) -> Literal f
              | Some _ -> failwith "[instantiate_flows]: unexpected argument for template (expecting name)"
            end
        | Literal _ -> rate
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
        let rec instantiate_indexed_update =
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
                let arr' = instantiate_indexed_update arr in
                Parsed_indexed_update (arr', index')
        in
        match inst with
          | Parsed_assignment (indexed_update, rhs) ->
              let indexed_update' = instantiate_indexed_update indexed_update in
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
    | Var_array_access (arr_name, Const_var i_name) -> instantiate_array_index param_map arr_name i_name
    | Var_array_access (_, Literal _) -> action

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
             unexpanded_stopped     = instantiate_stopped param_map loc.unexpanded_stopped;
             unexpanded_flow        = instantiate_flows param_map loc.unexpanded_flow;
             unexpanded_transitions = instantiate_transitions param_map loc.unexpanded_transitions
  }

let var_index_err_msg arr_name var_name =
  "Invalid usage of variable " ^ var_name ^ " to access array of actions " ^ arr_name ^ "."

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

(* The names and the corresponding array lengths of all syntatic variables in the problem *)
type synt_vars_data = (variable_name * synt_var_kind * int) list

let expand_synt_decls (synt_decls : synt_vars_data) : variable_declarations =
  let aux (name, kind, len) =
    match kind with
      | Clock_synt_array ->
        let ids = List.init len Fun.id in
        Some (List.map (fun i -> (gen_access_id name i, None)) ids)
      | Action_synt_array -> None
  in
  let packed_decls = List.filter_map aux synt_decls in
  List.map (fun packed_decl -> (DiscreteType.Var_type_clock, packed_decl)) packed_decls
    (* let expand_name name = *)
    (*   List.fold_left (fun acc i -> gen_access_id name i :: acc) [] ids in *)
    (* let expanded_names = List.concat_map expand_name names in *)
    (* let packed_expanded_names = List.map (fun name -> (name, None)) expanded_names in *)
    (* match kind with *)
    (*   | Clock_synt_array -> *)
    (*       Some (DiscreteType.Var_type_clock, packed_expanded_names) *)
    (*   | Action_synt_array -> None *)
  (* in *)
  (* List.filter_map aux synt_decls *)

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
        match List.find_map (fun (name, _, len) -> if arr_name = name then Some len else None) synt_arrays with
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
  | Var_array_access (arr_name, Literal i) -> gen_access_id arr_name (NumConst.to_bounded_int i)
  | Var_array_access (arr_name, Const_var var_name) ->
      failwith "[expand_synt_arrays_automaton]: " ^ (var_index_err_msg arr_name var_name)

let expand_name_or_access_list = List.map expand_name_or_access

let expand_sync : unexpanded_sync -> sync = function
  | UnexpandedSync (Var_name act_name) -> Sync act_name
  | UnexpandedSync (Var_array_access (arr_name, Literal id)) ->
      Sync (gen_access_id arr_name (NumConst.to_bounded_int id))
  | UnexpandedSync (Var_array_access (arr_name, Const_var var_name)) ->
      failwith ("[expand_sync]: " ^ var_index_err_msg arr_name var_name)
  | UnexpandedNoSync -> NoSync

let expand_indexed_update (synt_vars : synt_vars_data) : parsed_scalar_or_index_update_type -> parsed_scalar_or_index_update_type =
  fun e ->
    match e with
      | Parsed_indexed_update (Parsed_scalar_update (name, _), index) -> begin
          match List.find_map (fun (name', _, len) -> if name = name' then Some len else None) synt_vars with
            | None -> e
            | Some len ->
                let index_c = NumConst.to_bounded_int (get_const_disc_arith_expr index) in
                if index_c < len then
                  let new_name = gen_access_id name index_c in
                  Parsed_scalar_update (new_name, 0)
                else
                  failwith "[expand_indexed_update]: Index is greater or equal to length of syntatic array"
      end
      | _ -> e

let rec expand_instruction (synt_vars : synt_vars_data) : parsed_instruction -> parsed_instruction = function
  | Parsed_local_decl (name, tp, init) ->
      let init' = expand_parsed_boolean_expression synt_vars init in
      Parsed_local_decl (name, tp, init')
  | Parsed_assignment (upd, rhs) ->
      let rhs' = expand_parsed_boolean_expression synt_vars rhs in
      let upd' = expand_indexed_update synt_vars upd in
      Parsed_assignment (upd', rhs')
  | Parsed_instruction expr -> Parsed_instruction (expand_parsed_boolean_expression synt_vars expr)
  | Parsed_for_loop (var_ref, left_expr, right_expr, dir, bloc) ->
      let left_expr' = expand_parsed_discrete_arithmetic_expression synt_vars left_expr in
      let right_expr' = expand_parsed_discrete_arithmetic_expression synt_vars right_expr in
      let bloc' = expand_code_bloc synt_vars bloc in
      Parsed_for_loop (var_ref, left_expr', right_expr', dir, bloc')
  | Parsed_while_loop (cond, bloc) ->
      let cond' = expand_parsed_boolean_expression synt_vars cond in
      let bloc' = expand_code_bloc synt_vars bloc in
      Parsed_while_loop (cond', bloc')
  | Parsed_if (cond, then_branch, else_branch_opt) ->
      let cond' = expand_parsed_boolean_expression synt_vars cond in
      let then_branch' = expand_code_bloc synt_vars then_branch in
      let else_branch_opt' = Option.map (fun expr -> expand_code_bloc synt_vars expr) else_branch_opt in
      Parsed_if (cond', then_branch', else_branch_opt')

and expand_code_bloc (synt_vars : synt_vars_data) : parsed_seq_code_bloc -> parsed_seq_code_bloc =
  List.map (expand_instruction synt_vars)

let expand_loc (synt_vars : synt_vars_data) (loc : unexpanded_parsed_location) : parsed_location =
  let expand_transition (guard, bloc, sync, loc_name) =
    let sync' = expand_sync sync in
    let guard' = List.map (expand_parsed_discrete_boolean_expression synt_vars) guard in
    let bloc' = expand_code_bloc synt_vars bloc in
    (guard', bloc', sync', loc_name)
  in
  let expanded_transitions = List.map expand_transition loc.unexpanded_transitions in
  let expanded_invariant = List.map (expand_parsed_discrete_boolean_expression synt_vars) loc.unexpanded_invariant in
  let expanded_stopped = expand_name_or_access_list loc.unexpanded_stopped in
  let expand_flow (clock, rate) =
    let real_rate =
      match rate with
        | Literal r -> r
        | Const_var _ -> failwith "[expand_flow]: Flows must be either literals or template variables."
    in
    match clock with
      | Var_name name -> (name, real_rate)
      | Var_array_access (arr_name, Literal id) ->
          let new_name = gen_access_id arr_name (NumConst.to_bounded_int id) in
          (new_name, real_rate)
      | Var_array_access (_, Const_var _) -> failwith "[expand_flow]: Index of clock array must be a literal or a template variable."
  in
  let expanded_flow = List.map expand_flow loc.unexpanded_flow in
  {
    name        = loc.unexpanded_name;
    urgency     = loc.unexpanded_urgency;
    cost        = loc.unexpanded_cost;
    acceptance  = loc.unexpanded_acceptance;
    invariant   = expanded_invariant;
    stopped     = expanded_stopped;
    flow        = expanded_flow;
    transitions = expanded_transitions;
  }

let expand_synt_arrays_automaton (synt_vars : synt_vars_data) (automaton : unexpanded_parsed_automaton) : parsed_automaton =
  let name, unexpanded_actions, unexpanded_locs = automaton in
  let expanded_actions = expand_name_or_access_list unexpanded_actions in
  let expanded_locs = List.map (expand_loc synt_vars) unexpanded_locs in
  name, expanded_actions, expanded_locs

let expand_synt_arrays_automata (synt_vars : synt_vars_data) : unexpanded_parsed_automaton list -> parsed_automaton list =
  List.map (expand_synt_arrays_automaton synt_vars)

let eval_expr_err_msg = "[eval_boolean_expression]: Trying to evaluate an expression whose value is not known at compile time."

let rec eval_parsed_boolean_expression =
  function
    | Parsed_discrete_bool_expr e -> eval_parsed_discrete_bool_expr e
    | Parsed_conj_dis _ -> failwith eval_expr_err_msg

and eval_parsed_discrete_bool_expr =
  function
    | Parsed_arithmetic_expr e -> eval_parsed_arithmetic_expr e
    | _ -> failwith eval_expr_err_msg

and eval_parsed_arithmetic_expr =
  function
    | Parsed_sum_diff (arith_expr, term, Parsed_plus) -> (eval_parsed_arithmetic_expr arith_expr) + (eval_parsed_term term)
    | Parsed_sum_diff (arith_expr, term, Parsed_minus) -> (eval_parsed_arithmetic_expr arith_expr) - (eval_parsed_term term)
    | Parsed_term t -> eval_parsed_term t

and eval_parsed_term =
  function
    | Parsed_product_quotient (term, factor, Parsed_mul) -> eval_parsed_term term * eval_parsed_factor factor
    | Parsed_product_quotient (term, factor, Parsed_div) -> eval_parsed_term term / eval_parsed_factor factor
    | Parsed_factor factor -> eval_parsed_factor factor

and eval_parsed_factor =
  function
    | Parsed_constant v -> NumConst.to_bounded_int (ParsedValue.to_numconst_value v)
    | _ -> failwith eval_expr_err_msg

let expand_model (unexpanded_parsed_model : unexpanded_parsed_model) : parsed_model =
  let instantiated_automata = instantiate_automata unexpanded_parsed_model.template_definitions unexpanded_parsed_model.template_calls in
  let all_automata = unexpanded_parsed_model.unexpanded_automata @ instantiated_automata in
  let expand_literal_or_const_var = function
    | Literal l -> NumConst.to_bounded_int l
    | Const_var name ->
        let decls = unexpanded_parsed_model.unexpanded_variable_declarations in
        let inspect_decl (name', expr_opt) = if name = name' then expr_opt else None in
        let inspect_decls_of_type (_, decls) =
          List.find_map (fun decl -> (inspect_decl decl)) decls
        in
        let inspect_all_decls decls =
          List.find_map Fun.id (List.map inspect_decls_of_type decls)
        in
        match inspect_all_decls decls with
          | None -> failwith "[expand_model]: Size of syntatic array is a non-constant variable."
          | Some expr -> eval_parsed_boolean_expression expr
  in
  let synt_vars =
    List.concat_map
      (fun ((len, kind), names) -> List.map (fun name -> (name, kind, expand_literal_or_const_var len)) names)
      unexpanded_parsed_model.synt_declarations
  in
  (* NOTE: at this point `parsed_action` calls must have an integer as argument, not a name *)
  let expanded_automata = expand_synt_arrays_automata synt_vars all_automata in
  let expanded_decls = expand_synt_decls synt_vars in
  { controllable_actions = unexpanded_parsed_model.unexpanded_controllable_actions;
    variable_declarations = (unexpanded_parsed_model.unexpanded_variable_declarations @ expanded_decls);
    fun_definitions = unexpanded_parsed_model.unexpanded_fun_definitions;
    automata = expanded_automata;
    init_definition = unexpanded_parsed_model.unexpanded_init_definition;
  }
