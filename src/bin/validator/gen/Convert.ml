open Lib
let automaton_name n = Printf.sprintf "automaton_%d" n
let location_name n = Printf.sprintf "loc_%d" n

let clock_name n = Printf.sprintf "clock_%d" n 

let parameter_name n = Printf.sprintf "p_%d" n


let parsed_automaton_of_matrix (a_id : int) (matrix : (SimpleModel.transition option) array array) (accepting : bool array) (invariants : SimpleModel.bool_expr list array): ParsingStructure.parsed_automaton =
  let open SimpleModel in 
  let open ParsingStructure in 

  let parsed_term_of_term term = 
    let factor =
    match term with 
    | SConstant i -> Parsed_constant (Int_value (Int32.of_int i))
    | SClock i -> Parsed_variable (clock_name i, 0)
    | SParam i -> Parsed_variable (parameter_name i, 0) in
    Parsed_term (Parsed_factor factor) in

  let parsed_op_of_op = function EQ -> PARSED_OP_EQ | L -> PARSED_OP_L | LEQ -> PARSED_OP_LEQ | G -> PARSED_OP_G | GEQ -> PARSED_OP_GEQ in 

  let parsed_dicrete_boolean_expression_of_bool_expr bool_expr = 
    match bool_expr with 
    | SComp (t1, op, t2) -> 
      let parsed_t1 = parsed_term_of_term t1 in 
      let parsed_t2 = parsed_term_of_term t2 in 
      let parsed_op = parsed_op_of_op op in
      Parsed_comparison ((Parsed_arithmetic_expr parsed_t1), parsed_op, (Parsed_arithmetic_expr parsed_t2)) in
  
  let parsed_instruction_of_reset reset = 
    let term_0 = Parsed_term (Parsed_factor (Parsed_constant (Rat_value NumConst.zero))) in
    let bool_expr_0 = Parsed_discrete_bool_expr (Parsed_arithmetic_expr term_0) in
    let variable = Parsed_scalar_update (clock_name reset, 0) in
    Parsed_assignment (variable, bool_expr_0) in

  let locations =
    Array.mapi (fun i row -> 
      let transitions = ref [] in 
      Array.iteri (fun j transition ->
        match transition with 
        | Some {controllable; guard; resets} ->
          let guard = List.map parsed_dicrete_boolean_expression_of_bool_expr guard in  
          let label = Printf.sprintf (if controllable then "c%d" else "u%d") a_id in 
          let parsed_seq_code_bloc = List.map parsed_instruction_of_reset resets in 
          transitions := (guard, parsed_seq_code_bloc, Sync label, location_name j) :: !transitions
        | None -> ()
      ) row;
      let invariant = List.map parsed_dicrete_boolean_expression_of_bool_expr invariants.(i) in 
      {
        name = location_name i;
        urgency = Parsed_location_nonurgent;
        acceptance = if accepting.(i) then Parsed_location_accepting else Parsed_location_nonaccepting;
        cost = None;
        invariant;
        stopped = [];
        flow = [];
        transitions = !transitions;
      }
    ) matrix
    |> Array.to_list
  in
  (automaton_name a_id, ["c" ^ string_of_int a_id; "u" ^ string_of_int a_id], locations)


let parsed_model_of_simple_model (sm : SimpleModel.t) : ParsingStructure.parsed_model = 
  let open SimpleModel in
  let nb_automata = List.length sm.transitions in 
  
  let init_loc_definition = List.init nb_automata (fun i -> ParsingStructure.Parsed_loc_assignment (automaton_name i, location_name 0)) in
  let init_clock_definition = List.init sm.nb_clocks (fun i -> 
    ParsingStructure.Parsed_linear_predicate (Parsed_linear_constraint 
    (Linear_term (Variable (NumConst.one, clock_name i)), PARSED_OP_EQ, Linear_term (Constant (NumConst.zero)))))
  in
  let init_parameter_definition = List.init sm.nb_parameters (fun i -> 
    ParsingStructure.Parsed_linear_predicate (Parsed_linear_constraint 
    (Linear_term (Variable (NumConst.one, parameter_name i)), PARSED_OP_GEQ, Linear_term (Constant (NumConst.zero)))))
  in
  let controllable_actions_list = 
    List.filteri (fun i _ -> Array.exists (fun automata_transitions -> 
      Array.exists (fun transition -> match transition with None -> false | Some t -> t.controllable) automata_transitions)
      (List.nth sm.transitions i)
    ) @@ 
    List.init nb_automata (fun i -> Printf.sprintf "c%d" i) in 
  let automata = List.mapi (fun i matrix -> 
    parsed_automaton_of_matrix i matrix sm.accepting.(i) sm.invariants.(i)
  ) sm.transitions in
  
  let clock_decls = List.init sm.nb_clocks (fun i -> (clock_name i, None)) in 
  let param_decls = List.init sm.nb_parameters (fun i -> (parameter_name i, None)) in
  let variable_declarations = [DiscreteType.Var_type_clock, clock_decls; DiscreteType.Var_type_parameter, param_decls] in

  {
      automata;
      controllable_actions = Parsed_controllable_actions controllable_actions_list;
	    variable_declarations;
	    fun_definitions = [];
	    init_definition = init_loc_definition @ init_clock_definition @ init_parameter_definition;
  }