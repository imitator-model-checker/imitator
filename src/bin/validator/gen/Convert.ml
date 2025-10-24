open Lib
open ParsingStructure
open SimpleModel

module Names = struct 
  let automaton n = Printf.sprintf "automaton_%d" n
  let location n = Printf.sprintf "l%d" n
  let clock = function 0 -> "x" | 1 -> "y" | 2 -> "z" | n -> Printf.sprintf "clk_%d" n 
  let parameter = function 0 -> "p" | 1 -> "q" | 2 -> "r" | n -> Printf.sprintf "p_%d" n 
  let controllable_action a_id = Printf.sprintf "c%d" a_id
  let uncontrollable_action a_id = Printf.sprintf "u%d" a_id
  let actions a_id = [controllable_action a_id; uncontrollable_action a_id]
end

module Expr = struct 
  let term = function
    | SConstant i -> Parsed_term (Parsed_factor (Parsed_constant (Int_value (Int32.of_int i))))
    | SClock i    -> Parsed_term (Parsed_factor (Parsed_variable (Names.clock i, 0)))
    | SParam i    -> Parsed_term (Parsed_factor (Parsed_variable (Names.parameter i, 0)))

  let op = function
  | EQ -> PARSED_OP_EQ
  | L  -> PARSED_OP_L
  | LEQ -> PARSED_OP_LEQ
  | G  -> PARSED_OP_G
  | GEQ -> PARSED_OP_GEQ

  let bool_expr = function
    | SComp (t1, op_, t2) ->
        Parsed_comparison
          (Parsed_arithmetic_expr (term t1), op op_, Parsed_arithmetic_expr (term t2))

  let reset i =
    let zero = Parsed_term (Parsed_factor (Parsed_constant (Rat_value NumConst.zero))) in
    let value = Parsed_discrete_bool_expr (Parsed_arithmetic_expr zero) in
    Parsed_assignment (Parsed_scalar_update (Names.clock i, 0), value)
end 

let transition_to_parsed a_id destination_id = function {controllable;guard;resets} ->
  let guard = List.map Expr.bool_expr guard in
  let parsed_seq_code_bloc = List.map Expr.reset resets in
  let label =
    if controllable then Names.controllable_action a_id
    else Names.uncontrollable_action a_id 
  in
  (guard, parsed_seq_code_bloc, Sync label, Names.location destination_id)

let location_to_parsed a_id location_id simple_transitions invariants accepting = 
  let transitions = ref [] in 
  Array.iteri (fun j transition_option ->
    Option.may (fun simple_transition -> 
      transitions := (transition_to_parsed a_id j simple_transition)::!transitions
    ) transition_option
  ) simple_transitions;
  let invariant = List.map Expr.bool_expr invariants.(location_id) in 
  {
    name = Names.location location_id;
    urgency = Parsed_location_nonurgent;
    acceptance = if accepting.(location_id) then Parsed_location_accepting else Parsed_location_nonaccepting;
    cost = None;
    invariant;
    stopped = [];
    flow = [];
    transitions = !transitions;
  }

let automaton_to_parsed (a_id : int) (matrix : (transition option) array array) (accepting : bool array) (invariants : bool_expr list array): ParsingStructure.parsed_automaton =
  let locations =
    Array.mapi (fun i row -> 
      location_to_parsed a_id i row invariants accepting
    ) matrix
    |> Array.to_list
  in
  (Names.automaton a_id, Names.actions a_id, locations)


let init_locations nb_automata =
  List.init nb_automata (fun i ->
    Parsed_loc_assignment (Names.automaton i, Names.location 0))

let init_clocks nb_clocks =
  List.init nb_clocks (fun i ->
    Parsed_linear_predicate (Parsed_linear_constraint
      (Linear_term (Variable (NumConst.one, Names.clock i)),
       PARSED_OP_EQ,
       Linear_term (Constant (NumConst.zero)))))

let init_parameters nb_params =
  List.init nb_params (fun i ->
    Parsed_linear_predicate (Parsed_linear_constraint
      (Linear_term (Variable (NumConst.one, Names.parameter i)),
       PARSED_OP_GEQ,
       Linear_term (Constant (NumConst.zero)))))

let controllable_actions sm =
  List.filteri (fun i _ ->
    Array.exists (Array.exists (function Some t -> t.controllable | None -> false))
      (List.nth sm.automata i)
  ) (List.init (List.length sm.automata) Names.controllable_action)

let variable_declarations sm =
  let clocks = List.init sm.nb_clocks (fun i -> (Names.clock i, None)) in
  let params = List.init sm.nb_parameters (fun i -> (Names.parameter i, None)) in
  [DiscreteType.Var_type_clock, clocks;
   DiscreteType.Var_type_parameter, params]

let parsed_model_of_simple_model sm =
  let nb_automata = List.length sm.automata in
  let automata =
    List.mapi (fun i matrix ->
      automaton_to_parsed i matrix sm.accepting.(i) sm.invariants.(i))
    sm.automata
  in
  {
    automata;
    controllable_actions =
      Parsed_controllable_actions (controllable_actions sm);
    variable_declarations = variable_declarations sm;
    fun_definitions = [];
    init_definition =
      init_locations nb_automata
      @ init_clocks sm.nb_clocks
      @ init_parameters sm.nb_parameters;
  }
