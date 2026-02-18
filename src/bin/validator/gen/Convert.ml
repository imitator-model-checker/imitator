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

module PZoneConv = struct 
  open PZone
  let param i = Parsed_term (Parsed_factor (Parsed_variable (Names.parameter i, 0)))
  let clock i = Parsed_term (Parsed_factor (Parsed_variable (Names.clock i, 0)))
  let constant i = Parsed_term (Parsed_factor (Parsed_constant (Int_value (Int32.of_int i))))

  let comp l op r = Parsed_comparison (Parsed_arithmetic_expr l, op, Parsed_arithmetic_expr r)

  let comps_of_interval v ({ub;lb} : Interval.t) = 
    match ub with 
    | Some (s,b) ->   
      let op = if s = Strict then PARSED_OP_L else PARSED_OP_LEQ in 
      [comp v op @@ constant b]
    | None -> []
    @
    match lb with 
    | Some (s,b) ->   
      let op = if s = Strict then PARSED_OP_G else PARSED_OP_GEQ in 
      [comp v op @@ constant b]
    | None -> []
    



  let comps_of_box var_conv (box : Box.t) = 
    box
    |> Array.to_list
    |> List.mapi (fun i x -> (i, x))
    |> List.concat_map (fun (i, interval) -> comps_of_interval (var_conv i) interval)
  
  let comps_of_coupling (coupling : Coupling.t) = 
    coupling 
    |> Array.to_list
    |> List.mapi (fun i  -> function 
        | ParamBound.Upper (s, p) -> 
          let op = if s = Strict then PARSED_OP_L else PARSED_OP_LEQ in 
          Some (comp (clock i) op (param p))
        | ParamBound.Lower (s, p) -> 
          let op = if s = Strict then PARSED_OP_G else PARSED_OP_GEQ in 
          Some (comp (clock i) op (param p))
        | ParamBound.NoBound -> None
        )
    |> List.filter_map Fun.id
     
      
  let convert pz = 
    let clock_comps = comps_of_box clock pz.clocks in 
    let param_comps = comps_of_box param pz.params in 
    let coupling_comps = comps_of_coupling pz.coupling in 
    clock_comps @ param_comps @ coupling_comps
end 

let reset i =
  let zero = Parsed_term (Parsed_factor (Parsed_constant (Rat_value NumConst.zero))) in
  let value = Parsed_discrete_bool_expr (Parsed_arithmetic_expr zero) in
  Parsed_assignment (Parsed_scalar_update (Names.clock i, 0), value)


let transition_to_parsed a_id destination_id = function {controllable;guard;resets} ->
  let guard = PZoneConv.convert guard in
  let parsed_seq_code_bloc = List.map reset resets in
  let label =
    if controllable then Names.controllable_action a_id
    else Names.uncontrollable_action a_id 
  in
  (guard, parsed_seq_code_bloc, Sync label, Names.location destination_id)

let location_to_parsed a_id location_id simple_transitions invariants accepting = 
  let transitions = ref [] in 
  Array.iteri (fun j transition_list ->
    List.iter (fun simple_transition -> 
      transitions := (transition_to_parsed a_id j simple_transition)::!transitions
    ) transition_list
  ) simple_transitions;
  let invariant = PZoneConv.convert invariants.(location_id) in 
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

let automaton_to_parsed (a_id : int) (matrix : (transition list) array array) (accepting : bool array) (invariants : PZone.t array): ParsingStructure.parsed_automaton =
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
  Names.controllable_action
  |> List.init (List.length sm.automata)
  |> List.filteri (fun i _ ->
      Array.exists (Array.exists (List.exists (fun t -> t.controllable)))
      (List.nth sm.automata i)
  ) 

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
  let controllable_actions = controllable_actions sm in
  {
    automata;
    controllable_actions = if controllable_actions = [] then Parsed_no_controllable_actions else Parsed_controllable_actions controllable_actions;
    variable_declarations = variable_declarations sm;
    fun_definitions = [];
    init_definition =
      init_locations nb_automata
      @ init_clocks sm.nb_clocks
      @ init_parameters sm.nb_parameters;
  }
