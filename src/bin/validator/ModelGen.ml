open Crowbar
open Lib

(** Intermediate, AFL-friendly representation *)
module SimpleModel = struct
  type cop = 
  | EQ | L | LEQ | G | GEQ

  type term = 
  | SConstant of int 
  | SClock of int
  | SParam of int

  type bool_expr = 
  | SComp of term * cop * term

  

  type transition = {controllable : bool; guard : bool_expr list; resets: int list}
  type t = { 
    transitions : (transition option) array array list;
    accepting : bool array array;
    invariants : bool_expr list array array;
    nb_clocks: int;
    nb_parameters : int;
  }


  let const_int_from_sampler sampler = 
    let sample = Sampler.next_int sampler 10 in 
    SConstant sample

  let variable_from_sampler sampler ~nb_clocks ~nb_parameters = 
    let coinflip = Sampler.next_bool sampler ~prob:0.5 in
    if nb_parameters = 0 || coinflip then 
      let sample = Sampler.next_int sampler nb_clocks in 
      SClock sample
    else
      let sample = Sampler.next_int sampler nb_parameters in 
      SParam sample
    
    

  let comparison_from_sampler sampler ~nb_clocks ~nb_parameters ~opers = 
    let clock = variable_from_sampler sampler ~nb_clocks ~nb_parameters in 
    let constant = const_int_from_sampler sampler in 
    let oper = Sampler.sample_uniform sampler ~from:opers in 
    SComp (clock, oper, constant)

  let bool_expr_from_sampler sampler ~nb_clocks ~nb_parameters ~opers = 
    let comparison = comparison_from_sampler sampler ~nb_clocks ~nb_parameters ~opers in
    [comparison]
    
  let resets_from_sampler sampler nb_clocks =
    (* each clock has an independent chance to be added to the list of resets *)
    let should_add_reset () = Sampler.next_bool sampler ~prob:0.1 in
    let rec compute_resets acc = function
    | 0 -> acc
    | n -> if should_add_reset () then compute_resets ((n - 1)  :: acc) (n - 1) else acc in 
    compute_resets [] nb_clocks

  let transition_from_sampler sampler ~nb_clocks ~nb_parameters =
    let guard = bool_expr_from_sampler sampler ~nb_clocks ~nb_parameters ~opers:[EQ; GEQ; G; L; LEQ] in 
    let controllable = Sampler.next_bool sampler ~prob:0.5 in 
    let resets = resets_from_sampler sampler nb_clocks in
    {controllable; guard; resets}

  (* 
    Probability to be accepting = (i/(n-1))^2, scaled down such that the
    expected number of accepting locations per automaton = 1  
  *)
  let prob_accepting n i = 
    if n <= 1 then 1.0 else 

    let num = float_of_int (6 * i * i) in 
    let denom = float_of_int ((n - 1) * n * (2 * n - 1)) in

    num /. denom  


  let accepting_locations_from_sampler ~nb_auto ~nb_loc_of_automaton ~sampler ~guarantee_accepting  =
    Array.init nb_auto (fun i ->
      let accepting_loc_exists = ref false in 
      Array.init nb_loc_of_automaton.(i) (fun j ->
        let prob = prob_accepting (nb_loc_of_automaton.(i)) j in
        let accepting = Sampler.next_bool sampler ~prob in
        if accepting then accepting_loc_exists := true;
        let forced_accepting =  j = nb_loc_of_automaton.(i) - 1 && not !accepting_loc_exists && guarantee_accepting in
        accepting || forced_accepting 
      ))

  let invariants_from_sampler ~nb_auto ~nb_loc_of_automaton ~sampler ~nb_clocks ~nb_parameters = 
    Array.init nb_auto (fun i -> 
      Array.init nb_loc_of_automaton.(i) (fun _ ->
        bool_expr_from_sampler sampler ~nb_clocks ~nb_parameters ~opers:[L; LEQ]
      ))

  let adjacency_matrix_from_sampler ~rows ~columns ~sampler ~density ~nb_clocks ~nb_parameters  =
    let matrix : (transition option) array array = Array.make_matrix rows columns None in 

    (* Spanning tree *)
    let parents = ref [0] in
    for i = 1 to rows - 1 do 
      let parent = Sampler.sample_uniform sampler ~from:!parents in
      let trans = transition_from_sampler sampler ~nb_clocks ~nb_parameters in 
      matrix.(parent).(i) <- Some trans;
      parents := i::!parents
    done;
    
    let transform = function Some x -> Some x | None -> 
      if Sampler.next_bool sampler ~prob:density then Some (transition_from_sampler sampler ~nb_clocks ~nb_parameters) else None in  
    Array.map (Array.map transform) matrix

  let gen : t gen =                 
    map [range ~min:1 1; range ~min:1 2; range ~min:1 1; bytes_fixed 128] 
      (fun nb_auto nb_clocks nb_parameters random_blob ->
        let sampler = Sampler.create ~seed:random_blob in 
        let nb_loc_of_automaton = Array.init nb_auto (fun _ -> Sampler.next_int sampler ~min:10 5) in 
        let matrices =
        List.init nb_auto (fun a_id ->
          adjacency_matrix_from_sampler ~rows:(nb_loc_of_automaton.(a_id)) ~columns:(nb_loc_of_automaton.(a_id)) ~sampler ~density:0.1 ~nb_clocks ~nb_parameters)
        in
        let accepting = accepting_locations_from_sampler ~nb_auto ~nb_loc_of_automaton ~sampler ~guarantee_accepting:true in 
        let invariants = invariants_from_sampler ~nb_auto ~nb_loc_of_automaton ~nb_clocks ~nb_parameters ~sampler in
        { transitions = matrices; accepting; nb_clocks; nb_parameters; invariants})

end

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

let parsed_model : ParsingStructure.parsed_model gen =
  map [SimpleModel.gen] parsed_model_of_simple_model