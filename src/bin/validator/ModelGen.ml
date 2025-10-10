open Crowbar
open Lib

(** Intermediate, AFL-friendly representation *)
module SimpleModel = struct
  type cop = 
  | EQ | L | LEQ | G | GEQ

  type term = 
  | SConstant of int 
  | SClock of int

  type bool_expr = 
  | SComp of term * cop * term

  

  type transition = {controllable : bool; guard : bool_expr list}
  type t = { 
    transitions : (transition option) array array list;
    accepting : bool array array;
    nb_clocks: int
  }

  type byte_reader = { s : string; mutable i : int }

  let next_byte br =
    let c = String.get br.s (br.i mod String.length br.s) in
    br.i <- br.i + 1;
    int_of_char c

  let next_bool br threshold =
    next_byte br < threshold

  let next_int br ?(min = 0) range =
    let max = min + range in 
    assert (max <= 255);
    (next_byte br mod max) + min

  let sample_uniform br xs =
    let idx = next_int br (List.length xs) in 
    List.nth xs idx
    

  let const_int_from_bytes br = 
    let sample = next_int br 10 in 
    SConstant sample

  let clock_from_bytes br nb_clocks = 
    let sample = next_int br nb_clocks in 
    SClock sample
    

  let comparison_from_bytes br nb_clocks = 
    let clock = clock_from_bytes br nb_clocks in 
    let constant = const_int_from_bytes br in 
    let oper = sample_uniform br [EQ; GEQ; G; L; LEQ] in 
    SComp (clock, oper, constant)

  let guard_from_bytes br nb_clocks = 
    let comparison = comparison_from_bytes br nb_clocks in
    [comparison]
    
  let transition_from_bytes br nb_clocks =
    let guard = guard_from_bytes br nb_clocks in 
    if next_bool br 126 then {controllable = true; guard} else {controllable = false; guard}

  (* 
    Probability to be accepting = (i/(n-1))^2, scaled down such that the
    expected number of accepting locations per automaton = 1  
  *)
  let prob_accepting n i = 
    if n <= 1 then 1.0 else 

    let num = float_of_int (6 * i * i) in 
    let denom = float_of_int ((n - 1) * n * (2 * n - 1)) in

    num /. denom  


  let accepting_locations_from_bytes ~nb_auto ~nb_loc_of_automaton ~byte_reader  =
    Array.init nb_auto (fun i ->
      Array.init nb_loc_of_automaton.(i) (fun j ->
        let prob = prob_accepting (nb_loc_of_automaton.(i)) j in
        let threshold = int_of_float (255. *. prob) in
        next_bool byte_reader threshold
      ))


  let adjacency_matrix_from_bytes ~rows ~columns ~byte_reader ~density ~nb_clocks  =
    let matrix : (transition option) array array = Array.make_matrix rows columns None in 

    (* Spanning tree *)
    let parents = ref [0] in
    for i = 1 to rows - 1 do 
      let parent = sample_uniform byte_reader !parents in
      let trans = transition_from_bytes byte_reader nb_clocks in 
      matrix.(parent).(i) <- Some trans;
      parents := i::!parents
    done;
    
    let threshold = int_of_float (255. *. density) in 
    let transform = function Some x -> Some x | None -> 
      if next_bool byte_reader threshold then Some (transition_from_bytes byte_reader nb_clocks) else None in  
    Array.map (Array.map transform) matrix

  let gen : t gen =                 
    map [range ~min:1 1; range ~min:1 2; bytes_fixed 128] 
      (fun nb_auto nb_clocks random_blob ->
        let byte_reader = {s = random_blob; i = 0} in 
        let nb_loc_of_automaton = Array.init nb_auto (fun _ -> next_int byte_reader ~min:10 5) in 
        let matrices =
        List.init nb_auto (fun a_id ->
          adjacency_matrix_from_bytes ~rows:(nb_loc_of_automaton.(a_id)) ~columns:(nb_loc_of_automaton.(a_id)) ~byte_reader ~density:0.05 ~nb_clocks)
        in
        let accepting = accepting_locations_from_bytes ~nb_auto ~nb_loc_of_automaton ~byte_reader in 
        { transitions = matrices; accepting; nb_clocks})

end

let automaton_name n = Printf.sprintf "automaton_%d" n
let location_name n = Printf.sprintf "loc_%d" n

let clock_name n = Printf.sprintf "clock_%d" n 

let parameter_name n = Printf.sprintf "p_%d" n


let parsed_automaton_of_matrix (a_id : int) (matrix : (SimpleModel.transition option) array array) (accepting : bool array) : ParsingStructure.parsed_automaton =
  let open SimpleModel in 
  let open ParsingStructure in 

  let parsed_term_of_term term = 
    let factor =
    match term with 
    | SConstant i -> Parsed_constant (Int_value (Int32.of_int i))
    | SClock i -> Parsed_variable (clock_name i, i) in
    Parsed_term (Parsed_factor factor) in

  let parsed_op_of_op = function EQ -> PARSED_OP_EQ | L -> PARSED_OP_LEQ | LEQ -> PARSED_OP_LEQ | G -> PARSED_OP_G | GEQ -> PARSED_OP_GEQ in 

  let parsed_dicrete_boolean_expression_of_bool_exprs bool_expr = 
    match bool_expr with 
    | SComp (t1, op, t2) -> 
      let parsed_t1 = parsed_term_of_term t1 in 
      let parsed_t2 = parsed_term_of_term t2 in 
      let parsed_op = parsed_op_of_op op in
      Parsed_comparison ((Parsed_arithmetic_expr parsed_t1), parsed_op, (Parsed_arithmetic_expr parsed_t2)) in

  let locations =
    Array.mapi (fun i row -> 
      let transitions = ref [] in 
      Array.iteri (fun j transition ->
        match transition with 
        | Some {controllable; guard} ->
          let guard = List.map parsed_dicrete_boolean_expression_of_bool_exprs guard in  
          let label = Printf.sprintf (if controllable then "c%d" else "u%d") a_id in 
          transitions := (guard, [], Sync label, location_name j) :: !transitions
        | None -> ()
      ) row;
      {
        name = location_name i;
        urgency = Parsed_location_nonurgent;
        acceptance = if accepting.(i) then Parsed_location_accepting else Parsed_location_nonaccepting;
        cost = None;
        invariant = [];
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
  let controllable_actions_list = 
    List.filteri (fun i _ -> Array.exists (fun automata_transitions -> 
      Array.exists (fun transition -> match transition with None -> false | Some t -> t.controllable) automata_transitions)
      (List.nth sm.transitions i)
    ) @@ 
    List.init nb_automata (fun i -> Printf.sprintf "c%d" i) in 
  let automata = List.mapi (fun i matrix -> 
    parsed_automaton_of_matrix i matrix sm.accepting.(i)
  ) sm.transitions in
  
  let clocks = List.init sm.nb_clocks (fun i -> (DiscreteType.Var_type_clock, [clock_name i, None])) in

  {
      automata;
      controllable_actions = Parsed_controllable_actions controllable_actions_list;
	    variable_declarations = clocks;
	    fun_definitions = [];
	    init_definition = init_loc_definition @ init_clock_definition;
  }

let parsed_model : ParsingStructure.parsed_model gen =
  map [SimpleModel.gen] parsed_model_of_simple_model