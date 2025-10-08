open Crowbar
open Lib

(** Intermediate, AFL-friendly representation *)
module SimpleModel = struct

  type transition = {controllable : bool}
  type t = { 
    transitions : (transition option) array array list;
    accepting : bool array array;
  }

  type byte_reader = { s : string; mutable i : int }

  let next_byte br =
    let c = String.get br.s (br.i mod String.length br.s) in
    br.i <- br.i + 1;
    int_of_char c

  let next_bool br threshold =
    next_byte br < threshold

  let next_int br ~max =
    assert (max < 255);
    next_byte br mod (max + 1)
    
  let next_transition br =
    if next_bool br 126 then {controllable = true} else {controllable = false}

  (* 
    Probability to be accepting = (i/(n-1))^2, scaled down such that the
    expected number of accepting locations per automaton = 1  
  *)
  let prob_accepting n i = 
    if n <= 1 then 1.0 else 

    let num = float_of_int (6 * i * i) in 
    let denom = float_of_int ((n - 1) * n * (2 * n - 1)) in

    num /. denom  


  let accepting_locations_from_bytes ~nb_auto ~nb_loc_of_automaton ~byte_reader ~density  =
    Array.init nb_auto (fun i ->
      Array.init nb_loc_of_automaton.(i) (fun j ->
        let prob = prob_accepting (nb_loc_of_automaton.(i)) j in
        let threshold = int_of_float (255. *. prob) in
        next_bool byte_reader threshold
      ))


  let adjacency_matrix_from_bytes ~rows ~columns ~byte_reader ~density  =
    let matrix : (transition option) array array = Array.make_matrix rows columns None in 

    (* Spanning tree *)
    let parents = ref [0] in
    for i = 1 to rows - 1 do 
      let parent_pos = next_int byte_reader ~max:(List.length !parents - 1) in 
      let parent = List.nth !parents parent_pos in 
      let trans = next_transition byte_reader in 
      matrix.(parent).(i) <- Some trans;
      parents := i::!parents
    done;
    
    let threshold = int_of_float (255. *. density) in 
    let transform = function Some x -> Some x | None -> 
      if next_bool byte_reader threshold then Some (next_transition byte_reader) else None in  
    Array.map (Array.map transform) matrix

  let gen : t gen =                 
    map [range ~min:1 3; bytes_fixed 128] 
      (fun nb_auto random_blob ->
        let byte_reader = {s = random_blob; i = 0} in 
        let nb_loc_of_automaton = Array.init nb_auto (fun _ -> next_int byte_reader ~max:8 + 2) in 
        let matrices =
        List.init nb_auto (fun a_id ->
          adjacency_matrix_from_bytes ~rows:(nb_loc_of_automaton.(a_id)) ~columns:(nb_loc_of_automaton.(a_id)) ~byte_reader ~density:0.1)
        in
        let accepting = accepting_locations_from_bytes ~nb_auto ~nb_loc_of_automaton ~byte_reader ~density:0.1 in 
        { transitions = matrices; accepting})

end

let automaton_name n = Printf.sprintf "automaton_%d" n
let location_name n = Printf.sprintf "loc_%d" n

let parsed_automaton_of_matrix (a_id : int) (matrix : (SimpleModel.transition option) array array) (accepting : bool array) : ParsingStructure.parsed_automaton =
  let open ParsingStructure in 
  let open SimpleModel in 
  let locations =
    Array.mapi (fun i row -> 
      let transitions = ref [] in 
      Array.iteri (fun j transition ->
        match transition with 
        | Some {controllable} -> 
          let label = Printf.sprintf (if controllable then "c%d" else "u%d") a_id in 
          transitions := ([], [], Sync label, location_name j) :: !transitions
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
  let init_definition = List.init nb_automata (fun i -> ParsingStructure.Parsed_loc_assignment (automaton_name i, location_name 0)) in
  let controllable_actions_list = 
    List.filteri (fun i _ -> Array.exists (fun automata_transitions -> 
      Array.exists (fun transition -> match transition with None -> false | Some t -> t.controllable) automata_transitions)
      (List.nth sm.transitions i)
    ) @@ 
    List.init nb_automata (fun i -> Printf.sprintf "c%d" i) in 
  let automata = List.mapi (fun i matrix -> 
    parsed_automaton_of_matrix i matrix sm.accepting.(i)
  ) sm.transitions in
  {
      automata;
      controllable_actions = Parsed_controllable_actions controllable_actions_list;
	    variable_declarations = [];
	    fun_definitions = [];
	    init_definition = init_definition;
  }

let parsed_model : ParsingStructure.parsed_model gen =
  map [SimpleModel.gen] parsed_model_of_simple_model