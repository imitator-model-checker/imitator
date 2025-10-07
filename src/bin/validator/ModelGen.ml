open Crowbar
open Lib

(** Intermediate, AFL-friendly representation *)
module SimpleModel = struct
  type t = { transitions : bool array array list }

  let bool_matrix_from_bytes_offset n (s : string) (offset : int) =
    let bits = String.length s * 8 in
    Array.init n (fun i ->
      Array.init n (fun j ->
        let idx = (offset + i * n + j) mod bits in
        let byte = int_of_char (String.get s (idx / 8)) in
        (byte lsr (idx mod 8)) land 1 = 1))

  let gen : t gen =                    (* 1024-bit random blob *)
    map [range ~min:1 4; range ~min:1 5; bytes_fixed 128] (fun nb_auto n s ->
      let matrices =
        List.init nb_auto (fun a ->
          let offset = (a * n * n) / 8 in 
          bool_matrix_from_bytes_offset n s offset)
      in
      { transitions = matrices })

end

let automaton_name n = Printf.sprintf "automaton_%d" n
let location_name n = Printf.sprintf "loc_%d" n

let parsed_automaton_of_matrix (a_id : int) (matrix : bool array array) : ParsingStructure.parsed_automaton =
  let open ParsingStructure in 
  let locations =
    Array.mapi (fun i row -> 
      let transitions = ref [] in 
      Array.iteri (fun j has_edge ->
        if has_edge then transitions := ([], [], NoSync, location_name j) :: !transitions
      ) row;
      {
        name = location_name i;
        urgency = Parsed_location_nonurgent;
        acceptance = Parsed_location_nonaccepting;
        cost = None;
        invariant = [];
        stopped = [];
        flow = [];
        transitions = !transitions;
      }
    ) matrix
    |> Array.to_list
  in
  (automaton_name a_id, [], locations)


let to_parsed_model (sm : SimpleModel.t) : ParsingStructure.parsed_model = 
  let nb_automata = List.length sm.transitions in 
  let init_definition = List.init nb_automata (fun i -> ParsingStructure.Parsed_loc_assignment (automaton_name i, location_name 0)) in
  let automata = List.mapi parsed_automaton_of_matrix sm.transitions in
  {
      automata;
      controllable_actions = Parsed_no_controllable_actions;
	    variable_declarations = [];
	    fun_definitions = [];
	    init_definition = init_definition;
  }

(** Final generator for Crowbar *)
let parsed_model : ParsingStructure.parsed_model gen =
  map [SimpleModel.gen] to_parsed_model