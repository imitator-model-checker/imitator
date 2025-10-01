open Lib
open ParsingStructure
open Crowbar

(* Crowbar monadic binders *)
let ( let* ) = Crowbar.dynamic_bind
let ( let+ ) x f = Crowbar.map [x] f

(** [listni n f] returns a generator for a list of [n] elements of type ['a] where function f when given an index returns a generator ['a gen] *)
let listni (n : int) (f : int -> 'a gen) =
  let rec listni_inner i = 
    if i = n then const []
    else
      let* x = f i in 
      let+ xs = listni_inner (i + 1) in
      x :: xs
  in
  listni_inner 0

let small_positive_number : int gen = Crowbar.range ~min:1 9

let automaton_name n = Printf.sprintf "automaton_%d" n
let location_name n = Printf.sprintf "loc_%d" n

let transition nb_location : transition gen = 
  let+ target = range nb_location in 
  [], [], NoSync, location_name target


let parsed_location nb_location id : parsed_location gen = 
  let+ transitions = list @@ transition nb_location in 
  {
    name = location_name id;
    urgency = Parsed_location_urgent;
    acceptance = Parsed_location_nonaccepting;
    cost = None;
    invariant = [];
    stopped = [];
    flow = [];
    transitions;
  }

let parsed_automaton id : parsed_automaton gen = 
  let* nb_locations = small_positive_number in 
  let+ locations = listni nb_locations @@ parsed_location nb_locations in 
  automaton_name id, [], locations

let init_definition nb_automata = 
  List.init nb_automata (fun i -> Parsed_loc_assignment (automaton_name i, location_name 0))

let parsed_model : parsed_model gen = 
  let* nb_automata = small_positive_number in 
  let+ parsed_automata = listni nb_automata parsed_automaton in 
  {
      automata = parsed_automata;
      controllable_actions = Parsed_no_controllable_actions;
	    variable_declarations = [];
	    fun_definitions = [];
	    init_definition = init_definition nb_automata;
  }
  