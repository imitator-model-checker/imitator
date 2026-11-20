type transition = {controllable : bool; guard : PZone.t; resets: int list}

type t = { 
  automata : (transition list) array array list;
  accepting : bool array array;
  invariants : PZone.t array array;
  nb_clocks: int;
  nb_parameters : int;
}
