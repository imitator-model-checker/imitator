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

val gen : t Crowbar.gen