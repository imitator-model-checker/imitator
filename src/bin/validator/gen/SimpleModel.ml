
type cop = 
| EQ | L | LEQ | G | GEQ

type term = 
| SConstant of int 
| SClock of int
| SParam of int

type bool_expr = 
| SComp of term * cop * term

type formula = bool_expr list 

type transition = {controllable : bool; guard : formula; resets: int list}

type t = { 
  automata : (transition option) array array list;
  accepting : bool array array;
  invariants : formula array array;
  nb_clocks: int;
  nb_parameters : int;
}
