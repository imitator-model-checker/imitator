type dist =
  | Exact of int
  | Range of int * int (* Range (min, n): values are in [min, min + n) *)

type constraint_type = 
  | S_LE
  | S_LT
  | S_GE
  | S_GT
  | S_EQ


type t = {
  nb_automata : dist;
  nb_locations : dist;
  nb_clocks : dist;
  nb_parameters : dist;

  all_reachable : bool;
  transitions_per_location : dist;

  guard_types : constraint_type list;
  invariant_types : constraint_type list;

  guard_probability : float;
  invariant_probability : float;
  reset_probability : float;
}