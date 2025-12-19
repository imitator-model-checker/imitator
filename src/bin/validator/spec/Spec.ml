type dist =
  | Exact of int
  | Range of int * int


type t = {
  nb_automata : dist;
  nb_locations : dist;
  nb_clocks : dist;
  nb_parameters : dist;
}