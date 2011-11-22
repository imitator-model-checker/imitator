(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2010/03/10
 * Last modified: 2011/11/20
 *
 ****************************************************************)
 

(**************************************************)
(** {2 Types} *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Variables} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

type variable_index = int
type clock_index = variable_index
type parameter_index = variable_index
type discrete_index = variable_index
type variable_name = string
type value = NumConst.t

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Locations} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
type location_index = int
type location_name = string

(* Global location: location for each automaton + value of the discrete *)
type global_location

val location_equal: global_location -> global_location -> bool

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Automata} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

type automaton_index = int
type automaton_name = string






(**************************************************)
(** {2 Locations} *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Initialization} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** 'initialize nb_automata min_discrete_index max_discrete_index' initializes the min and max discrete indexes and the number of automata. *)
val initialize : int -> int -> int -> unit


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** 'make_location locations discrete_values' creates a new location. All automata should be given a location. Discrete variables may not be given a value (in which case they will be initialized to 0). *)
val make_location : (automaton_index * location_index) list -> (discrete_index * value) list -> global_location

(** 'update_location locations discrete_values location' creates a new location from the original location, and update the given automata and discrete variables. *)
val update_location : (automaton_index * location_index) list -> (discrete_index * value) list -> global_location -> global_location

(** Side-effet version of update_location. *)
val update_location_with : (automaton_index * location_index) list -> (discrete_index * value) list -> global_location -> unit

(** 'copy_location location' creates a fresh location identical to location. *)
val copy_location : global_location -> global_location


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Get array of locations *)
val get_locations : global_location -> location_index array

(** Get the location associated to some automaton *)
val get_location : global_location -> automaton_index -> location_index

(** Get the value associated to some discrete variable *)
val get_discrete_value : global_location -> discrete_index -> value

(** Get a hash value for a location *)
val location_hash_code : global_location -> int

(** Get a hash value for a location, including discrete values *)
val hash_code : global_location -> int


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** 'string_of_location automata_names location_names discrete_names location' converts a location to a string. *)
val string_of_location : (automaton_index -> automaton_name) -> (automaton_index -> location_index -> location_name) -> (discrete_index -> variable_name) -> global_location -> string
