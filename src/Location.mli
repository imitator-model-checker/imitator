(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: define global locations
 * 
 * File contributors        : Étienne André
 * Created                  : 2010/03/10
 * Renamed from Automaton.ml: 2015/10/22
 * Last modified            : 2019/10/16
 *
 ************************************************************)



(************************************************************)
(* Modules *)
(************************************************************)
open Automaton


(************************************************************)
(** {2 Types} *)
(************************************************************)

type discrete_value = NumConst.t

(** Unique identifier for each different global location *)
type global_location_index = int

(** Global location: location for each automaton + value of the discrete *)
type global_location





(************************************************************)
(** {2 Locations} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Initialization} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** 'initialize nb_automata min_discrete_index max_discrete_index' initializes the min and max discrete indexes and the number of automata. *)
val initialize : int -> int -> int -> unit


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** 'make_location locations discrete_values' creates a new location. All automata should be given a location. Discrete variables may not be given a value (in which case they will be initialized to 0). *)
val make_location : (automaton_index * location_index) list -> (discrete_index * discrete_value) list -> global_location

(** 'update_location locations discrete_values location' creates a new location from the original location, and update the given automata and discrete variables. *)
val update_location : (automaton_index * location_index) list -> (discrete_index * discrete_value) list -> global_location -> global_location

(** Side-effet version of update_location. *)
val update_location_with : (automaton_index * location_index) list -> (discrete_index * discrete_value) list -> global_location -> unit

(** 'copy_location location' creates a fresh location identical to location. *)
val copy_location : global_location -> global_location


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Equality between locations *)
val location_equal: global_location -> global_location -> bool

(** Get array of locations *)
val get_locations : global_location -> location_index array

(** Get the location associated to some automaton *)
val get_location : global_location -> automaton_index -> location_index

(** Get the value associated to some discrete variable *)
val get_discrete_value : global_location -> discrete_index -> discrete_value

(** Get a hash value for a location, including discrete values *)
val hash_code : global_location -> int

(** Checks whether a global_location satisfies a state_predicate *)
val match_state_predicate : AbstractProperty.state_predicate -> global_location -> bool



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** 'string_of_location automata_names location_names discrete_names location' converts a location to a string. The Boolean indicates whether the discrete variables should be converted into float or not *)
val string_of_location : (automaton_index -> automaton_name) -> (automaton_index -> location_index -> location_name) -> (discrete_index -> variable_name) -> bool -> global_location -> string
