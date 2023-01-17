(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: define discrete states (global locations)
 * 
 * File contributors        : Étienne André
 * Created                  : 2010/03/10
 * Renamed from Automaton.ml: 2015/10/22
 *
 ************************************************************)



(************************************************************)
(* Modules *)
(************************************************************)
open Automaton


(************************************************************)
(** {2 Types} *)
(************************************************************)

(** Unique identifier for each different global location *)
type global_location_index = int

(** Global location: location for each automaton + value of the discrete *)
type global_location

type discrete_valuation = Automaton.discrete_index -> AbstractValue.abstract_value
type discrete_setter = Automaton.discrete_index -> AbstractValue.abstract_value -> unit
type local_discrete_valuation = variable_ref -> AbstractValue.abstract_value
type local_discrete_setter = variable_ref -> AbstractValue.abstract_value -> unit
type discrete_access = discrete_valuation * discrete_setter * local_discrete_valuation * local_discrete_setter

(** Should the float be displaid using exact rationals or (possibly approximated) floats? *)
type rational_display =
	| Exact_display
	| Float_display

(* Local variables table type *)
type local_variables_table = (variable_ref, AbstractValue.abstract_value) Hashtbl.t

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
val make_location : (automaton_index * location_index) list -> (discrete_index * AbstractValue.abstract_value) list -> local_variables_table -> global_location

(** 'update_location locations discrete_values location' creates a new location from the original location, and update the given automata and discrete variables. *)
(*val update_location : (automaton_index * location_index) list -> (discrete_index * AbstractValue.abstract_value) list -> global_location -> global_location*)

(* Side-effect function for updating a discrete variable given a value at given location *)
(*val update_discrete_with : discrete_index * AbstractValue.abstract_value -> global_location -> unit*)

(** Side-effet version of update_location. *)
val update_location_with : (automaton_index * location_index) list -> global_location -> unit

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
val get_discrete_value : global_location -> discrete_index -> AbstractValue.abstract_value
(** Get the NumConst value associated to some discrete variable *)
val get_discrete_rational_value : global_location -> discrete_index -> NumConst.t
(** Set the value associated to some discrete variable *)
val set_discrete_value : global_location -> discrete_index -> AbstractValue.abstract_value -> unit
(** Get a tuple of functions for reading / writing a global variable at a given location *)
(* A discrete access enable to read or write a value of a variable at a given discrete index *)
val discrete_access_of_location : global_location -> discrete_access

(** Get a hash value for a location, including discrete values *)
val hash_code : global_location -> int

(** Check whether a global location is accepting according to the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
val is_accepting : (automaton_index -> location_index -> bool) -> global_location -> bool



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** 'string_of_location automata_names location_names discrete_names location' converts a location to a string. *)
val string_of_location : (automaton_index -> automaton_name) -> (automaton_index -> location_index -> location_name) -> (discrete_index -> variable_name) -> rational_display -> global_location -> string
