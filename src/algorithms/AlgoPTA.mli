(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Syncronized Product of Parametric Timed Automatas
 *
 * File contributors : Daniel Torres
 * Created           : 2026/09/15
 *
 ************************************************************)

(** Syncronized Product of Parametric Timed Automatas. *)

val pta_sync_prod_locations_number : int ref
val pta_sync_prod_transitions_number : int ref
val pta_sync_prod_automata_name : string array ref
val pta_sync_prod_location_names : string array ref
val pta_sync_prod_locations_per_automaton : int list ref
val pta_sync_prod_actions_per_automaton: int list ref
val pta_sync_prod_actions_per_location : int list array array ref
val pta_sync_prod_transitions : (int, int list) Hashtbl.t array ref
val pta_sync_prod_transitions_description : AbstractModel.transition array ref

val get_pta_sync_prod_locations_number : AbstractModel.abstract_model -> int
val get_pta_sync_prod_transitions_number : unit -> int
val set_pta_sync_prod_transitions_number : int -> int
val get_pta_sync_prod_automata_name : AbstractModel.abstract_model -> string array
val get_pta_sync_prod_location_names : AbstractModel.abstract_model -> string array
val get_pta_sync_prod_locations_per_automaton : AbstractModel.abstract_model -> int list
val get_pta_sync_prod_actions_per_automaton : AbstractModel.abstract_model -> int list
val get_pta_sync_prod_actions_per_location : AbstractModel.abstract_model -> int list array array
val get_pta_sync_prod_transitions : unit -> (int, int list) Hashtbl.t array
val get_pta_sync_prod_transitions_description : unit -> AbstractModel.transition array

val compute_PTA_syncronized_product : AbstractModel.abstract_model -> AbstractModel.abstract_model