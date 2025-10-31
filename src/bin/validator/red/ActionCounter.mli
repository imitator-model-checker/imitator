open Lib

type t

val create : ParsingStructure.parsed_model -> t

val remove_label : t -> automaton_id:int -> string -> unit

val filter_controllable_actions : t -> ParsingStructure.parsed_controllable_actions -> ParsingStructure.parsed_controllable_actions

val filter_local_actions : t -> automaton_id:int -> string list -> string list

val commit : t -> unit

val revert : t -> unit