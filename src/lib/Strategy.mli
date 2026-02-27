open Automaton

(* A partial view consists of the automaton taking the action and the list of locations visible to the coalition *)
type partial_view = automaton_index * (location_index list)
(* A strategy is a sorted array of (partial_view, action_index) pairs *)
type strategy = (partial_view * action_index) array
type strategy_index = int


val get_or_add_strategy : strategy -> strategy_index
val create_strategy : strategy_index -> partial_view -> action_index -> strategy_index 
val initialize_strategies:  (partial_view * action_index) list -> unit 
val get_strategy : strategy_index -> strategy
(* Kill une stratégie (la marquer comme inactive) *)
val kill_strategy : strategy_index -> bool -> unit

(** Renvoie toutes les stratégies encore actives *)
val find_all_alive_strategies : unit -> strategy_index list
val is_dead : strategy_index -> bool 

(** Propage les effets de la suppression d'une stratégie
    aux stratégies incluses (si l'option de test d'inclusion est activée) *)
val propagate_killed_strategy : unit -> unit