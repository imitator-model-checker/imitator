open AbstractModel
open Graph
open Options

val get_program: unit -> abstract_program
val set_program: abstract_program -> unit

val get_pi0: unit -> pi0
val set_pi0: pi0 -> unit

val get_pi0cube: unit -> pi0cube
val set_pi0cube: pi0cube -> unit

val get_options: unit -> imitator_options
val set_options: imitator_options -> unit

val get_reachability_graph: unit -> reachability_graph
val set_reachability_graph: reachability_graph -> unit

val get_abstract_reachability_graph: unit -> abstract_reachability_graph
val set_abstract_reachability_graph: abstract_reachability_graph -> unit

