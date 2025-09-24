(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Parametric timed game with reachability condition
 *
 * File contributors : Étienne André
 * Created           : 2022/11/30
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric
open State

(* State in or out of state space *)
type ptg_state = 
| InSP of state_index
| NotInSP of State.state

class virtual stateSpacePTG : object
	val mutable state_space : StateSpace.stateSpace
	method state_space : StateSpace.stateSpace
	method virtual initialize_state_space : unit -> unit
	method virtual compute_symbolic_successors : state_index -> state_index list
	method virtual unexplored_successors : int
	method virtual passed_states : State.stateIndexSet
	method virtual get_partioned_edges : state_index -> (StateSpace.combined_transition * ptg_state) list * (StateSpace.combined_transition * ptg_state) list
end

class stateSpacePTG_OTF : AbstractModel.abstract_model -> Options.imitator_options ->  object 
	inherit stateSpacePTG
	method initialize_state_space : unit -> unit
	method compute_symbolic_successors : state_index -> state_index list
	method unexplored_successors : int
	method passed_states : State.stateIndexSet
	method get_partioned_edges : state_index -> (StateSpace.combined_transition * ptg_state) list * (StateSpace.combined_transition * ptg_state) list
end

class stateSpacePTG_full : AbstractModel.abstract_model -> Options.imitator_options -> object 
	inherit stateSpacePTG
	method initialize_state_space : unit -> unit
	method compute_symbolic_successors : state_index -> state_index list
	method unexplored_successors : int
	method passed_states : State.stateIndexSet
	method get_partioned_edges : state_index -> (StateSpace.combined_transition * ptg_state) list * (StateSpace.combined_transition * ptg_state) list
end

(************************************************************)
(* Class definition *)
(************************************************************)
class algoPTG : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate -> ?state_predicate_avoid:AbstractProperty.state_predicate -> stateSpacePTG ->
	object inherit algoGeneric

		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Name of the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		method run : Result.imitator_result

end
