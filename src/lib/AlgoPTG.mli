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


class virtual stateSpacePTG : object
	val mutable state_space : StateSpace.stateSpace
	method state_space : StateSpace.stateSpace
	method virtual initialize_state_space : unit -> unit
	method virtual compute_symbolic_successors : state_index -> (StateSpace.combined_transition * state_index) list
end

class stateSpacePTG_OTF : AbstractModel.abstract_model -> Options.imitator_options ->  object 
	inherit stateSpacePTG
	method initialize_state_space : unit ->unit
	method compute_symbolic_successors : state_index -> (StateSpace.combined_transition * state_index) list
end

class stateSpacePTG_full : AbstractModel.abstract_model -> Options.imitator_options -> object 
	inherit stateSpacePTG
	method initialize_state_space : unit ->unit
	method compute_symbolic_successors : state_index -> (StateSpace.combined_transition * state_index) list
end

(************************************************************)
(* Class definition *)
(************************************************************)
class algoPTG : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate -> stateSpacePTG ->
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
