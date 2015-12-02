(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: main class to explore the state space in breadth-first search manner; virual class (no concrete instance of this class should be created)
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/23
 * Last modified     : 2015/12/02
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open AlgoGeneric

(**************************************************************)
(* Class definition *)
(**************************************************************)
class virtual algoBFS :
	object inherit algoGeneric
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable state_space : StateSpace.state_space

		(* Set the PaTATOR termination function *)
		method set_patator_termination_function : (unit -> unit) -> unit
	
		(* Main method to run the algorithm; implements here a BFS search, and call other functions that may be modified in subclasses *)
		method run : StateSpace.state -> Result.imitator_result
		
end
