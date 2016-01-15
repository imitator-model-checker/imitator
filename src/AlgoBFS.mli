(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: main class to explore the state space in breadth-first search manner; virual class (no concrete instance of this class should be created)
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/23
 * Last modified     : 2016/01/15
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
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable state_space : StateSpace.state_space

		(* Status of the analysis *)
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable termination_status : Result.algorithm_termination option

		(* Constraint of the initial state (used by some algorithms to initialize their variables) *)
		val mutable initial_constraint : LinearConstraint.px_linear_constraint option
		
		
		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(* Set the PaTATOR termination function *)
		method set_patator_termination_function : (unit -> unit) -> unit
	
		(* Main method to run the algorithm; implements here a BFS search, and call other functions that may be modified in subclasses *)
		method run : unit -> Result.imitator_result
		
end
