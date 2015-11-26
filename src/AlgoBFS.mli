(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: main class to explore the state space in breadth-first search manner
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/23
 * Last modified     : 2015/11/26
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)

(**************************************************************)
(* Class definition *)
(**************************************************************)
class virtual algoBFS :
	object
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable state_space : StateSpace.state_space
		
		method virtual algorithm_name : string
		
		method initialize_variables : unit
		
		method run : StateSpace.state -> Result.imitator_result
		
		method virtual compute_result : Result.imitator_result
end
