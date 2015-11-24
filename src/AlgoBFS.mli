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
 * Last modified     : 2015/11/24
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)

(**************************************************************)
(* Class definition *)
(**************************************************************)
class algoBFS :
	object
		method run : StateSpace.state -> Result.imitator_result
end