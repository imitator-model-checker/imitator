(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: EFsynth algorithm [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/25
 * Last modified     : 2015/11/25
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open AlgoBFS

(**************************************************************)
(* Class definition *)
(**************************************************************)
class algoEFsynth :
	object inherit algoBFS
		method algorithm_name : string

		method run : StateSpace.state -> Result.imitator_result
		
		method initialize_variables : unit
		
		method compute_result : Result.imitator_result
end