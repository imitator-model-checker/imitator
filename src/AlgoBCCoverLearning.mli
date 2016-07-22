(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Behavioral Cartography with exhaustive coverage of integer points and learning-based abstraction.
 * 
 * File contributors : Étienne André
 * Created           : 2016/07/22
 * Last modified     : 2016/07/22
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoBCCover



(************************************************************)
(* Class definition *)
(************************************************************)
class algoBCCoverLearning :
	object inherit algoBCCover
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method initialize_variables : unit
		
		(* Sets the function creating a new instance of the algorithm to call (typically IM or PRP) *)
		(*** NOTE: for this class, the method is redefined to throw an exception ***)
		method set_algo_instance_function : (unit -> AlgoBFS.algoBFS) -> unit

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Call the algorithm on the current point: 1) run the abstraction 2) call either EFsynth or PRP depending on the result *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method call_point : Result.imitator_result

end