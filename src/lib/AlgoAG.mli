(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: "AG not" algorithm (global invariant)
 * 
 * File contributors : Étienne André
 * Created           : 2023/12/22
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoAGnot


(************************************************************)
(* Class definition *)
(************************************************************)
class algoAG : AbstractModel.abstract_model -> Options.imitator_options -> AbstractProperty.state_predicate ->
	object inherit algoAGnot
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string


end