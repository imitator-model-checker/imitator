(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: "AG" algorithm (global invariant)
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
class algoAG : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate ->
	object inherit algoAGnot
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string


end