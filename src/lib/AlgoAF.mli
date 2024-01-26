(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: "AF" algorithm (always eventually)
 * 
 * File contributors : Étienne André
 * Created           : 2024/01/08
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoAUgen


(************************************************************)
(* Class definition *)
(************************************************************)
class algoAF : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate ->
	object inherit algoAUgen
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Name of the algorithm *)
		(*------------------------------------------------------------*)
		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)

end