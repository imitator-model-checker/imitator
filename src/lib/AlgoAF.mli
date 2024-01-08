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
open AlgoGeneric


(************************************************************)
(* Class definition *)
(************************************************************)
class algoAF : AbstractModel.abstract_model -> Options.imitator_options -> AbstractProperty.state_predicate ->
	object inherit algoGeneric
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

		(** Main method to run the algorithm *)
		method run : Result.imitator_result
end