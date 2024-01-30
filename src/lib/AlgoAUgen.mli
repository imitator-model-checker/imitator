(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: generic algorithm for "AU" and "AF" algorithms
 *
 * File contributors : Étienne André
 * Created           : 2024/01/26
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric


(************************************************************)
(* Class definition *)
(************************************************************)
class virtual algoAUgen : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> bool -> AbstractProperty.state_predicate option -> AbstractProperty.state_predicate ->
	object inherit algoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Name of the algorithm *)
		(*------------------------------------------------------------*)
		method virtual algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(** Main method to run the algorithm *)
		method run : Result.imitator_result
end