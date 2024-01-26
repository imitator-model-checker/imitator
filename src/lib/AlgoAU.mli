(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: "AU" algorithm (always … until …)
 * 
 * File contributors : Étienne André
 * Created           : 2024/01/26
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoAUgen


(************************************************************)
(* Class definition *)
(************************************************************)
class algoAU : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate -> AbstractProperty.state_predicate ->
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