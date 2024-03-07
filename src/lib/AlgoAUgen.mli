(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: algorithms based on "AU": "AF", "AU", "AW"
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
(* Class definition: AU (virtual) *)
(************************************************************)
class virtual algoAUgen : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> bool -> AbstractProperty.state_predicate option -> AbstractProperty.state_predicate -> AbstractProperty.timed_interval option ->
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


(************************************************************)
(* Class definition: AF *)
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


(************************************************************)
(* Class definition: AF (timed) *)
(************************************************************)
class algoAFtimed : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate -> AbstractProperty.timed_interval ->
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


(************************************************************)
(* Class definition: AU *)
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



(************************************************************)
(* Class definition: AW *)
(************************************************************)
class algoAW : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate -> AbstractProperty.state_predicate ->
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