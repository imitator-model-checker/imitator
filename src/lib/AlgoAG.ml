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
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open AbstractProperty
open AlgoAGnot



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
(*** NOTE: AG is implemented as AGnot called on the **negation** of the state predicate ***)
class algoAG (model : AbstractModel.abstract_model) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoAGnot model options (State_predicate_term (State_predicate_factor (State_predicate_factor_NOT (State_predicate state_predicate)))) (*as super*)
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method! algorithm_name = "AG"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
