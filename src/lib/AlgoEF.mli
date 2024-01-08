(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: "EF" algorithm (unsafe w.r.t. a set of bad states) [JLR15]
 *
 * File contributors : Étienne André
 * Created           : 2023/12/18
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoEUgen


(************************************************************)
(* Class definition *)
(************************************************************)
class algoEF : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate ->
	object inherit algoEUgen
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Name of the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method algorithm_name : string

		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Method packaging the result output by the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_result : Result.imitator_result

end