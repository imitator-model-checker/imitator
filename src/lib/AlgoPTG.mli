(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Parametric timed game with reachability condition
 *
 * File contributors : Étienne André
 * Created           : 2022/11/30
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric
open State

(* State in or out of state space *)
type ptg_state =
| InSP of state_index
| NotInSP of State.state

(************************************************************)
(* Class definition *)
(************************************************************)
class algoPTG : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate -> AbstractProperty.state_predicate option ->
	object inherit algoGeneric

		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Name of the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)

		method run : Result.imitator_result

end
