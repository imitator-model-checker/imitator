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

(** Parametric timed game with reachability condition. *)



(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric
open State

(** Successor is either cached in the state space (InSP) or a transient re-explored state (NotInSP) *)
type ptg_state =
| InSP of state_index
| NotInSP of DiscreteState.global_location * LinearConstraint.px_linear_constraint

(************************************************************)
(* Class definition *)
(************************************************************)
class algoPTG : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate -> AbstractProperty.state_predicate option -> (AbstractModel.abstract_model -> unit) ->
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
