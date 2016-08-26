(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Abstract tiles manager class to manage the tiles received in the cartography algorithms.
 * 
 * File contributors : Étienne André
 * Created           : 2016/08/15
 * Last modified     : 2016/08/26
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Result




(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)

(*------------------------------------------------------------*)
(** Check if a parameter valuation belongs to the constraint of an abstract_point_based_result *)
(*------------------------------------------------------------*)
let pi0_in_tiles pval (abstract_point_based_result : abstract_point_based_result) =
	match abstract_point_based_result.result with
(*	| LinearConstraint.Convex_p_constraint p_linear_constraint -> LinearConstraint.is_pi0_compatible pval#get_value p_linear_constraint
	| LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint -> LinearConstraint.p_nnconvex_constraint_is_pi0_compatible pval#get_value p_nnconvex_constraint*)
	
	(*** NOTE: we do not investigate soundness; i.e., even if the tile is invalid, we still check whether the valuation belongs to the tile ***)
	
	(* Only good valuations *)
	| Good_constraint (p_nnconvex_constraint, _)
	(* Only bad valuations *)
	| Bad_constraint (p_nnconvex_constraint, _)
		-> LinearConstraint.p_nnconvex_constraint_is_pi0_compatible pval#get_value p_nnconvex_constraint
		
	(* Both good and bad valuations *)
	| Good_bad_constraint good_and_bad_constraint ->
		let good_p_nnconvex_constraint, _ = good_and_bad_constraint.good in
		let bad_p_nnconvex_constraint, _ = good_and_bad_constraint.bad in
		LinearConstraint.p_nnconvex_constraint_is_pi0_compatible pval#get_value good_p_nnconvex_constraint
		||
		LinearConstraint.p_nnconvex_constraint_is_pi0_compatible pval#get_value bad_p_nnconvex_constraint


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual tilesManager =
	object (self)
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Initialize the manager *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual initialize : unit
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get the number of results processed and stored *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual get_nb_results : int
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check if a parameter valuation belongs to the tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual pval_in_tiles : PVal.pval -> bool

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Process a new tile, i.e., add it to the tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual process_tile : Result.abstract_point_based_result -> unit

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Process the result of the cartography *)
	(* If forced_coverage_option <> None, then the coverage is set to this argument; otherwise, it is computed as exepected (*** NOTE: used in Random to force the coverage to Unknown ***) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual process_result : float -> NumConst.t -> int -> Result.bc_algorithm_termination -> Result.bc_coverage option -> Result.imitator_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
