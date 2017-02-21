(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Concrete implementation of the tiles manager class to manage the tiles received in the cartography algorithms, using a single Result.good_or_bad_constraint
 * 
 * File contributors : Étienne André
 * Created           : 2016/08/26
 * Last modified     : 2017/01/18
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open Exceptions
open ImitatorUtilities
open Result
open TilesManager



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class tilesManagerConstraint =
	object (self) inherit tilesManager as super
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* List of results stored as a single good_or_bad_constraint *)
	(*** NOTE: initially, both constraints are false (no valuation computed) and exact ***)
	val mutable result : good_or_bad_constraint =
		Good_bad_constraint {
			good	= LinearConstraint.false_p_nnconvex_constraint (), Constraint_exact;
			bad		= LinearConstraint.false_p_nnconvex_constraint (), Constraint_exact;
		}
	
	(* Keep track of the number of results processed and added *)
	val mutable nb_results_added = 0

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Initialize the manager *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize =
	(*** BADPROG: in fact, this initialization is already done when the object is created (but safer to reinitialize just in case) ***)
	(*** NOTE: initially, both constraints are false (no valuation computed) and exact ***)
	result <-
		Good_bad_constraint {
			good	= LinearConstraint.false_p_nnconvex_constraint (), Constraint_exact;
			bad		= LinearConstraint.false_p_nnconvex_constraint (), Constraint_exact;
		}
	;
	(* Also reset the number of results processed *)
	nb_results_added <- 0

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get the number of results processed and stored *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method get_nb_results =
		nb_results_added

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check if a parameter valuation belongs to the tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method pval_in_tiles pval =
		pval_in_good_or_bad_constraint pval result
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Process a new tile, i.e., add it to the tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_tile abstract_point_based_result =
		(*** NOTE: for now, we do not keep the tiles in memory ***)
		(* Get the constraint *)
		let tile_result = abstract_point_based_result.result in
		(* Add to the local result *)
		good_or_bad_constraint_union_assign result tile_result;
		(* Increment nb_results_added (for statistics purpose) *)
		nb_results_added <- nb_results_added + 1

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Process the result of the cartography *)
	(* If forced_coverage_option <> None, then the coverage is set to this argument; otherwise, it is computed as exepected (*** NOTE: used in Random to force the coverage to Unknown ***) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_result start_time nb_points_in_v0 nb_unsuccessful_points termination_status (forced_coverage_option : Result.bc_coverage option) =

		(* Coverage is... *)
		let coverage = match forced_coverage_option with
			(* If set already: keep it *)
			| Some coverage -> coverage
			(* If not set: compute it in the standard manner *)
			| None ->
			(*** NOTE: this is only true for the original behavioral cartography; for variants this may not hold ***)
			
			(* If no constraint: empty *)
			if is_good_or_bad_constraint_empty result then Coverage_empty
			else
			(* If some points skipped: cannot say anything *)
			if point_skipped
				then Coverage_unknown
			else
			(* INTEGER COMPLETE if termination is regular and all tiles are exact or under-approximations *)
			if termination_status = BC_Regular_termination && is_good_or_bad_constraint_sound result
				then Coverage_integer_complete
			(* UNKNOWN otherwise *)
			else Coverage_unknown
		in

		(* Return result *)
		Multiple_synthesis_result {
			(* Number of points in V0 *)
			size_v0				= nb_points_in_v0;
			
			(* Good and/or bad valuations *)
			result				= result;

			(* List of tiles *)
			(*** NOTE: reverse as each result was added as first element ***)
(* 			tiles				= List.rev abstract_point_based_results; *)
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Number of points on which IM could not be called because already covered *)
			nb_unsuccessful_points = nb_unsuccessful_points;
			
			(* Evaluation of the coverage of V0 by tiles computed by the cartography *)
			coverage			= coverage;
			
			(* Termination *)
			termination			= termination_status;
		}



(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
