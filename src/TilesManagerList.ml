(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Concrete implementation of the tiles manager class to manage the tiles received in the cartography algorithms, using a list of Result.good_or_bad_constraint
 * 
 * File contributors : Étienne André
 * Created           : 2016/08/15
 * Last modified     : 2016/08/24
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
class tilesManagerList =
	object (self) inherit tilesManager as super
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* List of results stored as a list *)
	val mutable abstract_point_based_results : abstract_point_based_result list = []

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Initialize the manager *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize =
		abstract_point_based_results <- []

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get the number of results processed and stored *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method get_nb_results =
		List.length abstract_point_based_results

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check if a parameter valuation belongs to the tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method pval_in_tiles pval =
		List.exists (pi0_in_tiles pval) abstract_point_based_results
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Process a new tile, i.e., add it to the tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_tile tile =
		abstract_point_based_results <- tile :: abstract_point_based_results

	
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
			(* INTEGER COMPLETE if termination is regular and all tiles are exact or under-approximations *)
			if termination_status = BC_Regular_termination &&
(*				(List.for_all (fun abstract_point_based_result -> match abstract_point_based_result.soundness with
					| Constraint_exact | Constraint_maybe_under -> true
					| Constraint_maybe_over | Constraint_maybe_invalid -> false
					| Constraint_under_over -> raise (InternalError("BC is not suppose to handle under/over-approximations"))
				) abstract_point_based_results*)
				(List.for_all (fun (abstract_point_based_result : abstract_point_based_result) -> match abstract_point_based_result.result with
					| Good_constraint (_, Constraint_exact)
					| Good_constraint (_, Constraint_maybe_under)
					| Bad_constraint (_, Constraint_exact)
					| Bad_constraint (_, Constraint_maybe_under)
					-> true
					| Good_constraint (_, Constraint_maybe_over)
					| Good_constraint (_, Constraint_maybe_invalid)
					| Bad_constraint (_, Constraint_maybe_over)
					| Bad_constraint (_, Constraint_maybe_invalid)
					-> false
					| Good_bad_constraint good_and_bad_constraint ->
						let _, good_soundness = good_and_bad_constraint.good in
						let _, bad_soundness = good_and_bad_constraint.bad in
						let result =
						match (good_soundness, bad_soundness) with
						| Constraint_exact, Constraint_exact
						| Constraint_exact, Constraint_maybe_under
						| Constraint_maybe_under, Constraint_exact
						| Constraint_maybe_under, Constraint_maybe_under
						-> true
						| _ -> false
						in
						result
				) abstract_point_based_results
				)
				then Coverage_integer_complete
			(* UNKNOWN otherwise *)
			else Coverage_unknown
		in

		(*		(* Coverage is... *)
		(*** NOTE: this is only true for the original behavioral cartography; for variants this may not hold ***)
		let coverage =
			(* INTEGER COMPLETE if termination is regular and all tiles are valid *)
			if termination_status = BC_Regular_termination && (List.for_all (fun abstract_im_result -> match abstract_im_result.soundness with
					| Constraint_maybe_under | Constraint_exact | Constraint_maybe_over -> true
					| Constraint_maybe_invalid -> false
				) abstract_point_based_results)
				then Coverage_integer_complete
			(* UNKNOWN otherwise *)
			else Coverage_unknown
		in*)
		
		(* Return result *)
		Cartography_result {
			(* Number of points in V0 *)
			size_v0				= nb_points_in_v0;
			
			(* List of tiles *)
			(*** NOTE: reverse as each result was added as first element ***)
			tiles				= List.rev abstract_point_based_results;
			
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
