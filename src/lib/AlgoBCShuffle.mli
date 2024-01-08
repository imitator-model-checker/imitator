(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Shuffled version, used for the distributed cartography. [ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/14
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoCartoGeneric


(************************************************************)
(* Class definition *)
(************************************************************)
class algoBCShuffle : AbstractModel.abstract_model -> Options.imitator_options -> HyperRectangle.hyper_rectangle -> NumConst.t -> (PVal.pval -> AlgoStateBased.algoStateBased) -> tiles_storage ->
	object inherit algoCartoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method initialize_carto_variables : unit
		
		(* Create the initial point for the analysis *)
		method get_initial_point : more_points

		(* Find the next point *)
		method find_next_point : more_points

		method compute_bc_result : Result.imitator_result
end