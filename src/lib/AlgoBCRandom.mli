(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Random Behavioral Cartography with a maximum number of consecutive failed attempts to find a non-integer point not covered by any tile [AF10]
 * 
 * File contributors : Étienne André
 * Created           : 2016/02/02
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoCartoGeneric



(************************************************************)
(* Class definition *)
(************************************************************)
class algoBCRandom : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> HyperRectangle.hyper_rectangle -> NumConst.t -> int -> (PVal.pval -> AlgoStateBased.algoStateBased) -> tiles_storage ->
	object inherit algoCartoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		(* Create the initial point for the analysis *)
		method get_initial_point : more_points

		(* Find the next point *)
		method find_next_point : more_points

		method compute_bc_result : Result.imitator_result
end