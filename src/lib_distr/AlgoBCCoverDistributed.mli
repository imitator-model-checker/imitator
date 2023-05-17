(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with point-based distribution of points. [ACE14,ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/04
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric
open AlgoCartoGeneric


(************************************************************)
(* Class definition *)
(************************************************************)
class virtual algoBCCoverDistributed : AbstractModel.abstract_model -> HyperRectangle.hyper_rectangle -> NumConst.t -> (PVal.pval -> AlgoStateBased.algoStateBased) -> AlgoCartoGeneric.tiles_storage ->
	object
	inherit algoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		(* The current algorithm instance *)
		val mutable current_algo_instance : AlgoStateBased.algoStateBased

		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method virtual algorithm_name : string
		
		method virtual initialize_variables : unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Run IM and return an abstract_point_based_result. Parameters are the reference valuation and the termination function to be set in IM (for PaTATOR). *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method run_im : PVal.pval -> (unit -> unit) option -> Result.abstract_point_based_result
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Processing the result of IM *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual run : unit -> Result.imitator_result


end