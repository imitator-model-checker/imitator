(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Behavioral Cartography with exhaustive coverage of integer points and learning-based abstraction.
 * 
 * File contributors : Étienne André
 * Created           : 2016/07/22
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoBCCover



(************************************************************)
(* Class definition *)
(************************************************************)
class algoBCCoverLearning : AbstractModel.abstract_model -> AbstractProperty.state_predicate -> HyperRectangle.hyper_rectangle -> NumConst.t -> (PVal.pval -> AlgoStateBased.algoStateBased) -> AlgoCartoGeneric.tiles_storage ->
	object inherit algoBCCover
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method initialize_variables : unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Call the algorithm on the current point: 1) run the abstraction 2) call either EFsynth or PRP depending on the result *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method call_point : PVal.pval -> Result.imitator_result

end