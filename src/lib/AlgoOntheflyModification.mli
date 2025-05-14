(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Temporary on-the-fly model modification
 * 
 * File contributors : Étienne André
 * Created           : 2025/05/14
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric

(************************************************************)
(* Class definition *)
(************************************************************)
class algoOntheflyModification : AbstractModel.abstract_model -> Options.imitator_options ->
	object inherit algoGeneric
	
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		method algorithm_name : string

		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
			(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Main method to run the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method run : Result.imitator_result
end