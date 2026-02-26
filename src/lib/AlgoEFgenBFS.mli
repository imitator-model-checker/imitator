(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: algorithms based on "EF" (real BFS version)
 *
 * File contributors : Étienne André
 * Created           : 2024/02/07
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric


(************************************************************)
(* Class definition: EU (virtual) *)
(************************************************************)
class virtual algoEUgenBFS : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> bool -> AbstractProperty.state_predicate option -> AbstractProperty.state_predicate -> AbstractProperty.timed_interval option ->
	object inherit algoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Name of the algorithm *)
		(*------------------------------------------------------------*)
		method virtual algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(** Main method to run the algorithm *)
		method run : Result.imitator_result
end


(************************************************************)
(* Class definition: EF *)
(************************************************************)
class algoEFBFS : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate ->
	object inherit algoEUgenBFS
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Name of the algorithm *)
		(*------------------------------------------------------------*)
		method algorithm_name : string

end


(************************************************************)
(* Class definition: EU *)
(************************************************************)
class algoEUBFS : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate -> AbstractProperty.state_predicate ->
	object inherit algoEUgenBFS
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Name of the algorithm *)
		(*------------------------------------------------------------*)
		method algorithm_name : string

end


(************************************************************)
(* Class definition: EW *)
(************************************************************)
class algoEWBFS : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate -> AbstractProperty.state_predicate ->
	object inherit algoEUgenBFS
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Name of the algorithm *)
		(*------------------------------------------------------------*)
		method algorithm_name : string

end



(************************************************************)
(* Class definition: AGnot *)
(************************************************************)
class algoAGnotBFS : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate ->
	object inherit algoEUgenBFS
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Name of the algorithm *)
		(*------------------------------------------------------------*)
		method algorithm_name : string

end


(************************************************************)
(* Class definition: AGnot *)
(************************************************************)
class algoAGBFS : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> AbstractProperty.state_predicate ->
	object inherit algoEUgenBFS
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Name of the algorithm *)
		(*------------------------------------------------------------*)
		method algorithm_name : string

end
