(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Behavioral Cartography with exhaustive coverage of integer points and learning-based abstraction.
 * 
 * File contributors : Étienne André
 * Created           : 2016/07/22
 * Last modified     : 2016/07/22
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoBCCover



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoBCCoverLearning =
	object (self) inherit algoBCCover as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full coverage with learning-based abstraction)"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Safety redefinition *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: safety redefinition: this cartography algorithm should NOT be parameterized by a method, as the method to be called on each point is statically chosen (it will be either EFsynth or PRP, depending on the abstraction) ***)
	method set_algo_instance_function _ : unit =
		raise (InternalError "Method 'set_algo_instance_function' should NOT be used in BCCoverlearning")

			
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		(* The end *)
		()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Call the algorithm on the current point: 1) run the abstraction 2) call either EFsynth or PRP depending on the result *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method call_point =
	
		(*** TODO ***)
		
		(* Save the verbose mode as it may be modified *)
		let global_verbose_mode = get_verbose_mode() in

		(* Prevent the verbose messages (except in verbose medium, high or total) *)
		(*------------------------------------------------------------*)
		if not (verbose_mode_greater Verbose_medium) then
			set_verbose_mode Verbose_mute;
					
		(* Call the algorithm to be iterated on (typically IM or PRP) *)
		(*** NOTE: the bc time limit is NOT checked inside one execution of the algorithm to be iterated (but also note that the max execution time of the algorithm to be iterated is set to that of BC, in the Options pre-processing) ***)
		current_algo_instance <- self#get_algo_instance_function ();
		let imitator_result : imitator_result = current_algo_instance#run() in

		(** Create auxiliary files with the proper file prefix, if requested *)
		self#create_auxiliary_files imitator_result;

		(* Get the verbose mode back *)
		set_verbose_mode global_verbose_mode;
		(*------------------------------------------------------------*)
		
		(* Return result *)
		imitator_result
		


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
