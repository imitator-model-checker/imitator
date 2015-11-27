(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: EFsynth algorithm [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/25
 * Last modified     : 2015/11/27
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open OCamlUtilities
open ImitatorUtilities
open AbstractModel
open Result
open AlgoBFS



(**************************************************************)
(* Class definition *)
(**************************************************************)
class algoEFsynth =
	object (self) inherit algoBFS as super
	
	(* List of constraints allowing the reachability of the bad location *)
	val mutable bad_constraints = []

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFsynth"
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		bad_constraints <- [];

(*		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			(* Retrieve the model *)
			let model = Input.get_model () in
			print_message Verbose_low ("Initialized k_result to ");
			print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names k_result);
			print_message Verbose_low ("");
		)*)

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(*** HACK: dummy result to ensure compiling ***)
		EFsynth_result
		{
			(* List of constraints ensuring EF location *)
			constraints			= bad_constraints;
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Nature of the state space (needed??) *)
		(* 	tile_nature			: AbstractModel.tile_nature; *)
			
			(* Total computation time of the algorithm *)
			computation_time	= 0.;
			
			(* Termination *)
			termination			= Regular_termination;
		}
	
end;;