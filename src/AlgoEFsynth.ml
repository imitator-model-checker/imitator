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
 * Last modified     : 2015/11/25
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open OCamlUtilities
open ImitatorUtilities
open AbstractModel
open AlgoBFS



(**************************************************************)
(* Class definition *)
(**************************************************************)
class algoEFsynth =
	object (self) inherit algoBFS as super
	
	
	(*** TODO: put something else there ***)
	val mutable k_result = LinearConstraint.p_false_constraint()
	
	val mutable p_constraints = []

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFsynth"
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		(* First initialize superclass variables *)
		super#initialize_variables;

		(*** TODO !!! ***)
(* 		k_result <- LinearConstraint.px_hide_nonparameters_and_collapse init_constr; *)

		p_constraints <- [];

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			(* Retrieve the model *)
			let model = Input.get_model () in
			print_message Verbose_low ("Initialized k_result to ");
			print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names k_result);
			print_message Verbose_low ("");
		)

	

	method compute_result = Result.BFS_noresult
end;;