(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2014/03/15
 * Last modified: 2014/03/15
 *
 ****************************************************************) 

open Global


let run () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Get the model *)
	let model = Input.get_model() in

	(* Print some information *)
	print_message Debug_standard ("Hi guys! This is PaTATOR speaking!");

	
	(**************************************************)
	(* Bye bye! *)
	(**************************************************)
	terminate_program()

