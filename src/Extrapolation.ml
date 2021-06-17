(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: clocks extrapolation
 *
 * File contributors : Étienne André, Johan Arcile
 * Created           : 2021/06/17
 * Last modified     : 2021/06/17
 *
 ************************************************************)
 

(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open OCamlUtilities

open AbstractModel
open LinearConstraint
open Options


(************************************************************)
(* Type for NumConst or infinity *)
(************************************************************)

type numconst_or_infinity =
	(* Regular NumConst *)
	| Finite of NumConst.t
	(* Infinity *)
	| Infinity
	(* Minus-infinity *)
	| Minus_infinity

let add_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	match a, b with
	| Infinity, Infinity -> Infinity
	| Minus_infinity, Minus_infinity -> Minus_infinity
	| Finite a, Finite b-> Finite (NumConst.add a b)
	| _ -> raise (InternalError "Case addition infinity/-infinity")


(************************************************************)
(* Dummy global variables *)
(************************************************************)

let l = ref Infinity
let u = ref Minus_infinity


(*------------------------------------------------------------*)
(* Function for preparing data structures for extrapolation *)
(*------------------------------------------------------------*)
let prepare_extrapolation () : unit =
	(* Retrieve the model *)
	let model = Input.get_model() in
	
	let nb_parameters = model.nb_parameters in


	raise (NotImplemented "prepare_extrapolation")



(*------------------------------------------------------------*)
(* Apply extrapolation *)
(*------------------------------------------------------------*)
let px_lu_extrapolation (x : Automaton.clock_index) (px_linear_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =
	(* Retrieve the model *)
	let model = Input.get_model() in
	
	let nb_parameters = model.nb_parameters in

	raise (NotImplemented "px_lu_extrapolation")


