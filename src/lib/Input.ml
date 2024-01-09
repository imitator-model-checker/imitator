(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Global input elements (model, property)
 * 
 * File contributors : Étienne André
 * Created           : 2012/06/15
 *
 ************************************************************)
 

open Exceptions

(* internal references to global data structures *)
let model_ref = ref None
let options_ref = ref None


(************************************************************)
(* Set / get functions *)
(************************************************************)
(** Local set function *)
let set_model model =
	model_ref := Some model


let get_model () =
	match !model_ref with
		| None ->
			raise (InternalError "Input model not yet available");
		| Some model -> model

let get_options () =
	match !options_ref with
		| None ->
			raise (InternalError "Options not yet available");
		| Some options -> options

let set_options options =
	options_ref := Some options
	
