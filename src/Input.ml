(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Global input elements (model, property)
 * 
 * File contributors : Étienne André
 * Created           : 2012/06/15
 * Last modified     : 2019/10/16
 *
 ************************************************************)
 

open Exceptions

(* internal references to global data structures *)
let model_ref = ref None
(*let pi0_ref = ref None
let v0_ref = ref None*)
let property_ref = ref None
let options_ref = ref None


(************************************************************)
(** Set / get functions *)
(************************************************************)
(* Local set function *)
let set_model model =
	model_ref := Some model


let get_model _ =
	match !model_ref with
		| None ->
			raise (InternalError "Input model not yet available");
		| Some model -> model

let set_property property =
	property_ref := Some property


let get_property _ =
	match !property_ref with
		| None ->
			raise (InternalError "Input property not yet available");
		| Some property -> property

(*let get_pi0 _ =
	match !pi0_ref with
		| None ->
			raise (InternalError "Input pi0 not available");
		| Some pi0 -> pi0

let set_pi0 pi0 =
	pi0_ref := Some pi0

let get_v0 _ =
	match !v0_ref with
		| None ->
			raise (InternalError "Input v0 not available");
		| Some v0 -> v0

let set_v0 v0 =
	v0_ref := Some v0*)

let get_options _ =
	match !options_ref with
		| None ->
			raise (InternalError "Options not yet available");
		| Some options -> options

let set_options options =
	options_ref := Some options
	
