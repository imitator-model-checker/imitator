(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: defines indexes and names for variables,actions,etc. in PTA
 * 
 * File contributors : Étienne André
 * Created           : 2010/03/10
 * Last modified     : 2019/12/10
 *
 ************************************************************)


(************************************************************)
(** Indexes *)
(************************************************************)

type action_index		= int
type action_name		= string

type automaton_index	= int
type automaton_name		= string

type location_index		= int
type location_name		= string

type variable_index		= int
type clock_index		= variable_index
type parameter_index	= variable_index
type discrete_index		= variable_index
type discrete_value		= NumConst.t
type variable_value		= NumConst.t
type variable_name		= string
