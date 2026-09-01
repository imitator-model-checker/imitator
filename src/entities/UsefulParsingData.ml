(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Useful parsed model data for on-the-fly updates
 *
 * File contributors : Quang Minh Ta
 * Created           : 2026/09/01
 *
 ************************************************************)

(** Useful parsed model data for on-the-fly updates
Store some useful parsing attributes after parsing the initial model 
to use for future update
*)

(* open ParsingStructure
open ParsingStructureUtilities;;
open ImitatorUtilities;;

(** The same shape as the syntactic-variable data used during template expansion. *)
type synt_vars_data = (variable_name * synt_var_kind * int) list

(** A small bundle of data extracted from the initial parsed model and kept for
    later on-the-fly updates. *)
type useful_parsing_context = {
  variable_declarations : variable_declarations;
  synt_vars : synt_vars_data;
}

let empty = {
  variable_declarations = [];
  synt_vars = [];
} *)