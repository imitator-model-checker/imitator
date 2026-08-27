(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module helps to format Json documents
 *
 * File contributors : Benjamin L.
 * Created           : 2022/06/22
 *
 ************************************************************)

(** This module helps to format Json documents. *)


(** A JSON value (object, array, string, Boolean, int, float, null, or raw text). *)
type json_element =
    | Json_struct of json_property list
    | Json_array of json_element list
    | Json_string of string
    | Json_bool of bool
    | Json_int of int
    | Json_float of float
    | Json_null
    | Json_raw of string

(** A key/value pair inside a JSON object. *)
and json_property = string * json_element

(* Get Json string of a given Json structure *)
val to_string : ?pretty:bool -> json_element -> string