(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module helps to log custom details as JSON format to res file
 *
 * File contributors : Benjamin L.
 * Created           : 2022/07/19
 *
 ************************************************************)

(* Utils modules *)
open Exceptions
open JsonFormatter

val custom_details : (string, json_element) Hashtbl.t
(* Add a property to custom details json struct *)
val add_custom_detail_property : string -> json_element -> unit
(* Add a property to custom details json struct *)
val add_custom_detail_array : string -> json_element -> unit

(* Get json structure of all custom details *)
val json_struct_of_details : unit -> json_element
(* Get json string of all custom details *)
val json_string_of_details : unit -> string