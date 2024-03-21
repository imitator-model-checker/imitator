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

type json_element =
    | Json_struct of json_property list
    | Json_array of json_element list
    | Json_string of string
    | Json_bool of bool
    | Json_int of int
    | Json_float of float
    | Json_null
    | Json_raw of string

and json_property = string * json_element

let quote s = "\"" ^ s ^ "\""

(* Get Json string of a given Json structure *)
let to_string ?(pretty = false) json_element =

    let space, new_line = if pretty then " ", "\n" else "", "" in

    let rec to_pretty_string_rec level json_element =

        let tabs, tabs_1 =
            if pretty then
                OCamlUtilities.string_n_times level "  ",
                OCamlUtilities.string_n_times (level + 1) "  "
            else
                "", ""
        in

        match json_element with
        | Json_struct json_properties ->
            let str_properties = List.map (fun (key, value) -> tabs_1 ^ (quote key) ^ ":" ^ space ^ to_pretty_string_rec (level + 1) value) json_properties in
            "{" ^ new_line ^ OCamlUtilities.string_of_list_of_string_with_sep ("," ^ new_line) str_properties ^ new_line ^ tabs ^ "}"

        | Json_array json_elements ->
            let str_elements = List.map (fun e -> tabs_1 ^ to_pretty_string_rec (level + 1) e) json_elements in
            "[" ^ new_line ^ OCamlUtilities.string_of_list_of_string_with_sep ("," ^ new_line) str_elements ^ new_line ^ tabs ^ "]"

        | Json_string s -> quote s
        | Json_bool b -> string_of_bool b
        | Json_int i -> string_of_int i
        | Json_float f -> string_of_float f
        | Json_null -> "null"
        | Json_raw s -> s
    in
    to_pretty_string_rec 0 json_element