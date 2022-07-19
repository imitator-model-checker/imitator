(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module helps to log to res file
 *
 * File contributors : Benjamin L.
 * Created           : 2022/07/19
 *
 ************************************************************)

open Exceptions
open JsonFormatter

let custom_details = Hashtbl.create 0

(* Add a property to custom details json struct *)
let add_custom_detail_property key details =
    Hashtbl.replace custom_details key details

(* Add an element to custom details json array *)
let add_custom_detail_array key json_element =
    (* Try to find property *)
    let json_array_opt = Hashtbl.find_opt custom_details key in
    (* Update or create json array *)
    let updated_json_array =
        match json_array_opt with
        | Some (Json_array array) -> Json_array (json_element :: array)
        | None -> Json_array [json_element]
        | _ -> raise (InternalError ("Try to add custom detail to a json array but key `" ^ key ^ "` isn't a json array element."))
    in
    (* Update or create the entry with updated json array *)
    Hashtbl.replace custom_details key updated_json_array


let json_struct_of_details _ =
    (* Custom details from hashtbl to list *)
    let custom_details_list = custom_details |> Hashtbl.to_seq |> List.of_seq in
    (* Create JSON struct with each custom detail *)
    Json_struct custom_details_list

let json_string_of_details _ =
    JsonFormatter.to_string ~pretty:true (json_struct_of_details ())