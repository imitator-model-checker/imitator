open Spec

exception Parse_error of string

type partial = {
  nb_automata   : dist option;
  nb_locations  : dist option;
  nb_clocks     : dist option;
  nb_parameters : dist option;
}

let empty : partial = {
  nb_automata   = None;
  nb_locations  = None;
  nb_clocks     = None;
  nb_parameters = None;
}


let parse_dist (s : string) : dist =
  let s = String.trim s in
  match String.split_on_char '.' s with
  | [a; ""; b] ->
      let lo = int_of_string (String.trim a) in
      let hi = int_of_string (String.trim b) in
      if lo > hi then
        raise (Parse_error "invalid range: lower bound > upper bound");
      Range (lo, (hi - lo) + 1)
  | [_] ->
      Exact (int_of_string s)
  | _ ->
      raise (Parse_error ("invalid dist: " ^ s))


let parse_line (acc : partial) (line : string) : partial =
  let line = String.trim line in
  if line = "" || line.[0] = '#' then acc
  else
    match String.split_on_char '=' line with
    | [key; value] ->
        let key = String.trim key in
        let value = parse_dist value in
        begin match key with
        | "nb_automata"   -> { acc with nb_automata   = Some value }
        | "nb_locations"  -> { acc with nb_locations  = Some value }
        | "nb_clocks"     -> { acc with nb_clocks     = Some value }
        | "nb_parameters" -> { acc with nb_parameters = Some value }
        | _ ->
            raise (Parse_error ("unknown key: " ^ key))
        end
    | _ ->
        raise (Parse_error ("invalid line: " ^ line))

let require name = function
  | Some v -> v
  | None -> raise (Parse_error (Printf.sprintf "required field `%s` not provided" name))

let optional ~default = Option.value ~default

let finalize (p : partial) : Spec.t =
  {
    nb_automata   = optional p.nb_automata ~default:(Exact 1);
    nb_locations  = require "nb_locations" p.nb_locations;
    nb_clocks     = require "nb_clocks" p.nb_clocks;
    nb_parameters = require "nb_parameters" p.nb_parameters;
  }


let parse_lines (lines : string list) : Spec.t =
  let partial =
    List.fold_left parse_line empty lines
  in
  finalize partial


let parse_file filename : Spec.t =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in ic;
        parse_lines (List.rev acc)
  in
  loop []