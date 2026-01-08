
open Spec

exception Parse_error of string

let range_from_bounds lo hi =
  if lo > hi then
    invalid_arg "range_from_bounds: lo > hi";
  Range (lo, hi)

let parse_dist (s : string) : dist =
  let s = String.trim s in
  match String.split_on_char '.' s with
  | [a; ""; b] ->
      let lo = int_of_string (String.trim a) in
      let hi = int_of_string (String.trim b) in
      range_from_bounds lo hi
  | [_] ->
      Exact (int_of_string s)
  | _ ->
      raise (Parse_error ("invalid dist: " ^ s))

let parse_constraint_type (s : string) : constraint_type =
  match String.trim s with
  | "<=" -> S_LE
  | "<"  -> S_LT
  | ">=" -> S_GE
  | ">"  -> S_GT
  | "="  -> S_EQ
  | _    -> raise (Parse_error ("invalid constraint type: " ^ s))

let parse_string_list_semicolon (s : string) : string list =
  let s = String.trim s in
  let len = String.length s in
  if len < 2 || s.[0] <> '[' || s.[len - 1] <> ']' then
    raise (Parse_error ("invalid config list (expected [...]): " ^ s));

  let inner = String.sub s 1 (len - 2) |> String.trim in
  if inner = "" then
    []
  else
    inner
    |> String.split_on_char ';'
    |> List.map String.trim
    |> List.filter (fun x -> x <> "")

let parse_float s =
  try float_of_string (String.trim s)
  with Failure _ -> raise (Parse_error ("invalid float: " ^ s))

let parse_bool s =
  match String.trim s with
  | "true"  -> true
  | "false" -> false
  | _ -> raise (Parse_error ("invalid bool: " ^ s))

let parse_int s =
  try int_of_string (String.trim s)
  with Failure _ -> raise (Parse_error ("invalid int: " ^ s))

let split_on_first (c : char) (s : string) : (string * string) option =
  match String.index_opt s c with
  | None -> None
  | Some i ->
      let left = String.sub s 0 i in
      let right = String.sub s (i + 1) (String.length s - i - 1) in
      Some (left, right)

let require name = function
  | Some v -> v
  | None -> raise (Parse_error (Printf.sprintf "required field `%s` not provided" name))

let optional ~default = Option.value ~default


let argv_of_string (s : string) : string array =
  s
  |> String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c)
  |> String.trim
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> Array.of_list

let argv_list_of_configs (configs : string list) : string array list =
  List.map argv_of_string configs

let count_char s ch =
  let n = ref 0 in
  String.iter (fun c -> if c = ch then incr n) s;
  !n

let brackets_balanced s =
  count_char s '[' = count_char s ']'