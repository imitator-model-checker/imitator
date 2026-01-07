open Spec

exception Parse_error of string

let all_constraint_types = [S_LE; S_LT; S_GE; S_GT; S_EQ]


type partial = {
  nb_automata   : dist option;
  nb_locations  : dist option;
  nb_clocks     : dist option;
  nb_parameters : dist option;
  guard_types   : constraint_type list option;
  invariant_types : constraint_type list option;
  guard_probability : float option;
  invariant_probability : float option;
  reset_probability : float option;
  all_reachable : bool option;
  transitions_per_location : dist option;
  cycles : bool option;
  controllability_ratio : float option;
  initial_constraint_satisfiable : bool option;
}

let empty : partial = {
  nb_automata   = None;
  nb_locations  = None;
  nb_clocks     = None;
  nb_parameters = None;
  guard_types   = None;
  invariant_types = None;
  guard_probability = None;
  invariant_probability = None;
  reset_probability = None;
  all_reachable = None;
  transitions_per_location = None;
  cycles = None;
  controllability_ratio = None;
  initial_constraint_satisfiable = None;
}



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


let parse_list (s : string) (elem_parser : string -> 'a) : 'a list =
  if not (String.get s 0 = '[' && String.get s (String.length s - 1) = ']') then  
    raise (Parse_error ("invalid list - must start with `[` and end with `]`"))
  else
  let inner = String.sub s 1 (String.length s - 2) in 
  let items = String.split_on_char ',' inner in
  List.map (fun item -> elem_parser (String.trim item)) items

let parse_float s =
  try float_of_string (String.trim s)
  with Failure _ -> raise (Parse_error ("invalid float: " ^ s))

let parse_bool s =
  match String.trim s with
  | "true"  -> true
  | "false" -> false
  | _ -> raise (Parse_error ("invalid bool: " ^ s))



let split_on_first (c : char) (s : string) : (string * string) option =
  match String.index_opt s c with
  | None -> None
  | Some i ->
      let left = String.sub s 0 i in
      let right = String.sub s (i + 1) (String.length s - i - 1) in
      Some (left, right)


let parse_line (acc : partial) (line : string) : partial =
  let line = String.trim line in
  if line = "" || line.[0] = '#' then acc
  else
    match split_on_first '=' line with
    | Some (key, value) ->
        let key = String.trim key in
        let value = String.trim value in
        begin match key with
        | "nb_automata" ->
            { acc with nb_automata = Some (parse_dist value) }

        | "nb_locations" ->
            { acc with nb_locations = Some (parse_dist value) }

        | "nb_clocks" ->
            { acc with nb_clocks = Some (parse_dist value) }

        | "nb_parameters" ->
            { acc with nb_parameters = Some (parse_dist value) }

        | "guard_types" ->
            { acc with
              guard_types =
                Some (parse_list value parse_constraint_type)
            }

        | "invariant_types" ->
            { acc with
              invariant_types =
                Some (parse_list value parse_constraint_type)
            }

        | "guard_probability" ->
            { acc with guard_probability = Some (parse_float value) }

        | "invariant_probability" ->
            { acc with invariant_probability = Some (parse_float value) }

        | "reset_probability" ->
            { acc with reset_probability = Some (parse_float value) }

        | "all_reachable" ->
            { acc with all_reachable = Some (parse_bool value) }

        | "transitions_per_location" ->
            { acc with
              transitions_per_location = Some (parse_dist value)
            }
        | "cycles" ->
            { acc with cycles = Some (parse_bool value) }
        | "controllability_ratio" ->
            { acc with controllability_ratio = Some (parse_float value) }
        | "initial_constraint_satisfiable" ->
            { acc with initial_constraint_satisfiable = Some (parse_bool value) }
        | _ ->
            raise (Parse_error ("unknown key: " ^ key))
        end

    | None ->
        raise (Parse_error ("invalid line: " ^ line))


let require name = function
  | Some v -> v
  | None -> raise (Parse_error (Printf.sprintf "required field `%s` not provided" name))

let optional ~default = Option.value ~default

let finalize (p : partial) : Spec.t =
  {
    nb_automata =
      optional p.nb_automata ~default:(Exact 1);

    nb_locations =
      require "nb_locations" p.nb_locations;

    nb_clocks =
      require "nb_clocks" p.nb_clocks;

    nb_parameters =
      require "nb_parameters" p.nb_parameters;

    guard_types =
      optional p.guard_types ~default:all_constraint_types;

    invariant_types =
      optional p.invariant_types ~default:all_constraint_types;

    guard_probability =
      optional p.guard_probability ~default:0.5;

    invariant_probability =
      optional p.invariant_probability ~default:0.5;

    reset_probability =
      optional p.reset_probability ~default:0.2;

    all_reachable =
      optional p.all_reachable ~default:true;

    transitions_per_location =
      optional p.transitions_per_location ~default:(Exact 1);

    cycles =
      optional p.cycles ~default:true;

    controllability_ratio =
      optional p.controllability_ratio ~default:0.0;

    initial_constraint_satisfiable =
      optional p.initial_constraint_satisfiable ~default:true
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