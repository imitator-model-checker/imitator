open Spec
open ParsingUtil

module SpecParser = struct 
  type partial = {
    nb_automata   : dist option;
    nb_locations  : dist option;
    nb_clocks     : dist option;
    nb_parameters : dist option;
    guard_probability : float option;
    invariant_probability : float option;
    reset_probability : float option;
    all_reachable : bool option;
    transitions_per_location : dist option;
    cycles : bool option;
    controllability_ratio : float option;
    initial_constraint_satisfiable : bool option;
    max_constant : int option;
  }

  let empty : partial = {
    nb_automata   = None;
    nb_locations  = None;
    nb_clocks     = None;
    nb_parameters = None;
    guard_probability = None;
    invariant_probability = None;
    reset_probability = None;
    all_reachable = None;
    transitions_per_location = None;
    cycles = None;
    controllability_ratio = None;
    initial_constraint_satisfiable = None;
    max_constant = None;
  }

  let parse_kv (acc : partial) ~key ~value : partial =
    match key with
    | "nb_automata" ->
        { acc with nb_automata = Some (parse_dist value) }

    | "nb_locations" ->
        { acc with nb_locations = Some (parse_dist value) }

    | "nb_clocks" ->
        { acc with nb_clocks = Some (parse_dist value) }

    | "nb_parameters" ->
        { acc with nb_parameters = Some (parse_dist value) }

    | "max_constant" ->
        { acc with max_constant = Some (parse_int value) }

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
    max_constant =
      optional p.max_constant ~default:10;

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
      optional p.initial_constraint_satisfiable ~default:true;
  }

end

module ModeParser = struct
  type kind = SampleGenerator | Reduce | Compare | PropertyGuided

  type partial = {
    kind : kind option;
    pdf : bool option;
    samples : int option;

    configs : string list option;
    properties : Mode.property list;
    model_path : string option;
  }

  let empty : partial =
    { kind=None; pdf=None; samples=None; configs=None; properties=[]; model_path=None }


  let parse_property (s : string) : Mode.property =
    let s = String.trim s in
    let negated = String.length s >= 2 && String.sub s 0 2 = "! " in
    let property = if negated then String.sub s 2 (String.length s - 2) else s in
    { property; negated }

  let parse_kind (s : string) : kind =
    match String.trim s |> String.lowercase_ascii with
    | "sample" -> SampleGenerator
    | "reduce" -> Reduce
    | "compare" -> Compare
    | "property-guided" -> PropertyGuided
    | _ -> raise (Parse_error ("invalid mode kind: " ^ s))

  let parse_kv (acc : partial) ~key ~value : partial =
    match key with
    | "kind" ->
        { acc with kind = Some (parse_kind value) }

    | "pdf" ->
        { acc with pdf = Some (parse_bool value) }

    | "samples" ->
        { acc with samples = Some (parse_int value) }

    | "model_path" | "model" ->
        { acc with model_path = Some (String.trim value) }

    | "configs" ->
        { acc with configs = Some (parse_string_list_semicolon value) }

    | "properties" ->
        { acc with properties =  List.map parse_property (parse_string_list_semicolon value) }
    | _ ->
        raise (Parse_error ("unknown mode key: " ^ key))


  let finalize (p : partial) : Mode.t =
    match optional p.kind ~default:SampleGenerator with
    | SampleGenerator ->
        Mode.SampleGenerator {
          pdf = optional p.pdf ~default:false;
          samples = optional p.samples ~default:1;
        }

    | Reduce ->
        let configs = require "configs" p.configs in 
        Mode.Reduce {
          configs = argv_list_of_configs configs;
          model_path = require "model_path" p.model_path;
        }

    | Compare ->
        let configs = require "configs" p.configs in 
        Mode.Compare {
          configs = argv_list_of_configs configs;
        }
    | PropertyGuided ->
        Mode.PropertyGuided p.properties
end

let read_logical_line ic (first : string) =
  let buf = Buffer.create 128 in
  Buffer.add_string buf first;
  let rec loop () =
    let cur = Buffer.contents buf in
    if brackets_balanced cur then cur
    else begin
      match input_line ic with
      | line ->
          Buffer.add_char buf '\n';
          Buffer.add_string buf line;
          loop ()
      | exception End_of_file ->
          raise (Parse_error "unterminated multi-line value (missing ']')")
    end
  in
  loop ()



type section = Model | Mode

let parse_section (line : string) : section option =
  let line = String.trim line in
  if String.length line >= 3 && line.[0] = '[' && line.[String.length line - 1] = ']' then
    let name = String.sub line 1 (String.length line - 2) |> String.trim |> String.lowercase_ascii in
    match name with
    | "model" -> Some Model
    | "mode"  -> Some Mode
    | _ -> raise (Parse_error ("unknown section: [" ^ name ^ "]"))
  else
    None


type state = {
  section : section option;
  spec_p : SpecParser.partial;
  mode_p  : ModeParser.partial;
}

let empty_state : state =
  { section = None; spec_p = SpecParser.empty; mode_p = ModeParser.empty }


let parse_line (st : state) (line : string) : state =
  if line = "" || (String.length line > 0 && line.[0] = '#') then st
  else
  match parse_section line with
  | Some sec -> { st with section = Some sec }
  | None ->
      let sec = Option.value st.section ~default:Model in
      match split_on_first '=' line with
      | None ->
          raise (Parse_error ("invalid line (expected KEY = VALUE): " ^ line))
      | Some (key, value) ->
          let key = String.trim key in
          let value = String.trim value in
          begin match sec with
          | Model ->
              { st with spec_p = SpecParser.parse_kv st.spec_p ~key ~value }
          | Mode ->
              { st with mode_p = ModeParser.parse_kv st.mode_p ~key ~value }
          end


let parse_lines (lines : string list) : Config.t =
  let st = List.fold_left parse_line empty_state lines in
  {
    spec = SpecParser.finalize st.spec_p;
    mode  = ModeParser.finalize st.mode_p;
  }


let parse_file filename : Config.t =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> 
      let logical_line = read_logical_line ic line in 
      loop (logical_line :: acc)
    | exception End_of_file ->
        close_in ic;
        parse_lines (List.rev acc)
  in
  loop []