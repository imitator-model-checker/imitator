
type mode =
  | SampleModelGenerator of { draw_pdf : bool }
  | CompareOutput of { config_file_a : string; config_file_b : string }
  | Reduce of { model_file : string; config_file_a : string; config_file_b : string }

type t = {
  validator_file : string;          (* positional *)
  mode : mode;                      (* required *)
  output_folder_path : string;      (* optional *)
  time_limit : float option;        (* optional *)
}

let default_output = "validator-output"

let fail_without_usage (msg : string) : 'a =
  prerr_endline ("Error: " ^ msg);
  exit 1

let parse_mode (s : string) : mode =
  match String.split_on_char ':' s with
  | ["sample-pdf"] -> SampleModelGenerator { draw_pdf = true }
  | ["sample-nopdf"] -> SampleModelGenerator { draw_pdf = false }
  | ["compare"; config_file_a; config_file_b] ->
      CompareOutput { config_file_a; config_file_b }
  | ["reduce"; model_file; config_file_a; config_file_b] ->
      Reduce { model_file; config_file_a; config_file_b }
  | _ ->
      fail_without_usage
        ("invalid -mode argument \"" ^ s
         ^ "\". Expected:\n"
         ^ "  sample-pdf\n"
         ^ "  sample-nopdf\n"
         ^ "  compare:<config_file_a>:<config_file_b>\n"
         ^ "  reduce:<model_file>:<config_file_a>:<config_file_b>")

let parse () : t =
  let validator_file_ref : string option ref = ref None in
  let mode_ref : mode option ref = ref None in
  let output_ref : string ref = ref default_output in
  let time_limit_ref : float option ref = ref None in

  let usage_msg =
    "Usage: validator <validator_file> -mode <...> [options]\n\
     \n\
     <validator_file> is positional.\n\
     Options are:"
  in

  let speclist : (string * Arg.spec * string) list =
    [
      ( "-mode",
        Arg.String (fun s -> mode_ref := Some (parse_mode s)),
        "Select mode: sample-pdf | sample-nopdf | \
         compare:<a>:<b> | reduce:<model>:<a>:<b>" );
      ( "-output",
        Arg.String (fun s -> output_ref := s),
        Printf.sprintf "Output folder path (default: %s)" default_output );
      ( "-time-limit",
        Arg.Float (fun t -> time_limit_ref := Some t),
        "Time limit (seconds) for each internal imitator run. Supports decimals." );
    ]
  in

  let anon_fun (s : string) =
    match !validator_file_ref with
    | None -> validator_file_ref := Some s
    | Some _ -> fail_without_usage ("unexpected extra positional argument: " ^ s)
  in

  let fail_with_usage msg =
    prerr_endline ("Error: " ^ msg);
    Arg.usage speclist usage_msg;
    exit 1
  in

  Arg.parse speclist anon_fun usage_msg;

  let validator_file =
    match !validator_file_ref with
    | Some f -> f
    | None -> fail_with_usage "Missing <validator_file> positional argument."
  in
  let mode =
    match !mode_ref with
    | Some m -> m
    | None ->
        fail_with_usage
          "-mode is required. Expected: sample-pdf | sample-nopdf | compare:<a>:<b> | reduce:<model>:<a>:<b>"
  in

  { validator_file; mode; output_folder_path = !output_ref; time_limit = !time_limit_ref }