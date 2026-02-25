type t = {
  validator_file : string;
  output_folder_path : string;
  time_limit : float option;
  repetitions : int;
  seed : int option;
  results_file : string option;
}

let default_output = "validator-output"
let default_repetitions = 1000

let fail_without_usage (msg : string) : 'a =
  prerr_endline ("Error: " ^ msg);
  exit 1


let parse () : t =
  let validator_file_ref : string option ref = ref None in
  let output_ref : string ref = ref default_output in
  let time_limit_ref : float option ref = ref None in
  let repeititons_ref : int option ref = ref None in
  let seed_ref : int option ref = ref None in
  let results_file_ref : string option ref = ref None in

  let usage_msg =
    "Usage: validator <validator_file> [options]\n\
     \n\
     <validator_file> is positional.\n\
     Options are:"
  in

  let speclist : (string * Arg.spec * string) list =
    [
      ( "-output",
        Arg.String (fun s -> output_ref := s),
        Printf.sprintf "Output folder path (default: %s)" default_output );
      ( "-time-limit",
        Arg.Float (fun t -> time_limit_ref := Some t),
        "Time limit (seconds) for each internal imitator run. Supports decimals." );
      ( "-r",
        Arg.Int (fun r -> repeititons_ref := Some r),
        Printf.sprintf "In compare mode: Amount of repetitions (default: %d)" default_repetitions);
      ( "-seed",
        Arg.Int (fun s -> seed_ref := Some s),
        "Random seed (integer) to use for model generation" );
      ( "-results-file",
        Arg.String (fun s -> results_file_ref := Some s),
        "Path to write a JSON results summary after all runs complete" );
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

  let repetitions =
    match !repeititons_ref with
    | Some r -> r
    | None -> 1000
  in

  { validator_file; output_folder_path = !output_ref; time_limit = !time_limit_ref;
    repetitions; seed = !seed_ref; results_file = !results_file_ref }
