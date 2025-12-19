(** Command-line options for the validator. *)

type mode =
  | SampleModelGenerator of {draw_pdf:bool}
  | CompareOutput of { config_file_a : string; config_file_b : string }
  | Reduce of { model_file : string; config_file_a : string; config_file_b : string }

type t = {
  validator_file : string;
  mode : mode;
  output_folder_path : string;
  time_limit: float option;
}

let default_output = "validator-output"

(* Mutable holders populated during parsing *)
let mode_ref : mode option ref = ref None
let output_ref = ref default_output

let time_limit_ref = ref None 

let validator_file_ref : string option ref = ref None

let usage_msg =
  "Usage: validator -file <validator_file> [options]\n\
   Options are:"

let fail_without_usage msg = 
  prerr_endline ("Error: " ^ msg);
  exit 1

let parse_mode (s : string) : mode =
  match String.split_on_char ':' s with
  | ["sample-pdf"] -> SampleModelGenerator {draw_pdf=true}
  | ["sample-nopdf"] -> SampleModelGenerator {draw_pdf=false}
  | "compare" :: config_file_a :: config_file_b :: [] -> CompareOutput {config_file_a;config_file_b}
  | "reduce" :: model_file :: config_file_a :: config_file_b :: [] -> Reduce {model_file;config_file_a;config_file_b}
  | _ ->
      fail_without_usage
        ("invalid -mode argument \"" ^ s
        ^ "\". Expected \"sample-pdf\", \"sample-nopdf\", \"compare:<config_file_a>:<config_file_b>\" or \"reduce:<model_file>:<config_file_a>:<config_file_b>\".")

let set_mode s = mode_ref := Some (parse_mode s)
let set_output s = output_ref := s

let set_time_limit s = time_limit_ref := Some s

let set_validator_file s = validator_file_ref := Some s

let speclist : (string * Arg.spec * string) list =
  [
    ( "-file",
      Arg.String set_validator_file,
      "Path to the validator specification file.");
    ( "-mode",
      Arg.String set_mode,
      "Select mode: \"sample-pdf\", \"sample-nopdf\", \"compare:<config_file_a>:<config_file_b>\" or \"reduce:<model_file>:<config_file_a>:<config_file_b>\"." );
    ( "-output",
      Arg.String set_output,
      Printf.sprintf "Output folder path (default: %s)" default_output );
    ( "-time-limit",
      Arg.Float set_time_limit,
      Printf.sprintf "Set time limit for each internal imitator run.
      Providing -time-limit t is equivalent to including -time-limit=t each arg config file. Supports decimal values."
      )
  ]

let anon_fun (_ : string) = () (* no positional args right now *)


let fail_with_usage msg =
  prerr_endline ("Error: " ^ msg);
  Arg.usage speclist usage_msg;
  exit 1

let parse (args: string array) : t =
  Arg.parse_argv args speclist anon_fun usage_msg;
  let validator_file = match !validator_file_ref with
    | Some f -> f
    | None -> fail_with_usage "Missing validator specification file."
  in
  let mode =
    match !mode_ref with
    | Some m -> m
    | None ->
        fail_with_usage "-mode is required. Expected \"sample-pdf\", \"sample-nopdf\", \"compare:<config_file_a>:<config_file_b>\" or \"reduce:<model_file>:<config_file_a>:<config_file_b>\"."
  in
  { validator_file; mode; output_folder_path = !output_ref; time_limit = !time_limit_ref}

let arg_list : string list =
  List.map (fun (opt, _, _) -> opt) speclist