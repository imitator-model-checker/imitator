(** Command-line options for the validator. *)

type mode =
  | SampleModelGenerator of {draw_pdf:bool}
  | CompareOutput of { config_file_a : string; config_file_b : string }
  | Reduce of { model_file : string; config_file_a : string; config_file_b : string }

type t = {
  mode : mode;
  output_folder_path : string;
}

let default_output = "validator-output"

(* Mutable holders populated during parsing *)
let mode_ref : mode option ref = ref None
let output_ref = ref default_output

let usage_msg =
  "Usage: validator [options]\n\
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

let speclist : (string * Arg.spec * string) list =
  [
    ( "-mode",
      Arg.String set_mode,
      "Select mode: \"sample-pdf\", \"sample-nopdf\", \"compare:<config_file_a>:<config_file_b>\" or \"reduce:<model_file>:<config_file_a>:<config_file_b>\"." );
    ( "-output",
      Arg.String set_output,
      Printf.sprintf "Output folder path (default: %s)" default_output );
  ]

let anon_fun (_ : string) = () (* no positional args right now *)


let fail_with_usage msg =
  prerr_endline ("Error: " ^ msg);
  Arg.usage speclist usage_msg;
  exit 1

let parse (args: string array) : t =
  Arg.parse_argv args speclist anon_fun usage_msg;
  let mode =
    match !mode_ref with
    | Some m -> m
    | None ->
        fail_with_usage "-mode is required. Expected \"sample-pdf\", \"sample-nopdf\", \"compare:<config_file_a>:<config_file_b>\" or \"reduce:<model_file>:<config_file_a>:<config_file_b>\"."
  in
  { mode; output_folder_path = !output_ref }

let arg_list : string list =
  List.map (fun (opt, _, _) -> opt) speclist