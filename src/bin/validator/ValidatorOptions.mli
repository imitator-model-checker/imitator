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

val parse : string array -> t

val arg_list : string list