type t = {
  validator_file : string;
  output_folder_path : string;
  time_limit: float option;
  repetitions: int
}

val parse : unit -> t