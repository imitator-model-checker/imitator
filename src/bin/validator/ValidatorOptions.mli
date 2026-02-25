type t = {
  validator_file : string;
  output_folder_path : string;
  time_limit: float option;
  repetitions: int;
  seed : int option;
  results_file : string option;
}

val parse : unit -> t