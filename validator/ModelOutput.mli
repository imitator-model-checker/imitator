open Lib

val output_model : ?draw:bool -> file_name:string -> output_folder:string -> Options.imitator_options -> AbstractModel.abstract_model -> unit

(* Create a folder and its parents if they do not exist *)
val create_folder : string -> unit
