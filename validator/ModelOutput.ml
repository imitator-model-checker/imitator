open Lib

(* ModelConverter stores the transitions of each (automaton, location, action) in reverse order of declaration.
   Restore the declaration order so that the saved model, once parsed again, is the model that was tested. *)
let with_declared_transition_order (model : AbstractModel.abstract_model) : AbstractModel.abstract_model =
  { model with transitions = fun automaton location action -> List.rev (model.transitions automaton location action) }

let rec create_folder path =
  if not (Sys.file_exists path) then begin
    create_folder (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let output_model ?(draw=false) ~file_name ~output_folder options model  =
  let model = with_declared_transition_order model in
  create_folder output_folder;
  let path = Printf.sprintf "%s/%s" output_folder file_name in
  let imi_file_name = path ^ ".imi" in
  let imi_file = open_out imi_file_name in
  output_string imi_file @@ ModelPrinter.string_of_model model;
  close_out imi_file;
  if draw then
    let translated_model = PTA2dot.string_of_model options model in
    let dot_created_file_option = Graphics.dot "pdf" path translated_model in
    begin
    match dot_created_file_option with
    | None -> print_endline "Oops…! Something went wrong with dot."
    | Some _ -> ()
    end
