open Lib

let output_model ?(draw=false) ~file_name ~output_folder options model  = 
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