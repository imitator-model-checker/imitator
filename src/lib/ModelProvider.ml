(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Handling dynamic abstract model
 *
 * File contributors : Ta Quang Minh
 * Created           : 2026/08/22
 *
 ************************************************************)
type update_event =
  | Updated
  | Finished


class model_provider
    (model : AbstractModel.abstract_model)
    (filename : string) =
  object (self)

    val mutable last_modified =
      (Unix.stat filename).Unix.st_mtime


    method get_model =
      model


    method private read_key () =
      let old_settings = Unix.tcgetattr Unix.stdin in

      let new_settings = {
        old_settings with
        Unix.c_icanon = false;
        Unix.c_echo = false;
        Unix.c_vmin = 1;
        Unix.c_vtime = 0;
      } in

      Unix.tcsetattr Unix.stdin Unix.TCSANOW new_settings;

      let c = input_char stdin in

      Unix.tcsetattr Unix.stdin Unix.TCSANOW old_settings;

      c


    method private file_changed =
      let current_modified =
        (Unix.stat filename).Unix.st_mtime
      in

      if current_modified > last_modified then begin
        last_modified <- current_modified;
        true
      end
      else
        false


    method wait_for_update =

      let rec loop () =

        (* Check whether the file has changed *)
        if self#file_changed then
          Updated

        (* Check whether user pressed a key *)
        else begin

          let (ready, _, _) =
            Unix.select [Unix.stdin] [] [] 0.2
          in

          match ready with
          | [] ->
              loop ()

          | _ ->
              match self#read_key () with

              | 'q' ->
                  Finished

              | _ ->
                  loop ()
        end

      in

      loop ()

  end
;;