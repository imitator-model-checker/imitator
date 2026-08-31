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
  | Updated of string
  | Finished


class model_provider
    (model : AbstractModel.abstract_model)
    (filename : string) =
  object (self)

    (* val mutable last_modified =
      (Unix.stat filename).Unix.st_mtime *)

    val mutable dynamic_location_counter = 0

    (* Number of bytes already consumed from the file. *)
    val mutable file_position =
      let ic = open_in_bin filename in
      let position = Int64.of_int (in_channel_length ic) in
      close_in ic;
      position

    (* We assume the file only grows by appending at the end.
       So we only need to remember how many bytes have already been consumed. *)

    (* inotify watcher *)
    val inotify_fd =
      Inotify.create ()

    initializer
      ignore (
        Inotify.add_watch
          inotify_fd
          filename
          [Inotify.S_Modify]
      )



    method get_model =
      model
        
    method private read_key () =
      input_char stdin

    method private read_new_content =

      let ic = open_in_bin filename in

      try

        seek_in ic (Int64.to_int file_position);

        let file_length =
          Int64.of_int (in_channel_length ic)
        in

        if file_length <= file_position then begin
          close_in ic;
          ""
        end

        else begin

          let length =
            Int64.to_int
              (Int64.sub file_length file_position)
          in

          let content =
            really_input_string ic length
          in

          file_position <- file_length;

          close_in ic;

          content
        end

      with e ->
        close_in_noerr ic;
        raise e
 

  method wait_for_update =

    let old_settings = Unix.tcgetattr Unix.stdin in

    let new_settings = {
      old_settings with
      Unix.c_icanon = false;
      Unix.c_echo = false;
      Unix.c_vmin = 1;
      Unix.c_vtime = 0;
    } in

    Unix.tcsetattr Unix.stdin Unix.TCSANOW new_settings;

    Fun.protect
      ~finally:(fun () ->
        Unix.tcsetattr
          Unix.stdin
          Unix.TCSANOW
          old_settings)
      (fun () ->

        let rec loop () =

          let ready, _, _ =
            Unix.select
              [Unix.stdin; inotify_fd]
              []
              []
              (-1.)
          in

          if List.mem inotify_fd ready then begin

            Printf.printf "INOTIFY READY\n%!";

            ignore (Inotify.read inotify_fd);

            let content = self#read_new_content in

            if content <> "" then begin
              Printf.printf
                "NEW CONTENT:\n%s\n%!"
                content;

              Updated content
            end
            else
              loop ()

          end
          else if List.mem Unix.stdin ready then begin

            match self#read_key () with
            | 'q' -> Finished
            | _ -> loop ()

          end
          else
            loop ()

        in

        loop ()
    )

    (* below are just some quick test *)
    (* ---------------------------------------------------------- *)
    (* Test: add a location whenever the file changes            *)
    (* ---------------------------------------------------------- *)

    (* method private add_test_location =
      let location_name =
        Printf.sprintf
          "dynamic_location_%d"
          dynamic_location_counter
      in

      dynamic_location_counter <-
        dynamic_location_counter + 1;

      let new_location_index =
        model.add_location_onthefly
          0
          location_name
          false
      in

      Printf.printf
        "Added location: %s (index=%d)\n%!"
        location_name
        new_location_index

(* ============================================================ *)
(* Tests for add_location_onthefly                              *)
(* ============================================================ *)

    method private test_add_location =
      let automaton_index = 0 in

      let old_locations =
        model.locations_per_automaton automaton_index
      in

      let old_count =
        List.length old_locations
      in

      let new_index =
        model.add_location_onthefly
          automaton_index
          "TEST_LOCATION"
          false
          false
          False_guard
      in

      let new_locations =
        model.locations_per_automaton automaton_index
      in

      assert (
        List.length new_locations = old_count + 1
      );

      assert (
        model.location_names
          automaton_index
          new_index
        = "TEST_LOCATION"
      );

      assert (
        not (
          model.is_urgent
            automaton_index
            new_index
        )
      );

      assert (
        not (
          model.is_accepting
            automaton_index
            new_index
        )
      );

      Printf.printf
        "Basic location test passed: index=%d\n%!"
        new_index;


    method private test_add_urgent_location =
      let automaton_index = 0 in

      let new_index =
        model.add_location_onthefly
          automaton_index
          "TEST_URGENT_LOCATION"
          true
          false
          False_guard
      in

      assert (
        model.location_names
          automaton_index
          new_index
        = "TEST_URGENT_LOCATION"
      );

      assert (
        model.is_urgent
          automaton_index
          new_index
      );

      assert (
        not (
          model.is_accepting
            automaton_index
            new_index
        )
      );

      Printf.printf
        "Urgent location test passed: index=%d\n%!"
        new_index;


    method private test_location_attributes =
      let automaton_index = 0 in

      let new_index =
        model.add_location_onthefly
          automaton_index
          "TEST_ATTRIBUTES"
          false
          false
          False_guard
      in

      (* Actions *)
      let actions =
        model.actions_per_location
          automaton_index
          new_index
      in

      assert (
        actions = []
      );

      (* Stopwatches *)
      let stopwatches =
        model.stopwatches
          automaton_index
          new_index
      in

      assert (
        stopwatches = []
      );

      (* Flow *)
      let flow =
        model.flow
          automaton_index
          new_index
      in

      assert (
        flow = []
      );

      Printf.printf
        "Location attributes test passed: index=%d\n%!"
        new_index;


    method private test_transition_array =
      let automaton_index = 0 in

      let new_index =
        model.add_location_onthefly
          automaton_index
          "TEST_TRANSITIONS"
          false
          false
          False_guard
      in

      if model.nb_actions > 0 then begin

        let transitions =
          model.transitions
            automaton_index
            new_index
            0
        in

        assert (
          transitions = []
        );

        Printf.printf
          "Transition test passed: index=%d\n%!"
          new_index

      end
      else begin

        Printf.printf
          "Transition test skipped: model has no actions.\n%!"
      end;


    method private test_closure =
      let automaton_index = 0 in

      let before =
        model.locations_per_automaton
          automaton_index
      in

      let new_index =
        model.add_location_onthefly
          automaton_index
          "TEST_CLOSURE"
          true
          false
          False_guard
      in

      let after =
        model.locations_per_automaton
          automaton_index
      in

      assert (
        List.length after
        = List.length before + 1
      );

      assert (
        List.mem new_index after
      );

      assert (
        model.location_names
          automaton_index
          new_index
        = "TEST_CLOSURE"
      );

      assert (
        model.is_urgent
          automaton_index
          new_index
      );

      Printf.printf
        "Closure test passed: index=%d\n%!"
        new_index;


    method private run_location_tests =
      Printf.printf
        "\nRunning dynamic location tests...\n%!";

      self#test_add_location;
      self#test_add_urgent_location;
      self#test_location_attributes;
      self#test_transition_array;
      self#test_closure;

      Printf.printf
        "All dynamic location tests passed.\n%!"; *)
  end
;;

