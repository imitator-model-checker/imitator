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

    val mutable dynamic_location_counter = 0


    method get_model =
      model


    (* ---------------------------------------------------------- *)
    (* Test: add a location whenever the file changes            *)
    (* ---------------------------------------------------------- *)

    method private add_test_location =
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
        "All dynamic location tests passed.\n%!";
        
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

        if self#file_changed then begin

          (* Temporary test *)
          (* self#add_test_location; *)
          self#run_location_tests;

          Updated
        end

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

