(************************************************************
 *
 *                       IMITATOR
 *
 * Université Paris 13, LIPN, CNRS, France
 *
 * Module description: Dynamic elimination of clocks not used in the future [Andre13, FSFMA]
 *
 * File contributors : Étienne André, Jaime Arias
 * Created           : 2015/11/27
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open Statistics
open AbstractModel





(**************************************************************)
(* Global variable *)
(**************************************************************)
let useless_clocks = ref None (* not yet initialized *)


(**************************************************************)
(* Statistics *)
(**************************************************************)
let counter_preparation = ref None
let counter_elimination = ref None


(**************************************************************)
(* Functions *)
(**************************************************************)

let get_clocks_in_guard clocks : guard -> Automaton.clock_index list = function
  | True_guard -> []
  | False_guard -> []
  | Discrete_guard _ -> []
  | Continuous_guard continuous_guard -> LinearConstraint.pxd_find_variables clocks continuous_guard
  | Discrete_continuous_guard discrete_continuous_guard -> LinearConstraint.pxd_find_variables clocks discrete_continuous_guard.continuous_guard

let is_constrained_in_guard clock_index : guard -> bool = function
  | True_guard -> false
  | False_guard -> false
  | Discrete_guard _ -> false
  | Continuous_guard continuous_guard -> LinearConstraint.pxd_is_constrained continuous_guard clock_index
  | Discrete_continuous_guard discrete_continuous_guard -> LinearConstraint.pxd_is_constrained discrete_continuous_guard.continuous_guard clock_index

let clocks_in_update update =
    let clock_updates, _ = update in
    match clock_updates with
    (* No update at all *)
    | No_update -> []
    (* Reset to 0 only *)
    | Resets clock_reset_list -> clock_reset_list
    (* Reset to arbitrary value (including discrete, parameters and clocks) *)
    | Updates clock_update_list ->
      let result, _ = List.split clock_update_list in result

(*------------------------------------------------------------*)
(* Find the local clocks per automaton *)
(*------------------------------------------------------------*)
(*** WARNING: the use of clock_offset is not beautiful (and error prone) here ***)
let find_local_clocks () =
  (* Retrieve the model *)
  let model = Input.get_model() in

  (*** HACK: yes, clock_offset is the number of parameters, but quite hard coded ***)
  let clock_offset = model.nb_parameters in

  (* Create an empty array for the clocks of each automaton *)
  let clocks_per_automaton = Array.make model.nb_automata [] in

  (* Create an empty array for the local clocks of each automaton *)
  let local_clocks_per_automaton = Array.make model.nb_automata [] in

  (* Create an empty array for the automata associated with each clock *)
  let automata_per_clock = Array.make model.nb_clocks [] in

  (* For each automaton *)
  for automaton_index = 0 to model.nb_automata - 1 do
    (* Get the locations for this automaton *)
    let locations = model.locations_per_automaton automaton_index in

    (* For each location *)
    let clocks_for_locations = List.fold_left (fun list_of_clocks_for_previous_locations location_index ->
        (* Get the clocks in the invariant *)
        let invariant = model.invariants automaton_index location_index in
        let clocks_in_invariant = get_clocks_in_guard model.clocks invariant in
        (* Get the clocks from the stopwatches *)
        let clocks_in_stopwatches = model.stopwatches automaton_index location_index in

        (* Now find clocks in guards *)
        (* For each action for this automaton and location *)
        let actions_for_this_location = model.actions_per_location automaton_index location_index in
        let clocks_for_actions = List.fold_left (fun list_of_clocks_for_previous_actions action_index ->
            (* For each transition for this automaton, location and action *)
            let transitions_for_this_action = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in

            let clocks_for_transitions = List.fold_left (fun list_of_clocks_for_previous_transitions transition ->
                (* Name the elements in the transition *)
                let guard , updates = transition.guard, transition.updates in

                let clocks_in_guards = get_clocks_in_guard model.clocks guard in
                let clocks_in_updates = clocks_in_update updates in

                (* Add these 2 new lists to the current list *)
                List.rev_append (List.rev_append clocks_in_guards clocks_in_updates) list_of_clocks_for_previous_transitions
              ) [] transitions_for_this_action in

            (* Add the list for this action to the one for previous actions *)
            List.rev_append clocks_for_transitions list_of_clocks_for_previous_actions
          ) [] actions_for_this_location in

        (* Add all clocks *)
        List.rev_append (List.rev_append (List.rev_append clocks_in_invariant clocks_in_stopwatches) clocks_for_actions) list_of_clocks_for_previous_locations
      ) [] locations in

    (* Collapse the list *)
    let clocks_for_this_automaton = list_only_once clocks_for_locations in
    (* Update the clocks per automaton *)
    clocks_per_automaton.(automaton_index) <- clocks_for_this_automaton;
    (* Update the automaton for all clocks *)
    List.iter (fun clock ->
        (* Add current automaton to the list of automata for this clock *)
        automata_per_clock.(clock - clock_offset) <- (automaton_index :: automata_per_clock.(clock - clock_offset));
      ) clocks_for_this_automaton;
  done; (* end for each automaton *)

  (* Now compute the local clocks *)
  for clock_index = clock_offset to clock_offset + model.nb_clocks - 1 do
    (* Retrieve the automata in which this clock appears *)
    let automata_for_this_clock = automata_per_clock.(clock_index - clock_offset) in
    (* If size is 1, the clock is local *)
    match automata_for_this_clock with
    (* Only one element: clock is local *)
    | [automaton_index] ->
      (* 				print_message Verbose_high ("Automaton " ^ (string_of_int automaton_index) ^ " has local clock " ^ (string_of_int clock_index)); *)
      (* Add the clock to the automaton *)
      local_clocks_per_automaton.(automaton_index) <- (clock_index) :: local_clocks_per_automaton.(automaton_index);
      (* Otherwise, clock is not local *)
    | _ -> ()
  done;

  local_clocks_per_automaton


(*------------------------------------------------------------*)
(* Find the useless clocks in automata locations *)
(*------------------------------------------------------------*)
(** NOTE: this function is not related to model conversion, and could (should?) be defined elsewhere *)
let find_useless_clocks_in_automata local_clocks_per_automaton =

  (* Retrieve the model *)
  let model = Input.get_model() in

  (* Create the data structure *)
  let useless_clocks_per_location = Array.make model.nb_automata (Array.make 0 []) in

  (* For each automaton *)
  for automaton_index = 0 to model.nb_automata - 1 do


    (* Get the locations for this automaton *)
    let locations_for_this_automaton = model.locations_per_automaton automaton_index in
    let nb_locations = List.length locations_for_this_automaton in

    (* Initialize the data structure for this automaton *)
    useless_clocks_per_location.(automaton_index) <- Array.make nb_locations [];

    (* Retrieve the local clocks for this automaton *)
    let local_clocks = local_clocks_per_automaton.(automaton_index) in

    (* Compute the predecessor locations and lists of local clock reset *)
    let predecessors = Array.make nb_locations [] in
    (* For each location in this automaton: *)
    List.iter (fun location_index ->
        (* Get the actions for this location *)
        let actions_for_this_location = model.actions_per_location automaton_index location_index in

        (* For each action available in this location *)
        List.iter (fun action_index ->
            (* Retrieve the transitions from this location & action *)
            let transitions = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in

            (* For each transition starting from this location *)
            (** TODO: What happens here with the clock updates ?? *)
            List.iter (fun transition ->

                (* Get the clocks updated or reset *)
                let reset_clocks = clocks_in_update transition.updates in

                (* Compute the local clocks updated or reset *)
                let reset_local_clocks = list_inter reset_clocks local_clocks in
                (* Update the predecessors *)
                predecessors.(transition.target) <- (location_index, reset_local_clocks) :: predecessors.(transition.target);
              ) transitions; (* end for each transition *)
          ) actions_for_this_location; (* end for each action *)
      ) locations_for_this_automaton; (* end for each location *)

    (* Print some information *)
    if verbose_mode_greater Verbose_total then(
      print_message Verbose_total ("Computed predecessor locations and clock resets for automaton '" ^ (model.automata_names automaton_index) ^ "'");
      (* Iterate on locations *)
      List.iter (fun location_index ->
          print_message Verbose_total ("  Location '" ^ (model.location_names automaton_index location_index) ^ "' has predecessors:");
          let predecessors_string = string_of_list_of_string_with_sep ", " (List.map (
              fun (source_index, reset_local_clocks) ->
                (model.location_names automaton_index source_index) ^ "[resets: " ^ (string_of_list_of_string_with_sep ", " (List.map model.variable_names reset_local_clocks)) ^ "]"
            ) predecessors.(location_index)) in
          print_message Verbose_total ("    " ^ predecessors_string);
        ) locations_for_this_automaton; (* end for each location *)
    );

    (* For each local clock for this automaton *)
    List.iter(fun clock_index ->
        (* Create a list of marked locations (i.e., where the clock is useful) *)
        let marked = ref (list_union
                            (* All locations with an invariant involving this clock *)
                            (List.filter (fun location_index ->
                                 (* Retrieve the invariant *)
                                 let invariant = model.invariants automaton_index location_index in
                                 (* Check if the clock is present in the invariant *)
                                 let constrained = is_constrained_in_guard clock_index invariant in
                                 (* Print some information *)
                                 print_message Verbose_total ("Clock '" ^ (model.variable_names clock_index) ^ "' is " ^ (if constrained then "" else "NOT ") ^ "constrained in invariant of location '" ^ (model.location_names automaton_index location_index) ^ "'");
                                 (* Return true or false *)
                                 constrained
                               ) locations_for_this_automaton
                            )
                            (* All predecessor locations of transitions with a guard involving this clock *)
                            (
                              (* For each location *)
                              List.fold_left (fun current_list_of_locations location_index ->
                                  (* Get the actions for this location *)
                                  let actions_for_this_location = model.actions_per_location automaton_index location_index in
                                  (* For each action available in this location *)
                                  List.fold_left (fun current_list_of_locations action_index ->
                                      (* Retrieve the transitions from this location & action *)
                                      let transitions = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in
                                      (* Check if there exists a guard in an outgoing transition where the clock is constrained *)
                                      let exists_guard = List.exists (fun transition ->
                                          (* Check if the clock is present in the guard *)
                                          let constrained = is_constrained_in_guard clock_index transition.guard in
                                          (* Print some information *)
                                          if constrained then (
                                            print_message Verbose_high ("Found a transition where clock '" ^ (model.variable_names clock_index) ^ "' is constrained in guard from location '" ^ (model.location_names automaton_index location_index) ^ "', through '" ^ (model.action_names action_index) ^ "'");
                                          ) else (
                                            print_message Verbose_total ("Clock '" ^ (model.variable_names clock_index) ^ "' is not constrained in guard from location '" ^ (model.location_names automaton_index location_index) ^ "' through '" ^ (model.action_names action_index) ^ "'");
                                          );
                                          (* Return true or false *)
                                          constrained
                                        ) transitions in
                                      (* Keep the location if there exists a guard *)
                                      if exists_guard then location_index :: current_list_of_locations
                                      else current_list_of_locations
                                    ) current_list_of_locations actions_for_this_location
                                ) [] locations_for_this_automaton
                            )
                         ) in

        (* Create a waiting list *)
        let waiting = ref !marked in

        (* Print some information *)
        if verbose_mode_greater Verbose_medium then(
          print_message Verbose_medium ("Starting the dynamic clock elimination algorithm for local clock '" ^ (model.variable_names clock_index) ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "', with initial marked states:");
          print_message Verbose_medium (	"  " ^ (string_of_list_of_string_with_sep ", " (List.map (model.location_names automaton_index) !marked)));
        );

        (* Start the algorithm *)
        while !waiting <> [] do
          (* Pick a location from the waiting list *)
          match !waiting with
          | location_index :: rest ->
            (* Print some information *)
            print_message Verbose_medium ("Pick up location '" ^ (model.location_names automaton_index location_index) ^ "'");
            (* Remove the first element *)
            waiting := rest;
            (* For each transition leading to this location *)
            List.iter (fun (source_index, reset_local_clocks) ->
                (* Print some information *)
                print_message Verbose_high ("Considering predecessor transition from '" ^ (model.location_names automaton_index source_index) ^ "'");
                (* If the clock is not reset by the transition *)
                if not (List.mem clock_index reset_local_clocks) then(
                  (* Print some information *)
                  print_message Verbose_high ("Clock not reset by a transition.");
                  (* If the source location does not belong to the marked list *)
                  if not (List.mem source_index !marked) then(
                    (* Add it to the marked list *)
                    marked := source_index :: !marked;
                    print_message Verbose_high ("Location marked.");
                    (* Add it to the waiting list (if not present) *)
                    if not (List.mem source_index !waiting) then
                      print_message Verbose_high ("Location added to waiting list.");
                    waiting := source_index :: !waiting;
                  ); (* end if not in marked list *)
                );(* end if clock not reset *)
              ) predecessors.(location_index); (* end for each transition *)

          | _ -> raise (InternalError "Impossible situation: list should not be empty.");

            (* End the algorithm *)
        done;

        (* Return the list of locations where the clock can be removed *)
        let useless_locations = list_diff locations_for_this_automaton !marked in

        (* Print some information *)
        if verbose_mode_greater Verbose_low then(
          print_message Verbose_low ("List of useless locations for local clock '" ^ (model.variable_names clock_index) ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'");
          print_message Verbose_low ("  " ^ (string_of_list_of_string_with_sep ", " (List.map (model.location_names automaton_index) useless_locations)));
        );

        (* Update the data structure *)
        List.iter (fun location_index ->
            (useless_clocks_per_location.(automaton_index)).(location_index) <- clock_index :: (useless_clocks_per_location.(automaton_index)).(location_index);
          ) useless_locations;


      ) local_clocks; (* end for each local clock *)
  done; (* end for each automaton *)

  (* Return a functional structure *)
  (fun automaton_index location_index ->
     (useless_clocks_per_location.(automaton_index)).(location_index)
  )



(*------------------------------------------------------------*)
(* Function for preparing data structures for dynamic clock elimination *)
(*------------------------------------------------------------*)
(* NOTE: This function is only called if the dynamic clock elimination option is activated *)
let prepare_clocks_elimination () =
  (* Create counters *)
  let counter_preparation_ref = create_time_counter_and_register "dynamic clock elimination (preparation)" Algorithm_counter Verbose_standard in
  counter_preparation := Some counter_preparation_ref;
  counter_elimination := Some (create_hybrid_counter_and_register "dynamic clock elimination" Algorithm_counter Verbose_standard);

  (* Start counter *)
  counter_preparation_ref#start;

  (* Retrieve the model *)
  let model = Input.get_model() in

  (* Compute the local clocks per automaton *)
  print_message Verbose_low ("*** Building local clocks per automaton…");
  let local_clocks_per_automaton = find_local_clocks () in

  (* Debug print: local clocks per automaton *)
  if verbose_mode_greater Verbose_total then(
    print_message Verbose_total ("\n*** Local clocks per automaton:");
    (* For each automaton *)
    List.iter (fun automaton_index ->
        (* Get the actions *)
        let clocks = local_clocks_per_automaton.(automaton_index) in
        (* Print it *)
        let clocks_string = string_of_list_of_string_with_sep ", " (List.map model.variable_names clocks) in
        print_message Verbose_total ("  " ^ (model.automata_names automaton_index) ^ " : " ^ clocks_string)
      ) model.automata;
  );


  (* Compute and update useless clocks *)
  print_message Verbose_low ("*** Building useless clocks per location per automaton…");
  useless_clocks := Some (find_useless_clocks_in_automata local_clocks_per_automaton);

  (* Stop counter *)
  counter_preparation_ref#stop;

  (* The end *)
  ()


(*------------------------------------------------------------*)
(* Eliminating useless clocks in a linear constraint *)
(*------------------------------------------------------------*)
let dynamic_clock_elimination target_location current_constraint =
  (* Get counter *)
  let counter_elimination = match !counter_elimination with
    | Some counter_elimination -> counter_elimination
    | None -> raise (InternalError("Counter counter_elimination not yet initialized in ClockElimination.dynamic_clock_elimination"))
  in

  (* Start counter *)
  counter_elimination#start;
  (* Increment counter *)
  counter_elimination#increment;

  (* Retrieve the model *)
  let model = Input.get_model() in

  (* First check that initialization was performed *)
  let compute_clocks_to_eliminate =
    match !useless_clocks with
    | None -> raise (InternalError ("Trying to perform dynamic clock elimination before initializing the clocks to eliminate."))
    | Some f -> f
  in

  (* Compute the useless clocks *)
  let clocks_to_remove = List.fold_left (fun current_list_of_clocks automaton_index ->
      (* Retrieve target location for this automaton *)
      let location_index = DiscreteState.get_location target_location automaton_index in
      (* Get the clocks and append to previously computed clocks (rev_append because the order doesn't matter) *)
      List.rev_append current_list_of_clocks (compute_clocks_to_eliminate automaton_index location_index)
    ) [] model.automata in
  (* Print some information *)
  if verbose_mode_greater Verbose_low then(
    if clocks_to_remove = [] then print_message Verbose_low ("No clock will be dynamically removed.")
    else print_message Verbose_low ("The following clock" ^ (s_of_int (List.length clocks_to_remove)) ^ " will be dynamically removed: {" ^ (string_of_list_of_string_with_sep ", " (List.map model.variable_names clocks_to_remove)) ^ "}");
  );

  print_message Verbose_high ("\nRemoving useless clocks ");
  LinearConstraint.px_hide_assign clocks_to_remove current_constraint;
  (* Print some information *)
  if verbose_mode_greater Verbose_total then(
    print_message Verbose_total (LinearConstraint.string_of_px_linear_constraint model.variable_names current_constraint);
  );

  (* Stop counter *)
  counter_elimination#stop;

  (* The end *)
  ()
