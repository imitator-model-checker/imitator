(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: Parametric Timed Automatas Syncronized Product
 *
 * File contributors : Daniel Torres
 * Created           : 2026/09/15
 *
 ************************************************************)

open AbstractModel

let dummy_transition = {
  guard   = True_guard;
  action  = -1;
  updates = No_potential_update, [];
  target  = -1;
}

(*
  Function to compute the digits to concatenate after the location name: [000, 001, 002, ..., 243, 244]
*)
let rec digits base number =
  match base with
  | [] -> []
  | [last] -> [number mod last]
  | _ :: rest ->
    let tr = List.fold_left ( * ) 1 rest in
      (number / tr) :: digits rest (number mod tr)

(*
  Function to get the last numbers in a string name:
  trailing_number "lock2" -> 2
  trailing_number "P1_0" -> 0
  trailing_number "P2_1" -> 1
  trailing_number "P2_10" -> 10
*)
let trailing_number (s : string) : int =
  let l_s = String.length s in
  let i = ref (l_s - 1) in
    while !i >= 0 && s.[!i] >= '0' && s.[!i] <= '9' do
      decr i
    done;
  let start = !i + 1 in
  if start >= l_s then 0
  else int_of_string (String.sub s start (l_s - start))

let pta_sync_prod_locations_number                                  = ref 0
let pta_sync_prod_transitions_number                                = ref 0
let pta_sync_prod_automata_name                                     = ref (Array.make 0 "")
let pta_sync_prod_location_names                                    = ref (Array.make 0 "")
let pta_sync_prod_locations_per_automaton                           = ref (List.init 0 (fun i -> i))
let pta_sync_prod_actions_per_automaton                             = ref (List.init 0 (fun i -> i))
let pta_sync_prod_actions_per_location                              = ref (Array.make 0 (Array.make 0 []))
let pta_sync_prod_transitions : (int, int list) Hashtbl.t array ref = ref [||]
let pta_sync_prod_transitions_description                           = ref (Array.make 0 dummy_transition)
let pta_sync_prod_is_accepting                                      = ref (Array.make 0 (Array.make 0 Location_nonaccepting))
let pta_sync_prod_is_urgent                                         = ref (Array.make 0 (Array.make 0 Location_nonurgent))
let pta_sync_prod_invariants                                        = ref (Array.make 0 (Array.make 0 (False_guard)))

(*
  Function to compute the PTA syncronized product locations number
  The resulting automata has:
    length(A_{0}) * length(A_{1}) * ... * length(A_{n-2})  * length(A_{n-1})
  total locations
*)
let get_pta_sync_prod_locations_number (model : AbstractModel.abstract_model) : int =
  if !pta_sync_prod_locations_number = 0 then
    pta_sync_prod_locations_number :=
      List.fold_left (
        fun sum i -> sum * List.length (model.locations_per_automaton i)
      ) 1 (
        List.init model.nb_automata (fun i -> i)
      );
  !pta_sync_prod_locations_number

(*
  Function to get the PTA syncronized product transitions number
  The resulting automata has:
    length(apl_{0}) + length(apl_{1}) + ... + length(apl_{n-2}) + length(apl_{n-1})
  total transitions
*)
let get_pta_sync_prod_transitions_number () : int =
  !pta_sync_prod_transitions_number

(*
  Function to set the PTA syncronized product transitions number
*)
let set_pta_sync_prod_transitions_number (n_trans : int) : int =
  pta_sync_prod_transitions_number := n_trans;
  !pta_sync_prod_transitions_number

(*
  Function to initialize/get the PTA syncronized product automata name array
  To form name, it takes the name's first letter in each automata and concatenates them to form the name:
    "[lock, P1, P2] -> lPP"
  As the result is only one automata, the array is length 1
*)
let get_pta_sync_prod_automata_name (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_automata_name = 0 then
    pta_sync_prod_automata_name :=
      Array.make 1 (
        String.concat "" (
          List.map (fun name ->
            if name = "" then ""
            else String.make 1 name.[0]
          ) (List.init model.nb_automata model.automata_names)
        )
      );
  !pta_sync_prod_automata_name

(*
  Function to initialize/get the PTA syncronized product location names array
  The resulting array has:
    get_pta_sync_prod_locations_number model
  indices
*)
let get_pta_sync_prod_location_names (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_location_names = 0 then
    pta_sync_prod_location_names := Array.make (get_pta_sync_prod_locations_number model) "";

  for location_index = 0 to (get_pta_sync_prod_locations_number model) - 1 do
    if (!pta_sync_prod_location_names).(location_index) = "" then
      (!pta_sync_prod_location_names).(location_index) <- (
        let len_locations_per_automaton = List.init model.nb_automata (fun i -> List.length (model.locations_per_automaton i)) in

        (* Get digits and convert them into a string: ["000", "001", "002", ..., "243", "244"] *)
        let name_digits = digits len_locations_per_automaton location_index in
        let name = (get_pta_sync_prod_automata_name model).(0) in
        let s_name_digits = String.concat "" (List.map string_of_int name_digits) in

        (* Concatenate automata name with its digits: ["lPP000", "lPP001", "lPP002", ..., "lPP243", "lPP244"] *)
        let width = List.length len_locations_per_automaton in
        name ^ (
          if String.length s_name_digits >= width then s_name_digits
          else String.make (width - String.length s_name_digits) '0' ^ s_name_digits
        )
      );
  done;
  !pta_sync_prod_location_names

(*
  Function to initialize/get the PTA syncronized product locations_per_automaton array
*)
let get_pta_sync_prod_locations_per_automaton (model : AbstractModel.abstract_model) =
  if List.length !pta_sync_prod_locations_per_automaton = 0 then
    pta_sync_prod_locations_per_automaton := List.init (get_pta_sync_prod_locations_number model) (fun j -> j);
  !pta_sync_prod_locations_per_automaton

(*
  Function to initialize/get the PTA syncronized product action per automata array
*)
let get_pta_sync_prod_actions_per_automaton (model : AbstractModel.abstract_model) =
  if List.length !pta_sync_prod_actions_per_automaton = 0 then
    pta_sync_prod_actions_per_automaton := List.concat (List.init model.nb_automata model.actions_per_automaton)
    |> List.sort_uniq compare;
  !pta_sync_prod_actions_per_automaton

(*
  Function to initialize/get the PTA syncronized product action per location array
*)
let get_pta_sync_prod_actions_per_location (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_actions_per_location = 0 then
    pta_sync_prod_actions_per_location := Array.init (get_pta_sync_prod_locations_number model) (fun _ -> Array.make 0 []);
  !pta_sync_prod_actions_per_location

(*
  Function to initialize/get the PTA syncronized product transitions array
*)
let get_pta_sync_prod_transitions () =
  if Array.length !pta_sync_prod_transitions = 0 then
    pta_sync_prod_transitions := Array.init (get_pta_sync_prod_transitions_number ()) (fun _ -> Hashtbl.create 0);
  !pta_sync_prod_transitions

(*
  Function to initialize/get the PTA syncronized product transitions array
*)
let get_pta_sync_prod_transitions_description () =
  if Array.length !pta_sync_prod_transitions_description = 0 then
    pta_sync_prod_transitions_description := Array.init (get_pta_sync_prod_transitions_number ()) (fun _ -> dummy_transition);
  !pta_sync_prod_transitions_description

(*
  Function to initialize/get the PTA syncronized product is accepting array
*)
let get_pta_sync_prod_is_accepting (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_is_accepting = 0 then
    pta_sync_prod_is_accepting := Array.init (get_pta_sync_prod_locations_number model) (fun _ -> Array.make 0 Location_nonaccepting);
  !pta_sync_prod_actions_per_location

(*
  Function to initialize/get the PTA syncronized product is urgent array
*)
let get_pta_sync_prod_is_urgent (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_is_urgent = 0 then
    pta_sync_prod_is_urgent := Array.init (get_pta_sync_prod_locations_number model) (fun _ -> Array.make 0 Location_nonurgent);
  !pta_sync_prod_is_urgent

(*
  Function to initialize/get the PTA syncronized product invariants array
*)
let get_pta_sync_prod_invariants (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_invariants = 0 then
    pta_sync_prod_invariants := Array.init (get_pta_sync_prod_locations_number model) (fun _ -> Array.make 0 False_guard);
  !pta_sync_prod_invariants

(*
  Function to compute all location name indices having a digit "dig" at position "pos" in the name:
  indices_with_digit_at 1 0 -> [0; 1; 2; 3; 4; 25; 26; 27; 28; 29; 50; 51; 52; 53; 54]
*)
(*
let indices_with_digit_at (model : AbstractModel.abstract_model) (pos : int) (dig : int) =
  let automata_name = (get_pta_sync_prod_automata_name model).(0) in
  let loc_names = Array.to_list (get_pta_sync_prod_location_names model) in
  List.filter (fun li ->
    let loc_name = List.nth loc_names li in
    let l_loc_name = String.length loc_name in
    let l_automata_name = String.length automata_name in
    let actual_pos = l_automata_name + pos in
    if actual_pos >= l_loc_name then false
    else
      let c = loc_name.[actual_pos] in
      c = Char.chr (Char.code '0' + dig)
  ) (get_pta_sync_prod_locations_per_automaton model)
*)
(*
  Function to find a given location name and get its index
*)
let rec find_location_name_index (s : string) (i : int) =
  if i >= Array.length (!pta_sync_prod_location_names) then None
  else if (!pta_sync_prod_location_names).(i) = s then Some i
  else find_location_name_index s (i + 1)

(*
  Function to find a given integer and get its index
*)
(*
let find_index (target : int) (l : int list) : int option =
  let rec find_index_i i = function
    | [] -> None
    | x :: rest ->
      if x = target then Some i
      else find_index_i (i + 1) rest
  in
  find_index_i 0 l
*)
(*
  Function to find the max value in actions per location array
*)
(*
let max_action_per_location =
  Array.fold_left (fun max_curr1 actions ->
    List.fold_left (fun max_curr2 curr -> max max_curr2 curr) max_curr1 actions.(0)
  ) 0 (!pta_sync_prod_actions_per_location)
*)
(*
  Function to make PTA syncronized product initial location.
  Adapted from the original function in DiscreteState.ml
*)
let pta_sync_prod_make_location new_initial_location : DiscreteState.global_location =
  let locations_per_automaton = List.init 1 (fun _ -> 0, new_initial_location) in
  let discrete_values = List.init 1 (fun discrete_index -> discrete_index, AbstractValue.rational_zero) in
	(* Create an array for locations *)
  let locations = Array.make 1 0 in
	(* Create an array for discrete *)
  let discrete = Array.make 1 AbstractValue.rational_zero in
	(* Iterate on locations *)
  List.iter (fun (automaton_index, location_index) -> locations.(automaton_index) <- location_index) locations_per_automaton;
	(* Iterate on discrete *)
  List.iter (fun (discrete_index, value) -> discrete.(discrete_index) <- value) discrete_values;
	(* Return the new location *)
	(locations, discrete)

(*
  Function to build the PTA product model structure
*)
let compute_PTA_syncronized_product (model : AbstractModel.abstract_model) : AbstractModel.abstract_model =
  let sync_prod = {
    nb_actions                                      = model.nb_actions;
    nb_clocks                                       = model.nb_clocks;
    nb_discrete                                     = model.nb_discrete;
    nb_rationals                                    = model.nb_rationals;
    nb_parameters                                   = model.nb_parameters;
    nb_variables                                    = model.nb_variables;
    nb_ppl_variables                                = model.nb_ppl_variables;
    nb_transitions                                  = model.nb_transitions;
    has_invariants                                  = model.has_invariants;
    has_non_1rate_clocks                            = model.has_non_1rate_clocks;
    has_complex_updates                             = model.has_complex_updates;
    lu_status                                       = model.lu_status;
    strongly_deterministic                          = model.strongly_deterministic;
    has_silent_actions                              = model.has_silent_actions;

    bounded_parameters                              = model.bounded_parameters;
    parameters_bounds                               = model.parameters_bounds;

    observer_pta                                    = model.observer_pta;
    is_observer                                     = model.is_observer;

    clocks                                          = model.clocks;
    is_clock                                        = model.is_clock;
    special_reset_clock                             = model.special_reset_clock;
    global_time_clock                               = model.global_time_clock;
    clocks_without_special_reset_clock              = model.clocks_without_special_reset_clock;

    discrete                                        = model.discrete;
    discrete_rationals                              = model.discrete_rationals;
    is_discrete                                     = model.is_discrete;

    parameters                                      = model.parameters;
    clocks_and_discrete                             = model.clocks_and_discrete;
    parameters_and_discrete                         = model.parameters_and_discrete;
    parameters_and_clocks                           = model.parameters_and_clocks;

    variable_names                                  = model.variable_names;
    discrete_names_by_type_group                    = model.discrete_names_by_type_group;
    type_of_variables                               = model.type_of_variables;

    actions                                         = model.actions;
    controllable_actions                            = model.controllable_actions;
    has_controllable_or_uncontrollable_actions      = model.has_controllable_or_uncontrollable_actions;
    action_names                                    = model.action_names;
    action_types                                    = model.action_types;
    is_controllable_action                          = model.is_controllable_action;

    costs                                           = model.costs;
    stopwatches                                     = model.stopwatches;
    flow                                            = model.flow;

    functions_table                                 = model.functions_table;
    local_variables_table                           = model.local_variables_table;

    px_clocks_non_negative                          = model.px_clocks_non_negative;
    initial_constraint                              = model.initial_constraint;
    initial_p_constraint                            = model.initial_p_constraint;
    px_clocks_non_negative_and_initial_p_constraint = model.px_clocks_non_negative_and_initial_p_constraint;

(****************************************************************************************************************************)
(********************** WE ONLY RE-DEFINE WHAT WE NEED TO FORM THE NEW PTA SYNCRONIZED PRODUCT AUTOMATA *********************)
(****************************************************************************************************************************)

(****************************************************************************************************************************)
    (* PTA Sync Product has only 1 automata *)
(****************************************************************************************************************************)
    nb_automata                                     = 1; (* model.nb_automata; *)

(****************************************************************************************************************************)
    (* The resulting automata has: length(A_{0}) * length(A_{1}) * ... * length(A_{n-2}) * length(A_{n-1}) total locations *)
(****************************************************************************************************************************)
    nb_locations                                    = get_pta_sync_prod_locations_number model; (* model.nb_locations; *)

(****************************************************************************************************************************)
    (* The only automata *)
(****************************************************************************************************************************)
    automata                                        = OCamlUtilities.list_of_interval 0 0; (* model.automata; *)
    
(****************************************************************************************************************************)
    (* Takes the name's first letter in each automata and concatenates them to form the name: "[lock, P1, P2] -> lPP" *)
(****************************************************************************************************************************)
    automata_names                                  = (fun _ -> (get_pta_sync_prod_automata_name model).(0)); (* model.automata_names; *)

(****************************************************************************************************************************)
    (* Computes the number of locations and saves them as a list: [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; ...] *)
(****************************************************************************************************************************)
    locations_per_automaton                         = (fun _ -> get_pta_sync_prod_locations_per_automaton model); (* model.locations_per_automaton; *)
    
(****************************************************************************************************************************)
    (* Assigns a name to each location depending on the number of locations in each automata: [lock(3), P1(5), P2(5)] -> [lPP000, lPP001, lPP002, ..., lPP243, lPP244] *)
(****************************************************************************************************************************)
    location_names                                  = (fun _ location_index -> (get_pta_sync_prod_location_names model).(location_index)); (* model.location_names; *)

(****************************************************************************************************************************)
    (* Get all unique actions and assign them to automata 0 *)
(****************************************************************************************************************************)
    actions_per_automaton                           = (fun _ -> get_pta_sync_prod_actions_per_automaton model); (* model.actions_per_automaton; *)
    
(****************************************************************************************************************************)
    (* Automata 0 has all actions *)
(****************************************************************************************************************************)
    automata_per_action                             = (fun i -> if i >= 0 && i < model.nb_actions then [0] else []); (* model.automata_per_action; *)

(****************************************************************************************************************************)
    (* Assigns actions to locations *)
(****************************************************************************************************************************)
    actions_per_location                            = (fun _ location_index ->
                                                        (* Initialize the array *)
                                                        let _ = get_pta_sync_prod_actions_per_location model in
                                                        if Array.length (!pta_sync_prod_actions_per_location).(location_index) = 0 then
                                                          begin
                                                            (* Each array index has only 1 array containing all actions belonging to that location *)
                                                            (!pta_sync_prod_actions_per_location).(location_index) <- Array.make 1 [];

                                                            (* Check location name numbers *)
                                                            let location_name = (get_pta_sync_prod_location_names model).(location_index) in
                                                            let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

                                                            (* For each automata number automata_index *)
                                                            for automata_index = 0 to model.nb_automata - 1 do
                                                              (* Get location number location_index_curr at automata automata_index to concat the actions per location array at location position location_index *)
                                                              let location_index_curr = (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0') in

                                                              if location_index_curr < List.length (model.locations_per_automaton automata_index) then
                                                                begin
                                                                  (* Get the actions per location array in original model *)
                                                                  let actions_per_location = model.actions_per_location automata_index location_index_curr in
    
                                                                  (* Concat the actions per location array from automata automata_index and location location_index to new array *)
                                                                  (!pta_sync_prod_actions_per_location).(location_index).(0) <- (!pta_sync_prod_actions_per_location).(location_index).(0) @ actions_per_location;
                                                                end;
                                                            done;

                                                            (* Update transitions number *)
                                                            let _ = set_pta_sync_prod_transitions_number (!pta_sync_prod_transitions_number + List.length (!pta_sync_prod_actions_per_location).(location_index).(0)) in

                                                            (* Return actions per location array sorted and without repetitions *)
                                                            (!pta_sync_prod_actions_per_location).(location_index).(0);
                                                          end
                                                        else
                                                          (* Return actions per location array sorted and without repetitions *)
                                                          (!pta_sync_prod_actions_per_location).(location_index).(0);
                                                      ); (* model.actions_per_location; *)

(****************************************************************************************************************************)
    (* Assigns is_accepting boolean value to true if one location involved in the product is true *)
(****************************************************************************************************************************)
    is_accepting                                    = (fun _ location_index ->
                                                        (* Initialize the array *)
                                                        let _ = get_pta_sync_prod_is_accepting model in

                                                        if Array.length (!pta_sync_prod_is_accepting).(location_index) = 0 then
                                                          begin
                                                            (!pta_sync_prod_is_accepting).(location_index) <- Array.make 1 Location_nonaccepting;

                                                            (* Check location name numbers *)
                                                            let location_name = (get_pta_sync_prod_location_names model).(location_index) in
                                                            let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

                                                            (* For each automata number automata_index *)
                                                            for automata_index = 0 to model.nb_automata - 1 do
                                                              (* Get location number location_index_curr at automata automata_index to check is acceptance value at location position location_index *)
                                                              let location_index_curr = (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0') in

                                                              if location_index_curr < List.length (model.locations_per_automaton automata_index) then
                                                                (* Check if it is not true yet *)
                                                                if (!pta_sync_prod_is_accepting).(location_index).(0) = Location_nonaccepting then
                                                                  (* Assign is_accepting to PTA syncronized product new model *)
                                                                  (!pta_sync_prod_is_accepting).(location_index).(0) <- (
                                                                    match (model.is_accepting automata_index location_index_curr) with
                                                                    | false -> Location_nonaccepting
                                                                    | true -> Location_accepting
                                                                  );
                                                            done;
                                                          end;

                                                        (* Return true or false depending on location value *)
                                                        if (!pta_sync_prod_is_accepting).(location_index).(0) = Location_accepting then true
                                                        else false
                                                      ); (* model.is_accepting; *)

(****************************************************************************************************************************)
    (* Assigns is_urgent boolean value to true if one location involved in the product is true *)
(****************************************************************************************************************************)
    is_urgent                                       = (fun _ location_index ->
                                                        (* Initialize the array *)
                                                        let _ = get_pta_sync_prod_is_urgent model in

                                                        if Array.length (!pta_sync_prod_is_urgent).(location_index) = 0 then
                                                          begin
                                                            (!pta_sync_prod_is_urgent).(location_index) <- Array.make 1 Location_nonurgent;

                                                            (* Check location name numbers *)
                                                            let location_name = (get_pta_sync_prod_location_names model).(location_index) in
                                                            let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

                                                            (* For each automata number automata_index *)
                                                            for automata_index = 0 to model.nb_automata - 1 do
                                                              (* Get location number location_index_curr at automata automata_index to check is urgent value at location position location_index *)
                                                              let location_index_curr = (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0') in

                                                              if location_index_curr < List.length (model.locations_per_automaton automata_index) then
                                                                (* Check if it is not true yet *)
                                                                if (!pta_sync_prod_is_urgent).(location_index).(0) = Location_nonurgent then
                                                                  (* Assign is_urgent to PTA syncronized product new model *)
                                                                  (!pta_sync_prod_is_urgent).(location_index).(0) <- (
                                                                    match (model.is_urgent automata_index location_index_curr) with
                                                                    | false -> Location_nonurgent
                                                                    | true -> Location_urgent
                                                                  );
                                                            done;
                                                          end;

                                                        (* Return true or false depending on location value *)
                                                        if (!pta_sync_prod_is_urgent).(location_index).(0) = Location_urgent then true
                                                        else false
                                                      ); (* model.is_urgent; *)

(****************************************************************************************************************************)
    (* Assigns invariants to the product *)
(****************************************************************************************************************************)
    invariants                                      = (fun _ location_index ->
                                                        (* Initialize the array *)
                                                        let _ = get_pta_sync_prod_invariants model in

                                                        if Array.length (!pta_sync_prod_invariants).(location_index) = 0 then
                                                          begin
                                                            (!pta_sync_prod_invariants).(location_index) <- Array.make 1 False_guard;

                                                            (* Check location name numbers *)
                                                            let location_name = (get_pta_sync_prod_location_names model).(location_index) in
                                                            let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

                                                            (* For each automata number automata_index *)
                                                            for automata_index = 0 to model.nb_automata - 1 do
                                                              (* Get location number location_index_curr at automata automata_index to check its invariant at location position location_index *)
                                                              let location_index_curr = (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0') in

                                                              if location_index_curr < List.length (model.locations_per_automaton automata_index) then
                                                                (* Check if it is not true yet *)
                                                                if (!pta_sync_prod_invariants).(location_index).(0) = Location_nonurgent then
                                                                  (* Assign is_urgent to PTA syncronized product new model *)
                                                                  (!pta_sync_prod_invariants).(location_index).(0) <- (
                                                                    match (model.invariants automata_index location_index_curr) with
                                                                    | false -> Location_nonurgent
                                                                    | true -> Location_urgent
                                                                  );

                                                            done;
                                                          end;

                                                        (* Return invariant location value *)
                                                        (!pta_sync_prod_invariants).(location_index).(0)
                                                      );(* model.invariants; *)

(****************************************************************************************************************************)
    (* Automata 0 has all transitions *)
(****************************************************************************************************************************)
    transitions                                     = (fun _ location_index action_index ->
                                                        (* Initialize the array *)
                                                        let _ = get_pta_sync_prod_transitions () in

                                                        (* Initialize all location indices with empty Hash tables *)
                                                        for new_location_index = 0 to (get_pta_sync_prod_locations_number model) - 1 do
                                                          if Hashtbl.length (!pta_sync_prod_transitions).(new_location_index) = 0 then
                                                            (!pta_sync_prod_transitions).(new_location_index) <- Hashtbl.create (List.length (!pta_sync_prod_actions_per_location).(new_location_index).(0));
                                                        done;

                                                        (* Check if action_index has not the right transition number *)
                                                        if Hashtbl.length (!pta_sync_prod_transitions).(location_index) = 0 then
                                                          begin
                                                            (* Count transitions and locations *)
                                                            let transition_number = ref 0 in
                                                            let location_index_curr = ref 0 in

                                                            (* Get transition number *)
                                                            while !location_index_curr < location_index do
                                                              transition_number := !transition_number + (List.length (!pta_sync_prod_actions_per_location).(!location_index_curr).(0));
                                                              incr location_index_curr;
                                                            done;

                                                            (* Add transition number to the array *)
                                                            List.iter (fun action_index_curr ->
                                                              Hashtbl.replace (!pta_sync_prod_transitions).(location_index) action_index_curr [!transition_number];
                                                              incr transition_number
                                                            ) (!pta_sync_prod_actions_per_location).(location_index).(0);
                                                          end;

                                                        (* Return the requested transition index *)
                                                        Option.get (Hashtbl.find_opt (!pta_sync_prod_transitions).(location_index) action_index)
                                                      ); (* model.transitions; *)

(****************************************************************************************************************************)
    (* Automata 0 takes all transition "descriptions" from original model *)
(****************************************************************************************************************************)
    transitions_description                         = (fun transition_index ->
(*                                                        Printf.printf "\npta_sync_prod_transitions_description - Testing transition_index [%d]\n" transition_index; *)
                                                        (* Initialize the array *)
                                                        let _ = get_pta_sync_prod_transitions_description () in

                                                        (* If transitions list has nos been computed yet *)
                                                        if (!pta_sync_prod_transitions_description).(transition_index) = dummy_transition then
                                                          begin
(*                                                            Printf.printf "pta_sync_prod_transitions_description - Init transition_index [%d]\n" transition_index; *)
                                                            (* Count transitions and locations *)
                                                            let counter = ref 0 in
                                                            let location_index = ref 0 in

                                                            (* Get location index of transition index (location that owns the transition list) *)
                                                            while !counter + (List.length (!pta_sync_prod_actions_per_location).(!location_index).(0)) <= transition_index do
                                                              counter := !counter + (List.length (!pta_sync_prod_actions_per_location).(!location_index).(0));
                                                              incr location_index;
                                                            done;
(*                                                            Printf.printf "pta_sync_prod_transitions_description - location_index [%d]\n" !location_index; *)

                                                            (* Check location name numbers *)
                                                            let location_name = (get_pta_sync_prod_location_names model).(!location_index) in
                                                            let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in
                                                            let transition_index_counter = ref 0 in
(*                                                            Printf.printf "pta_sync_prod_transitions_description - location_name [%s]\n" location_name; *)

                                                            (* For each automata number automata_index *)
                                                            for automata_index = 0 to model.nb_automata - 1 do

                                                              (* Get location number location_index_curr at automata automata_index *)
                                                              let location_index_curr = (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0') in
(*                                                              Printf.printf "pta_sync_prod_transitions_description - location_index_curr [%d]\n" location_index_curr; *)

                                                              (* Look for transitions in automata automata_index and location_index_curr *)
                                                              let transitions = List.concat (
                                                                List.map (
                                                                  fun action_index -> model.transitions automata_index location_index_curr action_index
                                                                ) (model.actions_per_location automata_index location_index_curr)
                                                              ) in
(*                                                              Printf.printf "pta_sync_prod_transitions_description - transitions [%s]\n" (String.concat "; " (List.map string_of_int transitions)); *)

                                                              (* Attach transition descriptions list to new location in PTA sync product *)
                                                              for transition_index_curr = 0 to (List.length transitions) - 1 do

                                                                (* Get original transition *)
                                                                let transition = model.transitions_description (List.nth transitions transition_index_curr) in

                                                                (* Compute new target name according to original target name *)
                                                                let new_target_name = (
                                                                  let location_name_bytes = Bytes.of_string location_name in
                                                                    Bytes.set location_name_bytes (len_automata_name + automata_index) (
                                                                      char_of_int (
                                                                        trailing_number (model.location_names automata_index (transition.target)) + (Char.code '0')
                                                                      )
                                                                    );
                                                                    Bytes.to_string location_name_bytes
                                                                ) in
(*                                                                Printf.printf "pta_sync_prod_transitions_description - new_target_name [%s]\n" new_target_name; *)

                                                                (* Find new target index according to new target name *)
                                                                let new_target = Option.get (find_location_name_index new_target_name 0) in
(*
                                                                Printf.printf "pta_sync_prod_transitions_description - new_target [%d]\n" new_target;
                                                                Printf.printf "pta_sync_prod_transitions_description - transition_index_counter [%d] - transition_index + !transition_index_counter: [%d]\n" !transition_index_counter (transition_index + !transition_index_counter);
*)
                                                                (* Guard, action and updates remain the same *)
                                                                (!pta_sync_prod_transitions_description).(transition_index + !transition_index_counter) <- {
                                                                  guard   = transition.guard;
                                                                  action  = transition.action;
                                                                  updates = transition.updates;
                                                                  target  = new_target;
                                                                };
                                                                incr transition_index_counter;
                                                              done;
                                                            done;
                                                          end;

                                                        (* Return the requested transition *)
                                                        (!pta_sync_prod_transitions_description).(transition_index)
                                                      ); (* model.transitions_description *)

(****************************************************************************************************************************)
    (* Automata 0 is the owner of all transitions *)
(****************************************************************************************************************************)
    automaton_of_transition                         = (fun i -> if i >= 0 && i < get_pta_sync_prod_transitions_number () then 0 else -1); (* model.automaton_of_transition *)

(****************************************************************************************************************************)
    (* Initial location is the union of automata initial locations *)
(****************************************************************************************************************************)
    initial_location                                = (
                                                        (* Initialize location names array *)
                                                        let _ = get_pta_sync_prod_location_names model in

                                                        (* Form new initial location name with original automata initial location names *)
                                                        let new_initial_location_string = (get_pta_sync_prod_automata_name model).(0) ^ String.concat "" (
                                                          List.init model.nb_automata (fun automata_index ->
                                                            string_of_int (DiscreteState.get_location model.initial_location automata_index)
                                                          )
                                                        ) in

                                                        (* Find new initial location name index *)
                                                        let new_initial_location = Option.get (find_location_name_index new_initial_location_string 0) in

                                                        (* Return the new location *)
                                                        pta_sync_prod_make_location new_initial_location
                                                      ); (* model.initial_location; *)
	} in

  sync_prod
;;
