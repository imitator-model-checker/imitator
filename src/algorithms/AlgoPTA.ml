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
open DiscreteType
open Extrapolation
open LinearConstraint
open ModelConverter
open ModelPrinter

let string_of_lu_status = function
  | PTA_notLU -> "PTA_notLU"
  | PTA_LU _ -> "PTA_LU"
  | PTA_L -> "PTA_L"
  | PTA_U -> "PTA_U"

let string_of_option = function
  |	None -> "-"
  | Some n -> string_of_int n

(* Opción A: usar Gmp directamente *)
let string_of_numconst_or_infinity = function
  | Finite n -> Gmp.Q.to_string n
  | Infinity -> "+inf"
  | Minus_infinity -> "-inf"

let string_of_p_bounds p_bounds =
  let (min_b, max_b) = get_p_bounds p_bounds in
    Printf.sprintf "(%s, %s)" (string_of_numconst_or_infinity min_b) (string_of_numconst_or_infinity max_b)

let string_of_var_type_discrete_number = function
  | Dt_rat -> "rat"
  | Dt_int -> "int"
  | Dt_weak_number -> "weak_number"

let rec string_of_var_type_discrete = function
  | Dt_weak -> "weak"
  | Dt_void -> "void"
  | Dt_bool -> "bool"
  | Dt_number n -> "number(" ^ string_of_var_type_discrete_number n ^ ")"
  | Dt_bin m  -> Printf.sprintf "bin(%d)" m
  | Dt_array (t, n) -> Printf.sprintf "array(%s, %d)" (string_of_var_type_discrete t) n
  | Dt_list t -> "list("  ^ string_of_var_type_discrete t ^ ")"
  | Dt_stack t -> "stack(" ^ string_of_var_type_discrete t ^ ")"
  | Dt_queue t -> "queue(" ^ string_of_var_type_discrete t ^ ")"

let string_of_var_type = function
  | Var_type_clock -> "clock"
  | Var_type_discrete d -> string_of_var_type_discrete d
  | Var_type_parameter -> "parameter"

let string_of_var_type_group (vt, names) =
  Printf.sprintf "%s -> [%s]" (string_of_var_type vt) (String.concat "; " names)

let string_of_action_type = function
	| Action_type_sync -> "Action_type_sync"
	| Action_type_nosync -> "Action_type_nosync"

let rec string_of_linear_term = function
  | IR_Var v -> string_of_int v
  | IR_Coef c -> Gmp.Q.to_string c
  | IR_Plus (a, b) -> "(" ^ string_of_linear_term a ^ " + " ^ string_of_linear_term b ^ ")"
  | IR_Minus (a, b) -> "(" ^ string_of_linear_term a ^ " - " ^ string_of_linear_term b ^ ")"
  | IR_Times (c, a) -> Gmp.Q.to_string c ^ " * " ^ string_of_linear_term a

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

let pta_sync_prod_locations_number        = ref 0
let pta_sync_prod_transitions_number      = ref 0
let pta_sync_prod_automata_name           = ref (Array.make 0 "")
let pta_sync_prod_location_names          = ref (Array.make 0 "")
let pta_sync_prod_locations_per_automaton = ref (List.init 0 (fun i -> i))
let pta_sync_prod_actions_per_automaton   = ref (List.init 0 (fun i -> i))
let pta_sync_prod_actions_per_location    = ref (Array.make 0 (Array.make 0 []))
let pta_sync_prod_transitions             = ref (Array.make 0 (Array.make 0 []))
let pta_sync_prod_transitions_description = ref (Array.make 0 dummy_transition)

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
    pta_sync_prod_transitions := Array.init (get_pta_sync_prod_transitions_number ()) (fun _ -> Array.make 0 []);
  !pta_sync_prod_transitions

(*
  Function to initialize/get the PTA syncronized product transitions array
*)
let get_pta_sync_prod_transitions_description () =
  if Array.length !pta_sync_prod_transitions_description = 0 then
    pta_sync_prod_transitions_description := Array.init (get_pta_sync_prod_transitions_number ()) (fun _ -> dummy_transition);
  !pta_sync_prod_transitions_description

(*
  Function to compute all location name indices having a digit "dig" at position "pos" in the name:
  indices_with_digit_at 1 0 -> [0; 1; 2; 3; 4; 25; 26; 27; 28; 29; 50; 51; 52; 53; 54]
*)
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

(*
  Function to find a given location name and get its index
*)
let rec find_location_name_index (s : string) (i : int) =
  if i >= Array.length (!pta_sync_prod_location_names) then None
  else if (!pta_sync_prod_location_names).(i) = s then Some i
  else find_location_name_index s (i + 1)

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

    is_accepting                                    = model.is_accepting;
    is_urgent                                       = model.is_urgent;

    actions                                         = model.actions;
    controllable_actions                            = model.controllable_actions;
    has_controllable_or_uncontrollable_actions      = model.has_controllable_or_uncontrollable_actions;
    action_names                                    = model.action_names;
    action_types                                    = model.action_types;
    is_controllable_action                          = model.is_controllable_action;

    costs                                           = model.costs;
    invariants                                      = model.invariants;
    stopwatches                                     = model.stopwatches;
    flow                                            = model.flow;

    functions_table                                 = model.functions_table;
    local_variables_table                           = model.local_variables_table;

    px_clocks_non_negative                          = model.px_clocks_non_negative;

(****************************************************************************************************************************)
(****************************************************************************************************************************)
(********************** WE ONLY RE-DEFINE WHAT WE NEED TO FORM THE NEW PTA SYNCRONIZED PRODUCT AUTOMATA *********************)
(****************************************************************************************************************************)
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
                                                          (* Each array index has only 1 array containing all actions belonging to that location *)
                                                          (!pta_sync_prod_actions_per_location).(location_index) <- Array.make 1 [];

                                                        (* Check location name numbers *)
                                                        let location_name = (get_pta_sync_prod_location_names model).(location_index) in
                                                        let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

                                                        (* For each automata number automata_index *)
                                                        for automata_index = 0 to model.nb_automata - 1 do
                                                          (* Get location number location_index_curr at automata automata_index to concat the actions per location array at location position location_index *)
                                                          let location_index_curr = (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0') in

                                                          if location_index_curr < List.length (model.locations_per_automaton automata_index)then
                                                            begin
                                                              (* Get the actions per location array in original model *)
                                                              let actions_per_location = model.actions_per_location automata_index location_index_curr in

                                                              (* Concat the actions per location array from automata automata_index and location location_index to new array *)
                                                              (!pta_sync_prod_actions_per_location).(location_index).(0) <- (!pta_sync_prod_actions_per_location).(location_index).(0) @ actions_per_location;
                                                            end;
                                                        done;

                                                        let _ = List.sort_uniq compare (!pta_sync_prod_actions_per_location).(location_index).(0) in
                                                        let _ = set_pta_sync_prod_transitions_number (!pta_sync_prod_transitions_number + List.length (!pta_sync_prod_actions_per_location).(location_index).(0)) in

                                                        (* Return actions per location array sorted and without repetitions *)
                                                        (!pta_sync_prod_actions_per_location).(location_index).(0)
                                                      ); (* model.actions_per_location; *)

(****************************************************************************************************************************)
    (* Automata 0 has all transitions *)
(****************************************************************************************************************************)
    transitions                                     = (fun _ location_index action_index ->
                                                        (* Initialize the array *)
                                                        let _ = get_pta_sync_prod_transitions () in

                                                        (* Count transitions *)
                                                        let counter = ref 0 in

                                                        for new_location_index = 0 to (get_pta_sync_prod_locations_number model) - 1 do
                                                          if Array.length (!pta_sync_prod_transitions).(new_location_index) = 0 then
                                                            (* Each array index has only 1 array containing all transitions belonging to that location *)
                                                            (!pta_sync_prod_transitions).(new_location_index) <- Array.make 1 [];

                                                          (* Add transition number to the array *)
                                                          List.iter (fun _a ->
                                                            (!pta_sync_prod_transitions).(new_location_index).(0) <- (!pta_sync_prod_transitions).(new_location_index).(0) @ [!counter];
                                                            incr counter
                                                          ) (!pta_sync_prod_actions_per_location).(new_location_index).(0)
                                                        done;

                                                        (* Return the requested transition index *)
                                                        (!pta_sync_prod_transitions).(location_index).(action_index)
                                                      ); (* model.transitions; *)

(****************************************************************************************************************************)
    (* Automata 0 takes all transiction "descriptions" from original model *)
(****************************************************************************************************************************)
    transitions_description                         = (fun transition_index ->
                                                        (* Initialize the array *)
                                                        let _ = get_pta_sync_prod_transitions_description () in

                                                        (* If transitions list has nos been computed yet *)
                                                        if (!pta_sync_prod_transitions_description).(transition_index) = dummy_transition then
                                                          begin
                                                            (* Count transitions and locations *)
                                                            let counter = ref 0 in
                                                            let location_index = ref 0 in

                                                            (* Get location number of transition index *)
                                                            while !counter + (List.length (!pta_sync_prod_actions_per_location).(!location_index).(0)) <= transition_index do
                                                              counter := !counter + (List.length (!pta_sync_prod_actions_per_location).(!location_index).(0));
                                                              incr location_index;
                                                            done;
(*
                                                            Printf.printf "counter: [%d] - location_index: [%d]\n" !counter !location_index;
*)
                                                            (* Check location name numbers *)
                                                            let location_name = (get_pta_sync_prod_location_names model).(!location_index) in
                                                            let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in
                                                            let transition_index_counter = ref 0 in

                                                            (* For each automata number automata_index *)
                                                            for automata_index = 0 to model.nb_automata - 1 do

                                                              (* Get location number location_index_curr at automata automata_index *)
                                                              let location_index_curr = (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0') in

                                                              (* Look for transitions in automata automata_index and location_index_curr *)
                                                              let transitions = List.concat (
                                                                List.map (
                                                                  fun action_index -> model.transitions automata_index location_index_curr action_index
                                                                ) (model.actions_per_location automata_index location_index_curr)
                                                              ) in
(*
                                                              Printf.printf "transitions: [%s]\n" (String.concat "; " (List.map string_of_int transitions));
*)
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

                                                                (* Find new target index according to new target name *)
                                                                let new_target = Option.get (find_location_name_index new_target_name 0) in

                                                                (* Guard, action and updates remain the same *)
                                                                (!pta_sync_prod_transitions_description).(transition_index + !transition_index_counter) <- {
                                                                  guard   = transition.guard;
                                                                  action  = transition.action;
                                                                  updates = transition.updates;
                                                                  target  = new_target;
                                                                };
                                                                incr transition_index_counter;
(*
                                                                ImitatorUtilities.print_message Verbose_standard ("Transition " ^ (string_of_int (List.nth transitions transition_index_curr)) ^
                                                                                                                  ": in automaton `" ^ (model.automata_names automata_index) ^
                                                                                                                  "` via action `" ^ (model.action_names (transition.action)) ^
                                                                                                                  "` to location `" ^ (model.location_names automata_index (transition.target)) ^ "`")
*)
                                                              done;
                                                            done;
                                                          end;

                                                        (* Return the requested transition *)
                                                        (!pta_sync_prod_transitions_description).(transition_index)
                                                      ); (* model.transitions_description *)

    automaton_of_transition                         = (fun i -> if i >= 0 && i < get_pta_sync_prod_transitions_number () then 0 else -1); (* model.automaton_of_transition *)

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

    initial_constraint                              = model.initial_constraint; (* initial_constraint; *)

    initial_p_constraint                            = model.initial_p_constraint; (* initial_p_constraint; *)

    px_clocks_non_negative_and_initial_p_constraint = model.px_clocks_non_negative_and_initial_p_constraint; (* model.px_clocks_non_negative_and_initial_p_constraint; *)
	} in

  print_endline "nb_automata";
  print_int sync_prod.nb_automata;
  print_newline ();

  print_endline "nb_locations";
  print_int sync_prod.nb_locations;
  print_newline ();

  print_endline "nb_transitions";
  print_int sync_prod.nb_transitions;
  print_newline ();

  print_endline "automata";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.automata;

  print_endline "automata_names";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      Printf.printf "automata_names %d = %s\n" i (sync_prod.automata_names i);
    done
  in

  print_endline "locations_per_automaton";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
        Printf.printf "locations_per_automaton %d = [%s]\n" i (String.concat "; " (List.map string_of_int locs))
    done
  in

  print_endline "locations_names";
  let () =
    let locs = sync_prod.locations_per_automaton 0 in
      List.iter (fun l -> Printf.printf "  automaton %d, location %d = %s\n" 0 l (sync_prod.location_names 0 l)) locs
  in

  print_endline "actions_per_automaton";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      List.iter (fun x -> Printf.printf "%d\n" x) (sync_prod.actions_per_automaton i);
    done
  in

  print_endline "automata_per_action";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      List.iter (fun x -> Printf.printf "%d\n" x) (sync_prod.automata_per_action i);
    done
  in

  print_endline "actions_per_location";
  let () =
    let locs = sync_prod.locations_per_automaton 0 in
      List.iter (fun l -> Printf.printf "  automaton %d, location %d = [%s]\n" 0 l (String.concat "; " (List.map string_of_int (sync_prod.actions_per_location 0 l)))) locs
  in

  print_endline "transitions";
  let () =
    let _ = sync_prod.transitions 0 0 0 in
    for l = 0 to (get_pta_sync_prod_locations_number model) - 1 do
      List.iter2 (fun a t -> Printf.printf "  automaton 0, location %d, action %d = [%d]\n" l a t) (!pta_sync_prod_actions_per_location).(l).(0) (!pta_sync_prod_transitions).(l).(0)
    done
  in

  print_endline "transitions_description";
  let () =
		for j = 0 to get_pta_sync_prod_transitions_number () - 1 do
			let trans = sync_prod.transitions_description j in
			ImitatorUtilities.print_message Verbose_standard ("*****Transition " ^ (string_of_int j) ^
                                                        ": in automaton `" ^ (sync_prod.automata_names 0) ^
                                                        "` via action `" ^ (sync_prod.action_names (trans.action)) ^
                                                        "` to location `" ^ (sync_prod.location_names 0 (trans.target)) ^ "`")

		done
  in

  print_endline "automaton_of_transition";
  let () =
    for t = 0 to get_pta_sync_prod_transitions_number () - 1 do
      Printf.printf "  transition %d = automaton %d\n" t (sync_prod.automaton_of_transition t)
    done
  in

  print_endline "initial_location";
  let () =
    let initial_location = DiscreteState.get_location sync_prod.initial_location 0 in
    Printf.printf "initial_location %s\n" (get_pta_sync_prod_location_names model).(initial_location);
  in
(*
	sync_prod.initial_location : DiscreteState.global_location;
*)

  print_endline "initial_constraint";
(*
	sync_prod.initial_constraint : LinearConstraint.px_linear_constraint;
*)

  print_endline "initial_p_constraint";
(*
	sync_prod.initial_p_constraint : LinearConstraint.p_linear_constraint;
*)

  print_endline "px_clocks_non_negative_and_initial_p_constraint";
(*
	sync_prod.px_clocks_non_negative_and_initial_p_constraint: LinearConstraint.px_linear_constraint;
*)
(*
  print_endline "nb_actions";
  print_int sync_prod.nb_actions;
  print_newline ();

  print_endline "nb_clocks";
  print_int sync_prod.nb_clocks;
  print_newline ();

  print_endline "nb_discrete";
  print_int sync_prod.nb_discrete;
  print_newline ();

  print_endline "nb_rationals";
  print_int sync_prod.nb_rationals;
  print_newline ();

  print_endline "nb_parameters";
  print_int sync_prod.nb_parameters;
  print_newline ();

  print_endline "nb_variables";
  print_int sync_prod.nb_variables;
  print_newline ();

  print_endline "nb_ppl_variables";
  print_int sync_prod.nb_ppl_variables;
  print_newline ();

  print_endline "nb_transitions";
  print_int sync_prod.nb_transitions;
  print_newline ();

  print_endline "has_invariants";
  Printf.printf "%b\n" sync_prod.has_invariants;
	
  print_endline "has_non_1rate_clocks";
  Printf.printf "%b\n" sync_prod.has_non_1rate_clocks;

  print_endline "has_complex_updates";
  Printf.printf "%b\n" sync_prod.has_complex_updates;

  print_endline "lu_status";
  print_endline (string_of_lu_status sync_prod.lu_status);

  print_endline "strongly_deterministic";
  Printf.printf "%b\n" sync_prod.strongly_deterministic;

  print_endline "has_silent_actions";
  Printf.printf "%b\n" sync_prod.has_silent_actions;
	
  print_endline "bounded_parameters";
  Printf.printf "%b\n" sync_prod.bounded_parameters;

  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "parameters_bounds %d = %s\n" i (string_of_p_bounds (sync_prod.parameters_bounds i))
    done
  in

  print_endline "observer_pta";
  print_endline (string_of_option sync_prod.observer_pta);

  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "is_observer %d = %b\n" i (sync_prod.is_observer i);
    done
  in

  print_endline "clocks";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.clocks;

  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "is_clock %d = %b\n" i (sync_prod.is_clock i);
    done
  in

  print_endline "special_reset_clock";
  print_endline (string_of_option sync_prod.special_reset_clock);

  print_endline "clocks_without_special_reset_clock";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.clocks_without_special_reset_clock;

  print_endline "global_time_clock";
  print_endline (string_of_option sync_prod.global_time_clock);

  print_endline "discrete";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.discrete;

  print_endline "discrete_rationals";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.discrete_rationals;

  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "is_discrete %d = %b\n" i (sync_prod.is_discrete i);
    done
  in

  print_endline "parameters";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.parameters;

  print_endline "clocks_and_discrete";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.clocks_and_discrete;

  print_endline "parameters_and_discrete";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.parameters_and_discrete;

  print_endline "parameters_and_clocks";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.parameters_and_clocks;

  print_endline "variable_names";
  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "variable_names %d = %s\n" i (sync_prod.variable_names i);
    done
  in

  print_endline "discrete_names_by_type_group";
  List.iter (fun group -> print_endline ("  " ^ string_of_var_type_group group)) sync_prod.discrete_names_by_type_group;

  print_endline "type_of_variables";
  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "type_of_variables %d = %s\n" i (string_of_var_type (sync_prod.type_of_variables i));
    done
  in

  print_endline "locations_per_automaton";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
        Printf.printf "locations_per_automaton %d = [%s]\n" i (String.concat "; " (List.map string_of_int locs))
    done
  in

  print_endline "is_accepting";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
        List.iter (fun l -> Printf.printf "  automaton %d, location %d = %b\n" i l (sync_prod.is_accepting i l)) locs
    done
  in

  print_endline "is_urgent";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
        List.iter (fun l -> Printf.printf "  automaton %d, location %d = %b\n" i l (sync_prod.is_urgent i l)) locs
    done
  in

  print_endline "actions";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.actions;

  print_endline "controllable_actions";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.controllable_actions;

  print_endline "has_controllable_or_uncontrollable_actions";
  Printf.printf "%b\n" sync_prod.has_controllable_or_uncontrollable_actions;

  print_endline "action_names";
  List.iter (fun x -> print_endline (sync_prod.action_names x)) sync_prod.actions;

  print_endline "action_types";
  List.iter (fun x -> print_endline (string_of_action_type (sync_prod.action_types x))) sync_prod.actions;

  print_endline "is_controllable_action";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
      List.iter (fun l ->
        let actions = sync_prod.actions_per_location i l in
          List.iter (fun a -> Printf.printf "  automaton %d, location %d, action %d = %b\n" i l a (sync_prod.is_controllable_action a)) actions
      ) locs
    done
  in

	print_endline "costs";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
      List.iter (fun l ->
        match sync_prod.costs i l with
        | None -> Printf.printf "  automaton %d, location %d = None\n" i l
        | Some t -> Printf.printf "  automaton %d, location %d = %s\n" i l (string_of_linear_term t)
      ) locs
    done
  in

(*
	sync_prod.invariants : automaton_index -> location_index -> invariant;
*)

  print_endline "stopwatches";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
      List.iter (fun l ->
        Printf.printf "  automaton %d, location %d = [%s]\n" i l (String.concat "; " (List.map string_of_int (sync_prod.stopwatches i l)))
      ) locs
    done
  in

  print_endline "flow";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
      List.iter (fun l ->
        Printf.printf "  automaton %d, location %d = [%s]\n" i l (String.concat "; " (List.map (fun (c, v) -> Printf.sprintf "(%d, %s)" c (Gmp.Q.to_string v)) (sync_prod.flow i l)))
      ) locs
    done
  in

  print_endline "functions_table";
(*
  sync_prod.functions_table : (variable_name, fun_definition) Hashtbl.t;
*)

  print_endline "local_variables_table";
(*
  sync_prod.local_variables_table : (variable_ref, AbstractValue.abstract_value) Hashtbl.t;
*)

  print_endline "px_clocks_non_negative";
(*
	sync_prod.px_clocks_non_negative: LinearConstraint.px_linear_constraint;
*)
*)
  sync_prod
;;
