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

(** Syncronized Product of Parametric Timed Automatas. *)

open AbstractModel

(*
  Struct used just to "initialize" pta_sync_prod_transitions_description array (copy from ModelConverter.ml)
*)
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

(*
  Function to combine costs in two different locations.
*)
let combine_costs c1 c2 =
  match c1, c2 with
  | None, None -> None
  | Some a, None -> Some a
  | None, Some b -> Some b
  | Some a, Some b -> Some (LinearConstraint.IR_Plus (a, b))

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
let pta_sync_prod_flow                                              = ref (Array.make 0 (Array.make 0 []))
let pta_sync_prod_stopwatches                                       = ref (Array.make 0 (Array.make 0 []))
let pta_sync_prod_costs                                             = ref (Array.make 0 (Array.make 0 None))

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
let set_pta_sync_prod_transitions_number (n_trans : int) : unit =
  pta_sync_prod_transitions_number := n_trans

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
    begin
      (* Get all location per automaton lengths in the original model *)
      let len_locations_per_automaton = List.init model.nb_automata (
        fun i -> List.length (model.locations_per_automaton i)
      ) in

      (* Get original automata name *)
      let name = (get_pta_sync_prod_automata_name model).(0) in

      pta_sync_prod_location_names :=
        (* Initialize the array *)
        Array.init (get_pta_sync_prod_locations_number model) (
          fun location_index ->
            (* Get digits and convert them into a string: ["000", "001", "002", ..., "243", "244"] *)
            let name_digits = digits len_locations_per_automaton location_index in
            let s_name_digits = String.concat "" (List.map string_of_int name_digits) in

            (* Concatenate automata name with its digits: ["lPP000", "lPP001", "lPP002", ..., "lPP243", "lPP244"] *)
            name ^ (
              if String.length s_name_digits >= model.nb_automata then s_name_digits
              else String.make (model.nb_automata - String.length s_name_digits) '0' ^ s_name_digits
            )
        )
    end;
  !pta_sync_prod_location_names

(*
  Function to find a given location name and get its index
*)
let rec find_location_name_index (model : AbstractModel.abstract_model) (s : string) (i : int) =
  if i >= Array.length (get_pta_sync_prod_location_names model) then None
  else if (get_pta_sync_prod_location_names model).(i) = s then Some i
  else find_location_name_index model s (i + 1)

(*
  Function to initialize/get the PTA syncronized product locations_per_automaton array
*)
let get_pta_sync_prod_locations_per_automaton (model : AbstractModel.abstract_model) =
  if List.length !pta_sync_prod_locations_per_automaton = 0 then
    (* Initialize the array *)
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
    begin
      (* Get automata name length in the new model *)
      let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

      pta_sync_prod_actions_per_location :=
        (* Initialize the array *)
        Array.init (get_pta_sync_prod_locations_number model) (
          fun location_index ->
            (* Get location name at index location_index *)
            let location_name = (get_pta_sync_prod_location_names model).(location_index) in

            (* Get actions per automaton array *)
            let actions = List.init model.nb_automata (
              (* For each automata number automata_index *)
              fun automata_index ->

                (* Get location number location_index_curr at automata automata_index to concat the actions per location array at location position location_index *)
                let location_index_curr =
                  (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0')
                in

                (* Get the actions per location array in original model *)
                if location_index_curr < List.length (model.locations_per_automaton automata_index) then
                  model.actions_per_location automata_index location_index_curr
                else []
            )
            (* Concat the actions per location array from automata automata_index and location location_index to new array *)
            |> List.concat in

          (* Each array index has only 1 array containing all actions belonging to that location *)
          [| actions |]
        );

        (* Update transitions number *)
        set_pta_sync_prod_transitions_number (
          Array.fold_left (
            fun sum actions_per_location -> sum + List.length actions_per_location.(0)
          ) 0 !pta_sync_prod_actions_per_location
        );
    end;
  !pta_sync_prod_actions_per_location

(*
  Function to initialize/get the PTA syncronized product transitions array
*)
let get_pta_sync_prod_transitions (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_transitions = 0 then
    begin
      (* Count transitions *)
      let transition_number = ref 0 in

      pta_sync_prod_transitions :=
        (* Initialize the array *)
        Array.init (get_pta_sync_prod_locations_number model) (
          fun location_index ->
            (* Initialize a transition hash table to store transition numbers *)
            let transition_table = Hashtbl.create (
              List.length (get_pta_sync_prod_actions_per_location model).(location_index).(0)
            ) in

            (* Fill in transition table depending on actions per location array *)
            List.iter (
              fun action_index_curr ->
                (* Check if the current transition at action_index_curr has not appeared before *)
                if not (Hashtbl.mem transition_table action_index_curr) then
                  begin
                    (* Add transition number to the table *)
                    Hashtbl.add transition_table action_index_curr [!transition_number];
                    incr transition_number
                  end
            ) (get_pta_sync_prod_actions_per_location model).(location_index).(0);
            transition_table
        );
    end;
  !pta_sync_prod_transitions

(*
  Function to initialize/get the PTA syncronized product transitions array
*)
let get_pta_sync_prod_transitions_description (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_transitions_description = 0 then
    begin
      let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

      pta_sync_prod_transitions_description := Array.of_list (
        List.concat (
          (* Initialize the array *)
          List.init (get_pta_sync_prod_locations_number model) (
            (* Location index of a transition (location that owns the transition list) *)
            fun location_index ->
              (* Check location name numbers *)
              let location_name = (get_pta_sync_prod_location_names model).(location_index) in

              List.concat (
                List.init model.nb_automata (
                  (* For each automata number automata_index *)
                  fun automata_index ->
                    (* Get location number location_index_curr at automata automata_index *)
                    let location_index_curr =
                      (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0')
                    in

                    (* Look for transitions in automata automata_index and location_index_curr *)
                    let transitions =
                      List.concat_map (
                        fun action_index ->
                          model.transitions automata_index location_index_curr action_index
                      ) (model.actions_per_location automata_index location_index_curr)
                    in

                    (* Get transition descriptions list with updated targets *)
                    List.filter_map (
                      fun transition_index_curr ->
                        (* Get original transition *)
                        let transition = model.transitions_description transition_index_curr in

                        (* Compute new target name according to original target name *)
                        let new_target_name =
                          let location_name_bytes = Bytes.of_string location_name in
                          Bytes.set location_name_bytes (len_automata_name + automata_index) (
                            char_of_int (
                              trailing_number (model.location_names automata_index transition.target) + Char.code '0'
                            )
                          );
                          Bytes.to_string location_name_bytes
                        in

                        (* Find new target index according to new target name *)
                        match find_location_name_index model new_target_name 0 with
                        | Some new_target -> Some
                          (* Guard, action and updates remain the same *)
                          {
                            guard = transition.guard;
                            action = transition.action;
                            updates = transition.updates;
                            target = new_target
                          }
                        | None -> None
                    ) transitions
                )
              )
          )
        )
      )
    end;
  !pta_sync_prod_transitions_description

(*
  Function to initialize/get the PTA syncronized product is accepting array
*)
let get_pta_sync_prod_is_accepting (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_is_accepting = 0 then
    begin
      let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

      pta_sync_prod_is_accepting :=
        (* Initialize the array *)
        Array.init (get_pta_sync_prod_locations_number model) (
          fun location_index ->
            (* Check location name numbers *)
            let location_name = (get_pta_sync_prod_location_names model).(location_index) in

            (* At least one acceptance value in a component is true? *)
            let is_accepting = List.exists (
                (* For each automata number automata_index *)
                fun automata_index ->
                  (* Get location number location_index_curr at automata automata_index to check is acceptance value *)
                  let location_index_curr =
                    (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0')
                  in
                  location_index_curr < List.length (model.locations_per_automaton automata_index) &&
                  model.is_accepting automata_index location_index_curr
            ) (List.init model.nb_automata (fun i -> i))
            in

            (* Assign is_accepting to PTA syncronized product new model *)
            [| if is_accepting then Location_accepting else Location_nonaccepting |]
        )
    end;
  !pta_sync_prod_is_accepting

(*
  Function to initialize/get the PTA syncronized product is urgent array
*)
let get_pta_sync_prod_is_urgent (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_is_urgent = 0 then
    begin
      let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

      pta_sync_prod_is_urgent :=
        (* Initialize the array *)
        Array.init (get_pta_sync_prod_locations_number model) (
          fun location_index ->
            (* Check location name numbers *)
            let location_name = (get_pta_sync_prod_location_names model).(location_index) in

            (* At least one urgent value in a component is true? *)
            let is_urgent = List.exists (
                (* For each automata number automata_index *)
                fun automata_index ->
                  (* Get location number location_index_curr at automata automata_index to check is urgent value *)
                  let location_index_curr =
                    (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0')
                  in
                  location_index_curr < List.length (model.locations_per_automaton automata_index) &&
                  model.is_urgent automata_index location_index_curr
            ) (List.init model.nb_automata (fun i -> i))
            in

            (* Assign is_accepting to PTA syncronized product new model *)
            [| if is_urgent then Location_urgent else Location_nonurgent |]
        )
    end;
  !pta_sync_prod_is_urgent

(*
  Function to initialize/get the PTA syncronized product invariants array
*)
let get_pta_sync_prod_invariants (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_invariants = 0 then
    begin
      let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

      (* Guards combination (still missing more options) *)
      let combine_guards g1 g2 =
        match g1, g2 with
        | False_guard, _ | _, False_guard -> False_guard
        | True_guard, g | g, True_guard -> g
        | Discrete_guard _, _ | _, Discrete_guard _
        | Continuous_guard _, _ | _, Continuous_guard _
        | Discrete_continuous_guard _, _ -> True_guard
(*
        | Discrete_guard d1, Discrete_guard d2 -> Discrete_guard (d1 && d2)
        | Continuous_guard c1, Continuous_guard c2 -> Continuous_guard (c1 && c2)
        | Discrete_guard d, Continuous_guard c -> Discrete_continuous_guard (d && c)
        | Continuous_guard c, Discrete_guard d -> Discrete_continuous_guard (c && d)
        | Discrete_continuous_guard dc, Discrete_guard d -> Discrete_continuous_guard (dc && d)
        | Discrete_guard d, Discrete_continuous_guard dc -> Discrete_continuous_guard (d && dc)
        | Discrete_continuous_guard dc, Continuous_guard c -> Discrete_continuous_guard (dc && c)
        | Continuous_guard c, Discrete_continuous_guard dc -> Discrete_continuous_guard (c && dc)
        | Discrete_continuous_guard dc1, Discrete_continuous_guard dc2 -> Discrete_continuous_guard (dc1 && dc2)
*)
      in

      pta_sync_prod_invariants :=
        (* Initialize the array *)
        Array.init (get_pta_sync_prod_locations_number model) (
          fun location_index ->
            (* Check location name numbers *)
            let location_name = (get_pta_sync_prod_location_names model).(location_index) in

            let invariant = List.fold_left (
              (* For each automata number automata_index *)
              fun guard_curr automata_index ->
                (* Get location number location_index_curr at automata automata_index to check its invariant *)
                let location_index_curr =
                  (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0')
                in

                (* Combine guards (starting with a True_guard) *)
                if location_index_curr < List.length (model.locations_per_automaton automata_index) then
                  combine_guards guard_curr (model.invariants automata_index location_index_curr)
                else
                  guard_curr
            ) True_guard (List.init model.nb_automata (fun i -> i))
            in

            (* Assign invariant to PTA syncronized product new model *)
            [| invariant |]
        )
    end;
  !pta_sync_prod_invariants

(*
  Function to initialize/get the PTA syncronized product flows array
*)
let get_pta_sync_prod_flow (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_flow = 0 then
    begin
      (* Get automata name length in the new model *)
      let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

      pta_sync_prod_flow :=
        (* Initialize the array *)
        Array.init (get_pta_sync_prod_locations_number model) (
          fun location_index ->
            (* Get location name at index location_index *)
            let location_name = (get_pta_sync_prod_location_names model).(location_index) in

            (* Get flows array *)
            let flows = List.concat_map (
              (* For each automata number automata_index *)
              fun automata_index ->
                (* Get location number location_index_curr at automata automata_index to concat the flows array at location position location_index *)
                let location_index_curr = 
                  (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0')
                in

                (* Get the flows array in original model *)
                if location_index_curr < List.length (model.locations_per_automaton automata_index) then
                  model.flow automata_index location_index_curr
                else []
            ) (List.init model.nb_automata (fun i -> i))
            in

            (* Each array index has only 1 array containing all flows belonging to that location *)
            [| flows |]
        )
    end;
  !pta_sync_prod_flow

(*
  Function to initialize/get the PTA syncronized product stopwatches array
*)
let get_pta_sync_prod_stopwatches (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_stopwatches = 0 then
    begin
      (* Get automata name length in the new model *)
      let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

      pta_sync_prod_stopwatches :=
        (* Initialize the array *)
        Array.init (get_pta_sync_prod_locations_number model) (
          fun location_index ->
            (* Get location name at index location_index *)
            let location_name = (get_pta_sync_prod_location_names model).(location_index) in

            (* Get clocks array *)
            let clocks = List.concat_map (
              (* For each automata number automata_index *)
              fun automata_index ->
                (* Get location number location_index_curr at automata automata_index to concat the clocks array at location position location_index *)
                let location_index_curr =
                  (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0')
                in

                (* Get the clocks array in original model *)
                if location_index_curr < List.length (model.locations_per_automaton automata_index) then
                  model.stopwatches automata_index location_index_curr
                else []
            ) (List.init model.nb_automata (fun i -> i))
            in

            (* Each array index has only 1 array containing all clocks belonging to that location *)
            [| clocks |]
        )
    end;
  !pta_sync_prod_stopwatches

(*
  Function to initialize/get the PTA syncronized product costs array
*)
let get_pta_sync_prod_costs (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_costs = 0 then
    begin
      (* Get automata name length in the new model *)
      let len_automata_name = String.length (get_pta_sync_prod_automata_name model).(0) in

      pta_sync_prod_costs :=
        (* Initialize the array *)
        Array.init (get_pta_sync_prod_locations_number model) (
          fun location_index ->
            (* Get location name at index location_index *)
            let location_name = (get_pta_sync_prod_location_names model).(location_index) in

            (* Get costs array *)
            let costs = List.map (
              (* For each automata number automata_index *)
              fun automata_index ->
                (* Get location number location_index_curr at automata automata_index to concat the costs array at location position location_index *)
                let location_index_curr =
                  (int_of_char location_name.[len_automata_name + automata_index]) - (Char.code '0')
                in

                (* Get the costs array in original model *)
                if location_index_curr < List.length (model.locations_per_automaton automata_index) then
                  model.costs automata_index location_index_curr
                else None
              ) (List.init model.nb_automata (fun i -> i))
            in

            (* Each array index has only 1 array containing all costs belonging to that location *)
            [| List.fold_left combine_costs None costs |]
        )
    end;
  !pta_sync_prod_costs

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
let get_PTA_syncronized_product_struct (model : AbstractModel.abstract_model) : AbstractModel.abstract_model =
  let sync_prod = {
    nb_actions                                      = model.nb_actions;
    nb_clocks                                       = model.nb_clocks;
    nb_discrete                                     = model.nb_discrete;
    nb_rationals                                    = model.nb_rationals;
    nb_parameters                                   = model.nb_parameters;
    nb_variables                                    = model.nb_variables;
    nb_ppl_variables                                = model.nb_ppl_variables;
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
    (* The resulting automata transition number *)
(****************************************************************************************************************************)
    nb_transitions                                  = get_pta_sync_prod_transitions_number (); (* model.nb_transitions; *)

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
    actions_per_location                            = (fun _ location_index -> (get_pta_sync_prod_actions_per_location model).(location_index).(0)); (* model.actions_per_location; *)

(****************************************************************************************************************************)
    (* Assigns is_accepting boolean value to true if one location involved in the product is true *)
(****************************************************************************************************************************)
    is_accepting                                    = (fun _ location_index ->
                                                        if (get_pta_sync_prod_is_accepting model).(location_index).(0) = Location_accepting then true
                                                        else false
                                                      ); (* model.is_accepting; *)

(****************************************************************************************************************************)
    (* Assigns is_urgent boolean value to true if one location involved in the product is true *)
(****************************************************************************************************************************)
    is_urgent                                       = (fun _ location_index ->
                                                        if (get_pta_sync_prod_is_urgent model).(location_index).(0) = Location_urgent then true
                                                        else false
                                                      ); (* model.is_urgent; *)

(****************************************************************************************************************************)
    (* Assigns invariants to the product *)
(****************************************************************************************************************************)
    invariants                                      = (fun _ location_index -> (get_pta_sync_prod_invariants model).(location_index).(0));(* model.invariants; *)

(****************************************************************************************************************************)
    (* Automata 0 has all transitions *)
(****************************************************************************************************************************)
    transitions                                     = (fun _ location_index action_index ->
                                                        match (Hashtbl.find_opt (get_pta_sync_prod_transitions model).(location_index) action_index) with
                                                        | None -> []
                                                        | Some x -> x
                                                      ); (* model.transitions; *)

(****************************************************************************************************************************)
    (* Assigns clocks to the product *)
(****************************************************************************************************************************)
    stopwatches                                     = (fun _ location_index -> (get_pta_sync_prod_stopwatches model).(location_index).(0)); (* model.stopwatches; *)

(****************************************************************************************************************************)
    (* Assigns flows to the product *)
(****************************************************************************************************************************)
    flow                                            = (fun _ location_index -> (get_pta_sync_prod_flow model).(location_index).(0)); (* model.flow; *)

(****************************************************************************************************************************)
    (* Assigns costs to the product *)
(****************************************************************************************************************************)
    costs                                           = (fun _ location_index -> (get_pta_sync_prod_costs model).(location_index).(0)); (* model.costs; *)

(****************************************************************************************************************************)
    (* Automata 0 takes all transition "descriptions" from original model *)
(****************************************************************************************************************************)
    transitions_description                         = (fun transition_index -> (get_pta_sync_prod_transitions_description model).(transition_index)); (* model.transitions_description *)

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
                                                        let new_initial_location = Option.get (find_location_name_index model new_initial_location_string 0) in

                                                        (* Return the new location *)
                                                        pta_sync_prod_make_location new_initial_location
                                                      ); (* model.initial_location; *)
	} in

  sync_prod
;;

(*
  Function to initialize the PTA product model structure functions and arrays,
  by calling for the first time its internal functions.
*)
let init_PTA_syncronized_product_struct (model : AbstractModel.abstract_model) : unit =
  let _ = model.nb_actions in
  let _ = model.nb_clocks in
  let _ = model.nb_discrete in
  let _ = model.nb_rationals in
  let _ = model.nb_parameters in
  let _ = model.nb_variables in
  let _ = model.nb_ppl_variables in
  let _ = model.has_invariants in
  let _ = model.has_non_1rate_clocks in
  let _ = model.has_complex_updates in
  let _ = model.lu_status in
  let _ = model.strongly_deterministic in
  let _ = model.has_silent_actions in

  let _ = model.bounded_parameters in
  let _ = model.parameters_bounds in

  let _ = model.observer_pta in
  let _ = model.is_observer in

  let _ = model.clocks in
  let _ = model.is_clock in
  let _ = model.special_reset_clock in
  let _ = model.global_time_clock in
  let _ = model.clocks_without_special_reset_clock in

  let _ = model.discrete in
  let _ = model.discrete_rationals in
  let _ = model.is_discrete in

  let _ = model.parameters in
  let _ = model.clocks_and_discrete in
  let _ = model.parameters_and_discrete in
  let _ = model.parameters_and_clocks in

  let _ = model.variable_names in
  let _ = model.discrete_names_by_type_group in
  let _ = model.type_of_variables in

  let _ = model.actions in
  let _ = model.controllable_actions in
  let _ = model.has_controllable_or_uncontrollable_actions in
  let _ = model.action_names in
  let _ = model.action_types in
  let _ = model.is_controllable_action in

  let _ = model.costs 0 0 in
  let _ = model.stopwatches 0 0 in
  let _ = model.flow 0 0 in

  let _ = model.functions_table in
  let _ = model.local_variables_table in

  let _ = model.px_clocks_non_negative in
  let _ = model.initial_constraint in
  let _ = model.initial_p_constraint in
  let _ = model.px_clocks_non_negative_and_initial_p_constraint in

  let _ = model.nb_automata in
  let _ = model.nb_locations in
  let _ = model.nb_transitions in
  let _ = model.automata in
  let _ = model.automata_names 0 in
  let _ = model.locations_per_automaton 0 in
  let _ = model.location_names 0 0 in
  let _ = model.actions_per_automaton 0 in
  let _ = model.automata_per_action 0 in
  let _ = model.actions_per_location 0 0 in
  let _ = model.is_accepting 0 0 in
  let _ = model.is_urgent 0 0 in
  let _ = model.invariants 0 0 in
  let _ = model.transitions 0 0 0 in
  let _ = model.transitions_description 0 in
  let _ = model.automaton_of_transition 0 in
  let _ = DiscreteState.get_location model.initial_location 0 in
  ()
;;
