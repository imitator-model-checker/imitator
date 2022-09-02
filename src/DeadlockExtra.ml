(* JvdP: add some auxiliary code for deadlock checking, Paris July 2022 *)

type clock_updates = (Automaton.clock_index * LinearConstraint.pxd_linear_term) list

open LinearConstraint
open StateSpace
open DiscreteExpressionEvaluator

let dl_instantiate_discrete_gen discrete constr = 
    pxd_intersection_assign discrete [constr];
    pxd_hide_discrete_and_collapse discrete

(* go from pxd-constraint to px-constraint by substituting concrete values for discrete variables *)
let dl_instantiate_discrete state_space state_index constr = 
	let glob_location = get_location state_space (get_global_location_index state_space state_index) in
    let discrete = AlgoStateBased.discrete_constraint_of_global_location glob_location in
    dl_instantiate_discrete_gen discrete constr

(* go from pxd-constraint to px-constraint by substituting concrete values for discrete variables *)
let dl_instantiate_discrete_after_seq state_space state_index constr transition = 
	let glob_location = get_location state_space (get_global_location_index state_space state_index) in

    (* Copy location where we perform the destructive sequential updates*)
    let location = Location.copy_location glob_location in
    let model = Input.get_model() in 

    (* Get functions that enable reading / writing global variables at a given location *)
    let discrete_access = Location.discrete_access_of_location location in
    (* Make all sequential update first ! *)
	List.iter (fun transition_index ->
		(* Get the automaton concerned *)
		(* Access the transition and get the components *)
		let transitions_description = model.transitions_description transition_index in
		(** Collecting the updates by evaluating the conditions, if there is any *)
        let _ (* no clock update for seq updates *), discrete_seq_updates = AlgoStateBased.get_updates (Some model.variable_names) (Some model.functions_table) glob_location transitions_description.seq_updates in

        (* Make `seq` sequential updates (make these updates now, only on discrete) *)
        List.iter (direct_update (Some model.variable_names) (Some model.functions_table) discrete_access) (List.rev discrete_seq_updates);
	) transition;

    let discrete = AlgoStateBased.discrete_constraint_of_global_location location in

    dl_instantiate_discrete_gen discrete constr

(* "undo" the effect of updates on zone z (by computing the weakest precondition) *)
(* This is probably incomplete, if there was also a discrete update *) 
let dl_inverse_update state_space state_index z updates transition = 
    let model = Input.get_model () in (* only for printing *)
    let constr = px_copy z in
    let constr_pxd = pxd_of_px_constraint constr in
    (AlgoStateBased.apply_updates_assign_backward constr_pxd updates);

    let constr_px = dl_instantiate_discrete_after_seq state_space state_index constr_pxd transition in

    constr_px

(* Apply past time operator *)
let dl_inverse_time state_space state_index z =
	let glob_location = get_location state_space (get_global_location_index state_space state_index) in
    AlgoStateBased.apply_time_past glob_location z


(* compute direct predecessor of z2 in z1, linked by (guard,updates) *)
let dl_predecessor state_space state_index z1 guard updates z2 transition =
    let model = Input.get_model () in (* only for printing *)
    let constr = dl_inverse_update state_space state_index z2 updates transition in
    px_intersection_assign constr [z1];
    let constr_pxd = pxd_of_px_constraint constr in
    pxd_intersection_assign constr_pxd [guard];
    
    constr_pxd
    (* result *)

(* this extends get_resets: we return (x,0) for resets and (x,lt) for updates *)
let dl_get_clock_updates state_space combined_transition =
	(* Retrieve the model *)
	let model = Input.get_model () in

	(* For all transitions involved in the combined transition *)
	let updates = List.fold_left (fun current_updates transition_index ->
		(* Get the actual transition *)
		let transition = model.transitions_description transition_index in
        transition.updates.clock :: current_updates
	) [] combined_transition
	in

	(* Keep each update once *)
    (* TODO: check for inconsistent updates? *)
	OCamlUtilities.list_only_once updates

let dl_weakest_precondition state_space s1_index transition s2_index =
    let z1 = (get_state state_space s1_index).px_constraint in
    let z2 = (get_state state_space s2_index).px_constraint in
  

    let updates = dl_get_clock_updates state_space transition in
    let guard = get_guard state_space s1_index transition s2_index in
 
    dl_predecessor state_space s1_index z1 guard updates z2 transition
