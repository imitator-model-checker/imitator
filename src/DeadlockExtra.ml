(* JvdP: add some auxiliary code for deadlock checking, Paris July 2022 *)

type clock_updates = (Automaton.clock_index * LinearConstraint.pxd_linear_term) list

open LinearConstraint
open StateSpace

(* go from pxd-constraint to px-constraint by substituting concrete values for discrete variables *)
let dl_instantiate_discrete state_space state_index constr = 
	let glob_location = get_location state_space (get_global_location_index state_space state_index) in
    let discrete = AlgoStateBased.discrete_constraint_of_global_location glob_location in
    pxd_intersection_assign discrete [constr];
    pxd_hide_discrete_and_collapse discrete

(* "undo" the effect of updates on zone z (by computing the weakest precondition) *)
(* This is probably incomplete, if there was also a discrete update *) 
let dl_inverse_update state_space state_index z updates = 
    let model = Input.get_model () in (* only for printing *)
    let constr = px_copy z in
    let equality update = (* create an equlity x-lt=0 *)
        let (x,lt) = update in
        let clock = make_pxd_linear_term [(NumConst.one,x)] NumConst.zero in
        let diff = sub_pxd_linear_terms clock lt in
        let eq = make_pxd_linear_inequality diff Op_eq in
        ImitatorUtilities.print_message Verbose_medium  
           ("JvdP: \027[32mNew inverse update:\027[0m = \n" ^ 
             string_of_pxd_linear_inequality model.variable_names eq);
        eq in
    let updates_eqs = List.map equality updates in
    let updates_pxd = make_pxd_constraint updates_eqs in
    let updates_px = dl_instantiate_discrete state_space state_index updates_pxd in

    px_intersection_assign constr [updates_px];
    px_hide_assign (List.map fst updates) constr;
    constr

(* Apply past time operator *)
let dl_inverse_time state_space state_index z =
	let glob_location = get_location state_space (get_global_location_index state_space state_index) in
    AlgoStateBased.apply_time_past glob_location z


(* compute direct predecessor of z2 in z1, linked by (guard,updates) *)
let dl_predecessor state_space state_index z1 guard updates z2 =
    let model = Input.get_model () in (* only for printing *)
    let constr = dl_inverse_update state_space state_index z2 updates in
    px_intersection_assign constr [z1];
    let constr_pxd = pxd_of_px_constraint constr in
    pxd_intersection_assign constr_pxd [guard];

    ImitatorUtilities.print_message Verbose_medium  ("JvdP: \027[32mInverse_update\027[0m = \n" ^ string_of_px_linear_constraint model.variable_names constr);
    ImitatorUtilities.print_message Verbose_medium  ("JvdP: \027[32mWith guard\027[0m = \n" ^ string_of_pxd_linear_constraint model.variable_names constr_pxd);
    constr_pxd
    (* result *)

(* this extends get_resets: we return (x,0) for resets and (x,lt) for updates *)
let dl_get_clock_updates state_space combined_transition =
	(* Retrieve the model *)
	let model = Input.get_model () in

	(* For all transitions involved in the combined transition *)
	let resets = List.fold_left (fun current_resets transition_index ->
		(* Get the actual transition *)
		let transition = model.transitions_description transition_index in
        let zero =  make_pxd_linear_term [] NumConst.zero in
		(*** WARNING: we only accept clock resets (no arbitrary updates) ***)
		match transition.updates.clock with
			(* No update at all *)
			| No_update -> current_resets
			(* Reset to 0 only *)
			| Resets clock_resets -> List.rev_append (List.map (fun x -> (x,zero)) clock_resets) current_resets
			(* Reset to arbitrary value (including discrete, parameters and clocks) *)
			| Updates clock_updates -> List.rev_append clock_updates current_resets
	) [] combined_transition
	in

	(* Keep each update once *)
    (* TODO: check for inconsistent updates? *)
	OCamlUtilities.list_only_once resets

let dl_weakest_precondition state_space s1_index transition s2_index =
    let z1 = (get_state state_space s1_index).px_constraint in
    let z2 = (get_state state_space s2_index).px_constraint in
  

    let updates = dl_get_clock_updates state_space transition in
    let guard = get_guard state_space s1_index transition s2_index in
 
    dl_predecessor state_space s1_index z1 guard updates z2
