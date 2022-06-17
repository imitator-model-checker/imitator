(************************************************************
 *
 *                       IMITATOR
 * 
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: algorithm for 1-clock opacity (EXPERIMENTAL)
 * 
 * File contributors : Étienne André, Johan Arcile
 * Created           : 2022/06/10
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open AbstractProperty
open AlgoGeneric
open Result
open Statistics
open State
open Automaton
open LinearConstraint
open Location


(************************************************************)	
(************************************************************)
(* Function to check is the model is a 1-clock PTA in regards to the PTA definition (cf paper, Definition 1) *)
(* Note that aside from the restriction on the number of clocks, this definition is more restricted than the general expresivity of Imitator *)
(************************************************************)
(************************************************************)

let check_1cPTA (model : AbstractModel.abstract_model) : unit = (
	(*? has_non_1rate_clocks*)
	(*? stopwatches*)
	(*? flow*)
	(* The PTA has only 1 automaton *)
	if model.nb_automata != 1 then raise (Invalid_argument "Not a 1-clock PTA: exactly one automaton is requiered");
	(* The PTA has only 1 automaton *)
	if model.nb_clocks != 1 then raise (Invalid_argument "Not a 1-clock PTA: exactly one clock is requiered");	
	(* On any transition, the clock must either be untouched or reseted (no updates) *)
	for index=0 to model.nb_transitions-1 do 
		if ((model.transitions_description index).updates.clock != Resets(model.clocks) && (model.transitions_description index).updates.clock != No_update) then raise (Invalid_argument "Not a 1-clock PTA: clock updates are not authorized");
	done;
	)

(* Same function but result is a boolean *)
let is_1cPTA (model : AbstractModel.abstract_model) : bool =
	(*? has_non_1rate_clocks*)
	(*? stopwatches*)
	(*? flow*)
	let result = ref true in
	(* The PTA has only 1 automaton and 1 clock *)
	if (model.nb_automata != 1 || model.nb_clocks != 1) then (result := false);
	(* On any transition, the clock must either be untouched or reseted (no updates) *)
	for index=0 to model.nb_transitions-1 do 
		if ((model.transitions_description index).updates.clock != Resets(model.clocks) && (model.transitions_description index).updates.clock != No_update) then result := false
	done;
	!result
	
	
(************************************************************)	
(************************************************************)
(* Function to get index of target location *)
(************************************************************)
(************************************************************)

(* Gets the index of target location from state_predicate *)
let get_lf (state_predicate : AbstractProperty.state_predicate) =
	match state_predicate with
	|State_predicate_term(State_predicate_factor(Simple_predicate(Loc_predicate(Loc_predicate_EQ(0,lf))))) -> lf
	|_ -> raise (Invalid_argument "The state predicate should be a location of the PTA")
	

(************************************************************)	
(************************************************************)
(* Functions to compute the list of sub-automata *)
(* Warning : Only use on 1-clock PTAs *)
(************************************************************)
(************************************************************)


(************************************************************)
(* Sub-functions *)
(************************************************************)

(* Returns the set of locations with an reset on an ongoing transition *)
let get_reset_loc (model : AbstractModel.abstract_model) =
	let array_loc = Array.make model.nb_locations false in
	for index=0 to model.nb_transitions-1 do 
		if ((model.transitions_description index).updates.clock) = (Resets(model.clocks)) then array_loc.((model.transitions_description index).target) <- true;
	done;
	array_loc
	
(* Returns the set Pairs between l0 and lf (cf. paper Definition 8) *)	
let compute_couples reset_loc index_l0 index_lf =
	let couples = ref [] in
	for i=0 to (Array.length reset_loc)-1 do 
		if (reset_loc.(i) && i != index_lf) || i=index_l0 then
			for j=0 to (Array.length reset_loc)-1 do 
				if reset_loc.(j) || i=index_lf then
				couples := (!couples)@[(i,j)];
			done;
	done;
	!couples


(************************************************************)
(* Main function *)
(************************************************************)

(* Returns the set Pairs between the initial location and lf (cf. paper Definition 8) *)
let compute_sub_list (model : AbstractModel.abstract_model) (lf : int) : (int * int) list = 
	let l0 = get_location (model.initial_location) (List.hd model.automata) in
	compute_couples (get_reset_loc model) l0 lf



(************************************************************)	
(************************************************************)
(* Functions to build a given sub-automaton *)
(* Warning : Only use on 1-clock PTAs *)
(************************************************************)
(************************************************************)


(************************************************************)
(* Mutable type for manipulation of the model *)
(************************************************************)


type mutable_OneClockPTA = {
	(* Cardinality *)
	mutable nb_actions    : int;
	mutable nb_locations  : int;
	mutable nb_transitions: int;

	(* Does the model contain any transition labeled by a silent, non-observable action? *)
	mutable has_silent_actions : bool;

	(* The locations for each automaton *)
	mutable locations_per_automaton : automaton_index -> location_index list;
	(* The location names for each automaton *)
	mutable location_names : automaton_index -> location_index -> location_name;
	(* The acceptance for each location *)
	mutable is_accepting : automaton_index -> location_index -> bool;
	(* The urgency for each location *)
	mutable is_urgent : automaton_index -> location_index -> bool;

	(* All action indexes *)
	mutable actions : action_index list;
	(* Action names *)
	mutable action_names : action_index -> action_name;
	(* The type of actions *)
	mutable action_types : action_index -> action_type;
	(* The list of actions for each automaton *)
	mutable actions_per_automaton : automaton_index -> (action_index list);
	(* The list of automatons for each action *)
	mutable automata_per_action : action_index -> (automaton_index list);
	(* The list of actions for each automaton for each location *)
	mutable actions_per_location : automaton_index -> location_index -> (action_index list);
(*
	(* The cost for each automaton and each location *)
	costs : automaton_index -> location_index -> LinearConstraint.p_linear_term option;
*)
	(* The invariant for each automaton and each location *)
	mutable invariants : automaton_index -> location_index -> invariant;

	(* The transitions for each automaton and each location and each action *)
	mutable transitions : automaton_index -> location_index -> action_index -> (transition_index list);
	(* An array transition_index -> transition *)
	mutable transitions_description : transition_index -> transition;
	(* An array transition_index -> automaton_index *)
	mutable automaton_of_transition : transition_index -> automaton_index;

	(* Initial location of the model *)
	mutable initial_location : Location.global_location;
(*	(* Initial constraint of the model *)
	initial_constraint : LinearConstraint.px_linear_constraint;
	(* Initial constraint of the model projected onto P *)
	initial_p_constraint : LinearConstraint.p_linear_constraint;
	(* Initial constraint of the model projected onto P and all clocks non-negative *)
	px_clocks_non_negative_and_initial_p_constraint: LinearConstraint.px_linear_constraint;	*)

}


(************************************************************)
(* Sub-functions *)
(************************************************************)


(* Returns the upper cylindrification of a clock in a pxd_linear_constraint *)
let upper_cyl_constraint clock pxd_linear_constraint =		
	pxd_grow_to_infinity_assign [clock] (list_diff (pxd_get_dimensions_list pxd_linear_constraint) [clock]) pxd_linear_constraint;
	pxd_linear_constraint

(* Returns the upper cylindrification of a clock in the continuous part of a guard *)
let upper_cyl clock g =
	match g with
	|True_guard -> True_guard
	|False_guard -> False_guard
	|Discrete_guard(discrete_guard) -> Discrete_guard(discrete_guard)
	|Continuous_guard(pxd_linear_constraint) -> Continuous_guard(upper_cyl_constraint clock pxd_linear_constraint)
	|Discrete_continuous_guard(discrete_continuous_guard) -> Discrete_continuous_guard({discrete_guard = discrete_continuous_guard.discrete_guard; continuous_guard = (upper_cyl_constraint clock discrete_continuous_guard.continuous_guard);})


(* Returns the reset-free PTA (as a mutable structure) between li and lj (cf. paper Definition 9) *)
let compute_modifs (model : AbstractModel.abstract_model) (lf : int) ((li,lj) : (int * int)) : mutable_OneClockPTA =	
	let modifs = {
		nb_actions = model.nb_actions;
		nb_locations = model.nb_locations;
		nb_transitions = model.nb_transitions;
		has_silent_actions = model.has_silent_actions;
		locations_per_automaton = model.locations_per_automaton;
		location_names = model.location_names ;
		is_accepting = model.is_accepting;
		is_urgent = model.is_urgent;
		actions = model.actions;
		action_names = model.action_names;
		action_types = model.action_types;
		actions_per_automaton = model.actions_per_automaton;
		automata_per_action = model.automata_per_action;
		actions_per_location = model.actions_per_location;
		invariants = model.invariants;
		transitions = model.transitions;
		transitions_description = model.transitions_description;
		automaton_of_transition = model.automaton_of_transition;
		initial_location = model.initial_location;
	} in 
(* Step 1, Definition 9 *)
	(* Creates a duplicate of lj *)
	modifs.nb_locations <- modifs.nb_locations+1;
	let dup_lj = modifs.nb_locations-1 in
	let f = modifs.locations_per_automaton in
	modifs.locations_per_automaton <- (
		fun a -> match a with
		|a when (a=List.hd model.automata) -> (f a)@[dup_lj]
		|_ -> f a
	);
	let f = modifs.location_names in
	modifs.location_names <- (
		fun a l -> match (a,l) with
		|(a,l) when (a=List.hd model.automata && l=dup_lj) -> "duplicate"
		|_ -> f a l
	);
	let f = modifs.invariants in
	modifs.invariants <- (
		fun a l -> match (a,l) with
		|(a,l) when (a=List.hd model.automata && l=dup_lj) -> f a lj
		|_ -> f a l
	);
	(* Replaces incoming transitions to lj as incoming to duplicate *)
	let f = modifs.transitions_description in
	modifs.transitions_description <- (
		fun t -> match ((f t).updates.clock, (f t).target) with
		|(u,l) when (u=No_update && l=lj) -> {
			guard=(f t).guard;
			action=(f t).action;
			pre_updates=(f t).pre_updates;
			updates=(f t).updates;
			post_updates=(f t).post_updates;
			target=dup_lj;
		}
		|_ -> f t
	);
	if lj != lf then (
	(* Replaces outgoing from lj as outgoing from duplicate *)
		let f = modifs.transitions in
		modifs.transitions <- (
			fun a l e -> match (a,l,e) with
			|(a,l,e) when (a=List.hd model.automata && l=lj) -> []
			|(a,l,e) when (a=List.hd model.automata && l=dup_lj) -> modifs.transitions a lj e
			|_ -> f a l e
		);
		let f = modifs.actions_per_location in
		modifs.actions_per_location <- (
			fun a l -> match (a,l) with
			|(a,l) when (a=List.hd model.automata && l=dup_lj) -> f a lj
			|(a,l) when (a=List.hd model.automata && l=lj) -> []
			|_ -> f a l
		);
	) else (
	(* Sets duplicate as urgent *)
		let f = modifs.is_urgent in
		modifs.is_urgent<- (
			fun a l -> match (a,l) with
			|(a,l) when (a=List.hd model.automata && l=dup_lj) -> true
			|_ -> f a l
		);
	(* Adds an epsilon transition from duplicate to lj *)
		(* Adds a silent action *)
		modifs.nb_actions <- modifs.nb_actions+1;
		let epsilon = modifs.nb_actions-1 in
		modifs.actions <- modifs.actions@[epsilon];
		let f = modifs.action_names in
		modifs.action_names <- (
			fun a -> match a with
			|a when (a=epsilon) -> "epsilon"
			|_ -> f a
		);
		let f = modifs.action_types in
		modifs.action_types <- (
			fun a -> match a with
			|a when (a=epsilon)  -> Action_type_nosync
			|_ -> f a
		);
		let f = modifs.actions_per_automaton in
		modifs.actions_per_automaton <- (
			fun a -> match a with
			|a when a=List.hd model.automata -> (f a)@[epsilon]
			|_ -> f a
		);
		let f = modifs.automata_per_action in
		modifs.automata_per_action <- (
			fun a -> match a with
			|a when (a=epsilon) -> [List.hd model.automata]
			|_ -> f a
		);
		let f = modifs.actions_per_location in
		modifs.actions_per_location <- (
			fun a l -> match (a,l) with
			|(a,l) when (a=List.hd model.automata && l=dup_lj) -> [epsilon]
			|_ -> f a l
		);
		(* Adds the transition *)
		modifs.has_silent_actions <- true;
		modifs.nb_transitions <- modifs.nb_transitions+1;
		let silent_t = modifs.nb_transitions-1 in
		let f = modifs.transitions in
		modifs.transitions <- (
			fun a l e -> match (a,l,e) with
			|(a,l,e) when (a=List.hd model.automata && l=dup_lj && e = epsilon) -> (f a l e)@[silent_t]
			|_ -> f a l e
		);
		let f = modifs.transitions_description in
		modifs.transitions_description <- (
			fun t -> match t with
			|t when (t=silent_t) -> {
				guard=True_guard;
				action=epsilon;
				pre_updates={
					clock=No_update;
					discrete=[];
					conditional=[];
				};
				updates={
					clock=No_update;
					discrete=[];
					conditional=[];
				};
				post_updates={
					clock=No_update;
					discrete=[];
					conditional=[];
				};
				target=lj;
			}
			|_ -> f t
		);
		let f = modifs.automaton_of_transition in
		modifs.automaton_of_transition <- (
			fun t -> match t with
			|t when (t=silent_t) -> List.hd model.automata
			|_ -> f t
		);
	);
(* Step 2, Definition 9 *)
	(* Sets lj as urgent *)
	let f = modifs.is_urgent in
	modifs.is_urgent <- (
		fun a l -> match (a,l) with
		|(a,l) when (a=List.hd model.automata && l=lj) -> true
		|_ -> f a l
	);
	(* Performs the upper cylindrification of the clock on the invariant of lj *)
	let f = modifs.invariants in
	modifs.invariants <- (
		fun a l -> match (a,l) with
		|(a,l) when (a=List.hd model.automata && l=lj) -> upper_cyl (List.hd model.clocks) (f a l)
		|_ -> f a l
	);
(* Step 3, Definition 9 *)
	if li != lj then (
		modifs.initial_location <- (make_location ([(List.hd model.automata,li)]) ([]));
	) else (
		modifs.initial_location <- (make_location ([(List.hd model.automata,dup_lj)]) ([]));
	);
(* Step 4, Definition 9 *)
	(* Removes outgoing transitions from lf *)
	(* WARNING : automaton_of_transition not updated *)
	let f = modifs.transitions in
	modifs.transitions <- (
		fun a l e -> match (a,l) with
		|(a,l) when (a=List.hd model.automata && l=lf) -> []
		|_ -> f a l e
	);
	let f = modifs.actions_per_location in
	modifs.actions_per_location <- (
		fun a l -> match (a,l) with
		|(a,l) when (a=List.hd model.automata && l=lf) -> []
		|_ -> f a l
	);
	(* Removes resets on incoming transitions to lj *)
	let f = modifs.transitions_description in
	modifs.transitions_description <- (
		fun t -> match (f t).target with
		|l when (l = lj) -> {
			guard=(f t).guard;
			action=(f t).action;
			pre_updates={
				clock=No_update;
				discrete=(f t).pre_updates.discrete;
				conditional=(f t).pre_updates.conditional;
			};			
			updates={
				clock=No_update;
				discrete=(f t).updates.discrete;
				conditional=(f t).updates.conditional;
			};
			post_updates={
				clock=No_update;
				discrete=(f t).post_updates.discrete;
				conditional=(f t).post_updates.conditional;
			};
			target=lj;
		}
		|_ -> f t
	);
	(* Removes transitions with a reset *)
	(* WARNING : automaton_of_transition, actions_per_automaton, automata_per_action, actions_per_location not updated *)
	let f = modifs.transitions in
	modifs.transitions <- (
		fun a l e -> List.filter (fun e -> (modifs.transitions_description e).updates.clock=No_update) (f a l e)
	);
(* Returns the result *)	
	modifs
	
(* Creates a new model from the original one by replacing variables present in the mutable structure *)	
let transform_model (model : AbstractModel.abstract_model) (modifs : mutable_OneClockPTA) : AbstractModel.abstract_model =	
	let new_model = {
	(** General information **)
	(* Cardinality *)
	nb_automata = model.nb_automata;
	nb_actions = modifs.nb_actions;
	nb_clocks = model.nb_clocks;
	nb_discrete = model.nb_discrete;
	nb_parameters = model.nb_parameters;
	nb_variables = model.nb_variables;
	nb_locations = modifs.nb_locations;
	nb_transitions = modifs.nb_transitions;

	(* Is there any invariant in the model? *)
	has_invariants = model.has_invariants;
	(* Is there any clock going at a rate <> 1 in the model? *)
	has_non_1rate_clocks = model.has_non_1rate_clocks;
	(* Is the model an L/U-PTA? *)
	lu_status = model.lu_status;
	(* Is the model a strongly deterministic PTA? *)
	strongly_deterministic = model.strongly_deterministic;
	(* Does the model contain any transition labeled by a silent, non-observable action? *)
	has_silent_actions = modifs.has_silent_actions;
	(* Are all parameters bounded in the initial state? *)
	bounded_parameters = model.bounded_parameters;
	(* Function returning the bounds of each parameter *)
	parameters_bounds = model.parameters_bounds;

	(** Content of the PTA **)
	(* The observer *)
	observer_pta = model.observer_pta;
	is_observer = model.is_observer;

	(* The list of clock indexes *)
	clocks = model.clocks;
	(* True for clocks, false otherwise *)
	is_clock = model.is_clock;
	(* Index of the special clock to be reset at each transition to measure time elapsing (only used in NZ checking) *)
	special_reset_clock = model.special_reset_clock;
	(* The list of clock indexes except the reset clock (used, e.g., to print the model *)
	clocks_without_special_reset_clock = model.clocks_without_special_reset_clock;
	(* Index of a special clock meant to measure the global time (how this clock is actually used is up to the model designer *)
	global_time_clock = model.global_time_clock;
	(* The list of discrete indexes *)
	discrete = model.discrete;
	(* True for discrete, false otherwise *)
	is_discrete = model.is_discrete;
	(* The list of parameter indexes *)
	parameters = model.parameters;
	(* The non parameters (clocks and discrete) *)
	clocks_and_discrete = model.clocks_and_discrete;
	(* The non clocks (parameters and discrete) *)
	parameters_and_discrete = model.parameters_and_discrete;
	(* The non discrete (clocks and parameters) *)
	parameters_and_clocks = model.parameters_and_clocks;
	(* The function : variable_index -> variable name *)
	variable_names = model.variable_names;
	(* All discrete variable names group by types *)
    	discrete_names_by_type_group = model.discrete_names_by_type_group;
	(* The type of variables *)
	type_of_variables = model.type_of_variables;

	(* The automata *)
	automata = model.automata;
	(* The automata names *)
	automata_names = model.automata_names;

	(* The locations for each automaton *)
	locations_per_automaton = modifs.locations_per_automaton;
	(* The location names for each automaton *)
	location_names = modifs.location_names;
	(* The acceptance for each location *)
	is_accepting = modifs.is_accepting;
	(* The urgency for each location *)
	is_urgent = modifs.is_urgent;

	(* All action indexes *)
	actions = modifs.actions;
	(* Action names *)
	action_names = modifs.action_names;
	(* The type of actions *)
	action_types = modifs.action_types;
	(* The list of actions for each automaton *)
	actions_per_automaton = modifs.actions_per_automaton;
	(* The list of automatons for each action *)
	automata_per_action = modifs.automata_per_action;
	(* The list of actions for each automaton for each location *)
	actions_per_location = modifs.actions_per_location;

	(* The cost for each automaton and each location *)
	costs = model.costs;

	(* The invariant for each automaton and each location *)
	invariants = modifs.invariants;

	(* The transitions for each automaton and each location and each action *)
	transitions = modifs.transitions;
	(* The list of clocks stopped for each automaton and each location *)
	stopwatches = model.stopwatches;
	(* The list of pairs (clock, NumConst.t) defining the flow of some clocks at each automaton and each location *)
	flow = model.flow;
	(* An array transition_index -> transition *)
	transitions_description = modifs.transitions_description;
	(* An array transition_index -> automaton_index *)
	automaton_of_transition = modifs.automaton_of_transition;

	(* All clocks non-negative *)
	px_clocks_non_negative = model.px_clocks_non_negative;
	(* Initial location of the model *)
	initial_location = modifs.initial_location;
	(* Initial constraint of the model *)
	initial_constraint = model.initial_constraint;
	(* Initial constraint of the model projected onto P *)
	initial_p_constraint = model.initial_p_constraint;
	(* Initial constraint of the model projected onto P and all clocks non-negative *)
	px_clocks_non_negative_and_initial_p_constraint = model.px_clocks_non_negative_and_initial_p_constraint;
	} in new_model
	
	
(************************************************************)
(* Main function *)
(************************************************************)

(* Returns a model of the reset-free PTA between li and lj (cf. paper Definition 9) *)
let generate_sub_automaton (model : AbstractModel.abstract_model) (lf : int) ((li,lj) : (int * int)) : AbstractModel.abstract_model = 
	let modifs = compute_modifs model lf (li,lj)
	in transform_model model modifs
	
	
(************************************************************)	
(************************************************************)
(* Function to generate a regular expression from a set of arcs between l0 and lf *)
(************************************************************)
(************************************************************)

(* Syntax of regular expression with letters of the form (int * int) *)
type reg_exp =
	| Union of reg_exp * reg_exp
	| Concatenation of reg_exp * reg_exp
	| Star of reg_exp
	| Element of int * int
	| Epsilon
	| EmptySet

(* Structure of an arc in the finite automaton *)
type arc = {
	source : int;
	target : int;
	expression : reg_exp;
}

(* Converts a list of couple of locations to a list of arcs (directed finite automaton) *)
let initialize_DFA (sub_list : (int * int) list) : arc list =
	List.map (fun (i,j) -> {source=i;target=j;expression=Element(i,j);}) sub_list


(* Returns the union of reg_exp of a set of arcs, or Epsilon if the set is empty *)
let rec arcs_to_regexp_union arc_list =
	match arc_list with
	|h::t when (t != []) -> Union(h.expression, arcs_to_regexp_union t)
	|h::t -> h.expression
	|[] -> Epsilon (* This item allows that application of Star on the results wields Epsilon is the set is empty (for a correct union, this item should return EmptySet *)

(* Performs the elimination of loc from dfa *)	
let state_elimination (dfa : arc list) (loc : int) : arc list =
	let dfa = ref dfa in
	(* Computation of the sets of arcs to replace *)
	let source_list_init = List.filter (fun e -> e.source != loc && e.target = loc) !dfa in
	let loops_list = List.filter (fun e -> e.source = loc && e.target = loc) !dfa in
	let target_list_init = List.filter (fun e -> e.source = loc && e.target != loc) !dfa in
	(* Deletion of those arcs *)
	dfa := List.filter (fun e -> e.source != loc && e.target != loc) !dfa;
	(* Computation of the regular expression of the loops on loc*)
	let regexp_loops = Star(arcs_to_regexp_union loops_list) in
	(* Addition of the new arcs *)
	List.iter (
		fun s -> List.iter (
			fun t -> dfa := !dfa@[{
				source=s.source;
				target=t.target;
				expression=Concatenation(s.expression,Concatenation(regexp_loops,t.expression));
			}]
		)
		target_list_init
	) source_list_init;
	(* Return the dfa *)
	!dfa
	
	
(* Returns either a regular expression or an empty set by generating a DFA and performing a state elimination procedure on it *)
let compute_reg_exp (sub_list : (int * int) list) (l0 : int) (lf : int) (nb_locations : int) : reg_exp = 
	(* DFA generation *)
	let dfa = ref (initialize_DFA sub_list) in
	(* Addition of a pre-initial state with a silent arcs to l0 *)
	let pre_init = -1 in
	dfa := !dfa@[{
		source=pre_init;
		target=l0;
		expression=Epsilon;
			
	}];
	(* State elimination *)
	for i=0 to nb_locations-1 do 
		if i != lf then dfa := state_elimination (!dfa) i;
	done;
	(* Returns the regular expression *)
	let global_exp = ref EmptySet in
	List.iter (fun e -> global_exp := Union(!global_exp,e.expression)) !dfa;
	!global_exp
	

(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algo1cOpa (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoGeneric as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "1cOpa"

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
(* 		super#initialize_variables; *)
		
		(* The end *)
		()
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		(* Retrieve the model *)
		let model = Input.get_model () in
		(* Check is the model is 1cPTA*)
		check_1cPTA model;
		(* Retrieve the initial location *)
		let l0 = get_location (model.initial_location) (List.hd model.automata) in
		(* Retrieve the target location *)
		let lf = get_lf state_predicate in
		(* Compute the set of Pairs of localitions (cf paper, Definition 8 *)
		let sub_list = compute_sub_list model lf in
		(* Compute the set of reset-free automata (cf paper, Definition 9 *)
		let sub_automata = List.map (fun e -> (generate_sub_automaton model lf e)) sub_list in
		(* TODO : perform EF synth on each sub-automaton *)
		
		(* Compute the regular expression describing the language of the PTA *)
		let expression = compute_reg_exp sub_list l0 lf model.nb_locations in
		
		(* TODO : evaluate the regular expression with the "bar" operators (cf paper, after Definition 10)  *)
		
		raise (NotImplemented "1cOpa.run")
		

	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
