(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Abstract description of the input model
 * 
 * File contributors : Étienne André
 * Created           : 2009/09/11
 * Last modified     : 2015/11/23
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open Automaton
open Options


(************************************************************)
(** Pi 0 *)
(************************************************************)
(* type pi0 = NumConst.t array *)
type pi0 = (*variable_index -> NumConst.t*)PVal.pval

type v0 = HyperRectangle.hyper_rectangle


(************************************************************)
(** Types *)
(************************************************************)

(** Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_discrete
	| Var_type_parameter

(** Type of sync actions *)
type action_type =
	| Action_type_sync
	| Action_type_nosync



(************************************************************)
(** Locations *)
(************************************************************)
type location_urgency =
	(* Urgent location *)
	| Location_urgent
	(* Non-urgent location *)
	| Location_nonurgent


(************************************************************)
(** Transitions *)
(************************************************************)

(** update: variable_index := linear_term *)
type clock_update = clock_index 

type clock_updates =
	(* No update at all *)
	| No_update
	(* Reset to 0 only *)
	| Resets of clock_update list
	(** TO ADD: reset to constants / discrete and parameters (to allow for support by PDBM) *)
	(* Reset to arbitrary value (including discrete, parameters and clocks) *)
	| Updates of (clock_update * LinearConstraint.pxd_linear_term) list


(** update: variable_index := linear_term *)

(*** TO OPTIMIZE (in terms of dimensions!) ***)

type discrete_update = discrete_index * LinearConstraint.pxd_linear_term

(** Guard: linear constraint *)
type guard = LinearConstraint.pxd_linear_constraint

(** Invariant: linear constraint *)
type invariant = LinearConstraint.pxd_linear_constraint

(** Transition: guard, updates, destination location *)
type transition = guard * clock_updates * discrete_update list * location_index


(************************************************************)
(** Definition of correctness property *)
(************************************************************)
(** predicates for bad definition *)

type duration = LinearConstraint.p_linear_term

type unreachable_location = automaton_index * location_index

type discrete_value = NumConst.t

type discrete_constraint =
	| Discrete_l of discrete_index * discrete_value
	| Discrete_leq of discrete_index * discrete_value
	| Discrete_equal of discrete_index * discrete_value
	| Discrete_geq of discrete_index * discrete_value
	| Discrete_g of discrete_index * discrete_value
	| Discrete_interval of discrete_index * discrete_value * discrete_value

(* A global location is a list of locations (at most one per IPTA) and of simple atomic constraints on discrete variables (at most one constraint per discrete variable) *)
type unreachable_global_location = {
	unreachable_locations: unreachable_location list;
	discrete_constraints :  discrete_constraint list;
}

(** Definition of the property by the end user *)
type property =
	(* DEPRECATED *)
(* 	| Exists_action of action_index *)

	(* An "OR" list of global locations *)
	| Unreachable_locations of unreachable_global_location list

	(* if a2 then a1 has happened before *)
	| Action_precedence_acyclic of action_index * action_index
	(* everytime a2 then a1 has happened before *)
	| Action_precedence_cyclic of action_index * action_index
	(* everytime a2 then a1 has happened exactly once before *)
	| Action_precedence_cyclicstrict of action_index * action_index

	(*** NOTE: not implemented ***)
(*	(* if a1 then eventually a2 *)
	| Eventual_response_acyclic of action_index * action_index
	(* everytime a1 then eventually a2 *)
	| Eventual_response_cyclic of action_index * action_index
	(* everytime a1 then eventually a2 once before next *)
	| Eventual_response_cyclicstrict of action_index * action_index*)

	(* a no later than d *)
	| Action_deadline of action_index * duration

	(* if a2 then a1 happened within d before *)
	| TB_Action_precedence_acyclic of action_index * action_index * duration
	(* everytime a2 then a1 happened within d before *)
	| TB_Action_precedence_cyclic of action_index * action_index * duration
	(* everytime a2 then a1 happened once within d before *)
	| TB_Action_precedence_cyclicstrict of action_index * action_index * duration
	
	(* if a1 then eventually a2 within d *)
	| TB_response_acyclic of action_index * action_index * duration
	(* everytime a1 then eventually a2 within d *)
	| TB_response_cyclic of action_index * action_index * duration
	(* everytime a1 then eventually a2 within d once before next *)
	| TB_response_cyclicstrict of action_index * action_index * duration

	(* sequence: a1, ..., an *)
	| Sequence_acyclic of action_index list
	(* sequence: always a1, ..., an *)
	| Sequence_cyclic of action_index list
	
	(* Would be better to have an "option" type *)
	| Noproperty


type property_definition  = property


(** Reduction to (non-)reachability checking *)

type reachability_property =
	(* Location never reachable *)
	| Unreachable of unreachable_global_location list

	(* Location reachable for each trace *)
	(*** NOTE: not implemented ***)
	| Reachable of unreachable_global_location list (*automaton_index * location_index*)

	(* Combining the two properties *)
	(*** NOTE: not implemented ***)
	| Unreachable_and_reachable of (unreachable_global_location list) * (unreachable_global_location list) (*automaton_index * location_index * automaton_index * location_index*)


type correctness_condition = reachability_property option

type projection = (parameter_index list) option


(************************************************************)
(** Nature of the tiles *)
(************************************************************)
type lu_status =
	(* General PTA *)
	| PTA_notLU
	(* L/U-PTA with parameters partitioned into L- and U-parameters *)
	| PTA_LU of parameter_index list * parameter_index list
	(* L-PTA *)
	| PTA_L
	(* U-PTA *)
	| PTA_U


(************************************************************)
(** Nature of the tiles *)
(************************************************************)
(*** BADPROG : nothing to do with abstract model ! ***)
type tile_nature =
	| Good
	| Bad
	| Unknown




(************************************************************)
(** The abstract model *)
(************************************************************)
type abstract_model = {
	(** General information **)
	(* Cardinality *)
	nb_automata : int;
	nb_actions : int;
	nb_clocks : int;
	nb_discrete : int;
	nb_parameters : int;
	nb_variables : int;
	
	(* Is there any stopwatch in the model? *)
	has_stopwatches : bool;
	(* Is the model an L/U-PTA? *)
	lu_status : lu_status;

	(** Content of the PTA **)
	(* The observer *)
	observer_pta : automaton_index option;
	is_observer : automaton_index -> bool;

	(* The list of clock indexes *)
	clocks : clock_index list;
	(* True for clocks, false otherwise *)
	is_clock : variable_index -> bool;
	(* The list of discrete indexes *)
	discrete : discrete_index list;
	(* True for discrete, false otherwise *)
	is_discrete : variable_index -> bool;
	(* The list of parameter indexes *)
	parameters : parameter_index list;
	(* The non parameters (clocks and discrete) *)
	clocks_and_discrete : variable_index list;
	(* The non clocks (parameters and discrete) *)
	parameters_and_discrete : variable_index list;
	(* The function : variable_index -> variable name *)
	variable_names : variable_index -> variable_name;
	(* The type of variables *)
	type_of_variables : variable_index -> var_type;
	
	(* The automata *)
	automata : automaton_index list;
	(* The automata names *)
	automata_names : automaton_index -> automaton_name;
	
	(* The locations for each automaton *)
	locations_per_automaton : automaton_index -> location_index list;
	(* The location names for each automaton *)
	location_names : automaton_index -> location_index -> location_name;
	(* The urgency for each location *)
	is_urgent : automaton_index -> location_index -> bool;

	(* All action indexes *)
	actions : action_index list;
	(* Action names *)
	action_names : action_index -> action_name;
	(* The type of actions *)
	action_types : action_index -> action_type;
	(* The list of actions for each automaton *)
	actions_per_automaton : automaton_index -> (action_index list);
	(* The list of automatons for each action *)
	automata_per_action : action_index -> (automaton_index list);
	(* The list of actions for each automaton for each location *)
	actions_per_location : automaton_index -> location_index -> (action_index list);

	(* The cost for each automaton and each location *)
	costs : automaton_index -> location_index -> LinearConstraint.p_linear_term option;
	
	(* The invariant for each automaton and each location *)
	invariants : automaton_index -> location_index -> invariant;
	
	(* The transitions for each automaton and each location and each action *)
	transitions : automaton_index -> location_index -> action_index -> (transition list);
	(* The list of clocks stopped for each automaton and each location *)
	stopwatches : automaton_index -> location_index -> clock_index list;

	(* Init : the initial state *)
	initial_location : Location.global_location;
	(* Init : the initial state *)
	initial_constraint : LinearConstraint.px_linear_constraint;

	(* Property defined by the user *)
	user_property : property_definition;
	(* Property defined by the model *)
	correctness_condition : correctness_condition;
	(* List of parameters to project the result onto *)
	projection : projection;
	
	(* Set of polyhedra (only used for direct cartography without running the model) *)
	(*** BADPROG ***)
	carto : (LinearConstraint.p_linear_constraint * tile_nature) list * (NumConst.t * NumConst.t) * (NumConst.t * NumConst.t);
}


