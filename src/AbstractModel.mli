(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/11
 * Last modified: 2013/01/31
 *
 ****************************************************************)



(****************************************************************)
(* Modules *)
(****************************************************************)
open Global
open LinearConstraint
open Automaton
open Options

(****************************************************************)
(** Environment *)
(****************************************************************)


(****************************************************************)
(** Indexes *)
(****************************************************************)

type action_index = int
type action_name = string

(****************************************************************)
(** Pi 0 *)
(****************************************************************)
(* type pi0 = NumConst.t array *)
type pi0 = variable_index -> NumConst.t

type v0 = (NumConst.t * NumConst.t) array


(****************************************************************)
(** Types *)
(****************************************************************)

(** Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_discrete
	| Var_type_parameter

(** Type of sync actions *)
type action_type =
	| Action_type_sync
	| Action_type_nosync



(****************************************************************)
(** Transition *)
(****************************************************************)

(** update: variable_index := linear_term *)
type clock_update = clock_index 

type clock_updates =
	(* No update at all *)
	| No_update
	(* Reset to 0 only *)
	| Resets of clock_update list
	(* Reset to arbitrary value (including discrete, parameters and clocks) *)
	| Updates of (clock_update * linear_term) list


(** update: variable_index := linear_term *)
type discrete_update = discrete_index * linear_term

(** Guard: linear constraint *)
type guard = linear_constraint

(** Invariant: linear constraint *)
type invariant = linear_constraint

(** Transition: guard, updates, destination location *)
type transition = guard * clock_updates * discrete_update list * location_index


(****************************************************************)
(** Bad definition *)
(****************************************************************)
(** predicates for bad definition *)

type bad_predicate =
(* 	| Loc_assignment of automaton_name * location_name *)
	| Exists_action of action_index
	| Exists_location of automaton_index * location_index
	| Nobad


(* TODO: allow several definitions *)
type bad_definition  = bad_predicate (*list*)


(****************************************************************)
(** Nature of the tiles *)
(****************************************************************)
type tile_nature =
	| Good
	| Bad
	| Unknown


(****************************************************************)
(** The abstract program *)
(****************************************************************)
type abstract_program = {
	(* Cardinality *)
	nb_automata : int;
	nb_actions : int;
	nb_clocks : int;
	nb_discrete : int;
	nb_parameters : int;
	nb_variables : int;
	
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
	costs : automaton_index -> location_index -> linear_term option;
	
	(* The invariant for each automaton and each location *)
	invariants : automaton_index -> location_index -> invariant;
	
	(* The transitions for each automaton and each location and each action *)
	transitions : automaton_index -> location_index -> action_index -> (transition list);
	(* The list of clocks stopped for each automaton and each location *)
	stopwatches : automaton_index -> location_index -> clock_index list;
	(* Is there any stopwatch in the program? *)
	has_stopwatches : bool;

	(* Init : the initial state *)
	initial_location : global_location;
	(* Init : the initial state *)
	initial_constraint : linear_constraint;

	(* Bad states *)
	bad : bad_definition;
	(* Set of polyhedra (only used for direct cartography without running the model) *)
	carto : (linear_constraint * tile_nature) list * (NumConst.t * NumConst.t) * (NumConst.t * NumConst.t);
}



(****************************************************************)
(** Result *)
(****************************************************************)

(** Constraint returned by the inverse method *)
type returned_constraint =
	(** Constraint under convex form *)
	| Convex_constraint of LinearConstraint.linear_constraint * tile_nature
	(** Disjunction of constraints *)
	| Union_of_constraints of LinearConstraint.linear_constraint list * tile_nature

