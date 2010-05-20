(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/11
 * Last modified: 2010/03/29
 *
 ****************************************************************)



(****************************************************************)
(* Modules *)
(****************************************************************)
open Global
open LinearConstraint
open Automaton


(****************************************************************)
(** Environment *)
(****************************************************************)

(** Mode for IMITATOR *)
type imitator_mode =
	(** Classical parametric reachability analysis *)
	| Reachability_analysis
	(** Classical inverse method *)
	| Inverse_method
	(** Cover the whole cartography *)
	| Cover_cartography
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography of int


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

type pi0cube = (int * int) array


(****************************************************************)
(** Types *)
(****************************************************************)

(** Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_analog
	| Var_type_discrete
	| Var_type_parameter

(** Type of sync actions *)
type action_type =
	| Action_type_sync
	| Action_type_nosync

(****************************************************************)
(** State *)
(****************************************************************)

(** Location (control state): array automaton_index -> location_index *)
(* type location = location_index array *)

(** Discrete variables (control state): array automaton_index -> location_index *)
(* type discrete_values = variable_index array *)

(* type control_state = location  *)

(** State: location and constraint *)
type state = location * linear_constraint


(****************************************************************)
(** Transition *)
(****************************************************************)

(** update: variable_index := linear_term *)
type clock_update = clock_index * linear_term

(** update: variable_index := linear_term *)
type discrete_update = discrete_index * linear_term

(** Guard: linear constraint *)
type guard = linear_constraint

(** Transition: guard, updates, destination location *)
type transition = guard * clock_update list * discrete_update list * location_index


(****************************************************************)
(** The abstract program *)
(************************List.rev_append (List.rev_append discrete clocks) analogs;****************************************)
type abstract_program = {
	(* Cardinality *)
	nb_automata : int;
	nb_actions : int;
	(* nb_analogs : int; *)
	nb_clocks : int;
	nb_discrete : int;
	nb_parameters : int;
	nb_variables : int;
	
	(* The list of analog indexes *)
	(* analogs : clock_index list; *)	
	(* True for analogs, false otherwise *)
	is_analog : variable_index -> bool;
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
	(* non_parameters : variable_index list; *)
	(* The non parameters (clocks and discrete) *)
	clocks_and_discrete : variable_index list;
	(* The function : variable_index -> variable name *)
	variable_names : variable_index -> variable_name;
	(* The type of variables *)
	type_of_variables : variable_index -> var_type;
	
	(* Renamed clocks *)
	(* renamed_analogs : variable_index list; *)
	(* True for renamed clocks, false otherwise *)
	(* is_renamed_analog : variable_index -> bool; *)	
	(* Renamed clocks *)
	renamed_clocks : variable_index list;
	(* True for renamed clocks, false otherwise *)
	is_renamed_clock : variable_index -> bool;
	(* Get the 'prime' equivalent of a variable *)
	prime_of_variable : variable_index -> variable_index;
	(* Get the normal equivalent of a 'prime' variable *)
	variable_of_prime : variable_index -> variable_index;
	(* Parameter 'd' *)
	d : variable_index;
	(* Couples (x, x') for clock renamings *)
	(* renamed_analogs_couples : (variable_index * variable_index) list; *)
	(* Couples (x', x) for clock 'un'-renamings *)
	(* unrenamed_analogs_couples : (variable_index * variable_index) list; *)
	(* Couples (x, x') for clock renamings *)
	renamed_clocks_couples : (variable_index * variable_index) list;
	(* Couples (x', x) for clock 'un'-renamings *)
	unrenamed_clocks_couples : (variable_index * variable_index) list;

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

	(* The invariant for each automaton and each location *)
	invariants : automaton_index -> location_index -> linear_constraint;
	(* The rate condition for each automaton and each location *)
	flows : automaton_index -> location_index -> variable_index -> NumConst.t;
	(* The transitions for each automaton and each location and each action *)
	transitions : automaton_index -> location_index -> action_index -> (transition list);

	(* Time elapsing constraint : d >= 0 *)
	positive_d : linear_constraint;

	(* Init : the initial state *)
	init : state;

	(* Acyclic mode *)
	acyclic : bool;
	(* Inclusion for the post operation *)
	inclusion : bool;
	(* Random selection of the pi0-incompatible inequality *)
	random : bool;
	(* Mode for IMITATOR *)
	imitator_mode : imitator_mode;
	(* Mode with parametric constraints (clock elimination) in the log file *)
	with_parametric_log : bool;
	(* The name of the program *)
	program_name : string;
}


