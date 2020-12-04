(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Abstract description of the input model
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2009/09/11
 * Last modified     : 2020/12/04
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open Automaton


(************************************************************)
(** Reference parameter valuations *)
(************************************************************)
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
	(* Observable action label (does not necessarily mean that it is "synchronized", as it can belong to a single automaton) *)
	| Action_type_sync
	(* Non-observable, silent action label (necessarily non-synchronized) *)
	| Action_type_nosync


type discrete_value = NumConst.t


(************************************************************)
(** Locations *)
(************************************************************)
type location_accepting =
	(* accepting location *)
	| Location_accepting
	(* Non-accepting location *)
	| Location_nonaccepting

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

type discrete_update = discrete_index * DiscreteExpressions.discrete_arithmetic_expression

(** Guard: a linear constraint on the sole discrete variables, and a linear constraint on (possibly) all variables *)

type discrete_guard = LinearConstraint.d_linear_constraint
type continuous_guard = LinearConstraint.pxd_linear_constraint

type discrete_continuous_guard = {
	discrete_guard   : discrete_guard;
	continuous_guard : continuous_guard;
}
type guard =
	| True_guard
	| False_guard
	| Discrete_guard of discrete_guard
	| Continuous_guard of continuous_guard
	| Discrete_continuous_guard of discrete_continuous_guard


(** Invariant: linear constraint *)
type invariant = LinearConstraint.pxd_linear_constraint

(** Boolean expression *)
type boolean_expression =
	| True_bool (** True *)
	| False_bool (** False *)
	| Not_bool of boolean_expression (** Negation *)
	| And_bool of boolean_expression * boolean_expression (** Conjunction *)
	| Or_bool of boolean_expression * boolean_expression (** Disjunction *)
	| Discrete_boolean_expression of DiscreteExpressions.discrete_boolean_expression

(** Updates *)
type updates = {
  clock      : clock_updates;           (** Clock updates *)
  discrete   : discrete_update list;    (** List of discrete updates *)
  conditional: conditional_update list; (** List of conditional updates *)
}
(** Conditional updates *)
and conditional_update = boolean_expression * updates * updates

(** Transition: guard, action, list of updates, destination location *)
type transition = {
	guard		: guard;
	action		: action_index;
	updates		: updates;
	target		: location_index;
}

type transition_index = int




(************************************************************)
(** Subclass of the model *)
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
(** The abstract model *)
(************************************************************)
type abstract_model = {
	(** General information **)
	(* Cardinality *)
	nb_automata   : int;
	nb_actions    : int;
	nb_clocks     : int;
	nb_discrete   : int;
	nb_parameters : int;
	nb_variables  : int;
	nb_locations  : int;
	nb_transitions: int;

	(* Is there any clock going at a rate <> 1 in the model? *)
	has_non_1rate_clocks : bool;
	(* Is the model an L/U-PTA? *)
	lu_status : lu_status;
	(* Is the model a strongly deterministic PTA? *)
	strongly_deterministic : bool;
	(* Does the model contain any transition labeled by a silent, non-observable action? *)
	has_silent_actions : bool;

	(** Content of the PTA **)
	(* The observer *)
	observer_pta : automaton_index option;
	is_observer : automaton_index -> bool;

	(* The list of clock indexes *)
	clocks : clock_index list;
	(* True for clocks, false otherwise *)
	is_clock : variable_index -> bool;
	(* Index of the special clock to be reset at each transition to measure time elapsing (only used in NZ checking) *)
	special_reset_clock : clock_index option;
	(* The list of clock indexes except the reset clock (used, e.g., to print the model *)
	clocks_without_special_reset_clock : clock_index list;
	(* Index of a special clock meant to measure the global time (how this clock is actually used is up to the model designer *)
	global_time_clock : clock_index option;
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
	(* The non discrete (clocks and parameters) *)
	parameters_and_clocks : variable_index list;
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
	(* The acceptance for each location *)
	is_accepting : automaton_index -> location_index -> bool;
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
	transitions : automaton_index -> location_index -> action_index -> (transition_index list);
	(* The list of clocks stopped for each automaton and each location *)
	stopwatches : automaton_index -> location_index -> clock_index list;
	(* The list of pairs (clock, NumConst.t) defining the flow of some clocks at each automaton and each location *)
	flow : automaton_index -> location_index -> (clock_index * NumConst.t) list;
	(* An array transition_index -> transition *)
	transitions_description : transition_index -> transition;
	(* An array transition_index -> automaton_index *)
	automaton_of_transition : transition_index -> automaton_index;

	(* All clocks non-negative *)
	px_clocks_non_negative: LinearConstraint.px_linear_constraint;
	(* Initial location of the model *)
	initial_location : Location.global_location;
	(* Initial constraint of the model *)
	initial_constraint : LinearConstraint.px_linear_constraint;
	(* Initial constraint of the model projected onto P *)
	initial_p_constraint : LinearConstraint.p_linear_constraint;
	(* Initial constraint of the model projected onto P and all clocks non-negative *)
	px_clocks_non_negative_and_initial_p_constraint: LinearConstraint.px_linear_constraint;

}
