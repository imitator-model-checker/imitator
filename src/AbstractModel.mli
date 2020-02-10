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
 * Last modified     : 2020/02/06
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

(* The types for global, discrete variables *)
type var_type_discrete =
	| Boolean
	| Rational
	| String
	| StringSet

(** Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_parameter
	| Var_type_discrete of var_type_discrete

(** Type of sync actions *)
type action_type =
	(* Observable action label (does not necessarily mean that it is "synchronized", as it can belong to a single automaton) *)
	| Action_type_sync
	(* Non-observable, silent action label (necessarily non-synchronized) *)
	| Action_type_nosync


type discrete_rational_value = NumConst.t


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
(** Guards and invariants *)
(************************************************************)

(*------------------------------------------------------------*)
(* Continuous expressions (for guards) *)
(*------------------------------------------------------------*)

type relop = OP_L | OP_LEQ | OP_EQ | OP_NEQ | OP_GEQ | OP_G

type convex_continuous_expression =
	| CCE_plus of convex_continuous_expression * convex_continuous_term
	| CCE_minus of convex_continuous_expression * convex_continuous_term
	| CCE_term of convex_continuous_term

and convex_continuous_term =
	| CCT_mul of convex_continuous_term * convex_continuous_factor
	| CCT_div of convex_continuous_term * convex_continuous_factor
	| CCT_factor of convex_continuous_factor

and convex_continuous_factor =
	| CCF_variable of Automaton.variable_index
	| CCF_constant of Automaton.constant_value
	| CCF_expression of convex_continuous_expression
	| CCF_unary_min of convex_continuous_factor

type convex_continuous_boolean_inequality = convex_continuous_expression * relop * convex_continuous_expression

(** Convex Boolean expression on discrete and continuous variables *)
type convex_continuous_boolean_expression =
	| CCBE_True (** True *)
	| CCBE_False (** False *)
	| CCBE_conjunction of convex_continuous_boolean_inequality list (** Conjunction *)


(*------------------------------------------------------------*)
(** Convec Boolean expression on discrete variables (for guards) *)
(*------------------------------------------------------------*)

(* A discrete Boolean expression is just a continuous Boolean expression without clock variables in it *)
type convex_discrete_boolean_expression = convex_continuous_boolean_expression



(*------------------------------------------------------------*)
(* Guards and invariants *)
(*------------------------------------------------------------*)

(*type discrete_continuous_guard = {
	discrete_guard   : discrete_guard;
	continuous_guard : continuous_guard;
}
type guard =
	| True_guard
	| False_guard
	| Discrete_guard of discrete_guard
	| Continuous_guard of continuous_guard
	| Discrete_continuous_guard of discrete_continuous_guard*)

type discrete_guard				= convex_discrete_boolean_expression
type prebuilt_continuous_guard	= LinearConstraint.px_linear_constraint
type continuous_guard			= convex_continuous_boolean_expression

type discrete_continuous_guard = {
	(* Only involves discrete variable, no polyhedra *)
	discrete_guard   			: discrete_guard option;
	(* Only involves continuous variables, can be stored in memory as a prebuilt polyhedra *)
	prebuilt_continuous_guard	: prebuilt_continuous_guard option;
	(* Involves continuous and discrete variables, polyhedra cannot be prebuilt *)
	continuous_guard			: continuous_guard option;
}

type guard =
	| True_guard
	| False_guard
	| Discrete_continuous_guard of discrete_continuous_guard



(** Invariant: technically a guard *)
type invariant = guard

(************************************************************)
(** Updates *)
(************************************************************)

type rational_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| RBE_Expression of convex_continuous_expression * relop * convex_continuous_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| RBE_Expression_in of convex_continuous_expression * convex_continuous_expression * (*rational_arithmetic_expression*)convex_continuous_expression


(*------------------------------------------------------------*)
(* Discrete Boolean expressions (for conditional expressions in updates) *)
(*------------------------------------------------------------*)

type convex_discrete_expression = convex_continuous_expression

(** Boolean expression on discrete variables (for updates) *)
type discrete_boolean_expression =
	| DBE_True (** True *)
	| DBE_False (** False *)
	| DBE_Not of discrete_boolean_expression (** Negation *)
	| DBE_And of discrete_boolean_expression * discrete_boolean_expression (** Conjunction *)
	| DBE_Or of discrete_boolean_expression * discrete_boolean_expression (** Disjunction *)
	| DBE_Rational_boolean_expression of rational_boolean_expression


(*------------------------------------------------------------*)
(*** TODO ***)
(*------------------------------------------------------------*)
type string_term = string


(*------------------------------------------------------------*)
(* Updates *)
(*------------------------------------------------------------*)


type discrete_term =
	| Rational_term of convex_continuous_expression
	| String_term of string_term

type discrete_update = discrete_index * discrete_term


(** update: variable_index := linear_term *)
(* type clock_update = clock_index *)

type clock_update =
	(* Reset to linear constraints over constants, clocks and parameters: can use prebuilt polyhedra *)
	| Prebuilt_reset of clock_index * LinearConstraint.px_linear_term

	(*** TODO: add reset to constants / discrete and parameters (to allow for support by PDBM) ***)
	
	(* Reset to arbitrary linear constraints over discrete, constants, clocks and parameters: cannot use prebuilt polyhedra *)
	| Reset of clock_index * convex_continuous_expression


(** update: variable_index := linear_term *)

(** Updates *)
type updates = {
	clock      : clock_update list;       (** Clock updates *)
	discrete   : discrete_update list;    (** List of discrete updates *)
	conditional: conditional_update list; (** List of conditional updates *)
}
(** Conditional updates *)
and conditional_update = discrete_boolean_expression * updates * updates


(************************************************************)
(** Transitions *)
(************************************************************)

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

	(* Is there any stopwatch in the model? *)
	has_stopwatches : bool;
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

(*	(* Property defined by the user (not used in the analysis, only for printing purpose; at this stage, the user property is already transformed into the correctness_condition below) *)
	user_property : property_definition;
	(* Property defined by the model *)
	correctness_condition : correctness_condition;
	(* List of parameters to project the result onto *)
	projection : projection;
	(* Parameter to be minimized or maximized *)
	optimized_parameter : optimization;*)

}
