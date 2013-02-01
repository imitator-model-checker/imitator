(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/08
 * Last modified: 2013/01/31
 *
 ****************************************************************)


(****************************************************************)
(** Names *)
(****************************************************************)

type automaton_name = string
type location_name = string
type variable_name = string
type sync_name = string


(****************************************************************)
(** Declarations *)
(****************************************************************)
(* Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_discrete
	| Var_type_parameter

(* We allow for some variables a value *)
type var_value = NumConst.t

type variable_declaration = var_type * (variable_name * var_value option) list

type variable_declarations = variable_declaration list


(****************************************************************)
(** Convex predicates and linear expressions *)
(****************************************************************)

(** Operators *)

type relop = OP_L | OP_LEQ | OP_EQ | OP_GEQ | OP_G


(** Linear expressions *)

type linear_term =
	| Constant of  NumConst.t
	| Variable of  NumConst.t * variable_name


type linear_expression =
	| Linear_term of linear_term
	| Linear_plus_expression of linear_expression * linear_term
	| Linear_minus_expression of linear_expression * linear_term


type linear_constraint =
	| True_constraint (** True *)
	| False_constraint (** False *)
	| Linear_constraint of linear_expression * relop * linear_expression


type convex_predicate = linear_constraint list


(****************************************************************)
(** Automata *)
(****************************************************************)

type sync =
	| Sync of sync_name
	| NoSync

type update = variable_name * linear_expression

type guard = convex_predicate

(* Transition = Guard * update * sync label * destination location *)
type transition = guard * update list * sync * location_name

(* Location = Name * Cost * Invariant * list of stopped clocks * transitions *)
type location = location_name * linear_expression option * convex_predicate * (variable_name list) * (transition list)

type automaton = automaton_name * sync_name list * location list

type automata = automaton list


(****************************************************************)
(** Init definition *)
(****************************************************************)

(** State predicates *)

type state_predicate =
	| Loc_assignment of automaton_name * location_name
	| Linear_predicate of linear_constraint	


type init_definition = state_predicate list


(****************************************************************)
(** Bad definition *)
(****************************************************************)

(** predicates for bad definition *)

type bad_predicate =
	| Exists_location of automaton_name * location_name
	| Exists_action of sync_name

type bad_definition  = bad_predicate list

(****************************************************************)
(** Carto definition *)
(****************************************************************)

type tile_nature =
	| Good
	| Bad
	| Unknown

(*** BADPROG: should not mix AbstractModel here (but it's easier) *)
type carto_definition  = (convex_predicate * tile_nature) list * (NumConst.t * NumConst.t) * (NumConst.t * NumConst.t)


(****************************************************************)
(** Input program *)
(****************************************************************)

type parsing_structure = variable_declarations * automata * init_definition * bad_definition * carto_definition

(****************************************************************)
(** Input pi0 *)
(****************************************************************)

type pi0 = (string *  NumConst.t) list

type v0 = (string * NumConst.t * NumConst.t) list
