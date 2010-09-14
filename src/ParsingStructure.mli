(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/08
 * Last modified: 2010/03/04
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
	| Var_type_analog
	| Var_type_clock
	| Var_type_discrete
	| Var_type_parameter

type variable_scope = 
	| Global
	| Local of automaton_name

type variable_declaration = var_type * variable_name * variable_scope

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
	| PrimedVariable of NumConst.t * variable_name


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

type update = convex_predicate

type guard = convex_predicate

(*type flow = variable_name * NumConst.t*)

(* Transition = Guard * update * sync label * destination location *)
type transition = guard * update * sync * location_name

(* Location = Name * Invariant * Flow * transitions *)
type location = location_name * convex_predicate * convex_predicate * transition list

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
(** Input program *)
(****************************************************************)

type parsing_structure = variable_declarations * automata * init_definition

(****************************************************************)
(** Input pi0 *)
(****************************************************************)

type pi0 = (string *  NumConst.t) list

type pi0cube = (string * NumConst.t * NumConst.t * NumConst.t) list
