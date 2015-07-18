(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 *
 * Author:        Etienne Andre
 *
 * Created:       2009/09/08
 * Last modified: 2015/07/18
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
	| Var_type_constant
	| Var_type_discrete
	| Var_type_parameter

(* We allow for some variables (i.e., parameters and constants) a value *)
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
type invariant = convex_predicate

(* Transition = Guard * update * sync label * destination location *)
type transition = guard * update list * sync * location_name

(* Location = Name * Cost * Invariant * list of stopped clocks * transitions *)
type location = location_name * linear_expression option * invariant * (variable_name list) * (transition list)

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
(** Definition of the property *)
(****************************************************************)

type duration = linear_expression

(** Predicates for the definition of the correctness property *)

type property =
	| Unreachable_location of automaton_name * location_name
	
	(* DEPRECATED *)
(* 	| Unreachable_action of sync_name *)

	(* if a2 then a1 has happened before *)
	| Action_precedence_acyclic of sync_name * sync_name
	(* everytime a2 then a1 has happened before *)
	| Action_precedence_cyclic of sync_name * sync_name
	(* everytime a2 then a1 has happened exactly once before *)
	| Action_precedence_cyclicstrict of sync_name * sync_name

	(* if a1 then eventually a2 *)
	| Eventual_response_acyclic of sync_name * sync_name
	(* everytime a1 then eventually a2 *)
	| Eventual_response_cyclic of sync_name * sync_name
	(* everytime a1 then eventually a2 once before next *)
	| Eventual_response_cyclicstrict of sync_name * sync_name

	(* a no later than d *)
	| Action_deadline of sync_name * duration

	(* if a2 then a1 happened within d before *)
	| TB_Action_precedence_acyclic of sync_name * sync_name * duration
	(* everytime a2 then a1 happened within d before *)
	| TB_Action_precedence_cyclic of sync_name * sync_name * duration
	(* everytime a2 then a1 happened once within d before *)
	| TB_Action_precedence_cyclicstrict of sync_name * sync_name * duration
	
	(* if a1 then eventually a2 within d *)
	| TB_response_acyclic of sync_name * sync_name * duration
	(* everytime a1 then eventually a2 within d *)
	| TB_response_cyclic of sync_name * sync_name * duration
	(* everytime a1 then eventually a2 within d once before next *)
	| TB_response_cyclicstrict of sync_name * sync_name * duration

	(* sequence: a1, ..., an *)
	| Sequence_acyclic of sync_name list
	(* sequence: always a1, ..., an *)
	| Sequence_cyclic of sync_name list


type property_definition  = property option


(****************************************************************)
(** Projection definition *)
(****************************************************************)

type projection = (variable_name list) option


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

(* TODO: transform to structure *)
type parsing_structure =
	variable_declarations
	* automata
	* init_definition
	* property_definition
	* projection
	* carto_definition


(****************************************************************)
(** Input pi0 *)
(****************************************************************)

type pi0 = (string *  NumConst.t) list

type v0 = (string * NumConst.t * NumConst.t) list
