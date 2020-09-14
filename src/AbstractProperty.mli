(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Property description
 *
 * File contributors : Étienne André
 * Created           : 2019/10/08
 * Last modified     : 2020/09/14
 *
 ************************************************************)



(************************************************************)
(* Modules *)
(************************************************************)
open Automaton
(* open AbstractModel *)


(****************************************************************)
(** Predicates for properties *)
(****************************************************************)

type loc_predicate =
	| Loc_predicate_EQ of automaton_index * location_index
	| Loc_predicate_NEQ of automaton_index * location_index


type simple_predicate =
	| Discrete_boolean_expression of DiscreteExpressions.discrete_boolean_expression
	| Loc_predicate of loc_predicate
	| State_predicate_true
	| State_predicate_false
	| State_predicate_accepting


type state_predicate_factor =
	| State_predicate_factor_NOT of state_predicate_factor
	| Simple_predicate of simple_predicate
	| State_predicate of state_predicate

and state_predicate_term =
	| State_predicate_term_AND of state_predicate_term * state_predicate_term
	| State_predicate_factor of state_predicate_factor

and state_predicate =
	| State_predicate_OR of state_predicate * state_predicate
	| State_predicate_term of state_predicate_term


(************************************************************)
(** Definition of property *)
(************************************************************)

type duration = LinearConstraint.p_linear_term

type property =

	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF of state_predicate
	
	(* Safety *)
	| AGnot of state_predicate
	
	
(*	
	(* Unavoidability *)
	| AF of state_predicate
*)
	
	(*------------------------------------------------------------*)
	(* Reachability and specification illustration *)
	(*------------------------------------------------------------*)
	
	(** EF-synthesis with examples of (un)safe words *)
	| EFexemplify of state_predicate
	
	(*------------------------------------------------------------*)
	(* Optimized reachability *)
	(*------------------------------------------------------------*)
	
	(* Reachability with minimization of a parameter valuation *)
	| EFpmin of state_predicate * parameter_index
	
	(* Reachability with maximization of a parameter valuation *)
	| EFpmax of state_predicate * parameter_index
	
	(* Reachability with minimal-time *)
	| EFtmin of state_predicate
	

	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through of state_predicate

	(** Infinite-run (cycle) with non-Zeno assumption *)
	| NZ_Cycle
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Deadlock_Freeness

	
	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	
	(* Inverse method with complete, non-convex result *)
	| IM of PVal.pval

	(* Non-complete, non-deterministic inverse method with convex result *)
	| ConvexIM of PVal.pval

	(* Parametric reachability preservation *)
	| PRP of state_predicate * PVal.pval

	(* Variant IMK of the Inverse method *)
	| IMK of PVal.pval

	(* Variant IMunion of the Inverse method *)
	| IMunion of PVal.pval

	
	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)
	
	(*** NOTE: the last argument is the step; it is optional in the parser, and its value is otherwise defined in Constants ***)
	
	(* Cartography *)
	| Cover_cartography of HyperRectangle.hyper_rectangle * NumConst.t

	(** Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography of state_predicate * HyperRectangle.hyper_rectangle * NumConst.t
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography of HyperRectangle.hyper_rectangle * NumConst.t
	
	(** Look for the border using the cartography*)
	| Border_cartography of HyperRectangle.hyper_rectangle * NumConst.t
	
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography of HyperRectangle.hyper_rectangle * int * NumConst.t
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography of HyperRectangle.hyper_rectangle * int * NumConst.t

	(* Parametric reachability preservation *)
	| PRPC of state_predicate * HyperRectangle.hyper_rectangle * NumConst.t




type synthesis_type =
	(* (tentative) exhibition of at least one valuation for which a property holds *)
	| Witness
	(* (tentative) synthesis of all valuations for which a property holds *)
	| Synthesis


type projection = (parameter_index list) option

  
(************************************************************)
(** The actual property *)
(************************************************************)

type abstract_property = {
	(* Emptiness or synthesis *)
	synthesis_type	: synthesis_type;
	(* Property *)
	property		: property;
	(* Projection of the result *)
	projection		: projection;
}


