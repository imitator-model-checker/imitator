(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: ommon definitions for linear terms and constraints (interface to PPL)
 * 
 * File contributors : Étienne André
 * Created           : 2010/03/04
 * Last modified     : 2017/05/02
 *
 ************************************************************)


(************************************************************)
(* External modules *)
(************************************************************)

module Ppl = Ppl_ocaml
open Ppl

open Gmp.Z.Infixes



(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Statistics


(************************************************************)
(************************************************************)
(* Exceptions *)
(************************************************************)
(************************************************************)
(* Raised when a linear_term is not a clock guard, i.e., of the form x ~ plterm *)
exception Not_a_clock_guard



(************************************************************)
(* CONSTANTS *)
(************************************************************)
(** Check or not the number of dimensions of new polyhedra (not doing it may save around 0,5% of computation time, and no error ever occurred) *)
let check_assert_dimensions = true


(************************************************************)
(* Statistics for the use of PPL *)
(************************************************************)
(*let ppl_nb_space_dimension = ref 0
	let ppl_t_space_dimension = ref 0.0*)
	let ppl_tcounter_space_dimension = create_hybrid_counter_and_register "space_dimension" PPL_counter Verbose_low

(*let ppl_nb_normalize_linear_term = ref 0
	let ppl_t_normalize_linear_term = ref 0.0*)
	let ppl_tcounter_normalize_linear_term = create_hybrid_counter_and_register "normalize_linear_term" PPL_counter Verbose_low

(*let ppl_nb_true_constraint = ref 0
	let ppl_t_true_constraint = ref 0.0*)
	let ppl_tcounter_true_constraint = create_hybrid_counter_and_register "true_constraint" PPL_counter Verbose_low

(*let ppl_nb_false_constraint = ref 0
	let ppl_t_false_constraint = ref 0.0*)
	let ppl_tcounter_false_constraint = create_hybrid_counter_and_register "false_constraint" PPL_counter Verbose_low

(*let ppl_nb_is_true = ref 0
	let ppl_t_is_true = ref 0.0*)
	let ppl_tcounter_is_true = create_hybrid_counter_and_register "is_true" PPL_counter Verbose_low

(*let ppl_nb_is_false = ref 0
	let ppl_t_is_false = ref 0.0*)
	let ppl_tcounter_is_false = create_hybrid_counter_and_register "is_false" PPL_counter Verbose_low

(*let ppl_nb_is_equal = ref 0
	let ppl_t_is_equal = ref 0.0*)
	let ppl_tcounter_is_equal = create_hybrid_counter_and_register "is_equal" PPL_counter Verbose_low

(*let ppl_nb_contains = ref 0
	let ppl_t_contains = ref 0.0*)
	let ppl_tcounter_contains = create_hybrid_counter_and_register "contains" PPL_counter Verbose_low


(*let ppl_nb_contains_integer_point = ref 0
	let ppl_t_contains_integer_point = ref 0.0*)
	let ppl_tcounter_contains_integer_point = create_hybrid_counter_and_register "contains_integer_point" PPL_counter Verbose_low


(*let ppl_nb_get_constraints = ref 0
	let ppl_t_get_constraints = ref 0.0*)
	let ppl_tcounter_get_inequalities = create_hybrid_counter_and_register "get_inequalities" PPL_counter Verbose_low
	
(*let ppl_nb_get_generators = ref 0
	let ppl_t_get_generators = ref 0.0*)
	let ppl_tcounter_get_generators = create_hybrid_counter_and_register "get_generators" PPL_counter Verbose_low
	

let ppl_nb_add_constraints = ref 0
(*	let ppl_t_add_constraints = ref 0.0*)
	let ppl_tcounter_add_constraints = create_hybrid_counter_and_register "add_constraints" PPL_counter Verbose_low

	let ppl_tcounter_add_space_dimensions_and_project = create_hybrid_counter_and_register "add_space_dimensions_and_project" PPL_counter Verbose_low

	let ppl_tcounter_remove_higher_dimensions = create_hybrid_counter_and_register "remove_higher_space_dimensions" PPL_counter Verbose_low
	

	let ppl_tcounter_constrains = create_hybrid_counter_and_register "constrains" PPL_counter Verbose_low

	let ppl_tcounter_bounds_from_above = create_hybrid_counter_and_register "bounds_from_above" PPL_counter Verbose_low
	


(*let ppl_nb_hull = ref 0
	let ppl_t_hull = ref 0.0
	let ppl_tcounter_hull_assign = create_hybrid_counter_and_register "hull_assign" PPL_counter Verbose_low*)

(*let ppl_nb_hull_assign_if_exact = ref 0
	let ppl_t_hull_assign_if_exact = ref 0.0*)
	let ppl_tcounter_hull_assign_if_exact = create_hybrid_counter_and_register "hull_assign_if_exact" PPL_counter Verbose_low

(*let ppl_nb_hull_assign_if_exact_true = ref 0
	let ppl_t_hull_assign_if_exact_true = ref 0.0
let ppl_nb_hull_assign_if_exact_false = ref 0
	let ppl_t_hull_assign_if_exact_false = ref 0.0*)

(*let ppl_nb_difference = ref 0
	let ppl_t_difference = ref 0.0
	let ppl_tcounter_difference_assign = create_hybrid_counter_and_register "difference_assign" PPL_counter Verbose_low*)

	
(*let ppl_nb_intersection_assign = ref 0
	let ppl_t_intersection_assign = ref 0.0*)
	let ppl_tcounter_intersection_assign = create_hybrid_counter_and_register "intersection_assign" PPL_counter Verbose_low

(*let ppl_nb_unconstrain = ref 0
	let ppl_t_unconstrain = ref 0.0*)
	let ppl_tcounter_unconstrain = create_hybrid_counter_and_register "unconstrain" PPL_counter Verbose_low
	


(*let ppl_nb_map = ref 0
	let ppl_t_map = ref 0.0*)
	let ppl_tcounter_map_space_dimensions = create_hybrid_counter_and_register "map_space_dimensions" PPL_counter Verbose_low

(*let ppl_nb_preimage = ref 0
	let ppl_t_preimage = ref 0*)

(*let ppl_nb_remove_dim = ref 0
	let ppl_t_remove_dim = ref 0.0*)
	let ppl_tcounter_remove_space_dimensions = create_hybrid_counter_and_register "remove_space_dimensions" PPL_counter Verbose_low

	
(*let ppl_nb_elapse = ref 0
	let ppl_t_elapse = ref 0.0*)
	let ppl_time_elapse_assign = create_hybrid_counter_and_register "time_elapse_assign" PPL_counter Verbose_low


(*let ppl_nb_copy_polyhedron = ref 0
	let ppl_t_copy_polyhedron = ref 0.0*)
	let ppl_tcounter_copy = create_hybrid_counter_and_register "NNC_Polyhedron_from_NNC_Polyhedron" PPL_counter Verbose_low

	let ppl_nncc_tcounter_space_dimension = create_hybrid_counter_and_register "nncc_tcounter_space_dimension" PPL_counter Verbose_low

	let ppl_nncc_false_constraint = create_hybrid_counter_and_register "nncc_false_constraint" PPL_counter Verbose_low

	let ppl_nncc_true_constraint = create_hybrid_counter_and_register "nncc_true_constraint" PPL_counter Verbose_low

	let ppl_nncc_from_poly = create_hybrid_counter_and_register "nncc_from_poly" PPL_counter Verbose_low

	let ppl_nncc_copy = create_hybrid_counter_and_register "nncc_copy" PPL_counter Verbose_low

	let ppl_nncc_begin_iterator = create_hybrid_counter_and_register "nncc_begin_iterator" PPL_counter Verbose_low

	let ppl_nncc_end_iterator = create_hybrid_counter_and_register "nncc_end_iterator" PPL_counter Verbose_low

	let ppl_nncc_equals_iterator = create_hybrid_counter_and_register "nncc_equals_iterator" PPL_counter Verbose_low

	let ppl_nncc_increment_iterator = create_hybrid_counter_and_register "nncc_increment_iterator" PPL_counter Verbose_low

	let ppl_nncc_get_disjunct = create_hybrid_counter_and_register "nncc_get_disjunct" PPL_counter Verbose_low
	
	(* Counter for the higher-level function implemented here *)
	let ppl_nncc_get_disjuncts = create_hybrid_counter_and_register "nncc_get_disjuncts" PPL_counter Verbose_low

	let ppl_nncc_is_empty = create_hybrid_counter_and_register "nncc_is_empty" PPL_counter Verbose_low
	
	let ppl_nncc_is_universe = create_hybrid_counter_and_register "nncc_is_universe" PPL_counter Verbose_low
	
	let ppl_nncc_geometrically_covers = create_hybrid_counter_and_register "nncc_geometrically_covers" PPL_counter Verbose_low
	
	let ppl_nncc_geometrically_equals = create_hybrid_counter_and_register "nncc_geometrically_equals" PPL_counter Verbose_low
	
	let ppl_nncc_pairwise_reduce = create_hybrid_counter_and_register "nncc_pairwise_reduce" PPL_counter Verbose_low
	
	let ppl_nncc_omega_reduce = create_hybrid_counter_and_register "nncc_omega_reduce" PPL_counter Verbose_low

	let ppl_nncc_add_constraints = create_hybrid_counter_and_register "nncc_add_constraints" PPL_counter Verbose_low

	let ppl_nncc_add_disjunct = create_hybrid_counter_and_register "nncc_add_disjunct" PPL_counter Verbose_low

	let ppl_nncc_difference_assign = create_hybrid_counter_and_register "nncc_difference_assign" PPL_counter Verbose_low

	let ppl_nncc_remove_higher_space_dimensions = create_hybrid_counter_and_register "nncc_remove_higher_space_dimensions" PPL_counter Verbose_low


(* Other counters *)
	let tcounter_pi0_compatibility = create_hybrid_counter_and_register "pi0-compatibility" States_counter Verbose_low

(************************************************************)
(* TYPES *)
(************************************************************)

type variable = int
type coef = NumConst.t

(*type linear_term = Linexpr0.t*)

(* For legacy reasons (rational coefficients in input),      *)
(* the linear_term is a generalization of the corresponding  *)
(* PPL data structure Ppl.linear_expression, using rationals *)
(* instead of integers. *)
(*** WARNING: probably useless construction (by Ulrich Kuehne, around 2010) ***)
type linear_term =
	  Var of variable
	| Coef of coef
	| Pl of linear_term * linear_term
	| Mi of linear_term * linear_term
	| Ti of coef * linear_term

type p_linear_term = linear_term
type px_linear_term = linear_term
type pxd_linear_term = linear_term


(*** WARNING: probably useless construction (by Ulrich) ***)
type op =
	| Op_g
	| Op_ge
	| Op_eq
	| Op_le
	| Op_l

(** Reverse an operator: <= becomes >= and conversely. < becomes > and conversely. = remains =. *)
let reverse_op = function 
	| Op_g		-> Op_l
	| Op_ge		-> Op_le
	| Op_eq		-> Op_eq
	| Op_le		-> Op_ge
	| Op_l		-> Op_g


type linear_inequality = Ppl.linear_constraint
type p_linear_inequality = linear_inequality
type px_linear_inequality = linear_inequality
type pxd_linear_inequality = linear_inequality


type linear_constraint = Ppl.polyhedron

(** Convex constraint (polyhedron) on the parameters *)
type p_linear_constraint = linear_constraint

(** Convex constraint (polyhedron) on the parameters and clocks *)
type px_linear_constraint = linear_constraint

(** Convex constraint (polyhedron) on the discrete variables *)
type d_linear_constraint = linear_constraint

(** Convex constraint (polyhedron) on the parameters, clocks and discrete *)
type pxd_linear_constraint = linear_constraint


(* In order to convert a linear_term (with rational coefficients) *)
(* to the corresponding PPL data structure, it is normalized such *)
(* that the only non-rational coefficient is outside the term:    *)
(* p/q * ( ax + by + c ) *)
let normalize_linear_term (lt : linear_term) : (Ppl.linear_expression * NumConst.t) =
	(* Increment discrete counter *)
	ppl_tcounter_normalize_linear_term#increment;
	
	(* Start continuous counter *)
	ppl_tcounter_normalize_linear_term#start;
	
	let rec normalize_linear_term_rec lt =
(*	(*	(* Statistics *)*)
	(*** TODO ***)
		ppl_nb_normalize_linear_term := !ppl_nb_normalize_linear_term + 1;*)

		let result =
		match lt with
			| Var v -> Variable v, NumConst.one
			| Coef c -> (
					let p = NumConst.get_num c in
					let q = NumConst.get_den c in
					Coefficient p, NumConst.numconst_of_zfrac Gmp.Z.one q )
			| Pl (lterm, rterm) -> (
					let lterm_norm, fl = normalize_linear_term_rec lterm in
					let rterm_norm, fr = normalize_linear_term_rec rterm in
					let pl = NumConst.get_num fl in
					let ql = NumConst.get_den fl in
					let pr = NumConst.get_num fr in
					let qr = NumConst.get_den fr in
					(Plus (Times (pl *! qr, lterm_norm), (Times (pr *! ql, rterm_norm)))),
					NumConst.numconst_of_zfrac Gmp.Z.one (ql *! qr))
			| Mi (lterm, rterm) -> (
					let lterm_norm, fl = normalize_linear_term_rec lterm in
					let rterm_norm, fr = normalize_linear_term_rec rterm in
					let pl = NumConst.get_num fl in
					let ql = NumConst.get_den fl in
					let pr = NumConst.get_num fr in
					let qr = NumConst.get_den fr in
					(Minus (Times (pl *! qr, lterm_norm), (Times (pr *! ql, rterm_norm)))),
					NumConst.numconst_of_zfrac Gmp.Z.one (ql *! qr))
			| Ti (fac, term) -> (
					let term_norm, r = normalize_linear_term_rec term in
					let p = NumConst.get_num fac in
					let q = NumConst.get_den fac in
					term_norm, NumConst.mul r (NumConst.numconst_of_zfrac p q))
		in
	(* Return result *)
		result
	in
	
	let result = normalize_linear_term_rec lt in

	(* Stop continuous counter *)
	ppl_tcounter_normalize_linear_term#stop;
	
	(* Return result *)
	result
	


(** Add on for TA2CLP *)
let string_of_var names variable =
	"V_" ^ (names variable)


	
(***********************************************coef*************)
(************************************************************)
(** Global variables *)
(************************************************************)
(************************************************************)

(* Dimensions by type *)
let nb_parameters	= ref 0
let nb_clocks		= ref 0
let nb_discrete		= ref 0
(* Total numbers of dimensions *)
let p_dim			= ref 0
let px_dim			= ref 0
let pxd_dim			= ref 0



(************************************************************)
(************************************************************)
(* PPL-independent function *)
(************************************************************)
(************************************************************)
(*** WARNING: this strongly relies on the fact that the parameters are the first dimensions (followed by clocks and then discrete) ***)
(*** NOTE: would be smarter to compute this list only once, when the dimensions have been initialized ***)
(* let nonparameters () = list_of_interval !nb_parameters (!pxd_dim - 1) *)
let clocks () = list_of_interval !nb_parameters (!px_dim - 1)


(************************************************************)
(************************************************************)
(* Encapsulation of PPL functions *)
(************************************************************)
(************************************************************)

(*** NOTE: "ippl" stands for "interface to PPL" ***)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Generic and polymorphic function to encapsulate a PPL function with a counter:
 * 1) start the counter
 * 2) call the PPL function f (that must be of type unit -> _ )
 * 3) stop the counter
 * 4) return the result
 *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let ippl_generic f counter =
	(* Increment discrete counter *)
	counter#increment;
	
	(* Start time counter *)
	counter#start;
	
	(* Actual call to PPL *)
	let result = f() in
	
	(* Stop continuous counter *)
	counter#stop;
	
	(* Return result *)
	result


let ippl_space_dimension x =
	ippl_generic (fun () -> ppl_Polyhedron_space_dimension x) ppl_tcounter_space_dimension

let ippl_add_constraints x =
	ippl_generic (fun () -> ppl_Polyhedron_add_constraints x) ppl_tcounter_add_constraints

(* Return the list of inequalities that build the polyhedron (interface to PPL) *)
let ippl_get_inequalities x : linear_inequality list =
	ippl_generic (fun () -> ppl_Polyhedron_get_constraints x) ppl_tcounter_get_inequalities

let ippl_get_generators poly =
	ippl_generic (fun () -> ppl_Polyhedron_get_generators poly) ppl_tcounter_get_generators

let ippl_intersection_assign x =
	ippl_generic (fun () -> ppl_Polyhedron_intersection_assign x) ppl_tcounter_intersection_assign

let ippl_remove_dim poly remove =
	ippl_generic (fun () -> ppl_Polyhedron_remove_space_dimensions poly remove) ppl_tcounter_remove_space_dimensions

(** Create a false constraint *)
let ippl_false_constraint nb_dimensions =
	ippl_generic (fun () -> ppl_new_NNC_Polyhedron_from_space_dimension nb_dimensions Empty) ppl_tcounter_false_constraint


let ippl_true_constraint nb_dimensions = 
	ippl_generic (fun () -> ppl_new_NNC_Polyhedron_from_space_dimension nb_dimensions Universe) ppl_tcounter_true_constraint


(** Check if a constraint is false *)
let ippl_is_false c =
	ippl_generic (fun () -> ppl_Polyhedron_is_empty c) ppl_tcounter_is_false


(** Check if a constraint is true *)
let ippl_is_true c =
	ippl_generic (fun () -> ppl_Polyhedron_is_universe c) ppl_tcounter_is_true


(** Check if 2 constraints are equal *)
let ippl_is_equal c1 c2 =
	ippl_generic (fun () -> ppl_Polyhedron_equals_Polyhedron c1 c2) ppl_tcounter_is_equal


(** Check if a constraint is included in another one *)
let ippl_is_leq x y =
	ippl_generic (fun () -> ppl_Polyhedron_contains_Polyhedron y x) ppl_tcounter_contains


(** Check if a constraint contains an integer point *)
let ippl_contains_integer_point c =
	ippl_generic (fun () -> ppl_Polyhedron_contains_integer_point c) ppl_tcounter_contains_integer_point

(** Return true if the variable is constrained in a linear_constraint *)
let ippl_is_constrained =
	ippl_generic (fun () -> ppl_Polyhedron_constrains) ppl_tcounter_constrains
	
(** Return true if the variable is constrained from above by a linear_expression *)
let ippl_bounds_from_above =
	ippl_generic (fun () -> ppl_Polyhedron_bounds_from_above) ppl_tcounter_bounds_from_above
	

let ippl_copy_linear_constraint linear_constraint =
	ippl_generic (fun () -> ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint) ppl_tcounter_copy

(*
(** Perform the hull (version with side effect) *)
let ippl_hull_assign linear_constraint1 linear_constraint2 =
	ippl_generic (fun () -> ppl_Polyhedron_poly_hull_assign linear_constraint1 linear_constraint2) ppl_tcounter_hull_assign
*)

(** Perform the hull if the result is exact (version with side effect) *)
let ippl_hull_assign_if_exact linear_constraint1 linear_constraint2 =
	ippl_generic (fun () -> ppl_Polyhedron_poly_hull_assign_if_exact linear_constraint1 linear_constraint2) ppl_tcounter_hull_assign_if_exact
(*** TODO: also count if false / if true ***)
(*
	let hull_assign_if_exact linear_constraint1 linear_constraint2 =
	
	let start = Unix.gettimeofday() in

	(* Statistics *)
	ppl_nb_hull_assign_if_exact := !ppl_nb_hull_assign_if_exact + 1;
	
	(* Actual call to PPL *)
	let result =  in
	
	(* Statistics *)
	ppl_t_hull_assign_if_exact := !ppl_t_hull_assign_if_exact +. (Unix.gettimeofday() -. start);
	if result then(
		ppl_nb_hull_assign_if_exact_true := !ppl_nb_hull_assign_if_exact_true + 1;
		ppl_t_hull_assign_if_exact_true := !ppl_t_hull_assign_if_exact_true +. (Unix.gettimeofday() -. start);
	)else(
		ppl_nb_hull_assign_if_exact_false := !ppl_nb_hull_assign_if_exact_false + 1;
		ppl_t_hull_assign_if_exact_false := !ppl_t_hull_assign_if_exact_false +. (Unix.gettimeofday() -. start);
	);
	
	(* Return result *)
	result*)

(*(** Perform difference (version with side effect) *)
let ippl_difference_assign linear_constraint1 linear_constraint2 =
	ippl_generic (fun () -> ppl_Polyhedron_poly_difference_assign linear_constraint1 linear_constraint2) ppl_tcounter_difference_assign*)

(* Unconstrain, i.e., remove dimensions using variable elimination *)
let ippl_unconstrain linear_constraint variables =
	ippl_generic (fun () -> ppl_Polyhedron_unconstrain_space_dimensions linear_constraint variables) ppl_tcounter_unconstrain


(** Add nb_dimensions to a linear_constraint *)
let ippl_add_dimensions nb_dimensions linear_constraint =
	ippl_generic (fun () -> ppl_Polyhedron_add_space_dimensions_and_project linear_constraint nb_dimensions) ppl_tcounter_add_space_dimensions_and_project
	
(** Remove dimensions beyond 'new_dimensions' *)
let ippl_remove_higher_dimensions linear_constraint new_dimensions =
	ippl_generic (fun () -> ppl_Polyhedron_remove_higher_space_dimensions linear_constraint new_dimensions) ppl_tcounter_remove_higher_dimensions
	
(** Rename variables *)
let ippl_map_space_dimensions linear_constraint list_of_pairs =
	ippl_generic (fun () -> ppl_Polyhedron_map_space_dimensions linear_constraint list_of_pairs) ppl_tcounter_map_space_dimensions

(** Time elapsing *)
let ippl_time_elapse_assign linear_constraint linear_constraint_time =
	ippl_generic (fun () -> ppl_Polyhedron_time_elapse_assign linear_constraint linear_constraint_time) ppl_time_elapse_assign


(*------------------------------------------------------------*)
(* Pointset powerset NNC polyhedra *)
(*------------------------------------------------------------*)

let ippl_nncc_space_dimension c =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_space_dimension c) ppl_nncc_tcounter_space_dimension

(** Create a false non-necessarily convex constraint *)
let ippl_nncc_false_constraint nb_dimensions =
	ippl_generic (fun () -> ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension nb_dimensions Empty) ppl_nncc_false_constraint

(** Create a true non-necessarily convex constraint *)
let ippl_nncc_true_constraint nb_dimensions =
	ippl_generic (fun () -> ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension nb_dimensions Universe) ppl_nncc_true_constraint

(** Create a new p_nnconvex_constraint from a linear_constraint *)
let ippl_nncc_from_poly polyhedron =
	ippl_generic (fun () -> ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron polyhedron) ppl_nncc_from_poly

(** Create a true non-necessarily convex constraint *)
let ippl_nncc_copy nnconvex_constraint =
	ippl_generic (fun () -> ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron nnconvex_constraint ) ppl_nncc_copy

(** Iterators *)

let ippl_nncc_begin_iterator nnconvex_constraint =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator nnconvex_constraint) ppl_nncc_begin_iterator

let ippl_nncc_end_iterator nnconvex_constraint =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator nnconvex_constraint) ppl_nncc_end_iterator

let ippl_nncc_equals_iterator iterator1 iterator2 =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator iterator1 iterator2) ppl_nncc_equals_iterator

let ippl_nncc_increment_iterator iterator =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator iterator) ppl_nncc_increment_iterator

let ippl_nncc_get_disjunct iterator =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct iterator) ppl_nncc_get_disjunct


(** Check if a nnconvex_constraint is false *)
let ippl_nncc_is_empty nnconvex_constraint =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_is_empty nnconvex_constraint) ppl_nncc_is_empty

(** Check if a nnconvex_constraint is true *)
let ippl_nncc_is_universe nnconvex_constraint =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_is_universe nnconvex_constraint) ppl_nncc_is_universe

let ippl_nncc_geometrically_covers nnconvex_constraint nnconvex_constraint' =
	ippl_generic (fun () ->
		(*** NOTE: ppl_Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron is NOT the right function, as it checks whether each disjunct of p is contained in a disjunct of p'. ***)
		ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron nnconvex_constraint nnconvex_constraint') ppl_nncc_geometrically_covers

let ippl_nncc_geometrically_equals nnconvex_constraint nnconvex_constraint' =
	(*** NOTE: ppl_Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron is NOT the right function ***)
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron nnconvex_constraint nnconvex_constraint') ppl_nncc_geometrically_equals

let ippl_nncc_pairwise_reduce nnconvex_constraint =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_pairwise_reduce nnconvex_constraint) ppl_nncc_pairwise_reduce

let ippl_nncc_omega_reduce nnconvex_constraint =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_omega_reduce nnconvex_constraint) ppl_nncc_omega_reduce
	
let ippl_nncc_add_constraints nnconvex_constraint constraint_system =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_add_constraints nnconvex_constraint constraint_system) ppl_nncc_add_constraints

let ippl_nncc_add_disjunct nnconvex_constraint p_linear_constraint=
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_add_disjunct nnconvex_constraint p_linear_constraint) ppl_nncc_add_disjunct

let ippl_nncc_difference_assign nnconvex_constraint nnconvex_constraint' =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_difference_assign nnconvex_constraint nnconvex_constraint') ppl_nncc_difference_assign

let ippl_nncc_remove_higher_space_dimensions nnconvex_constraint new_dimensions =
	ippl_generic (fun () -> ppl_Pointset_Powerset_NNC_Polyhedron_remove_higher_space_dimensions nnconvex_constraint new_dimensions) ppl_nncc_remove_higher_space_dimensions


(*** TODO: more PPL interfaces ***)


(************************************************************)
(************************************************************)
(* Useful Functions *)
(************************************************************)
(************************************************************)

(** check the dimensionality of a polyhedron *)
let assert_dimensions nb_dimensions poly =
	
(* 	(*** NOTE: disabled since check_assert_dimensions is false ***) *)
	
	if check_assert_dimensions then(
		let ndim = ippl_space_dimension poly in
		if ndim <> nb_dimensions then (
			print_error ("Polyhedron does not have the expected number of dimensions (" ^ (string_of_int ndim) ^ " / " ^ (string_of_int nb_dimensions) ^ ")");
			raise (InternalError "Inconsistent polyhedron found")
		)
	)

(** check the dimensionality of a NNCC polyhedron *)
let nncc_assert_dimensions nb_dimensions nncc =
	if check_assert_dimensions then(
		let ndim = ippl_nncc_space_dimension nncc in
		if ndim <> nb_dimensions then (
			print_error ("NCC polyhedron does not have the expected number of dimensions (" ^ (string_of_int ndim) ^ " / " ^ (string_of_int nb_dimensions) ^ ")");
			raise (InternalError "Inconsistent polyhedron found")
		)
	)




(************************************************************)
(************************************************************)
(** {2 Linear terms} *)
(************************************************************)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Create a linear term from its list of members and its constant coefficient *)
let make_linear_term members coef =
	List.fold_left (fun term head ->
		let (c, v) = head in 
			if c = NumConst.one then
				Pl (Var v, term)
			else
				Pl ((Ti (c, Var v), term))
	)	(Coef coef) members


let make_p_linear_term = make_linear_term
let make_px_linear_term = make_linear_term
let make_pxd_linear_term = make_linear_term



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Modification functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Add two linear terms *)
let add_linear_terms lt1 lt2 =
	Pl (lt1, lt2)

let add_pxd_linear_terms = add_linear_terms


(** Substract two linear terms *)
let sub_linear_terms lt1 lt2 =
	Mi (lt1, lt2)


let sub_p_linear_terms = sub_linear_terms
let sub_px_linear_terms = sub_linear_terms
let sub_pxd_linear_terms = sub_linear_terms


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Access functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*------------------------------------------------------------*)
(** Check whether a variable appears in a linear_term (with coeff <> 0) *)
(*------------------------------------------------------------*)
let rec variable_in_linear_term v = function
	| Variable variable -> v = variable
	| Coefficient _ -> false
	| Unary_Plus linear_expression -> variable_in_linear_term v linear_expression
	| Unary_Minus linear_expression -> variable_in_linear_term v linear_expression
	| Plus (linear_expression1, linear_expression2) ->
		variable_in_linear_term v linear_expression1
		|| variable_in_linear_term v linear_expression2
	| Minus (linear_expression1, linear_expression2) ->
		variable_in_linear_term v linear_expression1
		|| variable_in_linear_term v linear_expression2
	| Times (coeff, rterm) ->
		if Gmp.Z.equal coeff (Gmp.Z.zero) then false
		else (match rterm with
			| Variable variable -> v = variable
			| _ -> raise (InternalError ("In function 'variable_in_linear_term', pattern 'Times' was expected to be only used for coeff * variable."))
		)

(*------------------------------------------------------------*)
(** Check whether a variable appears exactly one time in a linear_term (with coeff <> 0): if yes, return Some i, where i is its coefficient; otherwise return None *)
(*------------------------------------------------------------*)

(* Intermediate, recursive function. nb_times_ref is an int ref. coeff_option is a NumConst.t ref option. minus_flag is a flag to check whether we are in some negative coefficient. *)

let rec get_variable_coef_in_linear_term_rec nb_times_ref coeff_option minus_flag v = function
	| Variable variable -> if v = variable then(
			nb_times_ref := !nb_times_ref + 1;
			coeff_option := Some (if minus_flag then NumConst.minus_one else NumConst.one);
		)
	| Coefficient _ -> ()
	| Unary_Plus linear_expression -> get_variable_coef_in_linear_term_rec nb_times_ref coeff_option minus_flag v linear_expression
	(* If minus: revert flag *)
	| Unary_Minus linear_expression -> get_variable_coef_in_linear_term_rec nb_times_ref coeff_option (not minus_flag) v linear_expression
	| Plus (linear_expression1, linear_expression2) ->
		get_variable_coef_in_linear_term_rec nb_times_ref coeff_option minus_flag v linear_expression1;
		get_variable_coef_in_linear_term_rec nb_times_ref coeff_option minus_flag v linear_expression2
	| Minus (linear_expression1, linear_expression2) ->
		get_variable_coef_in_linear_term_rec nb_times_ref coeff_option minus_flag v linear_expression1;
		get_variable_coef_in_linear_term_rec nb_times_ref coeff_option (not minus_flag) v linear_expression2;
	| Times (coeff, rterm) ->
		if Gmp.Z.equal coeff (Gmp.Z.zero) then ()
		else (match rterm with
			| Variable variable -> if v = variable then(
				nb_times_ref := !nb_times_ref + 1;
				let coef = NumConst.numconst_of_mpz coeff in
				coeff_option := Some (if minus_flag then NumConst.neg coef else coef);
			)
			| _ -> raise (InternalError ("In function 'get_variable_coef_in_linear_term_rec', pattern 'Times' was expected to be only used for coeff * variable."))
		)

let get_variable_coef_in_linear_term v linear_term =
	let nb_times_ref = ref 0 in
	let coeff_option = ref None in
	(* Call the recursive function (the flag is initially false) *)
	get_variable_coef_in_linear_term_rec nb_times_ref coeff_option false v linear_term;
	(* If no occurrence: return none *)
	if !nb_times_ref = 0 then None else(
		(* If more than one occurrence: InternalError *)
		if !nb_times_ref > 1 then(
			raise (InternalError ("Variable found several times in a linear_term in 'get_variable_coef_in_linear_term'; that was assumed not to happen."));
		);
		(* Else: return the coefficient (and do a safety check that everything happened as expected...) *)
		match !coeff_option with
			| None -> raise (InternalError ("Impossible situation in 'get_variable_coef_in_linear_term': a coefficient was found > 0 times, but the coefficient was not saved."));
			| Some c -> Some c
	)

(*------------------------------------------------------------*)
(** Get the constant coefficient in a linear term *)
(*** NOTE: we assume there is at most one constant coefficient ***)
(*------------------------------------------------------------*)

exception Found_coef of NumConst.t

(* First a recursive function *)
let rec get_coefficient_in_linear_term_rec minus_flag = function
	| Variable variable -> ()
	| Coefficient c ->
		let numconst_coef = NumConst.numconst_of_mpz c in
		raise (Found_coef (if minus_flag then NumConst.neg numconst_coef else numconst_coef))
	| Unary_Plus linear_expression -> get_coefficient_in_linear_term_rec minus_flag linear_expression
	| Unary_Minus linear_expression -> get_coefficient_in_linear_term_rec (not minus_flag) linear_expression
	| Plus (linear_expression1, linear_expression2) ->
		get_coefficient_in_linear_term_rec minus_flag linear_expression1;
		get_coefficient_in_linear_term_rec minus_flag linear_expression2;
	| Minus (linear_expression1, linear_expression2) ->
		get_coefficient_in_linear_term_rec minus_flag linear_expression1;
		get_coefficient_in_linear_term_rec (not minus_flag) linear_expression2;
	| Times (coeff, rterm) ->
		if Gmp.Z.equal coeff (Gmp.Z.zero) then ()
		else (match rterm with
			| Variable variable -> ()
			| _ -> raise (InternalError ("In function 'get_coefficient_in_linear_term_rec', pattern 'Times' was expected to be only used for coeff * variable."))
		)

let get_coefficient_in_linear_term linear_term =
	try(
		get_coefficient_in_linear_term_rec false linear_term;
		(* If exception not raised: return 0 *)
		NumConst.zero
	) with Found_coef coef -> coef









(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Evaluation functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Evaluate a linear term with a function assigning a value to each variable. *)
let rec evaluate_linear_term valuation_function linear_term =
	match linear_term with
		| Coef c -> c
		| Var v -> (
			  try valuation_function v 
			  with _ -> raise(InternalError ("No value was found for variable " ^ (string_of_int v) ^ ", while trying to evaluate a linear term; this variable was probably not defined.")))
		| Pl (lterm, rterm) -> ( 
				let lval = evaluate_linear_term valuation_function rterm in
				let rval = evaluate_linear_term valuation_function lterm in
				NumConst.add lval rval)
		| Mi (lterm, rterm) -> (
				let lval = evaluate_linear_term valuation_function rterm in
				let rval = evaluate_linear_term valuation_function lterm in
				NumConst.sub lval rval)
		| Ti (fac, rterm) -> ( 
				let rval = evaluate_linear_term valuation_function rterm in
				NumConst.mul fac rval)

let evaluate_p_linear_term = evaluate_linear_term
let evaluate_pxd_linear_term = evaluate_linear_term



(** Evaluate a linear term (PPL) with a function assigning a value to each variable. *)
let rec evaluate_linear_term_ppl valuation_function linear_term =
	match linear_term with
		| Coefficient z -> NumConst.numconst_of_mpz z
		| Variable v -> (
			  try valuation_function v 
			  with _ -> raise(InternalError ("Error when evaluating variable " ^ (string_of_int v) ^ ", while trying to evaluate a linear term; this variable was probably not defined in the valuation function.")))
		| Unary_Plus t -> evaluate_linear_term_ppl valuation_function t
		| Unary_Minus t -> NumConst.neg (evaluate_linear_term_ppl valuation_function t)
		| Plus (lterm, rterm) -> (
				let lval = evaluate_linear_term_ppl valuation_function lterm in
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.add lval rval)
		| Minus (lterm, rterm) -> (
				let lval = evaluate_linear_term_ppl valuation_function lterm in
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.sub lval rval)
		| Times (z, rterm) -> ( 
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.mul (NumConst.numconst_of_mpz z) rval)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

let string_of_coef = NumConst.string_of_numconst
let string_of_constant = NumConst.string_of_numconst


(** Convert a linear term into a string *)	
let rec string_of_linear_term names linear_term =
	match linear_term with
		| Coef c -> string_of_coef c
		| Var v -> names v
		| Pl (lterm, rterm) -> (
			  let lstr = string_of_linear_term names lterm in
				let rstr = string_of_linear_term names rterm in
				lstr ^ " + " ^ rstr )
		| Mi (lterm, rterm) -> (
			  let lstr = string_of_linear_term names lterm in
				let rstr = string_of_linear_term names rterm in
				lstr ^ " - (" ^ rstr ^ ")" )
		| Ti (fac, rterm) -> (
				let fstr = string_of_coef fac in
				let tstr = string_of_linear_term names rterm in
				match rterm with
					| Coef _ -> fstr ^ "*" ^ tstr
					| Var  _ -> fstr ^ "*" ^ tstr
					| _ -> fstr ^ " * (" ^ tstr ^ ")" )

let string_of_p_linear_term = string_of_linear_term 
let string_of_pxd_linear_term = string_of_linear_term 

(** Convert a linear term (PPL) into a string *)
let rec string_of_linear_term_ppl names linear_term =
	match linear_term with
		| Coefficient z -> Gmp.Z.string_from z
		| Variable v -> names v
		| Unary_Plus t -> string_of_linear_term_ppl names t
		| Unary_Minus t -> (
				let str = string_of_linear_term_ppl names t in
				"-(" ^ str ^ ")")
		| Plus (lterm, rterm) -> (
			  let lstr = string_of_linear_term_ppl names lterm in
				let rstr = string_of_linear_term_ppl names rterm in
				lstr ^ " + " ^ rstr )
		| Minus (lterm, rterm) -> (
			  let lstr = string_of_linear_term_ppl names lterm in
				let rstr = string_of_linear_term_ppl names rterm in
				lstr ^ " - (" ^ rstr ^ ")" )
		| Times (z, rterm) -> (
				let fstr = Gmp.Z.string_from z in
				let tstr = string_of_linear_term_ppl names rterm in
				if (Gmp.Z.equal z (Gmp.Z.one)) then
					tstr
				else 
					match rterm with
						| Coefficient _ -> fstr ^ "*" ^ tstr
						| Variable    _ -> fstr ^ "*" ^ tstr
						| _ -> fstr ^ " * (" ^ tstr ^ ")" )
				

(************************************************************)
(************************************************************)
(** {2 Linear inequalities} *)
(************************************************************)
(************************************************************)

(*** TODO : minimize inequalities as soon as they have been created / changed ***)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Functions *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

let ppl_linear_expression_of_linear_term (linear_term : linear_term) : Ppl.linear_expression =
	let ppl_term, r = normalize_linear_term linear_term in
	let p = NumConst.get_num r in
	Times (p, ppl_term)


(** Create a linear inequality using a linear term and an operator *)
let make_linear_inequality linear_term op =
	let lin_term = ppl_linear_expression_of_linear_term linear_term in
	let zero_term = Coefficient Gmp.Z.zero in
	match op with
		| Op_g -> Greater_Than (lin_term, zero_term)
		| Op_ge -> Greater_Or_Equal (lin_term, zero_term)
		| Op_eq -> Equal (lin_term, zero_term)
		| Op_le -> Less_Or_Equal (lin_term, zero_term)
		| Op_l -> Less_Than (lin_term, zero_term)


let make_p_linear_inequality = make_linear_inequality
let make_px_linear_inequality = make_linear_inequality
let make_pxd_linear_inequality = make_linear_inequality

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** split a linear inequality into its two terms and the operator *)
let split_linear_inequality = function
	| Less_Than (lterm, rterm) -> lterm, rterm, Less_Than_RS
	| Less_Or_Equal (lterm, rterm) -> lterm, rterm, Less_Or_Equal_RS
	| Equal (lterm, rterm) -> lterm, rterm, Equal_RS
	| Greater_Than (lterm, rterm) -> lterm, rterm, Greater_Than_RS
	| Greater_Or_Equal (lterm, rterm) -> lterm, rterm, Greater_Or_Equal_RS
	
(** build a linear inequality from two terms and an operator *)
let build_linear_inequality lterm rterm op = 
	match op with
		| Less_Than_RS -> Less_Than (lterm, rterm)
		| Less_Or_Equal_RS -> Less_Or_Equal (lterm, rterm)
		| Equal_RS -> Equal (lterm, rterm)
		| Greater_Than_RS -> Greater_Than (lterm, rterm)
		| Greater_Or_Equal_RS -> Greater_Or_Equal (lterm, rterm)


(** evaluate a linear inequality for a given valuation *)
let evaluate_linear_inequality valuation_function linear_inequality =
	match linear_inequality with 
		| Less_Than (lterm, rterm) -> (
				let lval = evaluate_linear_term_ppl valuation_function lterm in
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.l lval rval )
		| Less_Or_Equal (lterm, rterm) -> (
				let lval = evaluate_linear_term_ppl valuation_function lterm in
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.le lval rval )
		| Equal (lterm, rterm) -> (
				let lval = evaluate_linear_term_ppl valuation_function lterm in
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.equal lval rval )
		| Greater_Than (lterm, rterm) -> (
				let lval = evaluate_linear_term_ppl valuation_function lterm in
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.g lval rval )
		| Greater_Or_Equal (lterm, rterm) -> (
				let lval = evaluate_linear_term_ppl valuation_function lterm in
				let rval = evaluate_linear_term_ppl valuation_function rterm in
				NumConst.ge lval rval )

(* Transform a strict inequality into a non-strict inequality *)
let strict_to_not_strict_inequality inequality =
	match inequality with
		| Less_Than (x,y) -> Less_Or_Equal (x,y)
		| Greater_Than (x,y) -> Greater_Or_Equal (x,y)
		|_ -> inequality




(*------------------------------------------------------------*)
(* Pi0-compatibility *)
(*------------------------------------------------------------*)

(** Check if a linear inequality is pi0-compatible *)
let is_pi0_compatible_inequality pi0 linear_inequality =
	evaluate_linear_inequality pi0 linear_inequality

(** Negate a linear inequality; for an equality, perform the pi0-compatible negation *)
let negate_wrt_pi0 pi0 linear_inequality = 
	match linear_inequality with
		| Less_Than (lterm, rterm) -> Greater_Or_Equal (lterm, rterm)
		| Less_Or_Equal (lterm, rterm) -> Greater_Than (lterm, rterm)
		| Greater_Than (lterm, rterm) -> Less_Or_Equal (lterm, rterm)
		| Greater_Or_Equal (lterm, rterm) -> Less_Than (lterm, rterm)
		| Equal (lterm, rterm) -> (
				(* perform the negation compatible with pi0 *)
				let lval = evaluate_linear_term_ppl pi0 lterm in
				let rval = evaluate_linear_term_ppl pi0 rterm in
				if NumConst.g lval rval then
					Greater_Than (lterm, rterm)
				else if NumConst.l lval rval then
					Less_Than (lterm, rterm)
				else(
					raise (InternalError "Trying to negate an equality already true w.r.t. pi0")
				)
			)


(* Negate an inequality ('=' is disallowed); raises InternalError if "=" is used *)
let negate_inequality = function
	| Less_Than (lterm, rterm) -> Greater_Or_Equal (lterm, rterm)
	| Less_Or_Equal (lterm, rterm) -> Greater_Than (lterm, rterm)
	| Greater_Than (lterm, rterm) -> Less_Or_Equal (lterm, rterm)
	| Greater_Or_Equal (lterm, rterm) -> Less_Than (lterm, rterm)
	| Equal (lterm, rterm) -> raise (InternalError "Trying to negate an equality in negate_inequality")



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)				   	


let is_zero_coef = function
	(*** NOTE: "=!" is the equality comparison, not the "!=" operator :) ***)
	| Coefficient c -> c =! Gmp.Z.zero
	| _ -> false


(** build a sum of two expressions; respects the case where one of the 
	  operands is zero *)
let compact_sum lexpr rexpr =
	if is_zero_coef lexpr then (
		rexpr
  ) else (
		if is_zero_coef rexpr then (
			lexpr
		) else (
			Plus (lexpr, rexpr)
		))

(** splits an expression into positive and negative part for pretty printing;
 	  an expression a-b is mapped to (a, b) *) 
let rec sign_split_expression = function
	| Coefficient c ->
		if c <! Gmp.Z.zero then (
			(Coefficient Gmp.Z.zero, Coefficient (Gmp.Z.neg c))
		) else (
			(Coefficient c, Coefficient Gmp.Z.zero)
		)
	| Variable v -> (Variable v, Coefficient Gmp.Z.zero)
	| Unary_Plus expr -> sign_split_expression expr
	| Unary_Minus expr ->
		let pos, neg = sign_split_expression expr in (neg, pos)
	| Plus (lexpr, rexpr) -> 
		let lpos, lneg = sign_split_expression lexpr in
		let rpos, rneg = sign_split_expression rexpr in
		let new_pos = compact_sum lpos rpos in 
		let new_neg = compact_sum lneg rneg in 
		(new_pos, new_neg)
	| Minus (lexpr, rexpr) -> 
		sign_split_expression (Plus (lexpr, Unary_Minus rexpr))
	| Times (c, expr) -> 
		let pos, neg = sign_split_expression expr in
		let invert = c <! Gmp.Z.zero in
		let new_c = if invert then Gmp.Z.neg c else c in
		if new_c =! Gmp.Z.one then (
			if invert then (neg, pos) else (pos, neg)
		) else (
			let new_pos = if is_zero_coef pos then Coefficient Gmp.Z.zero else Times (new_c, pos) in
			let new_neg = if is_zero_coef neg then Coefficient Gmp.Z.zero else Times (new_c, neg) in
			if invert then (new_neg, new_pos) else (new_pos, new_neg)
		)			


(** normalize an inequality for pretty printing; *)
(** the expressions are rearranged such that only posistive coefficients occur *)
let normalize_inequality ineq = 
	let lterm, rterm, op = split_linear_inequality ineq in
	let lpos, lneg = sign_split_expression lterm in
	let rpos, rneg = sign_split_expression rterm in
	let lnew = compact_sum lpos rneg in
	let rnew = compact_sum rpos lneg in
	build_linear_inequality lnew rnew op


(** Convert a linear inequality into a string *)
let string_of_linear_inequality names linear_inequality =
	let normal_ineq = normalize_inequality linear_inequality in
	let lterm, rterm, op = split_linear_inequality normal_ineq in
	let lstr = string_of_linear_term_ppl names lterm in
	let rstr = string_of_linear_term_ppl names rterm in	
	let opstr = match op with
		| Less_Than_RS -> " < "
		| Less_Or_Equal_RS -> " <= "
		| Equal_RS -> " = "
		| Greater_Or_Equal_RS -> " >= "
		| Greater_Than_RS -> " > "
	in
	lstr ^ opstr ^ rstr

let string_of_pxd_linear_inequality = string_of_linear_inequality
let string_of_p_linear_inequality = string_of_linear_inequality


(*------------------------------------------------------------*)
(** Convert a linear inequality into a clock guard (i.e. a triple clock, operator, parametric linear term); raises Not_a_clock_guard if the linear_inequality is not a proper clock guard x ~ plterm *)
(*------------------------------------------------------------*)
let clock_guard_of_linear_inequality linear_inequality =

	(*** NOTE: strongly relies on the fact that parameters indexes are from 0 to M-1, and clock indexes from M to M+H-1 ***)
	
(*	print_newline();
	print_string (string_of_pxd_linear_inequality (fun i -> "v_" ^ (string_of_int i)) linear_inequality);
	print_newline();*)

	(* First get both linear terms *)
	let lterm, rterm =
	match linear_inequality with
	| Less_Than (lterm, rterm) | Less_Or_Equal (lterm, rterm)  | Greater_Than (lterm, rterm)  | Greater_Or_Equal (lterm, rterm) | Equal (lterm, rterm) ->
		lterm, rterm
	in
	
	(* Compute lterm - rterm *)
	let linear_term = Minus (lterm, rterm) in
	
(*	print_newline();
	print_string (string_of_linear_term_ppl (fun i -> "v_" ^ (string_of_int i)) linear_term);
	print_newline();*)

	
	(* Variable to store the (necessarily unique) clock index *)
	let clock_index_option = ref None in

	(* Flag to remember whether the clock coefficient is 1 or -1 (None means not yet initialized) *)
	let positive_clock_option = ref None in
	
	(* Create an array to store the coefficient of all parameters *)
	let parameter_coefficients = Array.make !nb_parameters NumConst.zero in
	
	(*** WARNING: not efficient! for each variable, we go through the entire linear term, although it would be smarter to retrive all coefficients at once… ***)
	(*** TO OPTIMIZE ***)
	
	(* First iterate on clocks to check that exactly one clock is used *)
	for clock_index = !nb_parameters to !nb_parameters + !nb_clocks - 1 do
		(* Find the coefficient of the clock in the linear term *)
		let coeff_option = get_variable_coef_in_linear_term clock_index linear_term in
		match coeff_option with
		(* Clock not found *)
		| None -> ()
		(* Clock found *)
		| Some coeff ->
			(* If already found a non-null coeff for another clock before, raise an exception *)
			if !clock_index_option <> None then(
				raise Not_a_clock_guard;
			);
			(* If the coefficient is not 1 or -1, raise an exception *)
			if NumConst.neq coeff NumConst.one && NumConst.neq coeff NumConst.minus_one then(
				raise Not_a_clock_guard;
			);
			(* Otherwise, update the variables *)
			clock_index_option := Some (clock_index);
			if NumConst.equal coeff NumConst.one then(
				positive_clock_option := Some true;
			)else if NumConst.equal coeff NumConst.minus_one then(
				positive_clock_option := Some false;
			)else(
			(* Safety guard *)
				raise (InternalError("The clock coefficient must be either 1 or -1 at that point"))
			);
	done;
	
	(* Retrieve the (necessarily unique) clock index *)
	let clock_index =
	match !clock_index_option with
		| None -> raise Not_a_clock_guard;
		| Some index -> index
	in

	(* Second, iterate on discrete to check that none appear (otherwise not a well-formed guard) *)
	for discrete_index = !nb_parameters + !nb_clocks to !nb_parameters + !nb_clocks + !nb_discrete - 1 do
		(* Find the coefficient of the discrete in the linear term *)
		let coeff_option = get_variable_coef_in_linear_term discrete_index linear_term in
		match coeff_option with
		(* Variable not found *)
		| None -> ()
		(* Variable found *)
		| Some coeff -> raise Not_a_clock_guard;
	done;
	
	(* Third, iterate on parameters to retrieve their coefficients *)
	for parameter_index = 0 to !nb_parameters - 1 do
		(* Find the coefficient of the parameter in the linear term *)
		let coeff_option = get_variable_coef_in_linear_term parameter_index linear_term in
		match coeff_option with
		(* Variable not found *)
		| None -> ()
		(* Variable found: update array *)
		| Some coeff -> parameter_coefficients.(parameter_index) <- coeff;
	done;
	
	(* Retrieve the constant coefficient *)
	let coefficient = get_coefficient_in_linear_term linear_term in

	(* Gather a list of pairs (parameter_coef, parameter_index) *)
	(*** BADPROG: not tail-recursive... *)
	let members = ref [] in
	Array.iteri (fun parameter_index parameter_coef -> 
		(* If coefficient is not zero... *)
		if NumConst.neq parameter_coef NumConst.zero then(
			(* Add new pair to the list of members *)
			members := (parameter_coef, parameter_index) :: !members;
		);
	) parameter_coefficients;
	
	(* Reconstruct the parametric linear term *)
	let parametric_linear_term = make_linear_term !members coefficient in
	
	(* Negate it if needed: if the clock is NEGATIVE, it will be naturally moved to the other side, hence no need to change the sign of the plterm *)
	
	(*check this small code again*)
	
	let parametric_linear_term = if !positive_clock_option = Some true
		then Mi ((make_linear_term [] NumConst.zero) , parametric_linear_term)
		else parametric_linear_term 
	in
	
	
	(* Retrieve the operator *)
	let operator =
	match linear_inequality with
		| Less_Than _ -> if !positive_clock_option = Some true then Op_l else Op_g
		| Less_Or_Equal _ -> if !positive_clock_option = Some true then Op_le else Op_ge
		| Greater_Than _ -> if !positive_clock_option = Some true then Op_g else Op_l
		| Greater_Or_Equal _ -> if !positive_clock_option = Some true then Op_ge else Op_le
		| Equal _ -> Op_eq
	in

	(* Return the result *)
	(clock_index, operator, parametric_linear_term)



	
(*(** substitutes all variables in a linear term.
		The substitution is given as a function sub: var -> linear_term *)
let rec substitute_variables_in_term sub linear_term =
	match linear_term with		
		| Coefficient z -> Coefficient z
		| Variable v -> sub v
		| Unary_Plus t -> Unary_Plus t
		| Unary_Minus t -> Unary_Minus t
		| Plus (lterm, rterm) -> (
				Plus (substitute_variables_in_term sub lterm,
							substitute_variables_in_term sub rterm))
		| Minus (lterm, rterm) -> (
				Minus (substitute_variables_in_term sub lterm,
							 substitute_variables_in_term sub rterm))
		| Times (z, rterm) -> (
				Times (z, substitute_variables_in_term sub rterm))

		
(** substitutes all variables in a linear inequality *)
let substitute_variables sub linear_inequality =
	match linear_inequality with
		| Less_Than (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Less_Than (lsub, rsub))
		| Less_Or_Equal (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Less_Or_Equal (lsub, rsub))
		| Equal (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Equal (lsub, rsub))
		| Greater_Than (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Greater_Than (lsub, rsub))
		| Greater_Or_Equal (lterm, rterm) -> (
				let lsub = substitute_variables_in_term sub lterm in
				let rsub = substitute_variables_in_term sub rterm in
				Greater_Or_Equal (lsub, rsub))*)

(************************************************************)
(************************************************************)
(** {2 Linear Constraints} *)
(************************************************************)
(************************************************************)





(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Initialization} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Set the number of dimensions *)
let set_dimensions nb_p nb_c nb_d =
	nb_parameters	:= nb_p;
	nb_clocks 		:= nb_c;
	nb_discrete		:= nb_d;
	p_dim			:= nb_p;
	px_dim			:= nb_p + nb_c;
	pxd_dim			:= nb_p + nb_c + nb_d;
	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("\nDimensions set");
		print_message Verbose_high ("  nb_parameters := " ^ (string_of_int !nb_parameters));
		print_message Verbose_high ("  nb_clocks := " ^ (string_of_int !nb_clocks));
		print_message Verbose_high ("  nb_discrete := " ^ (string_of_int !nb_discrete));
		print_message Verbose_high ("  p_dim := " ^ (string_of_int !p_dim));
		print_message Verbose_high ("  px_dim := " ^ (string_of_int !px_dim));
		print_message Verbose_high ("  pxd_dim := " ^ (string_of_int !pxd_dim));
	);
	()


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Create a false constraint *)
let p_false_constraint () = ippl_false_constraint !p_dim
let pxd_false_constraint () = ippl_false_constraint !pxd_dim



(** Create a true constraint *)
let true_constraint nb_dimensions = ippl_true_constraint nb_dimensions
let p_true_constraint () = ippl_true_constraint !p_dim
let px_true_constraint () = ippl_true_constraint !px_dim
let pxd_true_constraint () = ippl_true_constraint !pxd_dim


(** Create a linear constraint from a list of linear inequalities *)
let make nb_dimensions inequalities = 
	let poly = true_constraint nb_dimensions in
	ippl_add_constraints poly inequalities;
	assert_dimensions nb_dimensions poly;
  poly


(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let make_p_constraint inequalities = make !p_dim inequalities
let make_px_constraint inequalities = make !px_dim inequalities
let make_pxd_constraint inequalities = make !pxd_dim inequalities


(** Create a linear constraint from a single point *)
(*** WARNING: non-robust (no check for variable existence) ***)
let constraint_of_point nb_dimensions (thepoint : (variable * coef) list) =
	let inequalities =
	List.map (fun (variable , value) ->
		(* Create linear inequality "variable = value" *)
		make_linear_inequality
			(* Create linear term "variable - value" *)
			(make_linear_term [NumConst.one, variable] (NumConst.neg value))
			Op_eq
	) thepoint
	in
	make nb_dimensions inequalities

(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let p_constraint_of_point v_c_list = constraint_of_point !p_dim v_c_list
let pxd_constraint_of_point v_c_list = constraint_of_point !pxd_dim v_c_list


(** "linear_constraint_of_clock_and_parameters x ~ d neg" will create a linear_constraint x ~ d, with "x" a clock, "~" in {>, >=, =}, "d" a PConstraint.linear_term, and "neg" indicates whether x and d should be kept in this direction or reversed (e.g., "x > p1 true" generates "x > p1" whereas "x >= p1+p2 false" generates "p1+p2 >= x" *)
let linear_constraint_of_clock_and_parameters nb_dimensions (x : variable) (op : op) (d : linear_term) (direction : bool) =
	(* Create a linear term made of x *)
	let lt_x = make_linear_term [NumConst.one, x] NumConst.zero in
	(* Handle order *)
	let lt =
		if direction
			then sub_linear_terms d lt_x
		else sub_linear_terms lt_x d
	in
	(* Create the constraint with the operator *)
	make nb_dimensions [make_linear_inequality lt op]

(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let px_linear_constraint_of_clock_and_parameters x = linear_constraint_of_clock_and_parameters !px_dim x
let pxd_linear_constraint_of_clock_and_parameters x = linear_constraint_of_clock_and_parameters !pxd_dim x


(** Create a constraint bounding all variables in the list to non-negative *)
let constraint_of_nonnegative_variables nb_dimensions variables = 
	(* First check that the variables are compatible with the dimensions *)
	List.iter (fun variable -> if variable >= nb_dimensions then raise (InternalError ("In function LinearConstraint.constraint_of_nonnegative_variables, trying to create a variable of dimension " ^ (string_of_int variable) ^ " for a polyhedron in " ^ (string_of_int nb_dimensions) ^ " dimension" ^ (s_of_int nb_dimensions) ^ ".") )) variables;
	
	let inequalities =
	List.map (fun variable ->
		(* Create linear inequality "variable >= 0" *)
		make_linear_inequality
			(* Create linear term "variable + 0" *)
			(make_linear_term [NumConst.one, variable] (NumConst.zero))
			Op_ge
	) variables
	in
	make nb_dimensions inequalities

(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let p_constraint_of_nonnegative_variables v = constraint_of_nonnegative_variables !p_dim v
let px_constraint_of_nonnegative_variables v = constraint_of_nonnegative_variables !px_dim v
let pxd_constraint_of_nonnegative_variables v = constraint_of_nonnegative_variables !pxd_dim v



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Check if a constraint is false *)
let is_false = ippl_is_false
let p_is_false = ippl_is_false
let px_is_false = ippl_is_false
let pxd_is_false = ippl_is_false


(** Check if a constraint is true *)
let is_true = ippl_is_true
let p_is_true = ippl_is_true
let pxd_is_true = ippl_is_true


(** Check if a constraint is satisfiable *)
let is_satisfiable c = not (is_false c)
let p_is_satisfiable = is_satisfiable
let px_is_satisfiable = is_satisfiable
let pxd_is_satisfiable = is_satisfiable


(** Check if 2 constraints are equal *)
let is_equal = ippl_is_equal
let p_is_equal = ippl_is_equal
let px_is_equal = ippl_is_equal
let pxd_is_equal = ippl_is_equal


(** Check if a constraint is included in another one *)
let is_leq = ippl_is_leq
let p_is_leq = ippl_is_leq
let px_is_leq = ippl_is_leq
let pxd_is_leq = ippl_is_leq


(** Check if a constraint contains an integer point *)
let contains_integer_point = ippl_contains_integer_point
let px_contains_integer_point = ippl_contains_integer_point


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Get the number of inequalities of a constraint *)
let p_nb_inequalities linear_constraint = 
	(* First check if true *)
	(*** NOTE: might be more costly than the general check; perhaps move after getting the list? ***)
	if p_is_true linear_constraint || p_is_false linear_constraint then 0
	else
	(* Get a list of linear inequalities *)
	let list_of_inequalities = ippl_get_inequalities linear_constraint in
	List.length list_of_inequalities

(* let p_nb_inequalities = nb_inequalities *)


(** Get the inequalities of a constraint *)
let pxd_get_inequalities = ippl_get_inequalities
let px_get_inequalities = ippl_get_inequalities
let p_get_inequalities = ippl_get_inequalities


(** Return true if the variable is constrained in a linear_constraint *)
let is_constrained = ippl_is_constrained
let pxd_is_constrained = ippl_is_constrained


(** Return the list of variables from l that are constrained in the constraint *)
(*** WARNING: no idea of the efficiency of this way of doing ***)
(* (but not crucial because only called for preprocessing) *)
let find_variables variables_list linear_constraint =
	List.filter (fun variable ->
		is_constrained linear_constraint variable
	) variables_list


let pxd_find_variables = find_variables



(*
let partition_lu_ineq variables (current_list_of_l, current_list_of_u) linear_inequality =
	(* Get the inequalities *)
	let inequalities = ippl_get_inequalities linear_constraint in
	List.fold_left
		(* 1st argument of fold_left: the function called on each linear_inequality *)
		(fun (current_list_of_l, current_list_of_u) linear_inequality -> 
			(current_list_of_l, current_list_of_u))
		(* 2nd argument of fold_left: initial list of l, initial list of u *)
		([], [])
		(* 3rd argument of fold_left: the list of inequalities *)
		inequalities*)




type l_u_type =
	| TypeLU_L
	| TypeLU_U
	| TypeLU_unknown

(* Given a list of variables V and a list of linear_constraint, partition the list V into variables appearing only as lower-bound in inequalities, and variables only appearing as upper-bounds in inequalities; raise Not_LU if some variables in V appear as both (or in equalities) *)
let partition_lu variables linear_constraints =

(*	(*** DEBUG ***)
	let stringp = fun var -> "p_" ^ (string_of_int var) in*)

	(* Create a hashtable v: l_u_type *)
	let status_lu = Hashtbl.create (List.length variables) in
	(* Fill table with TypeLU_unknown *)
	List.iter (fun variable ->
		Hashtbl.add status_lu variable TypeLU_unknown
	) variables;
	
	(* Function to update the hash table for one variable; the Bool lower_bound specifies whether the variable was met as a lower bound. Raise Not_LU if the new status is incompatible with the status stored in the hashtable *)
	let update_variable lower_bound variable =
		(* Only update if the variable belongs to the list of variables we search for *)
		if List.mem variable variables then (
			(* Get the current status of the variable *)
			let current_status = Hashtbl.find status_lu variable in
			(* If not yet met: just update the table *)
			if current_status = TypeLU_unknown then
				Hashtbl.replace status_lu variable (if lower_bound then TypeLU_L else TypeLU_U)
			else(
				(* If met as a lower bound but current status is upper bound *)
				if lower_bound && current_status = TypeLU_U
				||
				(* If met as an upper bound but current status is lower bound *)
				not lower_bound && current_status = TypeLU_L
				then raise Not_LU
			)
		)
	in
	
	(* Function to update the hash table for a linear_term; the Bool lower_side specifies whether the considered linear_term is on the lower side of an inequality *)
	let rec check_linear_term lower_side = function
		| Variable variable -> update_variable lower_side variable
		| Coefficient _ -> ()
		| Unary_Plus linear_expression -> check_linear_term lower_side linear_expression
		| Unary_Minus linear_expression -> check_linear_term (not lower_side) linear_expression
		| Plus (linear_expression1, linear_expression2) ->
			check_linear_term lower_side linear_expression1;
			check_linear_term lower_side linear_expression2
		| Minus (linear_expression1, linear_expression2) ->
			check_linear_term lower_side linear_expression1;
			check_linear_term (not lower_side) linear_expression2
		| Times (coeff, rterm) ->
			(* Coeff 0: equivalent to no variable *)
			if Gmp.Z.equal coeff (Gmp.Z.zero) then ()
			else (match rterm with
				| Variable variable ->
					update_variable
						(xor (coeff <! Gmp.Z.zero) lower_side)
						variable
				| _ -> raise (InternalError ("In function 'check_linear_term', pattern 'Times' was expected to be only used for coeff * variable."))
			)
	in
	
	(* FOR ALL CONSTRAINTS *)
	List.iter (fun linear_constraint ->
	
		(* Get the inequalities *)
		let inequalities = ippl_get_inequalities linear_constraint in
		
		(* FOR ALL INEQUALITIES IN THAT CONSTRAINT *)
		List.iter (function
			(* Case 1: equality --> check if any variable in 'variables' appears in it *)
			| Equal (lterm, rterm) -> 
				List.iter (fun variable -> 
					if variable_in_linear_term variable lterm || variable_in_linear_term variable rterm then raise Not_LU
				) variables

			(* Case 2a: < / <= --> find L/U variables and update the hash table *)
			| Less_Than (lterm, rterm)
			| Less_Or_Equal (lterm, rterm) ->
				check_linear_term true lterm;
				check_linear_term false rterm;
			
			(* Case 2b: > / >= --> find L/U variables and update the hash table *)
			| Greater_Or_Equal (lterm, rterm)
			| Greater_Than (lterm, rterm) ->
				check_linear_term false lterm;
				check_linear_term true rterm;
			
		) inequalities; (* end FOR ALL inequalities *)
	) linear_constraints; (* end FOR ALL constraints *)
	
	(* Extract L and U *)
	let l_variables, u_variables =
	Hashtbl.fold
		(* 1st argument of Hashtbl.fold: the function *)
		(fun variable status (current_l_variables, current_u_variables) ->
			if status = TypeLU_L then variable :: current_l_variables, current_u_variables
			else(
				if status = TypeLU_U then current_l_variables, variable :: current_u_variables
				else current_l_variables, current_u_variables
			)
		)
		(* 2nd argument of Hashtbl.fold: the hashtable *)
		status_lu
		(* 3rd argument of Hashtbl.fold: the initial value of l_variables, u_variables *)
		([], [])
	in
(*	(*** DEBUG ***)
	print_string ("\n*** lower-bound parameters {" ^ (string_of_list_of_string_with_sep ", " (List.map stringp l_variables)) ^ "}");
	(*** DEBUG ***)
	print_string ("\n*** upper-bound parameters {" ^ (string_of_list_of_string_with_sep ", " (List.map stringp u_variables)) ^ "}");*)
	(* Return both lists *)
	l_variables, u_variables



(*------------------------------------------------------------*)
(** Return the parametric linear term which is the upper bound of the clock x in a px_linear_constraint; return None if no upper bound *)
(*** NOTE: we asssume that all inequalities are of the form x \sim plt, and that a single inequality constrains x as an upper bound. Raises Not_a_clock_guard otherwise ***)
(*------------------------------------------------------------*)
exception Found_upper_bound of p_linear_term
let clock_upper_bound_in clock_index px_linear_constraint =
	let inequalities = px_get_inequalities px_linear_constraint in
	let p_linear_term_option =
	try(
		(* Iterate on all inequalities until x < plt is found *)
		List.iter (fun inequality -> 
			(* Transform to clock guard *)
			let clock_index', operator, parametric_linear_term = clock_guard_of_linear_inequality inequality in
			(* Only interested in our clock *)
			if clock_index' = clock_index then(
				(* Check the operator *)
				match operator with
					(* Case upper bound *)
					| Op_l | Op_le | Op_eq -> raise (Found_upper_bound parametric_linear_term)
					(* Otherwise not an upper bound *)
					| _ -> ()
			)
		) inequalities;
		(* If ending iterating without finding an upper bound: there is none *)
		None
	)
	with 
		Found_upper_bound parametric_linear_term -> Some parametric_linear_term
	in
	(* Return result *)
	p_linear_term_option


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** String for the false constraint *)
let string_of_false = "False"


(** String for the true constraint *)
let string_of_true = "True"


(** String for the intersection symbol *)
let string_of_intersection = "\n& "


(** Convert a linear constraint into a string *)
let string_of_linear_constraint names linear_constraint =


(*** 	TODO DEBUG HACK WARNING ***)
(*	let serialized = serialize_linear_constraint linear_constraint in
	print_warning(serialized);
	let unserialized = 
		try
			unserialize_linear_constraint serialized
		with Failure f -> raise (SerializationError("Failure while unserializing linear inequality '" ^ serialized ^ "'. Error: " ^ f))
	in
	print_string(string_of_bool( is_equal linear_constraint unserialized ));*)
(*** 	TODO DEBUG HACK WARNING ***)
	
	
	
	
	(* First check if true *)
	if is_true linear_constraint then string_of_true
	(* Then check if false *)
	else if is_false linear_constraint then string_of_false
	else
	(* Get a list of linear inequalities *)
	let list_of_inequalities = ippl_get_inequalities linear_constraint in
	" " ^
	(string_of_list_of_string_with_sep
		string_of_intersection
		(List.map (string_of_linear_inequality names) list_of_inequalities)
	)

let string_of_p_linear_constraint = string_of_linear_constraint
let string_of_px_linear_constraint = string_of_linear_constraint
let string_of_d_linear_constraint = string_of_linear_constraint
let string_of_pxd_linear_constraint = string_of_linear_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*------------------------------------------------------------*)
(* Copy *)
(*------------------------------------------------------------*)

let copy = ippl_copy_linear_constraint
let p_copy = ippl_copy_linear_constraint
let px_copy = ippl_copy_linear_constraint
let pxd_copy = ippl_copy_linear_constraint



(*------------------------------------------------------------*)
(* Intersection *)
(*------------------------------------------------------------*)

(** Perform the intersection of a linear constrain with a list of constraints (with side effect) *)
let intersection_assign nb_dimensions linear_constraint constrs =
(* 	try( *)
		List.iter (fun poly ->
			(* Perform the actual intersection *)
			ippl_intersection_assign linear_constraint poly;
			(* Check satisfiability *)
			(*** NOTE: this does not bring anything on the examples I tried -- on the contrary! ***)
(* 			if not (is_satisfiable linear_constraint) then raise Unsat_exception; *)
		) constrs;
		assert_dimensions nb_dimensions linear_constraint
	(* If false: stop *)
(* 	) with Unsat_exception -> () *)

let p_intersection_assign l c = intersection_assign !p_dim l c
let px_intersection_assign l c = intersection_assign !px_dim l c
let pxd_intersection_assign l c = intersection_assign !pxd_dim l c

let px_intersection_assign_p px_linear_constraint = function
	(* No constraint: nothing to do *)
	| [] -> ()
	(* Some p_linear_constraint's *)
	| first :: (rest : p_linear_constraint list) -> 
		(* Copy the first constraint *)
		let first_copy : p_linear_constraint = p_copy first in
		(* Intersect with others *)
		p_intersection_assign first_copy rest;
		(* Increase dimensions *)
		ippl_add_dimensions (!px_dim - !p_dim) first_copy;
		(* Intersect *)
		px_intersection_assign px_linear_constraint [first_copy]

	(*** NOTE: old version; less efficient????? (not formally tested) ***)
(*	
	(*** NOTE: we first need to increase the number of dimensions ***)
	(* Copy the intersected constraints *)
	let p_linear_constraint_list_copy = List.map p_copy p_linear_constraint_list in
	(* Increase dimensions *)
	(*** NOTE: 'ippl_add_dimensions nb' adds nb new dimensions ***)
	List.iter (ippl_add_dimensions (!px_dim - !p_dim)) p_linear_constraint_list_copy;
	(* Intersect *)
	px_intersection_assign px_linear_constraint p_linear_constraint_list_copy*)


(** Performs the intersection of a list of linear constraints *)
let intersection nb_dimensions linear_constraints =
	
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (
			"Entering 'LinearConstraint.intersection' with " ^ (string_of_int nb_dimensions) ^ " dimensions.
			The linear constraints to be intersected have " ^ (string_of_list_of_string_with_sep ", " (List.map (fun lc -> string_of_int (ippl_space_dimension lc)) linear_constraints )) ^ " dimensions."
	);

	let result_poly = true_constraint nb_dimensions in
	intersection_assign nb_dimensions result_poly linear_constraints;
	result_poly
(*	try(
		List.iter (fun poly ->
			if not (is_satisfiable poly) then raise Unsat_exception;
			intersection_assign result_poly poly
		) linear_constraints;
		assert_dimensions result_poly;
		result_poly
	) with Unsat_exception -> false_constraint ()*)

(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let p_intersection l = intersection !p_dim l
let px_intersection l = intersection !px_dim l
let pxd_intersection l = intersection !pxd_dim l

let pxd_intersection_with_d pxd_linear_constraint d_linear_constraint = intersection !pxd_dim [pxd_linear_constraint; d_linear_constraint]


(*------------------------------------------------------------*)
(* Difference *)
(*------------------------------------------------------------*)

(*** NOTE: unused function ***)
(** Perform difference (version with side effect) *)
(* let difference_assign = ippl_difference_assign *)


(*------------------------------------------------------------*)
(* Hull *)
(*------------------------------------------------------------*)

(** Perform the hull (version with side effect) *)
(* let hull_assign = ippl_hull_assign *)
	
let px_hull_assign_if_exact = ippl_hull_assign_if_exact


(*------------------------------------------------------------*)
(* Convex negation *)
(*------------------------------------------------------------*)
(** Assuming p_linear_constraint contains a single inequality, this function returns the negation of this inequality (in the form of a p_constraint). Raises InternalError if more than one inequality. *)
let negate_single_inequality_p_constraint p_linear_constraint =
	(* Retrieve the inequalities *)
	let inequalities = p_get_inequalities p_linear_constraint in
	(* Check *)
	if List.length inequalities <> 1 then(
		raise (InternalError("Exactly one inequality should be contained in negate_single_inequality_p_constraint"))
	);
	(* Get the (only) inequality *)
	let inequality = List.nth inequalities 0 in
	(* Negate it *)
	let negated_inequality = negate_inequality inequality in
	(* Reconstruct a linear constraint *)
	make_p_constraint [inequality]



(*------------------------------------------------------------*)
(* Variable elimination *)
(*------------------------------------------------------------*)

(** Eliminate a set of variables, side effects version *)
let hide_assign nb_dimensions variables linear_constraint =
	(* Print some information *)
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total (
			"Function 'LinearConstraint.hide_assign': hiding variables "
			^ (string_of_list_of_string_with_sep ", " (List.map string_of_int variables) )
			^ ". Number of dimensions: " ^ (string_of_int (ippl_space_dimension linear_constraint)) ^ "."
		);
	);
	
	(* Only hide a non-empty list *)
	if List.length variables > 0 then (
		ippl_unconstrain linear_constraint variables;
		assert_dimensions nb_dimensions linear_constraint
	)


(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let p_hide_assign v l = hide_assign !p_dim v l
let px_hide_assign v l = hide_assign !px_dim v l
let pxd_hide_assign v l = hide_assign !pxd_dim v l


(** Eliminate (using existential quantification) a set of variables in a linear constraint *)
let hide nb_dimensions variables linear_constraint =
	(* copy polyhedron, as PPL function has sideeffects *)
	let poly = copy linear_constraint in
	(* Call the function with side-effects *)
	hide_assign nb_dimensions variables poly;
	poly

(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let px_hide v l = hide !px_dim v l
let pxd_hide v l = hide !pxd_dim v l



(** Eliminate (using existential quantification) all non-parameters (clocks only, as it is a PX constraint) in a px_linear constraint *)
let px_hide_nonparameters_and_collapse px_linear_constraint = 
	let non_parameter_variables = clocks () in
	
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (
			"Function 'LinearConstraint.px_hide_nonparameters_and_collapse': hiding variables "
			^ (string_of_list_of_string_with_sep ", " (List.map string_of_int non_parameter_variables) )
			^ "."
		);
	(* First hide *)
	let result = px_hide non_parameter_variables px_linear_constraint in
	(* Remove higher space dimensions *)
	ippl_remove_higher_dimensions result !p_dim;
	(* Return result *)
	result


(** Eliminate (using existential quantification) all non-parameters (clocks only, as it is a PX constraint) and some parameters in a px_linear constraint *)
let px_hide_allclocks_and_someparameters_and_collapse parameters_to_hide px_linear_constraint = 
	let variables_to_hide = List.rev_append (clocks ()) parameters_to_hide in
	
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (
			"Function 'LinearConstraint.px_hide_allclocks_and_someparameters_and_collapse': hiding variables "
			^ (string_of_list_of_string_with_sep ", " (List.map string_of_int variables_to_hide) )
			^ "."
		);
	(* First hide *)
	let result = px_hide variables_to_hide px_linear_constraint in
	(* Remove higher space dimensions *)
	ippl_remove_higher_dimensions result !p_dim;
	(* Return result *)
	result



let pxd_hide_discrete_and_collapse pxd_linear_constraint = 
	(*** TODO: to move elsewhere, and to compute once for all, when the dimensions have been set ***)
	let discretes = list_of_interval (!nb_parameters + !nb_clocks) (!pxd_dim - 1) in
	
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (
			"Function 'LinearConstraint.pxd_hide_discrete_and_collapse': hiding variables "
			^ (string_of_list_of_string_with_sep ", " (List.map string_of_int discretes) )
			^ "."
		);
	(* First hide *)
	let result = pxd_hide discretes pxd_linear_constraint in
	(* Remove higher space dimensions *)
	ippl_remove_higher_dimensions result !px_dim;
	(* Return result *)
	result



(*------------------------------------------------------------*)
(* Adding and removing dimensions *)
(*------------------------------------------------------------*)

(** Add nb_dimensions to a linear_constraint *)
let add_dimensions = ippl_add_dimensions

let pxd_add_dimensions = add_dimensions




(** Remove the highest nb_dimensions from a linear_constraint *)
let remove_dimensions nb_dimensions linear_constraint =
	(* Compute the highest space dimension to keep *)
	let current_space_dimension = ippl_space_dimension linear_constraint in
	let new_space_dimension = current_space_dimension - nb_dimensions in

	(* Print some information *)
	if verbose_mode_greater Verbose_total then (
		print_message Verbose_total ("Function 'remove_dimensions': removing " ^ (string_of_int nb_dimensions) ^ " from " ^ (string_of_int current_space_dimension) ^ ", i.e., keeping " ^ (string_of_int new_space_dimension) ^ ".");
	);
	
	(* Projects the polyhedron referenced to by handle onto the first space_dimension dimensions *)
	ippl_remove_higher_dimensions linear_constraint new_space_dimension


let pxd_remove_dimensions = remove_dimensions


(*------------------------------------------------------------*)
(* Renaming *)
(*------------------------------------------------------------*)

(** rename variables in a constraint, with side effects *)
let pxd_rename_variables_assign list_of_couples linear_constraint =
	(* add reverse mapping *)
	let reverse_couples = List.map (fun (a,b) -> (b,a)) list_of_couples in
	let joined_couples = List.rev_append list_of_couples reverse_couples in
	(* find all dimensions that will be mapped *)
	let from, _  = List.split joined_couples in
	(* add identity pairs (x,x) for remaining dimensions *) 
	let rec add_id list i = 
		if i < 0 then list else
			if not (List.mem i from) then
				(i,i) :: add_id list (i-1)
			else
				add_id list (i-1)
		in 
	
	let complete_list = add_id joined_couples (!pxd_dim - 1) in
	
	(* Print some information *)
	if verbose_mode_greater Verbose_high then (
		let ndim = ippl_space_dimension linear_constraint in
		print_message Verbose_high ("mapping space dimensions, no. dimensions is " ^ string_of_int ndim);
		List.iter (fun (a,b) -> (print_message Verbose_high ("map v" ^ string_of_int a ^ " -> v" ^ string_of_int b))) complete_list;
	);
	
	(* perfom the mapping *)
	ippl_map_space_dimensions linear_constraint complete_list;

	assert_dimensions !pxd_dim linear_constraint

(* let pxd_rename_variables_assign = rename_variables_assign *)


(*(** Rename variables in a constraint *)
let rename_variables list_of_couples linear_constraint =
	(* copy polyhedron, as ppl function has sideeffects *)
	let poly = copy linear_constraint in
	rename_variables_assign list_of_couples poly;
	poly*)

(* let pxd_rename_variables = rename_variables *)


(*------------------------------------------------------------*)
(* Time elapsing and time past *)
(*------------------------------------------------------------*)

(* Generic time elapsing function *)
(* 'reverse_direction' should be minus_one for growing, one for decreasing *)
let time_elapse_gen_assign reverse_direction nb_dimensions variables_elapse variables_constant linear_constraint =
	(* Create the inequalities var = 1, for var in variables_elapse *)
	let inequalities_elapse = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] reverse_direction in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_eq
	) variables_elapse in
	(* Create the inequalities var = 0, for var in variables_constant *)
	let inequalities_constant = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] NumConst.zero in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_eq
	) variables_constant in
	(* Convert both sets of inequalities to a constraint *)
	let linear_constraint_time = make nb_dimensions (List.rev_append inequalities_elapse inequalities_constant) in
	
	(* Apply the time elapsing using PPL *)
	ippl_time_elapse_assign linear_constraint linear_constraint_time


(** Time elapsing function *)
let time_elapse_assign = time_elapse_gen_assign NumConst.minus_one

(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let pxd_time_elapse_assign c = time_elapse_assign !pxd_dim c



(*let time_elapse variables_elapse variables_constant linear_constraint =
	let linear_constraint = copy linear_constraint in
	time_elapse_assign variables_elapse variables_constant linear_constraint;
	linear_constraint*)


(** Time elapsing function, in backward direction (corresponds to the "past" operation in, e.g., [JLR15]) *)
let time_past_assign nb_dimensions variables_elapse variables_constant linear_constraint =
	(* 1) Apply generic function *)
	time_elapse_gen_assign NumConst.one nb_dimensions variables_elapse variables_constant linear_constraint;
	
	(* 2) Constrain the elapsing variables to be non-negative! *)
	(* Create the inequalities var >= 0, for var in variables_elapse *)
	let inequalities_nonnegative = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] NumConst.zero in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_ge
	) variables_elapse in
	(* Take intersection *)
	intersection_assign nb_dimensions linear_constraint [(make nb_dimensions inequalities_nonnegative)]
	

(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let pxd_time_past_assign c = time_past_assign !pxd_dim c


(*------------------------------------------------------------*)
(* Extrapolation to zero/infinity *)
(*------------------------------------------------------------*)

(** Perform an operation (?) on a set of variables: the first variable list will elapse, the second will remain constant *)
(*** TODO: describe better ***)
(*** WARNING: this function is certainly not optimized at all! ***)
let p_grow_to_infinity_assign variables_elapse variables_constant linear_constraint =
	(* Compute all variables *)
	let all_variables = List.rev_append variables_elapse variables_constant in
	(* Perform time elapsing on each variable *)
	List.iter (fun variable ->
		time_elapse_assign !p_dim [variable] (list_diff all_variables [variable]) linear_constraint;
	) variables_elapse;
	(* The end *)
	()


(** Perform an operation (?) on a set of variables: the first variable list will elapse, the second will remain constant *)
(** TODO: describe better *)
(** WARNING: this function is certainly not optimized at all! somehow we don't care considering it's not called "often" in IMITATOR *)
let p_grow_to_zero_assign variables_elapse variables_constant linear_constraint =
	(* Compute all variables *)
	let all_variables = List.rev_append variables_elapse variables_constant in
	(* Perform time elapsing on each variable *)
	List.iter (fun variable ->
		time_past_assign !p_dim [variable] (list_diff all_variables [variable]) linear_constraint;
	) variables_elapse;
	(* The end *)
	()


(*------------------------------------------------------------*)
(* Strict to non-strict *)
(*------------------------------------------------------------*)

(** Replace all strict inequalities with non-strict (and keeps others unchanged) within a p_linear_constraint *)
let render_non_strict_p_linear_constraint k =
	(* Get the list of inequalities *)
	let inequality_list = ippl_get_inequalities k in 
	(* Replace inequelities and convert back to a linear_constraint *)
	make_p_constraint (List.map strict_to_not_strict_inequality inequality_list)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 More testing functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*** NOTE: function after the modification functions, because makes use of 'px_intersection_assign' ***)

(** Check if a variable v is bound to be >= 0 in a constraint c *)
let px_is_positive_in v c =
	(* Idea: perform emptiness check of (v < 0 and c) *)
	(* Create v < 0, i.e., -v > 0 *)
	let v_lt = make_px_linear_term [
			NumConst.minus_one, v;
		] NumConst.zero in
	let v_l_zero = make !px_dim [make_px_linear_inequality v_lt Op_g] in
(* 	let variable_names variable_index ="v" ^ (string_of_int variable_index) in *)
	(* Intersect with c *)
	(*				print_string (string_of_linear_constraint variable_names v_l_zero);
					print_newline();
					print_string (string_of_linear_constraint variable_names c);
					print_newline();*)
	px_intersection_assign v_l_zero [c];
(*					print_string (string_of_linear_constraint variable_names v_l_zero);
					print_newline();*)
	(* Check *)
	not (is_satisfiable v_l_zero)


(** Check if a variable v is bound to be = 0 in a constraint *)
let is_zero_in nb_dimensions v c =
	(* Idea: perform equality check of (v = 0 & c) =?= c *)
	(* Create v = 0 *)
	let v_lt = make_px_linear_term [
			NumConst.one, v;
		] NumConst.zero in
	let v_l_zero = make nb_dimensions [make_px_linear_inequality v_lt Op_eq] in
	px_intersection_assign v_l_zero [c];
	(* Check *)
	px_is_equal v_l_zero c

(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let px_is_zero_in v c = is_zero_in !px_dim v c
let pxd_is_zero_in v c = is_zero_in !pxd_dim v c


(** Check if a variable v is bounded from above in a constraint *)
let px_is_bounded_from_above_in v c =
	(* Idea: use ppl_Polyhedron_bounds_from_above with 'v' as the linear_expression *)
	(* Create v *)
	let v_lt = ppl_linear_expression_of_linear_term (make_px_linear_term [NumConst.one, v;] NumConst.zero) in

	(* Apply *)
(* val ppl_Polyhedron_bounds_from_above : polyhedron -> linear_expression -> bool *)
	ippl_bounds_from_above c v_lt

let pxd_is_bounded_from_above_in = px_is_bounded_from_above_in


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Pi0-compatibility} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Check if a p_linear_constraint is pi0-compatible, i.e., whether the parameter valuation satisfies the linear constraint *)
let is_pi0_compatible pi0 linear_constraint =
	(* Increment discrete counter *)
	tcounter_pi0_compatibility#increment;

	(* Start continuous counter *)
	tcounter_pi0_compatibility#start;

	(* Get a list of linear inequalities *)
	let list_of_inequalities = ippl_get_inequalities linear_constraint in
	(* Check the pi0-compatibility for all *)
	let result =
	List.for_all (is_pi0_compatible_inequality pi0) list_of_inequalities
	in

	(* Stop continuous counter *)
	tcounter_pi0_compatibility#start;
	
	(* Return *)
	result


(** Check if a d_linear_constraint is pi0-compatible, i.e., whether the discrete valuation satisfies the linear constraint *)
let d_is_pi0_compatible = is_pi0_compatible



(** Compute the pi0-compatible and pi0-incompatible inequalities within a constraint *)
let partition_pi0_compatible pi0 linear_constraint =
	(* Get a list of linear inequalities *)
	let list_of_inequalities = ippl_get_inequalities linear_constraint in
	(* Partition *)
	List.partition (is_pi0_compatible_inequality pi0) list_of_inequalities



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion to GrML} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Convert a linear term (PPL) into a string *)								
let rec grml_of_linear_term_ppl names t_level = function
	| Coefficient z ->
		"\n" ^ (string_n_times t_level "\t") ^ "<attribute name=\"numValue\">" ^ (if Gmp.Z.equal z (Gmp.Z.from_int 0) then "0" else Gmp.Z.string_from z) ^ "</attribute>" 
	
	| Variable v ->
		"\n" ^ (string_n_times t_level "\t") ^ "<attribute name=\"name\">" ^ (names v) ^ "</attribute>" 
	
	| Unary_Plus t ->
		grml_of_linear_term_ppl names t_level t

	| Unary_Minus t -> 
		"\n" ^ (string_n_times t_level "\t") ^ "<attribute name=\"-\">"
		^ "\n" ^ (string_n_times (t_level + 1) "\t") ^ "<attribute name=\"numValue\">0</attribute>"
		^ (grml_of_linear_term_ppl names (t_level + 1) t)
		^ "\n" ^ (string_n_times t_level "\t") ^ "</attribute>"
	
	| Plus (lterm, rterm) ->
		let rightnull =
		match rterm with
			| Coefficient z -> Gmp.Z.equal z (Gmp.Z.from_int 0)
			| Times (z1 , Coefficient z2) -> Gmp.Z.equal z1 (Gmp.Z.from_int 0) || Gmp.Z.equal z2 (Gmp.Z.from_int 0)
			| Times (z , _) -> Gmp.Z.equal z (Gmp.Z.from_int 0)
			| _ -> false
		in
		(* If no right attribute: discard '+' *)
		if rightnull then grml_of_linear_term_ppl names t_level lterm
		else
		(* Else *)
		"\n" ^ (string_n_times t_level "\t") ^ "<attribute name=\"+\">"
		^ (grml_of_linear_term_ppl names (t_level + 1) lterm)
		^ (grml_of_linear_term_ppl names (t_level + 1) rterm)
		^ "\n" ^ (string_n_times t_level "\t") ^ "</attribute>"

	| Minus (lterm, rterm) ->
		"\n" ^ (string_n_times t_level "\t") ^ "<attribute name=\"-\">"
		^ (grml_of_linear_term_ppl names (t_level + 1) lterm)
		^ (grml_of_linear_term_ppl names (t_level + 1) rterm)
		^ "\n" ^ (string_n_times t_level "\t") ^ "</attribute>"
	
	| Times (z, rterm) ->
			(* Check that multiplication is not by one *)
			if (Gmp.Z.equal z (Gmp.Z.one)) then
				grml_of_linear_term_ppl names t_level rterm
			else 
				"\n" ^ (string_n_times t_level "\t") ^ "<attribute name=\"*\">"
				^ "\n" ^ (string_n_times (t_level + 1) "\t") ^ "<attribute name=\"numValue\">" ^ (Gmp.Z.string_from z) ^ "</attribute>" 
				^ (grml_of_linear_term_ppl names (t_level + 1) rterm)
				^ "\n" ^ (string_n_times t_level "\t") ^ "</attribute>"


(** Convert a linear inequality into a string *)
let grml_of_linear_inequality names t_level linear_inequality =
	let normal_ineq = normalize_inequality linear_inequality in
	let lterm, rterm, op = split_linear_inequality normal_ineq in
	let lstr = grml_of_linear_term_ppl names (t_level + 2) lterm in
	let rstr = grml_of_linear_term_ppl names (t_level + 2) rterm in
	let opstr = match op with
		| Less_Than_RS -> "less"
		| Less_Or_Equal_RS -> "lessEqual"
		| Equal_RS -> "equal"
		| Greater_Than_RS -> "greater"
		| Greater_Or_Equal_RS -> "greaterEqual" in
		""
	^ "\n" ^ (string_n_times (t_level) "\t") ^ "<attribute name=\"boolExpr\">"
	^ "\n" ^ (string_n_times (t_level + 1) "\t") ^ "<attribute name=\"" ^ opstr ^ "\">"
	  
	^ "\n" ^ (string_n_times (t_level + 2) "\t") ^ "<attribute name=\"expr\">" ^ lstr ^ "\n" ^ (string_n_times (t_level + 2) "\t") ^ "</attribute>"
	
	^ "\n" ^ (string_n_times (t_level + 2) "\t") ^ "<attribute name=\"expr\">" ^ rstr ^ "\n" ^ (string_n_times (t_level + 2) "\t") ^ "</attribute>"

	^ "\n" ^ (string_n_times (t_level + 1) "\t") ^ "</attribute>"
	^ "\n" ^ (string_n_times (t_level) "\t") ^ "</attribute>"

(** Convert a linear term into a string *)
let grml_of_linear_term names t_level linear_term =
	let linear_term, coef = normalize_linear_term linear_term in
(* 	grml_of_linear_term_ppl names t_level linear_term *)
	grml_of_linear_term_ppl names t_level linear_term

let grml_of_pxd_linear_term = grml_of_linear_term

(** Convert a linear constraint into a string *)
let grml_of_linear_constraint names t_level linear_constraint =
	(* First check if true or false *)
	if is_true linear_constraint then "<attribute name=\"boolExpr\"><attribute name=\"boolValue\">true</attribute></attribute>"
	else (if is_false linear_constraint then "<attribute name=\"boolExpr\"><attribute name=\"boolValue\">false</attribute></attribute>"
	else (
		(* Get a list of linear inequalities *)
		let list_of_inequalities = ippl_get_inequalities linear_constraint in
		let rec grml_of_linear_constraint_rec t_level = function
		| [] -> ""
		| first :: rest ->
			let several_inequalities = List.length rest > 0 in
			(* Conjunction : start *)
			(if several_inequalities then
				(
					""
					^ "\n" ^ (string_n_times (t_level) "\t") ^ "<attribute name=\"boolExpr\">"
					^ "\n" ^ (string_n_times (t_level+1) "\t") ^ "<attribute name=\"and\">"
			) else "")
			^
			(* Convert rest *)
			(*(string_of_list_of_string
				(List.map (grml_of_linear_inequality names (if several_inequalities then t_level + 1 else t_level)) list_of_inequalities)
			)*)
			(grml_of_linear_constraint_rec (t_level+1) rest)
			^
			(* Convert first *)
			(grml_of_linear_inequality names (if several_inequalities then t_level + 1 else t_level) first)
			^
			(* Conjunction : end *)
			(if several_inequalities then
				(
					""
					^ "\n" ^ (string_n_times (t_level+1) "\t") ^ "</attribute>"
					^ "\n" ^ (string_n_times (t_level) "\t") ^ "</attribute>"
			) else "")
		in grml_of_linear_constraint_rec t_level list_of_inequalities
	))

let grml_of_px_linear_constraint = grml_of_linear_constraint
let grml_of_pxd_linear_constraint = grml_of_linear_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion between types of constraints } *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Create a pxd_linear_constraint from a set of pairs (discrete variable, value) *)
let pxd_constraint_of_discrete_values (discrete_values : (variable * coef) list) =
(* 	raise (InternalError "Not implemented!!") *)
(* let instantiate_discrete discrete_values = *)
	let inequalities = List.map (fun (discrete_index, discrete_value) ->
		(* Create a linear term 'D - d' *)
		let linear_term = make_pxd_linear_term
			[(NumConst.one, discrete_index)]
			(NumConst.neg discrete_value)
		in
		(* Create a linear equality *)
		make_pxd_linear_inequality linear_term Op_eq
	) discrete_values in
	(* Create the linear constraint *)
	make_pxd_constraint inequalities


(** Convert (and copy) a PX into a PXD constraint by extending the number of dimensions; the original constraint remains unchanged *)
let px_of_p_constraint c =
	(* First copy *)
	let px_constraint = copy c in
	(* Extend number of dimensions *)
	ippl_add_dimensions (!px_dim - !p_dim) px_constraint;
	(* Assert *)
	assert_dimensions !px_dim px_constraint;
	(* Return *)
	px_constraint
	
(** Convert (and copy) a PX into a PXD constraint by extending the number of dimensions; the original constraint remains unchanged *)
let pxd_of_p_constraint c =
	(* First copy *)
	let pxd_constraint = copy c in
	(* Extend number of dimensions *)
	ippl_add_dimensions (!pxd_dim - !p_dim) pxd_constraint;
	(* Assert *)
	assert_dimensions !pxd_dim pxd_constraint;
	(* Return *)
	pxd_constraint
	
let pxd_of_px_constraint c =
	(* First copy *)
	let pxd_constraint = copy c in
	(* Extend number of dimensions *)
	ippl_add_dimensions (!pxd_dim - !px_dim) pxd_constraint;
	(* Assert *)
	assert_dimensions !pxd_dim pxd_constraint;
	(* Return *)
	pxd_constraint



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Brute-force casts (argh) } *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** "cast_p_of_pxd_linear_term p c" converts a PXD-constraint p to a P-constraint ; if c then a test if performed to check casting validity *)
(*** WARNING: in fact, for now NO TEST IS EVER PERFORMED ***)
let cast_p_of_pxd_linear_term p check = p (*** WARNING! should be copied here! ***)

(*** WARNING: in fact, for now NO TEST IS EVER PERFORMED ***)
let cast_p_of_pxd_linear_constraint pxd_linear_constraint check =
	(* First copy *)
	let p_constraint = copy pxd_linear_constraint in
	(* Decrease number of dimensions *)
	ippl_remove_higher_dimensions p_constraint !p_dim;
	(* Return *)
	p_constraint


(*** WARNING: in fact, for now NO TEST IS EVER PERFORMED ***)
let cast_d_of_pxd_linear_constraint check pxd_linear_constraint =
	(* Just copy *)
	let d_constraint = copy pxd_linear_constraint in
	(* Return *)
	d_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Plot interactions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


let unit_vector i =
 	fun j -> if i = j then NumConst.one else NumConst.zero


(* new type : Dot if the couple represent a Point and Vector if the couple represent a Ray *)
type couple = Dot of float*float
		|Vector of float*float
		|None

(* converts a generator to a 2d point wrt. the first two variables *)
let point_of_generator = function
	| Point (expr, c) -> 
			let x, y = 
				(evaluate_linear_term_ppl (unit_vector 0) expr,
				 evaluate_linear_term_ppl (unit_vector 1) expr) in
			let q = NumConst.numconst_of_mpz c in
			let xf = Gmp.Q.to_float (NumConst.mpq_of_numconst (NumConst.div x q)) in
			let yf = Gmp.Q.to_float (NumConst.mpq_of_numconst (NumConst.div y q)) in
			Dot (xf, yf)
	|Ray (expr) -> 
			let x, y = 
				(evaluate_linear_term_ppl (unit_vector 0) expr,
				 evaluate_linear_term_ppl (unit_vector 1) expr) in
			let xf = Gmp.Q.to_float (NumConst.mpq_of_numconst x) in
			let yf = Gmp.Q.to_float (NumConst.mpq_of_numconst y) in
			Vector (xf, yf)
	| _ -> None


(* comparison function for 2d points; used for sorting points *)
(* counter-clockwise wrt. a given center point (cx, cy) *)
let compare_points (cx, cy) (ax, ay) (bx, by) =
	let area = (ax -. cx) *. (by -. cy) -. (ay -. cy) *. (bx -. cx) in
	if area > 0.0 then 1 else
		if area = 0.0 then 0 else	-1

(* create a straight line from a point and a ray *)
let make_straight_line_ray r p =
	match (r,p) with
		|(Vector (a,b),Dot (c,d)) -> (let k=((a*.d)-.(b*.c)) in (b,(0.-.a),k))
		|_ -> (0.,0.,0.)


(* create a straight line from two points having the same abscissa or ordinate *)
let make_straight_line_points p1 p2 = 
	match (p1,p2) with
		|(Dot (a,b),Dot (c,d)) when a=c -> (1.,0.,0.-.a)
		|(Dot (a,b),Dot (c,d)) when b=d -> (0.,0.-.1.,b)
		|_ -> (0.,0.,0.)

(* find the intersection between two straight line *)
let intersection_straight_line d1 d2 =
	match (d1,d2) with
		|((0.,0.,0.),(d,e,f)) -> Dot ((0.),(0.))
		|((a,b,c),(0.,0.,0.)) -> Dot ((0.),(0.))
		|((a,b,c),(d,e,f)) -> (if b <> 0. then Dot ((((b*.f)-.(e*.c))/.((a*.e)-.(b*.d))),0.-.((c/.b)+.(((a*.b*.f)-.(a*.e*.c))/.((b*.a*.e)-.(b*.b*.d)))))
						else if e<>0. then Dot ((((b*.f)-.(e*.c))/.((a*.e)-.(b*.d))),0.-.((f/.e)+.(((d*.b*.f)-.(d*.e*.c))/.((e*.a*.e)-.(e*.b*.d)))))
						else Dot (0.,0.))

(* test if a point belong to a square line *)
let point_on_line p min_abs min_ord max_abs max_ord =
	match p with 
		|Dot (a,b) when ((min_abs <= a) && (a <= max_abs) && (min_ord <= b) && (b <= max_ord)) -> true
		|_ -> false


(* convert a linear constraint into two lists, one containing the points and the other containing the ray *)
let shape_of_poly x y linear_constraint =

	(* Print some information *)
	print_message Verbose_total ("Entering generate_points");
	
	(* Get the current number of dimensions *)
	let space_dimension = ippl_space_dimension linear_constraint in

	let poly = copy linear_constraint in
	(* project on variables x,y *)
	let remove = ref [] in
	for i = 0 to space_dimension - 1 do
		if i <> x && i <> y then
			remove := i :: !remove
	done;
	ippl_remove_dim poly !remove;
	let generators = ippl_get_generators poly in
	(* collect points for the generators *)
	let points = List.fold_left (fun ps gen ->
		let p = point_of_generator gen in 
		match p with
			| None -> ps
			| Dot (x,y) -> (x,y) :: ps
			|_-> ps
	) [] generators in
	(* collect ray for the generators *)
	let ray = List.fold_left (fun ps gen ->
		let p = point_of_generator gen in 
		match p with
			| None -> ps
			| Vector (x,y) -> (x,y) :: ps
			|_-> ps
	) [] generators in
	(points, ray)


type direction =
	| Nowhere
	| North
	| South
	| East
	| West


(* convert a linear constraint to a set of 2d points wrt. the variables x,y *)
let generate_points x y linear_constraint min_abs min_ord max_abs max_ord =

	(* Print some information *)
	if verbose_mode_greater Verbose_total then (
		print_message Verbose_total ("Entering generate_points");
		print_message Verbose_total ("Constraint: " ^ (string_of_linear_constraint (fun i->"v" ^ (string_of_int i)) linear_constraint));
	);

	let (points,ray) = shape_of_poly x y linear_constraint in
	(* add to points some new points generated by the ray elements *)
	let point_list = ref points in
	for i=0 to List.length points -1 do 
		for j=0 to List.length ray -1 do
			match List.nth ray j with
				|(u,v) when ((u>=0.) && (v>=0.)) -> point_list:=(max ((max_abs)*.(fst(List.nth ray j))) (fst (List.nth points i)) , max ((max_ord)*.(snd(List.nth ray j))) (snd (List.nth points i)))::!point_list
				|(u,v) when ((u<0.) && (v>=0.)) -> point_list:=(min ((min_abs)*.(fst(List.nth ray j))) (fst (List.nth points i)) , max ((max_ord)*.(snd(List.nth ray j))) (snd (List.nth points i)))::!point_list
				|(u,v) when ((u>=0.) && (v<0.)) -> point_list:=(max ((max_abs)*.(fst(List.nth ray j))) (fst (List.nth points i)) , min ((min_ord)*.(snd(List.nth ray j))) (snd (List.nth points i)))::!point_list
				|(u,v) when ((u<0.) && (v<0.)) -> point_list:=(min ((min_abs)*.(fst(List.nth ray j))) (fst (List.nth points i)) , min ((min_ord)*.(snd(List.nth ray j))) (snd (List.nth points i)))::!point_list
				| _ -> ()
		done
	done;
	(* add a point if there is two ray that cross two differents borders *)
	if List.length ray <> 0 then (
		(* create a point that will say which point is to be added *)
		let add_point = ref (Nowhere,Nowhere) in
		(* create a straight line for each side of the v0 space *)
		let high_border = make_straight_line_points (Dot (min_abs,max_ord)) (Dot (max_abs,max_ord)) in
		let right_border = make_straight_line_points (Dot (max_abs,min_ord)) (Dot (max_abs,max_ord)) in
		let low_border = make_straight_line_points (Dot (min_abs,min_ord)) (Dot (max_abs,min_ord)) in
		let left_border = make_straight_line_points (Dot (min_abs,min_ord)) (Dot (min_abs,max_ord)) in
		let i = ref 0 in
		(* lists in which the intersections points will be stored *)
		let l1 = ref [] in
		let l2 = ref [] in
		let l3 = ref [] in
		let l4 = ref [] in
		while !i <= (List.length ray -1) && (!add_point = (Nowhere,Nowhere)) do 
			let j = ref 0 in
			while !j <= (List.length points -1) do
				(* make the straight line to test from a point and a vector *)
				let straight_line = make_straight_line_ray (Vector (fst (List.nth ray !i), snd (List.nth ray !i))) (Dot (fst (List.nth points !j),snd (List.nth points !j))) in
				(* find the intersection between each v0 border and line to be checked *)
				let k1 = intersection_straight_line high_border straight_line in
				let k2 = intersection_straight_line right_border straight_line in
				let k3 = intersection_straight_line low_border straight_line in
				let k4 = intersection_straight_line left_border straight_line in
				(* store the intersection point into a list if it belongs to the v0 space *)
				if (point_on_line k1 min_abs min_ord max_abs max_ord) then l1:=k1::!l1
				else if (point_on_line k2 min_abs min_ord max_abs max_ord) then l2:=k2::!l2
				else if (point_on_line k3 min_abs min_ord max_abs max_ord) then l3:=k3::!l3
				else if (point_on_line k4 min_abs min_ord max_abs max_ord) then l4:=k4::!l4;
				j:=!j+1
			done;
			(* if two intersection points are on two consecutives border then mark it in add_point *)
			if List.length !l1 <> 0 && List.length !l2 <> 0 then add_point:=(North,East)
			else if List.length !l2 <> 0 && List.length !l3 <> 0 then add_point:=(East,South)
			else if List.length !l3 <> 0 && List.length !l4 <> 0 then add_point:=(South,West)
			else if List.length !l4 <> 0 && List.length !l4 <> 0 then add_point:=(West,North)
			(* if two intersection points are on two opposite border then mark it in add_point *)
			else if List.length !l1 <> 0 && List.length !l3 <> 0 then add_point:=(North,South)
			else if List.length !l2 <> 0 && List.length !l4 <> 0 then add_point:=(East,West)
			else if List.length !l3 <> 0 && List.length !l1 <> 0 then add_point:=(South,North)
			else if List.length !l4 <> 0 && List.length !l2 <> 0 then add_point:=(West,East);
			i:=!i+1
		done;
		(* add the intersection points between the border specified by add_point to point_list *)
		if !add_point = (North,East) then point_list:=(max_abs,max_ord)::!point_list
		else if !add_point = (East,South) then point_list:=(max_abs,min_ord)::!point_list
		else if !add_point = (South,West) then point_list:=(min_abs,min_ord)::!point_list
		else if !add_point = (West,North) then point_list:=(min_abs,max_ord)::!point_list
		else if !add_point = (North,South) then point_list:=(max_abs,max_ord)::(max_abs,min_ord)::!point_list
		else if !add_point = (East,West) then point_list:=(max_abs,min_ord)::(min_abs,min_ord)::!point_list
		else if !add_point = (South,North) then point_list:=(min_abs,min_ord)::(min_abs,max_ord)::!point_list
		else if !add_point = (West,East) then point_list:=(min_abs,max_ord)::(max_abs,max_ord)::!point_list;
	);
	(* swap coordinates if necessary *)
	let point_list = if x < y then !point_list else (
		List.map (fun (x,y) -> (y,x)) !point_list
	) in
	(* if points are present, sort them counter-clockwise *)
	match (point_list, List.length ray) with
		| ((p :: ps), 0) -> 
			let compare = compare_points p in
			(false,List.sort compare point_list)
		| ((p :: ps), _) -> 
			let compare = compare_points p in
			(true,List.sort compare point_list)
		| (_, 0) -> (false,point_list;)
		| (_, _) -> (true, point_list)

	
(* returns a string which indicate if some points have been found from ray and a string with 2d points of the given constraint *)
(*** WARNING: does not work if parameters are negative ***)
let plot_2d x y linear_constraint min_abs min_ord max_abs max_ord =

	(* Print some information *)
	print_message Verbose_total "Entering 'plot_2d'";

	let shape = generate_points x y linear_constraint min_abs min_ord max_abs max_ord in

	let str = List.fold_left (fun s (x, y) -> 
		s ^ (string_of_float x) ^ " " ^ (string_of_float y) ^ "\n"
	) "" (snd shape) in 
	((fst shape), str)



(************************************************************)
(************************************************************)
(** {2 PDBMs} *)
(************************************************************)
(************************************************************)


(*** WARNING! work in progress ***)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Types *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Relationship used in PDBMs *)
type pdbm_rel =
	(* <= *)
	| PDBM_leq
	(* < *)
	| PDBM_l

(* Actual value of e_ij *)
type pdbm_eij =
	| Infinity
	| Eij of p_linear_term

(* The actual PDBM: a matrix of size nb_clocks+1 *)
(* The 0-clock is the LAST clock for readability issues *)
type pdbm = (pdbm_eij * pdbm_rel) array array

(* Constrained PDBM = (C, D) *)
type cpdbm = p_linear_constraint * pdbm


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Creation *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Create a CPDBM with nb_clocks clocks, such that all clocks are set to 0;
	This CPDBM is called "E" in the init state definition of [HRSV02, p.20]*)
let make_zero_cpdbm (nb_clocks : int) =
	(* All elements initialized with ( 0 , <= ) *)
	let init_element() =
		Eij (make_p_linear_term [] NumConst.zero)
		,
		PDBM_leq
	in
	(** TODO: canonicalize ?!!! *)
	p_true_constraint ()
	,
	Array.make_matrix (nb_clocks+1) (nb_clocks+1) (init_element())


(** Create a CPDBM with nb_clocks clocks, such that it is equivalent to True *)
let make_true_cpdbm (nb_clocks : int) =

(* All elements first initialized with ( inf , <= ) *)
	let init_element() =
		Infinity
		,
		PDBM_l
	in
	
	let matrix = Array.make_matrix (nb_clocks+1) (nb_clocks+1) (init_element()) in

	(* Process special values *)
	let zero_clock = nb_clocks in
	for i = 0 to nb_clocks - 1 do
		(* Add clocks >= 0, i.e., zero_clock - x_i <= 0 *)
		matrix.(zero_clock).(i) <- Eij (make_px_linear_term [] NumConst.zero) , PDBM_leq;
		(* Generate correct diagonals, i.e., x_i - x_i <= 0 *)
		matrix.(i).(i) <- Eij (make_px_linear_term [] NumConst.zero) , PDBM_leq;
	done;
	
	(* Return PDBM *)
	p_true_constraint()
	,
	(** TODO: canonicalize ?!!! *)
	matrix


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Canonicalization *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Adding guard *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Reset *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Addition / substraction of PDBM matrix values *)
let pdbm_add_linear_terms eij1 eij2 = match (eij1, eij2) with
	| _, Infinity -> Infinity
	| Infinity, _ -> Infinity
	| Eij lt1, Eij lt2 -> Eij (add_linear_terms lt1 lt2)

let pdbm_sub_linear_terms eij1 eij2 = match (eij1, eij2) with
	| Infinity, _ -> Infinity
	| _, Infinity -> raise (InternalError "[PDBMs] Not sure what to do with 'lt - infinity'; such case shall never happen anyway.")
	| Eij lt1, Eij lt2 -> Eij (sub_linear_terms lt1 lt2)


(* Update a clock i to a p_linear_term b ("i := b" as in [HRSV02]) *)
let pdbm_update i (b:linear_term) pdbm =
	(* Number of regular clocks (excluding the 0-clock) *)
	let nb_clocks = Array.length pdbm - 1 in
	let zeroclock = nb_clocks in

 	(* for all j != i: ( *** WARNING: what about zeroclock ? I assume it should be included too) *)
	for j = 0 to nb_clocks (* - 1 *) do
		if j != i then(
			(* Dij <- (e0j + b), ~0j *)
			let (e0j : pdbm_eij), op0j = pdbm.(zeroclock).(j) in
			pdbm.(i).(j) <- pdbm_add_linear_terms e0j (Eij b), op0j;

			(* Dji <- (ej0 - b), ~j0 *)
			let (ej0 : pdbm_eij), opj0 = pdbm.(j).(zeroclock) in
			pdbm.(j).(i) <- pdbm_sub_linear_terms ej0 (Eij b), opj0;
		)
	done


(* Reset one clock *)
let cpdbm_reset clocki cpdbm =
	let _, pdbm = cpdbm in
	pdbm_update clocki (make_linear_term [] NumConst.zero) pdbm


(*** TODO: update to p_linear_term ; reset/update several clocks *)



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Time elapsing *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

let pdbm_time_elapsing pdbm =
	let _, matrix = pdbm in

	(* Number of regular clocks (excluding the 0-clock) *)
	let nb_clocks = Array.length matrix - 1 in
	let zeroclock = nb_clocks in

	(* Set (xi - xz) to (inf, <) for all i != zero_clock *)
	for i = 0 to nb_clocks - 1 do
		matrix.(i).(zeroclock) <- Infinity , PDBM_l;
	done


let cpdbm_time_elapsing cpdbm =
	let lt, pdbm = cpdbm in
	lt, (pdbm_time_elapsing pdbm)



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Conversion *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Convert a PDBM into a linear constraint *)
let px_linear_constraint_of_pdbm clock_offset pdbm =
	(* Number of regular clocks (excluding the 0-clock) *)
	let nb_clocks = Array.length pdbm - 1 in
	(* Convert PDBM relationship to linear_constraint relationship *)
	(*** BADPROG / WARNING: not the same direction! so bad..... *)
	let op_of_pdbm_rel = function
		| PDBM_leq -> Op_ge
		| PDBM_l -> Op_g
	in
	
	(* Convert a PDBM clock into a linear_constraint variable *)
	let variable_of_clock clock = clock + clock_offset in
	
	let zeroclock = nb_clocks in

	(* Convert all inequalities (eij, ~) into xi - xj ~ eij *)
	(* CHECK EFFICIENCY (alternative: gather all inequalities, then intersect using px_intersection) *)
	let linear_constraint : px_linear_constraint = px_true_constraint () in
	(* For each row (including the 0-clock)*)
	for i = 0 to nb_clocks do
		(* For each column (including the 0-clock) *)
		for j = 0 to nb_clocks do
		
			(* Get the linear term and the operator *)
			let eij, op = pdbm.(i).(j) in
			
			(* Does eij = infinity? *)
			begin
			match eij with 
			| Infinity ->
				(* xi - xj <= infinity means nothing *)
				()
			| Eij p_linear_term ->(
			(* Create xi - xj <= eij, i.e., xi - xj - eij <= 0 *)
			(*** BADPROG / WARNING: reverse order because operators not in the same direction! so bad..... *)
			(* Since the operators are reversed, we will create xj - xi + eij >= 0 *)
			
			let xixj_linear_term = make_px_linear_term
				(* Particular case with the 0-clock *)
				(
				if i = zeroclock && j = zeroclock then [] (* WARNING! check that no problem here? *)
				else if i = zeroclock  then [(NumConst.one, (variable_of_clock j))]
				else if j = zeroclock then [(NumConst.minus_one, (variable_of_clock i))]
				else [(NumConst.minus_one, (variable_of_clock i)) ; (NumConst.one, (variable_of_clock j))]
				)
			NumConst.zero in
			
			(* Add eij, so as to get " xj - xi + eij " *)
			let linear_term = add_linear_terms xixj_linear_term p_linear_term in
			
			(* Create linear inequality *)
			let inequality : px_linear_inequality = make_px_linear_inequality linear_term (op_of_pdbm_rel op) in
			
			(* Intersect with the current constraint *)
			(** WARNING / BADPROG: very unefficient ! *)
			px_intersection_assign linear_constraint [make_px_constraint [inequality]];
			);
			end
		done;
	done;
	
(*	(* 2) Convert the last row and last column corresponding to the 0-clock *)
	(* Last row *)
	for i = 0 to nb_clocks - 1 do
	
		(* Get the linear term and the operator *)
		let p_linear_term, op = pdbm.(i).(zeroclock) in
	
		(* Create the inequality *)
		(*** BADPROG / WARNING: reverse order because operators not in the same direction! so bad..... *)
		
		(* Create xi - xj *)
		let xixj_linear_term = make_px_linear_term [(NumConst.one, i) ; (NumConst.minus_one, j)] NumConst.zero in 
		
		(* Substract eij, so as to get " xi - xj - eij " *)
		let linear_term = sub_linear_terms xixj_linear_term p_linear_term in
		
		(* Create linear inequality *)
		let inequality : px_linear_inequality = make_px_linear_inequality linear_term (op_of_pdbm_rel op) in
		
		(* Intersect with the current constraint *)
		(** WARNING / BADPROG: very unefficient ! *)
		px_intersection_assign linear_constraint [make_px_constraint [inequality]];*)
	
	(* Return the constraint *)
	linear_constraint

	
(* Convert a PDBM into a linear constraint *)
let px_linear_constraint_of_cpdbm clock_offset cpdbm =
	(* Expand *)
	let p_linear_constraint , pdbm = cpdbm in
	(* Convert PDBM *)
	let px_linear_constraint = px_linear_constraint_of_pdbm clock_offset pdbm in
	(* Intersect *)
	px_intersection_assign_p px_linear_constraint [p_linear_constraint];
	(* Return *)
	px_linear_constraint






(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Tests *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let test_PDBMs () = 
	(* Keep constant number of clocks and parameters *)
	let test_nb_clocks = 4 in
	let test_nb_parameters = 3 in
	
	(* Set dimensions *)
	set_dimensions test_nb_parameters test_nb_clocks 0;
	
	(* Make variable names *)
	let variable_names variable_index =
		if (variable_index < test_nb_parameters)
			then "p" ^ (string_of_int variable_index)
			else "x" ^ (string_of_int (variable_index - test_nb_parameters))
	in
	
	(* Convert an op to a string *)
	let string_of_op = function
		| PDBM_leq -> "<="
		| PDBM_l -> "<"
	in
	
	(* Print a px constraint *)
	let print_px_constraint px_c =
		let stringpxc = string_of_px_linear_constraint variable_names px_c in
		print_string ("\n" ^ stringpxc)
	in
	
	(* Print a PDBM matrix *)
	let print_matrix pdbm =
		let nb_clocks = Array.length pdbm - 1 in
		
		let zeroclock = nb_clocks in
		
		print_string ("\nPrinting a PDBM of dimension " ^ (string_of_int nb_clocks) ^ " (+1) clocks");

		(* For each row (including the 0-clock)*)
		for i = 0 to nb_clocks do
			print_string "\n================\n";
			print_string ((if i = zeroclock then "z" else (string_of_int i)) ^ ":");

			(* For each column (including the 0-clock) *)
			for j = 0 to nb_clocks do
				(* Get the linear term and the operator *)
				let eij, op = pdbm.(i).(j) in

				(* Convert to strings *)
				let stringp = match eij with
				| Infinity -> "inf"
				| Eij p_linear_term ->
					string_of_p_linear_term variable_names p_linear_term
				in
				let stringop = string_of_op op in
				
				(* Print *)
				print_string (" | (" ^ stringp ^ " , " ^ stringop ^ ")")
			done;

			print_string " |";
		done;
		print_string "\n================\n";
		()
	in
	
	
	let print_cpdbm cpdbm =
		let _ , matrix = cpdbm in
		print_string "\nMatrix:";
		print_matrix matrix;
		print_string "\nConstraint:";
		print_px_constraint (px_linear_constraint_of_cpdbm test_nb_parameters cpdbm);
		()
	in

	let zero_clock = test_nb_clocks in
	
	print_string "\n*%*%*%*%*%*% STARTING PDBMs TESTS *%*%*%*%*%*%";
	(* Zero-PDBM *)
	print_string "\n\nEmpty PDBM";
	let cpdbm_void = make_zero_cpdbm test_nb_clocks in
	let _ , pdbm_void = cpdbm_void in
	print_cpdbm cpdbm_void;
	
	(* Add pconstraint *)
	print_string "\n\nEmpty PDBM with p-constraint";
	(* p0 + 1 >= p1 *)
	let plt1 = make_px_linear_term [(NumConst.one, 0); (NumConst.minus_one, 1)] (NumConst.one) in
	let pc1 = make_px_constraint [make_px_linear_inequality plt1 Op_ge] in
	print_string "\nP-constraint:";
	print_string (string_of_p_linear_constraint variable_names pc1);
	let cpdbm1 = (pc1, pdbm_void) in
	print_cpdbm cpdbm1;
	
	(* True PDBM *)
	print_string "\n\nTrue PDBM";
	let cpdbm2 = make_true_cpdbm test_nb_clocks in
	let _ , pdbm_true = cpdbm2 in
	print_cpdbm cpdbm2;

	
	(* Change some linear term *)
	print_string "\n\nAdding x1 - x2 <= 2p0 + p2";
	pdbm_true.(1).(2) <- Eij (
		make_p_linear_term [(NumConst.numconst_of_int 2, 0); (NumConst.one, 2)] (NumConst.zero) 
	) , PDBM_leq ;
	print_cpdbm cpdbm2;

	print_string "\n\nAdding x2 - x1 < 3p1 + 2";
	pdbm_true.(2).(1) <- Eij (
		make_p_linear_term [(NumConst.numconst_of_int 3, 1)] (NumConst.numconst_of_int 2) 
	) , PDBM_l ;
	print_cpdbm cpdbm2;

	print_string "\n\nAdding x0 < p0";
	pdbm_true.(0).(zero_clock) <- Eij (
		make_p_linear_term [(NumConst.one, 0)] (NumConst.zero)
	) , PDBM_l ;
	print_cpdbm cpdbm2;

	print_string "\n\nAdding x0 >= 5";
	(* x0 >= 5 (i.e., xz - x0 <= -5 *)
	pdbm_true.(zero_clock).(0) <- Eij (
		make_p_linear_term [] (NumConst.numconst_of_int (-5))
	) , PDBM_leq ;
	print_cpdbm cpdbm2;

	(* Apply time elapsing *)
	print_string "\n\nPDBM after time elapsing";
	pdbm_time_elapsing cpdbm2;
	print_cpdbm cpdbm2;
	
	(* Reset x6 *)
	print_string "\n\nResetting...";
	cpdbm_reset 3 cpdbm2;
	print_cpdbm cpdbm2;
	
	print_string "\n*%*%*%*%*%*% ENDING PDBMs TESTS *%*%*%*%*%*%";
	()



(************************************************************)
(** {2 Non-necessarily convex linear Constraints} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Non-necessarily convex constraint on the parameters ("pointset powerset" in the underlying PPL implementation) *)
type nnconvex_constraint = Ppl.pointset_powerset_nnc_polyhedron

type p_nnconvex_constraint = nnconvex_constraint
type px_nnconvex_constraint = nnconvex_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Create a false non-necessarily convex constraint *)
let false_p_nnconvex_constraint () = ippl_nncc_false_constraint !p_dim
let false_px_nnconvex_constraint () = ippl_nncc_false_constraint !px_dim


(** Create a true non-necessarily convex constraint *)
let true_p_nnconvex_constraint () = ippl_nncc_true_constraint !p_dim
let true_px_nnconvex_constraint () = ippl_nncc_true_constraint !px_dim


(** Create a new p_nnconvex_constraint from a linear_constraint *)
let p_nnconvex_constraint_of_p_linear_constraint (p_linear_constraint : p_linear_constraint) = ippl_nncc_from_poly p_linear_constraint

let px_nnconvex_constraint_of_px_linear_constraint c =
	(* Assert *)
	assert_dimensions !px_dim c;
	(* Copy *)
	let result = ippl_nncc_from_poly c in
	(* Assert *)
	nncc_assert_dimensions !px_dim result;
	(* Return result *)
	result

(** Copy a nnconvex_constraint *)
let nnconvex_copy nnconvex_constraint = ippl_nncc_copy nnconvex_constraint
let p_nnconvex_copy = nnconvex_copy
let px_nnconvex_copy = nnconvex_copy



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Get the list of p_linear_constraint the disjunction of which makes a p_nnconvex_constraint *)
let get_disjuncts p_nnconvex_constraint =
	(* Increment discrete counter *)
	ppl_nncc_get_disjuncts#increment;
	
	(* Start continuous counter *)
	ppl_nncc_get_disjuncts#start;

	(* Create ref for the result *)
	let disjuncts = ref [] in

	(* Create iterator *)
	let iterator = ippl_nncc_begin_iterator p_nnconvex_constraint in
	(* Create an iterator for the end *)
	let end_iterator = ippl_nncc_end_iterator p_nnconvex_constraint in
	
	(* Iterate until the end *)
	(*** NOTE: apparently, ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator represents the index AFTER the last element, hence the following test is correct ***)
	while not (ippl_nncc_equals_iterator iterator end_iterator) do
		(* Get the current disjunct *)
		let disjunct = ippl_nncc_get_disjunct iterator in
		
		(* Add it to the list of disjuncts *)
		disjuncts := disjunct :: !disjuncts;
		
		(* Increment the iterator *)
		ippl_nncc_increment_iterator iterator;
	done;
	
	(* Return disjuncts *)
	let result = List.rev (!disjuncts) in

	(* Start continuous counter *)
	ppl_nncc_get_disjuncts#stop;
	
	result



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Check if a nnconvex_constraint is false *)
let p_nnconvex_constraint_is_false = ippl_nncc_is_empty


(** Check if a nnconvex_constraint is true *)
let p_nnconvex_constraint_is_true = ippl_nncc_is_universe


(** Check if a nnconvex_constraint is pi0-compatible *)
(*** NOTE: here, we split the nnconvex_constraint into a list of convex constraints, and we perform the check; the other option would have been to create a nnconvex_constraint from the point, and check inclusion ***)
(*** WARNING: function not tested ***)
let p_nnconvex_constraint_is_pi0_compatible pval p_nnconvex_constraint =
	(* 1) Get the constraints *)
	let disjuncts = get_disjuncts p_nnconvex_constraint in
	
	(* 2) Check each of them *)
	List.exists (fun p_linear_constraint -> is_pi0_compatible pval p_linear_constraint) disjuncts


(** Check if a nnconvex_constraint is included in another one *)
let p_nnconvex_constraint_is_leq p_nnconvex_constraint p_nnconvex_constraint' =
	(*** NOTE: PPL works in the reverse order: the 2nd covers the 1st one ***)
	ippl_nncc_geometrically_covers p_nnconvex_constraint' p_nnconvex_constraint

(** Check if a nnconvex_constraint is equal to another one *)
let p_nnconvex_constraint_is_equal = ippl_nncc_geometrically_equals


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Simplification} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let simplify p_nnconvex_constraint =
	ippl_nncc_pairwise_reduce p_nnconvex_constraint;
	ippl_nncc_omega_reduce p_nnconvex_constraint;
	()
	

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Convert a p_nnconvex_constraint into a string *)
let string_of_p_nnconvex_constraint names p_nnconvex_constraint =
	(* First reduce (avoids identical disjuncts) *)
	simplify p_nnconvex_constraint;
	
	(* Get the disjuncts *)
	let disjuncts = get_disjuncts p_nnconvex_constraint in
	
	(* Case false *)
	if disjuncts = [] then string_of_false else(
	
		(* Convert each disjunct into a string *)
		let disjuncts_string = List.map (string_of_p_linear_constraint names) disjuncts in
		
		(* Concatenate using an "OR" *)
		string_of_list_of_string_with_sep "\nOR\n " disjuncts_string
	)

let string_of_px_nnconvex_constraint = string_of_p_nnconvex_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Modifications} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Performs the intersection of a p_nnconvex_constraint with a p_linear_constraint; the p_nnconvex_constraint is modified, the p_linear_constraint is not *)
let nnconvex_intersection nb_dimensions nnconvex_constraint linear_constraint =
	(* Assert *)
	nncc_assert_dimensions nb_dimensions nnconvex_constraint;
	assert_dimensions nb_dimensions linear_constraint;
	
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (
			"Entering 'LinearConstraint.p_nnconvex_intersection' with " ^ (string_of_int (ippl_nncc_space_dimension nnconvex_constraint)) ^ " and " ^ (string_of_int (ippl_space_dimension linear_constraint)) ^ " dimensions."
	);

	(* First retrieve inequalities *)
	let constraint_system =  ippl_get_inequalities linear_constraint in

	ippl_nncc_add_constraints nnconvex_constraint constraint_system;

	(* Simplify the constraint (avoids identical disjuncts) *)
	simplify nnconvex_constraint;
	
	(* The end *)
	()

(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let p_nnconvex_intersection c = nnconvex_intersection !p_dim c
let px_nnconvex_intersection c = nnconvex_intersection !px_dim c


(** Performs the union of a p_nnconvex_constraint with a p_linear_constraint; the p_nnconvex_constraint is modified, the p_linear_constraint is not *)
let nnconvex_union nb_dimensions nnconvex_constraint linear_constraint =
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (
			"Entering 'LinearConstraint.nnconvex_union' with " ^ (string_of_int (ippl_nncc_space_dimension nnconvex_constraint)) ^ " and " ^ (string_of_int (ippl_space_dimension linear_constraint)) ^ " dimensions. Expected: " ^ (string_of_int nb_dimensions) ^ "."
	);
	
	(* Assert *)
	nncc_assert_dimensions nb_dimensions nnconvex_constraint;
	assert_dimensions nb_dimensions linear_constraint;

	ippl_nncc_add_disjunct nnconvex_constraint linear_constraint;

	(* Simplify the constraint (avoids identical disjuncts) *)
	simplify nnconvex_constraint;
	
	(* The end *)
	()


(*** NOTE: must provide the argument so be sure the function is dyamically called; otherwise statically !p_dim is 0 ***)
let p_nnconvex_p_union c =
	print_message Verbose_total ("Entering 'LinearConstraint.p_nnconvex_p_union'");
	nnconvex_union !p_dim c

let px_nnconvex_px_union c =
	print_message Verbose_total ("Entering 'LinearConstraint.px_nnconvex_px_union'");
	nnconvex_union !px_dim c


(** Performs the union of a p_nnconvex_constraint with another p_nnconvex_constraint; the first p_nnconvex_constraint is modified, the second is not *)
let p_nnconvex_union p_nnconvex_constraint p_nnconvex_constraint' =
	(* Assert *)
(*	nncc_assert_dimensions nb_dimensions p_nnconvex_constraint;
	nncc_assert_dimensions nb_dimensions p_nnconvex_constraint';*)

	(* Get the disjuncts of the second p_nnconvex_constraint *)
	let disjuncts = get_disjuncts p_nnconvex_constraint' in
	
	(* Add each of them as a union *)
	List.iter (p_nnconvex_p_union p_nnconvex_constraint) disjuncts


(** Performs the difference between a first p_nnconvex_constraint and a second p_nnconvex_constraint; the first is modified, the second is not *)
let p_nnconvex_difference p_nnconvex_constraint p_nnconvex_constraint' =
	(* Assert *)
(*	nncc_assert_dimensions nb_dimensions p_nnconvex_constraint;
	nncc_assert_dimensions nb_dimensions p_nnconvex_constraint';*)

	(* Execute *)
	ippl_nncc_difference_assign p_nnconvex_constraint p_nnconvex_constraint';

	(* Simplify the constraint (avoids identical disjuncts) *)
	simplify p_nnconvex_constraint;
	
	(* The end *)
	()

let px_nnconvex_difference = p_nnconvex_difference


(*(** Eliminate a set of variables, side effects version *)
let nnconvex_hide_assign variables nnconvex_constraint =

	if List.length variables = 0 then(
		(*** DEBUG ***)
		print_warning "Attempting to hide an empty list of variables in nnconvex_hide_assign";
	)
	(* Only hide a non-empty list *)
	else (
		(*** DEBUG ***)
		print_string ("\nBefore eliminating non-parameters in: \n" ^ (string_of_p_nnconvex_constraint (fun i -> "v_" ^ (string_of_int i)) nnconvex_constraint));

	(* debug output *)
(*		if verbose_mode_greater Verbose_total then (
			print_message Verbose_total "About to hide:";
			List.iter (fun v -> print_message Verbose_total ("  - v" ^ string_of_int v)) variables;
		);*)
		(* Statistics *)
		(*** TODO ***)
(*		ppl_nb_unconstrain := !ppl_nb_unconstrain + 1;
		let start = Unix.gettimeofday() in*)
		(* Actual call to PPL *)
		ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimensions nnconvex_constraint variables;

		(*** DEBUG ***)
		print_string ("\nAfter eliminating non-parameters in: \n" ^ (string_of_p_nnconvex_constraint (fun i -> "v_" ^ (string_of_int i)) nnconvex_constraint));
		
		(* Statistics *)
		(*** TODO ***)
(* 		ppl_t_unconstrain := !ppl_t_unconstrain +. (Unix.gettimeofday() -. start); *)
		(*** TODO ***)
(* 		assert_dimensions linear_constraint *)
	)
*)



(** Create a new p_nnconvex_constraint from a list of p_linear_constraint *)
let p_nnconvex_constraint_of_p_linear_constraints (p_linear_constraints : p_linear_constraint list) =
	(* Create a false constraint *)
	let result = false_p_nnconvex_constraint() in
	(* Add each constraint as a disjunction *)
	List.iter (fun p_linear_constraint -> 
		p_nnconvex_p_union result p_linear_constraint;
	) p_linear_constraints;
	(* Return result *)
	result

(** Create a new px_nnconvex_constraint from a list of px_linear_constraint *)
let px_nnconvex_constraint_of_px_linear_constraints (px_linear_constraints : px_linear_constraint list) =
	(* Create a false constraint *)
	let result = false_px_nnconvex_constraint() in
	(* Add each constraint as a disjunction *)
	List.iter (fun px_linear_constraint -> 
		px_nnconvex_px_union result px_linear_constraint;
	) px_linear_constraints;
	(* Return result *)
	result


let p_nnconvex_hide variables p_nnconvex_constraint =
	(* 1) Get disjuncts *)
	let disjuncts = get_disjuncts p_nnconvex_constraint in
	
	(* 2) Hide in each disjuncts *)
	let disjuncts_hidden = List.map (hide !p_dim variables) disjuncts in
	
	(* 3) Recreate the nnconvex_constraint *)
	p_nnconvex_constraint_of_p_linear_constraints disjuncts_hidden
	

let px_nnconvex_hide variables px_nnconvex_constraint =
	(* 1) Get disjuncts *)
	let disjuncts = get_disjuncts px_nnconvex_constraint in
	
	(* 2) Hide in each disjuncts *)
	let disjuncts_hidden = List.map (hide !px_dim variables) disjuncts in
	
	(* 3) Recreate the nnconvex_constraint *)
	px_nnconvex_constraint_of_px_linear_constraints disjuncts_hidden




(** Eliminate (using existential quantification) all non-parameters (clocks) in a px_linear constraint *)
let px_nnconvex_hide_nonparameters_and_collapse px_nnconvex_constraint =
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (
			"Entering 'LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse' with " ^ (string_of_int (ippl_nncc_space_dimension px_nnconvex_constraint)) ^ " dimensions."
	);

	(* Assert *)
	nncc_assert_dimensions !px_dim px_nnconvex_constraint;

	(* Compute non-parameters *)
	let non_parameter_variables = clocks () in
	
	(* Call the actual elimination function *)
	let result = px_nnconvex_hide non_parameter_variables px_nnconvex_constraint in
	
	(* Decrease the number of dimensions *)
	ippl_nncc_remove_higher_space_dimensions result !p_dim;

	(* Assert *)
	nncc_assert_dimensions !p_dim result;

	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (
			"Exiting 'LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse' with " ^ (string_of_int (ippl_nncc_space_dimension result)) ^ " dimensions."
	);

	(* Return result *)
	result
	


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion to a list of p_linear_constraint} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Converts a p_nnconvex_constraint into a list of p_linear_constraint such that the union of this list is equal to the p_nnconvex_constraint *)
let p_linear_constraint_list_of_p_nnconvex_constraint =
	(* Get the disjuncts *)
	get_disjuncts


	
	
	
	

(************************************************************)
(************************************************************)
(** {2 Non-necessarily convex linear Constraints} *)
(************************************************************)
(************************************************************)
type p_convex_or_nonconvex_constraint =
	| Convex_p_constraint of p_linear_constraint
	| Nonconvex_p_constraint of p_nnconvex_constraint

(** Convert a p_convex_or_nonconvex_constraint into a string *)
let string_of_p_convex_or_nonconvex_constraint names = function
	| Convex_p_constraint p_linear_constraint ->  string_of_p_linear_constraint names p_linear_constraint
	| Nonconvex_p_constraint p_nnconvex_constraint -> string_of_p_nnconvex_constraint names p_nnconvex_constraint





(************************************************************)
(************************************************************)
(** {2 Serialization for PaTATOR} *)
(************************************************************)
(************************************************************)

(*
	General translation :

	2 p1 + p2 + p3 <= 4 p4
	^ 2/3 p1 + 5 p2 > 7 p3
	=>
	2,1+1,2+1,3l4,4^2/3,1+5,2>7,3
	
	Operators:
	< l = g >
*)

(** Separator between coef and variable *)
let serialize_SEP_CV = "*"

(** Separator between linear terms *)
let serialize_SEP_LT = "+"

(** Separator between linear inequalities ('a' stands for 'and'; not using '^' because makes conflicts in regular expressions) *)
let serialize_SEP_AND = "a"

(** Separator between non-convex linear constraints ('o' stands for 'or') *)
let serialize_SEP_OR = "o"


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Variables} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

let serialize_variable = string_of_int

let unserialize_variable variable_string =
	(* First check that it is an integer *)
	(*** NOTE: test already performed by int_of_string? ***)
	if not (Str.string_match (Str.regexp "^[0-9]+$") variable_string 0) then
		raise (SerializationError ("Cannot unserialize variable '" ^ variable_string ^ "': int expected."));
	(* First check that it is an integer *)
	int_of_string variable_string


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Coefficients} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let serialize_coef = Gmp.Z.string_from

let unserialize_coef = (*NumConst.numconst_of_string*)Gmp.Z.from_string


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Operators} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let serialize_ALL_OPS = "<l=g>"

let serialize_op = function
	| Less_Than_RS -> "<"
	| Less_Or_Equal_RS -> "l"
	| Equal_RS -> "="
	| Greater_Or_Equal_RS -> "g"
	| Greater_Than_RS -> ">"

let unserialize_op s = match s with
	| "<" -> Less_Than_RS
	| "l" -> Less_Or_Equal_RS
	| "=" -> Equal_RS
	| "g" -> Greater_Or_Equal_RS
	| ">" -> Greater_Than_RS
	| _ -> raise (SerializationError ("Cannot unserialize op '" ^ s ^ "'."))


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Coefficients} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Unserialize a pair (coef, variable) or a coef *)
let unserialize_coef_var coef_var_pair_string =
	match split serialize_SEP_CV coef_var_pair_string with
	(* Case variable with a coefficient *)
	| [coef_string ; variable_string ] ->
		Times (unserialize_coef coef_string , Variable (unserialize_variable variable_string))
	(* Case coefficient alone *)
	| [coef_string ] ->
		Coefficient (unserialize_coef coef_string)
	| _ -> raise (SerializationError ("Cannot unserialize string '" ^ coef_var_pair_string ^ "': (coef, variable_index) or coef expected."))



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Linear term} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Convert a linear term (PPL) into a string *)
let rec serialize_linear_term linear_term =
	match linear_term with
	(*** WARNING: slightly problematic translation, as we do not check that a variable is never without a coefficient (in which case it will be interpreted as a coefficient instead of a variable) ***)
		| Coefficient z -> serialize_coef z
		| Variable v -> serialize_variable v
		| Unary_Plus t -> (*serialize_linear_term t*)raise (InternalError("Match Unary_Plus not taken into account in serialization"))
		| Unary_Minus t -> raise (InternalError("Match Unary_Minus not taken into account in serialization"))
		| Plus (lterm, rterm) -> (
			  let lstr = serialize_linear_term lterm in
				let rstr = serialize_linear_term rterm in
				lstr ^ serialize_SEP_LT ^ rstr )
		| Minus (lterm, rterm) -> raise (InternalError("Match Minus not taken into account in serialization"))
		| Times (z, rterm) -> (
				let fstr = serialize_coef z in
				let tstr = serialize_linear_term rterm in
					match rterm with
						| Coefficient _ -> raise (InternalError("Case 'z * Coefficient' not taken into account in serialization"))
						| Variable    _ -> fstr ^ serialize_SEP_CV ^ tstr
						| _ -> raise (InternalError("Case '_ * Coefficient' not taken into account in serialization"))
						
				)


let unserialize_linear_term linear_term_string =
	(* Split according to the separator '+' *)
	let coef_var_pairs_string = split serialize_SEP_LT linear_term_string in
	(* Convert to proper coefs and vars *)
	let coef_var_pairs = List.map unserialize_coef_var coef_var_pairs_string in
	(* Reconstruct the linear_term *)
	match coef_var_pairs with
	(* Only one linear term *)
	| [ coef_var ] -> coef_var
	(* More than one linear term *)
	| coef_var :: rest ->
		List.fold_left (fun current_lt coef_var ->
			Plus(coef_var, current_lt)
		) coef_var rest
	| _ -> raise (SerializationError("Found empty linear term when unserializing."))


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Linear inequalities} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Serialize a linear inequality *)
let serialize_linear_inequality linear_inequality =
(* 	let normal_ineq = normalize_inequality linear_inequality in *)
	let lterm, rterm, op = split_linear_inequality linear_inequality in
	let lstr = serialize_linear_term lterm in
	let rstr = serialize_linear_term rterm in
	lstr ^ (serialize_op op) ^ rstr


let unserialize_linear_inequality linear_inequality_string =
	(* Split according to the operators *)
	let s = ("^\\(.+\\)\\([" ^ serialize_ALL_OPS ^ "]\\)\\(.+\\)$") in
	let r = Str.regexp s in
	(* Check accuracy *)
	let matched = (*try*)
		Str.string_match r linear_inequality_string 0
		(*with Failure f -> raise (SerializationError("Failure while unserializing linear inequality '" ^ linear_inequality_string ^ "'. Expected: (lterm, op, rterm). Error: " ^ f));*)
	in
	if not matched then(
		raise (SerializationError("Found unexpected linear inequality '" ^ linear_inequality_string ^ "'. Expected: (lterm, op, rterm)."))
	);
	(* Retrieve the 3 groups *)
	let lstr = Str.matched_group 1 linear_inequality_string in
	let op_string = Str.matched_group 2 linear_inequality_string in
	let rstr = Str.matched_group 3 linear_inequality_string in
	
	(*try*)
	(* Unserialize and build *)
	build_linear_inequality
		(unserialize_linear_term lstr)
		(unserialize_linear_term rstr)
		(unserialize_op op_string)
	(*with Failure f -> raise (SerializationError("Failure while unserializing linear inequality '" ^ linear_inequality_string ^ "'. Error: " ^ f))*)
	


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Linear constraints} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Serialize a linear constraint *)
let serialize_p_linear_constraint linear_constraint =
	(* Get a list of linear inequalities and serialize *)
	let list_of_inequalities = List.map serialize_linear_inequality (ippl_get_inequalities linear_constraint) in
	(* Add separators *)
	String.concat serialize_SEP_AND list_of_inequalities



let unserialize_p_linear_constraint linear_constraint_string =
	(* Split according to the separator serialize_SEP_AND *)
	let inequalities_string =
		try
			split serialize_SEP_AND linear_constraint_string
		with Failure f -> raise (SerializationError("Splitting failure while unserializing linear inequality '" ^ linear_constraint_string ^ "'. Error: " ^ f))
	in
	(* Convert to linear inequalities *)
	let inequalities =  List.map unserialize_linear_inequality inequalities_string in
	(* Reconstruct the linear constraint *)
	make !p_dim inequalities


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Parametric non-necessarily convex constraints} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let serialize_p_nnconvex_constraint p_nnconvex_constraint =
	(* Get a list of linear constraints and serialize *)
	let list_of_constraints = List.map serialize_p_linear_constraint (get_disjuncts p_nnconvex_constraint) in
	(* Add separators *)
	String.concat serialize_SEP_OR list_of_constraints

let unserialize_p_nnconvex_constraint p_nnconvex_constraint_string =
	(* Split according to the separator serialize_SEP_OR *)
	let constraints_string =
		try
			split serialize_SEP_OR p_nnconvex_constraint_string
		with Failure f -> raise (SerializationError("Splitting failure while unserializing linear inequality '" ^ p_nnconvex_constraint_string ^ "'. Error: " ^ f))
	in
	(* Convert to linear constraints *)
	let constraints =  List.map unserialize_p_linear_constraint constraints_string in
	(* Reconstruct the p_nnconvex_constraint *)
	p_nnconvex_constraint_of_p_linear_constraints constraints



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Non-necessarily convex linear Constraints} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let serialize_p_convex_or_nonconvex_constraint = function
	| Convex_p_constraint p_linear_constraint -> serialize_p_linear_constraint p_linear_constraint
	| Nonconvex_p_constraint p_nnconvex_constraint -> serialize_p_nnconvex_constraint p_nnconvex_constraint

let unserialize_p_convex_or_nonconvex_constraint p_convex_or_nonconvex_constraint_string =
	(* A bit a hack: if there is a disjunction, then this is a p_nnconvex_constraint *)
	(* First convert serialize_SEP_OR into a char *)
	(*** HACK ***)
	if String.length serialize_SEP_OR <> 1 then raise (InternalError("It was assumed that '" ^ serialize_SEP_OR ^ "' was only one character long."));
	let sep_char = String.get serialize_SEP_OR 0 in
	if String.contains p_convex_or_nonconvex_constraint_string sep_char then
		Nonconvex_p_constraint (unserialize_p_nnconvex_constraint p_convex_or_nonconvex_constraint_string)
	else
		Convex_p_constraint (unserialize_p_linear_constraint p_convex_or_nonconvex_constraint_string)








(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Gia's function for CUB **)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** IMITATOR operator style to string **)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*Begin - Needed functions for part 1*)

(*get string of operators*)
let operator2string op = match op with
	| Op_g  -> ">"
	| Op_ge -> ">="
	| Op_eq -> "="
	| Op_le -> "<="
	| Op_l  -> "<"

(*End - Needed functions for part 1*)


(*Begin - Needed functions for part 2*)

(* check whether vars in liear term 1 is a subset of linear term 2 *)
let is_var_subset var_list1 var_list2 = let result = ref true in
										List.iter 	(fun var ->	if not (List.mem var var_list2) 
																then 
																	(
																	result := false;
																	print_message Verbose_standard ("\n	 the var1: "^ string_of_int var ^" is not in var_list2 ");
																	);

													) var_list1;
										!result


(*for linear term*)
let rec isMinus linear_term =	(* let coef = ref NumConst.zero in *)
								let b = ref false in
								begin
								match linear_term with
								| Coef c -> ()
								| Var v -> ()
								| Pl (lterm, rterm) -> 	(
													(*** TODO: problem here?? (ÉA, 2017/02/08) ***)
			  											(* isMinus lterm;
														isMinus rterm; *)
														b := (isMinus lterm || isMinus rterm);
														()
														)
								| Mi (lterm, rterm) -> 	( 
														b := true; 
														() 
														)
								| Ti (c1, rterm) -> ()
								(*| _ -> raise (InternalError("Detection error 'get_coef' function"))*)
								end;
								!b


(*for linear term*)
let rec get_coefs_vars linear_term =	let coefs_vars = ref [] in
										let _ = match linear_term with
										| Coef c -> coefs_vars := !coefs_vars@[(9999, c)]
										| Var v -> coefs_vars := !coefs_vars@[(v, NumConst.one)] (*()*)
										| Pl (lterm, rterm) -> (
			  								coefs_vars := !coefs_vars@get_coefs_vars lterm;
											coefs_vars := !coefs_vars@get_coefs_vars rterm;
											() )
										| Mi (lterm, rterm) -> (
			  								coefs_vars := !coefs_vars@get_coefs_vars lterm;
											coefs_vars := !coefs_vars@get_coefs_vars rterm;
											() )
										| Ti (c1, rterm) -> (
															match rterm with
															| Var  v1 -> coefs_vars := !coefs_vars@[(v1, c1)]
															| _ -> raise (InternalError("Could not detect RightTerm in Time*RightTerm error 'get_coefs_vars' function")) 
															)
										in 

										!coefs_vars

let is_all_smaller_or_equal_mems coefs_vars1 coefs_vars2 = 	let result = ref true in
															List.iter 	(fun (var1, coef1) ->
																		let coef2 = List.assoc var1 coefs_vars2 in
																		if not ( NumConst.le coef1 coef2 )
																		then 
																			result := false
																		) coefs_vars1; 
															!result



type smaller_term =
	| NotDetermine (*not determined*)
	| First
	| Second

(*return true if 2 linear terms contain the same clocks*)
(*
This function used 
*)
let isSmaller term1 term2 	=	
								(*let result = ref true in*)

								print_message Verbose_standard ("\n	 Analyzing!!!!!");

								let coefs_vars1 = get_coefs_vars term1 in
								(*length of linear term 1*)
								let length_coefs_vars1 = List.length coefs_vars1 in

								let coefs_vars2 = get_coefs_vars term2 in
								(*length of linear term 2*)
								let length_coefs_vars2 = List.length coefs_vars2 in

								print_message Verbose_standard ("\n	 Linear term 1:");
								print_message Verbose_standard ("\n	 Mems/Length:" ^ (string_of_int length_coefs_vars1) );
								
								print_message Verbose_standard ("\n	 Linear term 2:");
								print_message Verbose_standard ("\n	 Mems/Length:" ^ (string_of_int length_coefs_vars2) );

								(*check if there have minus operation inside the linear term or coeff < 0*)
								let checkMinus1 = isMinus term1 in
								let checkMinus2 = isMinus term2 in		

								(*check whether the both linear terms contain negative coef*)
								let (vars1, coefs1) = List.split coefs_vars1 in
								let (vars2, coefs2) = List.split coefs_vars2 in
								
								(*
								let less_than_zero1 = is_mem_in_coef_list_less_than_zero coefs1 in
								let less_than_zero2 = is_mem_in_coef_list_less_than_zero coefs2 in
								*)
								
								(*check*)
								(* if (checkMinus1 || checkMinus2 || less_than_zero1 || less_than_zero2) *)
								if (checkMinus1 || checkMinus2) 
								then
									(*let _ = result := false in*)
									print_message Verbose_standard ("\n	 Contain Minus Sign!!!!!")
								else
									print_message Verbose_standard ("\n	 Ok! Not Contain Minus Sign!!!!!");

								(*check whether 1/2 is subset of the other*)
								let smaller = ref NotDetermine in 
								(* if !result = true
								then ( *)
									(*case: mems term 1 = mems term 2*)
									if length_coefs_vars1 - length_coefs_vars2 = 0 
									then
										(
										print_message Verbose_standard ("\n	 mems of term and term 2 are equal!!!!!");
										(*check 2 sets of vars are equal*)
										if is_var_subset vars2 vars1 && is_var_subset vars1 vars2 
										then (
											print_message Verbose_standard ("\n Sets of vars of term 1 and term 2 are equal!!!!!");
											if is_all_smaller_or_equal_mems coefs_vars1 coefs_vars2
											then
												(
												smaller := First;
												print_message Verbose_standard ("\n	 coefs in term 1 less than or equal coefs in term 2!!!!!");
												)
											else
												(
												if is_all_smaller_or_equal_mems coefs_vars2 coefs_vars1
												then
													(
													smaller := Second;
													print_message Verbose_standard ("\n	 coefs in term 2 less than or equal coefs in term 1!!!!!");
													)
												(*coefs of term 1 = coefs of term 2*)
												else
													(
													smaller := NotDetermine;
													(*result := false;*)
													print_message Verbose_standard ("\n	 Could not determine!!!!!");
													);
												);
											)
										else 
											(
											print_message Verbose_standard ("\n	 not is_var_subset vars2 vars1 && is_var_subset vars1 vars2!!!!!");
											smaller := NotDetermine;
											(*result := false*)
											);
										) 
									else
										(
											if length_coefs_vars1 - length_coefs_vars2 < 0 
											then(
												print_message Verbose_standard ("\n	 length_coefs_vars1 - length_coefs_vars2 < 0!!!!!");
												if is_var_subset vars1 vars2 
												then
													( 
													smaller := First;
													print_message Verbose_standard ("\n	 Set of vars of term 1 is subset of term 2!!!!!");

													(*test*)
													if is_all_smaller_or_equal_mems coefs_vars1 coefs_vars2
													then 
														(
														print_message Verbose_standard ("\n	 coefs in term 1 less than or equal coefs in term 2!!!!!");
														)
													else
														(
														smaller := NotDetermine;
														(*result := false;*)
														print_message Verbose_standard ("\n	 coefs in term 1 not less than or equal coefs in term 2!!!!!");
														);
													(*test*)

													)
												else
													( 
													if length_coefs_vars1 - length_coefs_vars2 > 0 
													then(
														print_message Verbose_standard ("\n	 length_coefs_vars1 - length_coefs_vars2 > 0!!!!!");
														if is_var_subset vars2 vars1 
														then 
															(
															smaller := Second;
															print_message Verbose_standard ("\n	 Set of vars of term 2 is subset of term 1!!!!!");

															(*test*)
															if is_all_smaller_or_equal_mems coefs_vars2 coefs_vars1
															then 
																(
																print_message Verbose_standard ("\n	 coefs in term 2 less than or equal coefs in term 1!!!!!");
																)
															else
																(
																smaller := NotDetermine;
																(*result := false;*)
																print_message Verbose_standard ("\n	 coefs in term 2 not less than or equal coefs in term 1!!!!!");
																);
															(*test*)

															)
														else 
															(
															smaller := NotDetermine;
															(*result := false;*)
															print_message Verbose_standard ("\n	 Could not determine!!!!!");
															);
														);
													);
												);

											

											);

								(* ); *)

								((*!result,*)!smaller)

(*End - Needed functions for part 2*)







(*olde version code, maybe reuse in future*)

(*
(*using *)
let compare coeff1 coeff2 = let first_minus_second = Gmp.Z.compare coeff1 coeff2 in first_minus_second 
*)

(*
let get_coef term = match term with
	| Coef coef -> coef 
	
	(* | Pl of linear_term * linear_term
	| Mi of linear_term * linear_term *)
	(* | Ti of coef * linear_term -> coef *)

	| _ -> raise (SerializationError("get_coef function error")) 	
*)


										


(*
(* check whether list of coefs contains a negative coef*)
let is_mem_in_coef_list_less_than_zero	list_coef	=	let result = ref false in
														List.iter (fun coef ->
															if (NumConst.l coef NumConst.zero)
															then (
																result := true; 
																print_message Verbose_standard ("\n	 coef: "^ NumConst.string_of_numconst coef ^" less than 0");
																);

														) list_coef;
														!result
*)

(*
(* check whether vars in liear term 1 is a subset of linear term 2 *)
let is_var_subset var_list1 var_list2 = let result = ref true in
										List.iter 	(fun var ->	if not (List.mem var var_list2) 
																then 
																	(
																	result := false;
																	print_message Verbose_standard ("\n	 the var1: "^ string_of_int var ^" is not in var_list2 ");
																	);

													) var_list1;
										!result
*)



(*

type smaller_term =
	| NotDetermine (*not determined*)
	| First
	| Second

(*return true if 2 linear terms contain the same clocks*)
let isComparable_linear_terms term1 term2 	=	
												let result = ref true in

												print_message Verbose_standard ("\n	 Analyzing!!!!!");

												let coefs_vars1 = get_coefs_vars term1 in
												(*length of linear term 1*)
												let length_coefs_vars1 = List.length coefs_vars1 in

												let coefs_vars2 = get_coefs_vars term2 in
												(*length of linear term 2*)
												let length_coefs_vars2 = List.length coefs_vars2 in

												print_message Verbose_standard ("\n	 Linear term 1:");
												print_message Verbose_standard ("\n	 Mems/Length:" ^ (string_of_int length_coefs_vars1) );
												
												print_message Verbose_standard ("\n	 Linear term 2:");
												print_message Verbose_standard ("\n	 Mems/Length:" ^ (string_of_int length_coefs_vars2) );

												(*check if there have minus operation inside the linear term or coeff < 0*)
												let checkMinus1 = isMinus term1 in
												let checkMinus2 = isMinus term2 in		

												(*check whether the both linear terms contain negative coef*)
												let (vars1, coefs1) = List.split coefs_vars1 in
												let (vars2, coefs2) = List.split coefs_vars2 in
												
												(*
												let less_than_zero1 = is_mem_in_coef_list_less_than_zero coefs1 in
												let less_than_zero2 = is_mem_in_coef_list_less_than_zero coefs2 in
												*)
												
												(*check*)
												(* if (checkMinus1 || checkMinus2 || less_than_zero1 || less_than_zero2) *)
												if (checkMinus1 || checkMinus2) 
												then
													let _ = result := false in 
													print_message Verbose_standard ("\n	 Contain Minus Sign!!!!!")
												else
													print_message Verbose_standard ("\n	 Not Contain Minus Sign!!!!!");

												(*check whether 1/2 is subset of the other*)
												let smaller = ref NotDetermine in 
												if !result = true
												then (
												(*case: mems term 1 = mems term 2*)
												if length_coefs_vars1 - length_coefs_vars2 = 0 
												then
													(
													print_message Verbose_standard ("\n	 mems of term and term 2 are equal!!!!!");
													(*check 2 sets of vars are equal*)
													if is_var_subset vars2 vars1 && is_var_subset vars1 vars2 
													then (
														print_message Verbose_standard ("\n Sets of vars of term 1 and term 2 are equal!!!!!");
														if is_all_smaller_or_equal_mems coefs_vars1 coefs_vars2
														then
															(
															smaller := First;
															print_message Verbose_standard ("\n	 coefs in term 1 less than or equal coefs in term 2!!!!!");
															)
														else
															(
															if is_all_smaller_or_equal_mems coefs_vars2 coefs_vars1
															then
																(
																smaller := Second;
																print_message Verbose_standard ("\n	 coefs in term 2 less than or equal coefs in term 1!!!!!");
																)
															(*coefs of term 1 = coefs of term 2*)
															else
																(
																smaller := NotDetermine;
																result := false;
																print_message Verbose_standard ("\n	 Could not determine!!!!!");
																);
															);
														)
													else 
														(
														print_message Verbose_standard ("\n	 not is_var_subset vars2 vars1 && is_var_subset vars1 vars2!!!!!");
														smaller := NotDetermine;
														result := false
														);
													) 
												else
													(
														if length_coefs_vars1 - length_coefs_vars2 < 0 
														then(
															print_message Verbose_standard ("\n	 length_coefs_vars1 - length_coefs_vars2 < 0!!!!!");
															if is_var_subset vars1 vars2 
															then
																( 
																smaller := First;
																print_message Verbose_standard ("\n	 Set of vars of term 1 is subset of term 2!!!!!");

																(*test*)
																if is_all_smaller_or_equal_mems coefs_vars1 coefs_vars2
																then 
																	(
																	print_message Verbose_standard ("\n	 coefs in term 1 less than or equal coefs in term 2!!!!!");
																	)
																else
																	(
																	smaller := NotDetermine;
																	result := false;
																	print_message Verbose_standard ("\n	 coefs in term 1 not less than or equal coefs in term 2!!!!!");
																	);
																(*test*)

																)
															else
																( 
																if length_coefs_vars1 - length_coefs_vars2 > 0 
																then(
																	print_message Verbose_standard ("\n	 length_coefs_vars1 - length_coefs_vars2 > 0!!!!!");
																	if is_var_subset vars2 vars1 
																	then 
																		(
																		smaller := Second;
																		print_message Verbose_standard ("\n	 Set of vars of term 2 is subset of term 1!!!!!");

																		(*test*)
																		if is_all_smaller_or_equal_mems coefs_vars2 coefs_vars1
																		then 
																			(
																			print_message Verbose_standard ("\n	 coefs in term 2 less than or equal coefs in term 1!!!!!");
																			)
																		else
																			(
																			smaller := NotDetermine;
																			result := false;
																			print_message Verbose_standard ("\n	 coefs in term 2 not less than or equal coefs in term 1!!!!!");
																			);
																		(*test*)

																		)
																	else 
																		(
																		smaller := NotDetermine;
																		result := false;
																		print_message Verbose_standard ("\n	 Could not determine!!!!!");
																		);
																	);
																);
															);

														

														);

												);

  												(!result,!smaller)
*)

















