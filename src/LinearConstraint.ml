(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 *
 * Description: common definitions for linear terms and constraints (based on PPL)
 *
 * Author:        Etienne Andre
 * 
 * Created:       2010/03/04
 * Last modified: 2014/01/15
 *
 ****************************************************************) 
 

(************************************************************)
(* Modules *)
(************************************************************)
(*open Apron   *)
(*open Lincons0*)

module Ppl = Ppl_ocaml
open Ppl

open Global
open Gmp.Z.Infixes


exception Unsat_exception

(************************************************************)
(* Statistics for the use of PPL *)
(************************************************************)
let ppl_nb_space_dimension = ref 0
	let ppl_t_space_dimension = ref 0.0

let ppl_nb_normalize_linear_term = ref 0
	let ppl_t_normalize_linear_term = ref 0.0

let ppl_nb_true_constraint = ref 0
	let ppl_t_true_constraint = ref 0.0
let ppl_nb_false_constraint = ref 0
	let ppl_t_false_constraint = ref 0.0

let ppl_nb_is_true = ref 0
	let ppl_t_is_true = ref 0.0
let ppl_nb_is_false = ref 0
	let ppl_t_is_false = ref 0.0
let ppl_nb_is_equal = ref 0
	let ppl_t_is_equal = ref 0.0
let ppl_nb_contains = ref 0
	let ppl_t_contains = ref 0.0
let ppl_nb_get_constraints = ref 0
	let ppl_t_get_constraints = ref 0.0
let ppl_nb_get_generators = ref 0
	let ppl_t_get_generators = ref 0.0

let ppl_nb_add_constraints = ref 0
	let ppl_t_add_constraints = ref 0.0

let ppl_nb_hull = ref 0
	let ppl_t_hull = ref 0.0
let ppl_nb_hull_assign_if_exact = ref 0
	let ppl_t_hull_assign_if_exact = ref 0.0
let ppl_nb_difference = ref 0
	let ppl_t_difference = ref 0.0
let ppl_nb_intersection_assign = ref 0
	let ppl_t_intersection_assign = ref 0.0
let ppl_nb_unconstrain = ref 0
	let ppl_t_unconstrain = ref 0.0
let ppl_nb_map = ref 0
	let ppl_t_map = ref 0.0
(*let ppl_nb_preimage = ref 0
	let ppl_t_preimage = ref 0*)
let ppl_nb_remove_dim = ref 0
	let ppl_t_remove_dim = ref 0.0
let ppl_nb_elapse = ref 0
	let ppl_t_elapse = ref 0.0

let ppl_nb_copy_polyhedron = ref 0
	let ppl_t_copy_polyhedron = ref 0.0


let get_statistics () =
	let which_statistics = [
		("space_dimension" , !ppl_nb_space_dimension , !ppl_t_space_dimension) ;
		("normalize_linear_term" , !ppl_nb_normalize_linear_term , !ppl_t_normalize_linear_term) ;
		("true_constraint" , !ppl_nb_true_constraint , !ppl_t_true_constraint) ;
		("false_constraint" , !ppl_nb_false_constraint , !ppl_t_false_constraint) ;
		("is_true" , !ppl_nb_is_true , !ppl_t_is_true) ;
		("is_false" , !ppl_nb_is_false , !ppl_t_is_false) ;
		("is_equals" , !ppl_nb_is_equal , !ppl_t_is_equal) ;
		("contains" , !ppl_nb_contains , !ppl_t_contains) ;
		("get_constraints" , !ppl_nb_get_constraints  , !ppl_t_get_constraints ) ;
		("get_generators" , !ppl_nb_get_generators, !ppl_t_get_generators) ;

		("add_constraints" , !ppl_nb_add_constraints, !ppl_t_add_constraints) ;
		("difference_assign" , !ppl_nb_difference, !ppl_t_difference) ;
		("hull_assign" , !ppl_nb_hull, !ppl_t_hull) ;
		("hull_assign_if_exact" , !ppl_nb_hull_assign_if_exact, !ppl_t_hull_assign_if_exact) ;
		("intersection_assign" , !ppl_nb_intersection_assign, !ppl_t_intersection_assign) ;
		("unconstrain" , !ppl_nb_unconstrain, !ppl_t_unconstrain) ;
		("map" , !ppl_nb_map, !ppl_t_map) ;
(* 		("preimage" , !ppl_nb_preimage, !ppl_t_preimage) ; *)
		("remove_dimension" , !ppl_nb_remove_dim, !ppl_t_remove_dim) ;
		("time_elapsing" , !ppl_nb_elapse, !ppl_t_elapse) ;
		("copy_polyhedron" , !ppl_nb_copy_polyhedron, !ppl_t_copy_polyhedron) ;
	] in
	(* Print info *)
	List.fold_left (fun current_string (name, nb, time) ->
		current_string ^ "\n" ^ (string_of_int nb) ^ " calls to " ^ name ^ "\n"
	^ (if nb > 0 then "Time: " ^ (string_of_float time) ^ " s"
	^ "\nTime per call: " ^ (string_of_float (time /. (float_of_int nb))) ^ " s \n" else "")
	) "" which_statistics;




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
type linear_term =
	  Var of variable
	| Coef of coef
	| Pl of linear_term * linear_term
	| Mi of linear_term * linear_term
	| Ti of coef * linear_term

type p_linear_term = linear_term
type px_linear_term = linear_term
type pxd_linear_term = linear_term


type op =
	| Op_g
	| Op_ge
	| Op_eq


type linear_inequality = Ppl.linear_constraint
type p_linear_inequality = linear_inequality
type px_linear_inequality = linear_inequality
type pxd_linear_inequality = linear_inequality


type linear_constraint = Ppl.polyhedron

type p_linear_constraint = linear_constraint
type px_linear_constraint = linear_constraint
type pxd_linear_constraint = linear_constraint


(* In order to convert a linear_term (with rational coefficients) *)
(* to the corresponding PPL data structure, it is normalized such *)
(* that the only non-rational coefficient is outside the term:    *)
(* p/q * ( ax + by + c ) *)
let rec normalize_linear_term lt =
	(* Statistics *)
	let start = Unix.gettimeofday() in
	ppl_nb_normalize_linear_term := !ppl_nb_normalize_linear_term + 1;
	let result =
	match lt with
		| Var v -> Variable v, NumConst.one
		| Coef c -> (
				let p = NumConst.get_num c in
				let q = NumConst.get_den c in
				Coefficient p, NumConst.numconst_of_zfrac Gmp.Z.one q )
		| Pl (lterm, rterm) -> (
				let lterm_norm, fl = normalize_linear_term lterm in
				let rterm_norm, fr = normalize_linear_term rterm in
				let pl = NumConst.get_num fl in
				let ql = NumConst.get_den fl in
				let pr = NumConst.get_num fr in
				let qr = NumConst.get_den fr in
				(Plus (Times (pl *! qr, lterm_norm), (Times (pr *! ql, rterm_norm)))),
				NumConst.numconst_of_zfrac Gmp.Z.one (ql *! qr))
		| Mi (lterm, rterm) -> (
				let lterm_norm, fl = normalize_linear_term lterm in
				let rterm_norm, fr = normalize_linear_term rterm in
				let pl = NumConst.get_num fl in
				let ql = NumConst.get_den fl in
				let pr = NumConst.get_num fr in
				let qr = NumConst.get_den fr in
				(Minus (Times (pl *! qr, lterm_norm), (Times (pr *! ql, rterm_norm)))),
				NumConst.numconst_of_zfrac Gmp.Z.one (ql *! qr))
		| Ti (fac, term) -> (
				let term_norm, r = normalize_linear_term term in
				let p = NumConst.get_num fac in
				let q = NumConst.get_den fac in
				term_norm, NumConst.mul r (NumConst.numconst_of_zfrac p q))
	in
	(* Statistics *)
	ppl_t_normalize_linear_term := !ppl_t_normalize_linear_term +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result


(** Add on for TA2CLP *)
let string_of_var names variable =
	"V_" ^ (names variable)

	
(************************************************************)
(** Global variables *)
(************************************************************)

(* The number of integer dimensions *)
(* let int_dim = ref 0 *)

(* The number of real dimensions *)
(* let real_dim = ref 0 *)

(* Total number of dimensions *)
let nb_parameters	= ref 0
let nb_clocks		= ref 0
let nb_discrete		= ref 0
let total_dim		= ref 0


(************************************************************)
(* Encapsulation of PPL functions *)
(************************************************************)
let space_dimension x =
	(* Statistics *)
	ppl_nb_space_dimension := !ppl_nb_space_dimension + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_space_dimension x in
	(* Statistics *)
	ppl_t_space_dimension := !ppl_t_space_dimension +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result

let add_constraints x =
	(* Statistics *)
	ppl_nb_add_constraints := !ppl_nb_add_constraints + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_add_constraints x in
	(* Statistics *)
	ppl_t_add_constraints := !ppl_t_add_constraints +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result
	

let get_constraints x =
	(* Statistics *)
	ppl_nb_get_constraints := !ppl_nb_get_constraints + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_get_constraints x in
	(* Statistics *)
	ppl_t_get_constraints := !ppl_t_get_constraints +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result
	

let get_generators poly =	
	(* Statistics *)
	ppl_nb_get_generators := !ppl_nb_get_generators + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_get_generators poly in
	(* Statistics *)
	ppl_t_get_generators := !ppl_t_get_generators +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result


let ppl_intersection_assign x =
	(* Statistics *)
	ppl_nb_intersection_assign := !ppl_nb_intersection_assign + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_intersection_assign x in
	(* Statistics *)
	ppl_t_intersection_assign := !ppl_t_intersection_assign +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result
	
let ppl_remove_dim poly remove =
	(* Statistics *)
	ppl_nb_remove_dim := !ppl_nb_remove_dim + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	ppl_Polyhedron_remove_space_dimensions poly remove;
	(* Statistics *)
	ppl_t_remove_dim := !ppl_t_remove_dim +. (Unix.gettimeofday() -. start)

	
	
(************************************************************)
(* Useful Functions *)
(************************************************************)

(** check the dimensionality of a polyhedron *)
let assert_dimensions poly =
	let ndim = space_dimension poly in
	if not (ndim = !total_dim) then (
		print_error ("Polyhedron has too few dimensions (" ^ (string_of_int ndim) ^ " / " ^ (string_of_int !total_dim) ^ ")");
		raise (InternalError "Inconsistent polyhedron found")
	)

(************************************************************)
(** {2 Linear terms} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

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
let make_pxd_linear_term = make_linear_term



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Add two linear terms *)
let add_linear_terms lt1 lt2 =
	Pl (lt1, lt2)

let add_pxd_linear_terms = add_linear_terms


(** Substract two linear terms *)
let sub_linear_terms lt1 lt2 =
	Mi (lt1, lt2)


(*--------------------------------------------------*)
(* Evaluation *)
(*--------------------------------------------------*)

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
			  with _ -> raise(InternalError ("No value was found for variable " ^ (string_of_int v) ^ ", while trying to evaluate a linear term; this variable was probably not defined.")))
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


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

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
(** {2 Linear inequalities} *)
(************************************************************)

(************** TO DO : minimize inequalities as soon as they have been created / changed *)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(* Functions *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a linear inequality using a linear term and an operator *)
let make_linear_inequality linear_term op =
	let ppl_term, r = normalize_linear_term linear_term in
	let p = NumConst.get_num r in
	let lin_term = Times (p, ppl_term) in
	let zero_term = Coefficient Gmp.Z.zero in
	match op with
		| Op_g -> Greater_Than (lin_term, zero_term)
		| Op_ge -> Greater_Or_Equal (lin_term, zero_term)
		| Op_eq -> Equal (lin_term, zero_term)


let make_pxd_linear_inequality = make_linear_inequality

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

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

(* Transform a strict inequality into a not strict inequality *)
let strict_to_not_strict_inequality inequality =
	match inequality with
		|Less_Than (x,y) -> Less_Or_Equal (x,y)
		|Greater_Than (x,y) -> Greater_Or_Equal (x,y)
		|_ -> inequality




(*--------------------------------------------------*)
(* Pi0-compatibility *)
(*--------------------------------------------------*)

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


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)				   	


let is_zero_coef = function
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
		| Greater_Than_RS -> " > "
		| Greater_Or_Equal_RS -> " >= " in
	lstr ^ opstr ^ rstr

let string_of_p_linear_inequality = string_of_linear_inequality


(************************************************************)
(** {2 Linear Constraints} *)
(************************************************************)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Initialization} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Set the number of dimensions *)
let set_dimensions nb_p nb_c nb_d =
	nb_parameters	:= nb_p;
	nb_clocks 		:= nb_c;
	nb_discrete		:= nb_d;
	total_dim		:= nb_p + nb_c + nb_d;
	if debug_mode_greater Debug_high then(
		print_message Debug_high ("\nDimensions set");
		print_message Debug_high ("  nb_parameters := " ^ (string_of_int !nb_parameters));
		print_message Debug_high ("  nb_clocks := " ^ (string_of_int !nb_clocks));
		print_message Debug_high ("  nb_discrete := " ^ (string_of_int !nb_discrete));
		print_message Debug_high ("  total_dim := " ^ (string_of_int !total_dim));
	);
	()


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a false constraint *)
let false_constraint () =
	(* Statistics *)
	ppl_nb_false_constraint := !ppl_nb_false_constraint + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_new_NNC_Polyhedron_from_space_dimension !total_dim Empty in
	(* Statistics *)
	ppl_t_false_constraint := !ppl_t_false_constraint +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result

let p_false_constraint = false_constraint
let pxd_false_constraint = false_constraint



(** Create a true constraint *)
let true_constraint () = 
	(* Statistics *)
	ppl_nb_true_constraint := !ppl_nb_true_constraint + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_new_NNC_Polyhedron_from_space_dimension !total_dim Universe in
	(* Statistics *)
	ppl_t_true_constraint := !ppl_t_true_constraint +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result

let p_true_constraint = true_constraint
let px_true_constraint = true_constraint
let pxd_true_constraint = true_constraint


(** Create a linear constraint from a list of linear inequalities *)
let make inequalities = 
	let poly = true_constraint () in
	add_constraints poly inequalities;
	assert_dimensions poly;
  poly


let make_p_constraint = make
let make_px_constraint = make
let make_pxd_constraint = make


(** Create a linear constraint from a single point *)
(* WARNING: non-robust (no check for variable existence) *)
let p_constraint_of_point (thepoint : (variable * coef) list) =
	let inequalities =
	List.map (fun (variable , value) ->
		(* Create linear inequality "variable = value" *)
		make_linear_inequality
			(* Create linear term "variable - value" *)
			(make_p_linear_term [NumConst.one, variable] (NumConst.neg value))
			Op_eq
	) thepoint
	in
	make_p_constraint inequalities



(** "linear_constraint_of_clock_and_parameters x ~ d neg" will create a linear_constraint x ~ d, with "x" a clock, "~" in {>, >=, =}, "d" a PConstraint.linear_term, and "neg" indicates whether x and d should be kept in this direction or reversed (viz., "x > p1 true" generates "x > p1" whereas "x >= p1+p2 false" generates "p1+p2 >= x" *)
let linear_constraint_of_clock_and_parameters (x : variable) (op : op) (d : linear_term) (direction : bool) =
	(* Create a linear term made of x *)
	let lt_x = make_linear_term [NumConst.one, x] NumConst.zero in
	(* Handle order *)
	let lt =
		if direction
			then sub_linear_terms d lt_x
		else sub_linear_terms lt_x d
	in
	(* Create the constraint with the operator *)
	make [make_linear_inequality lt op]

let px_linear_constraint_of_clock_and_parameters = linear_constraint_of_clock_and_parameters
let pxd_linear_constraint_of_clock_and_parameters = linear_constraint_of_clock_and_parameters


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a constraint is false *)
let is_false c =
	(* Statistics *)
	ppl_nb_is_false := !ppl_nb_is_false + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_is_empty c in
	(* Statistics *)
	ppl_t_is_false := !ppl_t_is_false +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result

let p_is_false = is_false


(** Check if a constraint is true *)
let is_true c =
	(* Statistics *)
	ppl_nb_is_true := !ppl_nb_is_true + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_is_universe c in
	(* Statistics *)
	ppl_t_is_true := !ppl_t_is_true +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result

let p_is_true = is_true
let pxd_is_true = is_true



(** Check if a constraint is satisfiable *)
let is_satisfiable c = not (is_false c)
let px_is_satisfiable = is_satisfiable
let pxd_is_satisfiable = is_satisfiable

(** Check if 2 constraints are equal *)
let is_equal c1 c2 =
	(* Statistics *)
	ppl_nb_is_equal := !ppl_nb_is_equal + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_equals_Polyhedron c1 c2 in
	(* Statistics *)
	ppl_t_is_equal := !ppl_t_is_equal +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result

let p_is_equal = is_equal
let px_is_equal = is_equal

(** Check if a constraint is included in another one *)
let is_leq x y =
	(* Statistics *)
	ppl_nb_contains := !ppl_nb_contains + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_contains_Polyhedron y x in
	(* Statistics *)
	ppl_t_contains := !ppl_t_contains +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result

let p_is_leq = is_leq
let px_is_leq = is_leq


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Get the number of inequalities of a constraint *)
let nb_inequalities linear_constraint = 
	(* First check if true *)
	if p_is_true linear_constraint || p_is_false linear_constraint then 0
	else
	(* Get a list of linear inequalities *)
	let list_of_inequalities = get_constraints linear_constraint in
	List.length list_of_inequalities

let p_nb_inequalities = nb_inequalities




(** Get the linear inequalities *)
let get_inequalities =
(*** TODO: add counter ***)
	ppl_Polyhedron_get_constraints


(** Return true if the variable is constrained in a linear_constraint *)
(*** TODO: add counter ***)
let is_constrained =
	ppl_Polyhedron_constrains

let pxd_is_constrained = is_constrained


(** Return the list of variables from l that are constrained in the constraint *)
(* WARNING: no idea of the efficiency of this way of doing *)
(* (but not crucial because only called for preprocessing) *)
let find_variables variables_list linear_constraint =
	List.filter (fun variable ->
		is_constrained linear_constraint variable
	) variables_list


let pxd_find_variables = find_variables


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** String for the false constraint *)
let string_of_false = "false"


(** String for the true constraint *)
let string_of_true = "true"


(** Convert a linear constraint into a string *)
let string_of_linear_constraint names linear_constraint =
	(* First check if true *)
	if is_true linear_constraint then string_of_true
	(* Then check if false *)
	else if is_false linear_constraint then string_of_false
	else
	(* Get a list of linear inequalities *)
	let list_of_inequalities = get_constraints linear_constraint in
	" " ^
	(string_of_list_of_string_with_sep
		"\n& "
		(List.map (string_of_linear_inequality names) list_of_inequalities)
	)

let string_of_p_linear_constraint = string_of_linear_constraint 
let string_of_px_linear_constraint = string_of_linear_constraint 
let string_of_pxd_linear_constraint = string_of_linear_constraint 


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
let copy linear_constraint =
	(* Statistics *)
	ppl_nb_copy_polyhedron := !ppl_nb_copy_polyhedron + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	(* Statistics *)
	ppl_t_copy_polyhedron := !ppl_t_copy_polyhedron +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result


let p_copy = copy
let px_copy = copy
let pxd_copy = copy

(** Perform the intersection of a linear constrain with a list of constraints (with side effect) *)
let intersection_assign linear_constraint constrs =
(* 	try( *)
		List.iter (fun poly ->
			(* Perform the actual intersection *)
			ppl_intersection_assign linear_constraint poly;
			(* Check satisfiability *)
			(** ACTUALLY: this does not bring anything on the examples I tried -- on the contrary! *)
(* 			if not (is_satisfiable linear_constraint) then raise Unsat_exception; *)
		) constrs;
		assert_dimensions linear_constraint
	(* If false: stop *)
(* 	) with Unsat_exception -> () *)

let p_intersection_assign = intersection_assign
let px_intersection_assign = intersection_assign
let pxd_intersection_assign = intersection_assign

let px_intersection_assign_p = intersection_assign


(** Performs the intersection of a list of linear constraints *)
let intersection linear_constraints =
	let result_poly = true_constraint () in
	intersection_assign result_poly linear_constraints;
	result_poly
(*	try(
		List.iter (fun poly ->
			if not (is_satisfiable poly) then raise Unsat_exception;
			intersection_assign result_poly poly
		) linear_constraints;
		assert_dimensions result_poly;
		result_poly
	) with Unsat_exception -> false_constraint ()*)

let p_intersection = intersection
let px_intersection = intersection
let pxd_intersection = intersection

(** Perform the hull (version with side effect) *)
let hull_assign linear_constraint1 linear_constraint2 =
	(* Statistics *)
	ppl_nb_hull := !ppl_nb_hull + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_poly_hull_assign linear_constraint1 linear_constraint2 in
	(* Statistics *)
	ppl_t_hull := !ppl_t_hull +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result
	

(** Perform the hull if the result is exact (version with side effect) *)
let hull_assign_if_exact linear_constraint1 linear_constraint2 =
	(* Statistics *)
	ppl_nb_hull_assign_if_exact := !ppl_nb_hull_assign_if_exact + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_poly_hull_assign_if_exact linear_constraint1 linear_constraint2 in
	(* Statistics *)
	ppl_t_hull_assign_if_exact := !ppl_t_hull_assign_if_exact +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result

let px_hull_assign_if_exact = hull_assign_if_exact


(** Perform difference (version with side effect) *)
let difference_assign linear_constraint1 linear_constraint2 =
	(* Statistics *)
	ppl_nb_difference := !ppl_nb_difference + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	let result = ppl_Polyhedron_poly_difference_assign linear_constraint1 linear_constraint2 in
	(* Statistics *)
	ppl_t_difference := !ppl_t_difference +. (Unix.gettimeofday() -. start);
	(* Return result *)
	result

	
(** Eliminate a set of variables, side effects version *)
let hide_assign variables linear_constraint =
	(* Only hide a non-empty list *)
	if List.length variables > 0 then (
		(* debug output *)
		if debug_mode_greater Debug_total then (
			print_message Debug_total "About to hide:";
			List.iter (fun v -> print_message Debug_total ("  - v" ^ string_of_int v)) variables;
		);
		(* Statistics *)
		ppl_nb_unconstrain := !ppl_nb_unconstrain + 1;
		let start = Unix.gettimeofday() in
		(* Actual call to PPL *)
		ppl_Polyhedron_unconstrain_space_dimensions linear_constraint variables;
		(* Statistics *)
		ppl_t_unconstrain := !ppl_t_unconstrain +. (Unix.gettimeofday() -. start);
		assert_dimensions linear_constraint
	)

let px_hide_assign = hide_assign
let pxd_hide_assign = hide_assign


(** Eliminate (using existential quantification) a set of variables in a linear constraint *)
let hide variables linear_constraint =



	(*** TO OPTIMIZE: check if variables is different from [] ***)
	
	
	
	(* copy polyhedron, as PPL function has sideeffects *)
	let poly = copy linear_constraint in
	(* Call the function with side-effects *)
	hide_assign variables poly;
	poly







(** Eliminate (using existential quantification) all non-parameters (clocks and discrete) in a px_linear constraint *)
let px_hide_nonparameters_and_collapse linear_constraint = 
	let nonparameters = list_of_interval !nb_parameters (!total_dim - 1) in
		if debug_mode_greater Debug_total then
			print_message Debug_total (
				"Function 'LinearConstraint.px_hide_nonparameters_and_collapse': hiding variables "
				^ (string_of_list_of_string_with_sep ", " (List.map string_of_int nonparameters) )
				^ "."
			);
		hide nonparameters linear_constraint 



let pxd_hide_discrete_and_collapse linear_constraint = 
	let discretes = list_of_interval (!nb_parameters + !nb_clocks) (!total_dim - 1) in
	hide discretes linear_constraint



(*(** Eliminate (using existential quantification) the non-parameters in a pxd_linear constraint, and remove the corresponding dimensions *)

(*** TO IMPLEMENT !!! ***)

let pxd_hide_nonparameters_and_collapse = 
	raise (InternalError "Not implemented!!!")*)





(** Add nb_dimensions to a linear_constraint *)
let add_dimensions nb_dimensions linear_constraint =

	(* TODO: add a counter *)
	
	ppl_Polyhedron_add_space_dimensions_and_project linear_constraint nb_dimensions


let pxd_add_dimensions = add_dimensions




(** Remove the highest nb_dimensions from a linear_constraint *)
let remove_dimensions nb_dimensions linear_constraint =

	(* TODO: add a counter *)
	
	(* Compute the highest space dimension to keep *)
	let current_space_dimension = ppl_Polyhedron_space_dimension linear_constraint in
	let new_space_dimension = current_space_dimension - nb_dimensions in

	(* Print some information *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("Function 'remove_dimensions': removing " ^ (string_of_int nb_dimensions) ^ " from " ^ (string_of_int current_space_dimension) ^ ", i.e., keeping " ^ (string_of_int new_space_dimension) ^ ".");
	);
	
	(* Projects the polyhedron referenced to by handle onto the first space_dimension dimensions *)
	ppl_Polyhedron_remove_higher_space_dimensions linear_constraint new_space_dimension

let pxd_remove_dimensions = remove_dimensions



(** rename variables in a constraint, with side effects *)
let rename_variables_assign list_of_couples linear_constraint =
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
	let complete_list = add_id joined_couples (!total_dim - 1) in
  (* debug output *)
	if debug_mode_greater Debug_high then (
		let ndim = space_dimension linear_constraint in
		print_message Debug_high ("mapping space dimensions, no. dimensions is " ^ string_of_int ndim);
		List.iter (fun (a,b) -> (print_message Debug_high ("map v" ^ string_of_int a ^ " -> v" ^ string_of_int b))) complete_list;
	);
	(* perfom the mapping *)
	(* Statistics *)
	ppl_nb_map := !ppl_nb_map + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	ppl_Polyhedron_map_space_dimensions linear_constraint complete_list;
	(* Statistics *)
	ppl_t_map := !ppl_t_map +. (Unix.gettimeofday() -. start);
	assert_dimensions linear_constraint

let pxd_rename_variables_assign = rename_variables_assign


(** Rename variables in a constraint *)
let rename_variables list_of_couples linear_constraint =
	(* copy polyhedron, as ppl function has sideeffects *)
	let poly = copy linear_constraint in
	rename_variables_assign list_of_couples poly;
	poly

(* let pxd_rename_variables = rename_variables *)

(* Generic time elapsing function *)
(* 'reverse_direction' should be minus_one for growing, one for decreasing *)
let time_elapse_gen_assign reverse_direction variables_elapse variables_constant linear_constraint =
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
	let linear_constraint_time = make (List.rev_append inequalities_elapse inequalities_constant) in
	(* Assign the time elapsing using PPL *)
	(* Statistics *)
	ppl_nb_elapse := !ppl_nb_elapse + 1;
	let start = Unix.gettimeofday() in
	(* Actual call to PPL *)
	ppl_Polyhedron_time_elapse_assign linear_constraint linear_constraint_time;
	(* Statistics *)
	ppl_t_elapse := !ppl_t_elapse +. (Unix.gettimeofday() -. start)

(** Time elapsing function *)
let time_elapse_assign = time_elapse_gen_assign NumConst.minus_one

let pxd_time_elapse_assign = time_elapse_assign



let time_elapse variables_elapse variables_constant linear_constraint =
	let linear_constraint = copy linear_constraint in
	time_elapse_assign variables_elapse variables_constant linear_constraint;
	linear_constraint

	
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

(** Time elapsing function, in backward direction *)
let time_backward_assign variables_elapse variables_constant linear_constraint =
	(* 1) Apply generic function *)
	time_elapse_gen_assign NumConst.one variables_elapse variables_constant linear_constraint;
	
	(* 2) Constrain the elapsing variables to be non-negative! *)
	(* Create the inequalities var >= 0, for var in variables_elapse *)
	let inequalities_nonnegative = List.map (fun variable ->
		(* Create a linear term *)
		let linear_term = make_linear_term [(NumConst.one, variable)] NumConst.zero in
		(* Create the inequality *)
		make_linear_inequality linear_term Op_ge
	) variables_elapse in
	(* Take intersection *)
	intersection_assign linear_constraint [(make inequalities_nonnegative)]
	



(** Perform an operation (?) on a set of variables: the first variable list will elapse, the second will remain constant *)
(** TODO: describe better *)
(** WARNING: this function is certainly not optimized at all! somehow we don't care considering it's not called "often" in IMITATOR *)
let grow_to_infinite_assign variables_elapse variables_constant linear_constraint =
	(* Compute all variables *)
	let all_variables = List.rev_append variables_elapse variables_constant in
	(* Perform time elapsing on each variable *)
	List.iter (fun variable ->
		time_elapse_assign [variable] (list_diff all_variables [variable]) linear_constraint;
	) variables_elapse;
	(* The end *)
	()


(** Perform an operation (?) on a set of variables: the first variable list will elapse, the second will remain constant *)
(** TODO: describe better *)
(** WARNING: this function is certainly not optimized at all! somehow we don't care considering it's not called "often" in IMITATOR *)
let grow_to_zero_assign variables_elapse variables_constant linear_constraint =
	(* Compute all variables *)
	let all_variables = List.rev_append variables_elapse variables_constant in
	(* Perform time elapsing on each variable *)
	List.iter (fun variable ->
		time_backward_assign [variable] (list_diff all_variables [variable]) linear_constraint;
	) variables_elapse;
	(* The end *)
	()



(** Replace all strict inequalities with non-strict (and keeps others unchanged) within a p_linear_constraint *)
let render_non_strict_p_linear_constraint k =
	(* Get the list of inequalities *)
	let inequality_list = get_inequalities k in 
	(* Replace inequelities and convert back to a linear_constraint *)
	make_p_constraint (List.map strict_to_not_strict_inequality inequality_list)



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Pi0-compatibility} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a linear constraint is pi0-compatible *)
let is_pi0_compatible pi0 linear_constraint =
	(* Get a list of linear inequalities *)
	let list_of_inequalities = get_constraints linear_constraint in
	(* Check the pi0-compatibility for all *)
	List.for_all (is_pi0_compatible_inequality pi0) list_of_inequalities


(** Compute the pi0-compatible and pi0-incompatible inequalities within a constraint *)
let partition_pi0_compatible pi0 linear_constraint =
	(* Get a list of linear inequalities *)
	let list_of_inequalities = get_constraints linear_constraint in
	(* Partition *)
	List.partition (is_pi0_compatible_inequality pi0) list_of_inequalities



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to GrML} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

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
		let list_of_inequalities = get_constraints linear_constraint in
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

let grml_of_pxd_linear_constraint = grml_of_linear_constraint


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion between types of constraints } *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

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


(** Convert a PX into a PXD constraint by extending the number of dimensions *)
let pxd_of_px_constraint c = copy c



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Brute-force casts (argh) } *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** "cast_p_of_pxd_linear_term p c" converts a PXD-constraint p to a P-constraint ; if c then a test if performed to check casting validity *)
let cast_p_of_pxd_linear_term p c = p (*** WARNING! should be copied here! *)
let cast_p_of_pxd_linear_constraint p c = copy p


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Plot interactions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)


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
		|Dot (a,b) when ((min_abs <= a) & (a <= max_abs) & (min_ord <= b) & (b <= max_ord)) -> true
		|_ -> false


(* convert a linear constraint into two lists, one containing the points and the other containing the ray *)
let shape_of_poly x y linear_constraint =

	(* Print some information *)
	print_message Debug_total ("Entering generate_points");
	
	(* Get the current number of dimensions *)
	let space_dimension = ppl_Polyhedron_space_dimension linear_constraint in

	let poly = copy linear_constraint in
	(* project on variables x,y *)
	let remove = ref [] in
	for i = 0 to space_dimension - 1 do
		if i <> x && i <> y then
			remove := i :: !remove
	done;
	(* Statistics *)
	ppl_nb_remove_dim := !ppl_nb_remove_dim + 1;
	ppl_remove_dim poly !remove;
	let generators = get_generators poly in
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
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("Entering generate_points");
		print_message Debug_total ("Constraint: " ^ (string_of_linear_constraint (fun i->"v" ^ (string_of_int i)) linear_constraint));
	);

	let (points,ray) = shape_of_poly x y linear_constraint in
	(* add to points some new points generated by the ray elements *)
	let point_list = ref points in
	for i=0 to List.length points -1 do 
		for j=0 to List.length ray -1 do
			match List.nth ray j with
				|(u,v) when ((u>=0.) & (v>=0.)) -> point_list:=(max ((max_abs)*.(fst(List.nth ray j))) (fst (List.nth points i)) , max ((max_ord)*.(snd(List.nth ray j))) (snd (List.nth points i)))::!point_list
				|(u,v) when ((u<0.) & (v>=0.)) -> point_list:=(min ((min_abs)*.(fst(List.nth ray j))) (fst (List.nth points i)) , max ((max_ord)*.(snd(List.nth ray j))) (snd (List.nth points i)))::!point_list
				|(u,v) when ((u>=0.) & (v<0.)) -> point_list:=(max ((max_abs)*.(fst(List.nth ray j))) (fst (List.nth points i)) , min ((min_ord)*.(snd(List.nth ray j))) (snd (List.nth points i)))::!point_list
				|(u,v) when ((u<0.) & (v<0.)) -> point_list:=(min ((min_abs)*.(fst(List.nth ray j))) (fst (List.nth points i)) , min ((min_ord)*.(snd(List.nth ray j))) (snd (List.nth points i)))::!point_list
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
		while !i <= (List.length ray -1) & (!add_point = (Nowhere,Nowhere)) do 
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
			if List.length !l1 <> 0 & List.length !l2 <> 0 then add_point:=(North,East)
			else if List.length !l2 <> 0 & List.length !l3 <> 0 then add_point:=(East,South)
			else if List.length !l3 <> 0 & List.length !l4 <> 0 then add_point:=(South,West)
			else if List.length !l4 <> 0 & List.length !l4 <> 0 then add_point:=(West,North)
			(* if two intersection points are on two opposite border then mark it in add_point *)
			else if List.length !l1 <> 0 & List.length !l3 <> 0 then add_point:=(North,South)
			else if List.length !l2 <> 0 & List.length !l4 <> 0 then add_point:=(East,West)
			else if List.length !l3 <> 0 & List.length !l1 <> 0 then add_point:=(South,North)
			else if List.length !l4 <> 0 & List.length !l2 <> 0 then add_point:=(West,East);
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
(*** WARNING: does not work if parameters are negative (which should not happen but...) *)
let plot_2d x y linear_constraint min_abs min_ord max_abs max_ord =

	(* Print some information *)
	print_message Debug_total "Entering 'plot_2d'";

	let shape = generate_points x y linear_constraint min_abs min_ord max_abs max_ord in

	let str = List.fold_left (fun s (x, y) -> 
		s ^ (string_of_float x) ^ " " ^ (string_of_float y) ^ "\n"
	) "" (snd shape) in 
	((fst shape), str)



(************************************************************)
(** {2 PDBMs} *)
(************************************************************)
(** WARNING! work in progress *)

(* Relationship used in PDBMs *)
type pdbm_rel =
	(* <= *)
	| PDBM_leq
	(* < *)
	| PDBM_l

(* The actual PDBM: a matrix of size nb_clocks+1 *)
(* The 0-clock is the LAST clock for readability issues *)
type pdbm = (px_linear_term * pdbm_rel) array array


(* Constrained PDBM = (C, D) *)
type cpdbm = p_linear_constraint * pdbm

(** Create a CPDBM with nb_clocks clocks *)
let make_cpdbm nb_clocks =
	(* All elements initialized with ( 0 , <= ) *)
	(** WARNING: check that all elements are different ! *)
	let init_element() =
		make_p_linear_term [] NumConst.zero
		,
		PDBM_leq
	in
	p_true_constraint()
	,
	Array.make_matrix (nb_clocks+1) (nb_clocks+1) (init_element())

(* Convert a PDBM into a linear constraint *)
let px_linear_constraint_of_pdbm pdbm =
	let nb_clocks = Array.length pdbm in
	(* Convert PDBM relationship to linear_constraint relationship *)
	(*** BADPROG / WARNING: not the same direction! so bad..... *)
	let op_of_pdbm_rel = function
		| PDBM_leq -> Op_ge
		| PDBM_l -> Op_g
	in
	
	()
(*	(* 1) Convert all inequalities (eij, ~) into xi - xj ~ eij *)
	(* CHECK EFFICIENCY (alternative: gather all inequalities, then intersect using px_intersection) *)
	let linear_constraint = px_true_constraint () in
	(* For each row *)
	for i = 0 to nb_clocks - 1 do
		(* For each column *)
		for j = 0 to nb_clocks - 1 do
			(* Create the inequality *)
			(*** BADPROG / WARNING: reverse order because operators not in the same direction! so bad..... *)
			let inequality = make_pxd_linear_inequality SOMETHING 
			(***** JE SUIS LAAAAAA !!!!!!!!!!!! ****)
			px_intersection_assign linear_constraint []
		done;

	done;
	
	let inequalities = Array.to_list(
		Array.map (fun row -> )
	px_intersection*)
	
	(* 2) Convert the last row and last column corresponding to the 0-clock *)