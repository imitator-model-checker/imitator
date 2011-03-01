(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2010/03/04
 * Last modified: 2010/03/16 TEST TOTO
 *
 ****************************************************************)

(**************************************************)
(* Modules *)
(**************************************************)
(*open Apron   *)
(*open Lincons0*)

module Ppl = Ppl_ocaml
open Ppl

open Global
open Gmp.Z.Infixes

(**************************************************)
(* TYPES *)
(**************************************************)

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

type op =
	| Op_g
	| Op_ge
	| Op_eq

(*type linear_inequality = Lincons0.t*)
type linear_inequality = Ppl.linear_constraint

(*type linear_constraint = Polka.strict Polka.t Abstract0.t *)
type linear_constraint = Ppl.polyhedron

(* split a rational number into numerator and denominator *)
let split_q r = 
	let p = NumConst.get_num r in
	let q = NumConst.get_den r in
	p, q


(* In order to convert a linear_term (with rational coefficients) *)
(* to the corresponding PPL data structure, it is normalized such *)
(* that the only non-rational coefficient is outside the term:    *)
(* p/q * ( ax + by + c ) *)
let rec normalize_linear_term lt =
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
					
	
(**************************************************)
(** Global variables *)
(**************************************************)

(* The number of integer dimensions *)
let int_dim = ref 0

(* The number of real dimensions *)
let real_dim = ref 0

(* Total number of dimensions *)
let total_dim = ref 0

let nb_dimensions () = !total_dim

(**************************************************)
(* Useful Functions *)
(**************************************************)

(** check the dimensionality of a polyhedron *)
let assert_dimensions poly =
	let ndim = ppl_Polyhedron_space_dimension poly in
	if not (ndim = !total_dim) then (
		print_error ("Polyhedron has too few dimensions (" ^ (string_of_int ndim) ^ " / " ^ (string_of_int !total_dim) ^ ")");
		raise (InternalError "Inconsistent polyhedron found")	
	)			 	

(**************************************************)
(** {2 Linear terms} *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a linear term using a list of coef and variables, and a constant *)
let make_linear_term members coef =
	List.fold_left (fun term head ->
		let (c, v) = head in 
			if c = NumConst.one then
				Pl (Var v, term)
			else
				Pl ((Ti (c, Var v), term))
	)	(Coef coef) members

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Add two linear terms *)
let add_linear_terms lt1 lt2 =
	Pl (lt1, lt2)


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
				

(**************************************************)
(** {2 Linear inequalities} *)
(**************************************************)

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

(** get the NumConst operator for a ppl relation symbol *)
let relop_from_rs = function
	| Less_Than_RS -> NumConst.l
	| Less_Or_Equal_RS -> NumConst.le
	| Equal_RS -> NumConst.equal
	| Greater_Than_RS -> NumConst.g
	| Greater_Or_Equal_RS -> NumConst.ge

(** inverts a relation symbol, where '=' maps to '=' *)
let invert_rs = function
	| Less_Than_RS -> Greater_Than_RS
	| Less_Or_Equal_RS -> Greater_Or_Equal_RS 
	| Equal_RS -> Equal_RS
	| Greater_Than_RS -> Less_Than_RS
	| Greater_Or_Equal_RS -> Less_Or_Equal_RS 

let make_linear_inequality_ppl lterm op rterm =
	let ppl_lterm, cl = normalize_linear_term lterm in
	let ppl_rterm, cr = normalize_linear_term rterm in
	let pl = NumConst.get_num cl in
	let ql = NumConst.get_den cl in
	let pr = NumConst.get_num cr in
	let qr = NumConst.get_den cr in
	let sl = pl *! qr in
	let sr = pr *! ql in
	let new_lterm = Times (sl, ppl_lterm) in
	let new_rterm = Times (sr, ppl_rterm) in
	build_linear_inequality new_lterm new_rterm op

(** evaluate a linear inequality for a given valuation *)
let evaluate_linear_inequality valuation_function linear_inequality =
	let lterm, rterm, op = split_linear_inequality linear_inequality in
	(* evaluate both terms *)
	let lval = evaluate_linear_term_ppl valuation_function lterm in
	let rval = evaluate_linear_term_ppl valuation_function rterm in
	(* compare according to the relation symbol *)
	(relop_from_rs op) lval rval	

(*--------------------------------------------------*)
(* Pi0-compatibility *)
(*--------------------------------------------------*)

(** Check if a linear inequality is pi0-compatible *)
let is_pi0_compatible_inequality pi0 linear_inequality =
	evaluate_linear_inequality pi0 linear_inequality

(** Negate a linear inequality *)
let negate_inequality ineq =
	match ineq with
		| Less_Than (lterm, rterm) -> Greater_Or_Equal (lterm, rterm)
		| Less_Or_Equal (lterm, rterm) -> Greater_Than (lterm, rterm)
		| Greater_Than (lterm, rterm) -> Less_Or_Equal (lterm, rterm)
		| Greater_Or_Equal (lterm, rterm) -> Less_Than (lterm, rterm)
		| Equal (lterm, rterm) -> (
			raise (InternalError "cannot negate equality")
		)
	
(** Negate a linear inequality; for an equality, perform the pi0-compatible negation *)
let negate_wrt_pi0 pi0 linear_inequality = 
	match linear_inequality with
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
		| _ -> negate_inequality linear_inequality


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)				   	


let is_zero_coef = function
	| Coefficient c -> c =! Gmp.Z.zero
	| _ -> false

let is_one_coef = function
	| Coefficient c -> c =! Gmp.Z.one
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
		
let compact_diff lexpr rexpr =
	if is_zero_coef lexpr then (
		Unary_Minus rexpr
	) else (
		if is_zero_coef rexpr then (
			lexpr
		) else (
			Minus (lexpr, rexpr)		
		))

let compact_prod c expr =
	if c =! Gmp.Z.zero then (
		Coefficient Gmp.Z.zero
	) else (
		if c =! Gmp.Z.one then (
			expr
		) else (
			Times (c, expr)
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

(**************************************************)
(** {2 Linear Constraints} *)
(**************************************************)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

let copy constr =
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron constr in
	assert_dimensions poly;
  poly
	 

(** Create a false constraint *)
let false_constraint () =	
	ppl_new_NNC_Polyhedron_from_space_dimension !total_dim Empty

(** Create a true constraint *)
let true_constraint () = 
	ppl_new_NNC_Polyhedron_from_space_dimension !total_dim Universe

(** Create a linear constraint from a list of linear inequalities *)
let make inequalities = 
	let poly = true_constraint () in
	ppl_Polyhedron_add_constraints poly inequalities;
	assert_dimensions poly;
  poly
	
(** Create a linear constraint x = y & ... for a list of variable pairs (x, y) *) 
let make_equalities variable_pairs =
	let equalities = List.map (fun (x, y) -> 
		Equal (Variable x, Variable y)
	) variable_pairs in
	make equalities
	
(** Create a linear constraint x = c & ... for list of variables and constants *)
let make_set_variables value_pairs =
	let equalities = List.map (fun (v, c) -> 
		let p, q = split_q c in
		if q =! Gmp.Z.one then
			Equal (Variable v, Coefficient p)
		else
			Equal (Times (q, Variable v), Coefficient p)
	) value_pairs in
	make equalities
	
(** Create a linear constraint x = c & y = c ... for list of variables and a constant *)	
let make_set_all_variables variables c =
	let p, q = split_q c in
	let build_equality =
		if q =! Gmp.Z.one then
			fun v -> Equal (Variable v, Coefficient p)
		else
			fun v -> Equal (Times (q, Variable v), Coefficient p)
		in	
	let equalities = List.map build_equality variables in
	make equalities

(** Creates a linear constraint x in [min, max] *)
let make_interval (v, min, max) = 
	let lower_bound = 
		let p,q = split_q min in Greater_Or_Equal (Times (q, Variable v), Coefficient p) in
	let upper_bound = 
		let p,q = split_q max in Less_Or_Equal (Times (q, Variable v), Coefficient p) in
	make [lower_bound; upper_bound]
	
(** Creates linear constraints x_i in [min_i, max_i] *)
let make_box bounds =
	let lower_bounds = List.map (fun (v, min, _) ->
		let p,q = split_q min in Greater_Or_Equal (Times (q, Variable v), Coefficient p)) bounds in
	let upper_bounds = List.map (fun (v, _, max) ->
		let p,q = split_q max in Less_Or_Equal (Times (q, Variable v), Coefficient p)) bounds in
	make (lower_bounds @ upper_bounds)

	
(** Set the constraint manager *)
let set_manager int_d real_d =
	int_dim := int_d;
	real_dim := real_d;
	total_dim := int_d + real_d 

	
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a constraint is false *)
let is_false = ppl_Polyhedron_is_empty

(** Check if a constraint is true *)
let is_true = ppl_Polyhedron_is_universe

(** Check if a constraint is satisfiable *)
let is_satisfiable = fun c -> not (is_false c)

(** Check if 2 constraints are equal *)
let is_equal = ppl_Polyhedron_equals_Polyhedron

(** Check if a constraint is included in another one *)
let is_leq x y  = ppl_Polyhedron_contains_Polyhedron y x

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Pi0-compatibility} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a linear constraint is pi0-compatible *)
let is_pi0_compatible pi0 linear_constraint =
	(* Get a list of linear inequalities *)
	let list_of_inequalities = ppl_Polyhedron_get_constraints linear_constraint in
	(* Check the pi0-compatibility for all *)
	List.for_all (is_pi0_compatible_inequality pi0) list_of_inequalities


(** Compute the pi0-compatible and pi0-incompatible inequalities within a constraint *)
let partition_pi0_compatible pi0 linear_constraint =
	(* Get a list of linear inequalities *)
	let list_of_inequalities = ppl_Polyhedron_get_constraints linear_constraint in
	(* Partition *)
	List.partition (is_pi0_compatible_inequality pi0) list_of_inequalities


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** convert to PPL polyhedron *)
let to_ppl_polyhedron x = x

(** construct from PPL polyhedron *)
let from_ppl_polyhedron x = x

(** convert to PPL constraints *)
let to_ppl_constraints x = ppl_Polyhedron_get_constraints x

(** construct from PPL constraints *)
let from_ppl_constraints constraints =
	let poly = true_constraint () in
	ppl_Polyhedron_add_constraints poly constraints;
	poly
		
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
	(* Get an array of linear inequalities *)
	let list_of_inequalities = ppl_Polyhedron_get_constraints linear_constraint in
	let array_of_inequalities = Array.of_list list_of_inequalities in
	"  " ^
	(string_of_array_of_string_with_sep
		"\n& "
		(Array.map (string_of_linear_inequality names) array_of_inequalities)
	)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

let rec minimize_term term =
	match term with
	| Unary_Plus expr -> minimize_term expr
	| Unary_Minus expr -> ( 
		let inner_expr = minimize_term expr in
		match inner_expr with
			| Coefficient c -> Coefficient (Gmp.Z.neg c)
			| Times (c, inner_term) -> Times (Gmp.Z.neg c, inner_term)
			| _ -> Unary_Minus inner_expr )
	| Plus (lexpr, rexpr) -> (
		let lterm = minimize_term lexpr 
		and rterm = minimize_term rexpr in
		match (lterm, rterm) with
			| (Coefficient cl, Coefficient cr) -> Coefficient (Gmp.Z.add cl cr)
			| _ -> compact_sum lterm rterm )			
	| Minus (lexpr, rexpr) -> ( 
		let lterm = minimize_term lexpr 
		and rterm = minimize_term rexpr in
		match (lterm, rterm) with
			| (Coefficient cl, Coefficient cr) -> Coefficient (Gmp.Z.sub cl cr)
			| _ -> compact_diff lterm rterm )
	| Times (c, expr) -> ( 
		let inner_term = minimize_term expr in
		match inner_term with
			| Coefficient d -> Coefficient (Gmp.Z.mul c d)
			| Times (d, inner_expr) -> compact_prod (Gmp.Z.mul c d) inner_expr
			| Plus (lexpr, rexpr) -> (
				let lterm = minimize_term (Times (c, lexpr)) 
				and rterm = minimize_term (Times (c, rexpr)) in
				Plus (lterm, rterm)
			)
			| _ -> compact_prod c inner_term) 
	| _ -> term			 


let minimize_inequality ineq =
	let lterm, rterm, op = split_linear_inequality ineq in
	build_linear_inequality (minimize_term lterm) (minimize_term rterm) op
	

let rec term_support term =
	match term with
		| Coefficient _ -> VariableSet.empty
		| Variable v -> VariableSet.add v VariableSet.empty
		| Unary_Plus t -> term_support t
		| Unary_Minus t -> term_support t
		| Plus  (lterm, rterm) -> VariableSet.union (term_support lterm) (term_support rterm)
		| Minus (lterm, rterm) -> VariableSet.union (term_support lterm) (term_support rterm)
		| Times (z, rterm) -> term_support rterm


let inequality_support ineq =
	let lterm, rterm, _ = split_linear_inequality ineq in
	VariableSet.union (term_support lterm) (term_support rterm)


(** returns a list of variables occuring in a linear constraint *)
let support linear_constraint = 
	let constr_list = ppl_Polyhedron_get_constraints linear_constraint in
	List.fold_left (fun varset constr -> 
		VariableSet.union varset (inequality_support constr)
	) VariableSet.empty constr_list
	

(** returns the lower and upper bounds of a variable wrt. a constraint *)
let bounds constr v =
	let min_bound = ( 
		let _, p, q, bounded = ppl_Polyhedron_minimize constr (Variable v) in
		if bounded then
			Some (NumConst.numconst_of_zfrac p q)
		else
			None
	) in
	let max_bound = ( 
		let _, p, q, bounded = ppl_Polyhedron_maximize constr (Variable v) in
		if bounded then
			Some (NumConst.numconst_of_zfrac p q)
		else
			None
	) in
	(min_bound, max_bound)
	

(** convex hull *)
let hull constraints =
	match constraints with
		| [] -> false_constraint ()
		| c :: tail -> 
			let hull = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron c in
			List.iter (ppl_Polyhedron_poly_hull_assign hull) tail;
			hull
			

(** Performs the intersection of a list of linear constraints *)
let intersection linear_constraints =
	let result_poly = true_constraint () in
	List.iter (fun poly -> ppl_Polyhedron_intersection_assign result_poly poly) linear_constraints;
	assert_dimensions result_poly;
	result_poly	

(** Performs the intersection of a list of linear constraints *)
let intersection_assign constr linear_constraints =
	List.iter (fun poly -> ppl_Polyhedron_intersection_assign constr poly) linear_constraints;
	assert_dimensions constr


(** Let time elapse according to a domain of derivatives *)
let time_elapse linear_constraint deriv_domain =
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	ppl_Polyhedron_time_elapse_assign poly deriv_domain;
	assert_dimensions poly;
	poly

(** Same function with sideeffects *)
let time_elapse_assign linear_constraint deriv_domain =
	ppl_Polyhedron_time_elapse_assign linear_constraint deriv_domain;
	assert_dimensions linear_constraint
	

(** Eliminate (using existential quantification) a set of variables in a linear constraint *)
let hide variables linear_constraint =
	(* copy polyhedron, as PPL function has sideeffects *)
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	ppl_Polyhedron_unconstrain_space_dimensions poly variables;
	assert_dimensions poly;
	poly
	
(** Eliminate (using existential quantification) a set of variables in a linear constraint *)
let hide_assign variables linear_constraint =
	ppl_Polyhedron_unconstrain_space_dimensions linear_constraint variables;
	assert_dimensions linear_constraint
		

(** rename variables in a constraint *)
let rename_variables list_of_couples linear_constraint =
	(* copy polyhedron, as ppl function has sideeffects *)
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
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
  (* perfom the mapping *)
	ppl_Polyhedron_map_space_dimensions poly complete_list;
	assert_dimensions poly;
	poly

				
(** rename variables in a constraint *)
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
  (* perfom the mapping *)
	ppl_Polyhedron_map_space_dimensions linear_constraint complete_list;
	assert_dimensions linear_constraint
												
				
(** substitutes all variables in a linear term.
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
	let lterm, rterm, op = split_linear_inequality linear_inequality in
	let lsub = substitute_variables_in_term sub lterm in
	let rsub = substitute_variables_in_term sub rterm in
	build_linear_inequality lsub rsub op
	
	
(** substitutes all variables in a linear constraint *)			
let substitute sub linear_constraint =
	let inequalities = ppl_Polyhedron_get_constraints linear_constraint in
	let new_inequalities =
		List.map (substitute_variables sub) inequalities in
	let poly = true_constraint () in
	ppl_Polyhedron_add_constraints poly new_inequalities;
	assert_dimensions poly;
	poly
	
	
(** Project on list of variables *)
let project_to variables linear_constraint =
	(* Find variables to remove *)
	let to_remove = ref [] in
	for i = 0 to nb_dimensions () - 1 do
		if not (List.mem i variables) then
			to_remove := i :: !to_remove
	done;		
	ppl_Polyhedron_remove_space_dimensions linear_constraint !to_remove


(** make constraint non-strict *)
let non_strictify linear_constraint =
	let constraint_list = ppl_Polyhedron_get_constraints linear_constraint in 
	let new_constraints = List.map (fun ineq -> 
		let lterm, rterm, op = split_linear_inequality ineq in
		let newop = match op with
			| Less_Than_RS -> Less_Or_Equal_RS
			| Greater_Than_RS -> Greater_Or_Equal_RS
			| _ -> op in
		build_linear_inequality lterm rterm newop
	) constraint_list in
	make new_constraints


(** compute bounding box wrt. to a list of variables *)
let bounding_box variables constr = 
	let bounds = List.map (fun v -> (v, bounds constr v)) variables in
	let cylinder = hide variables constr in
	let boxify (v, (lbound, ubound)) =
		 match lbound with
			| Some min -> (
					let lower_bound = 
						let p,q = split_q min in Greater_Or_Equal (Times (q, Variable v), Coefficient p) in
					intersection_assign cylinder [make [lower_bound]]
				)
			| None -> ();
		 match ubound with
			| Some max -> (
					let upper_bound = 
						let p,q = split_q max in Less_Or_Equal (Times (q, Variable v), Coefficient p) in
					intersection_assign cylinder [make [upper_bound]]
				)
			| None -> () in
	List.iter boxify bounds;
	cylinder

	
exception Found of linear_inequality	

(* find a facet of p that separates p from q *)
let separate p q =
	let facets = Ppl.ppl_Polyhedron_get_constraints p in
	try (
		List.iter (fun facet -> 
			let test_facet f =
				if not (is_satisfiable (intersection [q; make [f]])) then raise (Found f) in
			match facet with
				| Equal (lterm, rterm) -> begin
						(* test both half spaces <= and >= *)
						let upper_h = build_linear_inequality lterm rterm Greater_Or_Equal_RS in
						test_facet upper_h;
						let lower_h = build_linear_inequality lterm rterm Less_Or_Equal_RS in
						test_facet lower_h;
					end
				| _ -> test_facet facet			
		) facets;
		(* no facet left -> give up *)
		None	
	) with Found facet -> Some facet
	

(** Simplistic algorithm to find a separation plane for two polyhedra. 
	 It checks for each facet of the polyhedra, if it is a separating
	 plane. If this is not the case (not guaranteed for dim > 2), None
	 is returned.
**)
let simple_separation_plane p q =
	let facet = separate p q in
	match facet with
		| Some ineq -> Some ineq
		| None -> separate q p	

	
exception Unbounded

let make_variables n =
	let rec build i rest =
		if i = 0 then 0 :: rest else build (i-1) (i :: rest) in
	build (n - 1) []

let unit_vector i = fun j -> 
 	if i = j then NumConst.one else NumConst.zero  

let simple_varnames = fun i -> "x" ^ (string_of_int i)

(* get the vertices defining a polyhedron.
	 Raises Unbounded exception if input is unbounded *)
let get_vertices nvars p =
	(* converts a generator to a list of coordinates, if it is a point *)  
	let vertex_of_generator vars = function 
		| Closure_Point (expr, c) 
		| Point (expr, c) ->
				let den = NumConst.numconst_of_mpz c in  
				List.map (fun v -> 
					let num = evaluate_linear_term_ppl (unit_vector v) expr in
					(NumConst.div num den, v)
				) vars				
		| _ -> raise Unbounded in 
	let variables = make_variables nvars in
	(* get generators of polyhedron *)
	let generators = ppl_Polyhedron_get_minimized_generators p in
	let vertices = List.map (vertex_of_generator variables) generators in
	vertices 


let compact_coefficients =
	List.filter (fun (c, v) -> NumConst.neq c NumConst.zero)


(** Find a separating hyperplane for two polyhedra. The input polyhedra
	 must be bounded, as the algorithm is based on separation of the 
	 sets of vertices of both polyhedra, using linear programming.
	 
	 The LP formulation follows the description found in
   http://cgm.cs.mcgill.ca/~beezer/cs644/main.html
	
	 Given sets P = {p1,...,ph} and Q = {q1,...,qm},
	 find y \in R^h, z \in R^m, a \in R^n, b \in R that
	 
	 minimizes (1/h)[y1 + y2 +...+ yh] + (1/m)[z1 + z2 +...+ zm]  

   subject to
   yi >= -a^T * pi + b + 1  for i = 1,...,h 
   zj >=  a^T * qj - b + 1  for j = 1,...,m 
	 yi >=  0                 for i = 1,...,h 
	 zj >=  0                 for j = 1,...,m 
	
	 If the optimal value is 0, then a,b define a separating hyperplane.		
**)	
let separation_plane_LP nvars p q =
	(* project polyhedra to problem space *)
	let p_proj = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron p in
	let q_proj = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron q in
	ppl_Polyhedron_remove_higher_space_dimensions p_proj nvars;
	ppl_Polyhedron_remove_higher_space_dimensions q_proj nvars;
	(* check for intersection *) 
	(* get vertices of the polyhedra *)
	let vp = get_vertices nvars p_proj in
	let vq = get_vertices nvars q_proj in
	(* number of vertices *)
	let np = List.length vp in
	let nq = List.length vq in
	(* dimension of LP instance *)
	let dim = np + nq + nvars + 1 in
	(* variable indices *)
	let b_index = nvars in
	let y_index i = nvars + i + 1 in
	let z_index i = nvars + i + 1 + List.length vp in
	(* store constraints here *)
	let lpc = ref [] in
	(* positive y and z *)
	let pos_constraint v =
		Greater_Or_Equal ((Variable v), (Coefficient Gmp.Z.zero)) in
	for i = 0 to np-1 do
		lpc := (pos_constraint (y_index i)) :: !lpc
	done;
	for i = 0 to nq-1 do
		lpc := (pos_constraint (z_index i)) :: !lpc
	done;
	(* relation of P to the plane *)
	for i = 0 to np-1 do
		let vertex = List.nth vp i in
		let inv_vertex = List.map (fun (c, v) -> (NumConst.neg c, v)) vertex in
		let bcoef = (NumConst.one, b_index) in
		let rterm = make_linear_term (compact_coefficients (bcoef :: inv_vertex)) NumConst.one in
		let lterm = make_linear_term [(NumConst.one, y_index i)] NumConst.zero in
		let ineq = make_linear_inequality_ppl lterm Greater_Or_Equal_RS rterm in
		lpc := ineq :: !lpc
	done;
	(* relation of Q to the plane *)
	for i = 0 to nq-1 do
		let vertex = List.nth vq i in
		let bcoef = (NumConst.minus_one, b_index) in
		let rterm = make_linear_term (compact_coefficients (bcoef :: vertex)) NumConst.one in
		let lterm = make_linear_term [(NumConst.one, z_index i)] NumConst.zero in
		let ineq = make_linear_inequality_ppl lterm Greater_Or_Equal_RS rterm in
		lpc := ineq :: !lpc
	done;
	(* objective function *)
	let rec build_y_sum i =
		if i = 0 then Variable (y_index 0) else Plus (Variable (y_index i), build_y_sum (i-1)) in
	let rec build_z_sum i =
		if i = 0 then Variable (z_index 0) else Plus (Variable (z_index i), build_z_sum (i-1)) in
	let yfac = Gmp.Z.from_int nq in
	let zfac = Gmp.Z.from_int np in
	let y_sum = Times (yfac, build_y_sum (np-1)) in
	let z_sum = Times (zfac, build_z_sum (nq-1)) in
	let goal = Plus (y_sum, z_sum) in
	(* build LP problem *)
	let lp = ppl_new_MIP_Problem_from_space_dimension dim in
	ppl_MIP_Problem_set_objective_function lp goal;
	ppl_MIP_Problem_add_constraints lp !lpc;
	ppl_MIP_Problem_set_optimization_mode lp Minimization;
	let status = ppl_MIP_Problem_solve lp in
	if status = Unfeasible_Mip_Problem then None else (		
		(* construct hyperplane from solution *)
		let solution = ppl_MIP_Problem_optimizing_point lp in
		(* get optimal value *)
		let c, d = ppl_MIP_Problem_optimal_value lp in
		let f = NumConst.numconst_of_zfrac c d in
		(* Is the solution a real separating hyperplane? *)
		if NumConst.neq f NumConst.zero then None else (
			(* convert the LP solution to an inequality in the original space *) 
			let make_hyperplane = function
				| Point (expr, c) ->
						let den = NumConst.numconst_of_mpz c in
						let get_coef v =
							let num = evaluate_linear_term_ppl (unit_vector v) expr in
							(NumConst.div num den, v) in
						let a = List.map get_coef (make_variables nvars) in
						let b, _ = get_coef (b_index) in
						let lterm = make_linear_term (compact_coefficients a) NumConst.zero in
						let rterm = make_linear_term [] b in
						make_linear_inequality_ppl lterm Greater_Or_Equal_RS rterm
				| _ -> raise (InternalError "illegal LP solution") in				
			let hyperplane = make_hyperplane solution in
			Some hyperplane
		)
	)
	
	
(** Try to find a separation plane for two polyhedra. 
		Uses first the LP algorithm. If no solution is found in this
		way, this might be because the polyhedra are "touching" each
		other. In this case, one of the facets of the polyhedra
		can be used as separating hyperplane.
**)
let separation_plane nvars p q =
	(* If polyhedra intersect, there is surely no solution *)
	if is_satisfiable (intersection [p; q]) then None else
	(* If one of the polys is empty, just return an arbitrary facet of the other one *)
	let get_facet p = 
		let facets = Ppl.ppl_Polyhedron_get_constraints p in
		match facets with
			| [] -> None
			| f :: fs -> Some f in
	if not (is_satisfiable p) then 
		get_facet q
	else if not (is_satisfiable q) then
		get_facet p
	else begin
		(* Try LP solution *)
		let plane = separation_plane_LP nvars p q in
		match plane with
			| Some solution -> Some (minimize_inequality solution)
			| None -> simple_separation_plane p q
	end
	
