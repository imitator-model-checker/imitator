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

(* The manager *)
(*let manager = Polka.manager_alloc_strict ()*)

(* The number of integer dimensions *)
let int_dim = ref 0

(* The number of real dimensions *)
let real_dim = ref 0

(* Total number of dimensions *)
let total_dim = ref 0



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
(*let make_linear_term members coef =                          *)
(*	(* Convert the members *)                                  *)
(*	let members = List.map apron_coeff_dim_of_member members in*)
(*	(* Convert the coef *)                                     *)
(*	let coeff_option = apron_coeff_option_of_constant coef in  *)
(*		Linexpr0.of_list None members coeff_option               *)

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


(** Convert a linear inequality into a string *)
let string_of_linear_inequality names linear_inequality =
	match linear_inequality with
		| Less_Than (lterm, rterm) -> (
				let lstr = string_of_linear_term_ppl names lterm in
				let rstr = string_of_linear_term_ppl names rterm in
				lstr ^ " < " ^ rstr )
		| Less_Or_Equal (lterm, rterm) -> (
				let lstr = string_of_linear_term_ppl names lterm in
				let rstr = string_of_linear_term_ppl names rterm in
				lstr ^ " <= " ^ rstr )
		| Equal (lterm, rterm) -> (
				let lstr = string_of_linear_term_ppl names lterm in
				let rstr = string_of_linear_term_ppl names rterm in
				lstr ^ " = " ^ rstr )
		| Greater_Than (lterm, rterm) -> (
				let lstr = string_of_linear_term_ppl names lterm in
				let rstr = string_of_linear_term_ppl names rterm in
				lstr ^ " > " ^ rstr )
		| Greater_Or_Equal (lterm, rterm) -> (
				let lstr = string_of_linear_term_ppl names lterm in
				let rstr = string_of_linear_term_ppl names rterm in
				lstr ^ " >= " ^ rstr )
	

(**************************************************)
(** {2 Linear Constraints} *)
(**************************************************)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

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
let is_leq = ppl_Polyhedron_contains_Polyhedron

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
	match ineq with
		| Less_Than (lterm, rterm) -> VariableSet.union (term_support lterm) (term_support rterm)
		| Less_Or_Equal (lterm, rterm) -> VariableSet.union (term_support lterm) (term_support rterm)
		| Equal (lterm, rterm) -> VariableSet.union (term_support lterm) (term_support rterm)
		| Greater_Than (lterm, rterm) -> VariableSet.union (term_support lterm) (term_support rterm)
		| Greater_Or_Equal (lterm, rterm) -> VariableSet.union (term_support lterm) (term_support rterm)


(** returns a list of variables occuring in a linear constraint *)
let support linear_constraint = 
	let constr_list = ppl_Polyhedron_get_constraints linear_constraint in
	List.fold_left (fun varset constr -> 
		VariableSet.union varset (inequality_support constr)
	) VariableSet.empty constr_list
	


(** Performs the intersection of a list of linear constraints *)
let intersection linear_constraints =
	let result_poly = true_constraint () in
	List.iter (fun poly -> ppl_Polyhedron_intersection_assign result_poly poly) linear_constraints;
	assert_dimensions result_poly;
	result_poly	


(** Let time elapse according to a domain of derivatives *)
let time_elapse linear_constraint deriv_domain =
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	ppl_Polyhedron_time_elapse_assign poly deriv_domain;
	assert_dimensions poly;
	poly


(** Eliminate (using existential quantification) a set of variables in a linear constraint *)
let hide variables linear_constraint =
	(* copy polyhedron, as PPL function has sideeffects *)
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	ppl_Polyhedron_unconstrain_space_dimensions poly variables;
	assert_dimensions poly;
	poly
	

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
				Greater_Or_Equal (lsub, rsub))
		

(** 'add_d d coef variables c' adds a variable 'coef * d' to any variable in 'variables' *)
(***** FIXME: This is probably very expensive and stupid *)
(*let add_d d coef variable_list linear_constraint =                                             *)
(*	let p = NumConst.get_num coef in                                                             *)
(*	let q = NumConst.get_den coef in                                                             *)
(*	if (q <>! Gmp.Z.one) then                                                                    *)
(*		raise (InternalError "Only integer coefficients supported yet in add_d");                  *)
(*	(* build the substitution function *)	                                                      *)
(*	let coef_d = Times (p, Variable d) in                                                        *)
(*	let sub = fun v -> (                                                                         *)
(*		if List.mem v variable_list then                                                           *)
(*			Plus (Variable v, coef_d)                                                                *)
(*		else                                                                                       *)
(*			Variable v                                                                               *)
(*		) in                                                                                       *)
(*	(* get the constraints from polyhedron *)                                                    *)
(*	let constraint_list = ppl_Polyhedron_get_constraints linear_constraint in                    *)
(*	(* perform the substitution for each constraint *)                                           *)
(*	let new_constraints = List.map (fun ineq -> substitute_variables sub ineq) constraint_list in*)
(*	(* build a new polyhedron *)                                                                 *)
(*	let poly = true_constraint () in                                                             *)
(*	ppl_Polyhedron_add_constraints poly new_constraints; 	                                      *)
(*	assert_dimensions poly;                                                                      *)
(*	poly                                                                                         *)


let split_q r = 
	let p = NumConst.get_num r in
	let q = NumConst.get_den r in
	p, q

let add_d d coef variable_list linear_constraint =
	(* get numerator and denominator of rational coefficient *)
	let p, q = split_q coef in 
	(* function for building the affine translation of a variable: v -> v + coef*d *)
	let affine_translation = fun v -> Plus (Times (q, Variable v), Times (p, Variable d)) in
	(* copy linear constraint, as PPL functions have side effects *)	
	let result_poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	(* perform the affine translations *)
	List.iter (fun v -> 
		ppl_Polyhedron_affine_preimage result_poly v (affine_translation v) q
	) variable_list;
	assert_dimensions result_poly;
	result_poly


(*

let of_int = NumConst.numconst_of_int;;
let of_frac = NumConst.numconst_of_frac;;

let names = fun i -> if i = 4 then "d" else ("x" ^ (string_of_int i));;
let pi0 = [| 1; 2; 3; 4 |];;

let pi0 = fun i -> (of_int pi0.(i));;

let int_dim = 0;;
let real_dim = 5;;

LinearConstraint.set_manager int_dim real_dim;;

let lt1 = LinearConstraint.make_linear_term [(of_int (-7), 1); (of_frac (-2) 2, 2); (of_int (0), 3); (of_int (-1), 1); (of_frac 132 64, 3); ] (of_frac (-4) 3);;
let lt2 = LinearConstraint.make_linear_term [(of_int (-1), 2); (of_int (1), 3)] (of_frac (0) (1));;
let lt3 = LinearConstraint.make_linear_term [] (of_frac (-137) (2046));;
let lt4 = LinearConstraint.make_linear_term [] (of_frac (0) (9));;

print_string ("\n lt1 := " ^ (LinearConstraint.string_of_linear_term names lt1));;
print_string ("\n lt2 := " ^ (LinearConstraint.string_of_linear_term names lt2));;
print_string ("\n lt3 := " ^ (LinearConstraint.string_of_linear_term names lt3));;
print_string ("\n lt4 := " ^ (LinearConstraint.string_of_linear_term names lt4));;

print_string ("\n lt1 + lt2 := " ^ (LinearConstraint.string_of_linear_term names (LinearConstraint.add_linear_terms lt1 lt2)));;
print_string ("\n lt1 + lt2 + lt3 := " ^ (LinearConstraint.string_of_linear_term names (LinearConstraint.add_linear_terms (LinearConstraint.add_linear_terms lt1 lt2) lt3)));;

let li1 = LinearConstraint.make_linear_inequality lt1 LinearConstraint.Op_g;;
let li2 = LinearConstraint.make_linear_inequality lt2 LinearConstraint.Op_ge;;
let li3 = LinearConstraint.make_linear_inequality lt3 LinearConstraint.Op_g;;
let li4 = LinearConstraint.make_linear_inequality lt4 LinearConstraint.Op_ge;;

print_string ("\n li1 := " ^ (LinearConstraint.string_of_linear_inequality names li1));;
print_string ("\n li2 := " ^ (LinearConstraint.string_of_linear_inequality names li2));;
print_string ("\n li3 := " ^ (LinearConstraint.string_of_linear_inequality names li3));;
print_string ("\n li4 := " ^ (LinearConstraint.string_of_linear_inequality names li4));;

print_string ("\n li1 is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible_inequality pi0 li1)));;
print_string ("\n li2 is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible_inequality pi0 li2)));;
print_string ("\n li3 is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible_inequality pi0 li3)));;
print_string ("\n li4 is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible_inequality pi0 li4)));;


let negate = LinearConstraint.negate_wrt_pi0 pi0;;

print_string ("\n neg li1 := " ^ (LinearConstraint.string_of_linear_inequality names (negate li1)));;
print_string ("\n neg li2 := " ^ (LinearConstraint.string_of_linear_inequality names (negate li2)));;
print_string ("\n neg li3 := " ^ (LinearConstraint.string_of_linear_inequality names (negate li3)));;
print_string ("\n neg li4 := " ^ (LinearConstraint.string_of_linear_inequality names (negate li4)));;

let lc1 = LinearConstraint.make [li1];;
let lc2 = LinearConstraint.make [li2];;
let lc3 = LinearConstraint.make [li3];;
let lc4 = LinearConstraint.make [li4];;
let lc12 = LinearConstraint.make [li1; li2];;

print_string ("\n lc1 := " ^ (LinearConstraint.string_of_linear_constraint names lc1));;
print_string ("\n lc2 := " ^ (LinearConstraint.string_of_linear_constraint names lc2));;
print_string ("\n lc3 := " ^ (LinearConstraint.string_of_linear_constraint names lc3));;
print_string ("\n lc4 := " ^ (LinearConstraint.string_of_linear_constraint names lc4));;
print_string ("\n [li1 ; li2] := " ^ (LinearConstraint.string_of_linear_constraint names lc12));;

print_string ("\n lc1 is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible pi0 lc1)));;
print_string ("\n lc2 is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible pi0 lc2)));;
print_string ("\n lc3 is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible pi0 lc3)));;
print_string ("\n lc4 is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible pi0 lc4)));;
print_string ("\n [li1 ; li2] is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible pi0 lc12)));;

let lc1i2 = LinearConstraint.intersection [lc1 ; lc2] in
print_string ("\n lc1 ^ lc2 = " ^ (LinearConstraint.string_of_linear_constraint names lc1i2));;
print_string ("\n lc1 ^ lc3 = " ^ (LinearConstraint.string_of_linear_constraint names (LinearConstraint.intersection [lc1 ; lc3])));;

print_string ("\n lc1 after elimination of x1 : " ^ (LinearConstraint.string_of_linear_constraint names (LinearConstraint.hide [1] lc1)));;

print_string ("\n -------------------");;

let lt1 = LinearConstraint.make_linear_term [(of_int 1, 3); (of_int (-1), 1)] (of_int 0);;
let lt2 = LinearConstraint.make_linear_term [(of_int 1, 1); (of_int (-1), 2)] (of_int 0);;
let li1 = LinearConstraint.make_linear_inequality lt1 LinearConstraint.Op_ge;;
let li2 = LinearConstraint.make_linear_inequality lt2 LinearConstraint.Op_g;;
let lc1 = LinearConstraint.make [li1];;
let lc2 = LinearConstraint.make [li2];;
print_string ("\n lc1 := " ^ (LinearConstraint.string_of_linear_constraint names lc1));;
print_string ("\n lc2 := " ^ (LinearConstraint.string_of_linear_constraint names lc2));;

let lc12 = LinearConstraint.intersection [lc1 ; lc2];;
print_string ("\n lc1 ^ lc2 = " ^ (LinearConstraint.string_of_linear_constraint names lc12));;

print_string ("\n lc1 ^ lc2 is pi-compatible : " ^ (string_of_bool (LinearConstraint.is_pi0_compatible pi0 lc12)));;

let pi0comp, pi0incomp = LinearConstraint.partition_pi0_compatible pi0 lc12;;
print_string ("\n Partition of the inequalities of lc1 ^ lc2 according to their pi0-compatibility:");;
let print_inequalities = List.iter (fun i -> print_string ("\n    - " ^ (LinearConstraint.string_of_linear_inequality names i))) in
print_string ("\n  - pi0-compatible:");
print_inequalities pi0comp;
print_string ("\n  - pi0-incompatible:");
print_inequalities pi0incomp;;

let lc3 = LinearConstraint.hide [1] lc12 in
print_string ("\n (*1) lc1 ^ lc2 after elimination of x1 : " ^ (LinearConstraint.string_of_linear_constraint names lc3));;

(* INCLUSION AND EQUALITY *)

let lc4 = LinearConstraint.make [LinearConstraint.make_linear_inequality (LinearConstraint.make_linear_term [(of_int 1, 3); (of_int (-1), 2)] (of_int 0)) LinearConstraint.Op_ge] in
print_string ("\n (*2) : " ^ (LinearConstraint.string_of_linear_constraint names lc4));;

print_string ("\n (*1) = (*1) : " ^ (string_of_bool (LinearConstraint.is_equal lc3 lc3)));;
print_string ("\n (*1) = (*2) : " ^ (string_of_bool (LinearConstraint.is_equal lc3 lc4)));;
print_string ("\n (*1) <= (*2) : " ^ (string_of_bool (LinearConstraint.is_leq lc3 lc4)));;
print_string ("\n (*1) >= (*2) : " ^ (string_of_bool (LinearConstraint.is_leq lc4 lc3)));;


(* VARIABLE RENAMINGS *)

let renamed = LinearConstraint.rename_variables [(2, 3); (3, 2)] lc12 in
print_string ("\n lc1 ^ lc2 after switching x2 and x3 " ^ (LinearConstraint.string_of_linear_constraint names renamed));;

let with_d = LinearConstraint.add_d 4 (of_int 1) [1] lc12 in
print_string ("\n lc1 ^ lc2 after adding 'd' " ^ (LinearConstraint.string_of_linear_constraint names with_d));;

let with_d = LinearConstraint.add_d 4 (of_int 1) [1; 2] lc12 in
print_string ("\n lc1 ^ lc2 after adding 'd' " ^ (LinearConstraint.string_of_linear_constraint names with_d));;




(*(* APRON TESTS *)

let manager = Polka.manager_alloc_strict () in

let of_int = Coeff.s_of_int in
let linexpr1 = Linexpr0.of_list None [(of_int (1), 0); (of_int (-2), 1); (of_int (3), 2);] (Some (of_int (2))) in
let abstract1 = Abstract0.of_lincons_array manager int_dim real_dim [| Lincons0.make linexpr1 Lincons0.EQ|] in

print_string ("\n Before test: ");
Abstract0.print names Format.std_formatter abstract1;
flush stdout;

let linarray = [|
(*	Linexpr0.of_list None [(of_int 1, 0);] (Some (of_int (-5)));
	Linexpr0.of_list None [(of_int 1, 1);] (Some (of_int (-5)));*)
(*	Linexpr0.of_list None [(of_int (1), 3);] (Some (of_int (0)));
	Linexpr0.of_list None [(of_int (1), 3);] (Some (of_int (0)));*)
	Linexpr0.of_list None [(of_int (1), 1);] (Some (of_int (0)));
	Linexpr0.of_list None [(of_int (1), 0);] (Some (of_int (0)));
	|] in
let test = Abstract0.substitute_linexpr_array manager abstract1 [| 0 ; 1|] linarray None in

print_string ("\n After test: ");
Abstract0.print names Format.std_formatter test;
flush stdout;;*)


print_newline();;

exit 0;;

*)
