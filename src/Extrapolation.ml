(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: clocks extrapolation
 *
 * File contributors : Étienne André, Johan Arcile
 * Created           : 2021/06/17
 * Last modified     : 2021/06/17
 *
 ************************************************************)
 

(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open OCamlUtilities

open AbstractModel
open LinearConstraint
open Options


(************************************************************)
(* Type for NumConst or infinity *)
(************************************************************)

type numconst_or_infinity =
	(* Regular NumConst *)
	| Finite of NumConst.t
	(* Infinity *)
	| Infinity
	(* Minus-infinity *)
	| Minus_infinity
	
let neg_inf (a : numconst_or_infinity) =
	match a with
	| Infinity -> Minus_infinity
	| Minus_infinity -> Infinity
	| Finite a ->  Finite (NumConst.neg a )

let add_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	match a, b with
	| Infinity, Infinity -> Infinity
	| Infinity, Finite b -> Infinity
	| Finite a, Infinity -> Infinity
	| Minus_infinity, Minus_infinity -> Minus_infinity
	| Minus_infinity, Finite b -> Minus_infinity
	| Finite a, Minus_infinity -> Minus_infinity
	| Finite a, Finite b -> Finite (NumConst.add a b)
	| _ -> raise (InternalError "Case infinity-infinity")
	
let sub_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	add_inf a (neg_inf b)
	
let mul_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	match a, b with
	| Infinity, Infinity -> Infinity
	| Minus_infinity, Minus_infinity -> Infinity
	| Infinity, Finite b -> Infinity
	| Finite a, Infinity -> Infinity
	| Infinity, Minus_infinity -> Minus_infinity
	| Minus_infinity, Infinity -> Minus_infinity
	| Minus_infinity, Finite b -> Minus_infinity
	| Finite a, Minus_infinity -> Minus_infinity
	| Finite a, Finite b -> Finite (NumConst.mul a b)
	
let inv_inf (a : numconst_or_infinity) =
	match a with
	| Finite a -> Finite (NumConst.div (NumConst.numconst_of_int 1) a )
	| _ -> Finite (NumConst.numconst_of_int 0)

let div_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	mul_inf a (inv_inf b)
	
let lesser_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	match a, b with
	| Infinity, _ -> false
	| _, Minus_infinity -> false
	| Minus_infinity, Infinity -> true
	| Minus_infinity, Finite b -> true
	| Finite a, Infinity -> true
	| Finite a, Finite b -> a < b
	
let leq_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	a = b || (lesser_inf a b)

let greater_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	lesser_inf b a

let geq_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	leq_inf b a

let min_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	if greater_inf a b then b else a

let max_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	if lesser_inf a b then b else a

let eq_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	match a, b with
	| Infinity, Infinity -> true
	| Minus_infinity, Minus_infinity -> true
	| Finite a, Finite b when (a = b) -> true
	| _ -> false

(************************************************************
 *
 *Computation of the maximal constants for each clocks of a PTA with non-diagonal constraints (only one clock per constraint)
 *
 *Information is encoded as follows :
 *pta_type is a string that can either be "L", "U", "LU_Ubounded", "LU_Lbounded", "other" and indicates if the PTA is in one of those sub-classes.
 *bounds is an array of couple of float such that the couple (f1,f2) at index i indicates that the i-th parameter takes f1 as its lower bound and f2 as its upper bound.
 *guards is a list of all linear constraints present in guards and invariants, each of the form (clock, operator, factors), where a clock is identified by an integer, operator is a string that can either be "<", "<=", ">", ">=", "=", and factors denotes the factor of each parameter in the linear constraint and takes the form of an array of float of size k+1 with k the number of parameter (the float at index k+1 is in fact the constant part of the constraint).
 *h is an integer denoting the number of clocks.
 *
 *Here are some examples of valid input :
 *compute_maximal_constants "L" [|(0.,infinity)|] [(0,"=",[|0.;1.|]);(0,"<=",[|0.;1.|]);(0,"=",[|0.;0.|]);(1,">=",[|1.;0.|])] 2
 *compute_maximal_constants "L/U-U_bounded" [|(0.,infinity);(0.,5.)|] [(0,">",[|0.;1.;-3.|]);(1,"<",[|2.;0.;1.|]);(0,">=",[|0.;-1.;0.|]);(1,"<=",[|0.;0.;4.|])] 2
 *
 ************************************************************)
 
 
(************************************************************)
(* Utilities *)
(************************************************************)

(* Returns factorial n *)
(* Input = n : int*)
(* Output = int*)

let rec factorial n =
	if n <= 1 then 1
	else factorial (n-1) * n
	
(* Returns base power exponent *)	
(* Input = base : int, exponent : int*)
(* Output = int*)
	
let rec power base exponent =
	if exponent = 0 then 1
	else power base (exponent-1) * base
	
(* Returns true if elem is not in set *)	
(* Input = elem : 'a , set : a' list*)
(* Output = bool*)

let not_in elem set =
	let result = ref(true) in
	begin
		List.iter (fun e -> if elem = e then result := false) set;
		!result
	end

(* Returns true if l1 is included in l2*)	
(* Input = l1: 'a list , l2 : a' list*)
(* Output = bool*)

let includes l1 l2 =
	let result = ref(true) in
	begin
		List.iter (fun e -> if not_in e l2 then result := false) l1;
		!result
	end
		
(* Returns the maximum value in an array of numconst_or_infinity (intersected with 0) *)	
(* Input = a : float array *)
(* Output = float *)

let max_array a =
	let m = ref(Finite (NumConst.numconst_of_int 0)) in
	for i=0 to (Array.length a)-1 do 
		m := max_inf !m a.(i)
	done;
	!m
	

(************************************************************)
(* Sub-functions *)
(************************************************************)


(* Hide all bounded parameters in a list of guards by replacing them by their bounds *)	
(* Input = guards : (int, string, float array) list, bounds : (float,float) array *)
(* Output = (int, string, float array) list *)


let hide_bounded (guards : (int * string * numconst_or_infinity array) list) (bounds : (numconst_or_infinity * numconst_or_infinity) array) =
	let f (clock, operator, factors) = 
		let k = (Array.length factors) - 1 in
		for i = 0 to k-1 do
			let (min,max) = bounds.(i) in
			if (not (eq_inf max Infinity)) && (not (eq_inf min Minus_infinity)) then begin
				if greater_inf factors.(i) (Finite (NumConst.numconst_of_int 0)) then begin
					factors.(k) <- add_inf factors.(k) (mul_inf factors.(i) max);
					factors.(i) <- (Finite (NumConst.numconst_of_int 0));
				end
				else begin
					factors.(k) <- add_inf factors.(k) (mul_inf factors.(i) min);
					factors.(i) <- (Finite (NumConst.numconst_of_int 0));
				end
			end
		done;
	(clock, operator, factors)
	in List.map f guards
	

(* Returns a boolean array indicating for each clock if it is a parametric one *)	
(* Input = guards : (int, string, float array) list, h : int *)
(* Output = bool array *)

let compute_parametric_clocks_array guards h =
	let parametric_clocks_array = Array.make h false in
	let f (clock, operator, factors) =
		let k = (Array.length factors) - 1 in
		for i = 0 to k-1 do
			if not (eq_inf factors.(i) (Finite (NumConst.numconst_of_int 0))) then parametric_clocks_array.(clock) <- true;
		done
	in
	List.iter f guards;
	parametric_clocks_array
	
	
(* Counts the number of true values in a boolean array *)	
(* Input = parametric_clocks_array : bool array *)
(* Output = int *)

let compute_parametric_clocks_number parametric_clocks_array =
	let parametric_clocks_number = ref(0) in	
	Array.iter (fun p -> if p then parametric_clocks_number := !parametric_clocks_number + 1;) parametric_clocks_array;
	!parametric_clocks_number	
	
(* Bounds all parameters with n if their current bound is greater than n *)	
(* Input = bounds : (float,float) array, n : float *)
(* Output = (float,float) array *)

let set_bounds bounds n =
	let f (min,max) = 
		if lesser_inf max n then (min,max)
		else (min,n);
	in
	Array.map f bounds
	
	
(* Computes the maximal value (cf. "g_max" paper) of a guard *)	
(* Input = clock : int, operator : string ,factors : float array, bounds : (float,float) array *)
(* Output = float *)

let compute_gmax (clock, operator, factors) bounds =
	let k = (Array.length factors) - 1 in
	let gmax = ref(factors.(k)) in
	for i = 0 to k-1 do
		let (min,max) = bounds.(i) in
		if greater_inf factors.(i) (Finite (NumConst.numconst_of_int 0)) then gmax := add_inf !gmax (mul_inf factors.(i) max)
		else gmax := add_inf !gmax (mul_inf factors.(i) min);
	done;
	!gmax
	
		
(* Returns an array with the greatest non parametric constants compared to each clock (cf. "c_x" paper) *)	
(* Input = guards : (int, string, float array) list, h : int *)
(* Output = float array *)

let compute_greatest_nonparametric_constants guards h=
	let greatest_const = Array.make h (Finite (NumConst.numconst_of_int 0)) in
	let f (clock, operator, factors) =
		greatest_const.(clock) <- max_inf factors.((Array.length factors) - 1) greatest_const.(clock);
	in
	List.iter f guards;
	greatest_const
		
		
(* Compute the upper bound of the number of clock regions (cf. "R" paper) *)	
(* Input = greatest_nonparametric_constants : float array, h : int *)
(* Output = float *)

let compute_regions_upper_bound greatest_nonparametric_constants h =
	let regions_upper_bound = ref(Finite (NumConst.numconst_of_int ((factorial h) * (power 2 h)))) in
	Array.iter (fun z -> regions_upper_bound := 
		mul_inf 
			!regions_upper_bound 
			(add_inf 
				(mul_inf 
					(Finite (NumConst.numconst_of_int 2)) 
					z
				) 
				(Finite (NumConst.numconst_of_int 2))
			)
	) greatest_nonparametric_constants;
	!regions_upper_bound
	
		
(* Compute the parameter valuation to use as an upper bound (cf. "N" paper) *)	
(* Input = pta_type : string, guards : (int, string, float array) list, parametric_clocks_number : float, h : int *)
(* Output = float *)

let compute_n pta_type guards parametric_clocks_number h =
	let gnc = compute_greatest_nonparametric_constants guards h in
	let n = ref(
		mul_inf 
			(Finite 
				(NumConst.numconst_of_int (parametric_clocks_number))
			)
			(add_inf 
				(compute_regions_upper_bound gnc h) 
				(Finite (NumConst.numconst_of_int 1))
			)
	) in
	if pta_type = "U" then n := mul_inf !n (Finite (NumConst.numconst_of_int 8));
	add_inf !n (add_inf (max_array gnc) (Finite (NumConst.numconst_of_int 1)))
	
	
(************************************************************)
(* Main function *)
(************************************************************)

(* Return for each clock x the value to use for the LU-extrapolation of x (cf. "vec{LU}" paper) *)	
(* Input = pta_type : string, bounds : (float,float) array, guards : (int, string, float array) list, h : int *)
(* Output = (float array, float array) *)

let compute_maximal_constants (pta_type : string) (bounds : (numconst_or_infinity * numconst_or_infinity) array) (guards : (int * string * numconst_or_infinity array) list) (h : int) =
	let hide = 
		if pta_type = "LU_Ubounded" then (hide_bounded guards bounds , "L" )
		else if pta_type = "LU_Lbounded" then (hide_bounded guards bounds , "U" )
		else (guards, pta_type)
	in
	let set =
		let (guards, pta_type) = hide in
		if pta_type = "L" || pta_type = "U" then set_bounds 
			bounds 
			(compute_n 
				pta_type 
				guards 
				(compute_parametric_clocks_number 
					(compute_parametric_clocks_array 
						guards 
						h
					)
				)
				h
			)
		else bounds
	in
	let max_const_L = Array.make h Minus_infinity in
	let max_const_U = Array.make h Minus_infinity in
	List.iter (
	fun (clock, operator, factors) -> 
		if operator = ">" || operator = ">=" then max_const_L.(clock) <- max_inf (compute_gmax (clock, operator, factors) set) max_const_L.(clock) 
		else if operator = "<" || operator = "<=" then max_const_U.(clock) <- max_inf (compute_gmax (clock, operator, factors) set) max_const_U.(clock)
		else begin max_const_L.(clock) <- max_inf (compute_gmax (clock, operator, factors) set) max_const_L.(clock);
		max_const_U.(clock) <- max_inf (compute_gmax (clock, operator, factors) set) max_const_L.(clock);
		end
		
	) guards;
	(max_const_L,max_const_U)
	

	
(************************************************************)
(* Global variables *)
(************************************************************)

let lower_constants = ref [||]
let upper_constants = ref [||]

let greatest_constants = ref [||]
let max_lower_const = ref(Minus_infinity)
let max_upper_const = ref(Minus_infinity)
let max_greatest_const = ref(Minus_infinity)

let nb_parameters = ref 0
let clocks = ref []

(*------------------------------------------------------------*)
(* Functions for preparing data structures for extrapolation *)
(*------------------------------------------------------------*)
	
let get_p_bounds p_bounds =
	let min = 
	(* Check if unbounded below *)
	match p_bounds.lower with
	| Unbounded -> Minus_infinity
	(* A finite bound is a pair NumConst.t and a Boolean true iff it is closed (i.e., closed inequality, and not strict) *)
	| Bounded (bound, is_closed) -> Finite bound
	in
	let max = 
	(* Check if unbounded above *)
	match p_bounds.upper with
	| Unbounded -> Infinity
	(* A finite bound is a pair NumConst.t and a Boolean true iff it is closed (i.e., closed inequality, and not strict) *)
	| Bounded (bound, is_closed) -> Finite bound
	in (min,max)
	
let get_bounded p_bounds =
	let bounded_parameters = ref [] in
	for i=0 to (Array.length p_bounds)-1 do 
		let (min,max) = p_bounds.(i) in
		if (greater_inf min Minus_infinity) && (lesser_inf max Infinity) then bounded_parameters := List.append !bounded_parameters [i];
	done;
	!bounded_parameters
	
(*let get_guards invariants transitions =
	TODO
	*)


let get_pta_type pta_type bounded_parameters =
	(* Get the L/U nature *)
	match pta_type with
	(* General PTA *)
	| PTA_notLU -> "other"
	(* L/U-PTA with parameters partitioned into L- and U-parameters *)
	| PTA_LU (l_parameters, u_parameters) -> 
		if includes l_parameters bounded_parameters then "LU_Lbounded"
		else if includes u_parameters bounded_parameters then "LU_Ubounded"
		else "other"
			
	(* L-PTA *)
	| PTA_L -> "L"
	(* U-PTA *)
	| PTA_U -> "U"
	
let set_maximums l u nb_clocks : unit = 
	let g_c = l in
	for i=0 to nb_clocks-1 do 
		g_c.(i) <- max_inf l.(i) u.(i);
		max_lower_const := max_inf !max_lower_const l.(i);
		max_upper_const := max_inf !max_upper_const u.(i);
	done;
	begin
	greatest_constants := g_c;
	max_greatest_const := max_inf !max_lower_const !max_upper_const;
	end

let prepare_extrapolation () : unit =
	(* Retrieve the model *)
	let model = Input.get_model() in

	let p_bounds = Array.make model.nb_parameters (Minus_infinity,Infinity) in
	
	(*let guards = get_guards model.invariants model.transitions *)
	let guards = [(0,"=",[|(Finite (NumConst.numconst_of_int 0));(Finite (NumConst.numconst_of_int 1))|]);(0,"<=",[|(Finite (NumConst.numconst_of_int 0));(Finite (NumConst.numconst_of_int 1))|]);(0,"=",[|(Finite (NumConst.numconst_of_int 0));(Finite (NumConst.numconst_of_int 0))|]);(1,">=",[|(Finite (NumConst.numconst_of_int 1));(Finite (NumConst.numconst_of_int 0))|])] in
	
	let nb_clocks = model.nb_clocks in

	begin
		List.iter (fun (p) -> p_bounds.(p) <- get_p_bounds (model.parameters_bounds p) ) model.parameters;
		
		let pta_type = get_pta_type model.lu_status (get_bounded p_bounds) in
		
		let (l,u) = compute_maximal_constants pta_type p_bounds guards nb_clocks in
		
		begin
			lower_constants := l;
			upper_constants := u;
			
(**Test			
			lower_constants := [|Finite(NumConst.numconst_of_int 1);Finite(NumConst.numconst_of_int 67)|];
			upper_constants := [|Finite(NumConst.numconst_of_int 1);Minus_infinity|];		
if l = !lower_constants && u = !upper_constants then raise (InternalError "good :)") else raise (InternalError "bad :(");**)
	
			set_maximums l u nb_clocks;
	
			nb_parameters := model.nb_parameters;
			clocks := model.clocks;
		end
	end


(*************************************************************************)
(** clock extrapolation on a given linear constraint **)


(*------------------------------------------------------------*)
(** M-extrapolation: returns (the constraint ^ x <= M) , (the constraint ^ x >= M) *)
(*------------------------------------------------------------*)

let m_extrapolation_of_x (big_m : numconst_or_infinity) (x : variable) (px_linear_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =

	(* Case m is Finite *)
	let finite (m : NumConst.t) =
	
		(* Prepare `x <= M`, i.e. `x - M <= 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg m) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_le in
		let x_leq_M : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Intersect `x <= M` with the input constraint *)
		px_intersection_assign x_leq_M [px_linear_constraint];
		let px_linear_constraint1 = x_leq_M in
	
		(* Prepare `x > M`, i.e., `x - M > 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg m) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_g in
		let x_g_M : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Intersect `x > M` with the input constraint *)
		let px_linear_constraint_and_x_g_M = px_intersection [px_linear_constraint ; x_g_M] in

		(* Cylindrify: Eliminate x by variable elimination *)
		px_hide_assign [x] px_linear_constraint_and_x_g_M;

		(* Intersect again with `x > M` *)
		px_intersection_assign px_linear_constraint_and_x_g_M [x_g_M];
		let px_linear_constraint2 = px_linear_constraint_and_x_g_M in

		(* Return both constraints *)
		[px_linear_constraint1; px_linear_constraint2]
		
	in
	
	(* Case m is Minus_infinity *)
	
	let minus_inf () =

		(* Cylindrify: Eliminate x by variable elimination *)
		px_hide_assign [x] px_linear_constraint;

		(* Return constraint *)
		[px_linear_constraint]
		
	in
	
	(* Case m is Infinity *)
	
	let inf () =

		(* Return constraint *)
		[px_linear_constraint]
		
	in
	
	(* Matching the input to corresponding case *)
	match big_m with
	|Finite m -> finite m
	|Minus_infinity -> minus_inf ()
	|Infinity -> inf ()


(*------------------------------------------------------------*)
(** LU-extrapolation: returns (the constraint ^ x <= smaller bound) , (the constraint ^ x > smaller bound ^ x <= greater bound) , (the constraint ^ x > greater bound) *)
(*------------------------------------------------------------*)

let lu_extrapolation_of_x (big_l : numconst_or_infinity) (big_u : numconst_or_infinity) (x : variable) (px_linear_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =

	(* Case l is Finite and u is Finite with l < u *)
	let finite_finite_less (l : NumConst.t) (u : NumConst.t) = 
	
		(* Prepare `x <= L`, i.e. `x - L <= 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg l) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_le in
		let x_leq_L : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x > L`, i.e., `x - L > 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg l) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_g in
		let x_g_L : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x <= U`, i.e. `x - U <= 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg u) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_le in
		let x_leq_U : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x > U`, i.e., `x - U > 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg u) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_g in
		let x_g_U : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Intersect `x <= L` with the input constraint *)
		px_intersection_assign x_leq_L [px_linear_constraint];
		let px_linear_constraint1 = x_leq_L in


		(* Intersect `x > L` and `x <= U` with the input constraint *)
		let x_g_L_and_x_leq_U  = px_intersection [x_g_L ; x_leq_U] in
		let px_linear_constraint_and_x_g_L_and_x_leq_U = px_intersection [px_linear_constraint ; x_g_L_and_x_leq_U] in

		(* Upper cylindrification: Eliminate all upper bounds on x *)
		let other_dimensions = list_diff (LinearConstraint.px_get_dimensions_list px_linear_constraint) [x] in
		px_grow_to_infinity_assign [x] other_dimensions px_linear_constraint_and_x_g_L_and_x_leq_U;
		let px_linear_constraint2 = px_linear_constraint_and_x_g_L_and_x_leq_U in


		(* Intersect `x > U` with the input constraint *)
		let px_linear_constraint_and_x_g_U = px_intersection [px_linear_constraint ; x_g_U] in

		(* Cylindrify: Eliminate x by variable elimination *)
		px_hide_assign [x] px_linear_constraint_and_x_g_U;

		(* Intersect again with `x > U` *)
		px_intersection_assign px_linear_constraint_and_x_g_U [x_g_U];
		let px_linear_constraint3 = px_linear_constraint_and_x_g_U in


		(* Return all constraints *)
		[px_linear_constraint1; px_linear_constraint2; px_linear_constraint3]
		
	in
	(* End case *)
	
	
	(* Case l is Minus_infinity and u is Finite *)
	let minus_inf_finite (u : NumConst.t) = 
	
		(* Prepare `x <= U`, i.e. `x - U <= 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg u) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_le in
		let x_leq_U : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x > U`, i.e., `x - U > 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg u) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_g in
		let x_g_U : px_linear_constraint = make_px_constraint [px_linear_inequality] in


		(* Intersect `x <= U` with the input constraint *)
		let px_linear_constraint_and_x_leq_U = px_intersection [px_linear_constraint ; x_leq_U] in

		(* Upper cylindrification: Eliminate all upper bounds on x *)
		let other_dimensions = list_diff (LinearConstraint.px_get_dimensions_list px_linear_constraint) [x] in
		px_grow_to_infinity_assign [x] other_dimensions px_linear_constraint_and_x_leq_U;
		let px_linear_constraint1 = px_linear_constraint_and_x_leq_U in


		(* Intersect `x > U` with the input constraint *)
		let px_linear_constraint_and_x_g_U = px_intersection [px_linear_constraint ; x_g_U] in

		(* Cylindrify: Eliminate x by variable elimination *)
		px_hide_assign [x] px_linear_constraint_and_x_g_U;

		(* Intersect again with `x > U` *)
		px_intersection_assign px_linear_constraint_and_x_g_U [x_g_U];
		let px_linear_constraint2 = px_linear_constraint_and_x_g_U in


		(* Return all constraints *)
		[px_linear_constraint1; px_linear_constraint2]
		
	in
	(* End case *)
	
	
	(* Case l is Finite and u is Infinity *)
	let finite_inf (l : NumConst.t) = 
	
		(* Prepare `x <= L`, i.e. `x - L <= 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg l) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_le in
		let x_leq_L : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x > L`, i.e., `x - L > 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg l) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_g in
		let x_g_L : px_linear_constraint = make_px_constraint [px_linear_inequality] in
		

		(* Intersect `x <= L` with the input constraint *)
		px_intersection_assign x_leq_L [px_linear_constraint];
		let px_linear_constraint1 = x_leq_L in


		(* Intersect `x > L` with the input constraint *)
		let px_linear_constraint_and_x_g_L = px_intersection [px_linear_constraint ; x_g_L] in

		(* Upper cylindrification: Eliminate all upper bounds on x *)
		let other_dimensions = list_diff (LinearConstraint.px_get_dimensions_list px_linear_constraint) [x] in
		px_grow_to_infinity_assign [x] other_dimensions px_linear_constraint_and_x_g_L;
		let px_linear_constraint2 = px_linear_constraint_and_x_g_L in


		(* Return all constraints *)
		[px_linear_constraint1; px_linear_constraint2]
		
	in
	(* End case *)
	
	
	(* Case l is Minus_infinity and u is Infinity *)
	let minus_inf_inf () = 
		
		(* Upper cylindrification: Eliminate all upper bounds on x *)
		let other_dimensions = list_diff (LinearConstraint.px_get_dimensions_list px_linear_constraint) [x] in
		px_grow_to_infinity_assign [x] other_dimensions px_linear_constraint;

		(* Return constraint *)
		[px_linear_constraint]
		
	in
	(* End case *)
	
	
	(* Case l is Finite and u is Finite with l > u *)
	let finite_finite_great (l : NumConst.t) (u : NumConst.t) = 

		(* Prepare `x <= L`, i.e. `x - L <= 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg l) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_le in
		let x_leq_L : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x > L`, i.e., `x - L > 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg l) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_g in
		let x_g_L : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x <= U`, i.e. `x - U <= 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg u) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_le in
		let x_leq_U : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x > U`, i.e., `x - U > 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg u) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_g in
		let x_g_U : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Intersect `x <= U` with the input constraint *)
		px_intersection_assign x_leq_U [px_linear_constraint];
		let px_linear_constraint1 = x_leq_U in


		(* Intersect `x > U` and `x <= L` with the input constraint *)
		let x_g_U_and_x_leq_L  = px_intersection [x_g_U ; x_leq_L] in
		let px_linear_constraint_and_x_g_U_and_x_leq_L = px_intersection [px_linear_constraint ; x_g_U_and_x_leq_L] in

		(* Lower cylindrification: Eliminate all lower bounds on x *)
		let other_dimensions = list_diff (LinearConstraint.px_get_dimensions_list px_linear_constraint) [x] in
		px_grow_to_zero_assign [x] other_dimensions px_linear_constraint_and_x_g_U_and_x_leq_L;
		
	
		(* Intersect again with `x > U` *)
		px_intersection_assign px_linear_constraint_and_x_g_U_and_x_leq_L [x_g_U];
		let px_linear_constraint2 = px_linear_constraint_and_x_g_U_and_x_leq_L in


		(* Intersect `x > L` with the input constraint *)
		let px_linear_constraint_and_x_g_L = px_intersection [px_linear_constraint ; x_g_L] in

		(* Cylindrify: Eliminate x by variable elimination *)
		px_hide_assign [x] px_linear_constraint_and_x_g_L;

		(* Intersect again with `x > U` *)
		px_intersection_assign px_linear_constraint_and_x_g_L [x_g_U];
		let px_linear_constraint3 = px_linear_constraint_and_x_g_L in


		(* Return all constraints *)
		[px_linear_constraint1; px_linear_constraint2; px_linear_constraint3]
		
	in
	(* End case *)
	
	
	(* Case l is Finite and u is Minus_infinity *)
	let finite_minus_inf (l : NumConst.t) = 

		(* Prepare `x <= L`, i.e. `x - L <= 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg l) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_le in
		let x_leq_L : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x > L`, i.e., `x - L > 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg l) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_g in
		let x_g_L : px_linear_constraint = make_px_constraint [px_linear_inequality] in


		(* Intersect `x <= L` with the input constraint *)
		let px_linear_constraint_and_x_leq_L = px_intersection [px_linear_constraint ; x_leq_L] in

		(* Lower cylindrification: Eliminate all lower bounds on x *)
		let other_dimensions = list_diff (LinearConstraint.px_get_dimensions_list px_linear_constraint) [x] in
		px_grow_to_zero_assign [x] other_dimensions px_linear_constraint_and_x_leq_L;
		let px_linear_constraint1 = px_linear_constraint_and_x_leq_L in


		(* Intersect `x > L` with the input constraint *)
		let px_linear_constraint_and_x_g_L = px_intersection [px_linear_constraint ; x_g_L] in

		(* Cylindrify: Eliminate x by variable elimination *)
		px_hide_assign [x] px_linear_constraint_and_x_g_L;
		let px_linear_constraint2 = px_linear_constraint_and_x_g_L in


		(* Return all constraints *)
		[px_linear_constraint1; px_linear_constraint2]
		
	in
	(* End case *)
	
	(* Case l is Infinity and u is Finite *)
	let inf_finite (u : NumConst.t) = 

		(* Prepare `x <= U`, i.e. `x - U <= 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg u) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_le in
		let x_leq_U : px_linear_constraint = make_px_constraint [px_linear_inequality] in

		(* Prepare `x > U`, i.e., `x - U > 0` *)
		let px_linear_term : px_linear_term = make_px_linear_term [(NumConst.one, x)] (NumConst.neg u) in
		let px_linear_inequality : px_linear_inequality = make_px_linear_inequality px_linear_term Op_g in
		let x_g_U : px_linear_constraint = make_px_constraint [px_linear_inequality] in
		

		(* Intersect `x <= U` with the input constraint *)
		px_intersection_assign x_leq_U [px_linear_constraint];
		let px_linear_constraint1 = x_leq_U in


		(* Intersect `x > U` with the input constraint *)
		let px_linear_constraint_and_x_g_U = px_intersection [px_linear_constraint ; x_g_U] in

		(* Lower cylindrification: Eliminate all lower bounds on x *)
		let other_dimensions = list_diff (LinearConstraint.px_get_dimensions_list px_linear_constraint) [x] in
		px_grow_to_zero_assign [x] other_dimensions px_linear_constraint_and_x_g_U;
		
	
		(* Intersect again with `x > U` *)
		px_intersection_assign px_linear_constraint_and_x_g_U[x_g_U];
		let px_linear_constraint2 = px_linear_constraint_and_x_g_U in

		(* Return all constraints *)
		[px_linear_constraint1; px_linear_constraint2]
		
	in
	(* End case *)
	
	(* Case l is Infinity and u is Minus_infinity *)
	let inf_minus_inf () = 

		(* Lower cylindrification: Eliminate all lower bounds on x *)
		let other_dimensions = list_diff (LinearConstraint.px_get_dimensions_list px_linear_constraint) [x] in
		px_grow_to_zero_assign [x] other_dimensions px_linear_constraint;
		
		
		(* Return constraint *)
		[px_linear_constraint]
		
	in
	(* End case *)


	(* Matching the input to corresponding case *)
	match (big_l, big_u) with
	|Finite l, Finite u when l < u -> finite_finite_less l u
	|Minus_infinity, Finite u -> minus_inf_finite u
	|Finite l , Infinity -> finite_inf l
	|Minus_infinity, Infinity -> minus_inf_inf ()
	|Finite l, Finite u when l > u -> finite_finite_great l u
	|Finite l, Minus_infinity -> finite_minus_inf l
	|Infinity, Finite u -> inf_finite u
	|Infinity, Minus_infinity -> inf_minus_inf ()
	|big_l, big_u when big_l = big_u -> m_extrapolation_of_x big_l x px_linear_constraint
	|_ -> raise (InternalError "Match failure in lu_extrapolation_of_x (theoretically impossible !!!)")


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Apply M-extrapolation to a constraint *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let px_m_extrapolation (the_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =
	(* Set the constants *)
	let m = !greatest_constants in
	let base_indice = !nb_parameters in
	(* Return (for now) exactly ONE state *)
		
	(* Maintain a list of constraints to iteratively apply M-extrapolation for each dimension *)
	let constraints = ref [the_constraint] in
		
	(* Iterate on clocks*)
	List.iter (fun clock_id ->
		let new_constraints = ref [] in
			
		(* Iterate on the list of previously computed constraints *)
		List.iter (fun px_linear_constraint ->
			let c_list : LinearConstraint.px_linear_constraint list = m_extrapolation_of_x m.(clock_id-base_indice) clock_id px_linear_constraint in
			(* Test and add *)
			List.iter (fun c ->
				if LinearConstraint.px_is_satisfiable c then(new_constraints := c :: !new_constraints;);
			) c_list;
		)!constraints;
			
		(* Update new constraints *)
		constraints := !new_constraints;
		
	) !clocks;
	!constraints


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Apply Mglobal-extrapolation to a constraint *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let px_mglobal_extrapolation (the_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =
	(* Set the constants *)
	let m = !max_greatest_const in
	(* Return (for now) exactly ONE state *)
		
	(* Maintain a list of constraints to iteratively apply M-extrapolation for each dimension *)
	let constraints = ref [the_constraint] in
		
	(* Iterate on clocks*)
	List.iter (fun clock_id ->
		let new_constraints = ref [] in
			
		(* Iterate on the list of previously computed constraints *)
		List.iter (fun px_linear_constraint ->
			let c_list : LinearConstraint.px_linear_constraint list = m_extrapolation_of_x m clock_id px_linear_constraint in
			(* Test and add *)
			List.iter (fun c ->
				if LinearConstraint.px_is_satisfiable c then(new_constraints := c :: !new_constraints;);
			) c_list;
		)!constraints;
			
		(* Update new constraints *)
		constraints := !new_constraints;
		
	) !clocks;
	!constraints


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Apply LU-extrapolation to a constraint *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let px_lu_extrapolation (the_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =
	(* Set the constants *)
	let l = !lower_constants in
	let u = !upper_constants in
	let base_indice = !nb_parameters in
	(* Return (for now) exactly ONE state *)
		
	(* Maintain a list of constraints to iteratively apply M-extrapolation for each dimension *)
	let constraints = ref [the_constraint] in
		
	(* Iterate on clocks*)
	List.iter (fun clock_id ->
		let new_constraints = ref [] in
			
		(* Iterate on the list of previously computed constraints *)
		List.iter (fun px_linear_constraint ->
			let c_list : LinearConstraint.px_linear_constraint list = lu_extrapolation_of_x l.(clock_id-base_indice) u.(clock_id-base_indice) clock_id px_linear_constraint in
			(* Test and add *)
			List.iter (fun c ->
				if LinearConstraint.px_is_satisfiable c then(new_constraints := c :: !new_constraints;);
			) c_list;
		)!constraints;
			
		(* Update new constraints *)
		constraints := !new_constraints;
		
	) !clocks;
	!constraints


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Apply LU-extrapolation to a constraint *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let px_luglobal_extrapolation (the_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =
	(* Set the constants *)
	let l = !max_lower_const in
	let u = !max_upper_const in
	(* Return (for now) exactly ONE state *)
		
	(* Maintain a list of constraints to iteratively apply M-extrapolation for each dimension *)
	let constraints = ref [the_constraint] in
		
	(* Iterate on clocks*)
	List.iter (fun clock_id ->
		let new_constraints = ref [] in
			
		(* Iterate on the list of previously computed constraints *)
		List.iter (fun px_linear_constraint ->
			let c_list : LinearConstraint.px_linear_constraint list = lu_extrapolation_of_x l u clock_id px_linear_constraint in
			(* Test and add *)
			List.iter (fun c ->
				if LinearConstraint.px_is_satisfiable c then(new_constraints := c :: !new_constraints;);
			) c_list;
		)!constraints;
			
		(* Update new constraints *)
		constraints := !new_constraints;
		
	) !clocks;
	!constraints
		

