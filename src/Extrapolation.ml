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

let add_inf (a : numconst_or_infinity) (b : numconst_or_infinity) =
	match a, b with
	| Infinity, Infinity -> Infinity
	| Minus_infinity, Minus_infinity -> Minus_infinity
	| Finite a, Finite b-> Finite (NumConst.add a b)
	| _ -> raise (InternalError "Case addition infinity/-infinity")


(************************************************************
 *
 *Computation of the maximal constants for each clocks of a PTA with non-diagonal constraints (only one clock per constraint)
 *
 *Information is encoded as follows :
 *pta_type is a string that can either be "L", "U", "L/U-U_bounded", "L/U-L_bounded", "other" and indicates if the PTA is in one of those sub-classes.
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
	
	
(* Returns the maximum value in an array of floats (intersected with 0) *)	
(* Input = a : float array *)
(* Output = float *)

let max_array a =
	let m = ref(0.) in
	for i=0 to (Array.length a)-1 do 
		m := max !m a.(i)
	done;
	!m
	

(************************************************************)
(* Sub-functions *)
(************************************************************)


(* Hide all bounded parameters in a list of guards by replacing them by their bounds *)	
(* Input = guards : (int, string, float array) list, bounds : (float,float) array *)
(* Output = (int, string, float array) list *)

let hide_bounded guards bounds =
	let f (clock, operator, factors) = 
		let k = Array.length factors -1 in
		for i = 0 to k-1 do
			let (min,max) = bounds.(i) in
			if max != infinity then begin
				if factors.(i) > 0. then begin
					factors.(k) <- factors.(k) +. factors.(i)*.max;
					factors.(i) <- 0.;
				end
				else begin
					factors.(k) <- factors.(k) +. factors.(i)*.min;
					factors.(i) <- 0.;
				end
			end
		done;
	(clock, operator, factors)
	in List.map f guards
	

(* Returns a boolean array indicating for each clock if it is a parametric one *)	
(* Input = guards : (int, string, float array) list, h : int *)
(* Output = bool array *)

let compute_parametric_clocks_array guards h =
	let parametric_clocks_array = Array.make h true in
	let f (clock, operator, factors) =
		let k = Array.length factors -1 in
		if parametric_clocks_array.(clock) then
		for i = 0 to k-1 do
			if factors.(i) != 0. then
				parametric_clocks_array.(clock) <- false;
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
		if max < n then (min,max)
		else (min,n);
	in
	Array.map f bounds
	
	
(* Computes the maximal value (cf. "g_max" paper) of a guard *)	
(* Input = clock : int, operator : string ,factors : float array, bounds : (float,float) array *)
(* Output = float *)

let compute_gmax (clock, operator, factors) bounds =
	let k = Array.length factors - 1 in
	let gmax = ref(factors.(k)) in
	for i = 0 to k-1 do
		let (min,max) = bounds.(i) in
		if factors.(i) > 0. then gmax := !gmax +. factors.(i)*.max
		else gmax := !gmax +. factors.(i)*.min;
	done;
	!gmax
	
		
(* Returns an array with the greatest non parametric constants compared to each clock (cf. "c_x" paper) *)	
(* Input = guards : (int, string, float array) list, h : int *)
(* Output = float array *)

let compute_greatest_nonparametric_constants guards h=
	let greatest_const = Array.make h 0. in
	let f (clock, operator, factors) =
		greatest_const.(clock) <- max factors.(Array.length factors - 1) greatest_const.(clock);
	in
	List.iter f guards;
	greatest_const
		
		
(* Compute the upper bound of the number of clock regions (cf. "R" paper) *)	
(* Input = greatest_nonparametric_constants : float array, h : int *)
(* Output = float *)

let compute_regions_upper_bound greatest_nonparametric_constants h =
	let regions_upper_bound = ref(float_of_int((factorial h) * (power 2 h))) in
	Array.iter (fun z -> regions_upper_bound := !regions_upper_bound*.(2.*.z+.2.)) greatest_nonparametric_constants;
	!regions_upper_bound
	
		
(* Compute the parameter valuation to use as an upper bound (cf. "N" paper) *)	
(* Input = pta_type : string, guards : (int, string, float array) list, parametric_clocks_number : float, h : int *)
(* Output = float *)

let compute_n pta_type guards parametric_clocks_number h =
	let gnc = compute_greatest_nonparametric_constants guards h in
	let n = ref(float_of_int(parametric_clocks_number)*.((compute_regions_upper_bound gnc h)+.1.)) in
	if pta_type = "U" then n := !n*.8.;
	!n +. max_array gnc +. 1.
	
	
(************************************************************)
(* Main function *)
(************************************************************)

(* Return for each clock x the value to use for the LU-extrapolation of x (cf. "vec{LU}" paper) *)	
(* Input = pta_type : string, bounds : (float,float) array, guards : (int, string, float array) list, h : int *)
(* Output = (float array, float array) *)

	
let compute_maximal_constants pta_type bounds guards h =
	let hide = 
		if pta_type = "L/U-U_bounded" then (hide_bounded guards bounds , "L" )
		else if pta_type = "L/U-L_bounded" then (hide_bounded guards bounds , "U" )
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
	let max_const_L = Array.make h (-.infinity) in
	let max_const_U = Array.make h (-.infinity) in
	List.iter (
	fun (clock, operator, factors) -> 
		if operator = ">" || operator = ">=" then max_const_L.(clock) <- max (compute_gmax (clock, operator, factors) set) max_const_L.(clock) 
		else if operator = "<" || operator = "<=" then max_const_U.(clock) <- max (compute_gmax (clock, operator, factors) set) max_const_U.(clock)
		else begin max_const_L.(clock) <- max (compute_gmax (clock, operator, factors) set) max_const_L.(clock);
		max_const_U.(clock) <- max (compute_gmax (clock, operator, factors) set) max_const_L.(clock);
		end
		
	) guards;
	(max_const_L,max_const_U)
		
(************************************************************)
(* Global variables *)
(************************************************************)

let lower_constants = ref [||]
let upper_constants = ref [||]

let greatest_constants = ref [||]
let max_lower_const = ref(NumConst.numconst_of_int (-1)) (**TODO: Minus_infinity**)
let max_upper_const = ref(NumConst.numconst_of_int (-1)) (**TODO: Minus_infinity**)
let max_greatest_const = ref(NumConst.numconst_of_int (-1)) (**TODO: Minus_infinity**)

let nb_parameters = ref 0
let clocks = ref []

(*------------------------------------------------------------*)
(* Functions for preparing data structures for extrapolation *)
(*------------------------------------------------------------*)
let get_pta_type pta_type =
	(* Get the L/U nature *)
	match pta_type with
	(* General PTA *)
	| PTA_notLU -> "other"
	(* L/U-PTA with parameters partitioned into L- and U-parameters *)
	(**TODO**)
	| PTA_LU (l_parameters, u_parameters) -> "other"
	(* L-PTA *)
	| PTA_L -> "L"
	(* U-PTA *)
	| PTA_U -> "U"
	
let p_bounds_to_float p_bounds =
	let min = 
	(* Check if unbounded below *)
	match p_bounds.lower with
	| Unbounded -> Minus_infinity
	(* A finite bound is a pair NumConst.t and a Boolean true iff it is closed (i.e., closed inequality, and not strict) *)
	| Bounded (bound, is_closed) -> Finite bound
	in
	let max = 
	(* Check if unbounded above *)
	match p_bounds.lower with
	| Unbounded -> Infinity
	(* A finite bound is a pair NumConst.t and a Boolean true iff it is closed (i.e., closed inequality, and not strict) *)
	| Bounded (bound, is_closed) -> Finite bound
	in (min,max)
	
let set_maximums l u nb_clocks : unit = 
	let g_c = l in
	for i=0 to nb_clocks-1 do 
		g_c.(i) <- max l.(i) u.(i);
		max_lower_const := max !max_lower_const l.(i);
		max_upper_const := max !max_upper_const u.(i);
	done;
	begin
	greatest_constants := g_c;
	max_greatest_const := max !max_lower_const !max_upper_const;
	end

let prepare_extrapolation () : unit =
	(* Retrieve the model *)
	let model = Input.get_model() in
	
	let parameters = model.parameters in
	
(**	let p_bounds = List.iter (fun p -> (p_bounds_to_float model.parameters_bounds p)) parameters in**)
	let p_bounds = [|(0.,infinity)|] in
	
	let guards = [(0,"=",[|0.;1.|]);(0,"<=",[|0.;1.|]);(0,"=",[|0.;0.|]);(1,">=",[|1.;0.|])] in
	
	let nb_clocks = model.nb_clocks in
	
(**	let all_parameters_bounded = model.bounded_parameters in**)

	let (l,u) = compute_maximal_constants (get_pta_type model.lu_status) p_bounds guards nb_clocks in
	begin
		(*lower_constants := l;
		upper_constants := u;*)
		
		lower_constants := [|NumConst.numconst_of_int 1;NumConst.numconst_of_int 67|];
		upper_constants := [|NumConst.numconst_of_int 1;NumConst.numconst_of_int (-1)|] ;
		set_maximums !lower_constants !upper_constants nb_clocks; (** TODO : l u **)
		nb_parameters := model.nb_parameters;
		clocks := model.clocks;
	end


(*************************************************************************)
(** clock extrapolation on a given linear constraint **)


(*------------------------------------------------------------*)
(** M-extrapolation: returns (the constraint ^ x <= M) , (the constraint ^ x >= M) *)
(*** TODO: improve this description ***)
(*------------------------------------------------------------*)
let m_extrapolation_of_x (m : NumConst.t) (x : variable) (px_linear_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =
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


(*------------------------------------------------------------*)
(** LU-extrapolation: returns (the constraint ^ x <= smaller bound) , (the constraint ^ x > smaller bound ^ x <= greater bound) , (the constraint ^ x > greater bound) *)
(*** TODO: improve this description ***)
(*------------------------------------------------------------*)

let lu_extrapolation_of_x (l : NumConst.t) (u : NumConst.t) (x : variable) (px_linear_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =

	(* If L < U *)
	if l < u then

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
		(**let other_dimensions = list_diff (px_get_dimensions_list px_linear_constraint) [x] in
		p_grow_to_infinity_assign [x] other_dimensions px_linear_constraint_and_x_g_L_and_x_leq_U;**)
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


		(* If U < L *)
	else if l > u then

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
		(**let other_dimensions = list_diff (px_get_dimensions_list px_linear_constraint) [x] in
		p_grow_to_zero_assign [x] other_dimensions px_linear_constraint_and_x_g_U_and_x_leq_L;**)
		
	
		(* Intersect again with `x > U` *)
		px_intersection_assign px_linear_constraint_and_x_g_U_and_x_leq_L [x_g_U];
		let px_linear_constraint2 = px_linear_constraint_and_x_g_U_and_x_leq_L in


		(* Intersect `x > L` with the input constraint *)
		let px_linear_constraint_and_x_g_L = px_intersection [px_linear_constraint ; x_g_L] in

		(* Cylindrify: Eliminate x by variable elimination *)
		px_hide_assign [x] px_linear_constraint_and_x_g_L;

		(* Intersect again with `x > L` *)
		px_intersection_assign px_linear_constraint_and_x_g_L [x_g_L];
		let px_linear_constraint3 = px_linear_constraint_and_x_g_L in


		(* Return all constraints *)
		[px_linear_constraint1; px_linear_constraint2; px_linear_constraint3]


	(* If L = U *)
	else
		(* Call the M-extrapolation *)
		m_extrapolation_of_x l x px_linear_constraint




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


			

