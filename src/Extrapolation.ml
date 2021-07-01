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


(************************************************************)
(* Type for modified pta_type *)
(************************************************************)

type modified_pta_type =
	(* LU-PTA with bounded lower bound parameters*)
	| LU_Lbounded
	(* LU-PTA with bounded upper bound parameters*)
	| LU_Ubounded
	(* L-PTA *)
	| PTA_L
	(* U-PTA *)
	| PTA_U
	(* Any other, bounded or not *)
	| Other 

(************************************************************
 *Computation of the maximal constants for each clocks of a PTA with non-diagonal constraints (only one clock per constraint)
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

let hide_bounded (guards : (int * op * numconst_or_infinity array) list) (bounds : (numconst_or_infinity * numconst_or_infinity) array) =
	let new_guards = guards in
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
	in List.map f new_guards
	

(* Returns a boolean array indicating for each clock if it is a parametric one *)	

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

let compute_parametric_clocks_number parametric_clocks_array =
	let parametric_clocks_number = ref(0) in	
	Array.iter (fun p -> if p then parametric_clocks_number := !parametric_clocks_number + 1;) parametric_clocks_array;
	!parametric_clocks_number	
	
(* Bounds all parameters with n if their current bound is greater than n *)	

let set_bounds bounds n =
	let new_bounds = bounds in
	let f (min,max) = 
		if lesser_inf max n then (min,max)
		else (min,n);
	in
	Array.map f new_bounds
	
	
(* Computes the maximal value (cf. "g_max" paper) of a guard *)	

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

let compute_greatest_nonparametric_constants guards h=
	let greatest_const = Array.make h (Finite (NumConst.numconst_of_int 0)) in
	let f (clock, operator, factors) =
		greatest_const.(clock) <- max_inf factors.((Array.length factors) - 1) greatest_const.(clock);
	in
	List.iter f guards;
	greatest_const
		
		
(* Compute the upper bound of the number of clock regions (cf. "R" paper) *)	

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
	if pta_type = PTA_U then n := mul_inf !n (Finite (NumConst.numconst_of_int 8));
	add_inf !n (add_inf (max_array gnc) (Finite (NumConst.numconst_of_int 1)))
	
	
(************************************************************)
(* Main function *)
(************************************************************)

(* Return for each clock x the value to use for the LU-extrapolation of x (cf. "vec{LU}" paper) *)	

let get_max_bounds bounds guards h =
	let max_const_L = Array.make h Minus_infinity in
	let max_const_U = Array.make h Minus_infinity in
	List.iter (
	fun (clock, operator, factors) -> 
		if operator = Op_g || operator = Op_ge then max_const_L.(clock) <- max_inf (compute_gmax (clock, operator, factors) bounds) max_const_L.(clock) 
		else if operator = Op_l || operator = Op_le then max_const_U.(clock) <- max_inf (compute_gmax (clock, operator, factors) bounds) max_const_U.(clock)
		else begin max_const_L.(clock) <- max_inf (compute_gmax (clock, operator, factors) bounds) max_const_L.(clock);
		max_const_U.(clock) <- max_inf (compute_gmax (clock, operator, factors) bounds) max_const_L.(clock);
		end
	) guards;
	(max_const_L,max_const_U)

let compute_maximal_constants (pta_type : modified_pta_type) (bounds : (numconst_or_infinity * numconst_or_infinity) array) (guards : (int * op * numconst_or_infinity array) list) (h : int) : (numconst_or_infinity array * numconst_or_infinity array) =
	let hide = 
		if pta_type = LU_Ubounded then (hide_bounded guards bounds , PTA_L )
		else if pta_type = LU_Lbounded then (hide_bounded guards bounds , PTA_U )
		else (guards, pta_type)
	in
	let new_bounds =
		let (guards, pta_type) = hide in
		if pta_type = PTA_L || pta_type = PTA_U then set_bounds 
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
	in get_max_bounds new_bounds guards h


	
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
	

	
let get_raw_guards model =
	let raw_guards = ref [] in
	let get_actions automaton location = List.iter 
		(fun (action) -> raw_guards := List.append
			(List.map 
				(fun (transition_index) -> (model.transitions_description transition_index).guard ) 
				(model.transitions automaton location action)
			)
			!raw_guards
		)
		(model.actions_per_location automaton location)
	in
	let get_locations automaton = List.iter 
		(fun (location) -> 
			begin
			raw_guards := List.append [model.invariants automaton location] !raw_guards;
			get_actions automaton location
			end)
		(model.locations_per_automaton automaton)
	in
	List.iter get_locations model.automata;
	!raw_guards
	
let get_inequalities model =
	let inequalities = ref [] in
	let guard_to_inequalities (guard) =
		match guard with
		|Continuous_guard g -> inequalities := List.append (pxd_get_inequalities g) !inequalities
		|Discrete_continuous_guard g -> inequalities := List.append (pxd_get_inequalities g.continuous_guard) !inequalities
		|_ -> inequalities := !inequalities	
	in
	List.iter guard_to_inequalities (get_raw_guards model);
	!inequalities
	
let linear_term_to_coef_array linear_term nb_parameters =
	let coef_array = Array.make (nb_parameters+1) (Finite (p_get_coefficient_in_linear_term linear_term)) in
	for i=0 to nb_parameters-1 do 
		coef_array.(i) <- Finite (p_get_variable_coefficient_in_internal_linear_term i linear_term)
	done;
	coef_array
	
let revert_coef_array coef_array =
	for i=0 to (Array.length coef_array - 1) do 
		coef_array.(i) <- neg_inf coef_array.(i)
	done;
	coef_array
	
let get_guards model =
	let guards = ref [] in
	let f (i) =
		let (clock,op,linear_term) = clock_guard_of_linear_inequality i in
		let coef_array = linear_term_to_coef_array linear_term model.nb_parameters in
		if op = Op_l || op = Op_le 
		then guards := List.append [(clock-model.nb_parameters,op,coef_array)] !guards
		else guards := List.append [(clock-model.nb_parameters,op,(revert_coef_array coef_array))] !guards
	in 
	List.iter f (get_inequalities model);
	!guards



let get_bounded p_bounds =
	let bounded_parameters = ref [] in
	for i=0 to (Array.length p_bounds)-1 do 
		let (min,max) = p_bounds.(i) in
		if (greater_inf min Minus_infinity) && (lesser_inf max Infinity) then bounded_parameters := List.append !bounded_parameters [i];
	done;
	!bounded_parameters

let get_pta_type pta_type bounded_parameters =
	(* Get the L/U nature *)
	match pta_type with
	(* General PTA *)
	| PTA_notLU -> Other
	(* L/U-PTA with parameters partitioned into L- and U-parameters *)
	| PTA_LU (l_parameters, u_parameters) -> 
		if includes l_parameters bounded_parameters then LU_Lbounded
		else if includes u_parameters bounded_parameters then LU_Ubounded
		else Other
			
	(* L-PTA *)
	| PTA_L -> PTA_L
	(* U-PTA *)
	| PTA_U -> PTA_U 
	
	
	
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
	
	let guards = get_guards model in
	
	let nb_clocks = model.nb_clocks in

	begin
		List.iter (fun (p) -> p_bounds.(p) <- get_p_bounds (model.parameters_bounds p) ) model.parameters;
		
		let pta_type = get_pta_type model.lu_status (get_bounded p_bounds) in
		
		let (l,u) = compute_maximal_constants pta_type p_bounds guards nb_clocks in
		
		begin
			lower_constants := l;
			upper_constants := u;

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
		

