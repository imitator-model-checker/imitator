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


(************************************************************)
(* Dummy global variables *)
(************************************************************)

let l = ref Infinity
let u = ref Minus_infinity


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
		


(*------------------------------------------------------------*)
(* Function for preparing data structures for extrapolation *)
(*------------------------------------------------------------*)
let prepare_extrapolation () : unit =
	(* Retrieve the model *)
	let model = Input.get_model() in
	
	let nb_parameters = model.nb_parameters in
	
	begin
	(* Get the L/U nature *)
	match model.lu_status with
	(* General PTA *)
	| PTA_notLU -> raise (NotImplemented "prepare_extrapolation")
	(* L/U-PTA with parameters partitioned into L- and U-parameters *)
	| PTA_LU (l_parameters, u_parameters) -> raise (NotImplemented "prepare_extrapolation")
	(* L-PTA *)
	| PTA_L -> raise (NotImplemented "prepare_extrapolation")
	(* U-PTA *)
	| PTA_U -> raise (NotImplemented "prepare_extrapolation")
	end;
	
	let all_parameters_bounded = model.bounded_parameters in
	
	(*** DUMMY bound ***)
	let dummy_parameter = 0 in
	let p_bounds = model.parameters_bounds dummy_parameter in
	
	begin
	(* Check if unbounded below *)
	match p_bounds.lower with
	| Unbounded -> raise (NotImplemented "prepare_extrapolation")
	(* A finite bound is a pair NumConst.t and a Boolean true iff it is closed (i.e., closed inequality, and not strict) *)
	| Bounded (bound, is_closed) -> raise (NotImplemented "prepare_extrapolation")
	end;

	raise (NotImplemented "prepare_extrapolation")



(*------------------------------------------------------------*)
(* Apply extrapolation *)
(*------------------------------------------------------------*)
let px_lu_extrapolation (px_linear_constraint : LinearConstraint.px_linear_constraint) : LinearConstraint.px_linear_constraint list =
	(* Retrieve the model *)
	let model = Input.get_model() in
	
	let nb_parameters = model.nb_parameters in

	raise (NotImplemented "px_lu_extrapolation")


