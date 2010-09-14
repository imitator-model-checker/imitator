(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2010/07/13 
 *
 ****************************************************************)

(**************************************************)
(* Modules *)
(**************************************************)

open Global
open LinearConstraint
module Ppl = Ppl_ocaml
open Ppl
open AbstractImitatorFile

let unit_vector i =
 	fun j -> if i = j then NumConst.one else NumConst.zero

let simple_varnames i = "x" ^ (string_of_int i)


(* converts a generator to a 2d point wrt. the two given variables *)
let point_of_generator = function
	| Closure_Point (expr, c) 
	| Point (expr, c) ->  
			let x, y = 
				(evaluate_linear_term_ppl (unit_vector 0) expr,
				 evaluate_linear_term_ppl (unit_vector 1) expr) in
			let q = NumConst.numconst_of_mpz c in
			let xf = Gmp.Q.to_float (NumConst.mpq_of_numconst (NumConst.div x q)) in
			let yf = Gmp.Q.to_float (NumConst.mpq_of_numconst (NumConst.div y q)) in
			Some (xf, yf)
	| _ -> 
			print_message Debug_standard "No point";
			None

(* new type : FG_Point if the couple represent a Point and Vector if the couple represent a Ray *)
type float_generator = 
	| FG_Point of float*float
	| FG_Ray of float*float
	| FG_None

(* converts a generator to floating point representation *)
let convert_generator = function
	| Point (expr, c) -> 
			let x, y = 
				(evaluate_linear_term_ppl (unit_vector 0) expr,
				 evaluate_linear_term_ppl (unit_vector 1) expr) in
			let q = NumConst.numconst_of_mpz c in
			let xf = Gmp.Q.to_float (NumConst.mpq_of_numconst (NumConst.div x q)) in
			let yf = Gmp.Q.to_float (NumConst.mpq_of_numconst (NumConst.div y q)) in
			FG_Point (xf, yf)
	| Ray (expr) -> 
			let x, y = 
				(evaluate_linear_term_ppl (unit_vector 0) expr,
				 evaluate_linear_term_ppl (unit_vector 1) expr) in
			let xf = Gmp.Q.to_float (NumConst.mpq_of_numconst x) in
			let yf = Gmp.Q.to_float (NumConst.mpq_of_numconst y) in
			FG_Ray (xf, yf)
	| _ -> FG_None



(* comparison function for 2d points; used for sorting points *)
(* counter-clockwise wrt. a given center point (cx, cy) *)
let compare_points (cx, cy) (ax, ay) (bx, by) =
	let area = (ax -. cx) *. (by -. cy) -. (ay -. cy) *. (bx -. cx) in
	if area > 0.0 then 1 else	if area = 0.0 then 0 else	-1

let center_point points =
	match points with
		| p :: ps -> 
				let sumx, sumy =
					List.fold_left (fun (sx, sy) (x, y) -> 
						(sx +. x, sy +. y)
					) (0.0, 0.0) points in
				let len = float_of_int (List.length points) in
				(sumx /. len, sumy /. len)
		| _ -> (0.0, 0.0)
			

(* normalize a set of points (sort counter-clockwise, remove duplicates, close polygon) *)
let normalize_points points =
	(* sort counter-clockwise *)
	let compare = compare_points (center_point points) in
	let points = List.sort compare points in
	(* eat up consecutive identical points *)
  let head = List.hd points in
	let tail = List.tl points in
	let last = ref head in
	let points = head :: List.filter (fun p ->
		let unique = not (!last = p) in
		last := p; unique
	) tail in
	(* close polygon *)
	let points = List.append points [List.hd points] in
	(* return points *)
	points					
		
						
(* converts a linear_constraint to a set of 2d points wrt. the first two variables *)
let poly_to_points x y linear_constraint =
	(* copy constraint and replace strict operators by non-strict ones *)
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron (to_ppl_polyhedron (non_strictify linear_constraint)) in
	(* project to variables x,y *)
	project_to [x; y] poly;
	let generators = ppl_Polyhedron_get_generators poly in
	(* collect points for the generators *)
	let points = List.fold_left (fun ps gen ->
		let p = point_of_generator gen in 
		match p with
			| None -> ps
			| Some point -> point :: ps
	) [] generators in
	(* swap coordinates if necessary *)
	let points = if x < y then points else (
		List.map (fun (x,y) -> (y,x)) points
	) in
	normalize_points points
	
	
(* create a straight line from a point and a ray *)
let make_straight_line_ray r p =
	match (r,p) with
		| (FG_Ray  (a,b), FG_Point (c,d)) -> (let k=((a*.d)-.(b*.c)) in (b,(0.-.a),k))
		|_ -> (0.,0.,0.)


(* create a straight line from two points having the same abscissa or ordinate *)
let make_straight_line_points p1 p2 = 
	match (p1,p2) with
		|(FG_Point (a,b), FG_Point (c,d)) when a=c -> (1.,0.,0.-.a)
		|(FG_Point (a,b), FG_Point (c,d)) when b=d -> (0.,0.-.1.,b)
		|_ -> (0.,0.,0.)


(* find the intersection between two straight line *)
let intersection_straight_line d1 d2 =
	match (d1,d2) with
		|((0.,0.,0.),(d,e,f)) -> FG_Point ((0.),(0.))
		|((a,b,c),(0.,0.,0.)) -> FG_Point ((0.),(0.))
		|((a,b,c),(d,e,f)) -> (if b <> 0. then FG_Point ((((b*.f)-.(e*.c))/.((a*.e)-.(b*.d))),0.-.((c/.b)+.(((a*.b*.f)-.(a*.e*.c))/.((b*.a*.e)-.(b*.b*.d)))))
						else if e<>0. then FG_Point ((((b*.f)-.(e*.c))/.((a*.e)-.(b*.d))),0.-.((f/.e)+.(((d*.b*.f)-.(d*.e*.c))/.((e*.a*.e)-.(e*.b*.d)))))
						else FG_Point (0.,0.))

(* test if a point belong to a square line *)
let point_on_line p min_abs min_ord max_abs max_ord =
	match p with 
		|FG_Point (a,b) when ((min_abs <= a) & (a <= max_abs) & (min_ord <= b) & (b <= max_ord)) -> true
		|_ -> false


(* convert a linear constraint into two lists, one containing the points and the other containing the rays *)
let shape_of_poly x y linear_constraint =
	let poly = ppl_new_NNC_Polyhedron_from_NNC_Polyhedron linear_constraint in
	project_to [x; y] poly;
	(* get the generators *)
	let generators = ppl_Polyhedron_get_generators poly in
	(* convert generators to floating point format *)
	let float_generators = List.map convert_generator generators in
	(* split into points and rays *)
	let points, rays = List.fold_left (fun (ps, rs) fg -> 
		match fg with
			| FG_Point (x,y) -> ((x,y) :: ps, rs)
			| FG_Ray   (x,y) -> (ps, (x,y) :: rs)
			| FG_None -> (ps, rs)
	) ([], []) float_generators in
	(points, rays)


type direction =
	| Nowhere
	| North
	| South
	| East
	| West


(* convert a linear constraint to a set of 2d points wrt. the variables x,y *)
let points_of_zone x y linear_constraint min_abs min_ord max_abs max_ord =
	let points,ray = shape_of_poly x y linear_constraint in
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
		let high_border = make_straight_line_points (FG_Point (min_abs,max_ord)) (FG_Point (max_abs,max_ord)) in
		let right_border = make_straight_line_points (FG_Point (max_abs,min_ord)) (FG_Point (max_abs,max_ord)) in
		let low_border = make_straight_line_points (FG_Point (min_abs,min_ord)) (FG_Point (max_abs,min_ord)) in
		let left_border = make_straight_line_points (FG_Point (min_abs,min_ord)) (FG_Point (min_abs,max_ord)) in
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
				let straight_line = make_straight_line_ray (FG_Ray (fst (List.nth ray !i), snd (List.nth ray !i))) (FG_Point (fst (List.nth points !j),snd (List.nth points !j))) in
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
	let is_infinite = (List.length ray) > 0 in
	(is_infinite, normalize_points point_list)


(**************************************************)
(* Public Functions *)
(**************************************************)

(* returns a string with 2d points of the given constraint *)
let plot_2d x y linear_constraint =
	let shape = poly_to_points x y linear_constraint in
	List.fold_left (fun s (px, py) -> 
		s ^ (string_of_float px) ^ " " ^ (string_of_float py) ^ "\n"
	) "" shape	


let parameter_pairs pi0cube =
	let parameters = ref [] in
	for i=0 to Array.length pi0cube-1 do
		let min, max, _ = pi0cube.(i) in
		if NumConst.neq min max then parameters := i :: !parameters
	done;
	let rec make_pairs head tail =
		match tail with
			| [] -> []
			| _ -> List.rev_append 
				(List.map (fun b -> (head,b)) tail)
				(make_pairs (List.hd tail) (List.tl tail)) in
	make_pairs (List.hd !parameters) (List.tl !parameters)


(* print the cartography which correspond to the list of constraint *)
let cartography program pi0cube constraint_list nb_variables_projected cartography_name =
	(* replace strict inequalities *)
	let new_constraint_list = List.map LinearConstraint.non_strictify constraint_list in

	(* Find indexes of the projected variables *)
	let idx = DynArray.create () in
	let i = ref 0 in
	let nb = ref 0 in
	while !nb < nb_variables_projected && !i < (Array.length pi0cube) do
		match pi0cube.(!i) with
			| (x,y,_) when NumConst.neq x y -> (DynArray.insert idx !nb !i; nb := !nb + 1; i := !i + 1)
			|_-> i := !i + 1
	done;

	(* create an Array with all couple possible *)
	let couple_list = ref [] in
	for i=0 to DynArray.length idx -1 do
		for j=i to DynArray.length idx -1 do
			couple_list := (DynArray.get idx i,DynArray.get idx j)::!couple_list;
		done
	done;

	(* Keep only couple with diffrent values *)
	couple_list := List.filter ( fun (a,b) -> a<>b) !couple_list;

	(* make a cartography for each element of the couple_list *)
	let k = ref 0 in
	List.iter (fun (x_param, y_param) ->
		let x_name = program.variable_names x_param in
		let y_name = program.variable_names y_param in
		(* Create a script that will print the cartography *)
		let script_name = cartography_name^"_"^(string_of_int !k)^".sh" in
		let script = open_out script_name in
		(* Find the V0 zone *)
		let file_v0_name = cartography_name^"_v0_"^(string_of_int !k)^".txt" in
		let file_zone = open_out file_v0_name in
		let x_min, x_max, _ = pi0cube.(x_param) in
		let y_min, y_max, _ = pi0cube.(y_param) in
		let fstring numc = string_of_float (Gmp.Q.to_float (NumConst.mpq_of_numconst numc)) in
		let str_zone = 
				(fstring x_min)^" "^(fstring y_max)^"\n"
			^	(fstring x_max)^" "^(fstring y_max)^"\n"
			^	(fstring x_max)^" "^(fstring y_min)^"\n"
			^	(fstring x_min)^" "^(fstring y_min)^"\n"
			^	(fstring x_min)^" "^(fstring y_max)^"\n" in
		output_string file_zone str_zone;
		close_out file_zone;
		(* Beginning of the script *)
		let script_line = ref ("graph -T ps -C -X \"" ^ x_name ^ "\" -Y \"" ^ y_name ^ "\" ") in
		(* find the minimum and maximum abscissa and ordinate for each constraint and store them in a list *)

		(* get corners of v0 *)
		let init_min_abs, init_max_abs, _ = pi0cube.(x_param) in
		let init_min_ord, init_max_ord, _ = pi0cube.(y_param) in
		(* convert to float *)
		let init_min_abs = NumConst.float_of_numconst init_min_abs in
		let init_max_abs = NumConst.float_of_numconst init_max_abs in
		let init_min_ord = NumConst.float_of_numconst init_min_ord in
		let init_max_ord = NumConst.float_of_numconst init_max_ord in

		(* find mininma and maxima for axes *)
		let min_abs, max_abs, min_ord, max_ord =
		List.fold_left (fun limits constr ->
			let points, _ = shape_of_poly x_param y_param (from_ppl_polyhedron constr) in
			List.fold_left (fun limits (x,y) ->
				let current_min_abs, current_max_abs, current_min_ord, current_max_ord = limits in
				let new_min_abs = min current_min_abs x in
				let new_max_abs = max current_max_abs x in
				let new_min_ord = min current_min_ord y in
				let new_max_ord = max current_max_ord y in
				(new_min_abs, new_max_abs, new_min_ord, new_max_ord)
			) limits points
		) (init_min_abs, init_max_abs, init_min_ord, init_max_ord) new_constraint_list in
		(* add a margin of 1 unit *)
		let min_abs = min_abs -. 1.0 in
		let max_abs = max_abs +. 1.0 in
		let min_ord = min_ord -. 1.0 in
		let max_ord = max_ord +. 1.0 in

		(* print_message Debug_standard ((string_of_float !min_abs)^"  "^(string_of_float !min_ord)); *)
		(* Create a new file for each constraint *)
		for i=0 to List.length new_constraint_list-1 do
			(* find the points satisfying the constraint *)
			let is_infinite, vertices = points_of_zone (x_param) (y_param) (from_ppl_polyhedron (List.nth new_constraint_list i)) min_abs min_ord max_abs max_ord in
			(* print in the file the coordinates of the points *)
			let s = List.fold_left (fun s (px, py) -> 
				s ^ (string_of_float px) ^ " " ^ (string_of_float py) ^ "\n"
			) "" vertices in
			(* write to file *)
			let file_name = cartography_name^"_points_"^(string_of_int !k)^"_"^(string_of_int i)^".txt" in
			let file_out = open_out file_name in			
			output_string file_out s;
			close_out file_out;
			(* instructions to have the zones colored. If fst s = true then the zone is infinite *)
			if is_infinite then
				script_line := !script_line^"-m "^(string_of_int((i mod 5)+1+20))^" -q 0.3 "^file_name^" "
			else
				script_line := !script_line^"-m "^(string_of_int((i mod 5)+1))^" -q 0.7 "^file_name^" "
		done;

		(* File in which the cartography will be printed *)
		let final_name = cartography_name^"_"^(string_of_int !k)^".ps" in
		(* last part of the script *)
		script_line := !script_line^" -C -m 2 -q -1 "^file_v0_name^" > "^final_name;
		(* write the script into a file *)
		output_string script !script_line;
		(* Debug output *)
		print_message Debug_standard (
			"Plot cartography projected on parameters " ^ x_name ^ ", " ^ y_name
			^ " to file '" ^ final_name ^ "'");
		(* execute the script *)
		let execution = Sys.command !script_line in
		print_message Debug_high ("Result of the cartography execution: exit code "^(string_of_int execution));
		k := !k + 1
	) !couple_list

	
(* Compute the area of a convex polyhedron, given as a sorted list of points *)
let area_of_poly vertices = 
	match vertices with
		| (px, py) :: ps ->
			let last_x = ref px in
			let last_y = ref py in
			let det = List.fold_left (fun s (x, y) -> 
				let sum = s +. (!last_x *. y) -. (!last_y *. x) in
				last_x := x;
				last_y := y;
				sum
			) 0.0 ps in
			det /. 2.0 
		| _ -> 0.0
			
	
let coverage program pi0cube constraint_list =
	(* get the (first) pair of parameters for the analysis *)
	let x_param, y_param = List.hd (parameter_pairs pi0cube) in
	(* build a constraint representing the rectangle v0 *)
	let x_min, x_max, _ = pi0cube.(x_param) in
	let y_min, y_max, _ = pi0cube.(y_param) in
	let inequalities = [
		make_linear_inequality_ppl (Var x_param) Greater_Or_Equal_RS (Coef x_min);
		make_linear_inequality_ppl (Var x_param) Less_Or_Equal_RS (Coef x_max);
		make_linear_inequality_ppl (Var y_param) Greater_Or_Equal_RS (Coef y_min);
		make_linear_inequality_ppl (Var y_param) Less_Or_Equal_RS (Coef y_max);
	] in
	let v0 = make inequalities in
	(* intersect all zones with v0 *)
	let trimmed_constraints = List.map (fun constr -> 
		intersection [v0; constr]
	) constraint_list in
	(* get floating point vertices for zones *)
	let shapes = List.map (fun constr -> 
		poly_to_points x_param y_param constr
	) trimmed_constraints in
	(* get the area of v0 *)
	let v0_shape = poly_to_points x_param y_param v0 in
	let v0_area = area_of_poly v0_shape in
	(* compute the area of the zones *)
	let areas = List.map area_of_poly shapes in
	(* sum it up *)
	let area = List.fold_left (+.) 0.0 areas in
	(* compute the ratio *)
	area /. v0_area