open Gmp
open Gmp.Q.Infixes
open LinearConstraint


let len = Array.length and aini = Array.init

let print m mat = let i = len mat and j = len mat.(0) in
print_string m; print_newline ();
  for i = 0 to i-1 do for j = 0 to j-1 do print_float mat.(i).(j); print_char ' ' done;
  print_newline () done

(* gloabl reference to the offset for the first considered variable *)
let var_offset = ref 0

let one = Q.from_int 1
let zero = Q.zero
let abs q = if (Q.sgn q < 0) then Q.neg q else q 

let zero_vector = 
	fun _ -> NumConst.zero 

let unit_matrix n = 
	let mat = Array.make_matrix n n zero in
	for i = 0 to (n-1) do
		mat.(i).(i) <- one
	done;
	mat

let negate_vector =	Array.map NumConst.neg


(* build the row of a nxn matrix from a linear term *)
let make_row n term =
	(* function for term evaluation, considers the offset to the first variable *)
  let unit_vector i = 
		fun j -> if j = (i + !var_offset) then NumConst.one else NumConst.zero in
	let row = Array.make n zero in
	for i = 0 to (n-1) do
		let a = LinearConstraint.evaluate_linear_term (unit_vector i) term in
		let q = NumConst.mpq_of_numconst a in
		row.(i) <- q
	done;
	row

(* construct a (square) nxn matrix and a vector from a list of linear assignments,  *)
(* where an assignment is a pair of a variable index and a linear term *)
let make n assignments =	
	let mat = unit_matrix n in
	let vec = Array.make n NumConst.zero in
	let insert_row = fun assignment -> (
		(* build row *)
		let v, term = assignment in
		let row = make_row n term in
		mat.(v) <- row;
		(* get constant offset *)
		let coef = LinearConstraint.evaluate_linear_term zero_vector term in
		vec.(v) <- coef
	) in
	List.iter insert_row assignments;	
	(mat, vec) 

(* construct a linear term from a row and a constant *)
let make_term row coef =
	(* build a list of coefficients and variables for non-zero entries *)
	let coefs = ref [] in	
	Array.iteri (fun i a -> 	
		if a <>/ zero then
			let coef = (NumConst.numconst_of_mpq a, i + !var_offset) in
			coefs := coef :: !coefs
	) row;
	LinearConstraint.make_linear_term !coefs coef

(* convert an affine mapping to a list of variable updates *)
let make_updates mat vec =
	let updates = ref [] in
	Array.iteri (fun i row ->
		let update = make_term row vec.(i) in
		(* drop identities *)
		if not (update = Pl (Var (i + !var_offset), Coef NumConst.zero)) then
		  updates := (i + !var_offset, update) :: !updates
	) mat;
	!updates
	
(* invert a matrix *)
exception Singular
let invert mat =
	let l = len mat in
	let am = Array.mapi (fun m row -> (Array.append row
	      (aini l (fun n -> if m=n then one else zero)))) mat in
	for i = 0 to l-1 do (
		let im = ref 0 and mv = ref (abs am.(i).(i)) in
	  for j = i+1 to l-1 do (
			let ae = abs am.(j).(i) in
	  	if (!mv < ae) then (mv := ae; im := j)
		) done;
	  if !mv = zero then raise Singular;
	  if !im > i then (
			for n = i to (2*l - 1) do
	      (let s = am.(i).(n) in am.(i).(n) <- am.(!im).(n); am.(!im).(n) <- s)
			done
		);
	  let r = one // am.(i).(i) in
	  for j = i to 2*l - 1 do (
			am.(i).(j) <- r */ am.(i).(j)
		) done;
	  for k = i+1 to l-1 do (
			let f = am.(k).(i) in
	    for j = i+1 to 2*l - 1 do (
				am.(k).(j) <- am.(k).(j) -/ f */ am.(i).(j))
	    done);
		done)
	done;
	for i = 0 to l-1 do (
		for j = i+1 to l-1 do (
			let p = am.(i).(j) in
	    for k = i+1 to 2*l - 1 do
	    	(am.(i).(k) <- am.(i).(k) -/ am.(j).(k) */ p)
			done)
		done)
	done;
	Array.map (fun row -> Array.sub row l l) am

(* invert a list of discrete variable updates, where the number of variables is n *)
let invert_updates variables assignments =
	(* get the number of variables *)
	let n = List.length variables in
	(* if no variables present, return empty list *)
	if n = 0 then [] else (
		(* set the offset to the first variable *)
		var_offset := List.hd variables;
		(* convert the assignments to an affine mapping (A,B) *)
		let mat, vec = make n assignments in
		(* invert the matrix A *)
		let inv_mat = invert mat in
		(* negate the vector B *)
		let inv_vec = negate_vector vec in
		(* convert the affine mapping (A',B') back to variable updates *)
		make_updates inv_mat inv_vec
	)
