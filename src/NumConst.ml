(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2010/03/04
 * Last modified: 2013/01/30
 *
 ****************************************************************)

(**************************************************)
(* Type definition *)
(**************************************************)

open Gmp.Q.Infixes

(*type t =          *)
(*  | Mpq of Gmp.Q.t*)
type t = Gmp.Q.t

exception Unknown_numconst of string


(**************************************************)
(* Functions *)
(**************************************************)

let get_mpq a = a

(**************************************************)
(** {2 User Conversions} *)
(**************************************************)

let get_num = Gmp.Q.get_num

let get_den = Gmp.Q.get_den 

let numconst_of_int i = (Gmp.Q.from_int i)

let numconst_of_frac i j = (Gmp.Q.from_ints i j)

let numconst_of_zfrac i j = (Gmp.Q.from_zs i j)

(* Cannot string_of_int (can overflow) ; behavior unspecified if no integer string *)
let numconst_of_int_string s =
	Gmp.Q.from_z (Gmp.Z.from_string s)


let numconst_of_float f = (* Mpq (Mpq.of_float i) DOES NOT WORK WELL *)
	(* Split the float in integer and fractional part *)
	let (fractional, integer) = modf f in
	let integer = numconst_of_int (int_of_float integer) in
	(* Case of an integer *)
	if fractional = 0.0 then integer
	else(
		let fractional = string_of_float fractional in
		(* Remove the "0." in front of the fractional part *)
		let fractional = String.sub fractional 2 (String.length fractional -2) in
		(* Find the denominator *)
		let denominator = numconst_of_int_string ("1" ^ (String.make (String.length fractional) '0')) in
		let fractional = numconst_of_int_string fractional in
		(* Create the fraction *)
		 (integer */ denominator +/ fractional) // denominator
	)
	
let numconst_of_string str =
	(* Case int *)
	if Str.string_match (Str.regexp "^[0-9]+$") str 0 then
		numconst_of_int_string str
	(* Case fraction *)
	else if Str.string_match (Str.regexp "^[0-9]+/[0-9]+$") str 0 then
		let parts = Str.split (Str.regexp_string "/") str in
		let denominator =  numconst_of_int_string (List.nth parts 0) in
		let fractional = numconst_of_int_string(List.nth parts 1) in
		denominator // fractional
	(* Case float *)
	else if Str.string_match (Str.regexp "^[0-9]+.[0-9]+$") str 0 then numconst_of_float (float_of_string str)
	(* Otherwise *)
	else raise (Unknown_numconst ("Impossible to cast the string '" ^ str ^ "' to a NumConst in function numconst_of_string. Unknown type."))
	(* 	Gmp.Q.from_z (Gmp.Z.from_string str) *)
	

	
let numconst_of_mpq m = m

let numconst_of_mpz z = Gmp.Q.from_z z

let mpq_of_numconst = get_mpq

let string_of_numconst a =
	(* Avoid 0/1 *)
	if a =/ (Gmp.Q.zero) then "0" else(
		(* Avoid 1/1 *)
		let den = get_den a in 
		if den = (Gmp.Z.from_int 1) then
			Gmp.Z.to_string (get_num a)
		else
			(* Nice predefined function *)
			Gmp.Q.to_string (get_mpq a)
	)

(**************************************************)
(** {2 Constants} *)
(**************************************************)

let zero = Gmp.Q.zero
(*let zero = numconst_of_int 0*)
let one = numconst_of_int 1
let minus_one = numconst_of_int (-1)


(**************************************************)
(** {2 Arithmetic Functions} *)
(**************************************************)

(*let arithmetic_gen op a b =
	let a = get_mpq a in
	let b = get_mpq b in
	let result = Gmp.Q.create () in
	op result a b;
	result*)

let add = ( +/ ) 
(*	arithmetic_gen Gmp.Q.add*)

let sub = ( -/ ) 
(*	arithmetic_gen Gmp.Q.sub*)

let mul = ( */ ) 
(*	arithmetic_gen Gmp.Q.mul*)

let div = ( // ) 
(*	arithmetic_gen Gmp.Q.div*)

let neg a = Gmp.Q.neg a	
(*	let a = get_mpq a in           *)
(*	let result = Gmp.Q.create () in*)
(*	Gmp.Q.neg result a;            *)
(*	Mpq result                     *)

let abs a =
	if Gmp.Q.cmp (get_mpq a) (Gmp.Q.zero) >= 0 then a
	else neg a



(** WARNING: not really tested !!! *)
let find_multiple_gen tcdiv_q base_number step number =
	(* 1) Compute m = number - base_number *)
	let m = sub number base_number in
	
	(* 2) Compute d = m / step (hence, m = n * step) *)
	let d = div m step in
	
	(* 3) Find the closest integer k below d *)
		(* 3a) Extract numerator and denominator (integers) *)
	let d_num = get_num d in
	let d_den = get_den d in
		(* 3b) Use integer division (rounded above/below) *)
	let k = tcdiv_q d_num d_den in
	
	(* 4) Return n = k * step + base_number *)
	add
		(mul 
			(Gmp.Q.from_z k)
			step
		)
		base_number

(** WARNING: not really tested !!! *)
(** Find the closest multiple of step from base_number below (or equal to) number *)
(* That is: find the largest n s.t. n = k * step + base_number, with k integer, and n <= number *)
let find_multiple_below =
	find_multiple_gen Gmp.Z.tdiv_q
		

(** WARNING: not really tested !!! *)
(** Find the closest multiple of step from base_number above (or equal to) number *)
(* That is: find the smallest n s.t. n = k * step + base_number, with k integer, and n >= number *)
let find_multiple_above =
	find_multiple_gen Gmp.Z.cdiv_q
		


(**************************************************)
(** {2 Comparison Functions} *)
(**************************************************)
let equal a b =
	Gmp.Q.equal (get_mpq a) (get_mpq b)

let neq a b =
	not (equal a b)

let l  = ( </ )
let le = ( <=/ )
let ge = ( >=/ )
let g  = ( >/ )


(**************************************************)
(** {2 Test Functions} *)
(**************************************************)
let is_integer n =
	(* Zero is an integer! *)
	(*** WARNING: added one as well, because the cmp test below did not work for one *)
	equal n zero || equal n one ||
	(* Then check denominator = numerator *)
	(
		(*print_string (Gmp.Z.to_string (get_num n));
		print_string "/";
		print_string (Gmp.Z.to_string (get_num n));
		print_string "\n";
		print_string ("  Check num = den: " ^ (string_of_bool ((get_num n) = (get_den n))));
		print_string "\n";
		print_string ("  Check cmp (num , den): " ^ (string_of_int (Gmp.Z.cmp (get_num n) (get_den n))));
		print_string "\n";*)
		(*** WARNING: should not use directly (get_num n) = (get_den n), because got strange results *)
		(Gmp.Z.cmp (get_num n) (get_den n)) > 0
	)



(**************************************************)
(* Tests *)
(**************************************************)
(*
let a = NumConst.numconst_of_float 0.4 in
let b = NumConst.numconst_of_frac 1 3 in
let c = NumConst.numconst_of_int 2 in
let d = NumConst.numconst_of_float 2.00 in
let e = NumConst.numconst_of_frac 306 153 in

print_string ("\n a = " ^ (NumConst.string_of_numconst a));
print_string ("\n b = " ^ (NumConst.string_of_numconst b));
print_string ("\n c = " ^ (NumConst.string_of_numconst c));
print_string ("\n d = " ^ (NumConst.string_of_numconst d));
print_string ("\n e = " ^ (NumConst.string_of_numconst e));

print_string ("\n a + b = " ^ (NumConst.string_of_numconst (NumConst.add a b)));
print_string ("\n a + c = " ^ (NumConst.string_of_numconst (NumConst.add a c)));
print_string ("\n b + c = " ^ (NumConst.string_of_numconst (NumConst.add b c)));

print_string ("\n a - b = " ^ (NumConst.string_of_numconst (NumConst.sub a b)));
print_string ("\n a - c = " ^ (NumConst.string_of_numconst (NumConst.sub a c)));
print_string ("\n b - c = " ^ (NumConst.string_of_numconst (NumConst.sub b c)));

print_string ("\n a * b = " ^ (NumConst.string_of_numconst (NumConst.mul a b)));
print_string ("\n a * c = " ^ (NumConst.string_of_numconst (NumConst.mul a c)));
print_string ("\n b * c = " ^ (NumConst.string_of_numconst (NumConst.mul b c)));

print_string ("\n a / b = " ^ (NumConst.string_of_numconst (NumConst.div a b)));
print_string ("\n a / c = " ^ (NumConst.string_of_numconst (NumConst.div a c)));
print_string ("\n b / c = " ^ (NumConst.string_of_numconst (NumConst.div b c)));

print_string ("\n -a = " ^ (NumConst.string_of_numconst (NumConst.neg a)));
print_string ("\n -b = " ^ (NumConst.string_of_numconst (NumConst.neg b)));
print_string ("\n -c = " ^ (NumConst.string_of_numconst (NumConst.neg c)));

print_string ("\n |a| = " ^ (NumConst.string_of_numconst (NumConst.abs a)));
print_string ("\n |-a| = " ^ (NumConst.string_of_numconst (NumConst.abs (NumConst.neg a))));

print_string ("\n a = b ? " ^ (string_of_bool (NumConst.equal a b)));
print_string ("\n a <> b ? " ^ (string_of_bool (NumConst.neq a b)));
print_string ("\n a < b ? " ^ (string_of_bool (NumConst.l a b)));
print_string ("\n a <= b ? " ^ (string_of_bool (NumConst.le a b)));
print_string ("\n a >= b ? " ^ (string_of_bool (NumConst.ge a b)));
print_string ("\n a > b ? " ^ (string_of_bool (NumConst.g a b)));

print_string ("\n d = e ? " ^ (string_of_bool (NumConst.equal d e)));
print_string ("\n d <> e ? " ^ (string_of_bool (NumConst.neq d e)));
print_string ("\n d < e ? " ^ (string_of_bool (NumConst.l d e)));
print_string ("\n d <= e ? " ^ (string_of_bool (NumConst.le d e)));
print_string ("\n d >= e ? " ^ (string_of_bool (NumConst.ge d e)));
print_string ("\n d > e ? " ^ (string_of_bool (NumConst.g d e)));


print_newline();

exit 0;
*)
