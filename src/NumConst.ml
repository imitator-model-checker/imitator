(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2010/03/04
 * Last modified: 2010/03/09
 *
 ****************************************************************)

(**************************************************)
(* Type definition *)
(**************************************************)

open Gmp.Q.Infixes
open Gmp.Z.Infixes

(* type t *)

(*type t =          *)
(*  | Mpq of Gmp.Q.t*)
type t = Gmp.Q.t


(**************************************************)
(* Functions *)
(**************************************************)

(*let get_mpq = function*)
(*	| Mpq a -> a        *)
let get_mpq a = a

(**************************************************)
(** {2 User Conversions} *)
(**************************************************)

let get_num = Gmp.Q.get_num

let get_den = Gmp.Q.get_den 

let numconst_of_int i = (Gmp.Q.from_int i)

let numconst_of_frac i j = (Gmp.Q.from_ints i j)

let numconst_of_zfrac i j = (Gmp.Q.from_zs i j)

let numconst_of_float f = (* Mpq (Mpq.of_float i) DOES NOT WORK WELL *)
	(* Split the float in integer and fractional part *)
	let (fractional, integer) = modf f in
	let integer = int_of_float integer in
	(* Case of an integer *)
	if fractional = 0.0 then numconst_of_int integer
	else(
		let fractional = string_of_float fractional in
		(* Remove the "0." in front of the fractional part *)
		let fractional = String.sub fractional 2 (String.length fractional -2) in
		(* Find the denominator *)
		let denominator = int_of_string ("1" ^ (String.make (String.length fractional) '0')) in
		let fractional = int_of_string fractional in
		(* Create the fraction *)
		 numconst_of_frac (integer * denominator + fractional) denominator
	)
	
let numconst_of_string str =
	Gmp.Q.from_z (Gmp.Z.from_string str)

let numconst_of_mpq m = m

let numconst_of_mpz z = Gmp.Q.from_z z

let mpq_of_numconst = get_mpq

(**************************************************)
(** {2 Constants} *)
(**************************************************)

let zero = Gmp.Q.zero
(*let zero = numconst_of_int 0*)
let one = numconst_of_int 1
let minus_one = numconst_of_int (-1)

let string_of_numconst a =
	let q = get_den a in
	if q =! Gmp.Z.one then  		
		Gmp.Z.to_string (get_num a)
	else
		Gmp.Q.to_string (get_mpq a)


(**************************************************)
(** {2 Arithmetic Functions} *)
(**************************************************)

let arithmetic_gen op a b =
	let a = get_mpq a in
	let b = get_mpq b in
	let result = Gmp.Q.create () in
	op result a b;
	result

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

let random a b =
	let range = sub b a in
	let frnd  = Random.float 1.0 in
	let qrnd  = Gmp.Q.from_float frnd in
	add a (mul range qrnd)

(**************************************************)
(** {2 Comparison Functions} *)
(**************************************************)
let comparison_gen op a b =
	op (Gmp.Q.cmp (get_mpq a) (get_mpq b)) 0
	
let equal a b =
	Gmp.Q.equal (get_mpq a) (get_mpq b)

let neq a b =
	not (equal a b)

let l  = ( </ )
let le = ( <=/ )
let ge = ( >=/ )
let g  = ( >/ )

(*let l = comparison_gen (<)  *)
(*                            *)
(*let le = comparison_gen (<=)*)
(*                            *)
(*let ge = comparison_gen (>=)*)
(*                            *)
(*let g = comparison_gen (>)  *)


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
