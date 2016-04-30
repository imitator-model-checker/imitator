(************************************************************
 *
 *                       UnitTestator
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Module NOT linked to IMITATOR, but used to perform various unit tests
 * 
 * File contributors : Étienne André
 * Created           : 2016/04/30
 * Last modified     : 2016/04/30
 *
 ************************************************************)

 

(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open OCamlUtilities

open ImitatorUtilities
open AbstractModel
open Result
open ModelPrinter
open Options

open LinearConstraint


;;


(************************************************************)
(************************************************************)
(* STARTING PROGRAM *)
(************************************************************)
(************************************************************)

print_message Verbose_standard "Starting UnitTestator...";


(************************************************************)
(************************************************************)
(* Testing LinearConstraint *)
(************************************************************)
(************************************************************)

print_message Verbose_standard "Testing LinearConstraint...";

let nb_parameters = 3 in
let nb_clocks = 3 in
let nb_discrete = 2 in

(* Some shortcuts *)
let p1 = 0 in
let p2 = 1 in
let p3 = 2 in
let x1 = 3 in
let x2 = 4 in
let x3 = 5 in
let d1 = 6 in
let d2 = 7 in

(* Ad-hoc function to print names *)
let variable_names variable_index = 
	if variable_index < nb_parameters then "p_" ^ (string_of_int variable_index)
	else if variable_index < nb_parameters + nb_clocks then "x_" ^ (string_of_int (variable_index - nb_parameters))
	else if variable_index < nb_parameters + nb_clocks + nb_discrete then "d_" ^ (string_of_int (variable_index - nb_parameters - nb_clocks))
	else raise (InternalError ("Variable index "  ^ (string_of_int variable_index) ^ " not found!"))
in


let string_of_op = function
	| Op_g  -> ">"
	| Op_ge -> ">="
	| Op_eq -> "="
	| Op_le -> "<="
	| Op_l  -> "<"
in


(* Set dimensions *)
set_dimensions nb_parameters nb_clocks nb_discrete;

(* Create linear terms *)

(* p1 + 3p2 - 8/3 p3 *)
let lt1 = make_pxd_linear_term [(NumConst.one, p1) ; (NumConst.numconst_of_int 3, p2) ; (NumConst.numconst_of_frac 8 3, p3)] NumConst.zero in

(* p1 + 3p2 - 8/3 p3 + x1 + 1 *)
let lt2 = make_pxd_linear_term [(NumConst.one, p1) ; (NumConst.numconst_of_int 3, p2) ; (NumConst.numconst_of_frac 8 3, p3) ; (NumConst.one, x1)] NumConst.one in

(* p1 + 3p2 - 8/3 p3 - x1 + 5/2*)
let lt3 = make_pxd_linear_term [(NumConst.one, p1) ; (NumConst.numconst_of_int 3, p2) ; (NumConst.numconst_of_frac 8 3, p3) ; (NumConst.minus_one, x1)] (NumConst.numconst_of_frac 5 2) in

(* p1 + 3p2 - 8 p3 + x1 + 1 *)
let lt4 = make_pxd_linear_term [(NumConst.one, p1) ; (NumConst.numconst_of_int 3, p2) ; (NumConst.numconst_of_int 3, p3) ; (NumConst.one, x1)] NumConst.one in

print_message Verbose_standard (string_of_pxd_linear_term variable_names lt1);
print_message Verbose_standard (string_of_pxd_linear_term variable_names lt2);
print_message Verbose_standard (string_of_pxd_linear_term variable_names lt3);
print_message Verbose_standard (string_of_pxd_linear_term variable_names lt4);

(* Create linear inequalities *)

(* lt1 = 0 *)
let li1 = make_pxd_linear_inequality lt1 Op_eq in

(* lt2 > 0 *)
let li2 = make_pxd_linear_inequality lt2 Op_g in

(* lt3 <= 0 *)
let li3 = make_pxd_linear_inequality lt3 Op_le in

(* lt4 <= 0 *)
let li4 = make_pxd_linear_inequality lt4 Op_le in

print_message Verbose_standard (string_of_pxd_linear_inequality variable_names li1);
print_message Verbose_standard (string_of_pxd_linear_inequality variable_names li2);
print_message Verbose_standard (string_of_pxd_linear_inequality variable_names li3);
print_message Verbose_standard (string_of_pxd_linear_inequality variable_names li4);

(* Create linear constraints *)

(* [lt1 = 0] *)
let lc1 = make_pxd_constraint [li1] in
let lc2 = make_pxd_constraint [li2] in
let lc3 = make_pxd_constraint [li3] in
let lc4 = make_pxd_constraint [li4] in

print_message Verbose_standard (string_of_pxd_linear_constraint variable_names lc1);

(* Retrieve inequality *)
let li1' = List.nth (pxd_get_inequalities lc1) 0 in
(*** WARNING! printing or not the constraint seems to have a side-effect on that constraint! ***)
print_message Verbose_standard (string_of_pxd_linear_inequality variable_names li1');

(* Transform to guard *)

let print_clock_guard linear_inequality =
	print_message Verbose_standard ("\nIs the following constraint a clock guard...?");
	print_message Verbose_standard (string_of_pxd_linear_inequality variable_names linear_inequality);
	try(
		let x, op, plt = clock_guard_of_linear_inequality linear_inequality in
		print_message Verbose_standard ("Yes! " ^ (variable_names x) ^ " " ^ (string_of_op op)  ^ " " ^ (string_of_p_linear_term variable_names plt));
	) with Not_a_clock_guard -> print_message Verbose_standard ("No!");
in

print_clock_guard li1';
print_clock_guard (List.nth (pxd_get_inequalities lc2) 0);
print_clock_guard (List.nth (pxd_get_inequalities lc3) 0);
print_clock_guard (List.nth (pxd_get_inequalities lc4) 0);


(************************************************************)
(************************************************************)
(* STARTING PROGRAM *)
(************************************************************)
(************************************************************)

print_message Verbose_standard "The end of UnitTestator!";

(* The end *)
print_newline();
flush Pervasives.stdout;
exit(0)

