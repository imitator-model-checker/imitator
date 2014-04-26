/***********************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created       : 2009/11/02
 * Last modified : 2010/03/04
***********************************************/

%{

(****************************************************************)
(** Functions *)
(****************************************************************)

(* Convert a float to a  NumConst.t *)
(*
let num_of_float f =
	(* Split the float in integer and fractional part *)
	let (fractional, integer) = modf f in
	let integer = int_of_float integer in
	(* Case of an integer *)
	if fractional = 0.0 then Num.num_of_int integer
	else(
		let fractional = string_of_float fractional in
		(* Remove the "0." in front of the fractional part *)
		let fractional = String.sub fractional 2 (String.length fractional -2) in
		(* Find the denominator *)
		let denominator = int_of_string ("1" ^ (String.make (String.length fractional) '0')) in
		let fractional = int_of_string fractional in
		(* Create the fraction *)
		 NumConst.numconst_of_frac (integer * denominator + fractional) denominator
	)
*)

%}

%token <int> INT
%token <float> FLOAT
%token <string> NAME

%token OP_PLUS OP_MINUS OP_MULT OP_DIV
%token OP_EQ

%token LPAREN RPAREN AMPERSAND SEMICOLON

%token EOF

/*%left OP_L OP_LEQ OP_EQ OP_GEQ OP_G*/
%left AMPERSAND
/* %left OP_PLUS OP_MINUS        /* lowest precedence */

%start main             /* the entry point */
%type <ParsingStructure.pi0> main
// %type <ParsingStructure.parsing_structure> main
%%

/**********************************************/
main:
	 declarations semicolon EOF
	{
		$1
	}
;

/**********************************************/

declarations:
	declaration declarations {$1 :: $2}
	| { [] }
;

/**********************************************/

declaration:
	ampersand NAME OP_EQ arithmetic_expr { ($2, $4) }
;

/**********************************************/

ampersand:
	AMPERSAND {}
	| {}
;

semicolon:
	SEMICOLON {}
	| {}
;

/***********************************************
  ARITHMETIC EXPRESSIONS
***********************************************/

arithmetic_expr:
	arithmetic_expr OP_PLUS expr_mult {  NumConst.add $1 $3 }
	| arithmetic_expr OP_MINUS expr_mult {  NumConst.sub $1 $3 }
	| expr_mult { $1 }
;

expr_mult:
	expr_mult OP_MULT neg_atom {  NumConst.mul $1 $3 }
	| expr_mult OP_DIV neg_atom {  NumConst.div $1 $3 } // Divison par zero !!
	| neg_atom { $1 }
;

neg_atom:
	atom { $1 }
	| OP_MINUS atom {  NumConst.neg $2 }
;

atom:
	LPAREN arithmetic_expr RPAREN { $2 }
	| number { $1 }
;

// rational:
// 	integer { $1 }
// 	| integer OP_DIV pos_integer { (Num.div_num $1 $3) }
// ;
// 
// integer:
// 	pos_integer { $1 }
// 	| OP_MINUS pos_integer { Num.minus_num $2 }
// ;

number:
	| INT {  NumConst.numconst_of_int $1 }
	/*** WARNING: numconst_of_float 0.4 gives 400000000001/1000000000000 due to the floating point representation!! ***/
	| FLOAT {  NumConst.numconst_of_float $1 }
;

// AJOUTER LA VIRGULE POUR LES RATIONNELS !