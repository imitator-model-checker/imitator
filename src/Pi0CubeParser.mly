/***********************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created       : 2010/01/04
 * Last modified : 2010/03/04
***********************************************/

%{
	open Global;;
	open NumConst;;
%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> NAME

%token OP_PLUS OP_MINUS OP_MULT OP_DIV
%token OP_EQ

%token LPAREN RPAREN AMPERSAND DOUBLEDOT CT_STEP SEMICOLON

%token EOF

/*%left OP_L OP_LEQ OP_EQ OP_GEQ OP_G*/
%left AMPERSAND
/* %left OP_PLUS OP_MINUS        /* lowest precedence */

%start main             /* the entry point */
%type <ParsingStructure.pi0cube> main
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
	| ampersand NAME OP_EQ arithmetic_expr DOUBLEDOT arithmetic_expr CT_STEP arithmetic_expr { ($2, $4, $6, $8) }
	| ampersand NAME OP_EQ arithmetic_expr DOUBLEDOT arithmetic_expr { ($2, $4, $6, NumConst.one) }
	| ampersand NAME OP_EQ arithmetic_expr { ($2, $4, $4, NumConst.one) }
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
	arithmetic_expr OP_PLUS expr_mult { NumConst.add $1 $3 }
	| arithmetic_expr OP_MINUS expr_mult { NumConst.sub $1 $3 }
	| expr_mult { $1 }
;

expr_mult:
	expr_mult OP_MULT neg_atom { NumConst.mul $1 $3 }
	| neg_atom { $1 }
;

neg_atom:
	atom { $1 }
	| OP_MINUS atom { NumConst.neg $2 }
;

atom:
	LPAREN arithmetic_expr RPAREN { $2 }
	| rational { $1 }
;

rational:
	integer { $1 }
	| float { $1 }
	| integer OP_DIV pos_integer { (NumConst.div $1 $3) }
;

integer:
	pos_integer { $1 }
	| OP_MINUS pos_integer { NumConst.neg $2 }
;

pos_integer:
	INT { $1 }
;

float:
  pos_float { $1 }
	| OP_MINUS pos_float { NumConst.neg $2 }  
;

pos_float:
  FLOAT { 
		let fstr = $1 in
		let point = String.index fstr '.' in
		(* get integer part *)
		let f = if point = 0 then ref NumConst.zero else (
			let istr = String.sub fstr 0 point in
		  ref (NumConst.numconst_of_int (int_of_string istr))
		) in		
		(* add decimal fraction part *)
		let numconst_of_char = function
			| '0' -> NumConst.zero
			| '1' -> NumConst.one
			| '2' -> NumConst.numconst_of_int 2
			| '3' -> NumConst.numconst_of_int 3
			| '4' -> NumConst.numconst_of_int 4
			| '5' -> NumConst.numconst_of_int 5
			| '6' -> NumConst.numconst_of_int 6
			| '7' -> NumConst.numconst_of_int 7
			| '8' -> NumConst.numconst_of_int 8
			| '9' -> NumConst.numconst_of_int 9
			| _ ->  raise (ParsingError (0,0)) in
		let ten = NumConst.numconst_of_int 10 in
		let dec = ref (NumConst.numconst_of_frac 1 10) in
		for i = point+1 to (String.length fstr) - 1 do
			let c = fstr.[i] in
			let d = numconst_of_char c in
			f := NumConst.add !f (NumConst.mul !dec d);
			dec := NumConst.div !dec ten 
		done;		
		!f
	} 
;
