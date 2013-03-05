/***********************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created       : 2010/01/04
 * Last modified : 2013/03/05
***********************************************/

%{

	(* open Global *)
	
%}

%token <NumConst.t> INT
// %token <float> FLOAT
%token <string> NAME

%token OP_PLUS OP_MINUS OP_MULT OP_DIV
%token OP_EQ

%token LPAREN RPAREN AMPERSAND DOUBLEDOT SEMICOLON

%token EOF

/*%left OP_L OP_LEQ OP_EQ OP_GEQ OP_G*/
%left AMPERSAND
/* %left OP_PLUS OP_MINUS        /* lowest precedence */

%start main             /* the entry point */
%type <ParsingStructure.v0> main
%%

/**********************************************/
main:
	 declarations semicolon_opt EOF
	{
		$1
	}
	/* | EOF
	{ raise (InternalError "ploum"); } */
;

/**********************************************/

declarations:
	declaration declarations {$1 :: $2}
	| { [] }
;

/**********************************************/

declaration:
	| ampersand_opt NAME OP_EQ arithmetic_expr DOUBLEDOT arithmetic_expr { ($2, $4, $6) }
	| ampersand_opt NAME OP_EQ arithmetic_expr { ($2, $4, $4) }
;

/**********************************************/

ampersand_opt:
	AMPERSAND {}
	| {}
;

semicolon_opt:
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
	expr_mult OP_MULT atom { NumConst.mul $1 $3 }
	| expr_mult OP_DIV atom { NumConst.div $1 $3 } // WARNING: Possible divison par zero !!
	| atom { $1 }
;

atom:
	positive_atom { $1 }
	| OP_MINUS positive_atom { NumConst.neg $2 }
;

positive_atom:
	LPAREN arithmetic_expr RPAREN { $2 }
	| number { $1 }
;

number:
	| INT { $1 }
;

// AJOUTER LA VIRGULE POUR LES RATIONNELS !