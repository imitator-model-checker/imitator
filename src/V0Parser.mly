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

%}

%token <int> INT
%token <float> FLOAT
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
	| ampersand NAME OP_EQ arithmetic_expr DOUBLEDOT arithmetic_expr { ($2, $4, $6) }
	| ampersand NAME OP_EQ arithmetic_expr { ($2, $4, $4) }
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
	arithmetic_expr OP_PLUS expr_mult { $1 + $3 }
	| arithmetic_expr OP_MINUS expr_mult { $1 - $3 }
	| expr_mult { $1 }
;

expr_mult:
	expr_mult OP_MULT neg_atom { $1 * $3 }
	| expr_mult OP_DIV neg_atom { $1 / $3 } // Divison par zero !!
	| neg_atom { $1 }
;

neg_atom:
	atom { $1 }
	| OP_MINUS atom { 0 - $2 }
;

atom:
	LPAREN arithmetic_expr RPAREN { $2 }
	| number { $1 }
;

number:
	| INT { $1 }
;

// AJOUTER LA VIRGULE POUR LES RATIONNELS !