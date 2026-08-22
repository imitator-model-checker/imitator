/************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Parser for on-the-fly model updates
 *
 * File contributors : Étienne André
 * Created           : 2025/05/20
 *
 ************************************************************/


%{
open ParsingStructure;;
open ParsingStructureUtilities;;
open Exceptions;;
open ImitatorUtilities;;

%}

%token <string> NAME
/* %token <string> FLOAT */

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token APOSTROPHE COLON COMMA OP_CONJUNCTION OP_DISJUNCTION OP_IMPLIES SEMICOLON


%token
	CT_NEWLOCATION


%token EOF


%start main             /* the entry point */
%type <ParsingStructure.on_the_fly_update> main
%%

/************************************************************/
main:
	command EOF
	{
		$1
	}
;



/************************************************************
  CONTROLLABLE ACTIONS
************************************************************/
command:
	| CT_NEWLOCATION LPAREN NAME RPAREN semicolon_opt { Parsed_new_location $3 }
;


semicolon_opt:
	| SEMICOLON { }
	| { }
;

