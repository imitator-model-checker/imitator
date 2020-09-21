/***********************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, CNRS, LIPN (France)
 * 
 * Description:   parser for CosyVerif properties (temporary version, and quite ugly engineering)
 * WARNING:       This file is mostly copy-pasted from ModelParser.mll
 *
 * Author:        Etienne Andre
 * 
 * Created       : 2015/07/10
 * Last modified : 2015/07/31
***********************************************/

%{
open ParsingStructure;;
open Exceptions;;
open NumConst;;
  
  
let parse_error s =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	raise (ParsingError (symbol_start, symbol_end))
;;
 
%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> NAME
%token <string> STRING

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN /*LBRACE RBRACE */LSQBRA RSQBRA
%token /*AMPERSAND*/ /*APOSTROPHE*/ /*COLON*/ COMMA /*PIPE*/ /*SEMICOLON*/

%token
	CT_ALWAYS CT_AND CT_AUTOMATON
	CT_BAD CT_BEFORE
	CT_CARTO CT_CLOCK
	CT_DISCRETE CT_DO
	CT_END CT_EVENTUALLY CT_EVERYTIME
	CT_FALSE
	CT_GOTO
	CT_HAPPENED CT_HAS
	CT_IF CT_INIT CT_INITIALLY
	CT_LOC CT_LOCATIONS
	CT_NEXT CT_NOT
	CT_ONCE CT_OR
	CT_PARAMETER CT_PROJECTRESULT CT_PROPERTY
	CT_REGION
	CT_SEQUENCE CT_STOP CT_SYNC CT_SYNCLABS
	CT_THEN CT_TRUE
	CT_UNKNOWN CT_UNREACHABLE
	CT_VAR
	CT_WAIT CT_WHEN CT_WHILE CT_WITHIN


%token EOF

// %left PIPE CT_OR        /* lowest precedence */
// %left AMPERSAND CT_AND  /* medium precedence */
// %nonassoc CT_NOT        /* highest precedence */


%start main             /* the entry point */
%type <ParsingStructure.property_definition> main
%%

/**********************************************/
main:
	 pattern EOF
	{
		Some $1
	}
;


// List of patterns
// NOTE: copied from ModelParser.mly
pattern:
	// Unreachability
	| CT_UNREACHABLE bad_definition { $2 }
	
	/* if a2 then a1 has happened before */
	| CT_IF NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_BEFORE { Action_precedence_acyclic ($4, $2) }
	/* everytime a2 then a1 has happened before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_BEFORE { Action_precedence_cyclic ($4, $2) }
	/* everytime a2 then a1 has happened once before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_ONCE CT_BEFORE { Action_precedence_cyclicstrict ($4, $2) }
	
	// PATTERNS NOT IMPLEMENTED
	/* if a1 then eventually a2 */
// 	| CT_IF NAME CT_THEN CT_EVENTUALLY NAME { Eventual_response_acyclic ($2, $5) }
	/* everytime a1 then eventually a2 */
// 	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME { Eventual_response_cyclic ($2, $5) }
	/* everytime a1 then eventually a2 once before next */
// 	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_ONCE CT_BEFORE CT_NEXT { Eventual_response_cyclicstrict ($2, $5) }
	
	/* a within d */
	| NAME CT_WITHIN linear_expression { Action_deadline ($1, $3) }
	
	/* if a2 then a1 happened within d before */
	| CT_IF NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_WITHIN linear_expression CT_BEFORE { TB_Action_precedence_acyclic ($4, $2, $8) }
	/* everytime a2 then a1 happened within d before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_WITHIN linear_expression CT_BEFORE { TB_Action_precedence_cyclic ($4, $2, $8) }
	/* everytime a2 then a1 happened once within d before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_ONCE CT_WITHIN linear_expression CT_BEFORE { TB_Action_precedence_cyclicstrict ($4, $2, $9) }
	
	/* if a1 then eventually a2 within d */
	| CT_IF NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression { TB_response_acyclic ($2, $5, $7) }
	/* everytime a1 then eventually a2 within d */
	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression { TB_response_cyclic ($2, $5, $7) }
	/* everytime a1 then eventually a2 within d once before next */
	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression CT_ONCE CT_BEFORE CT_NEXT { TB_response_cyclicstrict ($2, $5, $7) }
	
	/* sequence a1, ..., an */
	| CT_SEQUENCE name_nonempty_list { Sequence_acyclic ($2) }
	| CT_SEQUENCE LPAREN name_nonempty_list RPAREN { Sequence_acyclic ($3) } /* with parentheses */
	/* always sequence a1, ..., an */
	| CT_ALWAYS CT_SEQUENCE name_nonempty_list { Sequence_cyclic ($3) }
	| CT_ALWAYS CT_SEQUENCE LPAREN name_nonempty_list RPAREN { Sequence_cyclic ($4) } /* with parentheses */
;


bad_definition:
	// HACK: only allow unreachability of ONE location
	| loc_predicate { let a,b = $1 in (Parsed_unreachable_locations [[Parsed_unreachable_loc (a , b)]]) }
;

loc_predicate:
// 	WARNING / HACK : we cannot use 'CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME' because the name of the PTA is not know in GrML… (in fact it is always "1"…)
// So since there is only one PTA in GrML, we drop the PTA name
	NAME { ("noname", $1) } // Here, "1" is the PTA name (huge HACK -- will be addressed in ParsingUtility), and $1 is the location name
;


linear_expression:
	linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) } /* linear_term a la deuxieme place */
;

linear_term:
	rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| OP_MINUS NAME { Variable (NumConst.minus_one, $2) }
	| NAME { Variable (NumConst.one, $1) }
// 	| LPAREN linear_expression RPAREN { $2 }
	| LPAREN linear_term RPAREN { $2 }
;

name_nonempty_list:
	NAME COMMA name_nonempty_list { $1 :: $3}
	| NAME comma_opt { [$1] }
;




/**********************************************/
/** RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES */
/***********************************************/

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



comma_opt:
	| COMMA { }
	| { }
;


