/************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created       : 2011/11/23
 * Last modified : 2011/11/23
************************************************************/


%{
open ParsingStructure;;
open Global;;
open NumConst;;
  
  
let parse_error s =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	raise (ParsingError (symbol_start, symbol_end))
;;
 
%}

%token <string> FLOAT
%token <NumConst.t> INT
%token <string> NAME
%token <string> STR_FLOAT
%token <NumConst.t> STR_INT

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_EQ

%token OPEN_ATTRIBUTE OPEN_MODEL OPEN_NODE OPEN_XML
%token OPEN_END_ATTRIBUTE OPEN_END_MODEL OPEN_END_NODE
%token CLOSE CLOSE_XML
%token SINGLE_CLOSE 

%token STR_AND STR_BOOLEXPR STR_CLOCKS STR_CONST STR_CONSTANTS STR_GLOBALCONSTANTS STR_DISCRETE STR_FINALSTATE STR_FORMALISM_URL STR_GUARD STR_INITIALSTATE STR_INVARIANT STR_NAME STR_PARAMETERS STR_STATE STR_TYPE STR_UTF8 STR_VARIABLES STR_XMLNS

%token CT_ENCODING CT_FORMALISMURL CT_ID CT_NAME CT_NODETYPE CT_VERSION CT_XMLNS

%token EOF


%start main             /* the entry point */
%type <ParsingStructure.parsing_structure> main
%%




/************************************************************/
main:
	 header body footer EOF
	{
		$2
	}
;

/************************************************************
  HEADER
************************************************************/

header:
/* <?xml version="1.0" encoding="UTF-8"?> */
	OPEN_XML CT_VERSION OP_EQ STR_FLOAT CT_ENCODING OP_EQ STR_UTF8 CLOSE_XML
/* <model formalismUrl="http://alligator.lip6.fr/timed-automata.fml" xmlns="http://gml.lip6.fr/model"> */
	OPEN_MODEL CT_FORMALISMURL OP_EQ STR_FORMALISM_URL CT_XMLNS OP_EQ STR_XMLNS CLOSE {}
;

footer:
	OPEN_END_MODEL CLOSE {}
;

/************************************************************
  USEFUL THINGS
************************************************************/
close_attribute:
	OPEN_END_ATTRIBUTE CLOSE { }
;


/************************************************************
  BODY
************************************************************/
body:
/* 	TO DO: allow different orders, even all mixed together  */
	declarations states transitions { $1, [], [], [] }
;

/************************************************************
  VARIABLES AND CONSTANTS
************************************************************/
declarations:
/* 1 shift / reduce conflict here! */
	| variables constants { List.rev_append $1 $2 }
;




/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  Variables
-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

variables:
	| { [] }
/*     <attribute name="variables"> */
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_VARIABLES CLOSE
		variables_body
		close_attribute { $6 }
;

variables_body:
/* 	TO DO: allow several definitions of clocks and discrete, all mixed together  */
	| { [] }
	| clocks { [$1] }
	| discrete { [$1] }
	| clocks discrete { [ $1 ; $2 ] }
	| discrete clocks { [ $1 ; $2 ] }
;



/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  Constants
-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

constants:
	| { [] }
/*     <attribute name="constants"> */
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_CONSTANTS CLOSE
		constants_body
		close_attribute { $6 }
;

constants_body:
/* 	TO DO: allow several definitions of constants and parameters, all mixed together  */
	| { [] }
	| globalconstants { [$1] }
	| parameters { [$1] }
	| globalconstants parameters { [ $1 ; $2 ] }
	| parameters globalconstants { [ $1 ; $2 ] }
;



/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  Clocks
-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

clocks:
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_CLOCKS CLOSE
		list_of_names
		close_attribute { (Var_type_clock, $6) }
;

list_of_names:
	| name list_of_names { $1 :: $2 }
	| { [] }
;

name:
	OPEN_ATTRIBUTE CT_NAME OP_EQ STR_NAME CLOSE
		NAME
		close_attribute { ($6, None) }
;

/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  Discrete
-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

discrete:
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_DISCRETE CLOSE
		list_of_names
		close_attribute { (Var_type_discrete, $6) }
;

/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  Global constants
-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

globalconstants:
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_GLOBALCONSTANTS CLOSE
		list_of_names
/* 		TO DO: add type constant ! */
		close_attribute { (Var_type_parameter, $6) }
;

/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  Parameters
-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

parameters:
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_PARAMETERS CLOSE
		list_of_names
		close_attribute { (Var_type_parameter, $6) }
;


/************************************************************
  STATES
************************************************************/
states:
	| state states { $1 :: $2 }
	| { [] }
;

state:
/* <node id="2" nodeType="state"> */
	| OPEN_NODE CT_ID OP_EQ STR_INT CT_NODETYPE OP_EQ STR_STATE CLOSE
		state_attributes
		OPEN_END_NODE CLOSE { $4, $9 }
;

state_attributes:
/* TO DO: allow different orders! */
/* WARNING: 1 shift/reduce conflict here because of non-empty invariant  */
	| state_name state_type state_invariant { $1, $2, $3 }
;

/*<node id="1" nodeType="state">
        <attribute name="name">p</attribute>
        <attribute name="type">
            <attribute name="initialState"/>
            <attribute name="finalState"/>
        </attribute>
    </node>*/

/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  Name
-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

state_name:
/* <attribute name="name">q</attribute> */
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_NAME CLOSE
		NAME
		close_attribute { $6 }
;

/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  Type
-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

state_type:
	| { false, false }

	/* <attribute name="type"/> */
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_TYPE SINGLE_CLOSE { false, false }

	/* <attribute name="type"> [...] </attribute> */
	| state_open
	close_attribute { false, false }

	| state_open
	state_type_init 
	close_attribute { $2, false }

	| state_open
	 state_type_final
	close_attribute { false, $2 }

	| state_open
	state_type_init state_type_final
	close_attribute { $2, $3 }

	| state_open
	state_type_final state_type_init
	close_attribute { $3, $2 }
;

state_open:
	OPEN_ATTRIBUTE CT_NAME OP_EQ STR_TYPE CLOSE {}
;

state_type_init:
/* <attribute name="initialState"/> */
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_INITIALSTATE SINGLE_CLOSE { true }
;

state_type_final:
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_FINALSTATE SINGLE_CLOSE { true }
;


/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  Invariant
-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/

state_invariant:
	/* <attribute name="invariant"> [...] </attribute> */
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_INVARIANT CLOSE
	bool_expr
	close_attribute { $6 }

	/* <attribute name="invariant"></attribute> */
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_INVARIANT CLOSE
	close_attribute { [True_constraint] }

	/* <attribute name="invariant" /> */
	| OPEN_ATTRIBUTE CT_NAME OP_EQ STR_INVARIANT SINGLE_CLOSE { [True_constraint] }

	| { [True_constraint] }
;


/************************************************************
  TRANSITIONS
************************************************************/
transitions:
	{ }
;


/************************************************************
  BOOLEAN EXPRESSIONS
************************************************************/
bool_expr:
	|  OPEN_ATTRIBUTE CT_NAME OP_EQ STR_BOOLEXPR CLOSE
	conjunction
	close_attribute { $6 }
;

conjunction:
/*	|  OPEN_ATTRIBUTE CT_NAME OP_EQ STR_AND CLOSE
	conjunction
	expression
	close_attribute { $7 :: $6 }*/
	
/*	|  OPEN_ATTRIBUTE CT_NAME OP_EQ STR_AND CLOSE
	conjunction
	conjunction
	close_attribute { List.rev_append $7 $6 }*/
	
/*	|  OPEN_ATTRIBUTE CT_NAME OP_EQ STR_AND CLOSE
	expression
	conjunction
	close_attribute { $6 :: $7 }*/
	
	| expression { [$1] }
;

/************************************************************
  EXPRESSIONS
************************************************************/
expression:
	| linear_expression { $1 }
;


linear_expression:
	| linear_term { Linear_constraint ($1 , OP_EQ , $1) }
;

linear_term:
	| const { Linear_term $1 } 
;

const:
	/*<attribute name="const">4</attribute>*/
	OPEN_ATTRIBUTE CT_NAME OP_EQ STR_CONST CLOSE
	rational
	close_attribute
		{ Constant (NumConst.zero) } 
;

/************************************************************
  NUMBERS
************************************************************/

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
