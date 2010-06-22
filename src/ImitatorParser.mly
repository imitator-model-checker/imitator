/***********************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created       : 2009/09/07
 * Last modified : 2010/05/07
***********************************************/

%{
open ParsingStructure;;
open Global;;
		  
(* global reference to collect all local variables in all automata *)
(* as triples (type, varname, scope).                              *)
let declarations = ref [];;	
  
let parse_error s =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	raise (ParsingError (symbol_start, symbol_end))
;;
 
%}

%token <int> INT
%token <string> NAME
%token <string> STRING

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token AMPERSAND APOSTROPHE COLON COMMA PIPE SEMICOLON

/* CT_ALL CT_ANALOG CT_ASAP CT_BACKWARD CT_CLDIFF CT_D  CT_ELSE CT_EMPTY  CT_ENDHIDE CT_ENDIF CT_ENDREACH CT_ENDWHILE CT_FORWARD CT_FREE CT_FROM  CT_HIDE CT_HULL CT_INTEGRATOR CT_ITERATE CT_NON_PARAMETERS CT_OMIT CT_POST CT_PRE CT_PRINT CT_PRINTS CT_PRINTSIZE CT_REACH  CT_STOPWATCH CT_THEN CT_TO CT_TRACE CT_USING  CT_WEAKDIFF CT_WEAKEQ CT_WEAKGE CT_WEAKLE  */

%token CT_AND CT_AUTOMATON CT_ANALOG CT_CLOCK CT_DISCRETE CT_DO CT_END CT_ENDREACH CT_FALSE CT_FORWARD CT_FROM CT_GOTO CT_IF CT_INIT CT_INITIALLY CT_IN CT_LOC CT_LOCATIONS CT_NOT CT_OR CT_PARAMETER CT_PRINT CT_REACH CT_REGION CT_SYNC CT_SYNCLABS CT_TRUE CT_VAR CT_WAIT CT_WHEN CT_WHILE

%token EOF

/*%left OP_L OP_LEQ OP_EQ OP_GEQ OP_G*/
%left PIPE CT_OR        /* lowest precedence */
%left AMPERSAND CT_AND  /* medium precedence */
%nonassoc CT_NOT        /* highest precedence * /


/* %left OP_PLUS OP_MINUS        /* lowest precedence */
/*
%left OP_DIV         /* medium precedence * /
%nonassoc UMINUS        /* highest precedence * /
*/

%start main             /* the entry point */
%type <ParsingStructure.parsing_structure> main
%%

/**********************************************/
main:
	 declarations automata commands EOF
	{
		(* get declarations in header *)
		let decls = $1 in
		(* tag as global *)
		let global_decls = List.map (fun (v,t) -> (v,t,Global)) decls in
		(* store in declaration list *)
		declarations := List.append !declarations global_decls;
		(* parse automata *)
		let automata = $2 in 
		(* parse commands *)
		let commands = $3 in
		(* return global and local variables, automata and commands *)
		!declarations, automata, commands
	}
;

/***********************************************
  MAIN DEFINITIONS
***********************************************/

/**********************************************/

declarations:
	CT_VAR var_lists { $2	}
	| { [] }
;


/**********************************************/

/**********************************************/

var_lists:
	var_list COLON var_type SEMICOLON var_lists {
		let t = $3 in
		let vars = $1 in
		let decls = List.map (fun v -> (t, v)) vars in
		List.append decls $5
	}
	| { [] }
;

/**********************************************/

var_list:
	  NAME { [$1] }
	| NAME COMMA var_list { $1 :: $3 }
;

/**********************************************/

var_type:
  | CT_ANALOG { Var_type_analog }
	| CT_CLOCK { Var_type_clock }
	| CT_DISCRETE { Var_type_discrete }
	| CT_PARAMETER { Var_type_parameter }
;

/**********************************************/

/**********************************************/

automata:
	automaton automata { $1 :: $2 }
	| { [] }
;

/**********************************************/

automaton:
	CT_AUTOMATON NAME prolog locations CT_END
	{
		let aut_name = $2 in
		let decls, labels = $3 in
		(* tag declarations as local with automaton name *)
		let ext_decls = List.map (fun (t, v) -> (t, v, Local aut_name)) decls in
		(* store declarations in global list *)
		declarations := List.append !declarations ext_decls; 		
		(aut_name, labels, $4)
	}
;

/**********************************************/

prolog:
	| declarations initialization sync_labels { $1, $3 }
	| declarations sync_labels initialization { $1, $2 }
	| declarations sync_labels { $1, $2 } /* L'initialisation n'est pas prise en compte, et est donc facultative */
;

/**********************************************/

initialization:
	| CT_INITIALLY NAME state_initialization SEMICOLON {}
;

/**********************************************/

state_initialization:
	AMPERSAND convex_predicate {}
	| {}
;

/**********************************************/

sync_labels:
	CT_SYNCLABS COLON sync_var_list SEMICOLON { $3 }
;

/**********************************************/

sync_var_list:
	sync_var_nonempty_list { $1 }
	| { [] }
;

/**********************************************/

sync_var_nonempty_list:
	NAME COMMA sync_var_nonempty_list { $1 :: $3}
	| NAME { [$1] }
;

/**********************************************/

locations:
	location locations { $1 :: $2}
	| { [] }
;

/**********************************************/

location:
  CT_LOC NAME COLON CT_WHILE convex_predicate CT_WAIT LBRACE rate_info RBRACE transitions { $2, $5, $8, $10 }
;

/**********************************************/
rate_info:
	ext_convex_predicate { $1 }
	| { [] }
;
 
/**********************************************/

transitions:
	transition transitions { $1 :: $2 }
	| { [] }
;

/**********************************************/

transition:
	CT_WHEN convex_predicate update_synchronization CT_GOTO NAME SEMICOLON
	{
		let update_list, sync = $3 in
			$2, update_list, sync, $5
	}
;

/**********************************************/

/* A l'origine de 3 conflits ("2 shift/reduce conflicts, 1 reduce/reduce conflict.") donc petit changement */
update_synchronization:
	| { [], NoSync }
	| updates { $1, NoSync }
	| syn_label { [], (Sync $1) }
	| updates syn_label { $1, (Sync $2) }
	| syn_label updates { $2, (Sync $1) }
;

/**********************************************/

updates:
	CT_DO LBRACE update_list RBRACE { $3 }
;

/**********************************************/

update_list:
	upd_convex_predicate { $1 }
	| { [] }
;

/**********************************************/

syn_label:
	CT_SYNC NAME { $2 }
;


/***********************************************
  RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES
***********************************************/

convex_predicate:
	linear_constraint AMPERSAND convex_predicate { $1 :: $3 }
	| linear_constraint { [$1] }
;

ext_convex_predicate:
	ext_linear_constraint AMPERSAND ext_convex_predicate { $1 :: $3 }
	| ext_linear_constraint { [$1] }
;

upd_convex_predicate:
	ext_linear_constraint COMMA upd_convex_predicate { $1 :: $3 }
	| ext_linear_constraint { [$1] }
;


linear_constraint:
	linear_expression relop linear_expression { Linear_constraint ($1, $2, $3) }
	| CT_TRUE { True_constraint }
	| CT_FALSE { False_constraint }
;

ext_linear_constraint:
	ext_linear_expression relop ext_linear_expression { Linear_constraint ($1, $2, $3) }
;

relop:
	  OP_L { OP_L }
	| OP_LEQ { OP_LEQ }
	| OP_EQ { OP_EQ }
	| OP_GEQ { OP_GEQ }
	| OP_G { OP_G }
;

linear_expression:
	linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) } /* linear_term a la deuxieme place */
;

ext_linear_expression:
	ext_linear_term { Linear_term $1 }
	| ext_linear_expression OP_PLUS ext_linear_term { Linear_plus_expression ($1, $3) }
	| ext_linear_expression OP_MINUS ext_linear_term { Linear_minus_expression ($1, $3) } /* linear_term a la deuxieme place */
;

linear_term:
	rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| NAME { Variable (NumConst.one, $1) }
	| LPAREN linear_term RPAREN { $2 }
;

ext_linear_term:
	rational { Constant $1 }
	| rational NAME APOSTROPHE { PrimedVariable ($1, $2) }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME APOSTROPHE { PrimedVariable ($1, $3) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| NAME APOSTROPHE { PrimedVariable (NumConst.one, $1) }	
	| NAME { Variable (NumConst.one, $1) }
	| LPAREN linear_term RPAREN { $2 }

rational:
	integer { $1 }
	| integer OP_DIV pos_integer { (NumConst.div $1 $3) }
;

integer:
	pos_integer { $1 }
	| OP_MINUS pos_integer { NumConst.neg $2 }
;

pos_integer:
	INT { NumConst.numconst_of_int $1 }
;


/***********************************************
  ANALYSIS COMMANDS
***********************************************/

commands:
	| init_declaration init_definition reach_command { $2 }
;


init_declaration:
	| CT_VAR CT_INIT COLON CT_REGION SEMICOLON { }
	| { }
;

reach_command:
	/* print (reach forward from init endreach); */
	| CT_PRINT LPAREN CT_REACH CT_FORWARD CT_FROM CT_INIT CT_ENDREACH RPAREN SEMICOLON { }
	| { }
;


init_definition:
	CT_INIT OP_ASSIGN region_expression SEMICOLON { $3 }
;

region_expression:
	| state_predicate { [$1] }
	| LPAREN region_expression RPAREN { $2 }
	| region_expression AMPERSAND region_expression { $1 @ $3 }
;

state_predicate:
	| CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME { Loc_assignment ($3, $6) }
	| linear_constraint { Linear_predicate $1 }
;

