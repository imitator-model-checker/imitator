/***********************************************
 *
 *                     HYMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created       : 2009/09/07
 * Last modified : 2010/05/07
***********************************************/

%{
open ParsingStructure;;
open Global;;
open NumConst;;
		  
(* global reference to collect all local variables in all automata *)
(* as triples (type, varname, scope).                              *)
let declarations = ref [];;	

(* global reference to constants defined in the header *)
let constants = ref [];;
  
let parse_error s =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	print_error s;
	raise (ParsingError (symbol_start, symbol_end))
;;

let check_constant name =
	List.iter (fun (n,_) -> 
		if n = name then ( 
			parse_error ("constant '" ^ n ^ "' already defined")	
		)
	) !constants
;;

let get_constant name =
	let rec get_const name clist =
		match clist with
			| [] -> None
			| (n, c) :: rest -> if name = n then Some c else get_const name rest in
	get_const name !constants
;;
 
%}

%token <NumConst.t> INT
%token <string> FLOAT
%token <string> NAME
%token <string> STRING

%token OP_PLUS OP_MINUS OP_MUL OP_DIV OP_POW
%token OP_L OP_LEQ OP_EQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token AMPERSAND APOSTROPHE COLON COMMA PIPE SEMICOLON

/* CT_ALL CT_ANALOG CT_ASAP CT_BACKWARD CT_CLDIFF CT_D  CT_ELSE CT_EMPTY  CT_ENDHIDE CT_ENDIF CT_ENDREACH CT_ENDWHILE CT_FORWARD CT_FREE CT_FROM  CT_HIDE CT_HULL CT_INTEGRATOR CT_ITERATE CT_NON_PARAMETERS CT_OMIT CT_POST CT_PRE CT_PRINT CT_PRINTS CT_PRINTSIZE CT_REACH  CT_STOPWATCH CT_THEN CT_TO CT_TRACE CT_USING  CT_WEAKDIFF CT_WEAKEQ CT_WEAKGE CT_WEAKLE  */

%token CT_AND CT_AUTOMATON CT_ANALOG CT_BAD CT_CLOCK CT_CONST CT_DISCRETE CT_DO CT_DOMAIN CT_END CT_ENDREACH CT_FALSE CT_FORWARD CT_FROM CT_GOTO CT_IF CT_INIT CT_INITIALLY CT_IN CT_LOC CT_LOCATIONS CT_NOT CT_OR CT_PARAMETER CT_PREDICATES CT_PRINT CT_REACH CT_REGION CT_SYNC CT_SYNCLABS CT_TRUE CT_VAR CT_WAIT CT_WHEN CT_WHILE

%token EOF

/*%left OP_L OP_LEQ OP_EQ OP_GEQ OP_G*/
%right SEMICOLON

%left PIPE CT_OR        /* lowest precedence */
%left AMPERSAND CT_AND  /* medium precedence */
%nonassoc CT_NOT        /* highest precedence */

%nonassoc OP_L OP_LEQ OP_EQ OP_GEQ OP_G OP_ASSIGN

%left OP_PLUS OP_MINUS /* lowest precedence */
%left OP_DIV  OP_MUL   /* medium precedence */
%left OP_POW           /* high precedence   */
%nonassoc UMINUS       /* highest precedence */


%start main             /* the entry point */
%type <ParsingStructure.parsing_structure> main
%%

/**********************************************/
main:
	 const_declarations declarations automata commands EOF
	{
		(* store constants in header *)
		$1;
		List.iter (fun (n, c)  -> print_message Debug_low ("const " ^ n ^ " = " ^ (NumConst.string_of_numconst c))) !constants;
		(* get declarations in header *)
		let decls = $2 in
		(* tag as global *)
		let global_decls = List.map (fun (v,t) -> (v,t,Global)) decls in
		(* store in declaration list *)
		declarations := List.append !declarations global_decls;
		(* parse automata *)
		let automata = $3 in 
		(* parse commands *)
		let init,bad,preds,domain = $4 in
		(* return global and local variables, automata and commands *)
		!declarations, automata, init, bad, preds, domain
	}
;

/***********************************************
  MAIN DEFINITIONS
***********************************************/

/**********************************************/

const_declarations:
	| const_declarations const_decl {
		 let (n,c) = $2 in
			check_constant n;
			constants := (n,c) :: !constants;
			$1 } 
	| { () }
;

const_decl:
	CT_CONST NAME OP_EQ const_expr SEMICOLON { ($2, $4) }
;

const_expr:
	| integer { $1 }
	| float { $1 }
	| const_id { $1 }
	| const_expr OP_PLUS const_expr { NumConst.add $1 $3 }
	| const_expr OP_MINUS const_expr { NumConst.sub $1 $3 }
	| const_expr OP_MUL const_expr { NumConst.mul $1 $3 }
	| const_expr OP_DIV const_expr { NumConst.div $1 $3 }
	| const_expr OP_POW const_expr {
			try (
				NumConst.pow $1 $3
			) with Arithmetic_exception e -> parse_error e 
		}
	| LPAREN const_expr RPAREN { $2 }
;

const_id:
	NAME { 
		let name = $1 in
		match get_constant name with
		| None -> parse_error ("Unknown constant '" ^ name ^ "'")
		| Some c -> c
	} 
;

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
	linear_constraint AMPERSAND convex_predicate { List.append $1 $3 }
	| linear_constraint { $1 }
;

ext_convex_predicate:
	ext_linear_constraint AMPERSAND ext_convex_predicate { List.append $1 $3 }
	| ext_linear_constraint { $1 }
;

upd_convex_predicate:
	ext_linear_constraint COMMA upd_convex_predicate { List.append $1 $3 }
	| ext_linear_constraint { $1 }
;


linear_constraint:
	linear_expression relop linear_expression { [ Linear_constraint ($1, $2, $3) ] }
	| interval_expression { $1 }
	| CT_TRUE  { [ True_constraint  ] }
	| CT_FALSE { [ False_constraint ]}
;

ext_linear_constraint:
	ext_linear_expression relop ext_linear_expression { [ Linear_constraint ($1, $2, $3) ] }
	| ext_interval_expression { $1 }
;

interval_expression:
	linear_expression CT_IN LSQBRA const_expr COMMA const_expr RSQBRA {
		[
			Linear_constraint ($1, OP_GEQ, Linear_term(Constant $4));
			Linear_constraint ($1, OP_LEQ, Linear_term(Constant $6))
		]			
	}
;

ext_interval_expression:
	ext_linear_expression CT_IN LSQBRA const_expr COMMA const_expr RSQBRA {
		[
			Linear_constraint ($1, OP_GEQ, Linear_term(Constant $4));
			Linear_constraint ($1, OP_LEQ, Linear_term(Constant $6))
		]	
	}
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
	| atom { $1 }
	| linear_term OP_MUL linear_term { 
		let a1 = $1 in
		let a2 = $3 in
		match (a1, a2) with
			| (Constant x, Constant y) -> Constant (NumConst.mul x y)
			| (Variable (x, v), Constant y) -> Variable ((NumConst.mul x y), v)
			| (Constant x, Variable (y, v)) -> Variable ((NumConst.mul x y), v)
			| _ -> parse_error "expression is not linear"
	}
	| linear_term OP_DIV linear_term {
		let a1 = $1 in
		let a2 = $3 in
		match (a1, a2) with
			| (Constant x, Constant y) -> Constant (NumConst.div x y)
			| (Variable (x, v), Constant y) -> Variable ((NumConst.div x y), v)
			| _ -> parse_error "expression is not linear"		
	}
	| linear_term OP_POW linear_term {
		let a1 = $1 in
		let a2 = $3 in
		match (a1, a2) with
			| (Constant x, Constant y) -> (try (
					Constant (NumConst.pow x y)
				) with Arithmetic_exception e -> parse_error e) 
			| _ -> parse_error "expression is not linear"
	}
	| LPAREN linear_term RPAREN { $2 }
;

atom:
	| integer { Constant $1 }
	| float { Constant $1 }
	| NAME {
			let id = $1 in
			let c = get_constant id in
			match c with
				| Some x -> Constant x
				| None -> Variable (NumConst.one, $1) 
		}
;

ext_linear_term:
	| ext_atom { $1 }
	| ext_linear_term OP_MUL ext_linear_term { 
		let a1 = $1 in
		let a2 = $3 in
		match (a1, a2) with
			| (Constant x, Constant y) -> Constant (NumConst.mul x y)
			| (Variable (x, v), Constant y) -> Variable ((NumConst.mul x y), v)
			| (Constant x, Variable (y, v)) -> Variable ((NumConst.mul x y), v)
 			| (PrimedVariable (x, v), Constant y) -> PrimedVariable ((NumConst.mul x y), v)
			| (Constant x, PrimedVariable (y, v)) -> PrimedVariable ((NumConst.mul x y), v)
			| _ -> parse_error "expression is not linear"
	}
	| ext_linear_term OP_DIV ext_linear_term {
		let a1 = $1 in
		let a2 = $3 in
		match (a1, a2) with
			| (Constant x, Constant y) -> Constant (NumConst.div x y)
			| (Variable (x, v), Constant y) -> Variable ((NumConst.div x y), v)
			| (PrimedVariable (x, v), Constant y) -> PrimedVariable ((NumConst.div x y), v)
			| _ -> parse_error "expression is not linear"		
	}
	| ext_linear_term OP_POW ext_linear_term {
		let a1 = $1 in
		let a2 = $3 in
		match (a1, a2) with
			| (Constant x, Constant y) -> (try (
					Constant (NumConst.pow x y)
				) with Arithmetic_exception e -> parse_error e) 
			| _ -> parse_error "expression is not linear"
	}
	| LPAREN ext_linear_term RPAREN { $2 }
;

ext_atom:
	| integer { Constant $1 }
	| float { Constant $1 }
	| NAME {
			let id = $1 in
			let c = get_constant id in
			match c with
				| Some x -> Constant x
				| None -> Variable (NumConst.one, $1) 
		}
	| NAME APOSTROPHE { PrimedVariable (NumConst.one, $1) }
;

integer:
	pos_integer { $1 }
	| OP_MINUS pos_integer %prec UMINUS { NumConst.neg $2 }
;

pos_integer:
	INT { $1 }
;

float:
  pos_float { $1 }
	| OP_MINUS pos_float %prec UMINUS { NumConst.neg $2 }  
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
			| _ ->  parse_error ("illegal floating point number") in
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

/***********************************************
  ANALYSIS COMMANDS
***********************************************/

commands:
	| init_declaration init_definition bad_declaration bad_definition reach_command predicate_abstraction_info 
		{	let preds, domain = $6 in	($2, $4, preds, domain) }
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

bad_declaration:
	| CT_VAR CT_BAD COLON CT_REGION SEMICOLON { }
	| { }
;

bad_definition:
	| CT_BAD OP_ASSIGN loc_expression SEMICOLON { $3 }
	| { [] } 
;

loc_expression:
	| loc_predicate { $1 }
	| loc_expression AMPERSAND loc_expression { $1 @ $3 }
;

region_expression:
	| state_predicate { $1 }
	| LPAREN region_expression RPAREN { $2 }
	| region_expression AMPERSAND region_expression { $1 @ $3 }
;

loc_predicate:
	CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME { [ Loc_assignment ($3, $6) ] }
;

state_predicate:
	| loc_predicate { $1 } 
	| linear_constraint { List.map (fun x -> Linear_predicate x) $1 }
;

predicate_abstraction_info:
	| predicates domain { $1, $2 }
	| domain predicates { $2, $1 }

predicates:
	| CT_PREDICATES COLON predicate_list SEMICOLON { $3 }
	| { [] }
;

predicate_list:
	linear_constraint COMMA predicate_list { $1 @ $3 }
	| linear_constraint { $1 }

domain:
	CT_DOMAIN COLON convex_predicate SEMICOLON { $3 }
	| { [] }
;
