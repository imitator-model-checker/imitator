/************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Parser for the input model
 *
 * File contributors : Étienne André
 * Created           : 2019/10/08
 *
 ************************************************************/


%{

open Exceptions
open ImitatorUtilities
open ParsingStructure


let parse_error _ =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	raise (ParsingError (symbol_start, symbol_end))
;;



(*
let f (decl_l, aut_l, init_l, prop) (decl,aut,init,prop,_,_,_) = (decl@decl_l,aut@aut_l, init@init_l, prop::prop);;
let unzip l = List.fold_left f ([],[],[], []) (List.rev l);;
let filter_opt = List.filter (function | None -> false | Some _ -> true);;

let resolve_property l =
	match filter_opt l with
	| [] -> None
	| [p] -> p
	| _ -> raise Parsing.Parse_error;
;;*)

%}

%token <int> INT
%token <string> FLOAT
%token <string> NAME
%token <string> BINARYWORD
%token <string> STRING

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_NEQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token COLON COMMA DOUBLEDOT OP_CONJUNCTION OP_DISJUNCTION OP_IMPLIES SEMICOLON

%token
	CT_A CT_ACCEPTING CT_ACCEPTINGCYCLE CT_AF CT_AF_timed CT_AG CT_AGnot CT_ALWAYS
	CT_BCBORDER CT_BCLEARN CT_BCRANDOM CT_BCRANDOMSEQ CT_BCSHUFFLE CT_BEFORE
	CT_COVERCARTOGRAPHY
	CT_DEADLOCKFREE
	CT_E CT_EF CT_EF_timed CT_EFpmax CT_EFpmin CT_EFtmin CT_EG CT_EVENTUALLY CT_EVERYTIME CT_EXEMPLIFY CT_EXHIBIT
	CT_FALSE
	CT_HAPPENED CT_HAS
	CT_IF CT_IMCONVEX CT_IMK CT_IMUNION CT_IN CT_INFCYCLE CT_INFCYCLETHROUGH CT_INFINITY CT_IS
	CT_LIST CT_LOC
	CT_NEXT CT_NOT CT_NZCYCLE
	CT_ONCE
	CT_PATTERN CT_PROJECTRESULT CT_PROPERTY CT_PRP CT_PRPC
	CT_QUEUE
	CT_R CT_R_timed
	CT_SEQUENCE CT_STACK CT_STEP CT_SYNTH
	CT_THEN CT_TRACEPRESERVATION CT_TRUE
	CT_U CT_U_timed
	CT_VALID
	CT_W CT_W_timed CT_WIN CT_WITHIN

	/*** NOTE: just to forbid their use in the input model and property ***/
	CT_NOSYNCOBS CT_OBSERVER CT_OBSERVER_CLOCK CT_SPECIAL_RESET_CLOCK_NAME

%token EOF

%left OP_DISJUNCTION OP_IMPLIES /* lowest precedence */
%left OP_CONJUNCTION            /* medium precedence */
%left DOUBLEDOT                 /* high precedence */
%nonassoc CT_NOT                /* highest precedence */

%left OP_PLUS OP_MINUS          /* lowest precedence */
%left OP_MUL OP_DIV             /* highest precedence */


%start main             /* the entry point */
%type <ParsingStructure.unexpanded_parsed_property> main
%%

/************************************************************/
main:
/************************************************************/
	| property_kw_opt quantified_property EOF { $2 }
;

/************************************************************/
property_kw_opt:
/************************************************************/
	| CT_PROPERTY OP_ASSIGN {}
	| {}
;

/************************************************************/
quantified_property:
/************************************************************/
	synth_or_exhibit property semicolon_opt projection_definition {
		{
			unexpanded_synthesis_type	= $1;
			unexpanded_property		= $2;
			(* Projection *)
			unexpanded_projection		= $4;
		}

	}
;

synth_or_exhibit:
	| CT_EXEMPLIFY	{ Parsed_exemplify }
	| CT_EXHIBIT	{ Parsed_witness }
	| CT_SYNTH		{ Parsed_synthesis }
;

/************************************************************/
property:
/************************************************************/
	/*------------------------------------------------------------*/
	/* Basic properties */
	/*------------------------------------------------------------*/
	/* Reachability */
	| CT_VALID { Unexpanded_Parsed_Valid }

	/*------------------------------------------------------------*/
	/* Non-nested CTL */
	/*------------------------------------------------------------*/

	/* Reachability */
	| CT_EF state_predicate { Unexpanded_Parsed_EF $2 }

	/* Safety */
	| CT_AGnot state_predicate { Unexpanded_Parsed_AGnot $2 }

	/* Global invariant */
	| CT_AG state_predicate { Unexpanded_Parsed_AG $2 }

	/* Exists globally */
	| CT_EG state_predicate { Unexpanded_Parsed_EG $2 }

	/* Exists release */
	| CT_E state_predicate CT_R state_predicate { Unexpanded_Parsed_ER ($2, $4) }

	/* Exists until */
	| CT_E state_predicate CT_U state_predicate { Unexpanded_Parsed_EU ($2, $4) }

	/* Exists weak until */
	| CT_E state_predicate CT_W state_predicate { Unexpanded_Parsed_EW ($2, $4) }

	/* Unavoidability */
	| CT_AF state_predicate { Unexpanded_Parsed_AF $2 }

	/* Always release */
	| CT_A state_predicate CT_R state_predicate { Unexpanded_Parsed_AR ($2, $4) }

	/* Always until */
	| CT_A state_predicate CT_U state_predicate { Unexpanded_Parsed_AU ($2, $4) }

	/* Always weak until */
	| CT_A state_predicate CT_W state_predicate { Unexpanded_Parsed_AW ($2, $4) }


	/*------------------------------------------------------------*/
	/* Non-nested CTL (timed version) */
	/*------------------------------------------------------------*/
	/* Reachability (timed version) */
	| CT_EF_timed timed_interval state_predicate {
		(* Optimization: `EF_timed [0, infinity) sp` is actually `EF sp` *)
		match $2 with
		| Parsed_closed_infinity_interval parsed_interval when parsed_interval = Linear_term (Constant NumConst.zero) ->
			Unexpanded_Parsed_EF $3
		| _ -> Unexpanded_Parsed_EF_timed ($2, $3)
	}

	/* ER (timed version) */
	| CT_E state_predicate CT_R_timed timed_interval state_predicate {
		(* Optimization: `ER_timed [0, infinity) sp sp` is actually `ER sp sp` *)
		match $4 with
		| Parsed_closed_infinity_interval parsed_interval when parsed_interval = Linear_term (Constant NumConst.zero) ->
			Unexpanded_Parsed_ER ($2, $5)
		| _ -> Unexpanded_Parsed_ER_timed ($4, $2, $5)
	}

	/* EU (timed version) */
	| CT_E state_predicate CT_U_timed timed_interval state_predicate {
		(* Optimization: `EU_timed [0, infinity) sp sp` is actually `EU sp sp` *)
		match $4 with
		| Parsed_closed_infinity_interval parsed_interval when parsed_interval = Linear_term (Constant NumConst.zero) ->
			Unexpanded_Parsed_EU ($2, $5)
		| _ -> Unexpanded_Parsed_EU_timed ($4, $2, $5)
	}

	/* EW (timed version) */
	| CT_E state_predicate CT_W_timed timed_interval state_predicate {
		(* Optimization: `EW_timed [0, infinity) sp sp` is actually `EW sp sp` *)
		match $4 with
		| Parsed_closed_infinity_interval parsed_interval when parsed_interval = Linear_term (Constant NumConst.zero) ->
			Unexpanded_Parsed_EW ($2, $5)
		| _ -> Unexpanded_Parsed_EW_timed ($4, $2, $5)
	}

	/* AF (timed version) */
	| CT_AF_timed timed_interval state_predicate {
		(* Optimization: `AF_timed [0, infinity) sp` is actually `AF sp` *)
		match $2 with
		| Parsed_closed_infinity_interval parsed_interval when parsed_interval = Linear_term (Constant NumConst.zero) ->
			Unexpanded_Parsed_AF $3
		| _ -> Unexpanded_Parsed_AF_timed ($2, $3)
	}

	/* AR (timed version) */
	| CT_A state_predicate CT_R_timed timed_interval state_predicate {
		(* Optimization: `AR_timed [0, infinity) sp sp` is actually `AR sp sp` *)
		match $4 with
		| Parsed_closed_infinity_interval parsed_interval when parsed_interval = Linear_term (Constant NumConst.zero) ->
			Unexpanded_Parsed_AR ($2, $5)
		| _ -> Unexpanded_Parsed_AR_timed ($4, $2, $5)
	}

	/* AU (timed version) */
	| CT_A state_predicate CT_U_timed timed_interval state_predicate {
		(* Optimization: `AU_timed [0, infinity) sp sp` is actually `AU sp sp` *)
		match $4 with
		| Parsed_closed_infinity_interval parsed_interval when parsed_interval = Linear_term (Constant NumConst.zero) ->
			Unexpanded_Parsed_AU ($2, $5)
		| _ -> Unexpanded_Parsed_AU_timed ($4, $2, $5)
	}

	/* AW (timed version) */
	| CT_A state_predicate CT_W_timed timed_interval state_predicate {
		(* Optimization: `AW_timed [0, infinity) sp sp` is actually `AW sp sp` *)
		match $4 with
		| Parsed_closed_infinity_interval parsed_interval when parsed_interval = Linear_term (Constant NumConst.zero) ->
			Unexpanded_Parsed_AW ($2, $5)
		| _ -> Unexpanded_Parsed_AW_timed ($4, $2, $5)
	}



	/*------------------------------------------------------------*/
	/* Optimized reachability */
	/*------------------------------------------------------------*/

	/* Reachability with minimization of a parameter valuation */
	| CT_EFpmin state_predicate COMMA NAME { Unexpanded_Parsed_EFpmin ($2, $4) }
	| CT_EFpmin LPAREN state_predicate COMMA NAME RPAREN { Unexpanded_Parsed_EFpmin ($3, $5) }

	/* Reachability with maximization of a parameter valuation */
	| CT_EFpmax state_predicate COMMA NAME { Unexpanded_Parsed_EFpmax ($2, $4) }
	| CT_EFpmax LPAREN state_predicate COMMA NAME RPAREN { Unexpanded_Parsed_EFpmax ($3, $5) }

	/* Reachability with minimal-time */
	| CT_EFtmin state_predicate { Unexpanded_Parsed_EFtmin ($2) }


	/*------------------------------------------------------------*/
	/* Cycles */
	/*------------------------------------------------------------*/

	/* Infinite-run (cycle) */
	| CT_INFCYCLE { Unexpanded_Parsed_Cycle_Through (Unexpanded_Parsed_state_predicate_term (Unexpanded_Parsed_state_predicate_factor(Unexpanded_Parsed_simple_predicate Unexpanded_Parsed_state_predicate_true))) }

	/* Accepting infinite-run (cycle) through a state predicate */
	| CT_INFCYCLETHROUGH LPAREN state_predicate_list RPAREN {
		(* Check whether the list is of size <= 1 *)
		match $3 with
		| []				-> Unexpanded_Parsed_Cycle_Through (Unexpanded_Parsed_state_predicate_term (Unexpanded_Parsed_state_predicate_factor(Unexpanded_Parsed_simple_predicate Unexpanded_Parsed_state_predicate_false))) (* NOTE: equivalent to False; this case probably cannot happen anyway *)
		| [state_predicate]	-> Unexpanded_Parsed_Cycle_Through state_predicate
		| _					-> Unexpanded_Parsed_Cycle_Through_generalized $3
		}

	/* Accepting infinite-run (cycle) through accepting locations */
	| CT_ACCEPTINGCYCLE { Unexpanded_Parsed_Cycle_Through (Unexpanded_Parsed_state_predicate_term (Unexpanded_Parsed_state_predicate_factor(Unexpanded_Parsed_simple_predicate Unexpanded_Parsed_state_predicate_accepting))) }

	/* Infinite-run (cycle) with non-Zeno assumption */
	| CT_NZCYCLE { Unexpanded_Parsed_NZ_Cycle }


	/*------------------------------------------------------------*/
	/* Deadlock-freeness */
	/*------------------------------------------------------------*/

	/* Deadlock-free synthesis */
	| CT_DEADLOCKFREE { Unexpanded_Parsed_Deadlock_Freeness }


	/*------------------------------------------------------------*/
	/* Inverse method, trace preservation, robustness */
	/*------------------------------------------------------------*/

	| CT_TRACEPRESERVATION LPAREN reference_valuation RPAREN { Unexpanded_Parsed_IM $3 }

	| CT_IMCONVEX LPAREN reference_valuation RPAREN { Unexpanded_Parsed_ConvexIM $3 }

	| CT_PRP LPAREN state_predicate COMMA reference_valuation RPAREN { Unexpanded_Parsed_PRP ($3 , $5) }

	| CT_IMK LPAREN reference_valuation RPAREN { Unexpanded_Parsed_IMK $3 }

	| CT_IMUNION LPAREN reference_valuation RPAREN { Unexpanded_Parsed_IMunion $3 }


	/*------------------------------------------------------------*/
	/* Cartography algorithms */
	/*------------------------------------------------------------*/

	/* Cartography */
	| CT_COVERCARTOGRAPHY LPAREN reference_rectangle RPAREN { Unexpanded_Parsed_Cover_cartography ($3 , Constants.default_cartography_step) }
	| CT_COVERCARTOGRAPHY LPAREN reference_rectangle COMMA CT_STEP OP_EQ rational RPAREN { Unexpanded_Parsed_Cover_cartography ($3 , $7) }

	| CT_BCLEARN LPAREN state_predicate COMMA reference_rectangle RPAREN { Unexpanded_Parsed_Learning_cartography ($3, $5, Constants.default_cartography_step) }
	| CT_BCLEARN LPAREN state_predicate COMMA reference_rectangle COMMA CT_STEP OP_EQ rational RPAREN { Unexpanded_Parsed_Learning_cartography ($3, $5, $9) }

	| CT_BCSHUFFLE LPAREN reference_rectangle RPAREN { Unexpanded_Parsed_Shuffle_cartography ($3, Constants.default_cartography_step) }
	| CT_BCSHUFFLE LPAREN reference_rectangle COMMA CT_STEP OP_EQ rational RPAREN { Unexpanded_Parsed_Shuffle_cartography ($3, $7) }

	| CT_BCBORDER LPAREN reference_rectangle RPAREN { Unexpanded_Parsed_Border_cartography ($3, Constants.default_cartography_step) }
	| CT_BCBORDER LPAREN reference_rectangle COMMA CT_STEP OP_EQ rational RPAREN { Unexpanded_Parsed_Border_cartography ($3, $7) }

	| CT_BCRANDOM LPAREN reference_rectangle COMMA pos_integer RPAREN { Unexpanded_Parsed_Random_cartography ($3, $5, Constants.default_cartography_step) }
	| CT_BCRANDOM LPAREN reference_rectangle COMMA pos_integer COMMA CT_STEP OP_EQ rational RPAREN { Unexpanded_Parsed_Random_cartography ($3, $5, $9) }

	| CT_BCRANDOMSEQ LPAREN reference_rectangle COMMA pos_integer RPAREN { Unexpanded_Parsed_RandomSeq_cartography ($3, $5, Constants.default_cartography_step) }
	| CT_BCRANDOMSEQ LPAREN reference_rectangle COMMA pos_integer COMMA CT_STEP OP_EQ rational RPAREN { Unexpanded_Parsed_RandomSeq_cartography ($3, $5, $9) }

	| CT_PRPC LPAREN state_predicate COMMA reference_rectangle RPAREN { Unexpanded_Parsed_PRPC ($3,$5, Constants.default_cartography_step) }
	| CT_PRPC LPAREN state_predicate COMMA reference_rectangle COMMA CT_STEP OP_EQ rational RPAREN { Unexpanded_Parsed_PRPC ($3,$5, $9) }


	/*------------------------------------------------------------*/
	/* Observer patterns */
	/*------------------------------------------------------------*/
	| CT_PATTERN LPAREN pattern RPAREN { Unexpanded_Parsed_pattern ($3) }

	/*------------------------------------------------------------*/
	/* Observer patterns */
	/*------------------------------------------------------------*/
	| CT_WIN state_predicate { Unexpanded_Parsed_Win ($2) }

;


/************************************************************/
pattern:
/************************************************************/
	/* if a2 then a1 has happened before */
	| CT_IF NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_BEFORE { Parsed_action_precedence_acyclic ($4, $2) }
	/* everytime a2 then a1 has happened before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_BEFORE { Parsed_action_precedence_cyclic ($4, $2) }
	/* everytime a2 then a1 has happened once before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_ONCE CT_BEFORE { Parsed_action_precedence_cyclicstrict ($4, $2) }

	/* a within d */
	| NAME CT_WITHIN linear_expression { Parsed_action_deadline ($1, $3) }

	/* if a2 then a1 happened within d before */
	| CT_IF NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_WITHIN linear_expression CT_BEFORE { Parsed_TB_Action_precedence_acyclic ($4, $2, $8) }
	/* everytime a2 then a1 happened within d before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_WITHIN linear_expression CT_BEFORE { Parsed_TB_Action_precedence_cyclic ($4, $2, $8) }
	/* everytime a2 then a1 happened once within d before */
	| CT_EVERYTIME NAME CT_THEN NAME CT_HAS CT_HAPPENED CT_ONCE CT_WITHIN linear_expression CT_BEFORE { Parsed_TB_Action_precedence_cyclicstrict ($4, $2, $9) }

	/* if a1 then eventually a2 within d */
	| CT_IF NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression { Parsed_TB_response_acyclic ($2, $5, $7) }
	/* everytime a1 then eventually a2 within d */
	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression { Parsed_TB_response_cyclic ($2, $5, $7) }
	/* everytime a1 then eventually a2 within d once before next */
	| CT_EVERYTIME NAME CT_THEN CT_EVENTUALLY NAME CT_WITHIN linear_expression CT_ONCE CT_BEFORE CT_NEXT { Parsed_TB_response_cyclicstrict ($2, $5, $7) }

	/* sequence a1, …, an */
	| CT_SEQUENCE name_nonempty_list { Parsed_Sequence_acyclic ($2) }
	| CT_SEQUENCE LPAREN name_nonempty_list RPAREN { Parsed_Sequence_acyclic ($3) } /* with parentheses */
	/* always sequence a1, …, an */
	| CT_ALWAYS CT_SEQUENCE name_nonempty_list { Parsed_Sequence_cyclic ($3) }
	| CT_ALWAYS CT_SEQUENCE LPAREN name_nonempty_list RPAREN { Parsed_Sequence_cyclic ($4) } /* with parentheses */

;

/************************************************************/
timed_interval:
/************************************************************/

	| LSQBRA linear_expression COMMA linear_expression RSQBRA {
		if ($2 = Linear_term (Constant NumConst.zero)) then
			Parsed_zero_closed_interval $4
		else
			Parsed_closed_closed_interval ($2, $4)
	}

	| LSQBRA linear_expression COMMA linear_expression RPAREN {
		if ($2 = Linear_term (Constant NumConst.zero)) then
			Parsed_zero_open_interval $4
		else
			Parsed_closed_open_interval ($2, $4)
	}

	| LPAREN linear_expression COMMA linear_expression RSQBRA { Parsed_open_closed_interval ($2, $4) }

	| LPAREN linear_expression COMMA linear_expression RPAREN { Parsed_open_open_interval ($2, $4) }

	| LSQBRA linear_expression COMMA CT_INFINITY RPAREN { Parsed_closed_infinity_interval ($2) }

	| LPAREN linear_expression COMMA CT_INFINITY RPAREN { Parsed_open_infinity_interval ($2) }
;



/************************************************************/
state_predicate_list:
/************************************************************/
	| non_empty_state_predicate_list { $1 }
	/* Also allow empty state predicate, equivalent to False */
	| { [Unexpanded_Parsed_state_predicate_term (Unexpanded_Parsed_state_predicate_factor(Unexpanded_Parsed_simple_predicate Unexpanded_Parsed_state_predicate_false))] }
;

/************************************************************/
non_empty_state_predicate_list:
/************************************************************/
	| non_empty_state_predicate COMMA non_empty_state_predicate_list { $1 :: $3 }
	| non_empty_state_predicate comma_opt { [$1] }
;

/************************************************************/
state_predicate:
/************************************************************/
	| non_empty_state_predicate { $1 }
	/* Also allow empty state predicate, equivalent to False */
	| { Unexpanded_Parsed_state_predicate_term (Unexpanded_Parsed_state_predicate_factor(Unexpanded_Parsed_simple_predicate Unexpanded_Parsed_state_predicate_false)) }
;

/************************************************************/
non_empty_state_predicate:
/************************************************************/
	| non_empty_state_predicate OP_DISJUNCTION state_predicate_term { Unexpanded_Parsed_state_predicate_OR ($1, Unexpanded_Parsed_state_predicate_term $3) }

	/* Translate 'a => b' to 'NOT a OR b' */
	| non_empty_state_predicate OP_IMPLIES state_predicate_term { Unexpanded_Parsed_state_predicate_OR (Unexpanded_Parsed_state_predicate_term (Unexpanded_Parsed_state_predicate_factor (Unexpanded_Parsed_state_predicate_factor_NOT (Unexpanded_Parsed_state_predicate ($1)))), Unexpanded_Parsed_state_predicate_term $3) }

	| state_predicate_term { Unexpanded_Parsed_state_predicate_term $1 }
;

state_predicate_term:
	| state_predicate_term OP_CONJUNCTION state_predicate_factor { Unexpanded_Parsed_state_predicate_term_AND ($1, Unexpanded_Parsed_state_predicate_factor $3) }
	| state_predicate_factor { Unexpanded_Parsed_state_predicate_factor $1 }
;

state_predicate_factor:
	| simple_predicate { Unexpanded_Parsed_simple_predicate $1 }
	| CT_NOT state_predicate_factor { Unexpanded_Parsed_state_predicate_factor_NOT $2 }
	| LPAREN non_empty_state_predicate RPAREN { Unexpanded_Parsed_state_predicate $2 }
;

/* A single definition of one bad location or one bad discrete definition */
simple_predicate:
	| discrete_boolean_predicate { Unexpanded_Parsed_discrete_boolean_expression($1) }
	| loc_predicate { Unexpanded_Parsed_loc_predicate ($1) }
	| CT_ACCEPTING { Unexpanded_Parsed_state_predicate_accepting }
;

/************************************************************/
name_or_array_access:
  | NAME { Var_name $1 }
  | NAME LSQBRA arithmetic_expression RSQBRA { Var_array_access ($1, $3) }
;
/************************************************************/

/************************************************************/
loc_predicate:
/************************************************************/
	/* loc[my_pta] = my_loc */
	| CT_LOC LSQBRA name_or_array_access RSQBRA OP_EQ NAME { Unexpanded_Parsed_loc_predicate_EQ ($3, $6) }
	/* my_pta IS IN my_loc */
	| name_or_array_access CT_IS CT_IN NAME { Unexpanded_Parsed_loc_predicate_EQ ($1, $4) }

	/* loc[my_pta] <> my_loc */
	| CT_LOC LSQBRA name_or_array_access RSQBRA OP_NEQ NAME { Unexpanded_Parsed_loc_predicate_NEQ ($3, $6) }
	/* my_pta IS NOT IN my_loc */
	| name_or_array_access CT_IS CT_NOT CT_IN NAME { Unexpanded_Parsed_loc_predicate_NEQ ($1, $5) }
;


/************************************************************/
/** BOOLEAN EXPRESSIONS */
/************************************************************/

boolean_expression:
	| discrete_boolean_predicate { Parsed_discrete_bool_expr $1 }
	| boolean_expression OP_CONJUNCTION boolean_expression { Parsed_conj_dis ($1, $3, Parsed_and) }
	| boolean_expression OP_DISJUNCTION boolean_expression { Parsed_conj_dis ($1, $3, Parsed_or) }
	/* Translate 'a => b' to 'NOT a OR b' */
	| boolean_expression OP_IMPLIES boolean_expression { Parsed_conj_dis ((Parsed_discrete_bool_expr (Parsed_not $1)), $3, Parsed_or) }
;

/************************************************************/
discrete_boolean_predicate:
/************************************************************/
	/* expr ~ expr */
  | arithmetic_expression { Parsed_arithmetic_expr $1 }
	| arithmetic_expression op_bool arithmetic_expression { Parsed_comparison (Parsed_arithmetic_expr $1, $2, Parsed_arithmetic_expr $3) }
	/* expr in [expr .. expr] */
	| arithmetic_expression CT_IN LSQBRA arithmetic_expression COMMA arithmetic_expression RSQBRA { Parsed_comparison_in ($1, $4, $6) }
	| arithmetic_expression CT_IN LSQBRA arithmetic_expression DOUBLEDOT arithmetic_expression RSQBRA { Parsed_comparison_in ($1, $4, $6) }
;

arithmetic_expression:
	| arithmetic_expression sum_diff discrete_term { Parsed_sum_diff ($1, $3, $2) }
	| discrete_term { Parsed_term $1 }
;

sum_diff:
  | OP_PLUS { Parsed_plus }
  | OP_MINUS { Parsed_minus }
;

discrete_term:
	| discrete_term product_quotient arithmetic_factor { Parsed_product_quotient ($1, $3, $2) }
	| arithmetic_factor { Parsed_factor $1 }
;

product_quotient:
    | OP_MUL { Parsed_mul }
    | OP_DIV { Parsed_div }
;

arithmetic_factor:
  | arithmetic_factor LSQBRA arithmetic_expression RSQBRA { Parsed_access ($1, $3) }
  | NAME LPAREN function_argument_fol RPAREN { Parsed_function_call ($1, $3) }
  | NAME LPAREN RPAREN { Parsed_function_call ($1, []) }
  | literal_scalar_constant { Parsed_constant $1 }
  | literal_non_scalar_constant { $1 }
  | NAME { Parsed_variable ($1, 0) }
  | LPAREN arithmetic_expression RPAREN { Parsed_nested_expr $2 }
;

literal_scalar_constant:
  | number { $1 }
  | CT_TRUE { ParsedValue.Bool_value true }
  | CT_FALSE { ParsedValue.Bool_value false }
  | binary_word { $1 }
;

literal_non_scalar_constant:
  | literal_array { Parsed_sequence ($1, Parsed_array) }
  | CT_LIST LPAREN literal_array RPAREN { Parsed_sequence ($3, Parsed_list) }
  | CT_STACK LPAREN RPAREN { Parsed_sequence ([], Parsed_stack) }
  | CT_QUEUE LPAREN RPAREN { Parsed_sequence ([], Parsed_queue) }
;

literal_array:
  /* Empty array */
  | LSQBRA RSQBRA { [] }
  /* Non-empty array */
  | LSQBRA literal_array_fol RSQBRA { $2 }
;

literal_array_fol:
	| boolean_expression COMMA literal_array_fol { $1 :: $3 }
	| boolean_expression { [$1] }
;

function_argument_fol:
  | boolean_expression COMMA function_argument_fol { $1 :: $3 }
  | boolean_expression { [$1] }
;

number:
	| integer { ParsedValue.Weak_number_value $1 }
	| float { ParsedValue.Rat_value $1 }
;

binary_word:
        BINARYWORD { ParsedValue.Bin_value (BinaryWord.binaryword_of_string $1) }
;





/************************************************************/
op_bool:
/************************************************************/
	| OP_L   { ParsingStructure.PARSED_OP_L }
	| OP_LEQ { ParsingStructure.PARSED_OP_LEQ }
	| OP_EQ  { ParsingStructure.PARSED_OP_EQ }
	| OP_NEQ { ParsingStructure.PARSED_OP_NEQ }
	| OP_GEQ { ParsingStructure.PARSED_OP_GEQ }
	| OP_G   { ParsingStructure.PARSED_OP_G }
;


/************************************************************/
/* rational_linear_term: */
/************************************************************/
/*	| positive_rational_with_div { $1 } */
	/*** NOTE: or should it be OP_MINUS rational_with_div? ***/
/*	| OP_MINUS rational_linear_term { NumConst.neg $2 } */
/*	| LPAREN rational_linear_term RPAREN { $2 }
;
*/

/*
positive_rational_with_div:
	| pos_integer { NumConst.numconst_of_int $1 }
	| pos_float { $1 }
	| pos_integer OP_DIV pos_integer { (NumConst.div (NumConst.numconst_of_int $1) (NumConst.numconst_of_int $3)) }
;

*/
/*** NOTE: unused as of 2024/02/22
positive_rational:
	| pos_integer { NumConst.numconst_of_int $1 }
	| pos_float { $1 }
;
*/

pos_integer:
	| INT { $1 }
;

pos_float:
  | FLOAT {
		NumConst.numconst_of_string $1
	}
;


/************************************************************/
projection_definition:
/************************************************************/
	| CT_PROJECTRESULT LPAREN name_nonempty_list RPAREN semicolon_opt { Some $3 }

	/* Case: no projection */
	|  { None }

;


/************************************************************/
/** LINEAR EXPRESSIONS */
/************************************************************/

/*** BADPROG: duplicate code from ModelParser ***/

/* Linear expression over variables and rationals */
linear_expression:
	| linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) } /* linear_term a la deuxieme place */
;

/* Linear term over variables and rationals (no recursion, no division) */
linear_term:
	| rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| OP_MINUS NAME { Variable (NumConst.minus_one, $2) }
	| NAME { Variable (NumConst.one, $1) }
	| LPAREN linear_term RPAREN { $2 }
;

rational:
	| integer { $1 }
	| float { $1 }
	| integer OP_DIV pos_integer {
		if $3 = 0 then(
			print_error "Division by 0 spotted during the parsing!";
			raise (InvalidModel)
		)else
			NumConst.div $1 (NumConst.numconst_of_int $3)
	}
;

integer:
	| pos_integer { (NumConst.numconst_of_int $1) }
	| OP_MINUS pos_integer { NumConst.neg (NumConst.numconst_of_int $2) }
;


float:
	| pos_float { $1 }
	| OP_MINUS pos_float { NumConst.neg $2 }
;



/************************************************************/
/** NAMES, etc. */
/************************************************************/


name_nonempty_list:
	NAME COMMA name_nonempty_list { $1 :: $3}
	| NAME comma_opt { [$1] }
;

/************************************************************/
/** PARAMETER VALUATION ("pi0") */
/************************************************************/

reference_valuation:
	| parameter_assignments semicolon_opt { $1 }
;

parameter_assignments:
	| parameter_assignment parameter_assignments {$1 :: $2}
	| { [] }
;

/* Form: param = [constant arithmetic expression] */
parameter_assignment:
	and_opt NAME OP_EQ constant_arithmetic_expr comma_opt { ($2, $4) }
;

/************************************************************/
/** HYPER RECTANGLE VALUATION ("v0") */
/************************************************************/

reference_rectangle:
	| rectangle_parameter_assignments semicolon_opt { $1 }
;

rectangle_parameter_assignments:
	| rectangle_parameter_assignment rectangle_parameter_assignments {$1 :: $2}
	| { [] }
;

/* Form: param = [constant arithmetic expression] */
rectangle_parameter_assignment:
	| and_opt NAME OP_EQ constant_arithmetic_expr DOUBLEDOT constant_arithmetic_expr { ($2, $4, $6) }
	| and_opt NAME OP_EQ constant_arithmetic_expr { ($2, $4, $4) }

;


/************************************************************/
/** CONSTANT ARITHMETIC EXPRESSIONS */
/************************************************************/

constant_arithmetic_expr:
	| constant_arithmetic_expr OP_PLUS constant_expr_mult { NumConst.add $1 $3 }
	| constant_arithmetic_expr OP_MINUS constant_expr_mult { NumConst.sub $1 $3 }
	| constant_expr_mult { $1 }
;

constant_expr_mult:
	| constant_expr_mult OP_MUL constant_neg_atom { NumConst.mul $1 $3 }
	| constant_expr_mult OP_DIV constant_neg_atom {
		if NumConst.equal $3 NumConst.zero then(
			print_error "Division by 0 spotted during the parsing!";
			raise (InvalidModel)
		)else
			NumConst.div $1 $3
	}
	| constant_neg_atom { $1 }
;

constant_neg_atom:
	| constant_atom { $1 }
	| OP_MINUS constant_atom { NumConst.neg $2 }
;

constant_atom:
	| LPAREN constant_arithmetic_expr RPAREN { $2 }
	| rational { $1 }
;


/************************************************************/
/** OPTIONAL SYMBOLS */
/************************************************************/

and_opt:
	| OP_CONJUNCTION {}
	| {}
;

comma_opt:
	| COMMA { }
	| { }
;


semicolon_opt:
	| SEMICOLON { }
	| { }
;
