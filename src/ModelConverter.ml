(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Convert a parsing structure into an abstract model
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2009/09/09
 * Last modified     : 2020/02/05
 *
 ************************************************************)



(************************************************************)
(************************************************************)
(** Modules *)
(************************************************************)
(************************************************************)
open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Options
open Automaton
open ParsingStructure
open RationalExpressions
open AbstractModel
open AbstractProperty


(************************************************************)
(************************************************************)
(** Exceptions *)
(************************************************************)
(************************************************************)
(* For constraint conversion *)
exception False_exception

(* For detecting strongly deterministic PTAs *)
exception Not_strongly_deterministic


exception InvalidModel

exception InvalidProperty



(************************************************************)
(************************************************************)
(** Type definition *)
(************************************************************)
(************************************************************)

(* Define a string set structure to gather sets of variables *)
module StringSet = Set.Make(String)


(************************************************************)
(************************************************************)
(** Constants *)
(************************************************************)
(************************************************************)

(* Maintain lists of all (parsed) types *)

let all_concrete_parsed_rational_types = [
	Parsed_boolean ;
	Parsed_rational ;
	Parsed_string ;
	Parsed_stringset ;
	]

let all_parsed_rational_types = List.map (fun parsed_type -> Parsed_var_type_discrete parsed_type) all_concrete_parsed_rational_types

let all_parsed_types = List.rev_append
	[Parsed_var_type_clock ; Parsed_var_type_parameter]
	all_parsed_rational_types

(************************************************************)
(************************************************************)
(** Global variables: saved data structures between model parsing and second file parsing *)
(************************************************************)
(************************************************************)

(*** HACK: at the end of model parsing, we will backup some data structures that will be used for reference valuation (pi0) / reference hyper-rectangle (v0) parsing and checking ***)
(*let saved_index_of_variables	= ref None
let saved_nb_parameters			= ref None
let saved_parameters			= ref None
let saved_parameters_names		= ref None
let saved_variables				= ref None*)


(************************************************************)
(************************************************************)
(** Useful data structure to avoid multiple parameters in functions *)
(************************************************************)
(************************************************************)

type useful_parsing_model_information = {
	(* The locations for each automaton: automaton_index -> location_index -> location_name *)
	actions								: action_name array;
	array_of_location_names				: location_name array array;
	automata							: automaton_index list;
	automata_names						: (automaton_index -> automaton_name);
	constants							: (Automaton.variable_name , NumConst.t) Hashtbl.t;
	discrete							: variable_index list;
	index_of_actions					: (Automaton.action_name , Automaton.action_index) Hashtbl.t;
	index_of_automata					: (Automaton.automaton_name , Automaton.automaton_index) Hashtbl.t;
	index_of_locations					: ((Automaton.location_name, Automaton.location_index) Hashtbl.t) array;
	index_of_variables					: (Automaton.variable_name , Automaton.variable_index) Hashtbl.t;
	nb_parameters						: int;
	parameter_names						: variable_name list;
	removed_action_names				: action_name list;
	type_of_variables					: Automaton.variable_index -> AbstractModel.var_type;
	variable_names						: variable_name list;
	variables							: variable_name array;
	removed_variable_names				: variable_name list;
}



(************************************************************)
(************************************************************)
(** Checking and converting discrete arithmetic expressions *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Getting variables *)
(************************************************************)

(*** TODO: these 'get' functions could be merged with the 'check' functions 'check_automata' ***)



(** Add variables names in the update expression *)
let rec get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref = function
	| Parsed_CAE_plus (parsed_continuous_arithmetic_expression, parsed_continuous_term)
	| Parsed_CAE_minus (parsed_continuous_arithmetic_expression , parsed_continuous_term) ->
		get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref parsed_continuous_arithmetic_expression;
		get_variables_in_parsed_continuous_term variables_used_ref parsed_continuous_term

	| Parsed_CAE_term parsed_continuous_term ->
		get_variables_in_parsed_continuous_term variables_used_ref parsed_continuous_term

and get_variables_in_parsed_continuous_term variables_used_ref = function
	| Parsed_CT_mul (parsed_continuous_term, parsed_continuous_factor)
	| Parsed_CT_div (parsed_continuous_term, parsed_continuous_factor) ->
		get_variables_in_parsed_continuous_term variables_used_ref parsed_continuous_term;
		get_variables_in_parsed_continuous_factor variables_used_ref parsed_continuous_factor

	| Parsed_CT_factor parsed_continuous_factor ->
		get_variables_in_parsed_continuous_factor variables_used_ref parsed_continuous_factor

and get_variables_in_parsed_continuous_factor variables_used_ref = function
	| Parsed_CF_variable variable_name ->
		(* Add the variable name to the set and update the reference *)
		if verbose_mode_greater Verbose_total then(
			print_message Verbose_total ("  New variable found: '" ^ variable_name ^ "'.");
		);
		variables_used_ref := StringSet.add variable_name !variables_used_ref

	| Parsed_CF_constant _ -> ()

	| Parsed_CF_expression parsed_continuous_arithmetic_expression ->
		get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref parsed_continuous_arithmetic_expression

	| Parsed_CF_unary_min parsed_rational_factor -> get_variables_in_parsed_continuous_factor variables_used_ref parsed_rational_factor



let get_variables_in_parsed_continuous_inequality variables_used_ref = function
	| Parsed_expression (arithmetic_expr1, _ (* relop *), arithmetic_expr2) ->
		get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref arithmetic_expr1;
		get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref arithmetic_expr2;
	| Parsed_expression_in (arithmetic_expr1, arithmetic_expr2, arithmetic_expr3) ->
		get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref arithmetic_expr1;
		get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref arithmetic_expr2;
		get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref arithmetic_expr3


let get_variables_in_parsed_convex_continuous_boolean_expression all_variables_used = function
	| Parsed_CCBE_True
	| Parsed_CCBE_False
		-> ()
		
	| Parsed_CCBE_continuous_inequality parsed_continuous_inequality ->
		get_variables_in_parsed_continuous_inequality all_variables_used parsed_continuous_inequality


let get_variables_in_parsed_convex_continuous_boolean_expressions all_variables_used =
	List.iter (get_variables_in_parsed_convex_continuous_boolean_expression all_variables_used)



(*------------------------------------------------------------*)
(* Gather all variable names used in an update expression *)
(*------------------------------------------------------------*)

(** Add variables names in normal and conditional updates *)
let rec get_variables_in_parsed_update variables_used_ref = function
	| Normal_update (_, Parsed_continuous_term parsed_continuous_arithmetic_expression) -> get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref parsed_continuous_arithmetic_expression
	| Normal_update (_, _) -> raise (NotImplemented "ModelConverter.get_variables_in_parsed_update")
	
	| Condition_update (parsed_continuous_boolean_expression, update_list_if, update_list_else) -> (** recolect in bool exprs *)
		get_variables_in_parsed_continuous_boolean_expression variables_used_ref parsed_continuous_boolean_expression;
		List.iter (fun (_, parsed_rational_term) ->
			match parsed_rational_term with
			| Parsed_continuous_term parsed_continuous_arithmetic_expression ->
				get_variables_in_parsed_continuous_arithmetic_expression variables_used_ref parsed_continuous_arithmetic_expression
			| _ -> raise (NotImplemented "ModelConverter.get_variables_in_parsed_update")
	
		) (update_list_if @ update_list_else)


and get_variables_in_parsed_continuous_boolean_expression variables_used_ref = function
	| Parsed_CBE_True
	| Parsed_CBE_False
		-> ()
	
	| Parsed_CBE_And (bool_expr1, bool_expr2)
	| Parsed_CBE_Or (bool_expr1, bool_expr2)
		->
		get_variables_in_parsed_continuous_boolean_expression variables_used_ref bool_expr1;
		get_variables_in_parsed_continuous_boolean_expression variables_used_ref bool_expr2;
	
	| Parsed_CBE_Not bool_expr -> get_variables_in_parsed_continuous_boolean_expression variables_used_ref bool_expr
	
	| Parsed_CBE_continuous_inequality parsed_continuous_inequality -> get_variables_in_parsed_continuous_inequality variables_used_ref parsed_continuous_inequality
	

(*
		
(************************************************************)
(** Checking discrete arithmetic expressions *)
(************************************************************)


(*------------------------------------------------------------*)
(* Generic function to test something in discrete updates *)
(*------------------------------------------------------------*)
(*** NOTE: f : variable_name -> bool is the function to check *)
let rec check_f_in_parsed_continuous_factor f = function
	| Parsed_CF_variable variable_name ->
		f variable_name

	| Parsed_CF_constant _ -> true

	| Parsed_CF_expression parsed_continuous_arithmetic_expression ->
		check_f_in_parsed_continuous_arithmetic_expression f parsed_continuous_arithmetic_expression
	
	| Parsed_CF_unary_min parsed_rational_factor ->
		check_f_in_parsed_continuous_factor f parsed_rational_factor



and check_f_in_parsed_continuous_term f = function
  | Parsed_CT_mul (parsed_continuous_term, parsed_continuous_factor)
  | Parsed_CT_div (parsed_continuous_term, parsed_continuous_factor) ->
    evaluate_and
      (check_f_in_parsed_continuous_term f parsed_continuous_term)
      (check_f_in_parsed_continuous_factor f parsed_continuous_factor)

  | Parsed_CT_factor parsed_continuous_factor ->
    check_f_in_parsed_continuous_factor f parsed_continuous_factor


and check_f_in_parsed_continuous_arithmetic_expression f = function
  | Parsed_CAE_plus (parsed_continuous_arithmetic_expression, parsed_continuous_term)
  | Parsed_CAE_minus (parsed_continuous_arithmetic_expression , parsed_continuous_term) ->
    evaluate_and
      (check_f_in_parsed_continuous_arithmetic_expression f parsed_continuous_arithmetic_expression)
      (check_f_in_parsed_continuous_term f parsed_continuous_term)

  | Parsed_CAE_term parsed_continuous_term ->
    check_f_in_parsed_continuous_term f parsed_continuous_term


(*------------------------------------------------------------*)
(* Check that all variables are defined in a discrete update *)
(*------------------------------------------------------------*)
let all_variables_defined_in_parsed_continuous_arithmetic_expression variable_names constants =
  check_f_in_parsed_continuous_arithmetic_expression (fun variable_name ->
      if not (List.mem variable_name variable_names) && not (Hashtbl.mem constants variable_name) then(
        print_error ("The variable '" ^ variable_name ^ "' used in an arithmetic expression was not declared."); false
      ) else true
    )


(*------------------------------------------------------------*)
(* Check that only discrete variables are used in a discrete update *)
(*------------------------------------------------------------*)
let check_only_discretes_in_parsed_continuous_arithmetic_expression index_of_variables type_of_variables constants =
  check_f_in_parsed_continuous_arithmetic_expression (fun variable_name ->
      (* Case constant: no problem *)
      if Hashtbl.mem constants variable_name then true
      else (
        (* Get the type of the variable *)
        try(
          let variable_index =
            Hashtbl.find index_of_variables variable_name
          in
          match type_of_variables variable_index with | Var_type_discrete _ -> true | _ -> false
        ) with Not_found -> (
            (* Variable not found! *)
            (*** TODO: why is this checked here…? It should have been checked before ***)
            print_error ("The variable '" ^ variable_name ^ "' used in an update was not declared.");
            false
          )
      )
    )*)


(*
(*** WARNING! duplicate code! removed 2019/12/13 ***)
(*------------------------------------------------------------*)
(* Check that a parsed_continuous_arithmetic_expression contains non-constant at only selected parts *)
(*------------------------------------------------------------*)
let valuate_parsed_continuous_arithmetic_expression constants =

	let rec check_constants_in_parsed_continuous_arithmetic_expression_rec = function
		| Parsed_CAE_plus (parsed_continuous_arithmetic_expression, parsed_continuous_term)
		| Parsed_CAE_minus (parsed_continuous_arithmetic_expression, parsed_continuous_term) ->
		evaluate_and
			(check_constants_in_parsed_continuous_arithmetic_expression_rec parsed_continuous_arithmetic_expression)
			(check_constants_in_parsed_continuous_term parsed_continuous_term)
		| Parsed_CAE_term parsed_continuous_term ->
		check_constants_in_parsed_continuous_term parsed_continuous_term

	and check_constants_in_parsed_continuous_term = function
		| Parsed_CT_mul (parsed_continuous_term, parsed_continuous_factor) ->
		(* Constants only forbidden in the parsed_continuous_term *)
		check_constants_in_parsed_continuous_term parsed_continuous_term
		| Parsed_CT_div (parsed_continuous_term, parsed_continuous_factor) ->
		(* Constants only forbidden in the parsed_continuous_factor *)
		check_constants_in_parsed_continuous_factor parsed_continuous_factor
		| Parsed_CT_factor parsed_continuous_factor -> check_constants_in_parsed_continuous_factor parsed_continuous_factor

	and check_constants_in_parsed_continuous_factor = function
		| Parsed_CF_variable variable_name ->
		if Hashtbl.mem constants variable_name then (
			true
		) else (
			print_error ("Variable '" ^ variable_name ^ "' cannot be used at this place in an update.");
			false
		)
		
		| Parsed_CF_constant var_value -> true
		
		| Parsed_CF_unary_min parsed_rational_factor -> check_constants_in_parsed_continuous_factor parsed_rational_factor
		
		| Parsed_CF_expression parsed_continuous_arithmetic_expression -> check_constants_in_parsed_continuous_arithmetic_expression_rec parsed_continuous_arithmetic_expression

	in check_constants_in_parsed_continuous_arithmetic_expression_rec
*)


(************************************************************)
(** Converting discrete arithmetic expressions *)
(************************************************************)

(*------------------------------------------------------------*)
(* Convert a parsed_continuous_arithmetic_expression into a rational_arithmetic_expression*)
(*------------------------------------------------------------*)

(*** TODO (though really not critical): try to do some simplifications… ***)
(*
(*** NOTE: define a top-level function to avoid recursive passing of all common variables ***)
let rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression index_of_variables constants =
	let rec rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression_rec = function
		| Parsed_CAE_plus (parsed_continuous_arithmetic_expression, parsed_continuous_term) ->
		DAE_plus ((rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression_rec parsed_continuous_arithmetic_expression), (rational_term_of_parsed_continuous_term parsed_continuous_term))
		| Parsed_CAE_minus (parsed_continuous_arithmetic_expression, parsed_continuous_term) ->
		DAE_minus ((rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression_rec parsed_continuous_arithmetic_expression), (rational_term_of_parsed_continuous_term parsed_continuous_term))
		| Parsed_CAE_term parsed_continuous_term ->
		DAE_term (rational_term_of_parsed_continuous_term parsed_continuous_term)

	and rational_term_of_parsed_continuous_term = function
		| Parsed_CT_mul (parsed_continuous_term, parsed_rational_factor) ->
		DT_mul ((rational_term_of_parsed_continuous_term parsed_continuous_term), (rational_factor_of_parsed_continuous_factor parsed_rational_factor))
		| Parsed_CT_div (parsed_continuous_term, parsed_rational_factor) ->
		DT_div ((rational_term_of_parsed_continuous_term parsed_continuous_term), (rational_factor_of_parsed_continuous_factor parsed_rational_factor))
		| Parsed_CT_factor parsed_rational_factor -> DT_factor (rational_factor_of_parsed_continuous_factor parsed_rational_factor)

	and rational_factor_of_parsed_continuous_factor = function
		| Parsed_CF_variable variable_name ->
		(* Try to find the variable_index *)
		if Hashtbl.mem index_of_variables variable_name then (
			let variable_index = Hashtbl.find index_of_variables variable_name in
			(* Convert *)
			DF_variable variable_index
			(* Try to find a constant *)
		) else (
			if Hashtbl.mem constants variable_name then (
			(* Retrieve the value of the global constant *)
			let value = Hashtbl.find constants variable_name in
			(* Convert *)
			DF_constant value
			) else (
			raise (InternalError ("Impossible to find the index of variable '" ^ variable_name ^ "' although this should have been checked before."))
			)
		)
		| Parsed_CF_constant var_value -> DF_constant var_value
		(*** TODO: here, we could very easily get rid of the DF_unary_min by negating the inside expression… ***)
		| Parsed_CF_unary_min parsed_rational_factor -> DF_unary_min (rational_factor_of_parsed_continuous_factor parsed_rational_factor)
		| Parsed_CF_expression parsed_continuous_arithmetic_expression -> DF_expression (rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression_rec parsed_continuous_arithmetic_expression)
	in
	rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression_rec




(*** TODO (though really not critical): try to do some simplifications… ***)

(*** NOTE: define a top-level function to avoid recursive passing of all common variables ***)
let rational_arithmetic_expression_of_parsed_rational_arithmetic_expression index_of_variables constants =
	let rec rational_arithmetic_expression_of_parsed_rational_arithmetic_expression_rec = function
		| Parsed_CAE_plus (parsed_rational_arithmetic_expression, parsed_rational_term) ->
			DAE_plus ((rational_arithmetic_expression_of_parsed_rational_arithmetic_expression_rec parsed_rational_arithmetic_expression), (rational_term_of_parsed_rational_term parsed_rational_term))
		| Parsed_CAE_minus (parsed_rational_arithmetic_expression, parsed_rational_term) ->
			DAE_minus ((rational_arithmetic_expression_of_parsed_rational_arithmetic_expression_rec parsed_rational_arithmetic_expression), (rational_term_of_parsed_rational_term parsed_rational_term))
		| Parsed_CAE_term parsed_rational_term ->
			DAE_term (rational_term_of_parsed_rational_term parsed_rational_term)

	and rational_term_of_parsed_rational_term = function
		| Parsed_CT_mul (parsed_rational_term, parsed_rational_factor) ->
			DT_mul ((rational_term_of_parsed_rational_term parsed_rational_term), (rational_factor_of_parsed_rational_factor parsed_rational_factor))
		| Parsed_CT_div (parsed_rational_term, parsed_rational_factor) ->
			DT_div ((rational_term_of_parsed_rational_term parsed_rational_term), (rational_factor_of_parsed_rational_factor parsed_rational_factor))
		| Parsed_CT_factor parsed_rational_factor -> DT_factor (rational_factor_of_parsed_rational_factor parsed_rational_factor)

	and rational_factor_of_parsed_rational_factor = function
		| Parsed_CF_variable variable_name ->
		(* Try to find the variable_index *)
		if Hashtbl.mem index_of_variables variable_name then (
			let variable_index = Hashtbl.find index_of_variables variable_name in
			(* Convert *)
			DF_variable variable_index
			(* Try to find a constant *)
		) else (
			if Hashtbl.mem constants variable_name then (
			(* Retrieve the value of the global constant *)
			let value = Hashtbl.find constants variable_name in
			(* Convert *)
			DF_constant value
			) else (
			raise (InternalError ("Impossible to find the index of variable '" ^ variable_name ^ "' although this should have been checked before."))
			)
		)
		| Parsed_CF_constant var_value -> DF_constant var_value
		| Parsed_CF_expression parsed_rational_arithmetic_expression -> DF_expression (rational_arithmetic_expression_of_parsed_rational_arithmetic_expression_rec parsed_rational_arithmetic_expression)
		| Parsed_CF_unary_min parsed_rational_factor -> DF_unary_min (rational_factor_of_parsed_rational_factor parsed_rational_factor)
	in
	(* Call high-level function *)
	rational_arithmetic_expression_of_parsed_rational_arithmetic_expression_rec



(*------------------------------------------------------------*)
(* Convert a parsed_continuous_arithmetic_expression into a linear_term *)
(*------------------------------------------------------------*)


(*** TODO (though really not critical): try to do some simplifications… ***)
(* First valuate a parsed_continuous_arithmetic_expression if requested; raises InternalError if some non-constant variable is met *)
let rec valuate_parsed_continuous_arithmetic_expression constants = function
	| Parsed_CAE_plus (parsed_continuous_arithmetic_expression, parsed_continuous_term) ->
		NumConst.add
		(valuate_parsed_continuous_arithmetic_expression constants parsed_continuous_arithmetic_expression)
		(valuate_parsed_continuous_term constants parsed_continuous_term)
	| Parsed_CAE_minus (parsed_continuous_arithmetic_expression, parsed_continuous_term) ->
		NumConst.sub
		(valuate_parsed_continuous_arithmetic_expression constants parsed_continuous_arithmetic_expression)
		(valuate_parsed_continuous_term constants parsed_continuous_term)
	| Parsed_CAE_term parsed_continuous_term ->
		valuate_parsed_continuous_term constants parsed_continuous_term;

	and valuate_parsed_continuous_term constants = function
	| Parsed_CT_mul (parsed_continuous_term, parsed_continuous_factor) ->
		NumConst.mul
		(valuate_parsed_continuous_term constants parsed_continuous_term)
		(valuate_parsed_continuous_factor constants parsed_continuous_factor)
	| Parsed_CT_div (parsed_continuous_term, parsed_continuous_factor) ->
		NumConst.div
		(valuate_parsed_continuous_term constants parsed_continuous_term)
		(valuate_parsed_continuous_factor constants parsed_continuous_factor)
	| Parsed_CT_factor parsed_continuous_factor -> valuate_parsed_continuous_factor constants parsed_continuous_factor

	and valuate_parsed_continuous_factor constants = function
	| Parsed_CF_variable variable_name ->
		if Hashtbl.mem constants variable_name then (
		(* Retrieve the value of the global constant *)
		Hashtbl.find constants variable_name
		) else (
		raise (InternalError ("Impossible to find the index of variable '" ^ variable_name ^ "' in function 'valuate_parsed_continuous_arithmetic_expression' although it should have been checked before."))
		)
	| Parsed_CF_constant var_value -> var_value
	| Parsed_CF_unary_min parsed_rational_factor -> NumConst.neg (valuate_parsed_continuous_factor constants parsed_rational_factor)
	| Parsed_CF_expression parsed_continuous_arithmetic_expression -> valuate_parsed_continuous_arithmetic_expression constants parsed_continuous_arithmetic_expression




(** Convert a Boolean operator to its abstract model *)
let convert_parsed_relop = function
	| PARSED_OP_L	-> OP_L
	| PARSED_OP_LEQ	-> OP_LEQ
	| PARSED_OP_EQ	-> OP_EQ
	| PARSED_OP_NEQ	-> OP_NEQ
	| PARSED_OP_GEQ	-> OP_GEQ
	| PARSED_OP_G 	-> OP_G


let convert_rational_bool_expr index_of_variables constants = function
	| Parsed_expression (expr1, relop, expr2) -> Expression (
		(rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression index_of_variables constants expr1),
		(convert_parsed_relop relop),
		(rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression index_of_variables constants expr2)
		)
	| Parsed_expression_in (expr1, expr2, expr3) -> Expression_in (
		(rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression index_of_variables constants expr1),
		(rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression index_of_variables constants expr2),
		(rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression index_of_variables constants expr3)
		)
		
(** Convert a boolean expression in its abstract model *)
let rec convert_bool_expr index_of_variables constants = function
	| Parsed_True -> True_bool
	| Parsed_False -> False_bool
	| Parsed_Not e -> Not_bool (convert_bool_expr index_of_variables constants e)
	| Parsed_And (e1,e2) -> And_bool ((convert_bool_expr index_of_variables constants e1), (convert_bool_expr index_of_variables constants e2))
	| Parsed_Or (e1, e2) -> Or_bool ((convert_bool_expr index_of_variables constants e1), (convert_bool_expr index_of_variables constants e2))
	| Parsed_rational_boolean_expression parsed_rational_boolean_expression ->
		Rational_boolean_expression (convert_rational_bool_expr index_of_variables constants parsed_rational_boolean_expression)



(*------------------------------------------------------------*)
(* Functions for property conversion *)
(*------------------------------------------------------------*)

(* Convert parsed_rational_arithmetic_expression *)
let rec convert_parsed_rational_arithmetic_expression useful_parsing_model_information = function
	| Parsed_CAE_plus (parsed_rational_arithmetic_expression , parsed_rational_term) ->
		DAE_plus (
			(convert_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression)
			,
			(convert_parsed_rational_term useful_parsing_model_information parsed_rational_term)
		)
	| Parsed_CAE_minus (parsed_rational_arithmetic_expression , parsed_rational_term) ->
		DAE_minus (
			(convert_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression)
			,
			(convert_parsed_rational_term useful_parsing_model_information parsed_rational_term)
		)
	| Parsed_CAE_term parsed_rational_term -> DAE_term (convert_parsed_rational_term useful_parsing_model_information parsed_rational_term)

and convert_parsed_rational_term useful_parsing_model_information = function
	| Parsed_CT_mul (parsed_rational_term , parsed_rational_factor) ->
		DT_mul ((convert_parsed_rational_term useful_parsing_model_information parsed_rational_term) , convert_parsed_rational_factor useful_parsing_model_information parsed_rational_factor)
	| Parsed_CT_div (parsed_rational_term , parsed_rational_factor) ->
		DT_div ((convert_parsed_rational_term useful_parsing_model_information parsed_rational_term) , convert_parsed_rational_factor useful_parsing_model_information parsed_rational_factor)
	| Parsed_CT_factor parsed_rational_factor -> DT_factor (convert_parsed_rational_factor useful_parsing_model_information parsed_rational_factor)

and convert_parsed_rational_factor useful_parsing_model_information = function
	| Parsed_CF_variable variable_name ->
		(* First check whether this is a constant *)
		if Hashtbl.mem useful_parsing_model_information.constants variable_name then
			DF_constant (Hashtbl.find useful_parsing_model_information.constants variable_name)
		(* Otherwise: a variable *)
		else DF_variable (Hashtbl.find useful_parsing_model_information.index_of_variables variable_name)
	| Parsed_CF_constant var_value -> DF_constant var_value
	| Parsed_CF_expression parsed_rational_arithmetic_expression -> DF_expression (convert_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression)
	| Parsed_CF_unary_min parsed_rational_factor -> DF_unary_min (convert_parsed_rational_factor useful_parsing_model_information parsed_rational_factor)


(* Convert parsed_rational_boolean_expression *)
let convert_parsed_rational_boolean_expression useful_parsing_model_information = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Parsed_expression (parsed_rational_arithmetic_expression1 , parsed_relop , parsed_rational_arithmetic_expression2) ->
		Expression (
			convert_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression1
			,
			convert_parsed_relop parsed_relop
			,
			convert_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression2
		)
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Parsed_expression_in (parsed_rational_arithmetic_expression1 , parsed_rational_arithmetic_expression2 , parsed_rational_arithmetic_expression3) ->
		Expression_in (
			convert_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression1
			,
			convert_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression2
			,
			convert_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression3
		)

	
(* Convert parsed_loc_predicate *)
let convert_parsed_loc_predicate useful_parsing_model_information = function
	| Parsed_loc_predicate_EQ (automaton_name, location_name) ->
		let automaton_index = Hashtbl.find useful_parsing_model_information.index_of_automata automaton_name in
		Loc_predicate_EQ ( automaton_index , (Hashtbl.find useful_parsing_model_information.index_of_locations.(automaton_index) location_name))
	| Parsed_loc_predicate_NEQ (automaton_name, location_name) ->
		let automaton_index = Hashtbl.find useful_parsing_model_information.index_of_automata automaton_name in
		Loc_predicate_NEQ (automaton_index , (Hashtbl.find useful_parsing_model_information.index_of_locations.(automaton_index) location_name))


(* Convert parsed_simple_predicate *)
let convert_parsed_simple_predicate useful_parsing_model_information = function
	| Parsed_rational_boolean_predicate parsed_rational_boolean_expression -> Rational_boolean_expression (convert_parsed_rational_boolean_expression useful_parsing_model_information parsed_rational_boolean_expression)
	| Parsed_loc_predicate parsed_loc_predicate -> Loc_predicate (convert_parsed_loc_predicate useful_parsing_model_information parsed_loc_predicate)


(* Convert parsed_state_predicate *)
let rec convert_parsed_state_predicate_factor useful_parsing_model_information = function
	| Parsed_state_predicate_factor_NOT parsed_state_predicate_factor -> State_predicate_factor_NOT (convert_parsed_state_predicate_factor useful_parsing_model_information parsed_state_predicate_factor)
	| Parsed_simple_predicate parsed_simple_predicate -> Simple_predicate (convert_parsed_simple_predicate useful_parsing_model_information parsed_simple_predicate)
	| Parsed_state_predicate parsed_state_predicate -> State_predicate (convert_parsed_state_predicate useful_parsing_model_information parsed_state_predicate)

and convert_parsed_state_predicate_term useful_parsing_model_information = function
	| Parsed_state_predicate_term_AND (parsed_state_predicate_term1, parsed_state_predicate_term2) ->
		State_predicate_term_AND (
			convert_parsed_state_predicate_term useful_parsing_model_information parsed_state_predicate_term1
			,
			convert_parsed_state_predicate_term useful_parsing_model_information parsed_state_predicate_term2
		)
	| Parsed_state_predicate_factor parsed_state_predicate_factor -> State_predicate_factor (convert_parsed_state_predicate_factor useful_parsing_model_information parsed_state_predicate_factor)

and convert_parsed_state_predicate useful_parsing_model_information = function
	| Parsed_state_predicate_OR (parsed_state_predicate1, parsed_state_predicate2) ->
		State_predicate_OR (
			convert_parsed_state_predicate useful_parsing_model_information parsed_state_predicate1
			,
			convert_parsed_state_predicate useful_parsing_model_information parsed_state_predicate2
		)
	| Parsed_state_predicate_term parsed_state_predicate_term -> State_predicate_term (convert_parsed_state_predicate_term useful_parsing_model_information parsed_state_predicate_term)
	| Parsed_state_predicate_true -> State_predicate_true
	| Parsed_state_predicate_false -> State_predicate_false

*)



(************************************************************)
(************************************************************)
(** Checking and converting linear constraints *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Getting variables *)
(************************************************************)

(*
(*------------------------------------------------------------*)
(* Gather all variable names used in a linear_term *)
(*------------------------------------------------------------*)
let get_variables_in_linear_term variables_used_ref = function
  | Constant _ -> ()
  | Variable (_, variable_name) ->
    (* Add the variable name to the set and update the reference *)
    variables_used_ref := StringSet.add variable_name !variables_used_ref


(*------------------------------------------------------------*)
(* Gather all variable names used in a linear_expression *)
(*------------------------------------------------------------*)
let rec get_variables_in_linear_expression variables_used_ref = function
  | Linear_term linear_term -> get_variables_in_linear_term variables_used_ref linear_term
  | Linear_plus_expression (linear_expression, linear_term)
    ->
    get_variables_in_linear_expression variables_used_ref linear_expression;
    get_variables_in_linear_term variables_used_ref linear_term
  | Linear_minus_expression (linear_expression, linear_term)
    ->
    get_variables_in_linear_expression variables_used_ref linear_expression; get_variables_in_linear_term variables_used_ref linear_term


(*------------------------------------------------------------*)
(* Gather all variable names used in a linear_constraint *)
(*------------------------------------------------------------*)
let get_variables_in_linear_constraint variables_used_ref = function
  | Parsed_true_constraint -> ()
  | Parsed_false_constraint -> ()
  | Parsed_linear_constraint (linear_expression1, (*relop*)_, linear_expression2) ->
    get_variables_in_linear_expression variables_used_ref linear_expression1;
    get_variables_in_linear_expression variables_used_ref linear_expression2

    
(*------------------------------------------------------------*)
(* Gather all variable names used in a parsed_init_state_predicate *)
(*------------------------------------------------------------*)
let get_variables_in_init_state_predicate variables_used_ref = function
	| Parsed_loc_assignment _ -> ()
	| Parsed_linear_predicate linear_constraint -> get_variables_in_linear_constraint variables_used_ref linear_constraint

	
(*(*------------------------------------------------------------*)
(* Gather all variable names used in a parsed_loc_predicate *)
(*------------------------------------------------------------*)
let get_variables_in_parsed_loc_predicate variables_used_ref = function
	| Parsed_loc_predicate_EQ of automaton_name * location_name
	| Parsed_loc_predicate_NEQ of automaton_name * location_name*)
*)

(*------------------------------------------------------------*)
(* Gather all variable names used in a Simple_predicate *)
(*------------------------------------------------------------*)
let get_variables_in_parsed_simple_predicate variables_used_ref = function
	| Parsed_rational_boolean_predicate parsed_continuous_inequality ->
		get_variables_in_parsed_continuous_inequality variables_used_ref parsed_continuous_inequality
		
	| Parsed_loc_predicate parsed_loc_predicate ->
		(* No variable in location predicate *)
		()


(*------------------------------------------------------------*)
(* Gather all variable names used in a parsed_state_predicate_factor *)
(*------------------------------------------------------------*)
let rec get_variables_in_parsed_state_predicate_factor variables_used_ref = function
	| Parsed_state_predicate_factor_NOT parsed_state_predicate_factor ->
		get_variables_in_parsed_state_predicate_factor variables_used_ref parsed_state_predicate_factor
		
	| Parsed_simple_predicate parsed_simple_predicate ->
		get_variables_in_parsed_simple_predicate variables_used_ref parsed_simple_predicate
		
	| Parsed_state_predicate parsed_state_predicate ->
		get_variables_in_parsed_state_predicate variables_used_ref parsed_state_predicate

	
	
(*------------------------------------------------------------*)
(* Gather all variable names used in a parsed_state_predicate_term *)
(*------------------------------------------------------------*)
and get_variables_in_parsed_state_predicate_term variables_used_ref = function
	| Parsed_state_predicate_term_AND ( parsed_state_predicate_term1, parsed_state_predicate_term2) ->
		get_variables_in_parsed_state_predicate_term variables_used_ref parsed_state_predicate_term1;
		get_variables_in_parsed_state_predicate_term variables_used_ref parsed_state_predicate_term2;
			
	| Parsed_state_predicate_factor parsed_state_predicate_factor ->
		get_variables_in_parsed_state_predicate_factor variables_used_ref parsed_state_predicate_factor


(*------------------------------------------------------------*)
(* Gather all variable names used in a parsed_state_predicate *)
(*------------------------------------------------------------*)
and get_variables_in_parsed_state_predicate variables_used_ref = function
	| Parsed_state_predicate_OR (parsed_state_predicate1 , parsed_state_predicate2) ->
		get_variables_in_parsed_state_predicate variables_used_ref parsed_state_predicate1;
		get_variables_in_parsed_state_predicate variables_used_ref parsed_state_predicate2;
	
	| Parsed_state_predicate_term parsed_state_predicate_term ->
		get_variables_in_parsed_state_predicate_term variables_used_ref parsed_state_predicate_term

	| Parsed_state_predicate_true | Parsed_state_predicate_false
		-> ()

(*
(*------------------------------------------------------------*)
(* Gather all variable names used in a convex predicate *)
(*------------------------------------------------------------*)
let get_variables_in_convex_predicate variables_used_ref =
  List.iter (get_variables_in_linear_constraint variables_used_ref)


  
(*------------------------------------------------------------*)
(* Find the clocks in a linear_constraint *)
(*------------------------------------------------------------*)

let get_clocks_in_linear_constraint clocks =
  (* Get a long list with duplicates, and then simplify *)
  (*	(* Should not be too inefficient, because our linear constraints are relatively small *)
    	let list_of_clocks = List.fold_left (fun current_list_of_clocks linear_inequality ->
    		list_append current_list_of_clocks (get_clocks_in_linear_inequality is_clock linear_inequality)
    	) [] linear_constraint
    	in
    	(* Simplify *)
    	list_only_once list_of_clocks*)
  LinearConstraint.pxd_find_variables clocks


(*** WARNING: duplicate function in ClockElimination ***)
let rec get_clocks_in_updates updates : clock_index list =
  let get_clocks: clock_updates -> clock_index list = function
    (* No update at all *)
    | No_update -> []
    (* Reset to 0 only *)
    | Resets clock_reset_list -> clock_reset_list
    (* Reset to arbitrary value (including discrete, parameters and clocks) *)
    | Updates clock_update_list ->
      let result, _ = List.split clock_update_list in result
  in
  let clocks_in_conditons = List.flatten (List.map
    (fun (b, u1, u2) -> (get_clocks_in_updates u1) @ (get_clocks_in_updates u2) )
    updates.conditional)
  in
  (get_clocks updates.clock) @ clocks_in_conditons


(************************************************************)
(** Checking linear constraints *)
(************************************************************)


(*------------------------------------------------------------*)
(* Check that all variables are defined in a linear_term *)
(*------------------------------------------------------------*)
let all_variables_defined_in_linear_term variable_names constants = function
  | Constant _ -> true
  | Variable (_, variable_name) -> if not (List.mem variable_name variable_names) && not (Hashtbl.mem constants variable_name) then(
      print_error ("The variable '" ^ variable_name ^ "' used in a linear constraint was not declared."); false
    ) else true


(*------------------------------------------------------------*)
(* Check that all variables are defined in a linear_expression *)
(*------------------------------------------------------------*)
let rec all_variables_defined_in_linear_expression variable_names constants = function
  | Linear_term linear_term -> all_variables_defined_in_linear_term variable_names constants linear_term
  | Linear_plus_expression (linear_expression, linear_term)
    -> evaluate_and (all_variables_defined_in_linear_expression variable_names constants linear_expression) (all_variables_defined_in_linear_term variable_names constants linear_term)
  | Linear_minus_expression (linear_expression, linear_term)
    -> evaluate_and (all_variables_defined_in_linear_expression variable_names constants linear_expression) (all_variables_defined_in_linear_term variable_names constants linear_term)


(*------------------------------------------------------------*)
(* Check that all variables are defined in a linear_constraint *)
(*------------------------------------------------------------*)
let all_variables_defined_in_linear_constraint variable_names constants = function
  | Parsed_true_constraint -> true
  | Parsed_false_constraint -> true
  | Parsed_linear_constraint (linear_expression1, relop, linear_expression2) ->
    evaluate_and (all_variables_defined_in_linear_expression variable_names constants linear_expression1)
      (all_variables_defined_in_linear_expression variable_names constants linear_expression2)


(*------------------------------------------------------------*)
(* Check that all variables are defined in a convex predicate *)
(*------------------------------------------------------------*)
let all_variables_defined_in_convex_predicate variable_names constants =
  List.fold_left
    (fun all_defined linear_constraint ->
       evaluate_and all_defined (all_variables_defined_in_linear_constraint variable_names constants linear_constraint)
    )
    true


(*------------------------------------------------------------*)
(* Generic function to test something in linear expressions *)
(*------------------------------------------------------------*)
let rec check_f_in_linear_expression f index_of_variables type_of_variables constants = function
  | Linear_term linear_term ->
    f index_of_variables type_of_variables constants linear_term
  | Linear_plus_expression (linear_expression, linear_term) ->
    check_f_in_linear_expression f index_of_variables type_of_variables constants linear_expression
    && f index_of_variables type_of_variables constants linear_term
  | Linear_minus_expression (linear_expression, linear_term) ->
    check_f_in_linear_expression f index_of_variables type_of_variables constants linear_expression
    && f index_of_variables type_of_variables constants linear_term


(*------------------------------------------------------------*)
(* Check that a linear expression contains only discrete variables and constants *)
(*------------------------------------------------------------*)
let only_rational_in_linear_term index_of_variables type_of_variables constants = function
  | Constant _ -> true
  | Variable (_, variable_name) ->
    (* Constants are allowed *)
    (Hashtbl.mem constants variable_name)

    (* Or discrete *)
    ||
    try(
      let variable_index =
        Hashtbl.find index_of_variables variable_name
      in
      match type_of_variables variable_index with | Var_type_discrete _ -> true | _ -> false
    ) with Not_found -> (
        (* Variable not found! *)
        (*** TODO: why is this checked here…? It should have been checked before ***)
        print_error ("The variable '" ^ variable_name ^ "' used in an update was not declared.");
        false
      )

let only_rational_in_linear_expression = check_f_in_linear_expression only_rational_in_linear_term

(*------------------------------------------------------------*)
(* Check that a linear expression contains no variables (neither discrete nor clock) *)
(*------------------------------------------------------------*)
let no_variables_in_linear_term index_of_variables type_of_variables constants = function
  | Constant _ -> true
  | Variable (_, variable_name) ->
    (* Constants are allowed *)
    (Hashtbl.mem constants variable_name)
    (* Or parameter *)
    ||
    let variable_index = Hashtbl.find index_of_variables variable_name in
    type_of_variables variable_index = Var_type_parameter

let no_variables_in_linear_expression = check_f_in_linear_expression no_variables_in_linear_term


*)
(************************************************************)
(** Converting linear constraints *)
(************************************************************)

(*


(*------------------------------------------------------------*)
(* Convert a ParsingStructure.linear_expression into an array of coef and constant *)
(*------------------------------------------------------------*)
let array_of_coef_of_linear_expression index_of_variables constants linear_expression =
  (* Create an array of coef *)
  let array_of_coef = Array.make (Hashtbl.length index_of_variables) NumConst.zero in
  (* Create a zero constant *)
  let constant = ref NumConst.zero in

  (* Internal function to update the array for a linear term *)
  let update_array_linear_term mul_coef = function
    (* Case constant -> update the constant with the coef *)
    | Constant c -> constant := NumConst.add !constant (NumConst.mul c mul_coef);
      (* Case variables -> update the array with the coef  *)
    | Variable (coef, variable_name) ->
      (* Try to find the variable_index *)
      if Hashtbl.mem index_of_variables variable_name then (
        let variable_index = Hashtbl.find index_of_variables variable_name in
        (* Update the variable with its coef *)
        array_of_coef.(variable_index) <- NumConst.add array_of_coef.(variable_index) (NumConst.mul coef mul_coef);
        (* Try to find a constant *)
      ) else (
        if Hashtbl.mem constants variable_name then (
          (* Retrieve the value of the global constant *)
          let value = Hashtbl.find constants variable_name in
          (* Update the NumConst *)
          constant := NumConst.add !constant (NumConst.mul (NumConst.mul value coef) mul_coef);
        ) else (
          raise (InternalError ("Impossible to find the index of variable '" ^ variable_name ^ "' although this should have been checked before."))
        )
      );
  in
  (* Internal function to update the array for a linear expression *)
  let rec update_array_linear_expression = function
    | Linear_term lt -> update_array_linear_term NumConst.one lt
    | Linear_plus_expression (le, lt) ->
      (* Fill the array with le *)
      update_array_linear_expression le;
      (* Fill the array with lt *)
      update_array_linear_term NumConst.one lt;
    | Linear_minus_expression (le, lt) ->
      (* Fill the array with le *)
      update_array_linear_expression le;
      (* Fill the array with lt *)
      update_array_linear_term NumConst.minus_one lt;
  in
  (* Call the recursive function *)
  update_array_linear_expression linear_expression;
  (* Return the array of coef and the constant *)
  array_of_coef, !constant


(*------------------------------------------------------------*)
(* Convert an array of variable coef into a linear term *)
(*------------------------------------------------------------*)
let linear_term_of_array array_of_coef constant =
  (* Create an empty list of members *)
  let members = ref [] in
  (* Iterate on the coef *)
  Array.iteri (fun variable_index coef ->
      if NumConst.neq coef NumConst.zero then (
        (* Add the member *)
        members := (coef, variable_index) :: !members;
      );
    ) array_of_coef;
  (* Create the linear term *)
  LinearConstraint.make_pxd_linear_term !members constant


(*------------------------------------------------------------*)
(* Direct conversion of a ParsingStructure.linear_expression into a Parsed_linear_constraint.linear_term *)
(*------------------------------------------------------------*)
let linear_term_of_linear_expression index_of_variables constants linear_expression =
  let array_of_coef, constant = array_of_coef_of_linear_expression index_of_variables constants linear_expression in
  linear_term_of_array array_of_coef constant


(*------------------------------------------------------------*)
(* Perform the substraction of 2 NumConst array of same size *)
(*------------------------------------------------------------*)
let sub_array array1 array2 =
  (* Create the result *)
  let result = Array.make (Array.length array1) NumConst.zero in
  (* Iterate on both arrays *)
  for i = 0 to (Array.length array1) - 1 do
    (* Perform array1 - array2 *)
    result.(i) <- NumConst.sub array1.(i) array2.(i);
  done;
  (* Return the result *)
  result


(*------------------------------------------------------------*)
(* Convert a ParsingStructure.linear_constraint into a Constraint.linear_inequality *)
(*------------------------------------------------------------*)
let linear_inequality_of_linear_constraint index_of_variables constants (linexpr1, relop, linexpr2) =
  (* Get the array of variables and constant associated to the linear terms *)
  let array1, constant1 = array_of_coef_of_linear_expression index_of_variables constants linexpr1 in
  let array2, constant2 = array_of_coef_of_linear_expression index_of_variables constants linexpr2 in
  (* Consider the operator *)
  match relop with
  (* a < b <=> b - a > 0 *)
  | PARSED_OP_L ->
    (* Create the array *)
    let array12 = sub_array array2 array1 in
    (* Create the constant *)
    let constant12 = NumConst.sub constant2 constant1 in
    (* Create the linear_term *)
    let linear_term = linear_term_of_array array12 constant12 in
    (* Return the linear_inequality *)
    LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_g
  (* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_g *)

  (* a <= b <=> b - a >= 0 *)
  | PARSED_OP_LEQ ->
    (* Create the array *)
    let array12 = sub_array array2 array1 in
    (* Create the constant *)
    let constant12 = NumConst.sub constant2 constant1 in
    (* Create the linear_term *)
    let linear_term = linear_term_of_array array12 constant12 in
    (* Return the linear_inequality *)
    LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_ge
  (* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_ge *)

  (* a = b <=> b - a = 0 *)
  | PARSED_OP_EQ ->
    (* Create the array *)
    let array12 = sub_array array2 array1 in
    (* Create the constant *)
    let constant12 = NumConst.sub constant2 constant1 in
    (* Create the linear_term *)
    let linear_term = linear_term_of_array array12 constant12 in
    (* Return the linear_inequality *)
    LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_eq

  (* 	(Constraint.substract_linear_terms lt1 lt2), Constraint.Op_eq *)

  (* a >= b <=> a - b >= 0 *)
  | PARSED_OP_GEQ ->
    (* Create the array *)
    let array12 = sub_array array1 array2 in
    (* Create the constant *)
    let constant12 = NumConst.sub constant1 constant2 in
    (* Create the linear_term *)
    let linear_term = linear_term_of_array array12 constant12 in
    (* Return the linear_inequality *)
    LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_ge
  (* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_ge *)

  (* a > b <=> a - b > 0 *)
  | PARSED_OP_G ->
    (* Create the array *)
    let array12 = sub_array array1 array2 in
    (* Create the constant *)
    let constant12 = NumConst.sub constant1 constant2 in
    (* Create the linear_term *)
    let linear_term = linear_term_of_array array12 constant12 in
    (* Return the linear_inequality *)
    LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_g
  (* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_g *)

  | PARSED_OP_NEQ ->
    raise (InternalError("Inequality <> not yet supported"))


(*------------------------------------------------------------*)
(* Convert a ParsingStructure.convex_predicate into a Constraint.linear_constraint *)
(*------------------------------------------------------------*)
let linear_constraint_of_convex_predicate index_of_variables constants convex_predicate : LinearConstraint.pxd_linear_constraint =
  try(
    (* Compute a list of inequalities *)
    let linear_inequalities = List.fold_left
        (fun linear_inequalities linear_inequality ->
           match linear_inequality with
           | Parsed_true_constraint -> linear_inequalities
           | Parsed_false_constraint -> raise False_exception
           | Parsed_linear_constraint (linexpr1, relop, linexpr2) -> (linear_inequality_of_linear_constraint index_of_variables constants (linexpr1, relop, linexpr2)) :: linear_inequalities
        ) [] convex_predicate
    in LinearConstraint.make_pxd_constraint linear_inequalities
    (* Stop if any false constraint is found *)
  ) with False_exception -> LinearConstraint.pxd_false_constraint ()
*)





(************************************************************)
(************************************************************)
(** Checking and converting model *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Getting variables *)
(************************************************************)

(*------------------------------------------------------------*)
(* A structure to collect declared variable names *)
(*------------------------------------------------------------*)
(*type declared_variable_names = {
	declared_parameters	: variable_name list;
	declared_clocks		: variable_name list;
	(* Associative list, keys are parsed_var_type_discrete, and values are variable names for this parsed_var_type_discrete *)
	declared_discrete	: (parsed_var_type_discrete, variable_name) list;
	declared_constants	: (variable_name, variable_value) list;
	unassigned_constant	: variable_name list;
}*)

(*(*------------------------------------------------------------*)
(* Get all (possibly identical) names of variables in the header *)
(*------------------------------------------------------------*)
let get_declared_variable_names variable_declarations =
	let get_variables_and_constants =
		List.fold_left (fun (current_list, constants) (name, possible_value) ->
			match variable_declaration with
			(* If no value: add to names *)
			| Parsed_variable_declaration variable_name -> (variable_name :: current_list , constants)
			(* Otherwise: add to constants *)
			| Parsed_constant_declaration constant_name, constant_value -> (current_list , (constant_name, constant_value) :: constants)
		) ([], [])
	in
	(* Get all (possibly identical) names of variables in one variable declaration and add it to the computed n-uple *)
	(*** NOTE: perhaps imperative programming with references would be smarter… ***)
	let get_variables_in_variable_declaration collected_variable_names(*(clocks, discrete, parameters, constants, unassigned_constants)*) (var_type, list_of_names) =
		let new_list, new_constants = get_variables_and_constants list_of_names in
		match var_type with
		| ParsingStructure.Parsed_var_type_clock ->
			{
				declared_parameters	= collected_variable_names.declared_parameters;
				declared_clocks		= List.rev_append new_list collected_variable_names.declared_clocks;
				declared_discrete	= collected_variable_names.declared_discrete;
				declared_constants	= List.rev_append new_constants collected_variable_names.declared_constants;
				unassigned_constant	= collected_variable_names.unassigned_constant;
			}
			
		| ParsingStructure.Parsed_var_type_constant ->
			{
				declared_parameters	= collected_variable_names.declared_parameters;
				declared_clocks		= collected_variable_names.declared_clocks;
				declared_discrete	= collected_variable_names.declared_discrete;
				declared_constants	= List.rev_append new_constants collected_variable_names.declared_constants;
				unassigned_constant	= List.rev_append new_list collected_variable_names.unassigned_constant;
			}
		
		| ParsingStructure.Parsed_var_type_parameter ->
			{
				declared_parameters	= List.rev_append new_list collected_variable_names.declared_parameters;
				declared_clocks		= collected_variable_names.declared_clocks;
				declared_discrete	= collected_variable_names.declared_discrete;
				declared_constants	= List.rev_append new_constants collected_variable_names.declared_constants;
				unassigned_constant	= collected_variable_names.unassigned_constant;
			}

		| ParsingStructure.Parsed_var_type_discrete parsed_var_type_discrete ->
			(* Try to find the variable names already collected for this parsed_var_type_discrete *)
			let list_of_variable_names_for_this_type = try(
				List.assoc parsed_var_type_discrete collected_variable_names.declared_discrete
			) with Not_found -> []
			in
			(* Remove the binding (if any) *)
			let all_rational_variables_except_for_this_type = List.remove_assoc parsed_var_type_discrete collected_variable_names.declared_discrete in
			(* Add the binding back *)
			let updated_rational_variables = (parsed_var_type_discrete , List.rev_append new_list all_rational_variables_except_for_this_type) :: all_rational_variables_except_for_this_type in
			
			{
				declared_parameters	= collected_variable_names.declared_parameters;
				declared_clocks		= collected_variable_names.declared_clocks;
				declared_discrete	= updated_rational_variables;
				declared_constants	= List.rev_append new_constants collected_variable_names.declared_constants;
				unassigned_constant	= collected_variable_names.unassigned_constant;
			}
	in
	let collected_variable_names =
		List.fold_left
			get_variables_in_variable_declaration
			{
				declared_parameters	= [];
				declared_clocks		= [];
				declared_discrete	= [];
				declared_constants	= [];
				unassigned_constant	= [];
			}
			variable_declarations
		in
	(* Do not reverse lists *)
	collected_variable_names*)

(*------------------------------------------------------------*)
(* Get all discrete variables *)
(*------------------------------------------------------------*)
let get_all_discrete_variables variables_per_type =
	(* Collect for each discrete type *)
	List.fold_left
		(fun current_list rational_type -> 
			try (
				let list_of_variables_for_this_type = Hashtbl.find variables_per_type rational_type in
				List.rev_append list_of_variables_for_this_type current_list
			(* If not found: keep the list unchanged *)
			) with Not_found -> current_list
		)
		[] all_parsed_rational_types


(*------------------------------------------------------------*)
(* Get all (possibly identical) names of automata *)
(*------------------------------------------------------------*)
let get_declared_automata_names =
  List.map (fun (automaton_name, _, _) -> automaton_name)

(*------------------------------------------------------------*)
(* Get all (all different) names of synclabs *)
(*------------------------------------------------------------*)
let get_declared_synclabs_names =
  List.fold_left (fun action_names (_, synclabs, _) -> list_union action_names synclabs) []

  
(*------------------------------------------------------------*)
(* Get the set of all variable names used in the parsed model *)
(*------------------------------------------------------------*)
let get_all_variables_used_in_model (parsed_model : ParsingStructure.parsed_model) =
	(* Create a set structure for variable names *)
	let all_variables_used = ref StringSet.empty in

	(*** NOTE: we pass this set by reference ***)

	(* Gather in each automaton *)
	List.iter (fun (automaton_name, sync_name_list, locations) ->
		print_message Verbose_total ("      Gathering variables used in automaton " ^ automaton_name);

		(* Gather in each location *)
		List.iter (fun (location : parsed_location) ->
			print_message Verbose_total ("        Gathering variables used in location " ^ location.name);

			(* Gather in the cost *)
			(*** TODO: reintroduce cost ***)
(*			begin
				match location.cost with
				| Some cost ->
				print_message Verbose_total ("          Gathering variables in used cost");
				get_variables_in_linear_expression all_variables_used cost;
				| None -> ()
			end;*)

			(* Gather in the stopwatches *)
			print_message Verbose_total ("          Gathering variables used in possible stopwatches");
			List.iter (fun stopwatch_name ->
				all_variables_used := StringSet.add stopwatch_name !all_variables_used
				) location.stopped;

			(* Gather in the invariant *)
			print_message Verbose_total ("          Gathering variables in invariant");
			get_variables_in_parsed_convex_continuous_boolean_expressions all_variables_used location.invariant;

			(* Gather in transitions *)
			print_message Verbose_total ("          Gathering variables in transitions");
			List.iter (fun (transition : parsed_transition) ->
				(* Gather in the convex predicate (guard) *)
				print_message Verbose_total ("            Gathering variables in guard");
				get_variables_in_parsed_convex_continuous_boolean_expressions all_variables_used transition.guard;

				(* Gather in the updates *)
				print_message Verbose_total ("            Gathering variables in updates");
				(* List.iter (fun (variable_name, arithmetic_expression) -> *)
				List.iter (fun update_expression ->
					(*** NOTE: let us NOT consider that a reset is a 'use' of a variable; it must still be used in a guard, an invariant, in the right-hand side term of a reset, or a property, to be considered 'used' in the model ***)
					(* First add the variable to be updated *)
					(* 					all_variables_used := StringSet.add variable_name !all_variables_used; *)
					(* Second add the variable names in the update expression *)
					(* get_variables_in_parsed_continuous_arithmetic_expression all_variables_used arithmetic_expression; *)
					get_variables_in_parsed_update all_variables_used update_expression
					) transition.updates;
				) location.transitions;
			) locations;
		) parsed_model.automata;

	(* Return the set of variables actually used *)
	!all_variables_used



(************************************************************)
(** Checking the model *)
(************************************************************)

(*------------------------------------------------------------*)
(* Check that variable names are all different, return false otherwise; warns if a variable is defined twice as the same type *)
(*------------------------------------------------------------*)
let check_variable_names variables_per_type constants =
	(* Function to warn if a variable is defined twice as the same type *)
	let warn_for_multiply_defined_variables list_of_variables =
		(* Compute the multiply defined variables *)
		let multiply_defined_variables = elements_existing_several_times list_of_variables in
		(* Print a warning for each of them *)
		List.iter (fun variable_name -> print_warning ("Multiply-declared variable '" ^ variable_name ^"'")) multiply_defined_variables;
	in

	(* Function to warn if a variable is also defined as a constant *)
	let different_from_constants l =
		try(
		List.iter (fun name ->
			if Hashtbl.mem constants name then (
				print_error ("Constant '" ^ name ^ "' is also defined as a variable.");
				raise False_exception;
			)
			) l;
		true
		) with False_exception -> false
	in
	
	(* Function to detect errors where a variable is defined as different types *)
	let error_for_multiply_defined_variables l1 l2 =
		let inter = list_inter l1 l2 in
		match inter with
		| [] -> true
		| _ -> List.iter (fun variable_name -> print_error ("The variable '" ^ variable_name ^ "' is defined twice as two different types.")) inter; false
	in

	(* Boolean flag to check whether all tests passed *)
	let all_correct = ref true in
	
	(* Maintain a list of already considered types to check equality with the yet to come types *)
	let types_for_comparison = ref [] in
	
	(* Iterate on all types *)
	List.iter (fun var_type -> 
		(* Get variables *)
		let list_of_variables_for_this_type = try (Hashtbl.find variables_per_type var_type) with Not_found -> [] in
		
		(* Check all variables are defined only once, and warn otherwise *)
		warn_for_multiply_defined_variables list_of_variables_for_this_type;
		
		(* Check all variables are different from constants *)
		if not (different_from_constants list_of_variables_for_this_type) then (all_correct := false);
		
		(* Compare with all previous types whether the variable is also defined for that type *)
		List.iter (fun former_type ->
			(* Get variables *)
			let list_of_variables_for_former_type = try (Hashtbl.find variables_per_type former_type) with Not_found -> [] in
			
			(* Compare *)
			if not (error_for_multiply_defined_variables list_of_variables_for_this_type list_of_variables_for_former_type) then (all_correct := false);
		) !types_for_comparison;
		
		(* Add this type for comparison with the yet to come types *)
		types_for_comparison := var_type :: !types_for_comparison;

	) all_parsed_types;

	(* Return the flag *)
	!all_correct


(*------------------------------------------------------------*)
(* Check that the names of automata are all different; return false otherwise *)
(*------------------------------------------------------------*)
let check_declared_automata_names automata_names =
	(* Compute the multiply defined variables *)
	let multiply_defined_names = elements_existing_several_times automata_names in
	(* Print an error for each of them *)
	match multiply_defined_names with
	| [] -> true
	| _ -> List.iter (fun variable_name -> print_error ("Several automata have name " ^ variable_name ^ ".")) multiply_defined_names; false


(*------------------------------------------------------------*)
(* Check that all locations of a given automaton are different *)
(*------------------------------------------------------------*)
let all_locations_different =
	(* Check for every automaton *)
	List.fold_left
		(fun all_different (automaton_name, _, locations) ->
		(* Get all the location names *)
		let locations =
			List.map (fun (location : parsed_location) -> location.name) locations in
		(* Look for multiply declared locations *)
		let multiply_declared_locations = elements_existing_several_times locations in
		List.iter (fun location_name -> print_error ("Several locations have name '" ^ location_name ^ "' in automaton '" ^ automaton_name ^ "'.")) multiply_declared_locations;
		if multiply_declared_locations = [] then all_different else false
		)
		true


(*
(*------------------------------------------------------------*)
(* Check that an update is well formed *)
(*------------------------------------------------------------*)
let check_update index_of_variables type_of_variables variable_names removed_variable_names constants automaton_name update =

	let check_update_normal (variable_name, parsed_rational_term) =
	match parsed_rational_term with
	| Parsed_continuous_term arithmetic_expression ->
		(* Check whether this variable is to be removed because unused elswhere than in resets *)
		let to_be_removed = List.mem variable_name removed_variable_names in

		(* Get the index of the variable *)
		let index, declared = try (Hashtbl.find index_of_variables variable_name, true)
		with Not_found -> (
			if to_be_removed then 0, true else (
				print_error ("The variable '" ^ variable_name ^ "' used in an update in automaton '" ^ automaton_name ^ "' was not declared."); 0, false
			)
			)
		in

		if not declared then false else (
		(* Only check the rest if the variable is not to be removed *)
		if to_be_removed then true else (
			(* Get the type of the variable *)
			print_message Verbose_total ("                Getting the type of the variable'" ^ variable_name ^ "'");

			let type_of_variable = try (type_of_variables index)
			with Invalid_argument comment -> (
				raise (InternalError ("The variable '" ^ variable_name ^ "' was not found in '" ^ automaton_name ^ "', although this has been checked before. OCaml says: " ^ comment ^ "."))
				) in

			print_message Verbose_total ("                Checking the type of the variable '" ^ variable_name ^ "'");
			match type_of_variable with
			(* Type clock: allow any linear term in updates: so just check that variables have been declared *)
			| AbstractModel.Var_type_clock ->
			print_message Verbose_total ("                A clock!");
			all_variables_defined_in_parsed_continuous_arithmetic_expression variable_names constants arithmetic_expression

			(* Case of a discrete var.: allow only an arithmetic expression of constants and discrete *)
			| AbstractModel.Var_type_discrete Rational ->
			print_message Verbose_total ("                A discrete rational!");
			let result = check_only_discretes_in_parsed_continuous_arithmetic_expression index_of_variables type_of_variables constants arithmetic_expression in
			if not result then
				(print_error ("The variable '" ^ variable_name ^ "' is a discrete rational and its update can only be an arithmetic expression over constants and discrete rationals in automaton '" ^ automaton_name ^ "'."); false)
			else (
				print_message Verbose_total ("                Check passed.");
				true
			)

			(* Type boolean: TODO *)
			| AbstractModel.Var_type_discrete Boolean ->
			print_message Verbose_total ("                A Boolean!");
			raise (NotImplemented "ModelConverter: Booleans in check_update")

			(* Case of a parameter: forbidden! *)
			| AbstractModel.Var_type_parameter -> print_error ("The variable '" ^ variable_name ^ "' is a parameter and cannot be updated in automaton '" ^ automaton_name ^ "'."); false
		)
		)
	
	| _ -> raise (NotImplemented "non-rational discrete in ModelConverter.check_update")
	
	in
	(* Print some information *)
	print_message Verbose_total ("              Checking one update");

	match update with
	| Normal_update update -> check_update_normal update
	| Condition_update (_, updates_if, updates_else) ->
		List.fold_left (fun acc u ->
			(check_update_normal u) && acc
		) true (updates_if @ updates_else)
*)

(*------------------------------------------------------------*)
(* Check that a sync is well formed *)
(*------------------------------------------------------------*)
let check_sync sync_name_list automaton_name = function
	| Sync sync_name ->  if not (List.mem sync_name sync_name_list) then (
		print_error ("The sync action '" ^ sync_name ^ "' used in automaton '" ^ automaton_name ^ "' was not declared for this automaton."); false)
		else true
	| NoSync -> true

(*------------------------------------------------------------*)
(* Check that a sync is used in all the automata where it is declared *)
(*------------------------------------------------------------*)
(*** WARNING: this function is inefficient! it is called for EACH action ***)
let synclab_used_everywhere automata synclab_name =
	(* Try to find the synclab in each of the automata where it is declared *)
	try(
		(* Check each automaton *)
		List.iter (fun (automaton_name, sync_name_list, locations) ->
			(* Only check if the synclab is declared here *)
			if List.mem synclab_name sync_name_list then(
			(* Check that at least one location contains the synclab *)
			if not (List.exists (fun (location : parsed_location) ->
				(* Check that at least one transition contains the synclab *)
				List.exists (fun transition -> transition.label = (Sync synclab_name)) location.transitions
				) locations ) then (
				(* No location contains the synclab: warning and exception (to save a bit of time) *)
				(*** TODO: perform exhaustive search, i.e., remove the exception mechanism ***)
				print_warning ("The synclab '" ^ synclab_name ^ "' is not used in (at least) the automaton '" ^ automaton_name ^ "' where it is declared: it will thus be removed from the whole model.");
				raise Not_found;
			);
			);
		) automata;
		(* The synclab was found everywhere: true *)
		true
		(* At least one automata does not use the synclab : false *)
	) with Not_found -> false


(*------------------------------------------------------------*)
(* Check that all variables mentioned in a list of stopwatches exist and are clocks *)
(*------------------------------------------------------------*)
let check_stopwatches index_of_variables type_of_variables stopwatches =
	let ok = ref true in
	List.iter (fun stopwatch ->
		(* Get variable name *)
		try (
			let variable_index = Hashtbl.find index_of_variables stopwatch in
			if type_of_variables variable_index != Var_type_clock then (
			print_error ("The variable '" ^ stopwatch ^ "' that should be stopped is not defined as a clock.");
			ok := false;
			);
		) with Not_found -> (
			print_error ("The variable '" ^ stopwatch ^ "' that should be stopped is not defined.");
			ok := false;
			);
		) stopwatches;
	!ok

(*
(*------------------------------------------------------------*)
(* Check that the automata are well-formed *)
(*------------------------------------------------------------*)
let check_automata useful_parsing_model_information automata =
	let constants				= useful_parsing_model_information.constants in
	let index_of_automata		= useful_parsing_model_information.index_of_automata in
	let index_of_variables		= useful_parsing_model_information.index_of_variables in
	let array_of_location_names	= useful_parsing_model_information.array_of_location_names in
	let removed_variable_names	= useful_parsing_model_information.removed_variable_names in
	let type_of_variables		= useful_parsing_model_information.type_of_variables in
	let variable_names			= useful_parsing_model_information.variable_names in

	let well_formed = ref true in

	(* Check each automaton *)
	List.iter (fun (automaton_name, sync_name_list, locations) ->
		print_message Verbose_total ("      Checking automaton " ^ automaton_name);
		(* Get the index of the automaton *)
		let index = try (Hashtbl.find index_of_automata automaton_name) with
			Not_found -> raise (InternalError ("Impossible to find the index of automaton '" ^ automaton_name ^ "'."))
		in
		(* Check each location *)
		List.iter (fun (location : parsed_location) ->
			print_message Verbose_total ("        Checking location " ^ location.name);
			(* Check that the location_name exists (which is obvious) *)
			if not (in_array location.name array_of_location_names.(index)) then(
				print_error ("The location '" ^ location.name ^ "' declared in automaton '" ^ automaton_name ^ "' does not exist.");
				well_formed := false);

			(* Check the cost *)
			begin
				match location.cost with
				| Some cost ->
				print_message Verbose_total ("          Checking cost");
				if not (all_variables_defined_in_linear_expression variable_names constants cost) then well_formed := false;
				| None -> ()
			end;

			(* Check the stopwatches *)
			print_message Verbose_total ("          Checking possible stopwatches");
			if not (check_stopwatches index_of_variables type_of_variables location.stopped) then well_formed := false;


			(* Check the convex predicate *)

			(*** TODO: preciser quel automate et quelle location en cas d'erreur ***)

			print_message Verbose_total ("          Checking convex predicate");
			if not (all_variables_defined_in_convex_predicate variable_names constants location.invariant) then well_formed := false;


			(* Check transitions *)
			print_message Verbose_total ("          Checking transitions");
			List.iter (fun (convex_predicate, updates, sync, target_location_name) ->
				(* Check the convex predicate *)
				print_message Verbose_total ("            Checking convex predicate");
				if not (all_variables_defined_in_convex_predicate variable_names constants convex_predicate) then well_formed := false;
				(* Check the updates *)
				print_message Verbose_total ("            Checking updates");
				List.iter (fun update -> if not (check_update index_of_variables type_of_variables variable_names removed_variable_names constants automaton_name update) then well_formed := false) updates;
				(* Check the sync *)
				print_message Verbose_total ("            Checking sync name ");
				if not (check_sync sync_name_list automaton_name sync) then well_formed := false;
				(* Check that the target location exists for this automaton *)
				if not (in_array target_location_name array_of_location_names.(index)) then(
					print_error ("The target location '" ^ target_location_name ^ "' used in automaton '" ^ automaton_name ^ "' does not exist.");
					well_formed := false);
				) location.transitions;
			) locations;
		) automata;

	(* Return whether the automata passed the tests *)
	!well_formed
*)
(*
(*------------------------------------------------------------*)
(* Check that the init_definition are well-formed *)
(*------------------------------------------------------------*)
let check_init useful_parsing_model_information init_definition observer_automaton =
	let array_of_location_names	= useful_parsing_model_information.array_of_location_names in
	let automata				= useful_parsing_model_information.automata in
	let automata_names			= useful_parsing_model_information.automata_names in
	let constants				= useful_parsing_model_information.constants in
	let discrete				= useful_parsing_model_information.discrete in
	let index_of_automata		= useful_parsing_model_information.index_of_automata in
	let index_of_variables		= useful_parsing_model_information.index_of_variables in
	let type_of_variables		= useful_parsing_model_information.type_of_variables in
	let variable_names			= useful_parsing_model_information.variable_names in
	let removed_variable_names	= useful_parsing_model_information.removed_variable_names in

	let well_formed = ref true in
	(* Check that (automaton / location / variable) names exist in each predicate *)
	List.iter (function
		| Parsed_loc_assignment (automaton_name, location_name) ->
			(* Check that the automaton_name exists *)
			let index, exists = try (Hashtbl.find index_of_automata automaton_name, true) with
				Not_found -> (print_error ("The automaton '" ^ automaton_name ^ "' mentioned in the init definition does not exist."); well_formed := false; 0, false) in
			(* Check that the location_name exists (only if the automaton_name exists) *)
			if exists && not (in_array location_name array_of_location_names.(index)) then (
			print_error ("The location '" ^ location_name ^ "' mentioned in the init definition does not exist in automaton '" ^ automaton_name ^ "'."); well_formed := false
			)
		| Parsed_linear_predicate linear_constraint ->
			begin
			(*** NOTE: do not check linear constraints made of a variable to be removed compared to a linear term ***)
			match linear_constraint with
			| Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , linear_expression) when List.mem variable_name removed_variable_names ->
				print_message Verbose_total ("Variable '" ^ variable_name ^ "' is compared to a linear term, but will be removed: no check." );
				(* Still check the second term *)
				if not (all_variables_defined_in_linear_expression variable_names constants linear_expression) then well_formed := false;
				(* General case: check *)
			| _ -> if not (all_variables_defined_in_linear_constraint variable_names constants linear_constraint) then well_formed := false;
			end
		) init_definition;

	(* Get all the Parsed_loc_assignment *)
	let loc_assignments, init_inequalities = List.partition (function
		| Parsed_loc_assignment _ -> true
		| Parsed_linear_predicate _ -> false
		) init_definition in
	(* Make pairs (automaton_name, location_name) *)
	let initial_locations = List.map (function
		| Parsed_loc_assignment (automaton_name, location_name) -> (automaton_name, location_name)
		| _ -> raise (InternalError "Something else than a Parsed_loc_assignment was found in a Parsed_loc_assignment list")
		) loc_assignments in

	(* Check that every automaton is given at most one initial location *)
	let init_locations_for_automata = Hashtbl.create (List.length automata) in
	List.iter (fun (automaton_name, location_name) ->
		(* Check if this automaton was already given an initial location before *)
		if Hashtbl.mem init_locations_for_automata automaton_name then(
			(* Get the initial location already declared previously *)
			let previous_location = Hashtbl.find init_locations_for_automata automaton_name in
			(* If identical : only warns *)
			if location_name = previous_location then (
			print_warning ("The automaton '" ^ automaton_name ^ "' is assigned twice the initial location '" ^ location_name ^ "' in the init definition.");
			(* If different : error *)
			) else (
			print_error ("The automaton '" ^ automaton_name ^ "' is assigned several different locations in the init definition.");
			well_formed := false;
			);
			(* If not already given : add it *)
		) else (
			Hashtbl.add init_locations_for_automata automaton_name location_name;
		);
		) initial_locations;
	(* Check that every automaton is given at least one initial location *)
	List.iter (fun automaton_index ->
		let is_observer i = match observer_automaton with
			| None -> false
			| Some observer_id -> i = observer_id
		in
		(* No check for the observer (will be added later) *)
		if not (is_observer automaton_index) then (
			(* Get the name *)
			let automaton_name = automata_names automaton_index in
			(* Look for it in the hash table *)
			if not (Hashtbl.mem init_locations_for_automata automaton_name) then (
			(* Error *)
			print_error ("The automaton '" ^ automaton_name ^ "' is not given any initial location in the init definition.");
			well_formed := false;
			);
		);
		) automata;

	(* Remove the inequalities of which the left-hand term is a removed variable *)
	let filtered_init_inequalities = List.filter (function
		| Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , _)) ->
			(* Filter out if the left-hand is in the removed variable names *)
			not (List.mem variable_name removed_variable_names)
		(* Any other combination is OK *)
		| _ ->
			true
		) init_inequalities
	in

	(* Partition the init inequalities between the discrete init assignments, and other inequalities *)
	let rational_init, other_inequalities = List.partition (function
		(* Check if the left part is only a variable name *)
		| Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , _)) ->
			let is_discrete =
			(* Try to get the variable index *)
			if (Hashtbl.mem index_of_variables variable_name) then (
				let variable_index =  Hashtbl.find index_of_variables variable_name in
				(* Keep if this is a discrete *)
				match type_of_variables variable_index with | Var_type_discrete _ -> true | _ -> false
			) else (
				(* Case constant *)
				if (Hashtbl.mem constants variable_name) then false
				else (
				(* Otherwise: problem! *)
				raise (InternalError ("The variable '" ^ variable_name ^ "' mentioned in the init definition does not exist."));
				))
			in is_discrete
		(* Otherwise false *)
		| _ -> false
		) filtered_init_inequalities in

	(* Check that every discrete variable is given only one (rational) initial value *)
	let init_values_for_discrete = Hashtbl.create (List.length discrete) in
	List.iter (fun lp ->
		match lp with
		| Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (coeff, rational_name)), op , expression)) ->
			if NumConst.neq coeff NumConst.one then (
			print_error ("The discrete variable '" ^ rational_name ^ "' must have a coeff 1 in the init definition.");
			well_formed := false;
			);
			(* Check if the assignment is well formed, and keep the discrete value *)
			let rational_value =
			match (op, expression) with
			(* Simple constant: OK *)
			| (PARSED_OP_EQ, Linear_term (Constant c)) -> c
			(* Constant: OK *)
			| (PARSED_OP_EQ, Linear_term (Variable (coef, variable_name))) ->
				(* Get the value of  the variable *)
				let value = Hashtbl.find constants variable_name in
				NumConst.mul coef value
			| _ -> print_error ("The initial value for discrete variable '" ^ rational_name ^ "' must be given in the form '" ^ rational_name ^ " = c', where c is an integer, a rational or a constant.");
				well_formed := false;
				NumConst.zero
			in
			(* Get the variable index *)
			let discret_index =  Hashtbl.find index_of_variables rational_name in
			(* Check if it was already declared *)
			if Hashtbl.mem init_values_for_discrete discret_index then(
			print_error ("The discrete variable '" ^ rational_name ^ "' is given an initial value several times in the init definition.");
			well_formed := false;
			) else (
			(* Else add it *)
			Hashtbl.add init_values_for_discrete discret_index rational_value;
			);
		| _ -> raise (InternalError ("Must have this form since it was checked before."))
		) rational_init;

	(* Check that every discrete variable is given at least one (rational) initial value (if not: warns) *)
	List.iter (fun rational_index ->
		if not (Hashtbl.mem init_values_for_discrete rational_index) then(
			print_warning ("The discrete variable '" ^ (List.nth variable_names rational_index) ^ "' was not given an initial value in the init definition: it will be assigned to 0.");
			Hashtbl.add init_values_for_discrete rational_index NumConst.zero
		);
		) discrete;

	(* Convert the Hashtbl to pairs (rational_index, init_value) *)
	let rational_values_pairs =
		List.map (fun rational_index ->
			rational_index, Hashtbl.find init_values_for_discrete rational_index
		) discrete
	in

	(* Check that no discrete variable is used in other inequalities (warns if yes) *)
	(**** TO DO ****) (*use 'other_inequalities' *)

	(* Return whether the init declaration passed the tests *)
	rational_values_pairs, !well_formed*)



(************************************************************)
(** Converting the model *)
(************************************************************)



(*------------------------------------------------------------*)
(* Convert a ParsingStructure.tile_nature into a Result.tile_nature *)
(*------------------------------------------------------------*)
(*** TODO: this part has nothing to do with model conversion and should preferably go elsewhere… ***)
(*let convert_tile_nature = function
  | ParsingStructure.Good -> StateSpace.Good
  | ParsingStructure.Bad -> StateSpace.Bad
  | ParsingStructure.Unknown -> StateSpace.Unknown*)

  

(*------------------------------------------------------------*)
(* Create the hash table of constants ; check the validity on-the-fly *)
(*------------------------------------------------------------*)
let make_constants constants =
  (* Create hash table *)
  let constants_hashtable : (string, NumConst.t) Hashtbl.t = Hashtbl.create (List.length constants) in
  (* Manage Boolean for checking errors *)
  let correct = ref true in
  List.iter (fun (name, value) ->
      if (Hashtbl.mem constants_hashtable name) then (
        let old_value = Hashtbl.find constants_hashtable name in
        (* If same: warning *)
        if(NumConst.equal old_value value) then(
          print_warning ("Constant '" ^ name ^ "' is defined twice.");
        )else(
          (* If different: error *)
          print_error ("Constant '" ^ name ^ "' is given different values.");
          correct := false;
        );
      )else(
        (* Otherwise: add it *)
        Hashtbl.add constants_hashtable name value;
      );
    ) constants;
  (* Return hash table *)
  constants_hashtable, !correct


(*------------------------------------------------------------*)
(* Translate the discrete types *)
(*------------------------------------------------------------*)

let abstract_rational_type_of_parsed_rational_type = function
	| Parsed_boolean	-> Boolean
	| Parsed_rational	-> Rational
	| Parsed_string		-> String
	| Parsed_stringset	-> StringSet

(*let parsed_rational_type_of_abstract_rational_type = function
	| Rational	-> Parsed_rational
	| Boolean	-> Parsed_boolean*)


(*------------------------------------------------------------*)
(* Get the exact type of a discrete variable *)
(*------------------------------------------------------------*)

exception Found_type of parsed_var_type_discrete

let get_type_of_rational_variable (variable_names : variable_name array) (variables_per_type :  (parsed_var_type , variable_name list) Hashtbl.t) (variable_index : variable_index) =
	(* Get the name of the variable *)
	let variable_name = variable_names.(variable_index) in
	
	(*** BADPROG: check for every type if we find the type (perhaps implement something more efficient??) ***)
	try(
	List.iter (fun (rational_type : ParsingStructure.parsed_var_type_discrete) : unit ->
		(* Get all variables for this type *)
		let all_variables_for_this_type : variable_name list = try (Hashtbl.find variables_per_type (Parsed_var_type_discrete rational_type)) with Not_found -> [] in
		
		(* Check whether our variable belongs to this list *)
		if List.mem variable_name all_variables_for_this_type then(
			(* If yes, stop *)
			raise (Found_type rational_type)
		);
		()
	
	) all_concrete_parsed_rational_types;
	
	(* If still there: variable not found, impossible situation *)
	raise (InternalError ("Discrete type not found for variable " ^ variable_name ^ " although it should have been checked before."));
	
	) with Found_type rational_type -> (
		(* Convert parsed type to abstract type *)
		abstract_rational_type_of_parsed_rational_type rational_type
	)
	

(*------------------------------------------------------------*)
(* Get all the declared actions for every automaton *)
(*------------------------------------------------------------*)
let make_actions_per_automaton index_of_actions index_of_automata automata =
  (* Create an empty array for every automaton *)
  let actions_per_automaton = Array.make (List.length automata) [] in
  (* Fill it *)
  List.iter (fun (automaton_name, sync_name_list, _) ->
      (* Get the index of the automaton *)
      let automaton_index = Hashtbl.find index_of_automata automaton_name in
      (* Update the array *)
      actions_per_automaton.(automaton_index) <-
        List.map (fun sync_name ->
            (* Get the index of the action *)
            let action_index = Hashtbl.find index_of_actions sync_name in
            (* Return the action index *)
            action_index
          ) sync_name_list;
    ) automata;
  (* Return the array *)
  actions_per_automaton


(*------------------------------------------------------------*)
(* Get all the locations for every automaton *)
(*------------------------------------------------------------*)
let make_locations_per_automaton index_of_automata parsed_automata nb_automata =
  (* Create an empty array for every automaton *)
  let locations_per_automaton = Array.make nb_automata (Array.make 0 "") in
  (* For each automaton: *)
  List.iter
    (fun (automaton_name, _, transitions) ->
       (* Get the index of the automaton *)
       let index = try(Hashtbl.find index_of_automata automaton_name)
         with Not_found -> raise (InternalError ("Automaton name '" ^ automaton_name ^ "' not found in function 'make_locations_per_automaton' although this had been checked before."))
       in
       (* Get the location names *)
       let location_names = List.map (fun (location : parsed_location) -> location.name) transitions in
       (* Update the array *)
       locations_per_automaton.(index) <- Array.of_list location_names
    )
    parsed_automata;
  (* Return the array *)
  locations_per_automaton




(*(*------------------------------------------------------------*)
  (* Convert the costs *)
  (*** TODO: do not call if actually no cost! ***)
  (*------------------------------------------------------------*)
  (* Convert the structure: 'automaton_index -> location_index -> ParsingStructure.linear_expression' into a structure: 'automaton_index -> location_index -> Constraint.linear_expression' *)
  let convert_costs index_of_variables constants costs =
  	(* Convert for each automaton *)
  	let costs = Array.map (
  		(* Convert for each location *)
  		Array.map (
  			function
  			| None -> None
  			| Some cost -> Some (linear_term_of_linear_expression index_of_variables constants cost)
  		)
  	) costs in
  	(* Functional representation *)
  	fun automaton_index location_index -> costs.(automaton_index).(location_index)*)


(*(*------------------------------------------------------------*)
  (* Convert the invariants *)
  (*------------------------------------------------------------*)
  (* Convert the structure: 'automaton_index -> location_index -> ParsingStructure.convex_predicate' into a structure: 'automaton_index -> location_index -> Constraint.linear_constraint' *)
  let convert_invariants index_of_variables constants invariants =
  	(* Convert for each automaton *)
  	let invariants = Array.map (
  		(* Convert for each location *)
  		Array.map (linear_constraint_of_convex_predicate index_of_variables constants)
  	) invariants in
  	(* Functional representation *)
  	fun automaton_index location_index -> invariants.(automaton_index).(location_index)*)

(*
(*------------------------------------------------------------*)
(* Get all the possible actions for every location of every automaton *)
(*------------------------------------------------------------*)
let make_automata useful_parsing_model_information parsed_automata (with_observer_action : bool) =
	let constants				= useful_parsing_model_information.constants in
	let index_of_actions		= useful_parsing_model_information.index_of_actions in
	let index_of_automata		= useful_parsing_model_information.index_of_automata in
	let index_of_locations		= useful_parsing_model_information.index_of_locations in
	let index_of_variables		= useful_parsing_model_information.index_of_variables in
	let actions					= useful_parsing_model_information.actions in
	let removed_action_names	= useful_parsing_model_information.removed_action_names in

	(* Number of automata *)
	let nb_automata = Hashtbl.length index_of_automata in
	(* Create an empty array for the actions of every automaton *)
	let actions_per_automaton = Array.make nb_automata [] in
	(* Create an empty array for the actions of every location of every automaton *)
	let actions_per_location = Array.make nb_automata (Array.make 0 []) in
	(* Create an empty array for the actions of every location of every automaton *)
	let location_urgency = Array.make nb_automata (Array.make 0 Location_nonurgent) in
	(* Create an empty array for the actions of every location of every automaton *)
	let location_acceptance = Array.make nb_automata (Array.make 0 Location_nonaccepting) in
	(* Create an empty array for the costs *)
	let costs = Array.make nb_automata (Array.make 0 None) in
	(* Create an empty array for the transitions *)
	let transitions = Array.make nb_automata (Array.make 0 []) in
	(* Create an empty array for the invariants *)
	let invariants = Array.make nb_automata (Array.make 0 (LinearConstraint.pxd_false_constraint ())) in
	(* Create an empty array for the invariants *)
	let stopwatches_array = Array.make nb_automata (Array.make 0 []) in
	(* Does the model has any stopwatch? *)
	let has_stopwatches = ref false in
	(* Maintain the index of no_sync *)
	let no_sync_index = ref (Array.length actions) in

	(* For each automaton (except the observer, if any): *)
	List.iter
		(fun (automaton_name, _, locations) ->
		(* Get the index of the automaton *)
		print_message Verbose_total ("    - Building automaton " ^ automaton_name);
		let automaton_index = try (Hashtbl.find index_of_automata automaton_name) with Not_found -> raise (InternalError ("Impossible to find the index of automaton '" ^ automaton_name ^ "'.")) in
		(* Get the number of locations *)
		let nb_locations = List.length locations in
		(* Create the array of lists of actions for this automaton *)
		actions_per_location.(automaton_index) <- Array.make nb_locations [];
		(* Create the array of urgent locations for this automaton (default: non-urgent) *)
		location_urgency.(automaton_index) <- Array.make nb_locations Location_nonurgent;
		(* Create the array of accepting locations for this automaton (default: non-accepting) *)
		location_acceptance.(automaton_index) <- Array.make nb_locations Location_nonaccepting;
		(* Create the array of costs for this automaton *)
		costs.(automaton_index) <- Array.make nb_locations None;
		(* Create the array of list of transitions for this automaton *)
		transitions.(automaton_index) <- Array.make nb_locations [];
		(* Create the array of invariants for this automaton *)
		invariants.(automaton_index) <- Array.make nb_locations (LinearConstraint.pxd_false_constraint ());
		(* Create the array of stopwatches for this automaton *)
		stopwatches_array.(automaton_index) <- Array.make nb_locations [];

		(* For each location: *)
		List.iter
			(fun (location : parsed_location) ->
				(* Get the index of the location *)
				let location_index = try (Hashtbl.find index_of_locations.(automaton_index) location.name) with Not_found -> raise (InternalError ("Impossible to find the index of location '" ^ location.name ^ "'.")) in

				(* Create the list of actions for this location, by iterating on parsed_transitions *)
				let list_of_actions, list_of_transitions =  List.fold_left (fun (current_list_of_actions, current_list_of_transitions) (guard, updates, sync, target_location_name) ->
					(* Get the index of the target location *)
					let target_location_index = try (Hashtbl.find index_of_locations.(automaton_index) target_location_name) with Not_found -> raise (InternalError ("Impossible to find the index of location '" ^ target_location_name ^ "'.")) in
					(* Depend on the action type *)
					match sync with
					| ParsingStructure.Sync action_name ->
					(* If the 'sync' is within the removed actions, do nothing *)
					if List.mem action_name removed_action_names then (
						current_list_of_actions, current_list_of_transitions
						(* Else : *)
					) else (
						(* Get the action index *)
						let action_index =
						try (Hashtbl.find index_of_actions action_name) with Not_found -> raise (InternalError ("Impossible to find the index of action '" ^ action_name ^ "'."))
						in
						(* Compute the list of actions *)
						(action_index :: current_list_of_actions)
						,
						(* Compute the list of transitions *)
						((action_index, guard, updates, target_location_index) :: current_list_of_transitions)
					)
					| ParsingStructure.NoSync ->
					(* Get the action index *)
					let action_index = !no_sync_index in
					(* Increment the number of nosync indexes *)
					no_sync_index := !no_sync_index + 1;
					(* Compute the list of actions *)
					(action_index :: current_list_of_actions)
					,
					(* Compute the list of transitions *)
					((action_index, guard, updates, target_location_index) :: current_list_of_transitions)
				) ([], []) location.transitions in

				(* Update the array of actions per location *)
				actions_per_location.(automaton_index).(location_index) <- (List.rev (list_only_once list_of_actions));

				(* Update the array of costs per location *)
				begin
				match location.cost with
				| Some cost ->
					costs.(automaton_index).(location_index) <- Some (
						LinearConstraint.cast_p_of_pxd_linear_term
						(linear_term_of_linear_expression index_of_variables constants cost)
						true
					);
				| None -> ()
				end;

				(* Update the array of urgency *)
				let urgency =
				match location.urgency with
				| Parsed_location_urgent -> Location_urgent
				| Parsed_location_nonurgent -> Location_nonurgent
				in
				location_urgency.(automaton_index).(location_index) <- urgency;

				(* Update the array of acceptance *)
				let acceptance =
				match location.acceptance with
				| Parsed_location_accepting -> Location_accepting
				| Parsed_location_nonaccepting -> Location_nonaccepting
				in
				location_acceptance.(automaton_index).(location_index) <- acceptance;

				(* Update the array of transitions per location *)
				transitions.(automaton_index).(location_index) <- (List.rev list_of_transitions);

				(* Update the array of invariants *)
				invariants.(automaton_index).(location_index) <- linear_constraint_of_convex_predicate index_of_variables constants location.invariant;

				(* Does the model has stopwatches? *)
				if location.stopped != [] then has_stopwatches := true;
				(* Convert the stopwatches names into variables *)
				let list_of_stopwatch_names = list_only_once location.stopped in
				(* Update the array of stopwatches *)
				stopwatches_array.(automaton_index).(location_index) <- List.map (fun stopwatch_index ->
					Hashtbl.find index_of_variables stopwatch_index
				)
					list_of_stopwatch_names;

			) locations;
		(* Update the array of actions per automaton *)
		let all_actions_for_this_automaton = Array.fold_left (fun list_of_all_actions list_of_actions ->
			list_union list_of_all_actions list_of_actions
			) [] actions_per_location.(automaton_index) in
		actions_per_automaton.(automaton_index) <- all_actions_for_this_automaton
		) parsed_automata;


	(* Create the array of action names; add 1 no_sync for the observer, if any *)
	let nb_actions = !no_sync_index + (if with_observer_action then 1 else 0) in
	let array_of_action_names = Array.make nb_actions "" in
	(* Create the array of action types (sync / no_sync) *)
	let array_of_action_types = Array.make nb_actions Action_type_sync in
	(* Fill the sync actions *)
	for i = 0 to (Array.length actions) - 1 do
		array_of_action_names.(i) <- actions.(i);
	done;
	(* Fill the no sync actions *)
	for i = Array.length actions to nb_actions - 1 - (if with_observer_action then 1 else 0) do
		array_of_action_names.(i) <- ("nosync_" ^ (string_of_int (i - (Array.length actions) + 1)));
		array_of_action_types.(i) <- Action_type_nosync;
	done;
	(* Fill the array for the observer no_sync *)
	if with_observer_action then(
		array_of_action_names.(nb_actions - 1) <- ("nosync_obs");
		array_of_action_types.(nb_actions - 1) <- Action_type_nosync;
	);

	(* Create the action list *)
	let actions = list_of_interval 0 (nb_actions - 1) in

	(* Return all the structures in a functional representation *)
	actions, array_of_action_names, array_of_action_types, actions_per_automaton, actions_per_location, location_acceptance, location_urgency, costs, invariants, stopwatches_array, !has_stopwatches, transitions, (if with_observer_action then Some (nb_actions - 1) else None)
*)


(*------------------------------------------------------------*)
(* Get the automata for every action *)
(*------------------------------------------------------------*)
let make_automata_per_action actions_per_automaton nb_automata nb_actions =
  (* Create an empty array for actions *)
  let automata_per_action = Array.make nb_actions [] in
  (* For all automaton *)
  for automaton_index = 0 to nb_automata - 1 do
    (* For all action *)
    List.iter (fun action_index ->
        (* Add the automaton_index to the action_index *)
        automata_per_action.(action_index) <- automaton_index :: automata_per_action.(action_index);
      ) (actions_per_automaton automaton_index);
  done;
  (* Reverse it (not so important…) and return it *)
  let automata_per_action = Array.map List.rev automata_per_action in
  (* Return a functional representation *)
  fun automaton_index -> automata_per_action.(automaton_index)

(*
(*------------------------------------------------------------*)
(** Split between the discrete and continuous inequalities of a convex predicate; raises False_exception if a false linear expression is found *)
(*------------------------------------------------------------*)
let split_convex_predicate_into_rational_and_continuous index_of_variables type_of_variables constants convex_predicate =
  (* Compute a list of inequalities *)
  List.partition
    (fun linear_inequality ->
       match linear_inequality with
       | Parsed_true_constraint -> true (*** NOTE: we arbitrarily send "true" to the discrete part ***)
       | Parsed_false_constraint -> raise False_exception
       | Parsed_linear_constraint (linexpr1, _, linexpr2) -> only_rational_in_linear_expression index_of_variables type_of_variables constants linexpr1 && only_rational_in_linear_expression index_of_variables type_of_variables constants linexpr2
    ) convex_predicate
*)

(*(*------------------------------------------------------------*)
(* Convert a guard *)
(*------------------------------------------------------------*)
let convert_guard index_of_variables type_of_variables constants guard_convex_predicate =

  try(
    (* Separate the guard into a discrete guard (on discrete variables) and a continuous guard (on all variables) *)
    let discrete_guard_convex_predicate, continuous_guard_convex_predicate = split_convex_predicate_into_rational_and_continuous index_of_variables type_of_variables constants guard_convex_predicate in

    match discrete_guard_convex_predicate, continuous_guard_convex_predicate with
    (* No inequalities: true *)
    | [] , [] -> True_guard
    (* Only discrete inequalities: discrete *)
    | discrete_guard_convex_predicate , [] -> Discrete_guard (LinearConstraint.cast_d_of_pxd_linear_constraint (verbose_mode_greater Verbose_low) (linear_constraint_of_convex_predicate index_of_variables constants discrete_guard_convex_predicate))
    (* Only continuous inequalities: continuous *)
    | [] , continuous_guard_convex_predicate -> Continuous_guard (linear_constraint_of_convex_predicate index_of_variables constants continuous_guard_convex_predicate)
    (* Otherwise: both *)
    | discrete_guard_convex_predicate , continuous_guard_convex_predicate ->
      (* Convert both parts *)
      let discrete_guard = LinearConstraint.cast_d_of_pxd_linear_constraint (verbose_mode_greater Verbose_low) (linear_constraint_of_convex_predicate index_of_variables constants discrete_guard_convex_predicate) in
      let continuous_guard = linear_constraint_of_convex_predicate index_of_variables constants continuous_guard_convex_predicate in

      (*** NOTE: try to simplify a bit if possible (costly, but would save a lot of time later if checks are successful) ***)
      let intersection = LinearConstraint.pxd_intersection_with_d continuous_guard discrete_guard in

      if LinearConstraint.pxd_is_true intersection then True_guard
      else if LinearConstraint.pxd_is_false intersection then False_guard
      else
        (* Else create mixed guard as planned *)
        Discrete_continuous_guard
          {
            discrete_guard		= discrete_guard;
            continuous_guard	= continuous_guard;
          }

    (* If some false construct found: false guard *)
  ) with False_exception -> False_guard
*)

(*------------------------------------------------------------*)
(* Convert updates *)
(*------------------------------------------------------------*)

(** Checks if a update is a normal update *)
let is_normal_update = function
  | Normal_update _ -> true
  | _ -> false

(** Returns the value of a normal update *)
let get_normal_update_value = function
  | Normal_update u -> u
  | _ -> assert false

(** Returns the value of a conditonal update *)
let get_conditional_update_value = function
  | Condition_update u -> u
  | _ -> assert false


(*
(*** TODO (though really not critical): try to do some simplifications… ***)
(*** NOTE: define a top-level function to avoid recursive passing of all common variables ***)
let linear_term_of_parsed_continuous_arithmetic_expression index_of_variables constants pdae =
	(* Create an array of coef *)
	let array_of_coef = Array.make (Hashtbl.length index_of_variables) NumConst.zero in
	(* Create a zero constant *)
	let constant = ref NumConst.zero in

	let rec update_coef_array_in_parsed_continuous_arithmetic_expression mult_factor = function
		| Parsed_CAE_plus (parsed_continuous_arithmetic_expression, parsed_continuous_term) ->
		(* Update coefficients in the arithmetic expression *)
		update_coef_array_in_parsed_continuous_arithmetic_expression mult_factor parsed_continuous_arithmetic_expression;
		(* Update coefficients in the term *)
		update_coef_array_in_parsed_continuous_term mult_factor parsed_continuous_term;
		| Parsed_CAE_minus (parsed_continuous_arithmetic_expression, parsed_continuous_term) ->
		(* Update coefficients in the arithmetic expression *)
		update_coef_array_in_parsed_continuous_arithmetic_expression mult_factor parsed_continuous_arithmetic_expression;
		(* Update coefficients in the term: multiply by -1 for negation *)
		update_coef_array_in_parsed_continuous_term (NumConst.neg mult_factor) parsed_continuous_term;
		| Parsed_CAE_term parsed_continuous_term ->
		update_coef_array_in_parsed_continuous_term mult_factor parsed_continuous_term;

	and update_coef_array_in_parsed_continuous_term mult_factor = function
		(* Multiplication is only allowed with a constant multiplier *)
		| Parsed_CT_mul (parsed_continuous_term, parsed_continuous_factor) ->
		(* Valuate the term *)
		let valued_term = valuate_parsed_continuous_term constants parsed_continuous_term in
		(* Update coefficients *)
		update_coef_array_in_parsed_continuous_factor (NumConst.mul valued_term mult_factor) parsed_continuous_factor

		| Parsed_CT_div (parsed_continuous_term, parsed_continuous_factor) ->
		(* Valuate the discrete factor *)
		let valued_factor = valuate_parsed_continuous_factor constants parsed_continuous_factor in
		(* Update coefficients *)
		update_coef_array_in_parsed_continuous_term (NumConst.div mult_factor valued_factor) parsed_continuous_term

		| Parsed_CT_factor parsed_continuous_factor ->
		update_coef_array_in_parsed_continuous_factor mult_factor parsed_continuous_factor

	and update_coef_array_in_parsed_continuous_factor mult_factor = function
		| Parsed_CF_variable variable_name ->
			(* Try to find the variable_index *)
			if Hashtbl.mem index_of_variables variable_name then (
				let variable_index = Hashtbl.find index_of_variables variable_name in
				(* Update the array *)
				array_of_coef.(variable_index) <- NumConst.add array_of_coef.(variable_index) (mult_factor);
				(* Try to find a constant *)
			) else (
				if Hashtbl.mem constants variable_name then (
				(* Retrieve the value of the global constant *)
				let value = Hashtbl.find constants variable_name in
				(* Update the constant *)
				constant := NumConst.add !constant (NumConst.mul mult_factor value)
				) else (
				raise (InternalError ("Impossible to find the index of variable '" ^ variable_name ^ "' in function 'linear_term_of_parsed_continuous_arithmetic_expression' although this should have been checked before."))
				)
			)
		| Parsed_CF_constant var_value ->
			(* Update the constant *)
			constant := NumConst.add !constant (NumConst.mul mult_factor var_value)
		| Parsed_CF_unary_min parsed_rational_factor ->
			update_coef_array_in_parsed_continuous_factor mult_factor parsed_rational_factor
		| Parsed_CF_expression parsed_continuous_arithmetic_expression ->
			update_coef_array_in_parsed_continuous_arithmetic_expression mult_factor parsed_continuous_arithmetic_expression
	in

	(* Call the recursive function updating the coefficients *)
	update_coef_array_in_parsed_continuous_arithmetic_expression NumConst.one pdae;

	(* Create the linear term *)
	linear_term_of_array array_of_coef !constant
*)


  
(* Filter the updates that should assign some variable name to be removed to any expression *)
let filtered_updates removed_variable_names updates =
  let not_removed_variable (variable_name, _) =
    not (List.mem variable_name removed_variable_names)
  in
  List.fold_left (fun acc u ->
      match u with
      | Normal_update (update) ->
        if (not_removed_variable update) then u::acc else acc
      | Condition_update (bool, updates_if, updates_else) ->
        let filtered_if = List.filter (not_removed_variable) updates_if in
        let filtered_else = List.filter (not_removed_variable) updates_else in
        Condition_update (bool, filtered_if, filtered_else)::acc
    ) [] updates


(*(** Translate a parsed discrete update into its abstract model *)
let to_abstract_discrete_update index_of_variables constants (variable_name, (parsed_discrete_term : parsed_discrete_term)) : discrete_update =
	let variable_index = Hashtbl.find index_of_variables variable_name in
	match parsed_discrete_term with
	| Parsed_continuous_term parsed_continuous_arithmetic_expression ->
		let arithmetic_expression = rational_arithmetic_expression_of_parsed_continuous_arithmetic_expression index_of_variables constants parsed_continuous_arithmetic_expression in
		(variable_index, Rational_term arithmetic_expression)
	| _ -> raise (NotImplemented "non-rational discrete terms in ModelConverter.to_abstract_discrete_update")


(** Translate a parsed clock update into its abstract model *)
let to_abstract_clock_update index_of_variables constants only_resets updates_list =

	(** Translate parsed clock updte into the tuple clock_index, linear_term *)
	let to_intermediate_abstract_clock_update (variable_name, parsed_rational_term) =
		match parsed_rational_term with
		| Parsed_continuous_term parsed_continuous_arithmetic_expression ->
			let variable_index = Hashtbl.find index_of_variables variable_name in
			let linear_term = linear_term_of_parsed_continuous_arithmetic_expression index_of_variables constants parsed_continuous_arithmetic_expression in
			(variable_index, linear_term)
		| Parsed_string_term _ -> raise (InternalError "Parsed_string_term found in ModelConverter.to_abstract_clock_update but this should not have happened for a clock update")
	in

	let converted_clock_updates = List.map to_intermediate_abstract_clock_update updates_list in

	(* Differentiate between different kinds of clock updates *)
	let clock_updates : clock_updates =
		(* Case 1: no update *)
		if converted_clock_updates = [] then No_update
		else (
		(* Case 2: resets only *)
		if !only_resets then (
			(* Keep only the clock ids, not the linear terms *)
			let clocks_to_reset, _ = List.split converted_clock_updates in
			Resets (List.rev clocks_to_reset)
		) else
			(* Case 3: complex with linear terms *)
			Updates (List.rev converted_clock_updates)
		)
	in

	(** abstract clock updates *)
	clock_updates

(** Split normal updates into clock, discrete updates *)
let split_to_clock_discrete_updates index_of_variables only_resets type_of_variables updates =
  (** Check if a normal update is a clock update *)
	let is_clock_update (variable_name, parsed_rational_term) =
		match parsed_rational_term with
		| Parsed_continuous_term parsed_continuous_arithmetic_expression ->
			(* Retrieve variable type *)
			if type_of_variables (Hashtbl.find index_of_variables variable_name) = Var_type_clock then (
				(* Update flag *)
				if parsed_continuous_arithmetic_expression <> Parsed_CAE_term (Parsed_CT_factor (Parsed_CF_constant NumConst.zero)) then (
					only_resets := false;
				);
				true
			) else
			false
		(* A string term cannot be assigned to a clock *)
		| Parsed_string_term _ -> false
(* 		| _ -> raise (NotImplemented "non-rational discrete terms in ModelConverter.split_to_clock_discrete_updates") *)
	in
	List.partition is_clock_update updates

(** Translate a normal parsed update into its abstract model *)
let convert_normal_updates index_of_variables constants type_of_variables updates_list =
	(* Flag to check if there are clock resets only to 0 *)
	let only_resets = ref true in

	(** split clock and discrete updates *)
	let parsed_clock_updates, parsed_discrete_updates = split_to_clock_discrete_updates index_of_variables only_resets type_of_variables updates_list in

	(* Convert the discrete updates *)
	let discrete_updates : discrete_update list = List.map (to_abstract_discrete_update index_of_variables constants) parsed_discrete_updates in

	(* Convert the clock updates *)
	let converted_clock_updates : clock_updates = to_abstract_clock_update index_of_variables constants only_resets parsed_clock_updates in

	(** update abstract model *)
	{
		clock = converted_clock_updates;
		discrete = discrete_updates;
		conditional = [];
	}


(** convert normal and conditional updates *)
let convert_updates index_of_variables constants type_of_variables updates : updates =

  (** split normal and conditional updates *)
  let normal_updates, conditional_updates = List.partition is_normal_update updates in

  (** convert normal parsed updates *)
  let converted_updates = convert_normal_updates index_of_variables constants type_of_variables (List.map get_normal_update_value normal_updates) in

  (** convert normal parsed updates inside conditional updates *)
  let conditional_updates_values : conditional_update list = List.map (fun u ->
      let boolean_value, if_updates, else_updates = get_conditional_update_value u in
      let convert_boolean = convert_bool_expr index_of_variables constants boolean_value in
      let convert_if_updates = convert_normal_updates index_of_variables constants type_of_variables if_updates in
      let convert_else_updates = convert_normal_updates index_of_variables constants type_of_variables else_updates in
      (convert_boolean, convert_if_updates, convert_else_updates)
    ) conditional_updates in

  (** updates abstract model *)
  { converted_updates with conditional = conditional_updates_values }


(*------------------------------------------------------------*)
(* Convert the transitions *)
(*------------------------------------------------------------*)
(* Convert the structure: 'automaton_index -> location_index -> list of (action_index, guard, resets, target_state)'
	into a structure:
	'automaton_index -> location_index -> action_index -> list of (transition_index)'
	and creates a structure transition_index -> (guard, action_index, resets, target_state)
	and creates a structure transition_index -> automaton_index
*)
let convert_transitions nb_transitions nb_actions index_of_variables constants removed_variable_names type_of_variables transitions
	: (((AbstractModel.transition_index list) array) array) array * (AbstractModel.transition array) * (Automaton.automaton_index array)
	=
  (* Create the empty array of transitions automaton_index -> location_index -> action_index -> list of (transition_index) *)
  
  (*** NOTE/TODO: why (Array.length transitions) ?! ***)
  
  let array_of_transitions : (((AbstractModel.transition_index list) array) array) array = Array.make (Array.length transitions) (Array.make 0 (Array.make 0 [])) in
  (* Create the empty array transition_index -> transition *)
  let dummy_transition = {
	guard		= True_guard;
	action		= -1;
	updates		= { clock = No_update; discrete = [] ; conditional = []};
	target		= -1;
	} in
  let transitions_description : AbstractModel.transition array = Array.make nb_transitions dummy_transition in
  let automaton_of_transition : Automaton.automaton_index array = Array.make nb_transitions (-1) in
  
  (* Maintain an index for the next transition *)
  let transition_index = ref 0 in

  (* Iterate on automata *)
  Array.iteri (fun automaton_index transitions_for_this_automaton ->
      let nb_locations = Array.length transitions_for_this_automaton in

      (* Set the array for this automaton *)
      array_of_transitions.(automaton_index) <- Array.make nb_locations (Array.make 0 []);

      (* Iterate on locations *)
      Array.iteri (fun location_index transitions_for_this_location ->

          (*** WARNING !!! Here, a BIG array is created (as big as the number of actions !!) ***)
          (*** TODO: convert to HashTbl ? (test efficiency?) ***)

          (* Set the array for this location *)
          array_of_transitions.(automaton_index).(location_index) <- Array.make nb_actions [];

          (* Iterate on transitions *)
          List.iter (fun (action_index, guard, updates, target_location_index) ->

              (* Convert the guard *)
              let converted_guard = convert_guard index_of_variables type_of_variables constants guard in

              (* Filter the updates that should assign some variable name to be removed to any expression *)
              (* let filtered_updates = List.filter (fun (variable_name, (*linear_expression*)_) ->
                 					not (List.mem variable_name removed_variable_names)
                 				) updates
                 				in *)

              let filtered_updates = filtered_updates removed_variable_names updates in

              (* Flag to check if there are clock resets only to 0 *)
              (* let only_resets = ref true in *)

              (* Split between the clock and discrete updates *)
              (* let clock_updates, discrete_updates = List.partition (fun (variable_index, linear_term) ->
                 					if type_of_variables variable_index = Var_type_clock then(
                 						(* Update flag *)
                 						if linear_term <> (LinearConstraint.make_pxd_linear_term [] NumConst.zero) then(
                 							only_resets := false;
                 						);
                 						true
                 					)else
                 						false
                 				) converted_updates
                 				in *)

              (* Split between the clock and discrete updates *)
              (* let parsed_clock_updates, parsed_discrete_updates = List.partition (is_clock_update index_of_variables only_resets) filtered_updates
                 				in *)

              (* translate parsed updates into their abstract model *)
              let converted_updates = convert_updates index_of_variables constants type_of_variables filtered_updates in

              (* Convert the updates *)
              (* let converted_updates = List.map (fun (variable_name, parsed_continuous_arithmetic_expression) ->
                 					let variable_index = Hashtbl.find index_of_variables variable_name in
                 					let linear_term = linear_term_of_parsed_continuous_arithmetic_expression index_of_variables constants parsed_continuous_arithmetic_expression in
                 					(variable_index, linear_term)
                 				) filtered_updates in*)

              (* Differentiate between different kinds of clock updates *)
              (* let clock_updates : clock_updates =
                 					(* Case 1: no update *)
                 					if converted_clock_updates = [] then No_update
                 					else (
                 						(* Case 2: resets only *)
                 						if !only_resets then (
                 							(* Keep only the clock ids, not the linear terms *)
                 							let clocks_to_reset, _ = List.split converted_clock_updates in
                 							Resets clocks_to_reset
                 						) else
                 						(* Case 3: complex with linear terms *)
                 							Updates converted_clock_updates
                 					)
                 				in *)

              (* Update the transition array *)
              array_of_transitions.(automaton_index).(location_index).(action_index) <- !transition_index :: array_of_transitions.(automaton_index).(location_index).(action_index);
              
              (* Add the transition to the description *)
              transitions_description.(!transition_index) <- {
					guard   = converted_guard;
					action  = action_index;
					updates = converted_updates;
					target  = target_location_index;
				};
              (* Add the automaton *)
              automaton_of_transition.(!transition_index) <- automaton_index;
              
              (* Increment the index *)
              transition_index := !transition_index + 1;

            ) transitions_for_this_location;
        ) transitions_for_this_automaton;
    ) transitions;

  (* Return transitions and the arrays transition_index -> transition and transition_index -> automaton_index *)
  array_of_transitions, transitions_description, automaton_of_transition




(*------------------------------------------------------------*)
(* Create the initial state *)
(*------------------------------------------------------------*)
let make_initial_state index_of_automata locations_per_automaton index_of_locations index_of_variables parameters removed_variable_names constants type_of_variables variable_names init_rational_pairs init_definition =
	(* Get the location initialisations and the constraint *)
	let loc_assignments, linear_predicates = List.partition (function
		| Parsed_loc_assignment _ -> true
		| _ -> false
		) init_definition in
	(* Make pairs (automaton_name, location_name) *)
	let initial_locations = List.map (function
		| Parsed_loc_assignment (automaton_name, location_name) -> (automaton_name, location_name)
		| _ -> raise (InternalError "Something else than a Parsed_loc_assignment was found in a Parsed_loc_assignment list")
		) loc_assignments in
	(* Convert the pairs to automaton_index, location_index *)
	let locations = List.map (fun (automaton_name, location_name) ->
		(* Find the automaton index *)
		let automaton_index = Hashtbl.find index_of_automata automaton_name in
		(* Find the location index *)
		automaton_index,
		Hashtbl.find index_of_locations.(automaton_index) location_name
		) initial_locations in

	(* Construct the initial location *)
	let initial_location = Location.make_location locations init_rational_pairs in

	(* Remove the init definitions for discrete variables *)
	let other_inequalities = List.filter (function
		(* Check if the left part is only a variable name *)
		| Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable (_, variable_name)), _ , _)) ->
			(* First check whether it was removed *)
			if List.mem variable_name removed_variable_names then false
			else
			let is_discrete =
				(* Try to get the variable index *)
				if (Hashtbl.mem index_of_variables variable_name) then (
				let variable_index =  Hashtbl.find index_of_variables variable_name in
				(* Keep if this is a discrete *)
				match type_of_variables variable_index with | Var_type_discrete _ -> true | _ -> false
				) else (
				(* Case constant *)
				if (Hashtbl.mem constants variable_name) then false
				else (
					(* Otherwise: problem! *)
					raise (InternalError ("The variable '" ^ variable_name ^ "' mentioned in the init definition does not exist, although this should have been checked before."));
				))
			in not is_discrete
		| _ -> true
		) linear_predicates in
	(* Convert the inequalities *)
	let convex_predicate = List.map (function
		| Parsed_linear_predicate lp -> lp
		| _ -> raise (InternalError "Something else than a Parsed_linear_predicate was found in a Parsed_linear_predicate list.")
		) other_inequalities in
	let initial_constraint : LinearConstraint.px_linear_constraint =

		(* Create pairs of (index , value) for discrete variables *)
		(* 		let rational_values = List.map (fun rational_index -> rational_index, (Location.get_rational_value initial_location rational_index)) model.discrete in *)

		(* Create a constraint encoding the value of the discretes *)
		let discretes = LinearConstraint.pxd_constraint_of_discrete_values init_rational_pairs in

		(* Create initial constraint (through parsing) *)
		let initial_constraint = (linear_constraint_of_convex_predicate index_of_variables constants convex_predicate) in

		(* Intersects initial constraint with discretes *)
		LinearConstraint.pxd_intersection_assign initial_constraint [discretes];

		(* Remove discretes *)
		LinearConstraint.pxd_hide_discrete_and_collapse initial_constraint
	in

	(* PERFORM VERIFICATIONS *)
	(* Check that all parameters are bound to be >= 0 *)
	List.iter (fun parameter_id ->
		(* Print some information *)
		print_message Verbose_low ("Checking that parameter '" ^ (variable_names parameter_id) ^ "' is >= 0 in the initial constraint…");

		(* Check *)
		if not (LinearConstraint.px_is_positive_in parameter_id initial_constraint) then
			print_warning ("Parameter '" ^ (variable_names parameter_id) ^ "' is not necessarily positive in the initial constraint. The behavior of " ^ Constants.program_name ^ " is unspecified in this case. You are advised to add inequality '" ^ (variable_names parameter_id) ^ " >= 0' to the initial state of the model.");
		) parameters;

	(* Return the initial state *)
	initial_location, initial_constraint


*)


(************************************************************)
(************************************************************)
(** Checking and converting property *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Getting variables *)
(************************************************************)

(*------------------------------------------------------------*)
(* Gather the set of all variable names used in a parsed reference valuation *)
(*------------------------------------------------------------*)
let get_variables_in_parsed_pval (parsed_pval : ParsingStructure.parsed_pval) : variable_name list =
	(* Return the left part of all pairs *)
	let left, _ = List.split parsed_pval in
	left


(*------------------------------------------------------------*)
(* Gather the set of all variable names used in the parsed property *)
(*------------------------------------------------------------*)
let get_variables_in_property_option (parsed_property_option : ParsingStructure.parsed_property option) =
	(* First create the set *)
	let variables_used_ref = ref StringSet.empty in
	
	(* Gather variables to the set, passed by reference *)
	begin
	match parsed_property_option with
	| None -> ()
	| Some parsed_property ->
		begin
		match parsed_property.property with
	
		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Parsed_EF parsed_state_predicate
		(* Safety *)
		| Parsed_AGnot parsed_state_predicate
			-> get_variables_in_parsed_state_predicate variables_used_ref parsed_state_predicate
		
		
		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)
		
		(* Reachability with minimization of a parameter valuation *)
		| Parsed_EFpmin (parsed_state_predicate , parameter_name)
			(* First get the variables in the state predicate *)
			-> get_variables_in_parsed_state_predicate variables_used_ref parsed_state_predicate;
			(* Then add the parameter name *)
			variables_used_ref := StringSet.add parameter_name !variables_used_ref
		
		(* Reachability with minimal-time *)
		| Parsed_EFtmin parsed_state_predicate
			-> get_variables_in_parsed_state_predicate variables_used_ref parsed_state_predicate

		
		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)
		
		(** Infinite-run (cycle) *)
		| Parsed_Cycle -> ()

		(** Accepting infinite-run (cycle) *)
		| Parsed_Acc_Cycle parsed_state_predicate
			-> get_variables_in_parsed_state_predicate variables_used_ref parsed_state_predicate
		
		(*------------------------------------------------------------*)
		(* Inverse method, trace preservation, robustness *)
		(*------------------------------------------------------------*)
		
		(* Inverse method with complete, non-convex result *)
		| Parsed_IM parsed_pval ->
			variables_used_ref := StringSet.of_list (get_variables_in_parsed_pval parsed_pval);
		
		
		
		
(*		(*** TODO ***)
		| _ -> raise (NotImplemented "get_variables_in_property")*)
	
		end;
	end;
	(* Return the set *)
	!variables_used_ref



(*------------------------------------------------------------*)
(* Gathers all (unique) variable names in both model and property *)
(* Returns clock_names, discrete_names, parameter_names, removed_variable_names *)
(*------------------------------------------------------------*)
let build_variables_lists parsed_model parsed_property_option options possibly_multiply_defined_clock_names possibly_multiply_defined_discrete_names possibly_multiply_defined_parameter_names =

	(* First remove multiply defined variable names *)
	let single_clock_names = list_only_once possibly_multiply_defined_clock_names in
	let single_discrete_names = list_only_once possibly_multiply_defined_discrete_names in
	let single_parameter_names = list_only_once possibly_multiply_defined_parameter_names in
	
	(*------------------------------------------------------------*)
	(* Remove unused variables *)
	(*------------------------------------------------------------*)

	(* Unless a specific option is activated, we first remove all variables declared but unused *)
	let clock_names, discrete_names, parameter_names, removed_variable_names =
	if options#no_variable_autoremove then(
		(* Nothing to do *)
		single_clock_names, single_discrete_names, single_parameter_names, []
	)else (
		(* Gather all variables used *)
		let all_variables_used_in_model = get_all_variables_used_in_model parsed_model in
		let all_variables_used_in_property = get_variables_in_property_option parsed_property_option in
		let all_variable_used = StringSet.union all_variables_used_in_model all_variables_used_in_property in

		(* Remove variable unused *)
		let remove_unused_variables_gen variable_type_name = List.partition (fun variable_name ->
			(* The variable is kept if… *)
			if
				(* Either it is used somewhere *)
				(StringSet.mem variable_name all_variable_used)
				(* Or it is a clock with the special global_time name *)
				||
				(variable_name = Constants.global_time_clock_name && List.mem variable_name single_clock_names)
			then true
			else (
				(* First print a warning *)
				print_warning ("The " ^ variable_type_name ^ " '" ^ variable_name ^ "' is declared but never used in the model; it is therefore removed from the model. Use option -no-var-autoremove to keep it.");
				(* Filter out *)
				false
			)
		) in
		(* Create all three lists *)
		let single_clock_names, removed_clock_names = remove_unused_variables_gen "clock" single_clock_names in
		let single_discrete_names, removed_discrete_names = remove_unused_variables_gen "discrete" single_discrete_names in
		let single_parameter_names, removed_parameter_names = remove_unused_variables_gen "parameter" single_parameter_names in
		(* Return and append removed variable names *)
		single_clock_names, single_discrete_names, single_parameter_names, List.rev_append removed_clock_names (List.rev_append removed_discrete_names removed_parameter_names)
	) in
	clock_names, discrete_names, parameter_names, removed_variable_names




(*
	
	OLD CODE

et get_variables_in_property variables_used_ref = function
  | Parsed_unreachable_locations (parsed_unreachable_global_location_list) ->
    List.iter (fun parsed_unreachable_global_location ->
        List.iter (function
            | Parsed_unreachable_discrete parsed_rational_constraint ->
              begin
                match parsed_rational_constraint with
                | Parsed_rational_l (variable_name, (*rational_value*)_)
                | Parsed_rational_leq (variable_name, (*rational_value*)_)
                | Parsed_rational_equal (variable_name, (*rational_value*)_)
                | Parsed_rational_geq (variable_name, (*rational_value*)_)
                | Parsed_rational_g (variable_name, (*rational_value*)_)
                  -> variables_used_ref := StringSet.add variable_name !variables_used_ref
                | Parsed_rational_interval (variable_name, (*rational_value*)_, (*rational_value*)_)
                  -> variables_used_ref := StringSet.add variable_name !variables_used_ref
              end
            | Parsed_unreachable_loc _ -> ()
          ) parsed_unreachable_global_location;
      ) parsed_unreachable_global_location_list

  (* if a2 then a1 has happened before *)
  | Action_precedence_acyclic _
  (* everytime a2 then a1 has happened before *)
  | Action_precedence_cyclic _
  (* everytime a2 then a1 has happened exactly once before *)
  | Action_precedence_cyclicstrict _
    -> ()

  (* a no later than d *)
  | Action_deadline (sync_name , duration)
    -> get_variables_in_linear_expression variables_used_ref duration

  (* if a2 then a1 happened within d before *)
  | TB_Action_precedence_acyclic ((*sync_name*)_, (*sync_name*)_, duration)
  (* everytime a2 then a1 happened within d before *)
  | TB_Action_precedence_cyclic ((*sync_name*)_, (*sync_name*)_, duration)
  (* everytime a2 then a1 happened once within d before *)
  | TB_Action_precedence_cyclicstrict ((*sync_name*)_, (*sync_name*)_, duration)

  (* if a1 then eventually a2 within d *)
  | TB_response_acyclic ((*sync_name*)_, (*sync_name*)_, duration)
  (* everytime a1 then eventually a2 within d *)
  | TB_response_cyclic ((*sync_name*)_, (*sync_name*)_, duration)
  (* everytime a1 then eventually a2 within d once before next *)
  | TB_response_cyclicstrict ((*sync_name*)_, (*sync_name*)_, duration)
    -> get_variables_in_linear_expression variables_used_ref duration

  (* sequence: a1, …, an *)
  | Sequence_acyclic _
  (* sequence: always a1, …, an *)
  | Sequence_cyclic _
    -> ()*)

    
(************************************************************)
(** Checking the property *)
(************************************************************)


(*

(*------------------------------------------------------------*)
(* Local functions checking existence of a name in property specifications
 * May print warnings or errors
*)
(*------------------------------------------------------------*)
let check_automaton_name index_of_automata automaton_name =
  if not (Hashtbl.mem index_of_automata automaton_name)
  then (
    print_error ("The automaton name '" ^ automaton_name ^ "' used in the correctness property does not exist.");
    false
  )
  else true

(*------------------------------------------------------------*)
let check_location_name index_of_locations automaton_index automaton_name location_name =
  if not (Hashtbl.mem index_of_locations.(automaton_index) location_name)
  then (
    print_error ("The location name '" ^ location_name ^ "' used in the correctness property does not exist in automaton '" ^ automaton_name ^ "'.");
    false
  )
  else true

*)
(*------------------------------------------------------------*)
let check_action_name index_of_actions action_name =
	if not (Hashtbl.mem index_of_actions action_name)
	then (
		print_error ("The action '" ^ action_name ^ "' used in the property does not exist in this model.");
		false
	)
	else true

(*
(*------------------------------------------------------------*)
(* Check a list of local location declaration (i.e., of the form 'loc[automaton] = location')
 * Return a list of unreachable_location, and a Boolean encoding whether all checks passed
 * May print warnings or errors
*)
(*------------------------------------------------------------*)
let check_and_convert_unreachable_local_locations index_of_automata index_of_locations parsed_unreachable_locations =
  (* Create a hash table to check for double declarations *)
  let hashtable = Hashtbl.create (Hashtbl.length index_of_automata) in

  (* Global check *)
  let checks_passed = ref true in

  (* Iterate on parsed_unreachable_locations and check for names *)
  List.iter(fun parsed_unreachable_predicate ->
      let automaton_name , location_name =
        match parsed_unreachable_predicate with
        | Parsed_unreachable_loc (automaton_name , location_name) -> automaton_name , location_name
        | _ -> raise (InternalError("Expecting a Parsed_unreachable_loc, which should have been checked earlier."))
      in
      (* Check automaton name *)
      let check1 = check_automaton_name index_of_automata automaton_name in

      (* Check location name *)
      let check2 =
        (* Only perform check2 if check1 passed *)
        if not check1 then false else(
          (* Retrieve automaton index *)
          let automaton_index = Hashtbl.find index_of_automata automaton_name in
          (* Check location name for this automaton *)
          check_location_name index_of_locations automaton_index automaton_name location_name
        )
      in

      (* Check whether another location was declared for this automaton and, if not, store the location *)
      let check3 = if Hashtbl.mem hashtable automaton_name then(
          (* Retrieve former location name *)
          let old_name = Hashtbl.find hashtable automaton_name in
          (* If same name: just warning *)
          if old_name = location_name then(
            (* Warning *)
            print_warning ("Automaton '" ^ automaton_name ^ "' is assigned several times to location name '" ^ location_name ^ "' in the correctness property.");
            (* No problem *)
            true
          )else(
            (* Otherwise: error *)
            print_warning ("Automaton '" ^ automaton_name ^ "' is assigned to several different location names (e.g., '" ^ location_name ^ "' and '" ^ old_name ^ "') in the correctness property.");
            (* Problem *)
            false
          )
        ) else (
          (* Add to hash table *)
          Hashtbl.add hashtable automaton_name location_name;
          (* No problem *)
          true
        )
      in

      (* Update checks *)
      checks_passed := !checks_passed && check1 && check2 && check3;

    ) parsed_unreachable_locations;

  (* If problem: returns dummy result *)
  if not !checks_passed then(
    [] , false
  )else(
    (* Return all pairs 'loc[automaton] = location' *)
    let pairs = Hashtbl.fold (fun automaton_name location_name former_pairs ->
        (* Retrieve automaton index (no test because was tested earlier) *)
        let automaton_index = Hashtbl.find index_of_automata automaton_name in
        (* Retrieve location index (no test because was tested earlier) *)
        let location_index = Hashtbl.find index_of_locations.(automaton_index) location_name in
        (* Add new pair *)
        (automaton_index, location_index) :: former_pairs
      ) hashtable [] in
    pairs , true
  )


(*------------------------------------------------------------*)
(* Check a list of discrete constraints declaration (i.e., of the form 'd ~ constant(s)')
 * Return a list of rational_constraint, and a Boolean encoding whether all checks passed
 * May print warnings or errors
*)
(*------------------------------------------------------------*)
let check_and_convert_unreachable_rational_constraints index_of_variables type_of_variables variable_names discrete parsed_unreachable_locations =
  (* Create a hash table to check for double declarations *)
  let hashtable = Hashtbl.create (List.length discrete) in

  (*(* Horrible hack: store not the constants, but their string representation (to allow for a quick representation of single constants and pairs of constants in intervals) *)
    	let string_of_parsed_rational_constraint = function
    		| Parsed_rational_l (_ , v)
    			-> "<" ^ (NumConst.string_of_numconst v)
    		| Parsed_rational_leq (_ , v)
    			-> "<=" ^ (NumConst.string_of_numconst v)
    		| Parsed_rational_equal (_ , v)
    			-> "=" ^ (NumConst.string_of_numconst v)
    		| Parsed_rational_geq (_ , v)
    			-> ">=" ^ (NumConst.string_of_numconst v)
    		| Parsed_rational_g (_ , v)
    			-> ">" ^ (NumConst.string_of_numconst v)
    		| Parsed_rational_interval (_ , min_bound, max_bound)
    			-> " in [" ^ (NumConst.string_of_numconst min_bound)
    					^ " .. "
    					^ (NumConst.string_of_numconst max_bound)
    					^ "]"
    	in*)

  (* Local function to convert a parsed constraint into an abstract discrete constraint *)
  (*** NOTE: will only be called once the discrete variable name has been checked for name and type ***)
  let convert_rational_constraint = function
    | Parsed_rational_l (rational_name , v)
      -> Discrete_l (Hashtbl.find index_of_variables rational_name , v)
    | Parsed_rational_leq (rational_name , v)
      -> Discrete_leq (Hashtbl.find index_of_variables rational_name , v)
    | Parsed_rational_equal (rational_name , v)
      -> Discrete_equal (Hashtbl.find index_of_variables rational_name , v)
    | Parsed_rational_geq (rational_name , v)
      -> Discrete_geq (Hashtbl.find index_of_variables rational_name , v)
    | Parsed_rational_g (rational_name , v)
      -> Discrete_g (Hashtbl.find index_of_variables rational_name , v)
    | Parsed_rational_interval (rational_name , min_bound, max_bound)
      -> Discrete_interval (Hashtbl.find index_of_variables rational_name , min_bound, max_bound)
  in

  (* Global check *)
  let checks_passed = ref true in

  (* Iterate on parsed_unreachable_predicate and check for names and values *)
  List.iter(fun parsed_unreachable_predicate ->
      let parsed_rational_constraint =
        match parsed_unreachable_predicate with
        | Parsed_unreachable_discrete parsed_rational_constraint -> parsed_rational_constraint
        | _ -> raise (InternalError("Expecting a Parsed_unreachable_discrete, which should have been checked earlier."))
      in

      (* Get discrete variable name *)
      let rational_name = match parsed_rational_constraint with
        | Parsed_rational_l (variable_name , _)
        | Parsed_rational_leq (variable_name , _)
        | Parsed_rational_equal (variable_name , _)
        | Parsed_rational_geq (variable_name , _)
        | Parsed_rational_g (variable_name , _)
          -> variable_name
        | Parsed_rational_interval (variable_name , _, _)
          -> variable_name
      in

      (* Check discrete name and type *)
      let check1 =
        (* 1a. Check for name *)
        if not (Hashtbl.mem index_of_variables rational_name) then(
          (* Print error *)
          print_error ("The discrete variable '" ^ rational_name ^ "' used in the correctness property does not exist in this model.");
          (* Problem *)
          false
          (* 1b. Check for type *)
        )else(
          (* Get variable index *)
          (*** NOTE: safe because Hashtbl.mem was checked in test 1a ***)
          let variable_index = Hashtbl.find index_of_variables rational_name in
          (* Check type *)
          let variable_type = type_of_variables variable_index in
          if variable_type <> Var_type_discrete then(
            (* Print error *)
            print_error ("The variable '" ^ rational_name ^ "' used in the correctness property must be a discrete variable (clocks and parameters are not allowed at this stage).");
            (* Problem *)
            false
          )else true
        )
      in

      (* Check value1 <= value2 *)
      let check2 = match parsed_rational_constraint with
        | Parsed_rational_l _
        | Parsed_rational_leq _
        | Parsed_rational_equal _
        | Parsed_rational_geq _
        | Parsed_rational_g _
          -> true
        | Parsed_rational_interval (_ , min_bound , max_bound )
          -> if NumConst.g min_bound max_bound then(
            print_error ("The interval '[" ^ (NumConst.string_of_numconst min_bound) ^ " , " ^ (NumConst.string_of_numconst max_bound) ^ "]' used in the correctness property is not well-formed.");
            (* Problem *)
            false
          ) else true
      in

      (* Check whether another value was declared for this discrete and, if not, store the value *)
      let check3 =
        (* Only perform this if further checks passed *)
        if not (check1 && check2) then false else(
          (* Compute abstract constraint *)
          let abstract_constraint = convert_rational_constraint parsed_rational_constraint in
          (* Check whether another value was declared for this discrete *)
          if Hashtbl.mem hashtable rational_name then(
            (* Retrieve former value *)
            let old_constraint = Hashtbl.find hashtable rational_name in
            (* If same name: just warning *)
            if old_constraint = abstract_constraint then(
              (* Warning *)
              print_warning ("Discrete variable '" ^ rational_name ^ "' is compared several times to the same constant in the correctness property.");
              (* No problem *)
              true
            )else(
              (* Otherwise: error *)
              print_warning ("Discrete variable '" ^ rational_name ^ "' is compared several times to different constants in the correctness property.");
              (* Problem *)
              false
            )
          ) else (
            (* Add to hash table *)
            Hashtbl.add hashtable rational_name abstract_constraint;
            (* No problem *)
            true
          )
        )
      in

      (* Update checks *)
      checks_passed := !checks_passed && check1 && check2 && check3;

    ) parsed_unreachable_locations;

  (* If problem: returns dummy result *)
  if not !checks_passed then(
    [] , false
  )else(
    (* Return all abstract constraints *)
    let abstract_constraints = Hashtbl.fold (fun _ abstract_constraint former_abstract_constraints ->
        (*			(* Retrieve automaton index (no test because was tested earlier) *)
          			let rational_index = Hashtbl.find index_of_variables rational_name in*)
        (* Add new abstract_constraint *)
        abstract_constraint :: former_abstract_constraints
      ) hashtable [] in
    abstract_constraints , true
  )


(*------------------------------------------------------------*)
(* Check one global location declaration (i.e., a list of location predicates and/or of discrete constraints)
 * Return a converted global location, and a Boolean encoding whether all checks passed
 * May print warnings or errors
*)
(*------------------------------------------------------------*)
let check_and_convert_unreachable_global_location index_of_variables type_of_variables discrete variable_names index_of_automata index_of_locations parsed_unreachable_global_location =
  (* Split the predicates in location and discrete *)
  let parsed_unreachable_locations, parsed_rational_constraints = List.partition (function
      | Parsed_unreachable_loc _ -> true
      | _ -> false
    ) parsed_unreachable_global_location
  in

  (* Call dedicated functions *)
  let unreachable_locations, check1 = check_and_convert_unreachable_local_locations index_of_automata index_of_locations parsed_unreachable_locations in
  let rational_constraints, check2 = check_and_convert_unreachable_rational_constraints index_of_variables type_of_variables variable_names discrete parsed_rational_constraints in

  (* Return *)
  {
    unreachable_locations = unreachable_locations;
    rational_constraints  = rational_constraints;
  }
  ,
  check1 && check2

*)

(*------------------------------------------------------------*)
(** Check a discrete Boolean expression *)
(*------------------------------------------------------------*)
(*
let rec check_parsed_rational_arithmetic_expression useful_parsing_model_information = function
	| Parsed_CAE_plus (parsed_rational_arithmetic_expression , parsed_rational_term)
	| Parsed_CAE_minus (parsed_rational_arithmetic_expression , parsed_rational_term) ->
		evaluate_and
			(check_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression)
			(check_parsed_rational_term useful_parsing_model_information parsed_rational_term)
	| Parsed_CAE_term parsed_rational_term -> check_parsed_rational_term useful_parsing_model_information parsed_rational_term

and check_parsed_rational_term useful_parsing_model_information = function
	| Parsed_CT_mul (parsed_rational_term , parsed_rational_factor)
	| Parsed_CT_div (parsed_rational_term , parsed_rational_factor) ->
		evaluate_and
			(check_parsed_rational_term useful_parsing_model_information parsed_rational_term)
			(check_parsed_rational_factor useful_parsing_model_information parsed_rational_factor)
	| Parsed_CT_factor parsed_rational_factor ->
		check_parsed_rational_factor useful_parsing_model_information parsed_rational_factor

and check_parsed_rational_factor useful_parsing_model_information = function
	| Parsed_CF_variable variable_name ->
		if not (Hashtbl.mem useful_parsing_model_information.index_of_variables variable_name) && not (Hashtbl.mem useful_parsing_model_information.constants variable_name) then (
			print_error ("Undefined variable name '" ^ variable_name ^ "' in the property");
			false
		)else(
			true
		)
	| Parsed_CF_constant _ -> true
	| Parsed_CF_expression parsed_rational_arithmetic_expression ->
		check_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression
	| Parsed_CF_unary_min parsed_rational_factor ->
		check_parsed_rational_factor useful_parsing_model_information parsed_rational_factor


(* Check correct variable names in parsed_rational_boolean_expression *)
let check_parsed_rational_boolean_expression useful_parsing_model_information = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Parsed_expression (parsed_rational_arithmetic_expression1 , _ , parsed_rational_arithmetic_expression2) ->
		evaluate_and
			(check_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression1)
			(check_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression2)
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Parsed_expression_in (parsed_rational_arithmetic_expression1 , parsed_rational_arithmetic_expression2 , parsed_rational_arithmetic_expression3) ->
		let check1 = check_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression1 in
		let check2 = check_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression2 in
		let check3 = check_parsed_rational_arithmetic_expression useful_parsing_model_information parsed_rational_arithmetic_expression3 in
		check1 && check2 && check3


		
(*------------------------------------------------------------*)
(** Check a state predicate *)
(*------------------------------------------------------------*)
let check_parsed_loc_predicate useful_parsing_model_information = function
	| Parsed_loc_predicate_EQ (automaton_name , location_name)
	| Parsed_loc_predicate_NEQ (automaton_name , location_name)
		->
		(* Useful variables *)
		let index_of_automata  = useful_parsing_model_information.index_of_automata  in
		let index_of_locations : ((Automaton.location_name, Automaton.location_index) Hashtbl.t) array = useful_parsing_model_information.index_of_locations in
		
		(* Find the automaton *)
		if not (Hashtbl.mem index_of_automata automaton_name) then(
			print_error ("Unknown automaton name '" ^ automaton_name ^ "' in the property.");
			false
		)else(
			let automaton_index : Automaton.automaton_index = Hashtbl.find index_of_automata automaton_name in
			(* Find the location *)
			if not (Hashtbl.mem index_of_locations.(automaton_index) location_name) then(
				print_error ("Unknown location name '" ^ location_name ^ "' in automaton '" ^ automaton_name ^ "' in the property.");
				false
			)else(
				(* Both checks passed *)
				true
			)
		)


let check_parsed_simple_predicate useful_parsing_model_information = function
	| Parsed_rational_boolean_predicate parsed_rational_boolean_expression ->
		check_parsed_rational_boolean_expression useful_parsing_model_information parsed_rational_boolean_expression
	| Parsed_loc_predicate parsed_loc_predicate ->
		check_parsed_loc_predicate useful_parsing_model_information parsed_loc_predicate


let rec check_parsed_state_predicate useful_parsing_model_information = function
	| Parsed_state_predicate_OR (parsed_state_predicate_1 , parsed_state_predicate_2) ->
		(* Check both even if one fails, so as to provide users with more errors at once *)
		evaluate_and
			(check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate_1)
			(check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate_2)
		
	| Parsed_state_predicate_term parsed_state_predicate_term ->
		check_parsed_state_predicate_term useful_parsing_model_information parsed_state_predicate_term

	| Parsed_state_predicate_true | Parsed_state_predicate_false ->
		true

and check_parsed_state_predicate_term useful_parsing_model_information = function
	| Parsed_state_predicate_term_AND (parsed_state_predicate_term_1 , parsed_state_predicate_term_2) ->
		(* Check both even if one fails, so as to provide users with more errors at once *)
		evaluate_and
			(check_parsed_state_predicate_term useful_parsing_model_information parsed_state_predicate_term_1)
			(check_parsed_state_predicate_term useful_parsing_model_information parsed_state_predicate_term_2)
	| Parsed_state_predicate_factor parsed_state_predicate_factor ->
		check_parsed_state_predicate_factor useful_parsing_model_information parsed_state_predicate_factor

and check_parsed_state_predicate_factor useful_parsing_model_information = function
	| Parsed_state_predicate_factor_NOT parsed_state_predicate_factor ->
		check_parsed_state_predicate_factor useful_parsing_model_information parsed_state_predicate_factor
	| Parsed_simple_predicate parsed_simple_predicate ->
		check_parsed_simple_predicate useful_parsing_model_information parsed_simple_predicate
	| Parsed_state_predicate parsed_state_predicate ->
		check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
*)


(*------------------------------------------------------------*)
(** Generic function checking whether a name is a valid parameter name *)
(*------------------------------------------------------------*)
let check_parameter_name suffix_explanation_string useful_parsing_model_information parameter_name =
	(* First check it is a variable *)
	if not(Hashtbl.mem useful_parsing_model_information.index_of_variables parameter_name) then(
		print_error ("Parameter " ^ parameter_name ^ " is not a defined variable" ^ suffix_explanation_string);
		false
	) else(
		let parameter_index = Hashtbl.find useful_parsing_model_information.index_of_variables parameter_name in
		if not(useful_parsing_model_information.type_of_variables parameter_index = Var_type_parameter) then(
			print_error ("Variable " ^ parameter_name ^ " is not a parameter" ^ suffix_explanation_string);
			false
		)else true
	)

(*------------------------------------------------------------*)
(** Check that all parameters in the projection definition are valid *)
(*------------------------------------------------------------*)
let check_projection_definition useful_parsing_model_information = function
	| None -> true
	| Some parsed_parameters -> (
		let well_formed = ref true in
		List.iter (fun parsed_parameter ->
			if not (check_parameter_name " in the projection definition" useful_parsing_model_information parsed_parameter) then
				well_formed := false
			) parsed_parameters;
		!well_formed
		)

(*------------------------------------------------------------*)
(** Check that the optimization definition is valid *)
(*------------------------------------------------------------*)
(*let check_optimization parameters_names = function
	| No_parsed_optimization -> true
	
	| Parsed_minimize parameter_name | Parsed_maximize parameter_name ->
		if not (List.mem parameter_name parameters_names) then(
		print_error ("Parameter " ^ parameter_name  ^ " is not a valid parameter in the optimization definition.");
		false
		) else true*)


(*------------------------------------------------------------*)
(* Check the parsed_pval w.r.t. the model parameters *)
(*------------------------------------------------------------*)
let check_parsed_pval useful_parsing_model_information (parsed_pval : ParsingStructure.parsed_pval) =
	(* Compute the list of variable names *)
	(**** TO OPTIMIZE: not tail recursvie ****)
	let list_of_variables, _ = List.split parsed_pval in

	(* Compute the multiply defined variables *)
	let multiply_defined_variables = elements_existing_several_times list_of_variables in
	(* Print an error for each of them *)
	List.iter (fun variable_name -> print_error ("The parameter '" ^ variable_name ^ "' was assigned several times a valuation in parsed_pval.")) multiply_defined_variables;

	(*** TODO: only warns if it is always defined to the same value ***)

		(* Check if the variables are all defined *)
		let all_defined = List.fold_left
				(fun all_defined variable_name ->
					if List.mem variable_name list_of_variables then all_defined
					else (
					print_error ("The parameter '" ^ variable_name ^ "' was not assigned a valuation in parsed_pval.");
					false
					)
				)
				true
				useful_parsing_model_information.parameter_names
		in

	(* Check if some defined variables are not parameters (and warn) *)
	List.iter
		(fun variable_name ->
		if not (List.mem variable_name useful_parsing_model_information.parameter_names) then (
			print_warning ("'" ^ variable_name ^ "', which is assigned a valuation in parsed_pval, is not a valid parameter name.")
		)
		)
		list_of_variables
	;
	
	(* If something went wrong: launch an error *)
	multiply_defined_variables = [] && all_defined


(*
(*------------------------------------------------------------*)
(* Check the correctness property declaration       *)
(*------------------------------------------------------------*)
let check_property_option useful_parsing_model_information (parsed_property_option : ParsingStructure.parsed_property option) =
	let constants			= useful_parsing_model_information.constants in
	let discrete			= useful_parsing_model_information.discrete in
	let index_of_actions	= useful_parsing_model_information.index_of_actions in
	let index_of_automata	= useful_parsing_model_information.index_of_automata in
	let index_of_locations	= useful_parsing_model_information.index_of_locations in
	let index_of_variables	= useful_parsing_model_information.index_of_variables in
	let type_of_variables	= useful_parsing_model_information.type_of_variables in
	let variable_names		= useful_parsing_model_information.variable_names in

	(* Generic check function for 2 actions *)
	let gen_check_2act property a1 a2 =
		(* Check action names (perform 2 even if one fails) *)
		evaluate_and
			(check_action_name index_of_actions a1)
			(check_action_name index_of_actions a2)
	in

	(* Generic check function for 2 actions and one deadline *)
	let gen_check_2actd property a1 a2 d =
		(* Check action names and deadline (perform 3 even if one fails) *)
		let check1 = check_action_name index_of_actions a1 in
		let check2 = check_action_name index_of_actions a2 in
		let check3 = all_variables_defined_in_linear_expression variable_names constants d in
		let check4 = (if no_variables_in_linear_expression index_of_variables type_of_variables constants d
					then true
					else (print_error("No variable is allowed in the property definition (only constants and parameters)."); false))
		in
		check1 && check2 && check3 && check4
	in

	(* Generic check and conversion function for a list of actions *)
	let gen_list property actions_list =
		(* Check action names (use a fold_left instead of forall to ensure that all actions will be checked) *)
		List.fold_left (fun current_result a -> current_result && (check_action_name index_of_actions a)) true actions_list
	in


	(* Check *)
	match parsed_property_option with
	| None -> true
	| Some parsed_property ->
		begin
		match parsed_property.property with

		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Parsed_EF parsed_state_predicate
		
		(* Safety *)
		| Parsed_AGnot parsed_state_predicate
			->
			check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
		
		
		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)
		
		(* Reachability with minimization of a parameter valuation *)
		| Parsed_EFpmin (parsed_state_predicate , parameter_name) ->
			(*** NOTE: two checks to allow to check both side of the equality whatever happens ***)
			evaluate_and
				(check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate)
				(check_parameter_name " in the optimization definition" useful_parsing_model_information parameter_name)

		
		(* Reachability with minimal-time *)
		| Parsed_EFtmin parsed_state_predicate ->
			check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
		
		
		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)
		
		(** Infinite-run (cycle) *)
		| Parsed_Cycle -> true

		(** Accepting infinite-run (cycle) *)
		| Parsed_Acc_Cycle parsed_state_predicate ->
			check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
		
		
		(*------------------------------------------------------------*)
		(* Inverse method, trace preservation, robustness *)
		(*------------------------------------------------------------*)
		
		(* Inverse method with complete, non-convex result *)
		| Parsed_IM parsed_pval ->
			check_parsed_pval useful_parsing_model_information parsed_pval
		
		
		
		
(*		(*** TODO ***)
		| Parsed_Action_deadline _
		| _
			->
			raise (NotImplemented "ModelConverter : check_property_option")*)
		end
		
		
		
		(*
		(* CASE NON-REACHABILITY *)
		| Parsed_unreachable_locations parsed_unreachable_global_location_list (*(automaton_name , location_name)*) ->
			(* Global flag for checks *)
			let checks_passed = ref true in
			(* Check and convert each global location *)
			let unreachable_global_location_list = List.map (fun parsed_unreachable_global_location ->
				(* Check and convert this parsed_unreachable_global_location *)
				let unreachable_global_location , checked = check_and_convert_unreachable_global_location index_of_variables type_of_variables discrete variable_names index_of_automata index_of_locations parsed_unreachable_global_location in
				(* Update global variable *)
				checks_passed := !checks_passed && checked;
				(* Keep abstract global location *)
				unreachable_global_location
			) parsed_unreachable_global_location_list
			in
			(* Return abstract structure and flag for checks *)
			Unreachable_locations unreachable_global_location_list , !checks_passed


		(* CASE TWO ACTIONS *)
		(* if a2 then a1 has happened before *)
		| ParsingStructure.Action_precedence_acyclic ( a1 , a2 )
		(* everytime a2 then a1 has happened before *)
		| ParsingStructure.Action_precedence_cyclic ( a1 , a2 )
		(* everytime a2 then a1 has happened exactly once before *)
		| ParsingStructure.Action_precedence_cyclicstrict ( a1 , a2 )
			(*** NOT IMPLEMENTED ***)
			(*		(* if a1 then eventually a2 *)
					| ParsingStructure.Eventual_response_acyclic ( a1 , a2 )
					(* everytime a1 then eventually a2 *)
					| ParsingStructure.Eventual_response_cyclic ( a1 , a2 )
					(* everytime a1 then eventually a2 once before next *)
					| ParsingStructure.Eventual_response_cyclicstrict ( a1 , a2 )*)
			-> gen_check_and_convert_2act property a1 a2

		(* CASE ACTION + DEADLINE *)
		| ParsingStructure.Action_deadline ( a , d )
			->
			(* Check action name and deadline (perform 2 even if one fails) *)
			let check1 = check_action_name index_of_actions a in
			let check2 = all_variables_defined_in_linear_expression variable_names constants d in
			let check3 = all_variables_defined_in_linear_expression variable_names constants d in
			let check4 = (if no_variables_in_linear_expression index_of_variables type_of_variables constants d
						then true
						else (print_error("No variable is allowed in the property definition (only constants and parameters)."); false))
			in
			if not (check1 && check2 && check3 && check4) then (None , false)
			else (
			(* Get action indexes *)
			let action_index = Hashtbl.find index_of_actions a in
			(* Convert deadline *)
			let d = LinearConstraint.cast_p_of_pxd_linear_term (linear_term_of_linear_expression index_of_variables constants d) true in
			AbstractModel.Action_deadline ( action_index , d ), true
			)

		(* CASE 2 ACTIONS + DEADLINE *)
		(* if a2 then a1 happened within d before *)
		| ParsingStructure.TB_Action_precedence_acyclic (a1, a2, d)
		(* everytime a2 then a1 happened within d before *)
		| ParsingStructure.TB_Action_precedence_cyclic (a1, a2, d)
		(* everytime a2 then a1 happened once within d before *)
		| ParsingStructure.TB_Action_precedence_cyclicstrict (a1, a2, d)
		(* if a1 then eventually a2 within d *)
		| ParsingStructure.TB_response_acyclic (a1, a2, d)
		(* everytime a1 then eventually a2 within d *)
		| ParsingStructure.TB_response_cyclic (a1, a2, d)
		(* everytime a1 then eventually a2 within d once before next *)
		| ParsingStructure.TB_response_cyclicstrict (a1, a2, d)
			-> gen_check_and_convert_2actd property a1 a2 d

		(* CASE SEQUENCES (list of actions) *)
		(* sequence: a1, …, an *)
		| ParsingStructure.Sequence_acyclic (actions_list)
		(* sequence: always a1, …, an *)
		| ParsingStructure.Sequence_cyclic (actions_list)
			-> gen_check_and_convert_list property actions_list

		(*		(* Otherwise : error ! *)
					| _ -> raise (InternalError ("In the bad definition, not all possibilities are implemented yet."))*)
		end
	(*	in
		[n action_index], !well_formed*)

	*)*)


(************************************************************)
(** Converting the property  *)
(************************************************************)


(*------------------------------------------------------------*)
(* Convert the parsed parsed_pval into a valid parsed_pval *)
(*------------------------------------------------------------*)
let convert_parsed_pval useful_parsing_model_information (parsed_pval : ParsingStructure.parsed_pval) : PVal.pval =
	let pval = new PVal.pval in
	for i = 0 to useful_parsing_model_information.nb_parameters - 1 do
		let parameter_name = useful_parsing_model_information.variables.(i) in
		let valuation = try(
			List.assoc parameter_name parsed_pval
		) with Not_found ->
			raise (InternalError ("The parameter name '" ^ parameter_name ^ "' was not found in parsed_pval although checks should have been performed before."))
		in
		pval#set_value i valuation
	done;
	(* Return the parameter valuation *)
	pval


let convert_synthesis_type = function
	| Parsed_witness	-> Witness
	| Parsed_synthesis	-> Synthesis


(*
type converted_observer_structure = {
	(*  observer_actions, observer_actions_per_location, observer_location_urgency, observer_invariants, observer_transitions *)
	observer_structure			: Automaton.action_index list * (Automaton.action_index list) array * AbstractModel.location_urgency array * LinearConstraint.pxd_linear_constraint array * AbstractModel.transition list array array;
	
	nb_transitions_for_observer	: int;
	initial_observer_constraint	: LinearConstraint.px_linear_constraint;
}

(* Convert ParsingStructure.parsed_property into AbstractProperty.property *)
let convert_property_option useful_parsing_model_information (parsed_property_option : ParsingStructure.parsed_property option) : (AbstractProperty.abstract_property option * converted_observer_structure option) =
	let constants			= useful_parsing_model_information.constants in
	let discrete			= useful_parsing_model_information.discrete in
	let index_of_actions	= useful_parsing_model_information.index_of_actions in
	let index_of_automata	= useful_parsing_model_information.index_of_automata in
	let index_of_locations	= useful_parsing_model_information.index_of_locations in
	let index_of_variables	= useful_parsing_model_information.index_of_variables in
	let type_of_variables	= useful_parsing_model_information.type_of_variables in
	let variable_names		= useful_parsing_model_information.variable_names in

	
	(*
	(*** BEGIN old code below ***)
	match observer_automaton with

	(* First handle observer-free user-defined properties *)
	| None ->
		print_message Verbose_total ("*** (No observer)");
		begin
		match property_option with
			| None -> None
			| Some 
			| Unreachable_locations unreachable_global_location_list -> None, 0, None, Some (Unreachable unreachable_global_location_list)
			| _ -> None, 0, None, None
		end
	
	(* Second: handle observer-based user-defined properties *)
	| Some observer_id ->
		print_message Verbose_low ("*** Generating the observer…");
		(* Get the silent action index for the observer *)
		let nosync_obs = match nosync_obs with
			| Some nosync_obs -> nosync_obs
			| None -> raise (InternalError ("An observer action should have been defined."))
		in
		(* Get the local clock for the observer *)
		(*** WARNING: quite a HACK, here ***)
		let clock_obs = nb_parameters + nb_clocks - 1 in
		(* Get the info from the observer pattern *)
		let observer_actions, observer_actions_per_location, observer_location_urgency, observer_invariants, observer_transitions, initial_observer_constraint, correctness_condition =
			ObserverPatterns.get_automaton nb_actions observer_id nosync_obs clock_obs property in

		(* Return the structure and the correctness_condition *)
		Some (observer_actions, observer_actions_per_location, observer_location_urgency, observer_invariants, observer_transitions),
		((* Iterate on locations *)
			Array.fold_left (fun nb_transitions_for_locations transitions_for_this_location ->
				Array.fold_left (fun nb_transitions_for_actions transitions_for_this_action ->
				nb_transitions_for_actions + (List.length transitions_for_this_action)
				) nb_transitions_for_locations transitions_for_this_location
			) 0 observer_transitions
		),
		Some correctness_condition,
		initial_observer_constraint
	(*** END old code below ***)
	in

	
	*)
	(* Generic check and conversion function for 2 actions *)
	let gen_convert_2act property a1 a2 =
		(* Check action names (perform 2 even if one fails) *)
		let check1 = check_action_name index_of_actions a1 in
		let check2 = check_action_name index_of_actions a2 in
		if not (check1 && check2)
		then (None , false)
		else (
		(* Get action indexes *)
		let action_index1 = Hashtbl.find index_of_actions a1 in
		let action_index2 = Hashtbl.find index_of_actions a2 in
		(*** BADPROG (but couldn't see how to do better!) *)
		(* Match again and create the property *)
		match property with
		
		(*** TODO: finish! ***)
		
	(*      | ParsingStructure.Action_precedence_acyclic _ -> AbstractModel.Action_precedence_acyclic (action_index1, action_index2), true
		| ParsingStructure.Action_precedence_cyclic _ -> AbstractModel.Action_precedence_cyclic (action_index1, action_index2), true
		| ParsingStructure.Action_precedence_cyclicstrict _ -> AbstractModel.Action_precedence_cyclicstrict (action_index1, action_index2), true*)
		(*** NOT IMPLEMENTED ***)
		(*			| ParsingStructure.Eventual_response_acyclic _ -> AbstractModel.Eventual_response_acyclic (action_index1, action_index2), true
						| ParsingStructure.Eventual_response_cyclic _ -> AbstractModel.Eventual_response_cyclic (action_index1, action_index2), true
						| ParsingStructure.Eventual_response_cyclicstrict _ -> AbstractModel.Eventual_response_cyclicstrict (action_index1, action_index2), true*)
		| _ -> raise (InternalError ("Impossible case while looking for properties with 2 actions; all cases should have been taken into account."))
		)
	in

	(* Generic check and conversion function for 2 actions and one deadline *)
	let gen_convert_2actd property a1 a2 d =
		(* Check action names and deadline (perform 3 even if one fails) *)
		let check1 = check_action_name index_of_actions a1 in
		let check2 = check_action_name index_of_actions a2 in
		let check3 = all_variables_defined_in_linear_expression variable_names constants d in
		let check4 = (if no_variables_in_linear_expression index_of_variables type_of_variables constants d
					then true
					else (print_error("No variable is allowed in the property definition (only constants and parameters)."); false))
		in
		if not (check1 && check2 && check3 && check4)
		then (None , false)
		else (
		(* Get action indexes *)
		let action_index1 = Hashtbl.find index_of_actions a1 in
		let action_index2 = Hashtbl.find index_of_actions a2 in
		(* Convert deadline *)
		let d = LinearConstraint.cast_p_of_pxd_linear_term (linear_term_of_linear_expression index_of_variables constants d) true in


		(*** BADPROG (but couldn't see how to do better!) ***)

		(* Match again and create the property *)
		match property with
		
		(*** TODO: finish! ***)
		
(*		| ParsingStructure.TB_Action_precedence_acyclic _ -> AbstractModel.TB_Action_precedence_acyclic (action_index1, action_index2, d), true
		| ParsingStructure.TB_Action_precedence_cyclic _ -> AbstractModel.TB_Action_precedence_cyclic (action_index1, action_index2, d), true
		| ParsingStructure.TB_Action_precedence_cyclicstrict _ -> AbstractModel.TB_Action_precedence_cyclicstrict (action_index1, action_index2, d), true
		| ParsingStructure.TB_response_acyclic _ -> AbstractModel.TB_response_acyclic (action_index1, action_index2, d), true
		| ParsingStructure.TB_response_cyclic _ -> AbstractModel.TB_response_cyclic (action_index1, action_index2, d), true
		| ParsingStructure.TB_response_cyclicstrict _ -> AbstractModel.TB_response_cyclicstrict (action_index1, action_index2, d), true*)
		| _ -> raise (InternalError ("Impossible case while looking for properties with 2 actions and a deadline; all cases should have been taken into account."))
		)
	in

	(* Generic check and conversion function for a list of actions *)
	let gen_convert_list property actions_list =
		(* Check action names (use a fold_left instead of forall to ensure that all actions will be checked) *)
		if not (List.fold_left (fun current_result a -> check_action_name index_of_actions a && current_result) true actions_list)
		then (None , false)
		else (
		(* Get action indexes *)
		let action_index_list = List.map (Hashtbl.find index_of_actions) actions_list in
		(*** BADPROG (but couldn't see how to do better!) ***)
		(* Match again and create the property *)
		match property with
		
		(*** TODO: finish! ***)
		
(*		| ParsingStructure.Sequence_acyclic _ -> AbstractModel.Sequence_acyclic action_index_list, true
		| ParsingStructure.Sequence_cyclic _ -> AbstractModel.Sequence_cyclic action_index_list, true*)
		| _ -> raise (InternalError ("Impossible case while looking for properties with a sequence; all cases should have been taken into account."))
		)
	in


	(* Convert *)
	match parsed_property_option with
	(* No property, no observer *)
	| None -> None, None
	
	(* Some property *)
	| Some parsed_property ->
		let property , converted_observer_structure_option =
		match parsed_property.property with

		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Parsed_EF parsed_state_predicate ->
			(* Return a property and no observer *)
			EF (convert_parsed_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
			
		(* Safety *)
		| Parsed_AGnot parsed_state_predicate ->
			(* Return a property and no observer *)
			AGnot (convert_parsed_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
		
		
		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)
		
		(* Reachability with minimization of a parameter valuation *)
		| Parsed_EFpmin (parsed_state_predicate , parameter_name) ->
			EFpmin (
				convert_parsed_state_predicate useful_parsing_model_information parsed_state_predicate
				,
				Hashtbl.find useful_parsing_model_information.index_of_variables parameter_name
			)
			,
			None
		
		(* Reachability with minimal-time *)
		| Parsed_EFtmin parsed_state_predicate ->
			EFtmin (convert_parsed_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
		
		
		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)
		
		(** Infinite-run (cycle) *)
		| Parsed_Cycle -> Cycle, None

		(** Accepting infinite-run (cycle) *)
		| Parsed_Acc_Cycle parsed_state_predicate ->
			Accepting_cycle (convert_parsed_state_predicate useful_parsing_model_information parsed_state_predicate)
			,
			None
		
		
		(*------------------------------------------------------------*)
		(* Inverse method, trace preservation, robustness *)
		(*------------------------------------------------------------*)
		
		(* Inverse method with complete, non-convex result *)
		| Parsed_IM parsed_pval ->
			IM (convert_parsed_pval useful_parsing_model_information parsed_pval)
			,
			None
			
			

			
		(*** TODO ***)
(*		| Parsed_Action_deadline _
		| _
			->
			raise (NotImplemented "ModelConverter.convert_property_option")*)
		
		
		
		(*
		(* CASE NON-REACHABILITY *)
		| Parsed_unreachable_locations parsed_unreachable_global_location_list (*(automaton_name , location_name)*) ->
			(* Global flag for checks *)
			let checks_passed = ref true in
			(* Check and convert each global location *)
			let unreachable_global_location_list = List.map (fun parsed_unreachable_global_location ->
				(* Check and convert this parsed_unreachable_global_location *)
				let unreachable_global_location , checked = check_and_convert_unreachable_global_location index_of_variables type_of_variables discrete variable_names index_of_automata index_of_locations parsed_unreachable_global_location in
				(* Update global variable *)
				checks_passed := !checks_passed && checked;
				(* Keep abstract global location *)
				unreachable_global_location
			) parsed_unreachable_global_location_list
			in
			(* Return abstract structure and flag for checks *)
			Unreachable_locations unreachable_global_location_list , !checks_passed


		(* CASE TWO ACTIONS *)
		(* if a2 then a1 has happened before *)
		| ParsingStructure.Action_precedence_acyclic ( a1 , a2 )
		(* everytime a2 then a1 has happened before *)
		| ParsingStructure.Action_precedence_cyclic ( a1 , a2 )
		(* everytime a2 then a1 has happened exactly once before *)
		| ParsingStructure.Action_precedence_cyclicstrict ( a1 , a2 )
			(*** NOT IMPLEMENTED ***)
			(*		(* if a1 then eventually a2 *)
					| ParsingStructure.Eventual_response_acyclic ( a1 , a2 )
					(* everytime a1 then eventually a2 *)
					| ParsingStructure.Eventual_response_cyclic ( a1 , a2 )
					(* everytime a1 then eventually a2 once before next *)
					| ParsingStructure.Eventual_response_cyclicstrict ( a1 , a2 )*)
			-> gen_check_and_convert_2act property a1 a2

		(* CASE ACTION + DEADLINE *)
		| ParsingStructure.Action_deadline ( a , d )
			->
			(* Check action name and deadline (perform 2 even if one fails) *)
			let check1 = check_action_name index_of_actions a in
			let check2 = all_variables_defined_in_linear_expression variable_names constants d in
			let check3 = all_variables_defined_in_linear_expression variable_names constants d in
			let check4 = (if no_variables_in_linear_expression index_of_variables type_of_variables constants d
						then true
						else (print_error("No variable is allowed in the property definition (only constants and parameters)."); false))
			in
			if not (check1 && check2 && check3 && check4) then (None , false)
			else (
			(* Get action indexes *)
			let action_index = Hashtbl.find index_of_actions a in
			(* Convert deadline *)
			let d = LinearConstraint.cast_p_of_pxd_linear_term (linear_term_of_linear_expression index_of_variables constants d) true in
			AbstractModel.Action_deadline ( action_index , d ), true
			)

		(* CASE 2 ACTIONS + DEADLINE *)
		(* if a2 then a1 happened within d before *)
		| ParsingStructure.TB_Action_precedence_acyclic (a1, a2, d)
		(* everytime a2 then a1 happened within d before *)
		| ParsingStructure.TB_Action_precedence_cyclic (a1, a2, d)
		(* everytime a2 then a1 happened once within d before *)
		| ParsingStructure.TB_Action_precedence_cyclicstrict (a1, a2, d)
		(* if a1 then eventually a2 within d *)
		| ParsingStructure.TB_response_acyclic (a1, a2, d)
		(* everytime a1 then eventually a2 within d *)
		| ParsingStructure.TB_response_cyclic (a1, a2, d)
		(* everytime a1 then eventually a2 within d once before next *)
		| ParsingStructure.TB_response_cyclicstrict (a1, a2, d)
			-> gen_check_and_convert_2actd property a1 a2 d

		(* CASE SEQUENCES (list of actions) *)
		(* sequence: a1, …, an *)
		| ParsingStructure.Sequence_acyclic (actions_list)
		(* sequence: always a1, …, an *)
		| ParsingStructure.Sequence_cyclic (actions_list)
			-> gen_check_and_convert_list property actions_list

		(*		(* Otherwise : error ! *)
					| _ -> raise (InternalError ("In the bad definition, not all possibilities are implemented yet."))*)
		end
	(*	in
		[n action_index], !well_formed*)

	*)
	
		in
		
		(* Get the synthesis or emptiness type *)
		let synthesis_type = convert_synthesis_type parsed_property.synthesis_type in
		
		(* Get the projection *)
		
		(*** TODO: !!! ***)
		
		let projection = None in
		
		(* Return the property *)
		Some {
			(* Emptiness or synthesis *)
			synthesis_type	= synthesis_type;
			(* Property *)
			property		= property;
			(* Projection of the result *)
			projection		= projection;
		}
		,
		converted_observer_structure_option
*)

(*------------------------------------------------------------*)
(** Convert a list of parsed parameters into a list of variable_index *)
(*------------------------------------------------------------*)
let convert_projection_definition (index_of_variables : (Automaton.variable_name, Automaton.variable_index) Hashtbl.t)  = function
	(* No property *)
	| None -> None
	(* Some property *)
	| Some (parsed_property : parsed_property) ->
	begin
		match parsed_property.projection with
		| None -> None
		| Some (parsed_parameters : string list) -> Some (List.map (fun (parsed_parameter_name : string) ->
			(* No check because this was checked before *)
			Hashtbl.find index_of_variables parsed_parameter_name
			) parsed_parameters)
	end





(************************************************************)
(************************************************************)
(** MODEL AND PROPERTY CONVERSION *)
(************************************************************)
(************************************************************)



(*------------------------------------------------------------*)
(* Convert the parsed model and the parsed property into an abstract model and an abstract property *)
(*------------------------------------------------------------*)
let abstract_structures_of_parsing_structures options (parsed_model : ParsingStructure.parsed_model) (parsed_property_option : ParsingStructure.parsed_property option) : AbstractModel.abstract_model * (AbstractProperty.abstract_property option) =
	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug functions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug print function for arrays *)
	let debug_print_array verbose_level =
		Array.iteri (fun i e ->
			print_message verbose_level ((string_of_int i) ^ " -> " ^ e)
		)
	in

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get names *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	(* Get the parsed structure *)
	let variables_declarations = parsed_model.variable_declarations in
	
	(* Get the constants and unassigned constants *)
	let constants : (variable_name * constant_value) list	= variables_declarations.constants in
	let unassigned_constants : variable_name list			= variables_declarations.unassigned_constants in
	let variables_per_type : (parsed_var_type , variable_name list) Hashtbl.t = variables_declarations.variables_per_type in
	
	(* Get the declared variable names *)
(* 	let collected_variable_names = get_declared_variable_names parsed_model.variable_declarations in *)

	let possibly_multiply_defined_clock_names		: variable_name list = try (Hashtbl.find variables_per_type Parsed_var_type_clock) with Not_found -> [] in
	let possibly_multiply_defined_parameter_names	: variable_name list = try (Hashtbl.find variables_per_type Parsed_var_type_parameter) with Not_found -> [] in

	(* Discrete request a slightly more expensive treatment as we get them for each type *)
	let possibly_multiply_defined_discrete_names	: variable_name list = get_all_discrete_variables variables_per_type in

	(* Get the declared automata names *)
	let declared_automata_names = get_declared_automata_names parsed_model.automata in
	(* Get the declared synclabs names *)
	let action_names = get_declared_synclabs_names parsed_model.automata in



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the synclabs declarations *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let action_names, removed_action_names = if options#sync_auto_detection then action_names, [] else (
		(* Keep only the synclabs which are used in ALL the automata where they are declared *)
		List.partition (synclab_used_everywhere parsed_model.automata) (*(fun synclab_name -> if synclab_used_everywhere parsed_model.automata synclab_name then
			(* If it is used everywhere: keep *)
			true
			(* If there exists an automaton where it is not used : warns and remove *)
			else (print_warning ("The synclab '" ^ synclab_name ^ "' is not used in some of the automata where it is declared: it will thus be removed."); false)
		)*) action_names
	) in

	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Make the array of constants *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let (constants : (Automaton.variable_name , NumConst.t) Hashtbl.t), constants_consistent = make_constants constants in

	if verbose_mode_greater Verbose_high then(
		(* Constants *)
		print_message Verbose_high ("\n*** Constants:");
		Hashtbl.iter (fun key value ->
			print_message Verbose_total (key ^ " = " ^ (NumConst.string_of_numconst value) ^ "")
		) constants;
	);



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the variable_declarations *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that all variable names are different (and print warnings for multiply-defined variables if same type) *)
	let all_variables_different = check_variable_names variables_per_type constants in
	
	(* Check that all automata names are different *)
	let all_automata_different = check_declared_automata_names declared_automata_names in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Errors if unassigned constants *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let check_no_unassigned_constants =
		if unassigned_constants = [] then true
		else
		(
			List.iter (fun unassigned_constant ->
				print_error("Constant '" ^ unassigned_constant ^ "' is not assigned a value in the variable declarations.");
			) unassigned_constants;
			(* Check failed *)
			false;
		)
	in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Exit if not well formed *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that at least one automaton is defined *)
	let at_least_one_automaton =
		if List.length declared_automata_names = 0 then (
			print_error ("At least one automaton should be declared."); false
		) else true in

	(* Stop here if model not well formed *)
 	if not (constants_consistent && all_variables_different && all_automata_different && at_least_one_automaton) then raise InvalidModel;
 	

 	
 	
 	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add clock and automaton for the observer *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
 	
 	let observer_automaton, observer_clock_option = None, None in

(*
	(*** TODO: reintroduce observers! ***)
(* Note that the observer has not been checked yet, but it doesn't matter *)
	let observer_automaton, observer_clock_option = match parsed_property_option with
		| None -> None, None
		| Some parsed_property -> ObserverPatterns.new_elements parsed_property
	in

	(* Print some information *)
	if verbose_mode_greater Verbose_high then(
		begin
			match observer_automaton with
			| None -> ()
			| Some observer_automaton -> print_message Verbose_high ("Adding extra automaton '" ^ observer_automaton ^ "' for the observer.");
		end;
		begin
			match observer_clock_option with
			| None -> ()
			| Some observer_clock_name -> print_message Verbose_high ("Adding extra clock '" ^ observer_clock_name ^ "' for the observer.");
		end;
	);

	
	(* Print some information *)
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("Automata names : " ^ (string_of_list_of_string_with_sep ", " declared_automata_names));
	);*)


	

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Start building variable lists *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	let clock_names, discrete_names, parameter_names, removed_variable_names = build_variables_lists parsed_model parsed_property_option options possibly_multiply_defined_clock_names possibly_multiply_defined_discrete_names possibly_multiply_defined_parameter_names in


 	(*------------------------------------------------------------*)
	(* Special clocks *)
	(*------------------------------------------------------------*)

	(* First handle the observer clock if any *)
	let observer_clock_list = match observer_clock_option with
		| None -> []
		| Some observer_clock_name -> [observer_clock_name]
	in
	
	(* Second handle the special_reset_clock *)
	
	
	(*** TODO: will depend on the actual algorithm ***)
	
	
	let with_special_reset_clock = false in
	
	let special_reset_clock_list = if with_special_reset_clock then [Constants.special_reset_clock_name] else [] in
	

	(*------------------------------------------------------------*)
	(* Create lists *)
	(*------------------------------------------------------------*)
	
	let clock_names = list_append (list_append clock_names observer_clock_list) special_reset_clock_list in
	let discrete_names = discrete_names in

	(* Make only one list for all variables *)
	let variable_names = list_append (list_append parameter_names clock_names) discrete_names in

	(* Update automata names with the observer automaton *)
	let declared_automata_names = match observer_automaton with
		| None -> declared_automata_names
		| Some automaton_obs -> list_append declared_automata_names [automaton_obs]
	in

	(* Numbers *)
	let nb_automata		= List.length declared_automata_names in
	let nb_actions		= List.length action_names in
	let nb_clocks		= List.length clock_names in
	let nb_discrete		= List.length discrete_names in
	let nb_parameters	= List.length parameter_names in
	let nb_variables	= List.length variable_names in


	(* Compute the index for the observer automaton *)
	let observer_automaton = match observer_automaton with
		| None -> None
		| Some _ -> Some (nb_automata-1)
	in



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the LinearConstraint dimensions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("\nSetting dimensions…");
	LinearConstraint.set_dimensions nb_parameters nb_clocks;
	
	
 	raise (NotImplemented "ModelConverted")
 	(*
	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the parameter dimensions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*** NOTE: must be done one and exactly one time ***)

	(* Set dimensions for hyper rectangles *)
	HyperRectangle.set_dimensions nb_parameters;

	(* Set dimensions for parameter valuations *)
	PVal.set_dimensions nb_parameters;




	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the arrays of automata, variables and actions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* The list of automata *)
	let automata = list_of_interval 0 (nb_automata - 1) in

	(* The array of automata names ; index -> automaton name *)
	let array_of_automata_names = Array.of_list declared_automata_names in
	(* A (constant) hash table 'automaton name -> index' *)
	let index_of_automata : (Automaton.automaton_name, Automaton.automaton_index) Hashtbl.t = Hashtbl.create nb_automata in
	for i = 0 to nb_automata - 1 do
		Hashtbl.add index_of_automata array_of_automata_names.(i) i;
	done;

	(* Functional version *)
	let (automata_names : automaton_index -> automaton_name) = fun automaton_index -> array_of_automata_names.(automaton_index) in

	(* The array of actions ; index -> action name *)
	let actions : action_name array = Array.of_list action_names in
	(* A (constant) hash table 'action name -> index' *)
	let index_of_actions = Hashtbl.create nb_actions in
	for i = 0 to nb_actions - 1 do
		Hashtbl.add index_of_actions actions.(i) i;
	done;

	(* The array of variables names ; index -> variable name *)
	let variables = Array.of_list variable_names in
	(* A (constant) hash table 'variable name -> index' *)
	let index_of_variables : (Automaton.variable_name, Automaton.variable_index) Hashtbl.t = Hashtbl.create nb_variables in
	for i = 0 to nb_variables - 1 do
		Hashtbl.add index_of_variables variables.(i) i;
	done;

	let first_parameter_index = 0 in
	let first_clock_index    = first_parameter_index + nb_parameters in
	let first_rational_index  = first_clock_index + nb_clocks in

	(* An array 'variable index -> AbstractModel.var_type' *)
	let type_of_variables = Array.make nb_variables AbstractModel.Var_type_parameter in
	for i = first_clock_index to first_rational_index - 1 do
		type_of_variables.(i) <- AbstractModel.Var_type_clock;
	done;
	for i = first_rational_index to nb_variables - 1 do
		type_of_variables.(i) <- AbstractModel.Var_type_discrete (get_type_of_rational_variable variables variables_per_type i);
	done;
	(* Functional representation *)
	let type_of_variables = fun variable_index -> type_of_variables.(variable_index) in

	(* Create the lists of different variables *)
	let (parameters : parameter_index list)	= list_of_interval first_parameter_index (first_clock_index - 1) in
	let (clocks : clock_index list)			= list_of_interval first_clock_index (first_rational_index - 1) in
	let (discrete : discrete_index list)	= list_of_interval first_rational_index (nb_variables - 1) in

	(* Create the type check functions *)
	let is_clock = (fun variable_index -> try (type_of_variables variable_index = Var_type_clock) with Invalid_argument _ ->  false) in
	let is_discrete = (fun variable_index -> try (match type_of_variables variable_index with | Var_type_discrete _ -> true | _ -> false) with Invalid_argument _ ->  false) in


	(* Detect the clock with a special global time name, if any *)
	let global_time_clock =
		if List.mem Constants.global_time_clock_name clock_names then Some (Hashtbl.find index_of_variables Constants.global_time_clock_name)
		else None
	in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug prints *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Automata *)
	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("\n*** Array of automata names:");
		debug_print_array Verbose_high array_of_automata_names;

		(* Actions *)
		print_message Verbose_high ("\n*** Array of declared synchronization action names:");
		debug_print_array Verbose_high actions;

		(* Variables *)
		print_message Verbose_total ("\n*** Variable names:");
		Array.iteri (fun i e ->
			print_message Verbose_total ((string_of_int i) ^ " -> " ^ e ^ " : " ^ (ModelPrinter.string_of_var_type (type_of_variables i)))
		) variables;
	);


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get all the locations *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that all the location names of an automaton are different *)
	if not (all_locations_different parsed_model.automata) then raise InvalidModel;

	(* Get all the locations for each automaton: automaton_index -> location_index -> location_name *)
	let (array_of_location_names : location_name array array) = make_locations_per_automaton index_of_automata parsed_model.automata nb_automata in
	(* Add the observer locations *)
	begin
	match observer_automaton with
		| None -> ()
			(*** WARNING: we assume here that observer automaton is the last one ! ***)
		| Some automaton_index ->
			(* Get the property *)
			let parsed_property = match parsed_property_option with
				| None -> raise (InternalError "A property must be defined at this point since we have an observer automaton")
				| Some parsed_property -> parsed_property
			in
			print_message Verbose_high ("Adding the observer locations.");
			array_of_location_names.(automaton_index) <- ObserverPatterns.get_locations parsed_property
	end;

	(* A (constant) array of hash tables 'automaton_index -> location_name -> location_index' *)
	let index_of_locations = Array.make nb_automata (Hashtbl.create 0) in
	for automaton_index = 0 to nb_automata - 1 do
		let nb_locations = Array.length (array_of_location_names.(automaton_index)) in
		let location_index_table = Hashtbl.create nb_locations in
		for location_index = 0 to nb_locations - 1 do
			Hashtbl.add location_index_table array_of_location_names.(automaton_index).(location_index) location_index;
		done;
		index_of_locations.(automaton_index) <- location_index_table;
	done;
	(* Create the access function returning a list of locations*)
	let array_of_locations_per_automaton = Array.make nb_automata [] in
	for automaton_index = 0 to nb_automata - 1 do
		array_of_locations_per_automaton.(automaton_index) <-
		Array.to_list (Array.mapi (fun location_index _ -> location_index) array_of_location_names.(automaton_index));
	done;
	let (locations_per_automaton : automaton_index -> location_index list) = fun automaton_index -> array_of_locations_per_automaton.(automaton_index) in
	(* Create the access function returning a location name *)
	let location_names = fun automaton_index location_index -> array_of_location_names.(automaton_index).(location_index) in


	(* Debug print *)
	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("\n*** Locations per automaton:");
		List.iter (fun automaton_index ->
			print_message Verbose_high ((automata_names automaton_index) ^ " : ");
			List.iter (fun location_index ->
				print_message Verbose_high ("    " ^ (string_of_int location_index) ^ " -> " ^ (location_names automaton_index location_index) ^ "");
			)
			(locations_per_automaton automaton_index);
		) automata;
	);



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create useful parsing structure, used in subsequent functions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	let useful_parsing_model_information = {
		actions						= actions;
		array_of_location_names		= array_of_location_names;
		automata_names				= automata_names;
		automata					= automata;
		constants					= constants;
		discrete					= discrete;
		index_of_actions			= index_of_actions;
		index_of_automata			= index_of_automata;
		index_of_locations			= index_of_locations;
		index_of_variables			= index_of_variables;
		nb_parameters				= nb_parameters;
		parameter_names				= parameter_names;
		removed_action_names		= removed_action_names;
		type_of_variables			= type_of_variables;
		variable_names				= variable_names;
		variables					= variables;
		removed_variable_names		= removed_variable_names;
	} in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the automata *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("*** Checking automata…");
	let well_formed_automata = check_automata useful_parsing_model_information parsed_model.automata in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* exit if not well formed *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: might be a problem in the init checking if the check_automata test fails, hence test first ***)
	if not (check_no_unassigned_constants && well_formed_automata)
		then raise InvalidModel;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the init_definition *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("*** Checking init definition…");
	(* Get pairs for the initialisation of the discrete variables, and check the init definition *)

	let init_rational_pairs, well_formed_init = check_init useful_parsing_model_information parsed_model.init_definition observer_automaton in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check projection definition *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let well_formed_projection = match parsed_property_option with
		| None -> true
		| Some parsed_property -> check_projection_definition useful_parsing_model_information parsed_property.projection
	in


(*	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check optimization definition *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let well_formed_optimization = check_optimization parameter_names parsed_optimization_definition in*)



	

	(*	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check polyhedra definition in (optional) carto mode *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let well_formed_carto = ref true in

*)

	(*** TODO!!! reintroduce differently ***)
(*	let parsed_constraints , (p1_min , p1_max) , (p2_min , p2_max)  = parsed_carto_definition in
	let carto_linear_constraints = List.map (fun (parsed_convex_predicate , tile_nature) ->
		(* Check well-formedness *)
		if all_variables_defined_in_convex_predicate variable_names constants parsed_convex_predicate then(
			(* Convert to a AbstractModel.linear_constraint *)
			LinearConstraint.cast_p_of_pxd_linear_constraint (linear_constraint_of_convex_predicate index_of_variables constants parsed_convex_predicate) true
			,
			(* Convert the tile nature *)
			convert_tile_nature tile_nature
		)else(
			(* Set well-formedness to false *)
			well_formed_carto := false;
			(* Return anything *)
			LinearConstraint.p_false_constraint () , AbstractModel.Unknown
		)
	) parsed_constraints in*)
	(*** TODO!!! reintroduce differently ***)


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check property definition *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING: might be a problem if the check_automata test fails ***)
	let well_formed_property = check_property_option useful_parsing_model_information parsed_property_option in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* exit if not well formed *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	if not (check_no_unassigned_constants && well_formed_automata && well_formed_projection && well_formed_init && well_formed_property)
		then raise InvalidModel;

	print_message Verbose_medium ("Model syntax successfully checked.");


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the automata without the observer, and with the transitions in a non-finalized form *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_high ("*** Building automata…");
	(* Get all the possible actions for every location of every automaton *)
	let actions, array_of_action_names, action_types, actions_per_automaton, actions_per_location, location_acceptance, location_urgency, costs, invariants, stopwatches_array, has_stopwatches, transitions, nosync_obs = make_automata useful_parsing_model_information parsed_model.automata (observer_automaton != None) in
	let nb_actions = List.length actions in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create the abstract property from the parsed property *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	(* We may need to create additional structures for the observer, if any *)
	
	let abstract_property_option, converted_observer_structure_option = convert_property_option useful_parsing_model_information parsed_property_option in
	
	(* Convert some variables to catch up with older code below *)
	let observer_structure_option, nb_transitions_for_observer, initial_observer_constraint_option = match converted_observer_structure_option with
		| None -> None, 0, None
		| Some converted_observer_structure ->
			Some converted_observer_structure.observer_structure,
			converted_observer_structure.nb_transitions_for_observer,
			Some converted_observer_structure.initial_observer_constraint
	in
	
	
(* 	let observer_structure_option, nb_transitions_for_observer, initial_observer_constraint_option, abstract_property_option = convert_property_option useful_parsing_model_information parsed_property_option in *)

	



	

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Convert the transitions to their final form *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	print_message Verbose_high ("*** Building transitions…");
	(* Count the number of transitions *)
	let nb_transitions_without_observer =
		(* Iterate on automata *)
		Array.fold_left (fun nb_transitions_for_automata transitions_for_this_automaton ->
			(* Iterate on locations *)
			Array.fold_left (fun nb_transitions_for_locations transitions_for_this_location ->
				nb_transitions_for_locations + (List.length transitions_for_this_location)
			) nb_transitions_for_automata transitions_for_this_automaton
		) 0 transitions
	in
	let nb_transitions = nb_transitions_without_observer + nb_transitions_for_observer in
	
	(* Print some information *)
	print_message Verbose_total ("" ^ (string_of_int nb_transitions_without_observer) ^ " transition" ^ (s_of_int nb_transitions_without_observer) ^ " in the model, and " ^ (string_of_int nb_transitions_for_observer) ^ " additional transition" ^ (s_of_int nb_transitions_for_observer) ^ " for the observer");

	(* Convert transitions *)
	(*** TODO: integrate inside 'make_automata' (?) ***)
	let transitions, transitions_description, automaton_of_transition = convert_transitions nb_transitions nb_actions index_of_variables constants removed_variable_names type_of_variables transitions in

	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add the observer structure to the automata *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	begin
	match observer_automaton with
	| None -> ()
	| Some observer_id ->
		(* Get the info from the observer pattern *)
		begin
		match observer_structure_option with
		| None -> raise (InternalError ("No observer structure saved although it should have been set at that point"))
		
		(*** TODO: create a structure !!! ***)
		
		| Some (observer_actions, observer_actions_per_location, observer_location_urgency, observer_invariants, observer_transitions) ->
			print_message Verbose_high ("*** Adding observer data to automata…");
			(* Retrieve the number of locations of the observer *)
			let nb_locations = Array.length observer_actions_per_location in
			(* Update actions per automaton *)
			actions_per_automaton.(observer_id)	<- observer_actions;
			(* Update actions per location *)
			actions_per_location.(observer_id)	<- observer_actions_per_location;
			(* Update urgency *)
			location_urgency.(observer_id)		<- observer_location_urgency;
			(* Create the array of accepting locations for this automaton (for now: all non accepting) *)
			location_acceptance.(observer_id)	<- Array.make nb_locations Location_nonaccepting;
			(* Update invariants *)
			invariants.(observer_id)			<- observer_invariants;
			(* Update costs (no costs in observers) *)
			costs.(observer_id)					<- Array.make nb_locations None;
			
			(* Update transitions *)
			
			(* First convert the transitions to transition_index, and update the transitions_description *)

			(* Maintain an index for the next transition *)
			let transition_index = ref nb_transitions_without_observer in
			let observer_transitions =
			(* Iterate on locations *)
			Array.map (fun actions_for_this_location ->
				(* Iterate on actions *)
				Array.map (fun transitions_for_this_action ->
					(* Iterate on transitions for this action *)
					List.map (fun transition ->
						let current_transition_index = !transition_index in
						(* Add the transition to the description *)
						begin
						try(
							(* Update transition description *)
							transitions_description.(current_transition_index) <- transition;
							(* Update automaton of transition *)
							automaton_of_transition.(current_transition_index) <- observer_id;
						) with
							| Invalid_argument e -> raise (InternalError ("Invalid argument '" ^ e ^ "' when updating observer transitions (current index: " ^ (string_of_int current_transition_index) ^ " max size: " ^ (string_of_int (Array.length transitions_description)) ^ ")"))
						;
						end;
						
						(* Increment the index *)
						transition_index := !transition_index + 1;

						(* Update the transition array *)
						current_transition_index
					) transitions_for_this_action;
				) actions_for_this_location;
			) observer_transitions
			in
			
			(* Then update transitions *)
			transitions.(observer_id) <- observer_transitions;
			
		end;
	end;
	
	(*** TODO : perform init for observer (location) ***)









	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Convert to functional view *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	print_message Verbose_high ("*** Converting to functional view…");

	let array_of_variable_names = Array.make nb_variables "" in
	(* Add normal names *)
	for variable_index = 0 to nb_variables - 1 do
		array_of_variable_names.(variable_index) <- variables.(variable_index);
	done;

	(* Create the functional representation *)
	let variable_names = fun i -> array_of_variable_names.(i) in
	(* Create the functional representation for action types *)
	let action_types = fun action_index -> action_types.(action_index) in
	(* Create the functional representation for the actions of every automaton *)
	let actions_per_automaton = fun automaton_index -> actions_per_automaton.(automaton_index) in
	(* Create the functional representation for the actions of every location of every automaton *)
	let actions_per_location = fun automaton_index location_index -> actions_per_location.(automaton_index).(location_index) in
	(* Invariants *)
	let invariants = fun automaton_index location_index -> invariants.(automaton_index).(location_index) in
	(* Accepting locations *)
	let is_accepting = fun automaton_index location_index -> location_acceptance.(automaton_index).(location_index) = Location_accepting in
	(* Urgency *)
	let is_urgent = fun automaton_index location_index -> location_urgency.(automaton_index).(location_index) = Location_urgent in
	(* Costs *)
	let costs = fun automaton_index location_index -> costs.(automaton_index).(location_index) in
	(* Stopwatches *)
	let stopwatches = (fun automaton_index location_index -> stopwatches_array.(automaton_index).(location_index)) in
	(* Transitions *)
	let transitions = fun automaton_index location_index action_index -> transitions.(automaton_index).(location_index).(action_index) in
	(* Transition description *)
	let transitions_description = fun transition_index -> transitions_description.(transition_index) in
	(* Automaton of transition *)
	let automaton_of_transition = fun transition_index -> automaton_of_transition.(transition_index) in
	
	(* Actions *)
	let action_names = fun action_index ->
		try (array_of_action_names.(action_index))
		with _ -> raise (InternalError ("Action index " ^ (string_of_int action_index) ^ " does not exist in the model."))
	in
	
	let nb_locations = List.fold_left (fun current_nb automaton -> current_nb + (List.length (locations_per_automaton automaton))) 0 automata in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the number of discrete variables *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let min_rational_index = first_rational_index in
	let max_rational_index = nb_variables - 1 in
	Location.initialize nb_automata min_rational_index max_rational_index;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the automata per action *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* List of automata for every action *)
	print_message Verbose_high ("*** Building automata per action…");
	let automata_per_action = make_automata_per_action actions_per_automaton nb_automata nb_actions in

(*	(* Convert the costs *)
	print_message Verbose_total ("*** Building costs (if any)…");
	let costs = convert_costs index_of_variables constants costs in*)

(*	(* Convert the invariants *)
	print_message Verbose_total ("*** Building invariants…");
	let invariants = convert_invariants index_of_variables constants invariants in*)

	(* Convert the stopwatches *)
(* 	print_message Verbose_total ("*** Building stopwatches…"); *)
(*	let stopwatches_fun = (fun automaton_index location_index -> stopwatches.(automaton_index).(location_index)) in*)


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Handling the special reset clock *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*** NOTE: if it is defined, then it is the last clock in the list of clocks ***)
	let special_reset_clock : clock_index option = if with_special_reset_clock then
		Some (List.nth clocks (nb_clocks - 1))
	else None
	in
	(* The list of clock indexes except the reset clock (used, e.g., to print the model *)
	let clocks_without_special_reset_clock = match special_reset_clock with
		| None -> clocks
		| Some clock_index -> list_remove_first_occurence clock_index clocks
	in

	(* Print metrics if verbose low or in any case in mode translation *)
	if verbose_mode_greater Verbose_low || (match options#imitator_mode with AbstractAlgorithm.Translation _ -> true | _ -> false) then(
		print_message Verbose_standard (
			(string_of_int nb_automata) ^ " automat" ^ (if nb_automata > 1 then "a" else "on")
			^ ", "
			^ (string_of_int nb_locations) ^ " location" ^ (s_of_int nb_locations) ^ ", "
			^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ ", "
			^ (string_of_int nb_actions) ^ " declared synchronization action" ^ (s_of_int nb_actions) ^ ", "
			^ (string_of_int nb_clocks) ^ " clock variable" ^ (s_of_int nb_clocks) ^ ", "
			^ (string_of_int nb_discrete) ^ " discrete variable" ^ (s_of_int nb_discrete) ^ ", "
			^ (string_of_int nb_parameters) ^ " parameter" ^ (s_of_int nb_parameters) ^ ", "
			^ (string_of_int nb_variables) ^ " variable" ^ (s_of_int nb_variables) ^ ", "
			^ (string_of_int (Hashtbl.length constants)) ^ " constant" ^ (s_of_int (Hashtbl.length constants)) ^ "."
		);
	);


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect the strongly deterministic nature of the PTA *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: strongly deterministic = for each PTA, for each location, for each VISIBLE (non-silent) action action, at most one outgoing transition with this action action ***)

	(* Print some information *)
	print_message Verbose_high ("*** Detecting the strongly-deterministic nature of the model…");

	(* By default: yes, it is strongly deterministic *)
	let strongly_deterministic = ref true in
	
	(* And now check for any counterexample *)
	(* For all PTA *)
	begin
	try(
	List.iter (fun automaton_index ->
		let locations_for_this_automaton = locations_per_automaton automaton_index in
		(* For all locations *)
		List.iter (fun location_index ->

			let actions_for_this_location = actions_per_location automaton_index location_index in
			(* For all actions *)
			List.iter (fun action_index ->
				let transitions_for_this_location = transitions automaton_index location_index action_index in
				(* If more than one transition with this action: not strongly deterministic *)
				(*** NOTE: we don't care about silent actions as, by construction, all silent actions are different and therefore their cardinality can only be 1 ***)
				if List.length transitions_for_this_location > 1 then(
					(* Write a message *)
					if verbose_mode_greater Verbose_high then(
						print_message Verbose_high ("This network of PTAs is not strongly deterministic: in automaton '" ^ (automata_names automaton_index) ^ "', in location '" ^ (location_names automaton_index location_index) ^ "', there are " ^ (string_of_int (List.length transitions_for_this_location)) ^ "outgoing transitions labeled with action '" ^ (action_names action_index) ^ "'.");
					);

					(* Update flag *)
					strongly_deterministic := false;
					
					(* Raise exception to not test further *)
					raise Not_strongly_deterministic;
				);
			) actions_for_this_location;
		) locations_for_this_automaton;
	) automata;
	) with
		(* Just to accelerate exit from the loops *)
		Not_strongly_deterministic -> ();
	;
	end;
	
	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect the presence of silent transitions *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Print some information *)
	print_message Verbose_high ("*** Detecting the presence of silent transitions in the model…");

	(*** NOTE: instead of enumerating all transitions, we just check for the presence of a silent action; if an action was created but unused, it should have been deleted ***)
	let has_silent_actions = List.exists (fun action_index ->
		action_types action_index = Action_type_nosync
	) actions in
	

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect the L/U nature of the PTA *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*** NOTE/HACK: duplicate function in StateSpace ***)
	let continuous_part_of_guard (*: LinearConstraint.pxd_linear_constraint*) = function
		| True_guard -> LinearConstraint.pxd_true_constraint()
		| False_guard -> LinearConstraint.pxd_false_constraint()
		| Discrete_guard discrete_guard -> LinearConstraint.pxd_true_constraint()
		| Continuous_guard continuous_guard -> continuous_guard
		| Discrete_continuous_guard rational_continuous_guard -> rational_continuous_guard.continuous_guard
	in

	(* 1) Get ALL constraints of guards and invariants *)
	
	(* Print some information *)
	print_message Verbose_total ("*** Retrieving all constraints to detect the L/U nature of the model…");
	
	(*** BADPROG ***)
	let all_constraints = ref [] in
	(* For all PTA *)
	List.iter (fun automaton_index ->
		let locations_for_this_automaton = locations_per_automaton automaton_index in
		(* For all locations *)
		List.iter (fun location_index ->
			let invariant = invariants automaton_index location_index in

			(* Add invariant *)
			all_constraints := invariant :: !all_constraints;

			let actions_for_this_location = actions_per_location automaton_index location_index in
			(* For all actions *)
			List.iter (fun action_index ->
				let transitions_for_this_location = List.map transitions_description (transitions automaton_index location_index action_index) in
				(* For all transitions *)
				List.iter (fun transition ->

					(* Add guard *)
					(*** NOTE: quite inefficient as we create a lot of pxd_true_constraint() although we just want to know whether they are L/U or not (but OK because prior to model analysis) ***)
					all_constraints := (continuous_part_of_guard transition.guard) :: !all_constraints;
				) transitions_for_this_location;
			) actions_for_this_location;
		) locations_for_this_automaton;
	) automata;

	(* 2) Try to partition parameters as L- and U- *)

	(* Print some information *)
	print_message Verbose_high ("*** Detecting the L/U nature of the model…");
	
	let lu_status =
	try(
		let l_parameters, u_parameters = LinearConstraint.partition_lu parameters !all_constraints in
		(*** TODO: check that all parameters are indeed partitioned (if some parameters appear nowhere, that might be a problem) ***)
		match l_parameters, u_parameters with
		| [] , [] -> PTA_notLU
		| _ , [] -> PTA_L
		| [] , _ -> PTA_U
		| l_parameters, u_parameters -> PTA_LU (l_parameters, u_parameters)
	) with Not_LU -> PTA_notLU
	in



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the initial state *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Print some information *)
	print_message Verbose_high ("*** Building initial state…");
	
	let (initial_location, initial_constraint) =
		make_initial_state index_of_automata array_of_location_names index_of_locations index_of_variables parameters removed_variable_names constants type_of_variables variable_names init_rational_pairs parsed_model.init_definition in

	(* Add the observer initial constraint *)
	begin
	match initial_observer_constraint_option with
		| None -> ()
		| Some c -> LinearConstraint.px_intersection_assign initial_constraint [c];
	end;



	(* Build the K0 constraint *)
	let initial_p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse initial_constraint in

	(* Print some information *)
	if verbose_mode_greater Verbose_low then(
		print_message Verbose_low ("Initial parameter constraint:");
		print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint variable_names initial_p_constraint);
	);


	(* Build the X >= 0 constraint *)
	let px_clocks_non_negative = LinearConstraint.px_constraint_of_nonnegative_variables clocks in

	(* Print some information *)
	if verbose_mode_greater Verbose_medium then(
		print_message Verbose_medium ("Constraint X >= 0:");
		print_message Verbose_medium (LinearConstraint.string_of_px_linear_constraint variable_names px_clocks_non_negative);
	);


	(* Build the X >= 0 ^ K0 constraint *)
	let px_clocks_non_negative_and_initial_p_constraint = LinearConstraint.px_intersection [px_clocks_non_negative; (LinearConstraint.px_of_p_constraint initial_p_constraint)] in

	(* Print some information *)
	if verbose_mode_greater Verbose_medium then(
		print_message Verbose_medium ("Constraint X >= 0 ^ K0:");
		print_message Verbose_medium (LinearConstraint.string_of_px_linear_constraint variable_names px_clocks_non_negative_and_initial_p_constraint);
	);


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Convert the projection definition *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Verbose_total ("*** Building the projection definition…");
	let projection = convert_projection_definition index_of_variables parsed_property_option in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug prints *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variables *)
	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("\n*** All variables:");
		for i = 0 to nb_variables - 1 do
			print_message Verbose_high ("  "
				^ (string_of_int i) ^ " : " ^ (variable_names i)
	(* 			^ (if is_renamed_clock i then " (renamed clock)" else "") *)
			);
		done;
	);

	if verbose_mode_greater Verbose_total then(
		(* Urgency of locations *)
		print_message Verbose_total ("\n*** Urgency and acceptance of locations:");
		(* For each automaton *)
		List.iter (fun automaton_index ->
			(* Print the automaton name *)
			print_message Verbose_total ("" ^ (automata_names automaton_index) ^ " :");
			(* For each location *)
			List.iter (fun location_index ->
				(* Get the urgency *)
				let urgency_string =
					if is_urgent automaton_index location_index then "URGENT" else "non-urgent"
				in
				(* Get the acceptance *)
				let acceptance_string =
					if is_accepting automaton_index location_index then "ACCEPTING" else "non-accepting"
				in
				print_message Verbose_total (" - " ^ (location_names automaton_index location_index) ^ " :" ^ urgency_string ^ "/" ^ acceptance_string);
			) (locations_per_automaton automaton_index);
		) automata;

		(* All action names *)
		print_message Verbose_total ("\n*** All action names:");
		(* For each action *)
		List.iter (fun action_index ->
			print_message Verbose_total ((string_of_int action_index) ^ " -> " ^ (action_names action_index));
		) actions;

		(* Debug print: actions per automaton *)
		print_message Verbose_total ("\n*** Actions per automaton:");
		(* For each automaton *)
		List.iter (fun automaton_index ->
			(* Get the actions *)
			let actions = actions_per_automaton automaton_index in
			(* Print it *)
			let actions_string = string_of_list_of_string_with_sep ", " (List.map action_names actions) in
			print_message Verbose_total ((automata_names automaton_index) ^ " : " ^ actions_string)
		) automata;

		(* Debug print: automata per action *)
		print_message Verbose_total ("\n*** Automata per action:");
		(* For each action *)
		List.iter (fun action_index ->
			(* Get the automata *)
			let automata = automata_per_action action_index in
			(* Print it *)
			let automata_string = string_of_list_of_string_with_sep ", " (List.map automata_names automata) in
			print_message Verbose_total ((action_names action_index) ^ " : " ^ automata_string)
		) actions;


		(* Possible actions per location *)
		print_message Verbose_total ("\n*** Possible actions per location:");
		(* For each automaton *)
		List.iter (fun automaton_index ->
			(* Print the automaton name *)
			print_message Verbose_total ("" ^ (automata_names automaton_index) ^ " :");
			(* For each location *)
			List.iter (fun location_index ->
				(* Get the actions *)
				let actions = actions_per_location automaton_index location_index in
				(* Print it *)
				let my_string = string_of_list_of_string_with_sep ", " (List.map action_names actions) in
				print_message Verbose_total (" - " ^ (location_names automaton_index location_index) ^ " :" ^ my_string);
			) (locations_per_automaton automaton_index);
		) automata;

		(* Debug print: transition indexes *)
		print_message Verbose_total ("\n*** Transitions:");
		(* For each transition *)
		for transition_index = 0 to nb_transitions - 1 do
			(* Get the automaton *)
			let automaton_index = automaton_of_transition transition_index in
			(* Get the transition *)
			let transition = transitions_description transition_index in
			(* Print automaton + action *)
				(*** TODO: print source too (and guard, and reset?!) ***)
			print_message Verbose_total ("Transition " ^ (string_of_int transition_index) ^ ": in automaton '" ^ (automata_names automaton_index) ^ "' via action '" ^ (action_names (transition.action)) ^ "' to location '" ^ (location_names automaton_index (transition.target)) ^ "'")
		done;
		
		print_message Verbose_total ("");
	);

	(* Debug print: L/U *)
	begin
	match lu_status with
	| PTA_notLU ->
		print_message Verbose_low ("This model is not an L/U-PTA.");

	| PTA_LU (l_parameters, u_parameters) ->
		print_message Verbose_standard ("This model is an L/U-PTA:");
		print_message Verbose_standard ("- lower-bound parameters {" ^ (string_of_list_of_string_with_sep ", " (List.map variable_names l_parameters)) ^ "}");
		print_message Verbose_standard ("- upper-bound parameters {" ^ (string_of_list_of_string_with_sep ", " (List.map variable_names u_parameters)) ^ "}");

	| PTA_L ->
		print_message Verbose_standard ("This model is an L-PTA.");

	| PTA_U ->
		print_message Verbose_standard ("This model is a U-PTA.");

	end;

	(* Debug print: special global clock *)
	begin
	match global_time_clock with
		| Some name -> print_message Verbose_standard ("A global time clock '" ^ Constants.global_time_clock_name ^ "' has been detected.");
		| None -> print_message Verbose_medium ("No global time clock '" ^ Constants.global_time_clock_name ^ "' detected.");
	end;

	
	(* Debug print: strong determinism *)
	if !strongly_deterministic then
		print_message Verbose_standard ("This PTA is strongly deterministic.")
	else
		print_message Verbose_medium ("This PTA is not strongly deterministic.")
	;

	
	(* Debug print: silent transitions *)
	if has_silent_actions then
		print_message Verbose_low ("Silent actions detected.")
	else
		print_message Verbose_standard ("No silent action detected.")
	;



	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** HACK (big big hack): save some data structures to be used by the parsing and checking of additional file (pi0 or v0), if any ***)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(*	saved_index_of_variables	:= Some index_of_variables;
	saved_nb_parameters			:= Some nb_parameters;
	saved_parameters			:= Some parameters;
	saved_parameter_names		:= Some parameter_names;
	saved_variables				:= Some variables;*)


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Build the final structure *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	{
	(* Cardinality *)
	nb_automata    = nb_automata;
	nb_actions     = nb_actions;
	nb_clocks      = nb_clocks;
	nb_discrete    = nb_discrete;
	nb_parameters  = nb_parameters;
	nb_variables   = nb_variables;
	nb_locations   = nb_locations;
	nb_transitions = nb_transitions;

	(* Is there any stopwatch in the model? *)
	has_stopwatches = has_stopwatches;
	(* Is the model an L/U-PTA? *)
	lu_status = lu_status;
	(* Is the model a strongly deterministic PTA? *)
	strongly_deterministic = !strongly_deterministic;
	(* Does the model contain any transition labeled by a silent, non-observable action? *)
	has_silent_actions = has_silent_actions;


	(* The observer *)
	observer_pta = observer_automaton;
	is_observer = (fun automaton_index ->
		match observer_automaton with
		| Some pta -> pta = automaton_index
		| None -> false
	);

	(* The list of clock indexes *)
	clocks = clocks;
	(* True for clocks, false otherwise *)
	is_clock = is_clock;
	(* Index of the special clock to be reset at each transition to measure time elapsing (only used in NZ checking) *)
	special_reset_clock = special_reset_clock;
	(* Index of a special clock meant to measure the global time (how this clock is actually used is up to the model designer *)
	global_time_clock = global_time_clock;
	(* The list of clock indexes except the reset clock (used, e.g., to print the model *)
	clocks_without_special_reset_clock = clocks_without_special_reset_clock;
	(* The list of discrete indexes *)
	discrete = discrete;
	(* True for discrete, false otherwise *)
	is_discrete = is_discrete;
	(* The list of parameter indexes *)
	parameters = parameters;
	(* The non parameters (clocks and discrete) *)
	clocks_and_discrete = list_append clocks discrete;
	(* The non clocks (parameters and discrete) *)
	parameters_and_discrete = list_append parameters discrete;
	(* The non discrete (clocks and parameters) *)
	parameters_and_clocks = list_append parameters clocks;
	(* The function : variable_index -> variable name *)
	variable_names = variable_names;
	(* The type of variables *)
	type_of_variables = type_of_variables;

	(* The automata *)
	automata = automata;
	(* The automata names *)
	automata_names = automata_names;

	(* The locations for each automaton *)
	locations_per_automaton = locations_per_automaton;
	(* The location names for each automaton *)
	location_names = location_names;
	(* The location attributes for each automaton *)
	is_accepting = is_accepting;
	is_urgent = is_urgent;

	(* All action indexes *)
	actions = actions;
	(* Action names *)
	action_names = action_names;
	(* The type of actions *)
	action_types = action_types;
	(* The list of actions for each automaton *)
	actions_per_automaton = actions_per_automaton;
	(* The list of automatons for each action *)
	automata_per_action = automata_per_action;
	(* The list of actions for each automaton for each location *)
	actions_per_location = actions_per_location;

	(* The cost for each automaton and each location *)
	costs = costs;

	(* The invariant for each automaton and each location *)
	invariants = invariants;
	(* The transitions for each automaton and each location and each action *)
	transitions = transitions;
	(* The list of clocks stopped for each automaton and each location *)
	stopwatches = stopwatches;
	(* An array transition_index -> transition *)
	transitions_description = transitions_description;
	(* An array transition_index -> automaton_index *)
	automaton_of_transition = automaton_of_transition;

	(* All clocks non-negative *)
	px_clocks_non_negative = px_clocks_non_negative;
	(* Initial location of the model *)
	initial_location = initial_location;
	(* Initial constraint of the model *)
	initial_constraint = initial_constraint;
	(* Initial constraint of the model projected onto P *)
	initial_p_constraint = initial_p_constraint;
	(* Initial constraint of the model projected onto P and all clocks non-negative *)
	px_clocks_non_negative_and_initial_p_constraint = px_clocks_non_negative_and_initial_p_constraint;

	}
	
(*	,
	
	(* Also return the useful parsing information (used for property parsing) *)
	{
		constants         : constants;
		index_of_automata : index_of_automata;
		index_of_variables: index_of_variables;
	}*)


	,
	
	abstract_property_option
	(*** TODO: handle property! ***)

	(*type abstract_property = {
	(* Emptiness or synthesis *)
	synthesis_type	: synthesis_type;
	(* Property *)
	property		: property;
	(* Projection of the result *)
	projection		: projection;
}*)
	*)



(************************************************************)
(************************************************************)
(** Property conversion *)
(************************************************************)
(************************************************************)
(*
(*------------------------------------------------------------*)
(* Check the property w.r.t. the model variables *)
(*------------------------------------------------------------*)

(* Check correct variable names in parsed_rational_arithmetic_expression *)
	
(* Check correct variable names in parsed_loc_predicate *)
let check_parsed_loc_predicate useful_parsing_model_information = function
	| Parsed_loc_predicate_EQ (automaton_name, location_name)
	| Parsed_loc_predicate_NEQ (automaton_name, location_name)
	->
		(* Check automaton name *)
		let check_automaton = Hashtbl.mem useful_parsing_model_information.index_of_automata automaton_name in
		if not check_automaton then (
			print_error ("Automaton name '" ^ automaton_name ^ "' undefined in the property");
		);
		(* Check location name *)
		let check_location = Hashtbl.mem useful_parsing_model_information.index_of_locations.(automaton_index) location_name in
		if not check_location then (
			print_error ("Location name '" ^ location_name ^ "' undefined in the property");
		);
		(* Return *)
		check_automaton && check_location


(* Check correct variable names in parsed_simple_predicate *)
let check_parsed_simple_predicate useful_parsing_model_information = function
	| Parsed_rational_boolean_expression parsed_rational_boolean_expression -> check_parsed_rational_boolean_expression useful_parsing_model_information parsed_rational_boolean_expression
	| Parsed_loc_predicate parsed_loc_predicate -> check_parsed_loc_predicate useful_parsing_model_information parsed_loc_predicate


(* Check correct variable names in parsed_state_predicate *)
let rec check_parsed_state_predicate_factor useful_parsing_model_information = function
	| Parsed_state_predicate_factor_NOT parsed_state_predicate_factor -> check_parsed_state_predicate_factor useful_parsing_model_information parsed_state_predicate_factor
	| Parsed_simple_predicate parsed_simple_predicate -> check_parsed_simple_predicate useful_parsing_model_information parsed_simple_predicate
	| Parsed_state_predicate parsed_state_predicate -> check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate

and check_parsed_state_predicate_term useful_parsing_model_information = function
	| Parsed_state_predicate_term_AND (parsed_state_predicate_term1, parsed_state_predicate_term2) ->
		let check1 = check_parsed_state_predicate_term useful_parsing_model_information parsed_state_predicate_term1 in
		let check2 = check_parsed_state_predicate_term useful_parsing_model_information parsed_state_predicate_term2 in
		check1 && check2
	| Parsed_state_predicate_factor parsed_state_predicate_factor -> check_parsed_state_predicate_factor useful_parsing_model_information parsed_state_predicate_factor

and check_parsed_state_predicate useful_parsing_model_information = function
	| Parsed_state_predicate_OR (parsed_state_predicate1, parsed_state_predicate2) ->
		let check1 = check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate1 in
		let check2 = check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate2 in
		check1 && check2
	| Parsed_state_predicate_term parsed_state_predicate_term -> check_parsed_state_predicate_term useful_parsing_model_information parsed_state_predicate_term


	
let check_property useful_parsing_model_information parsed_property : bool =
	match parsed_property with
	(* Reachability *)
	| EF parsed_state_predicate
	(* Unavoidability *)
	| AF parsed_state_predicate	
	(* Liveness *)
	| AG parsed_state_predicate
		-> check_parsed_state_predicate useful_parsing_model_information parsed_state_predicate



(** Check and convert the parsing structure into an abstract property *)
let abstract_property_of_parsed_property (options : Options.imitator_options) (useful_parsing_model_information : useful_parsing_model_information) (parsed_property : ParsingStructure.parsed_property) : ImitatorUtilities.synthesis_algorithm =
	(* First check *)
	if not (check_property useful_parsing_model_information parsed_property) then(
		raise InvalidProperty
	);
	
	(* Now convert *)
	convert_property useful_parsing_model_information parsed_property*)

(*
(************************************************************)
(************************************************************)
(** Pi0 conversion *)
(************************************************************)
(************************************************************)


(*------------------------------------------------------------*)
(* Main function for pi0 parsing and converting *)
(*------------------------------------------------------------*)
let check_and_make_pi0 parsed_pi0 =
  (* Print some information *)
  print_message Verbose_total ("*** Building reference valuation…");

  (*** HACK (big big hack): retrieve the structures saved at the end of the model parsing, and kept for efficiency ***)
  let nb_parameters, parameters, parameters_names, variables =
    match !saved_nb_parameters, !saved_parameters, !saved_parameters_names, !saved_variables with
    | Some nb_parameters, Some parameters, Some parameters_names, Some variables -> nb_parameters, parameters, parameters_names, variables
    | _ -> raise (InternalError("Saved data structures (nb_parameters, parameters_names, variables) not available when parsing reference valuation"))
  in

  (* Verification of the pi0 *)
  if not (check_pi0 parsed_pi0 parameters_names) then raise InvalidPi0;

  (* Construction of the pi0 *)
  let pi0 = make_pi0 parsed_pi0 variables nb_parameters in

  (* Print some information *)
  print_message Verbose_medium ("\n*** Reference valuation pi0:");
  List.iter (fun parameter ->
      print_message Verbose_medium (
        variables.(parameter) ^ " : " ^ (NumConst.string_of_numconst (pi0#get_value parameter))
      )
    ) parameters;

  (* Return the valuation *)
  pi0



(************************************************************)
(************************************************************)
(** V0 conversion *)
(************************************************************)
(************************************************************)

(*------------------------------------------------------------*)
(* Check the V0 w.r.t. the model parameters *)
(*------------------------------------------------------------*)
let check_v0 parsed_v0 parameters_names =
  (* Compute the list of variable names *)
  let list_of_variables = List.map (fun (v, _, _) -> v) parsed_v0 in

  (* Compute the multiply defined variables *)
  let multiply_defined_variables = elements_existing_several_times list_of_variables in
  (* Print an error for each of them *)
  List.iter (fun variable_name -> print_error ("The parameter '" ^ variable_name ^ "' was assigned several times a valuation in v0.")) multiply_defined_variables;

  (*** TODO: only warns if it is always defined to the same value ***)

  (* Check if the variables are all defined *)
  let all_defined = List.fold_left
      (fun all_defined variable_name ->
         if List.mem variable_name list_of_variables then all_defined
         else (
           print_error ("The parameter '" ^ variable_name ^ "' was not assigned a valuation in v0.");
           false
         )
      )
      true
      parameters_names
  in

  (* Check that the intervals are not null *)
  let all_intervals_ok = List.fold_left
      (fun all_intervals_ok (variable_name, a, b) ->
         if NumConst.le a b then all_intervals_ok
         else (
           print_error ("The interval [" ^ (NumConst.string_of_numconst a) ^ ", " ^ (NumConst.string_of_numconst b) ^ "] is null for parameter '" ^ variable_name ^ "' in v0.");
           false
         )
      )
      true
      parsed_v0
  in

  (* Check if some defined variables are not parameters (and warn) *)
  List.iter
    (fun variable_name ->
       if not (List.mem variable_name parameters_names) then (
         print_warning ("'" ^ variable_name ^ "', which is assigned a valuation in v0, is not a valid parameter name.")
       )
    )
    list_of_variables
  ;

  (* If something went wrong: launch an error *)
  multiply_defined_variables = [] && all_defined && all_intervals_ok


(*------------------------------------------------------------*)
(* Convert the parsed v0 into a valid v0 *)
(*------------------------------------------------------------*)
let make_v0 parsed_v0 index_of_variables =
  let v0 = new HyperRectangle.hyper_rectangle in
  List.iter (fun (variable_name, a, b) ->
      try
        (* Get the variable index *)
        let variable_index = Hashtbl.find index_of_variables variable_name in
        (* Update the variable value *)
        v0#set_min variable_index a;
        v0#set_max variable_index b;
      with Not_found ->
        (* No problem: this must be an invalid parameter name (which is ignored) *)
        ()
        (* 			raise (InternalError ("The variable name '" ^ variable_name ^ "' was not found in the list of variables although checks should have been performed before.")) *)
    ) parsed_v0;
  v0


(*------------------------------------------------------------*)
(* Main function for v0 parsing and converting *)
(*------------------------------------------------------------*)
let check_and_make_v0 parsed_v0 =
  (* Print some information *)
  print_message Verbose_total ("*** Building reference hyper-rectangle…");

  (*** HACK (big big hack): retrieve the structures saved at the end of the model parsing, and kept for efficiency ***)
  let index_of_variables, nb_parameters, parameters_names, variables =
    match !saved_index_of_variables, !saved_nb_parameters, !saved_parameters_names, !saved_variables with
    | Some index_of_variables, Some nb_parameters, Some parameters_names, Some variables -> index_of_variables, nb_parameters, parameters_names, variables
    | _ -> raise (InternalError("Saved data structures (index_of_variables, nb_parameters, parameters_names, variables) not available when parsing hyper-rectangle"))
  in

  (* Verification *)
  if not (check_v0 parsed_v0 parameters_names) then raise InvalidV0;

  (* Construction *)
  let v0 = make_v0 parsed_v0 index_of_variables in

  (* Print some information *)
  print_message Verbose_medium ("\n*** Reference rectangle V0:");
  for parameter_index = 0 to nb_parameters - 1 do
    let min = v0#get_min parameter_index in
    let max = v0#get_max parameter_index in
    print_message Verbose_medium (
      variables.(parameter_index) ^ " : [" ^ (NumConst.string_of_numconst min) ^ ", " ^ (NumConst.string_of_numconst max) ^ "]"
    );
  done;

  (* Return the hyper-rectangle *)
  v0
*)
