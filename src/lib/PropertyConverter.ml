(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module aims to convert a property expression to a abstract property expression
 *
 * File contributors : Benjamin L.
 * Created           : 2021/12/07
 *
 ************************************************************)

open ParsingStructure
open AbstractProperty
open TypedStructure


(** Convert a ParsingStructure.parsed_interval into a AbstractProperty.timed_interval *)
let timed_interval_of_parsed_interval (useful_parsing_model_information : ParsingStructure.useful_parsing_model_information) (parsed_interval : ParsingStructure.parsed_interval) : AbstractProperty.timed_interval =
	(* Retrieve variable information *)
    let variable_infos = useful_parsing_model_information.variable_infos in

    (* Create the function converting a ParsingStructure.parsed_duration into a LinearConstraint.p_linear_term *)
	let p_linear_term_of_parsed_duration (parsed_duration : ParsingStructure.parsed_duration) : LinearConstraint.p_linear_term = LinearConstraint.cast_p_of_pxd_linear_term (ExpressionConverter.Convert.linear_term_of_linear_expression variable_infos parsed_duration) true in

	match parsed_interval with
	| Parsed_zero_closed_interval parsed_duration ->
		Zero_closed_interval (p_linear_term_of_parsed_duration parsed_duration)
	| Parsed_zero_open_interval parsed_duration ->
		Zero_open_interval (p_linear_term_of_parsed_duration parsed_duration)
	| Parsed_closed_closed_interval (parsed_duration_1, parsed_duration_2) ->
		Closed_closed_interval (p_linear_term_of_parsed_duration parsed_duration_1, p_linear_term_of_parsed_duration parsed_duration_2)
	| Parsed_closed_open_interval (parsed_duration_1, parsed_duration_2) ->
		Closed_open_interval (p_linear_term_of_parsed_duration parsed_duration_1, p_linear_term_of_parsed_duration parsed_duration_2)
	| Parsed_open_closed_interval (parsed_duration_1, parsed_duration_2) ->
		Open_closed_interval (p_linear_term_of_parsed_duration parsed_duration_1, p_linear_term_of_parsed_duration parsed_duration_2)
	| Parsed_open_open_interval (parsed_duration_1, parsed_duration_2) ->
		Open_open_interval (p_linear_term_of_parsed_duration parsed_duration_1, p_linear_term_of_parsed_duration parsed_duration_2)
	| Parsed_closed_infinity_interval parsed_duration ->
		Closed_infinity_interval (p_linear_term_of_parsed_duration parsed_duration)
	| Parsed_open_infinity_interval parsed_duration ->
		Open_infinity_interval (p_linear_term_of_parsed_duration parsed_duration)



(* Convert parsed_loc_predicate *)
let loc_predicate_of_typed_loc_predicate model_info = function
	| Typed_loc_predicate_EQ (automaton_name, location_name) ->
		let automaton_index = Hashtbl.find model_info.index_of_automata automaton_name in
		Loc_predicate_EQ ( automaton_index , (Hashtbl.find model_info.index_of_locations.(automaton_index) location_name))

	| Typed_loc_predicate_NEQ (automaton_name, location_name) ->
		let automaton_index = Hashtbl.find model_info.index_of_automata automaton_name in
		Loc_predicate_NEQ (automaton_index , (Hashtbl.find model_info.index_of_locations.(automaton_index) location_name))

(* Convert parsed_simple_predicate *)
let simple_predicate_of_typed_simple_predicate model_info = function
	| Typed_discrete_boolean_expression (expr, _) ->

	    State_predicate_discrete_boolean_expression (
            ExpressionConverter.Convert.bool_expression_of_typed_discrete_boolean_expression model_info.variable_infos expr
        )

	| Typed_loc_predicate predicate ->
	    Loc_predicate (
	        loc_predicate_of_typed_loc_predicate model_info predicate
	    )

	| Typed_state_predicate_true -> State_predicate_true
	| Typed_state_predicate_false -> State_predicate_false
	| Typed_state_predicate_accepting -> State_predicate_accepting

(* Convert parsed_state_predicate *)
let rec state_predicate_of_typed_state_predicate_factor model_info = function
	| Typed_state_predicate_factor_NOT predicate ->
	    State_predicate_factor_NOT (
	        state_predicate_of_typed_state_predicate_factor model_info predicate
        )

	| Typed_simple_predicate (predicate, _) ->
	    Simple_predicate (
	        simple_predicate_of_typed_simple_predicate model_info predicate
        )

	| Typed_state_predicate (predicate, _) ->
	    State_predicate (
	        state_predicate_of_typed_state_predicate model_info predicate
        )

and state_predicate_term_of_typed_state_predicate_term model_info = function
	| Typed_state_predicate_term_AND (l_predicate_term, r_predicate_term) ->
		State_predicate_term_AND (
			state_predicate_term_of_typed_state_predicate_term model_info l_predicate_term,
			state_predicate_term_of_typed_state_predicate_term model_info r_predicate_term
		)

	| Typed_state_predicate_factor (predicate_factor, _) ->
	    State_predicate_factor (
	        state_predicate_of_typed_state_predicate_factor model_info predicate_factor
        )

and state_predicate_of_typed_state_predicate model_info = function
	| Typed_state_predicate_OR (l_predicate, r_predicate) ->
		State_predicate_OR (
			state_predicate_of_typed_state_predicate model_info l_predicate,
			state_predicate_of_typed_state_predicate model_info r_predicate
		)

	| Typed_state_predicate_term (predicate_term, _) ->
	    State_predicate_term (
	        state_predicate_term_of_typed_state_predicate_term model_info predicate_term
        )


let convert_state_predicate model_info predicate =
    (* Type check *)
    let typed_predicate = ExpressionConverter.TypeChecker.check_state_predicate model_info.variable_infos predicate in
    (* Convert *)
    state_predicate_of_typed_state_predicate model_info typed_predicate