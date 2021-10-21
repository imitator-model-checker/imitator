(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * All (?) constants of IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2014/10/24
 * Last modified     : 2021/04/21
 *
 ************************************************************)

 
(************************************************************)
(************************************************************)
(* IMITATOR NAME AND VERSION *)
(************************************************************)
(************************************************************)

val program_name : string

val version_string : string

val version_name : string

(* Path ending with "/" *)
val path_to_program : string


(************************************************************)
(************************************************************)
(* External binaries *)
(************************************************************)
(************************************************************)

val dot_binary_name : string

(************************************************************)
(************************************************************)
(* PARSING / MODEL SYNTAX *)
(************************************************************)
(************************************************************)

(* Name for the global time clock in the input model *)
val global_time_clock_name : string


(************************************************************)
(************************************************************)
(* FILE EXTENSIONS *)
(************************************************************)
(************************************************************)


(** Extension for input model files *)
val model_extension          : string

(** Extension for property files *)
val property_extension       : string

(** Extension for files output *)
val result_file_extension    : string

val state_space_image_format : string
val pta_default_image_format : string
val dot_file_extension       : string
val default_dot_image_extension  : string
val states_file_extension    : string

val cartography_extension    : string
val cartography_size         : string

val signals_image_extension  : string


(************************************************************)
(************************************************************)
(* File suffixes *)
(************************************************************)
(************************************************************)

val cart_file_suffix			: string


(************************************************************)
(************************************************************)
(* Hashtable initial size (just a guess) *)
(************************************************************)
(************************************************************)
val guessed_nb_states_for_hashtable : int


(************************************************************)
(************************************************************)
(* Internal cuisine *)
(************************************************************)
(************************************************************)

(* Name of the observer automaton internal action *)
val observer_nosync_name		: string

val observer_automaton_name		: string
val observer_clock_name			: string



(* Name of the special clock always reset (used for NZ model checking, and not to be printed in normal operations) *)
val special_reset_clock_name	: string


(************************************************************)
(************************************************************)
(* Algorithms *)
(************************************************************)
(************************************************************)

(* Default step for the cartography algorithms *)
val default_cartography_step	: NumConst.t

(************************************************************)
(************************************************************)
(* Strings *)
(************************************************************)
(************************************************************)

(** Data structure allowing for customizing string conversions of bool symbols *)
type customized_boolean_string = {
	true_string  : string;
	false_string : string;
	and_operator : string;
	or_operator  : string;
	l_operator   : string;
	le_operator  : string;
	eq_operator  : string;
	neq_operator : string;
	ge_operator  : string;
	g_operator   : string;
	not_operator : string;
	in_operator  : string;
}

(** Data structure allowing for customizing string conversions of arithmetic symbols *)
type customized_arithmetic_string = {
    plus_string : string;
    minus_string : string;
    mul_string : string;
    div_string : string;
    unary_min_string : string;
}

(** Data structure allowing for customizing string conversions of array delimiters symbols *)
type customized_array_string = {
    array_literal_delimiter : string * string;
    array_access_delimiter : string * string;
}

type customized_binary_word_representation =
    | Standard
    | Int

(** Data structure allowing for customizing string conversions of symbols *)
type customized_string = {
    arithmetic_string : customized_arithmetic_string;
    boolean_string : customized_boolean_string;
    array_string : customized_array_string;
    binary_word_representation : customized_binary_word_representation;
}

val global_default_string : customized_string
(** Default string values of bool symbols *)
val default_string : customized_boolean_string
(** Default string values of bool symbols without white spaces around operators *)
val default_string_without_whitespace : customized_boolean_string
(** Default string values of arithmetic symbols *)
val default_arithmetic_string : customized_arithmetic_string
(** Default string values of arithmetic symbols without white spaces around operators *)
val default_arithmetic_string_without_whitespace : customized_arithmetic_string
(** Default string values of array delimiters symbols without white spaces around operators *)
val default_array_string : customized_array_string

(** Default string of symbols *)
val global_default_string : customized_string