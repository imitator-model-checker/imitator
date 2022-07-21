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
 *
 ************************************************************)

 
(************************************************************)
(************************************************************)
(* IMITATOR NAME AND VERSION *)
(************************************************************)
(************************************************************)

let program_name = "IMITATOR"

let version_string = "3.3"

let version_name = "Cheese Caramel au beurre salé"

(* Path ending with "/" *)
let path_to_program =
	(* Try to find the last occurrence of '/' in the string *)
	try(
		let full_path_to_binary = Sys.executable_name in
		let last_position = String.rindex full_path_to_binary '/' in
		(* Remove last element (program name) *)
		(String.sub full_path_to_binary 0 last_position)
		(* Add final "/" *)
		^ "/"
	) with Not_found ->
	(* If unexpected form, assume path is root *)
		"/"


(************************************************************)
(************************************************************)
(* External binaries *)
(************************************************************)
(************************************************************)

let dot_binary_name = "dot"


(************************************************************)
(************************************************************)
(* PARSING / MODEL SYNTAX *)
(************************************************************)
(************************************************************)

(* Name for the global time clock in the input model *)
let global_time_clock_name		= "global_time"


(************************************************************)
(************************************************************)
(* FILE EXTENSIONS *)
(************************************************************)
(************************************************************)


(** Extension for input model files *)
let model_extension				= ".imi"

(** Extension for property files *)
let property_extension			= ".prop"

(** Extension for files output *)
let result_file_extension		= ".res"

let state_space_image_format	= "pdf"
let pta_default_image_format	= "jpg"
let dot_file_extension			= "dot"
let default_dot_image_extension	= "dot"
let states_file_extension		= "states"

let cartography_extension		= "png"
let cartography_size			= "1024x1024"

let signals_image_extension		= "ps"


(************************************************************)
(************************************************************)
(* File suffixes *)
(************************************************************)
(************************************************************)

let cart_file_suffix			= "_cart"


(************************************************************)
(************************************************************)
(* Hashtable initial size (just a guess) *)
(************************************************************)
(************************************************************)
let guessed_nb_states_for_hashtable = 100


(************************************************************)
(************************************************************)
(* Internal cuisine *)
(************************************************************)
(************************************************************)

(* Name of the observer automaton internal action *)
let observer_nosync_name		= "nosync_obs"

let observer_automaton_name		= "automatically_generated_observer"
let observer_clock_name			= "automatically_generated_x_obs"


(* Name of the special clock always reset (used for NZ model checking, and not to be printed in normal operations) *)
let special_reset_clock_name	= "special_0_clock"


(************************************************************)
(************************************************************)
(* Algorithms *)
(************************************************************)
(************************************************************)

(* Default step for the cartography algorithms *)
let default_cartography_step	= NumConst.one

(************************************************************)
(************************************************************)
(* Strings constants *)
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
    | Binary_word_representation_standard
    | Binary_word_representation_int

(** Data structure allowing for customizing string conversions of symbols *)
type customized_string = {
    arithmetic_string : customized_arithmetic_string;
    boolean_string : customized_boolean_string;
    array_string : customized_array_string;
    binary_word_representation : customized_binary_word_representation;
}

(** Default string values of bool symbols *)
let default_string = {
	true_string   = "True";
	false_string  = "False";
	and_operator  = "\n& ";
	or_operator   = " or ";
	l_operator    = " < ";
	le_operator   = " <= ";
	eq_operator   = " = ";
	neq_operator  = " <> ";
	ge_operator   = " >= ";
	g_operator    = " > ";
	not_operator  = " not ";
	in_operator   = " in ";
}

(** Default string values of bool symbols without white spaces around operators *)
let default_string_without_whitespace = {
	true_string   = "True";
	false_string  = "False";
	and_operator  = "\n& ";
	or_operator   = " or ";
	l_operator    = "<";
	le_operator   = "<=";
	eq_operator   = "=";
	neq_operator  = "<>";
	ge_operator   = ">=";
	g_operator    = ">";
	not_operator  = " not ";
	in_operator   = " in ";
}

(** Default string values of arithmetic symbols *)
let default_arithmetic_string = {
    plus_string = " + ";
    minus_string = " - ";
    mul_string = " * ";
    div_string = " / ";
    unary_min_string = "-";
}

(** Default string values of arithmetic symbols without white spaces around operators *)
let default_arithmetic_string_without_whitespace = {
    plus_string = "+";
    minus_string = "-";
    mul_string = "*";
    div_string = "/";
    unary_min_string = "-";
}

(** Default string values of array delimiters symbols without whitespaces around operators *)
let default_array_string = {
    array_literal_delimiter = "[", "]";
    array_access_delimiter = "[", "]";
}

(** Default string values of parenthesis delimiters symbols without whitespaces *)
let default_paren_delimiter = "(", ")"

(** Default string values of symbols *)
let global_default_string = {
    arithmetic_string = default_arithmetic_string;
    boolean_string = default_string;
    array_string = default_array_string;
    binary_word_representation = Binary_word_representation_standard;
}
