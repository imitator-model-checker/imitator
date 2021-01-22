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
 * Last modified     : 2021/01/19
 *
 ************************************************************)

 
(************************************************************)
(************************************************************)
(* IMITATOR NAME AND VERSION *)
(************************************************************)
(************************************************************)

let program_name = "IMITATOR"

let version_string = "3.0"

let version_name = "Cheese"

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

let dot_command = "dot"


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
