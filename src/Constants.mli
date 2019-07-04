(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * All (?) constants of IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2014/10/24
 * Last modified     : 2019/07/05
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

val dot_command : string

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

(** Extension for files output *)
val result_file_extension    : string

val state_space_image_format : string
val pta_default_image_format : string
val dot_file_extension       : string
val default_dot_image_extension  : string
val states_file_extension    : string

val cartography_extension    : string
val cartography_size         : string


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
(* Name of the special clock always reset (used for NZ model checking, and not to be printed in normal operations) *)
val special_reset_clock_name : string
