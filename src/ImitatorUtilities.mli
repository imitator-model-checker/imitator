(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Useful and general functions for IMITATOR
 * 
 * File contributors : Étienne André, Laure Petrucci
 * Created           : 2014/10/24
 * Last modified     : 2019/12/18
 *
 ************************************************************)



(************************************************************)
(** Versioning *)
(************************************************************)

(* Name + version *)
val program_name_and_version : string

(* Name + version + nickname *)
val program_name_and_version_and_nickname : string

(* Name + version + build *)
val program_name_and_version_and_build : string

(* Name + version + nickname + build number *)
val program_name_and_version_and_nickname_and_build : string

(* Name + version + nickname + build number + build time *)
val program_name_and_version_and_nickname_and_build_time : unit -> string

(** GitHub branch and first 7 characters of git hash (if applicable) *)
val git_branch_and_hash : string

(** GitHub branch and full git hash (if applicable) *)
val git_branch_and_full_hash : string

(* URL of IMITATOR without http:// *)
val imitator_url : string


(************************************************************)
(** Verbosity modes *)
(************************************************************)

type verbose_mode =
	| Verbose_mute
	| Verbose_warnings
	| Verbose_standard
	| Verbose_experiments
	| Verbose_low
	| Verbose_medium
	| Verbose_high
	| Verbose_total


(* Return true if the global verbose mode is greater than 'verbose_mode', false otherwise *)
val verbose_mode_greater : verbose_mode -> bool

(* Convert a string into a verbose_mode; raise Not_found if not found *)
val verbose_mode_of_string : string -> verbose_mode

(* Set the verbose mode *)
val set_verbose_mode : verbose_mode -> unit

(* Get the verbose mode *)
val get_verbose_mode : unit -> verbose_mode







(************************************************************)
(** Global time counter *)
(************************************************************)

(* Compute the duration in ms between 2 times *)
(* val duration : float -> float -> float *)

(** Get the value of the counter *)
val get_time : unit -> float

(** Compute the duration since time t *)
val time_from : float -> float

(** Print a number of seconds *)
val string_of_seconds : float -> string

(** Convert a % to a nice string *)
val string_of_percent : float -> string

(** Create a string of the form 'after x seconds', where x is the time since the program started *)
val after_seconds : unit -> string

(** Set the timed mode *)
val set_timed_mode : unit -> unit





(**************************************************)
(** System functions *)
(**************************************************)

val delete_file : string -> unit

(** Obtain a string giving information on the memory used *)
val memory_used : unit -> string

(** Print info on the memory used *)
(* val print_memory_used : verbose_mode -> unit *)


(************************************************************)
(** Messages *)
(************************************************************)

type shell_highlighting_type =
	| Shell_bold
	| Shell_error
	| Shell_normal
	| Shell_result
	| Shell_soundness
	| Shell_warning

(* Print a message if global_verbose_mode >= message_verbose_mode *)
val print_message : verbose_mode -> string -> unit

(* Print a message with some highlighting *)
val print_highlighted_message : shell_highlighting_type -> verbose_mode -> string -> unit

(* Print a warning *)
val print_warning : string -> unit

(* Print an error *)
val print_error : string -> unit

(* Print the name of the contributors *)
val print_contributors : unit -> unit

val print_header_string : unit -> unit



(************************************************************)
(** Terminating functions *)
(************************************************************)
(* Abort program *)
val abort_program : unit -> unit

(* Terminate program *)
val terminate_program : unit -> unit
