(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2014/10/24
 * Last modified: 2015/04/01
 *
 ****************************************************************)



(****************************************************************)
(** Debug modes *)
(****************************************************************)

type debug_mode =
	| Verbose_mute
	| Verbose_warnings
	| Verbose_standard
	| Verbose_low
	| Verbose_medium
	| Verbose_high
	| Verbose_total


(* Return true if the global debug mode is greater than 'debug_mode', false otherwise *)
val debug_mode_greater : debug_mode -> bool

(* Convert a string into a debug_mode; raise Not_found if not found *)
val debug_mode_of_string : string -> debug_mode

(* Set the debug mode *)
val set_debug_mode : debug_mode -> unit

(* Get the debug mode *)
val get_debug_mode : unit -> debug_mode


(****************************************************************)
(** Global types *)
(****************************************************************)

(** Mode for IMITATOR *)
type imitator_mode =
	(** Translation to another language: no analysis *)
	| Translation
	(** Classical state space exploration *)
	| State_space_exploration
	(** EF-synthesis *)
	| EF_synthesis
	(** Classical inverse method *)
	| Inverse_method
	(** Cover the whole cartography *)
	| Cover_cartography
	(** Look for the border using the cartography*)
	| Border_cartography
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography of int


(****************************************************************)
(** Global time counter *)
(****************************************************************)

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

(** Print info on the memory used *)
val print_memory_used : debug_mode -> unit


(****************************************************************)
(** Messages *)
(****************************************************************)
(*(* Print a message if global_debug_mode >= message_debug_mode *)
val print_debug_message : debug_mode -> debug_mode -> string -> unit*)

(* Print a message if global_debug_mode >= message_debug_mode *)
val print_message : debug_mode -> string -> unit

(* Print a warning *)
val print_warning : string -> unit

(* Print an error *)
val print_error : string -> unit

(* Print the name of the contributors *)
val print_contributors : unit -> unit

val print_header_string : unit -> unit



(****************************************************************)
(** Terminating functions *)
(****************************************************************)
(* Abort program *)
val abort_program : unit -> unit

(* Terminate program *)
val terminate_program : unit -> unit
