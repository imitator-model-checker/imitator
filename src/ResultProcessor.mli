(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: process the result of IMITATOR: print results, creates files, generates graphics, etc.
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/03
 *
 ************************************************************)

(** Convert a Result.good_or_bad_constraint into a string *)
val string_of_good_or_bad_constraint : (int -> string) -> Result.good_or_bad_constraint -> string


(** Process the result of IMITATOR. The 3rd optional argument is the file name prefix (otherwise options#files_prefix is used). *)
val process_result : Result.imitator_result -> string -> string option -> unit

(** Process the result of IMITATOR. The 3rd optional argument is the file name prefix (otherwise options#files_prefix is used). Then successfully terminate program. *)
val process_result_and_terminate : Result.imitator_result -> string -> string option -> Statistics.timeCounter -> unit

(** Process the result of IMITATOR. The 3rd optional argument is the file name prefix (otherwise options#files_prefix is used). Then terminate program with failure. *)
val process_result_and_abort : Result.error_type -> string -> string option -> Statistics.timeCounter -> unit

