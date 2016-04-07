(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: process the result of IMITATOR: print results, creates files, generates graphics, etc.
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/03
 * Last modified     : 2016/03/03
 *
 ************************************************************)

(** Process the result of IMITATOR. The 3rd optional argument is the file name prefix (otherwise options#files_prefix is used). *)
val process_result : Result.imitator_result -> string -> string option -> unit
