(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Orchestrates a model translation: select the right
 *                     exporter (via {!ExporterRegistry}), render the
 *                     model and write the result to the conventional output
 *                     file.
 *
 * File contributors : Jaime Arias
 * Created           : 2026/06/15
 *
 ************************************************************)

(** Orchestrates a model translation: select the right exporter (via {!ExporterRegistry}), render the model and write the result to the conventional output file. *)


(** [string_of_model options model target] renders [model] to the textual
    format designated by [target].
    @raise ExporterRegistry.No_textual_exporter for non-textual targets. *)
val string_of_model :
	Options.imitator_options -> AbstractModel.abstract_model ->
	AbstractAlgorithm.translation -> string

(** [export_and_write options model target] renders [model] for [target]
    and writes it to "<files_prefix><suffix>", returning the path of the
    created file. Verbose logging is performed along the way.
    @raise ExporterRegistry.No_textual_exporter for non-textual targets. *)
val export_and_write :
	Options.imitator_options -> AbstractModel.abstract_model ->
	AbstractAlgorithm.translation -> string
