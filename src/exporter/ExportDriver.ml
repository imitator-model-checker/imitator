(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: see ExportDriver.mli
 *
 * File contributors : Jaime Arias
 * Created           : 2026/06/15
 *
 ************************************************************)

open OCamlUtilities
open ImitatorUtilities


let string_of_model (options : Options.imitator_options) (model : AbstractModel.abstract_model) (target : AbstractAlgorithm.translation) : string =
	let (module T : ModelExporter.MODEL_EXPORTER) = ExporterRegistry.exporter_of_target target in
	T.string_of_model options model


let export_and_write (options : Options.imitator_options) (model : AbstractModel.abstract_model) (target : AbstractAlgorithm.translation) : string =
	let (module T : ModelExporter.MODEL_EXPORTER) = ExporterRegistry.exporter_of_target target in

	print_message Verbose_standard ("Regenerating the input model to a new model.");

	let translated_model = T.string_of_model options model in
	let target_language_file = options#files_prefix ^ T.file_suffix in

	if verbose_mode_greater Verbose_total then
		print_message Verbose_total ("\n" ^ translated_model ^ "\n");

	(* Write *)
	write_to_file target_language_file translated_model;
	print_message Verbose_standard ("File '" ^ target_language_file ^ "' successfully created.");

	target_language_file
