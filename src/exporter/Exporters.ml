(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Adapters turning each concrete renderer (PTA2HyTech,
 *                     PTA2JaniSpec, PTA2TikZ, PTA2dot, PTA2Uppaal and the
 *                     IMITATOR re-printer) into a uniform
 *                     {!ModelExporter.MODEL_EXPORTER}.
 *
 * File contributors : Jaime Arias
 * Created           : 2026/06/15
 *
 ************************************************************)

module HyTech : ModelExporter.MODEL_EXPORTER = struct
	let file_suffix = ".hy"
	let string_of_model = PTA2HyTech.string_of_model
end

module JaniSpec : ModelExporter.MODEL_EXPORTER = struct
	let file_suffix = ".jani"
	let string_of_model = PTA2JaniSpec.string_of_model
end

module TikZ : ModelExporter.MODEL_EXPORTER = struct
	let file_suffix = ".tex"
	(* Adapts the historical name `tikz_string_of_model` to the common contract *)
	let string_of_model = PTA2TikZ.tikz_string_of_model
end

module Dot : ModelExporter.MODEL_EXPORTER = struct
	let file_suffix = ".dot"
	let string_of_model = PTA2dot.string_of_model
end

module Uppaal : ModelExporter.MODEL_EXPORTER = struct
	let file_suffix = "-uppaal.xml"
	let string_of_model = PTA2Uppaal.string_of_model
end

(* The IMITATOR re-printer lives in the `lib` layer (ModelPrinter) because it
   is also used by the analysis code. We adapt it here so that "regenerate an
   IMITATOR model" is just another translation target, without duplicating
   logic (Dependency Inversion: we depend on ModelPrinter's API, not the
   other way around). *)
module Imitator : ModelExporter.MODEL_EXPORTER = struct
	let file_suffix = "-regenerated" ^ Constants.model_extension
	(* ModelPrinter.string_of_model does not take the options argument *)
	let string_of_model _options model = ModelPrinter.string_of_model model
end
