(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: see ExporterRegistry.mli
 *
 * File contributors : Jaime Arias
 * Created           : 2026/06/15
 *
 ************************************************************)

open AbstractAlgorithm

exception No_textual_exporter of translation

(* The single source of truth associating a target with its exporter.
   Adding a format = adding one line here. *)
let exporter_of_target (target : translation) : (module ModelExporter.MODEL_EXPORTER) =
	match target with
	| DOT      -> (module Exporters.Dot)
	| HyTech   -> (module Exporters.HyTech)
	| IMI      -> (module Exporters.Imitator)
	| JaniSpec -> (module Exporters.JaniSpec)
	| TikZ     -> (module Exporters.TikZ)
	| Uppaal   -> (module Exporters.Uppaal)

	(* Targets handled by dedicated, non-textual pipelines: *)
	| ImiProp        (* property regeneration *)
	| JPG | PDF | PNG (* graphics rasterised via Graphics.dot *)
		-> raise (No_textual_exporter target)

let is_textual_export (target : translation) : bool =
	match exporter_of_target target with
	| _ -> true
	| exception No_textual_exporter _ -> false
