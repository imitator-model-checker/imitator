(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Maps a textual {!AbstractAlgorithm.translation} target
 *                     to the exporter module that implements it.
 *
 * File contributors : Jaime Arias
 * Created           : 2026/06/15
 *
 ************************************************************)

(** Raised when a translation target has no associated textual exporter
    (e.g. the graphics targets JPG/PDF/PNG, or the property target ImiProp,
    which are handled by dedicated pipelines). *)
exception No_textual_exporter of AbstractAlgorithm.translation

(** Look up the exporter implementing a given target.
    @raise No_textual_exporter if the target is not a textual translation. *)
val exporter_of_target : AbstractAlgorithm.translation -> (module ModelExporter.MODEL_EXPORTER)

(** [is_textual_export target] is [true] iff [target] is handled by a
    registered textual exporter (i.e. {!exporter_of_target} succeeds). *)
val is_textual_export : AbstractAlgorithm.translation -> bool
