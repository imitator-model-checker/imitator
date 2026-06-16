(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Common interface implemented by every model
 *                     exporter (HyTech, JaniSpec, TikZ, dot, Uppaal,
 *                     IMITATOR…). Centralising the contract here is what
 *                     lets the rest of the code depend on the *abstraction*
 *                     `MODEL_EXPORTER` rather than on any concrete target.
 *
 * File contributors : Jaime Arias
 * Created           : 2026/06/15
 *
 ************************************************************)

(** Common interface implemented by every model exporter (HyTech, JaniSpec, TikZ, dot, Uppaal, IMITATOR…).

    Centralising the contract here is what lets the rest of the code depend
    on the *abstraction* `MODEL_EXPORTER` rather than on any concrete
    target. *)


module type MODEL_EXPORTER = sig
	(** File extension (including the leading dot, e.g. ".hy") associated with
	    the produced representation. *)
	val file_suffix : string

	(** Render a model to a string in the target format. *)
	val string_of_model : Options.imitator_options -> AbstractModel.abstract_model -> string
end
