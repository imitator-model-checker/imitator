(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Parsing functions for input elements
 *
 * File contributors : Ulrich Kühne, Étienne André
 * Created           : 2014/03/15
 *
 ************************************************************)

(*(** Compile the concrete model and convert it into an abstract model *)
val compile_model: Options.imitator_options -> bool -> AbstractModel.abstract_model * AbstractModel.useful_parsing_model_information

(** Parse the property file and convert it into an abstract representation *)
val compile_property: Options.imitator_options -> AbstractModel.useful_parsing_model_information -> AbstractProperty.property*)

(** Parse the model, and property file if needed, and convert them into an abstract representation *)
val compile_model_and_property: Options.imitator_options -> AbstractModel.abstract_model * (AbstractProperty.abstract_property option)


(*
(** Parse the pi0 file and convert it into an abstract representation *)
val compile_pi0: Options.imitator_options -> PVal.pval

(** Parse the v0 file and convert it into an abstract representation *)
val compile_v0: Options.imitator_options -> HyperRectangle.hyper_rectangle*)
