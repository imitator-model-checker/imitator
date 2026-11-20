(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Convert a parsing structure into an abstract model
 *
 * File contributors : Étienne André, Jaime Arias
 * Created           : 2009/09/09
 *
 ************************************************************)

(** Convert a parsing structure into an abstract model. *)



(****************************************************************)
(* Modules *)
(****************************************************************)


(****************************************************************)
(* Exceptions *)
(****************************************************************)

exception InvalidProperty

(****************************************************************)
(* Types *)
(****************************************************************)


(****************************************************************)
(* Conversion functions *)
(****************************************************************)
(** Convert the (already expanded) parsed model and parsed property into an abstract model and an abstract property *)
val abstract_structures_of_parsing_structures : Options.imitator_options -> ParsingStructure.parsed_model -> (ParsingStructure.parsed_property option) -> AbstractModel.abstract_model * (AbstractProperty.abstract_property option)

(** Expand templates and syntactic variables in the unexpanded parsed model and parsed property, then convert them into an abstract model and an abstract property *)
val abstract_structures_of_unexpanded_parsing_structures : Options.imitator_options -> ParsingStructure.unexpanded_parsed_model -> (ParsingStructure.unexpanded_parsed_property option) -> AbstractModel.abstract_model * (AbstractProperty.abstract_property option)
