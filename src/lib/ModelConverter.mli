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
(** Convert the parsed model and the parsed property into an abstract model and an abstract property *)
val abstract_structures_of_parsing_structures : Options.imitator_options -> ParsingStructure.unexpanded_parsed_model -> (ParsingStructure.parsed_property option) -> AbstractModel.abstract_model * (AbstractProperty.abstract_property option)
