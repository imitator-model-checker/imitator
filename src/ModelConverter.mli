(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Convert a parsing structure into an abstract model
 *
 * File contributors : Étienne André, Jaime Arias
 * Created           : 2009/09/09
 * Last modified     : 2020/01/17
 *
 ************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)


(****************************************************************)
(** Exceptions *)
(****************************************************************)

exception InvalidProperty

(****************************************************************)
(** Types *)
(****************************************************************)


(****************************************************************)
(** Conversion functions *)
(****************************************************************)
(** Convert the parsed model and the parsed property into an abstract model and an abstract property *)
val abstract_structures_of_parsing_structures : Options.imitator_options -> ParsingStructure.parsed_model -> (ParsingStructure.parsed_property option) -> AbstractModel.abstract_model * (AbstractProperty.abstract_property option)

(** Check and convert the parsing structure into an abstract property *)
(* val abstract_model_of_parsed_property : Options.imitator_options -> AbstractModel.abstract_model * useful_parsing_model_information -> ParsingStructure.parsed_property -> ImitatorUtilities.synthesis_algorithm *)

(*(** Check and convert the parsed reference parameter valuation into an abstract representation *)
val check_and_make_pi0 : ParsingStructure.pi0 -> (*Options.imitator_options ->*) PVal.pval

(** Check and convert the parsed hyper-rectangle into an abstract representation *)
val check_and_make_v0 : ParsingStructure.v0 -> (*Options.imitator_options ->*) HyperRectangle.hyper_rectangle*)

(** Get clocks index used on the updates *)
val get_clocks_in_updates : AbstractModel.updates -> Automaton.clock_index list
