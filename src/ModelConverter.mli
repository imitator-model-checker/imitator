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
 * Last modified     : 2019/10/17
 *
 ************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)


(****************************************************************)
(** Exceptions *)
(****************************************************************)

(* When checking pi0 *)
exception InvalidPi0
(* When checking v0 *)
exception InvalidV0

exception InvalidModel

exception InvalidProperty


(****************************************************************)
(** Types *)
(****************************************************************)

(* This structure is returned by the model converter, as it can be possibly use when analyzing the property *)
type useful_parsing_model_information = {
	constants         : (Automaton.variable_name, NumConst.t) Hashtbl;
	index_of_automata : (Automaton.automaton_name, Automaton.automaton_index) Hashtbl;
	index_of_variables: (Automaton.variable_name, Automaton.variable_index) Hashtbl;
}

(****************************************************************)
(** Conversion functions *)
(****************************************************************)
(** Check and convert the parsing structure into an abstract model *)
val abstract_model_of_parsing_structure : Options.imitator_options -> bool -> ParsingStructure.parsing_structure -> AbstractModel.abstract_model * useful_parsing_model_information

(** Check and convert the parsing structure into an abstract property *)
val abstract_model_of_parsed_property : Options.imitator_options -> AbstractModel.abstract_model * useful_parsing_model_information -> ParsingStructure.parsed_property -> ImitatorUtilities.synthesis_algorithm

(*(** Check and convert the parsed reference parameter valuation into an abstract representation *)
val check_and_make_pi0 : ParsingStructure.pi0 -> (*Options.imitator_options ->*) PVal.pval

(** Check and convert the parsed hyper-rectangle into an abstract representation *)
val check_and_make_v0 : ParsingStructure.v0 -> (*Options.imitator_options ->*) HyperRectangle.hyper_rectangle*)

(** Get clocks index used on the updates *)
val get_clocks_in_updates : AbstractModel.updates -> Automaton.clock_index list
