(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module aims to convert a property expression to a abstract property expression
 *
 * File contributors : Benjamin L.
 * Created           : 2021/12/07
 *
 ************************************************************)

open ParsingStructure
open AbstractProperty

(** Convert a ParsingStructure.parsed_interval into a AbstractProperty.timed_interval *)
val timed_interval_of_parsed_interval : ParsingStructure.useful_parsing_model_information -> ParsingStructure.parsed_interval -> AbstractProperty.timed_interval

val convert_state_predicate : useful_parsing_model_information -> parsed_state_predicate -> state_predicate