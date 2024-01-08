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

val convert_state_predicate : useful_parsing_model_information -> parsed_state_predicate -> state_predicate