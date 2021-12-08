(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module aims to convert a property expression to a abstract property expression
 *
 * File contributors : Benjamin L.
 * Created           : 2021/12/07
 * Last modified     : 2021/12/07
 *
 ************************************************************)

open Constants
open Exceptions
open ParsingStructure
open AbstractModel
open AbstractProperty
open DiscreteExpressions
open DiscreteType
open TypeChecker2

val convert_state_predicate : useful_parsing_model_information -> parsed_state_predicate -> state_predicate