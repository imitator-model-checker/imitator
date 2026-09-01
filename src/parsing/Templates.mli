
(** Expansion of parameterised PTA templates into a plain parsed model and property. *)

open ParsingStructure

val expand_model : unexpanded_parsed_model -> parsed_model
val expand_property : variable_declarations -> unexpanded_parsed_property -> parsed_property

type synt_vars_data

type useful_parsing_context = {
  variable_declarations : variable_declarations;
  synt_vars : synt_vars_data;
}

val make_expansion_context :
  unexpanded_parsed_model -> useful_parsing_context

val expand_model_with_context_internal :
  unexpanded_parsed_model ->
  useful_parsing_context ->
  parsed_model

val expand_loc :
  variable_declarations ->
  synt_vars_data ->
  unexpanded_parsed_location ->
  parsed_location