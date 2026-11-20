open Lib

val convert : Options.imitator_options -> ParsingStructure.parsed_model -> ParsingStructure.parsed_property option -> AbstractModel.abstract_model * AbstractProperty.abstract_property option

val run : Options.imitator_options -> ParsingStructure.parsed_model -> ParsingStructure.parsed_property option -> Result.imitator_result * AbstractModel.abstract_model
