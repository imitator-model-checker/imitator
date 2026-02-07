open Validator_config


val formula : nb_clocks:int -> nb_parameters:int -> opers:(Spec.constraint_type list) -> SimpleModel.formula QCheck2.Gen.t