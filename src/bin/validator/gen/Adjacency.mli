open Validator_spec
val generate : nb_locations:int -> spec:Spec.t -> int array array QCheck2.Gen.t