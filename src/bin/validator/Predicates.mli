open Lib

val negate : CounterExampleFinder.predicate_result -> CounterExampleFinder.predicate_result

val result_is_empty : Result.imitator_result -> CounterExampleFinder.predicate_result

val results_intersect : (Result.imitator_result * bool) list -> CounterExampleFinder.predicate_result