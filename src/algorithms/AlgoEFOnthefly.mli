(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: EF reachability with on-the-fly model updates
 *
 ************************************************************)

class algoEFOnthefly :
  AbstractModel.abstract_model ->
  ModelProvider.model_provider ->
  AbstractProperty.abstract_property ->
  Options.imitator_options ->
  AbstractProperty.state_predicate ->
  object
    inherit AlgoGeneric.algoGeneric
    method algorithm_name : string
    method process_current_model : unit
    method compute_result : Result.imitator_result
    method run : Result.imitator_result
  end

class algoEFtimedonthefly :
  AbstractModel.abstract_model ->
  ModelProvider.model_provider ->
  AbstractProperty.abstract_property ->
  Options.imitator_options ->
  AbstractProperty.state_predicate ->
  AbstractProperty.timed_interval ->
  object
    inherit AlgoGeneric.algoGeneric
    method algorithm_name : string
    method process_current_model : unit
    method compute_result : Result.imitator_result
    method run : Result.imitator_result
  end
