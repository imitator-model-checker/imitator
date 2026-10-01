(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: EF reachability with on-the-fly model updates
 *
 * File contributors : Ta Quang Minh
 * Created           : 2026/09/30
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric
open ImitatorUtilities
open ModelProvider


(************************************************************)
(* Shared on-the-fly lifecycle *)
(************************************************************)
class algoEFOnTheFlyBase
    (model : AbstractModel.abstract_model)
    (provider : model_provider)
    (options : Options.imitator_options)
    (ef_algorithm :
      < compute_result : Result.imitator_result;
        mark_interrupted : unit;
        print_progress : unit;
        resume_exploration : unit;
        run_exploration : Result.imitator_result option;
        set_interruption_check : (unit -> bool) -> unit;
        set_defer_result_packaging : bool -> unit;
        .. >) =
  object (self)
    inherit algoGeneric model options

    initializer
      ef_algorithm#set_defer_result_packaging true

    method algorithm_name = "EF (on-the-fly model updates)"

    method process_current_model =
      print_message Verbose_standard (ModelPrinter.string_of_model model)

    method compute_result =
      let interrupted = ref false in
      let previous_sigint_handler =
        Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> interrupted := true))
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sys.signal Sys.sigint previous_sigint_handler))
        (fun () ->
          ef_algorithm#set_interruption_check (fun () -> !interrupted);
          self#print_algo_message Verbose_standard
            "Exploring initial model.";
          self#process_current_model;

          ignore (ef_algorithm#run_exploration);
          ef_algorithm#print_progress;
          let finished = ref false in

          while not !finished && not !interrupted do
            match provider#wait_for_update (fun () -> !interrupted) with
            | Updated content ->
                self#print_algo_message Verbose_standard
                  ("Received model update: " ^ content);
                self#process_current_model;
                ef_algorithm#resume_exploration;
                ef_algorithm#print_progress
            | Finished ->
                self#print_algo_message Verbose_standard
                  "Model provider finished; exploring final model.";
                ef_algorithm#resume_exploration;
                finished := true
            | Stop_requested ->
                ef_algorithm#mark_interrupted;
                finished := true
          done;

          if !interrupted then begin
            ef_algorithm#mark_interrupted;
            options#set_output_result true;
            self#print_algo_message Verbose_standard
              "SIGINT received; preserving partial EF result in the result file."
          end
          else
            self#print_algo_message Verbose_standard
              "On-the-fly EF analysis completed.";
          ef_algorithm#compute_result)

    method run = self#compute_result
  end


(************************************************************)
(* Untimed on-the-fly EF *)
(************************************************************)
class algoEFOnthefly
    (model : AbstractModel.abstract_model)
    (provider : model_provider)
    (property : AbstractProperty.abstract_property)
    (options : Options.imitator_options)
    (state_predicate : AbstractProperty.state_predicate) =
  algoEFOnTheFlyBase model provider options
    (new AlgoEF.algoEF model property options state_predicate)


(************************************************************)
(* Timed on-the-fly EF *)
(************************************************************)
class algoEFtimedonthefly
    (model : AbstractModel.abstract_model)
    (provider : model_provider)
    (property : AbstractProperty.abstract_property)
    (options : Options.imitator_options)
    (state_predicate : AbstractProperty.state_predicate)
    (timed_interval : AbstractProperty.timed_interval) =
  algoEFOnTheFlyBase model provider options
    (new AlgoEF.algoEFtimed model property options state_predicate timed_interval)
