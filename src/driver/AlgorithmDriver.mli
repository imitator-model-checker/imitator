(************************************************************
 *
 *                       IMITATOR
 *
 * Aarhus University
 *
 * Module description: Set the options and select the algorithm to run for a model and a property
 *
 * File contributors : Mikael Bisgaard Dahlsen-Jensen
 * Created           : 2026/11/14
 *
 ************************************************************)

(** Set the options and select the algorithm to run for a model and a property. *)


(** Set the options not given by the user to their default value for the property, then recall them and warn about incompatibilities *)
val set_default_options : Options.imitator_options -> AbstractModel.abstract_model -> AbstractProperty.abstract_property option -> unit

(** Prepare clock elimination and extrapolation if needed; must be called before creating the algorithm. Aborts the program if the model does not support extrapolation *)
val prepare_model : Options.imitator_options -> AbstractModel.abstract_model -> unit

(** Create the algorithm for the property; the last argument is called on the controller synthesized by PTG algorithms. Aborts the program if the property needs a global clock that the model does not have *)
val algorithm_of_property : Options.imitator_options -> AbstractModel.abstract_model -> AbstractProperty.abstract_property -> (AbstractModel.abstract_model -> unit) -> AlgoGeneric.algoGeneric
