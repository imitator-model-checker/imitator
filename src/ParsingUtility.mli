(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2014/03/15
 * Last modified: 2014/03/15
 *
 ****************************************************************)


open AbstractModel

(** Compile the different files and set the models *)
val compile: Options.imitator_options -> abstract_model * pi0 * v0

