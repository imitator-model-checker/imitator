(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Describes some of the options associated with algorithms
 *
 * File contributors : Étienne André
 * Created           : 2020/08/25
 * Last modified     : 2021/09/16
 *
 ************************************************************)


val default_state_comparison	: AbstractProperty.abstract_property -> AbstractAlgorithm.state_comparison_operator
(* Does the use of a given state_comparison_operator for a given abstract_property preserve the result correctness? *)
val is_state_comparison_correct	: AbstractProperty.abstract_property -> AbstractAlgorithm.state_comparison_operator -> bool
val merge_needed				: AbstractProperty.abstract_property -> bool

(* val default_exploration_order : AbstractProperty.abstract_property -> AbstractAlgorithm.exploration_order *)

val supports_witness			: AbstractProperty.abstract_property -> bool

val supports_exemplification	: AbstractProperty.abstract_property -> bool

val is_cartography				: AbstractProperty.abstract_property -> bool
(* Gives a textual description of a property *)
val text_of_property			: AbstractProperty.abstract_property -> string
