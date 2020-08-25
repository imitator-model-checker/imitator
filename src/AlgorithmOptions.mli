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
 * Last modified     : 2020/08/25
 *
 ************************************************************)


val inclusion_needed : AbstractProperty.abstract_property -> bool
val merge_needed     : AbstractProperty.abstract_property -> bool
val is_cartography   : AbstractProperty.abstract_property -> bool
