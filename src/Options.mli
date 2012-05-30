(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/05/10
 * Last modified: 2012/05/30
 *
 ****************************************************************)

class imitator_options :
	object
		val mutable nb_args : int 
		val mutable acyclic : bool ref
		val mutable branch_and_bound : bool ref
		val mutable cart : bool ref
		val mutable dynamic : bool ref
		val mutable fancy : bool ref
		val mutable file : string ref
		val mutable fromGML : bool ref
		val mutable imitator_mode : Global.imitator_mode ref
		val mutable inclusion : bool ref
		val mutable nb_args : int
		val mutable no_dot : bool ref
		val mutable no_log : bool ref
		val mutable no_merging : bool ref
		val mutable no_random : bool ref
		val mutable pi0file : string ref
		val mutable pi_compatible : bool ref
		val mutable post_limit : int option ref
		val mutable program_prefix : string ref
		val mutable pta2clp : bool ref
		val mutable pta2gml : bool ref
		val mutable statistics : bool ref
		val mutable step : NumConst.t ref
		val mutable sync_auto_detection : bool ref
		val mutable time_limit : int option ref
		val mutable timed_mode : bool ref
		val mutable tree : bool ref
		val mutable union : bool ref
		val mutable with_parametric_log : bool ref
		
		method acyclic : bool
		method acyclic_unset : unit
		method branch_and_bound : bool
		method cart : bool
		method dynamic : bool
		method fancy : bool
		method file : string
		method fromGML : bool
		method imitator_mode : Global.imitator_mode
		method inclusion : bool
		method nb_args : int
		method no_dot : bool
		method no_log : bool
		method no_merging : bool
		method no_random : bool
		method parse : unit
		method pi0file : string
		method pi_compatible : bool
		method post_limit : int option
		method program_prefix : string
		method pta2clp : bool
		method pta2gml : bool
		method statistics : bool
		method step : NumConst.t
		method sync_auto_detection : bool
		method time_limit : int option
		method timed_mode : bool
		method tree : bool
		method union : bool
		method with_parametric_log : bool
end