(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with point-based distribution of points. [ACE14,ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/04
 * Last modified     : 2016/04/01
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoGeneric
open DistributedUtilities


(************************************************************)
(************************************************************)
(* Internal exceptions *)
(************************************************************)
(************************************************************)


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoBCCoverDistributed =
	object (self)
	inherit algoGeneric as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*** BADPROG: code shared with AlgoCartoGeneric ***)
	(* The function creating a new instance of the algorithm to call (typically IM or PRP). Initially None, to be updated immediatly after the object creation. *)
	(*** NOTE: this should be a parameter of the class; but cannot due to inheritance from AlgoGeneric ***)
	val mutable algo_instance_function = None
	
		
	
	(************************************************************)
	(* Class methods to simulate class parameters *)
	(************************************************************)
	
	(*** BADPROG: code shared with AlgoBCCoverDistributedMSPointBased ***)
	(* Sets the function creating a new instance of the algorithm to call (typically IM or PRP) *)
	method set_algo_instance_function (f : unit -> AlgoIMK.algoIMK) : unit =
		match algo_instance_function with
		| Some _ -> 
			raise (InternalError("algo_instance_function was already set in algoBCCoverDistributedMSPointBased."))
		| None ->
			algo_instance_function <- Some f
	
	(*** BADPROG: code shared with AlgoBCCoverDistributedMSPointBased ***)
	(* Get the function creating a new instance of the algorithm to call (typically IM or PRP) *)
	method get_algo_instance_function =
		match algo_instance_function with
		| Some f -> f
		| None ->
			raise (InternalError("algo_instance_function not yet set in algoBCCoverDistributedMSPointBased."))

			
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual initialize_variables : unit

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Run IM and return an abstract_im_result *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run_im pi0 patator_termination_function_option =
		(* Create instance of the algorithm to be called *)
		let algo = self#get_algo_instance_function () in
		
		(* Set up the pi0 *)
		(*** NOTE/BADPROG: a bit ugly… pi0 could have been a parameter of the algorithm! ***)
		Input.set_pi0 pi0;
		
		(* Set up the termination function for PaTATOR *)
		begin
		match patator_termination_function_option with
			| None -> ()
			| Some f -> algo#set_patator_termination_function f;
		end;
		
		(* Print some messages *)
		if verbose_mode_greater Verbose_low then(
			(* Retrieve the model *)
			let model = Input.get_model() in
			
			self#print_algo_message Verbose_medium ("**************************************************");
			self#print_algo_message Verbose_medium ("BEHAVIORAL CARTOGRAPHY ALGORITHM: "(* ^ (string_of_int !current_iteration) ^ ""*));
			self#print_algo_message Verbose_low ("Running IM for the following reference valuation:" (*^ (string_of_int !current_iteration)*));
			self#print_algo_message Verbose_low (ModelPrinter.string_of_pi0 model pi0);
		);

		(* Save verbose mode *)
		let global_verbose_mode = get_verbose_mode() in 
		
		(* Prevent the verbose messages (except in verbose modes high or total) *)
		if not (verbose_mode_greater Verbose_high) then
				set_verbose_mode Verbose_mute;

		(* Call IM *)
		
		(*** NOTE: the initial state is computed again and again for each new instance of IM; TO OPTIMIZE? ***)
		
		let imitator_result = algo#run() in

		(* Get the verbose mode back *)
		set_verbose_mode global_verbose_mode;
		
		self#print_algo_message Verbose_low ("Finished a computation of " ^ (algo#algorithm_name) ^ ".");
		
		(* Checking the result type *)
		let im_result = match imitator_result with
			(* Result for IM, IMK, IMunion *)
			| IM_result im_result -> im_result
			(* Other *)
			| _ -> raise (InternalError("An im_result is expected as an output of the execution of " ^ algo#algorithm_name ^ "."))
		in
		
		(* Abstracting the result *)
		let abstract_im_result = AlgoCartoGeneric.abstract_im_result_of_im_result im_result pi0 in
		
		(* Return the abstract result *)
		abstract_im_result
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual run : unit -> Result.imitator_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
