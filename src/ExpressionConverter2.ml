(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module aims to convert a parsed expression to a abstract typed expression
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 * Last modified     : 2021/11/20
 *
 ************************************************************)

open Constants
open Exceptions
open ParsingStructure
open AbstractModel
open DiscreteExpressions
open DiscreteType
open TypeChecker2


type discrete_index = int

(* Variable kind type represent a variable or a constant kind *)
type variable_kind =
    | Variable_kind of discrete_index
    | Constant_kind of DiscreteValue.discrete_value

(* Know if variable with a given name is a variable or a constant *)
let variable_kind_of_variable_name variable_infos variable_name =

    (* First check whether this is a constant *)
    if Hashtbl.mem variable_infos.constants variable_name then (
        let value = Hashtbl.find variable_infos.constants variable_name in
        Constant_kind value
    )
    (* Otherwise: a variable *)
    else
        Variable_kind (Hashtbl.find variable_infos.index_of_variables variable_name)

(** Convert a Boolean operator to its abstract model *)
let convert_parsed_relop = function
	| PARSED_OP_L -> OP_L
	| PARSED_OP_LEQ	-> OP_LEQ
	| PARSED_OP_EQ	-> OP_EQ
	| PARSED_OP_NEQ	-> OP_NEQ
	| PARSED_OP_GEQ	-> OP_GEQ
	| PARSED_OP_G -> OP_G


(*------------------------------------------------------------*)
(* Convert an array of variable coef into a linear term *)
(*------------------------------------------------------------*)
let linear_term_of_array array_of_coef constant =
  (* Create an empty list of members *)
  let members = ref [] in
  (* Iterate on the coef *)
  Array.iteri (fun variable_index coef ->
      if NumConst.neq coef NumConst.zero then (
        (* Add the member *)
        members := (coef, variable_index) :: !members;
      );
    ) array_of_coef;
  (* Create the linear term *)
  LinearConstraint.make_pxd_linear_term !members constant


(*------------------------------------------------------------*)
(* Convert a ParsingStructure.linear_expression into an array of coef and constant *)
(*------------------------------------------------------------*)
let array_of_coef_of_linear_expression index_of_variables constants linear_expression =
  (* Create an array of coef *)
  let array_of_coef = Array.make (Hashtbl.length index_of_variables) NumConst.zero in
  (* Create a zero constant *)
  let constant = ref NumConst.zero in

  (* Internal function to update the array for a linear term *)
  let update_array_linear_term mul_coef = function
    (* Case constant -> update the constant with the coef *)
    | Constant c -> constant := NumConst.add !constant (NumConst.mul c mul_coef);
      (* Case variables -> update the array with the coef  *)
    | Variable (coef, variable_name) ->
      (* Try to find the variable_index *)
      if Hashtbl.mem index_of_variables variable_name then (
        let variable_index = Hashtbl.find index_of_variables variable_name in
        (* Update the variable with its coef *)
        array_of_coef.(variable_index) <- NumConst.add array_of_coef.(variable_index) (NumConst.mul coef mul_coef);
        (* Try to find a constant *)
      ) else (
        if Hashtbl.mem constants variable_name then (
          (* Retrieve the value of the global constant *)
          let value = Hashtbl.find constants variable_name in
          let numconst_value = DiscreteValue.to_numconst_value value in
          (* Update the NumConst *)
          constant := NumConst.add !constant (NumConst.mul (NumConst.mul numconst_value coef) mul_coef);
        ) else (
          raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))
        )
      );
  in

  (* Internal function to update the array for a linear expression *)
  let rec update_array_linear_expression = function
    | Linear_term lt -> update_array_linear_term NumConst.one lt
    | Linear_plus_expression (le, lt) ->
      (* Fill the array with le *)
      update_array_linear_expression le;
      (* Fill the array with lt *)
      update_array_linear_term NumConst.one lt;
    | Linear_minus_expression (le, lt) ->
      (* Fill the array with le *)
      update_array_linear_expression le;
      (* Fill the array with lt *)
      update_array_linear_term NumConst.minus_one lt;
  in
  (* Call the recursive function *)
  update_array_linear_expression linear_expression;
  (* Return the array of coef and the constant *)
  array_of_coef, !constant

(*------------------------------------------------------------*)
(* Convert a ParsingStructure.linear_constraint into a Constraint.linear_inequality *)
(*------------------------------------------------------------*)
let linear_inequality_of_linear_constraint index_of_variables constants (linexpr1, relop, linexpr2) =
    (* Get the array of variables and constant associated to the linear terms *)
    let array1, constant1 = array_of_coef_of_linear_expression index_of_variables constants linexpr1 in
    let array2, constant2 = array_of_coef_of_linear_expression index_of_variables constants linexpr2 in
    (* Consider the operator *)
    match relop with
    (* a < b <=> b - a > 0 *)
    | PARSED_OP_L ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array2 array1 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant2 constant1 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_g
        (* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_g *)

    (* a <= b <=> b - a >= 0 *)
    | PARSED_OP_LEQ ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array2 array1 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant2 constant1 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_ge
        (* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_ge *)

    (* a = b <=> b - a = 0 *)
    | PARSED_OP_EQ ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array2 array1 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant2 constant1 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_eq

        (* 	(Constraint.substract_linear_terms lt1 lt2), Constraint.Op_eq *)

    (* a >= b <=> a - b >= 0 *)
    | PARSED_OP_GEQ ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array1 array2 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant1 constant2 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_ge
        (* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_ge *)

    (* a > b <=> a - b > 0 *)
    | PARSED_OP_G ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array1 array2 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant1 constant2 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_g
        (* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_g *)

    | PARSED_OP_NEQ ->
        raise (InternalError("Inequality <> not yet supported"))

(*------------------------------------------------------------*)
(* Convert a ParsingStructure.convex_predicate into a Constraint.linear_constraint *)
(*------------------------------------------------------------*)
let linear_constraint_of_convex_predicate variable_infos convex_predicate : LinearConstraint.pxd_linear_constraint =
  try (
    (* Compute a list of inequalities *)
    let linear_inequalities = List.fold_left
        (fun linear_inequalities linear_inequality ->
           match linear_inequality with
           | Parsed_true_constraint -> linear_inequalities
           | Parsed_false_constraint -> raise False_exception
           | Parsed_linear_constraint (linexpr1, relop, linexpr2) -> (linear_inequality_of_linear_constraint variable_infos.index_of_variables variable_infos.constants (linexpr1, relop, linexpr2)) :: linear_inequalities
        ) [] convex_predicate
    in LinearConstraint.make_pxd_constraint linear_inequalities
    (* Stop if any false constraint is found *)
  ) with False_exception -> LinearConstraint.pxd_false_constraint ()

(*------------------------------------------------------------*)
(* Direct conversion of a ParsingStructure.linear_expression into a Parsed_linear_constraint.linear_term *)
(*------------------------------------------------------------*)
let linear_term_of_linear_expression variable_infos linear_expression =
    let index_of_variables = variable_infos.index_of_variables in
    let constants = variable_infos.constants in

    let array_of_coef, constant = array_of_coef_of_linear_expression index_of_variables constants linear_expression in
    linear_term_of_array array_of_coef constant

(*** NOTE: define a top-level function to avoid recursive passing of all common variables ***)
let linear_term_of_parsed_update_arithmetic_expression variable_infos pdae =

    let index_of_variables = variable_infos.index_of_variables in
    let constants = variable_infos.constants in

	(* Create an array of coef *)
	let array_of_coef = Array.make (Hashtbl.length index_of_variables) NumConst.zero in
	(* Create a zero constant *)
	let constant = ref NumConst.zero in

	let rec update_coef_array_in_parsed_update_arithmetic_expression mult_factor = function
		| Parsed_DAE_plus (parsed_update_arithmetic_expression, parsed_update_term) ->
		(* Update coefficients in the arithmetic expression *)
		update_coef_array_in_parsed_update_arithmetic_expression mult_factor parsed_update_arithmetic_expression;
		(* Update coefficients in the term *)
		update_coef_array_in_parsed_update_term mult_factor parsed_update_term;
		| Parsed_DAE_minus (parsed_update_arithmetic_expression, parsed_update_term) ->
		(* Update coefficients in the arithmetic expression *)
		update_coef_array_in_parsed_update_arithmetic_expression mult_factor parsed_update_arithmetic_expression;
		(* Update coefficients in the term: multiply by -1 for negation *)
		update_coef_array_in_parsed_update_term (NumConst.neg mult_factor) parsed_update_term;
		| Parsed_DAE_term parsed_update_term ->
		update_coef_array_in_parsed_update_term mult_factor parsed_update_term;

	and update_coef_array_in_parsed_update_term mult_factor = function
		(* Multiplication is only allowed with a constant multiplier *)
		| Parsed_DT_mul (parsed_update_term, parsed_update_factor) ->
		(* Valuate the term *)
		let valued_term = ParsingStructureUtilities.try_reduce_parsed_term constants parsed_update_term in
		let numconst_valued_term = DiscreteValue.to_numconst_value valued_term in
		(* Update coefficients *)
		update_coef_array_in_parsed_update_factor (NumConst.mul numconst_valued_term mult_factor) parsed_update_factor

		| Parsed_DT_div (parsed_update_term, parsed_update_factor) ->
		(* Valuate the discrete factor *)
		let valued_factor = ParsingStructureUtilities.try_reduce_parsed_factor constants parsed_update_factor in
		let numconst_valued_factor = DiscreteValue.to_numconst_value valued_factor in
		(* Update coefficients *)
		update_coef_array_in_parsed_update_term (NumConst.div mult_factor numconst_valued_factor) parsed_update_term

		| Parsed_DT_factor parsed_update_factor ->
		update_coef_array_in_parsed_update_factor mult_factor parsed_update_factor

	and update_coef_array_in_parsed_update_factor mult_factor = function
		| Parsed_DF_variable variable_name ->
			(* Try to find the variable_index *)
			if Hashtbl.mem index_of_variables variable_name then (
				let variable_index = Hashtbl.find index_of_variables variable_name in
				(* Update the array *)
				array_of_coef.(variable_index) <- NumConst.add array_of_coef.(variable_index) (mult_factor);
				(* Try to find a constant *)
			) else (
				if Hashtbl.mem constants variable_name then (
				(* Retrieve the value of the global constant *)
				let value = Hashtbl.find constants variable_name in
				let numconst_value = DiscreteValue.to_numconst_value value in
				(* Update the constant *)
				constant := NumConst.add !constant (NumConst.mul mult_factor numconst_value)
				) else (
				raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` in function 'update_coef_array_in_parsed_update_factor' although this should have been checked before."))
				)
			)
		| Parsed_DF_constant var_value ->
            (* Update the constant *)
            let numconst_value = DiscreteValue.to_numconst_value var_value in
            constant := NumConst.add !constant (NumConst.mul mult_factor numconst_value)
		| Parsed_DF_unary_min parsed_discrete_factor ->
			update_coef_array_in_parsed_update_factor mult_factor parsed_discrete_factor
		| Parsed_DF_expression parsed_update_arithmetic_expression ->
            update_coef_array_in_parsed_update_arithmetic_expression mult_factor parsed_update_arithmetic_expression
		| _ as factor ->
            raise (InternalError ("Use of " ^ ParsingStructureUtilities.label_of_parsed_factor_constructor factor ^ " is forbidden in linear term, something failed before."))
	in

	(* Call the recursive function updating the coefficients *)
	update_coef_array_in_parsed_update_arithmetic_expression NumConst.one pdae;

	(* Create the linear term *)
	linear_term_of_array array_of_coef !constant

let linear_term_of_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expression expr ->
        linear_term_of_parsed_update_arithmetic_expression variable_infos expr
    | expr ->
        raise (
            InternalError (
                "Impossible to convert boolean expression \""
                ^ ParsingStructureUtilities.string_of_parsed_discrete_boolean_expression variable_infos expr
                ^ "\" to a linear expression, but it should was already type checked, maybe type check has failed"
            )
        )

let linear_term_of_parsed_boolean_expression variable_infos = function
    | Parsed_Discrete_boolean_expression expr ->
        linear_term_of_parsed_discrete_boolean_expression variable_infos expr
    | _ as expr ->
        raise (
            InternalError (
                "Impossible to convert boolean expression \""
                ^ ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos expr
                ^ "\" to a linear expression, but it should was already type checked, maybe type check has failed"
            )
        )

let linear_term_of_global_expression variable_infos = function
    | Parsed_global_expression expr ->
        linear_term_of_parsed_boolean_expression variable_infos expr


(* Convert discrete expressions *)

let rec global_expression_of_typed_global_expression variable_infos = function
    | Typed_global_expr (expr, discrete_type) ->
        global_expression_of_typed_boolean_expression variable_infos expr discrete_type

(* TODO benjamin pas beau *)
and global_expression_of_typed_boolean_expression variable_infos expr = function
    | Var_type_discrete_number _ ->
        Arithmetic_expression (
            discrete_arithmetic_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_bool ->
        Bool_expression (
            bool_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_binary_word _ ->
        Binary_word_expression (
            binary_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_array _ ->
        Array_expression (
            array_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_list _ ->
        List_expression (
            list_expression_of_typed_boolean_expression variable_infos expr
        )

and discrete_arithmetic_expression_of_typed_boolean_expression variable_infos = function
	| Typed_discrete_bool_expr (expr, discrete_type) ->
	    (match discrete_type with
	    | Var_type_discrete_number Var_type_discrete_rational ->
	        Rational_arithmetic_expression (rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr)
	    | Var_type_discrete_number Var_type_discrete_int ->
	        Int_arithmetic_expression (int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr)
	    | _ -> raise (InternalError ("a" ^ DiscreteType.string_of_var_type_discrete discrete_type))
	    )
	| _ -> raise (InternalError "b")

and discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
	| Typed_arithmetic_expr (expr, discrete_type) ->
	    (match discrete_type with
	    | Var_type_discrete_number Var_type_discrete_rational ->
	        Rational_arithmetic_expression (rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr)
	    | Var_type_discrete_number Var_type_discrete_int ->
	        Int_arithmetic_expression (int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr)
	    | _ -> raise (InternalError "c")
	    )
	| _ -> raise (InternalError "d")

(* TODO benjamin CLEAN review this function *)
and discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos = function
	| Typed_plus (expr, term, discrete_number_type) ->
        (match discrete_number_type with
        | Var_type_discrete_rational ->
            Rational_arithmetic_expression (
                DAE_plus (
                    rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    rational_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | Var_type_discrete_int ->
            Int_arithmetic_expression (
                Int_plus (
                    int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    int_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | _ -> raise (InternalError "e")
        )

	| Typed_minus (expr, term, discrete_number_type) ->
        (match discrete_number_type with
        | Var_type_discrete_rational ->
            Rational_arithmetic_expression (
                DAE_minus (
                    rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    rational_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | Var_type_discrete_int ->
            Int_arithmetic_expression (
                Int_minus (
                    int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    int_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | _ -> raise (InternalError "f")
        )

	| Typed_term (term, discrete_type) ->
        (match discrete_type with
        | Var_type_discrete_number Var_type_discrete_rational ->
            Rational_arithmetic_expression (DAE_term (rational_arithmetic_expression_of_typed_term variable_infos term))
        | Var_type_discrete_number Var_type_discrete_int ->
            Int_arithmetic_expression (Int_term (int_arithmetic_expression_of_typed_term variable_infos term))
        | _ -> raise (InternalError "g")
        )

(* --------------------*)
(* Bool conversion *)
(* --------------------*)

and bool_expression_of_typed_boolean_expression variable_infos = function
	| Typed_And (l_expr, r_expr) ->
	    And_bool (
	        bool_expression_of_typed_boolean_expression variable_infos l_expr,
	        bool_expression_of_typed_boolean_expression variable_infos r_expr
	    )

	| Typed_Or (l_expr, r_expr) ->
	    Or_bool (
	        bool_expression_of_typed_boolean_expression variable_infos l_expr,
	        bool_expression_of_typed_boolean_expression variable_infos r_expr
	    )

    | Typed_discrete_bool_expr (expr, _) ->
        Discrete_boolean_expression (
            bool_expression_of_typed_discrete_boolean_expression variable_infos expr
        )

and bool_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        bool_expression_of_typed_arithmetic_expression variable_infos expr

	| Typed_comparison (l_expr, relop, r_expr, _, _) ->
	    bool_expression_of_typed_comparison variable_infos l_expr relop r_expr

	| Typed_comparison_in (in_expr, lw_expr, up_expr, _) ->
	    Expression_in (
	        discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos in_expr,
	        discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos lw_expr,
	        discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos up_expr
	    )

	| Typed_bool_expr (expr, _) ->
	    Boolean_expression (
	        bool_expression_of_typed_boolean_expression variable_infos expr
	    )

	| Typed_not_expr expr ->
	    Not_bool (
	        bool_expression_of_typed_boolean_expression variable_infos expr
	    )

and bool_expression_of_typed_comparison variable_infos l_expr relop r_expr =
    let discrete_type = TypeChecker2.type_of_typed_discrete_boolean_expression l_expr in

    match discrete_type with
    | Var_type_discrete_number _ ->
        Expression (
            discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos r_expr
        )
    | Var_type_discrete_bool ->
        Boolean_comparison (
            bool_expression_of_typed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            bool_expression_of_typed_discrete_boolean_expression variable_infos r_expr
        )
    | Var_type_discrete_binary_word _ ->
        Binary_comparison (
            binary_expression_of_typed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            binary_expression_of_typed_discrete_boolean_expression variable_infos r_expr
        )
    | Var_type_discrete_array _ ->
        Array_comparison (
            array_expression_of_typed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            array_expression_of_typed_discrete_boolean_expression variable_infos r_expr
        )
    | Var_type_discrete_list _ ->
        List_comparison (
            list_expression_of_typed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            list_expression_of_typed_discrete_boolean_expression variable_infos r_expr
        )
    | _ -> raise (InternalError "h")

and bool_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
	    bool_expression_of_typed_term variable_infos term
    | _ ->
        raise (InternalError "i")

and bool_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
	    bool_expression_of_typed_factor variable_infos factor
    | _ ->
        raise (InternalError "j")

and bool_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> DB_constant (DiscreteValue.bool_value value)
        | Variable_kind discrete_index -> DB_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    DB_constant (DiscreteValue.bool_value value)

    | Typed_expr (expr, _) ->
        bool_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, inner_type, discrete_type) ->
        Bool_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    bool_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "k")

and bool_expression_of_typed_function_call variable_infos argument_expressions = function
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Bool_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    (*
    | "list_mem" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        List_mem (
            global_expression_of_typed_boolean_expression variable_infos arg_0,
            list_expression_of_typed_boolean_expression variable_infos arg_1
        )
    *)
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Rational conversion *)
(* --------------------*)

and rational_arithmetic_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "l")

and rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "m")

and rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_plus (expr, term, _) ->
	    DAE_plus (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_minus (expr, term, _) ->
	    DAE_minus (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_term (term, _) ->
	    DAE_term (
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

and rational_arithmetic_expression_of_typed_term variable_infos = function
	| Typed_mul (term, factor, _) ->
	    DT_mul (
	        rational_arithmetic_expression_of_typed_term variable_infos term,
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_div (term, factor, _) ->
	    DT_div (
	        rational_arithmetic_expression_of_typed_term variable_infos term,
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_factor (factor, _) ->
	    DT_factor (
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

and rational_arithmetic_expression_of_typed_factor variable_infos = function
	| Typed_unary_min (factor, _) ->
	    DF_unary_min (
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> DF_constant (DiscreteValue.to_numconst_value value)
        | Variable_kind discrete_index -> DF_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    DF_constant (DiscreteValue.to_numconst_value value)

	| Typed_expr (expr, _) ->
	    DF_expression (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
        )

    | Typed_access (factor, index_expr, inner_type, discrete_type) ->
        Rational_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    rational_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "n")

and rational_expression_of_typed_function_call variable_infos argument_expressions = function
    | "pow" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Rational_pow (
            rational_arithmetic_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1
        )
    | "rational_of_int" ->
        let arg_0 = List.nth argument_expressions 0 in
        DF_rational_of_int (
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_0
        )
    (*
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Rational_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    *)
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Int conversion *)
(* --------------------*)

and int_arithmetic_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "o")

and int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "p")

and int_arithmetic_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_plus (expr, term, _) ->
	    Int_plus (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_minus (expr, term, _) ->
	    Int_minus (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_term (term, _) ->
	    Int_term (
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

and int_arithmetic_expression_of_typed_term variable_infos = function
	| Typed_mul (term, factor, _) ->
	    Int_mul (
	        int_arithmetic_expression_of_typed_term variable_infos term,
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_div (term, factor, _) ->
	    Int_div (
	        int_arithmetic_expression_of_typed_term variable_infos term,
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_factor (factor, _) ->
	    Int_factor (
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

and int_arithmetic_expression_of_typed_factor variable_infos = function
	| Typed_unary_min (factor, _) ->
	    Int_unary_min (
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Int_constant (DiscreteValue.to_int_value value)
        | Variable_kind discrete_index -> Int_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    Int_constant (DiscreteValue.to_int_value value)

	| Typed_expr (expr, _) ->
	    Int_expression (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
        )

    | Typed_access (factor, index_expr, inner_type, discrete_type) ->
        Int_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    int_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "q")

and int_expression_of_typed_function_call variable_infos argument_expressions = function
    | "pow" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Int_pow (
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Int_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    (* TODO benjamin, in the future replace raise by custom function call as comment below *)
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Binary word conversion *)
(* --------------------*)

and binary_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        binary_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "r")

and binary_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        binary_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "s")

and binary_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
	        binary_expression_of_typed_term variable_infos term
    | _ ->
        raise (InternalError "t")

and binary_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
	        binary_expression_of_typed_factor variable_infos factor
    | _ ->
        raise (InternalError "u")

and binary_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, discrete_type) ->

	    let binary_word_length =
	        match discrete_type with
	        | Var_type_discrete_binary_word length -> length
	        | _ -> raise (InternalError "v")
	    in

        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Binary_word_constant (DiscreteValue.binary_word_value value)
        | Variable_kind discrete_index -> Binary_word_variable (discrete_index, binary_word_length)
        )

	| Typed_constant (value, discrete_type) ->
	    Binary_word_constant (DiscreteValue.binary_word_value value)

	| Typed_expr (expr, _) ->
        binary_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, inner_type, discrete_type) ->

	    let binary_word_length =
	        match inner_type with
	        | Var_type_discrete_binary_word length -> length
	        | _ -> raise (InternalError "w")
	    in

        Binary_word_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr,
            binary_word_length
        )

	| Typed_function_call (function_name, argument_expressions, discrete_type) ->

	    let binary_word_length =
	        match discrete_type with
	        | Var_type_discrete_binary_word length -> length
	        | _ -> raise (InternalError "x")
	    in

	    binary_expression_of_typed_function_call variable_infos binary_word_length argument_expressions function_name

	| _ -> raise (InternalError "y")

and binary_expression_of_typed_function_call variable_infos binary_word_length argument_expressions = function
    | "shift_left" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_shift_left (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "shift_right" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_shift_right (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "fill_left" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_fill_left (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "fill_right" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_fill_right (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "logand" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_and (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            binary_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "logor" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_or (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            binary_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "logxor" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_xor (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            binary_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )

    | "lognot" ->
        let arg_0 = List.nth argument_expressions 0 in

        Logical_not (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            binary_word_length
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Binary_word_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Array conversion *)
(* --------------------*)

and array_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        array_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "z")

and array_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        array_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "aa")

and array_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
        array_expression_of_typed_term variable_infos term
    | _ ->
        raise (InternalError "ab")

and array_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
        array_expression_of_typed_factor variable_infos factor
    | _ ->
        raise (InternalError "ac")

and array_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Array_constant (DiscreteValue.array_value value)
        | Variable_kind discrete_index -> Array_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    Array_constant (DiscreteValue.array_value value)

    | Typed_array (expr_array, discrete_type) ->
        let inner_type =
            match discrete_type with
            | Var_type_discrete_array (inner_type, _) -> inner_type
            | _ -> raise (InternalError "")
        in
        Literal_array (Array.map (fun expr -> global_expression_of_typed_boolean_expression variable_infos expr inner_type) expr_array)

	| Typed_expr (expr, _) ->
        array_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, inner_type, discrete_type) ->
        Array_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    array_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "ad")

and array_expression_of_typed_function_call variable_infos argument_expressions = function
    | "array_append" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Array_concat (
            array_expression_of_typed_boolean_expression variable_infos arg_0,
            array_expression_of_typed_boolean_expression variable_infos arg_1
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Array_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | function_name -> raise (UndefinedFunction function_name)
    (*
    Array_function_call (
        name,
        List.map (global_expression_of_parsed_boolean_expression variable_infos) argument_expressions
    )
    *)

(* --------------------*)
(* List conversion *)
(* --------------------*)

and list_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        list_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "ae")

and list_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        list_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "af")

and list_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
        list_expression_of_typed_term variable_infos term
    | _ ->
        raise (InternalError "ag")

and list_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
        list_expression_of_typed_factor variable_infos factor
    | _ ->
        raise (InternalError "ah")

and list_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> List_constant (DiscreteValue.list_value value)
        | Variable_kind discrete_index -> List_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    List_constant (DiscreteValue.list_value value)

    | Typed_list (expr_list, discrete_type) ->
        let inner_type =
            match discrete_type with
            | Var_type_discrete_list inner_type -> inner_type
            | _ -> raise (InternalError "")
        in
        Literal_list (List.map (fun expr -> global_expression_of_typed_boolean_expression variable_infos expr inner_type) expr_list)

	| Typed_expr (expr, _) ->
        list_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, inner_type, discrete_type) ->
        List_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    list_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "ai")

and list_expression_of_typed_function_call variable_infos argument_expressions = function
    (*
    | "list_cons" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        List_cons (
            global_expression_of_typed_boolean_expression variable_infos arg_0,
            list_expression_of_typed_boolean_expression variable_infos arg_1
        )
    *)
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | "list_tl" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_tl (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | "list_rev" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_rev (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Access conversion *)
(* --------------------*)

and expression_access_type_of_typed_factor variable_infos factor =
    let discrete_type = TypeChecker2.type_of_typed_discrete_factor factor in
    (match discrete_type with
    | Var_type_discrete_array _ ->
        Expression_array_access (
            array_expression_of_typed_factor variable_infos factor
        )
    | Var_type_discrete_list _ ->
        Expression_list_access (
            list_expression_of_typed_factor variable_infos factor
        )
    | _ ->
        raise (InternalError (
            "An access on other element than an array or a list was found, "
            ^ " although it was been type checked before."
        ))
    )
    (*
    | Typed_access (factor, _, inner_type, discrete_type) as typed_access ->
        (match discrete_type with
        | Var_type_discrete_array _ ->
            Expression_array_access (
                array_expression_of_typed_factor variable_infos factor
            )
        | Var_type_discrete_list _ ->
            Expression_list_access (
                list_expression_of_typed_factor variable_infos factor
            )
        )
    *)

let nonlinear_constraint_of_typed_nonlinear_constraint = bool_expression_of_typed_discrete_boolean_expression

let rec variable_access_of_typed_variable_access variable_infos = function
    | Typed_variable_name (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> raise (InternalError "Unable to set a constant expression")
        | Variable_kind discrete_index -> Discrete_variable_index discrete_index
        )

    | Typed_variable_access (variable_access, index_expr, discrete_type) ->
        Discrete_variable_access (
            variable_access_of_typed_variable_access variable_infos variable_access,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )


let convert_discrete_init3 variable_infos variable_name expr =
    (* Get typed expression *)
    let typed_expr = TypeChecker2.check_discrete_init3 variable_infos variable_name expr in
    (* Print *)
    ImitatorUtilities.print_message Verbose_standard (TypeChecker2.string_of_typed_global_expression variable_infos typed_expr);
    (* Convert *)
    global_expression_of_typed_global_expression variable_infos typed_expr

let convert_discrete_constant initialized_constants (name, expr, var_type) =

    let variable_infos = {
        constants = initialized_constants;
        variable_names = [];
        index_of_variables = Hashtbl.create 0;
        removed_variable_names = [];
        type_of_variables = (fun _ -> raise (TypeError "oops!"));
    }
    in

    let typed_expr = TypeChecker2.check_constant_expression variable_infos (name, expr, var_type) in
    global_expression_of_typed_global_expression variable_infos typed_expr



(*------------------------------------------------------------*)
(* Convert a guard *)
(*------------------------------------------------------------*)
let nonlinear_constraint_of_convex_predicate variable_infos guard =
    (* Type check guard *)
    let typed_guard = TypeChecker2.check_guard variable_infos guard in

    let str_typed_nonlinear_constraints = List.map (string_of_typed_discrete_boolean_expression variable_infos) typed_guard in
    let str = OCamlUtilities.string_of_list_of_string_with_sep "\n & " str_typed_nonlinear_constraints in
    ImitatorUtilities.print_message Verbose_standard str;

    (* Convert *)
    let converted_nonlinear_constraints = List.rev_map (nonlinear_constraint_of_typed_nonlinear_constraint variable_infos) typed_guard in

    (* Try reduce *)
    NonlinearConstraint.Nonlinear_constraint converted_nonlinear_constraints
    (* TODO benjamin here add reducing with Some *)
    (*
    let reduced_nonlinear_constraints = converted_nonlinear_constraints in
    (match reduced_nonlinear_constraints with
    | Some true -> True_guard
    | Some false -> False_guard
    | None -> Discrete_guard (reduced_nonlinear_constraints)
    )
    *)

(* Split convex_predicate into two lists *)
(* One only contain discrete expression to nonlinear_constraint *)
(* One that doesn't only contain discrete expression to linear_constraint *)
let split_convex_predicate_into_discrete_and_continuous variable_infos convex_predicate =
  (* Compute a list of inequalities *)
  let partitions = List.partition
    (fun nonlinear_inequality ->
       match nonlinear_inequality with
       (* TODO benjamin REFACTOR, in ParsingStructureUtilities create a function that check if a nonlinear constraint is true or false *)
       | Parsed_arithmetic_expression (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant v))) when DiscreteValue.bool_value v = true -> true
       | Parsed_arithmetic_expression (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant v))) when DiscreteValue.bool_value v = false -> raise False_exception
       | nonlinear_constraint -> ParsingStructureUtilities.only_discrete_in_nonlinear_expression variable_infos nonlinear_constraint
    ) convex_predicate
    in
    (* Get discrete part as a nonlinear constraint but convert back continuous part to a linear constraint *)
    let discrete_part, continuous_part = partitions in
        discrete_part,
        List.map (fun nonlinear_constraint -> ParsingStructureUtilities.linear_constraint_of_nonlinear_constraint nonlinear_constraint) continuous_part


let convert_guard variable_infos guard_convex_predicate =
    try (

        (* Separate the guard into a discrete guard (on discrete variables) and a continuous guard (on all variables) *)
        let discrete_guard_convex_predicate, continuous_guard_convex_predicate = split_convex_predicate_into_discrete_and_continuous variable_infos guard_convex_predicate in

        match discrete_guard_convex_predicate, continuous_guard_convex_predicate with
        (* No inequalities: true *)
        | [] , [] -> True_guard
        (* Only discrete inequalities: discrete *)
        | discrete_guard_convex_predicate , [] ->
            Discrete_guard (
                nonlinear_constraint_of_convex_predicate variable_infos discrete_guard_convex_predicate
            )

        (* Only continuous inequalities: continuous *)
        | [] , continuous_guard_convex_predicate ->
            Continuous_guard (
                linear_constraint_of_convex_predicate variable_infos continuous_guard_convex_predicate
            )

        (* Otherwise: both *)
        | discrete_guard_convex_predicate , continuous_guard_convex_predicate ->
            (* Convert both parts *)
            let discrete_guard = nonlinear_constraint_of_convex_predicate variable_infos discrete_guard_convex_predicate in
            let continuous_guard = linear_constraint_of_convex_predicate variable_infos continuous_guard_convex_predicate in

            (* TODO maybe it's possible to make this optimisation with non linear discrete guard ? *)
            (*** NOTE: try to simplify a bit if possible (costly, but would save a lot of time later if checks are successful) ***)
            (*      let intersection = LinearConstraint.pxd_intersection_with_d continuous_guard discrete_guard in*)

            (*      if LinearConstraint.pxd_is_true intersection then True_guard*)
            (*      else if LinearConstraint.pxd_is_false intersection then False_guard*)
            (*      else*)
            (* Else create mixed guard as planned *)
            Discrete_continuous_guard
            {
                discrete_guard = discrete_guard;
                continuous_guard = continuous_guard;
            }

    (* If some false construct found: false guard *)
    ) with False_exception -> False_guard

let convert_update variable_infos variable_access expr =
    let typed_variable_access, typed_expr = TypeChecker2.check_update variable_infos variable_access expr in
    variable_access_of_typed_variable_access variable_infos typed_variable_access,
    global_expression_of_typed_global_expression variable_infos typed_expr

let convert_conditional variable_infos expr =
    (* Check *)
    let typed_expr = TypeChecker2.check_conditional variable_infos expr in
    (* Convert *)
    bool_expression_of_typed_boolean_expression variable_infos typed_expr