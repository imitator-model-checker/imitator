# Updates

## Sequential and continuous updates



## Assignment mode

### Assignment by copy

In an update, in both sequential or not-sequential, variables are assigned by copy. It means that when we make an assignment of one variable `x` to another `y`, the value of `x` is copied to `y`. The two variables are independent. Therefore, modifying the value of one of these doesn't impact the value of the other.

For example in the update `a := b`, `b` is copied and assigned to `a`. This behavior is valid whatever the type of the variable.
The copy is a deep copy.

Why ? because as a function can be executed many time in a given transition, using reference instead of a copy of a variable make unpredictable results. Especially when update is make through a user function used in a guard or invariant (as they may be called many time given one transition).

The function that make the copy can be found at `Location.copy_discrete`.

### Assignment by reference

The only case where a variable is assigned by reference, is when we get the return value of some side effect functions like `stack_push`, `stack_clear`, `queue_push` and `queue_clear`. In this case, the reference returned is the reference to the element that is modified by the function.

For example:

```
(* here s1 get the ref of s2 *)
s1 := stack_push(0, s2);
(* We push an element to s2 *)
stack_push(1, s2);
(* After instruction above s1 is a ref to s2 s1 = [0, 1] ans s2 = [0, 1] *)
```

Why ?

The side effect builtin functions in IMITATOR aims to perform quick operations and avoid long copy. Moreover, a side effect function cannot be used in a guard / invariant, therefore it's safe to get a reference in the case of an update, because it will be perform just one time by transition.

The functions that return reference can be found at:

 - `DiscreteExpressionEvaluator.eval_stack_expression_with_context`
 - `DiscreteExpressionEvaluator.eval_queue_expression_with_context`
