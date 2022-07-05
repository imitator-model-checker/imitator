# Toolboxes

## ParsingStructureUtilities module

This module contains a lot of generic functions that allow you to easily traverse and fold the parsing structure tree and applying a given function to the leaves of the tree. Like `List` in OCaml, this module contains `exists`, `for_all`, `iterate` functions on all parsing structure types.

Note that a __leaf__ in parsing structure is either a variable, constant or a function call.

```ocaml
(* Leaf for parsing structure *)
type parsing_structure_leaf =
    | Leaf_variable of variable_name
    | Leaf_constant of DiscreteValue.discrete_value
    | Leaf_fun of variable_name
```



### Some examples

__Example 1__:
```ocaml
(* Check if leaf is a constant *)
let is_constant variable_infos = function
    | Leaf_variable variable_name -> is_constant_is_defined variable_infos variable_name
    | Leaf_constant _ -> true
    | Leaf_fun _ -> false
    
(* Check if a parsed boolean expression is constant *)
let is_parsed_boolean_expression_constant variable_infos =
    for_all_in_parsed_boolean_expression (is_constant variable_infos)
```

# Type inference

TODO

# Function signature resolver

TODO

# Graph dependency resolver

When the user making a model, he declares some automatons, global variables, user defined functions, local variables, etc. Each of them (called a "component" of the model) have relations. The only relation described is `use`. For example, `pta1` use `i`.

You can find a graph dependency resolver in the module `ParsedModelMetadata`. The main principle of the dependency resolver is to create a dependency graph of the components of the model. It gathers all global variables, user defined functions, local variables, etc. and link them together following some rules. 

## Graph dependency goal

Graph dependency is used to detect used / unused variables and functions of the model and found cycles between functions, etc.

Graph dependency can easily be traversed using recursive functions.

## Internal representation

Internally, the dependency graph is represented through a tuple `(component list * relation list)`. The first tuple element is the list of the components of the model. The second tuple element is the list of relations between these components.

The list of the relations is represented through a list of tuples, and as a relation is `relation = component * component`, so a list of relation is `(component * component) list`. A relation between two component `x` and `y` `('x', 'y')` can be read as `x` use `y`.

Finally, a component is a unique reference to some program component.

__An example of dependency graph for the following model:__

```IMITATOR
var

  s : int stack;
  used_1, used_2, used_3 : int;
  unused_1, unused_2 : int;
  used_in_unused_function : int;

(* used_function_1 used because not used by any pta *)
fn unused_function() : int
begin
  used_in_unused_function + 1
end

(* used_function_1 used because used pta1 *)
fn used_function_2() : int
begin
  used_function_1()
end

(* used_function_1 used because used by used_function_2 *)
fn used_function_1() : int
begin
  let x : int = 1 in
  let y : int = x + 2 in
  used_1 + x * used_2 + y
end

automaton pta1

  loc s0: invariant True
  when used_function_2() = 0 goto lend;

  accepting loc lend : invariant True

end

automaton pta2

  loc s0: invariant True
  when True
  do {
    seq
      (* used_3 used because in seq update (see rules) *)
      used_3 := stack_pop(s);
    then
      (* unused_1 not used because used only in not sequential update *)
      unused_1 := stack_top(s)
  }
  goto lend;

  accepting loc lend : invariant True

end


init := {
  discrete =
    loc[pta1]:=s0,
    loc[pta2]:=s0
  ;

}
```

![](images/dependency-graph-example.png)

__Note__:

IMITATOR have a function that return a string representation of the dependency graph in the [DOT format](https://graphviz.org/docs/layouts/dot/).

If you want to generate a visual view of graph dependency of the model, you can use these commands below:

- `imitator model.imi -verbose high | grep "digraph" > model.dot`
- Then, `dot model.dot > model.png`

It's possible that `dot` file contains some illegal characters. In this case you have to manually remove it before executing the second command. You need to have [DOT](https://graphviz.org/docs/layouts/dot/) installed on your computer.



## Relation creation rules

Below, the list of rules describing when a relation is created between two components according to their type.

__system -> automatons__

Each declared automatons are considered as used by the system. Relation between these two components are always created. There is only one system (it's the representation of the model).

__automaton -> global variable__

Create:
 - A global variable is used in a guard / invariant
 - A global variable is assigned in a sequential update

Don't create:
 - A global variable is assigned in a non sequential update

__automaton -> function__

Create:

 - A function is used in a guard / invariant
 - A function is used in an update

__variable -> variable__

 - A l-value variable is assigned by right member variables

### Between functions and local variables

Local variables are always considered as used. They are all reachable from system through their relation with the function (where they are declared).

## Unused components

We consider as __used__ all components reachable from `system` (`sys` as you can see on the picture above). In order to find unused components of the model, we just get the dependency graph of the model, and make difference between all declared components of the model and all components reachable from `system`. It means `all_components - used_components`. 

### Unused components remove rules

 - Unused global variables are removed automatically from the model if the option `-no-var-autoremove` is not set. 
 - Unused user defined functions are always removed from the model.
 - Unused function parameter are only reported as a warning.
 - There is no unused local variables possible (see rules), so they will never be removed.
  


# Updates

## Sequential and continuous updates

All updates are triggered from the `AlgoStateBased` module, by using the `DiscreteExpressionEvaluator` module.

There is two types of update:

 - Sequential update (Direct update)
 - Delayed update

Updates are makes at `AlgoStateBased.compute_new_location_guards_updates`.

### Sequential

Sequential

## Assignment mode

### Assignment by copy

In an update, in both sequential or not-sequential, variables are assigned by copy. It means that when we make an assignment of one variable `x` to another `y`, the value of `x` is copied to `y`. The two variables are independent. Therefore, modifying the value of one of these doesn't impact the value of the other.

For example in the update `a := b`, `b` is copied and assigned to `a`. This behavior is valid whatever the type of the variable.
The described copy is a deep copy.

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

The side effect builtin functions in IMITATOR aims to perform quick operations and avoid long copy. Moreover, a side effect function cannot be used in a guard / invariant, therefore it's safe to get a reference in the case of an update, because it will be performed just one time by transition.

The functions that return reference can be found at:

 - `DiscreteExpressionEvaluator.eval_stack_expression_with_context`
 - `DiscreteExpressionEvaluator.eval_queue_expression_with_context`

# Value packing

TODO
