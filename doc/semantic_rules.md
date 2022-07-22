# Init

## Discrete init

```ocaml
discrete =
    (* Allowed *)
    i := 1 + 5 + k * 2, (* with k a constant *)
    b := k > 5,
    loc[pta] := l1,
    
    (* Not allowed *)
    x := 0, (* with x a clock *)
    p := 0, (* with p a parameter *)
    i2 := i * 2, (* with j a variable *)
    a := 0, a := 0 (* several init *)
;
```

Only init of location or discrete variable with constant expression is allowed.

 - Init a constant is forbidden
 - Init a variable with a non-constant expression is forbidden
 - Init a clock or parameter is forbidden
 - Init a variable several times is forbidden
 - Init automaton at two different locations is forbidden

## Continuous init