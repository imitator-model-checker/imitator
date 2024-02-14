# Templates documentation

## Design decisions
    - By default, every action declared in a template that is not part of the parameters is not shared between instiations. To share an action you have to instantiate multiple times with the same name in the argument.
