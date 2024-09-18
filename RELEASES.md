# Releases

*******************************************************
## future release (202x-xx-xx) Cheese Durian

### Major features
* implement the integer hull [JLR15] to ensure termination of IM and EF over bounded parameter domains (work in progress)

*******************************************************
## release 3.4-alpha2 (2024-09-18) Cheese Durian

### Major features
* implement the integer hull [JLR15] to ensure termination of IM over bounded parameter domains (work in progress)

### New options
* Add a new option `-imi2imiprop` to regenerate the property to a file

### Syntax improvement
* Assignment can now be written using either `:=` (as before) or `<-` (new).

*******************************************************
## release 3.4-alpha (2024-03-21) Cheese Durian

### Major features
* Updates are now fully sequential in the model, including clock updates; **WARNING**: this might result in backward-incompatibility for elaborate models involving discrete and continuous updates in the same transition.
* New user-defined functions: imperative instructions, function calls, definition of local variables, variable shadowing…
* New AG ("global invariant") algorithm, implemented as the negation of EF
* New EU ("Exists … until") algorithm
* New EW ("exists … weak until") algorithm
* New AF ("always eventually") algorithm, and variants:
	- AR ("always … release")
	- AU ("always … until")
	- AW ("always … weak until")
	- EG ("exists globally"), implemented as the negation of AF
* New timed variants of EF, EU, AF, AR, AU, AW

### Syntax improvement
* The Boolean implication (`a => b`) is now allowed both in the model and in the property.

### Syntax changes in the model
* Actions declaration is now defined using the `actions` keyword instead of `synclabs`; backward-compatibility remains ensured
* Terminate backward-compatibility on updates syntax: updates are now only defined using the `var := expr` syntax
* Terminate backward-compatibility on the permissive `initially: ` declarations in automata (already not taken into account in the semantics)
* Both `;` and `,` instruction separators accepted

### Internal
* Source code entirely moved and restructured as a more standard tree

### Known issues
* Some divisions by zero (e.g., explicitly written `0 / 0` in the model) may not be detected; dynamic divisions by zero (`j := 2 / i`, with `i = 0`) are however in principle detected.



*******************************************************
## release 3.3 (2022-07-21) Cheese Caramel au beurre salé

### Syntax improvement
* New complex types for discrete global variables: lists, arrays, stacks, queues, and associated functions

### Major features
* New: extrapolation for parametric zones [[AA22](https://www.doi.org/10.1007/978-3-031-06773-0_24)]
* Refactoring of merging functions, with more efficient heuristics [[AMPP22](https://www.doi.org/10.1007/978-3-031-15839-1_12)]
* Exemplification algorithm: given a parametric timed specification, exhibit concrete parameter valuations, and concrete positive and negative runs, with a graphical representation of the evolution over time of the clock and discrete variables [[AWUH22](https://www.doi.org/10.1007/978-3-031-06773-0_25)] (slightly experimental)



*******************************************************
## release 3.2 (2021-11-03) Cheese Blueberries

### Syntax improvement
* New type: "binary words", of the form `0b00111`, for bitwise binary operations. Can be used in guards, invariants, updates. Example: `when logor(bw1, 0b1011) <> bw1 do {shift_left(bw1, 2)}`
* The syntax `var' := expr` in updates is discontinued. Official syntax remains `var := expr` (backward-compatibility for `var' = expr` remains ensured until further notice)

### Major features
* New accepting cycle synthesis with generalized condition. Syntax `property := #synth CycleThrough(condition_1, …, condition_n)` Each of the conditions must hold on at least one state of the same cycle, in order for this cycle to be accepting.




*******************************************************
## release 3.1 (2021-07-20) Cheese Artichoke

### Syntax improvement
* New types for discrete global variables: Boolean variables (`bool`), integer variables (`int`, over 32 bits)
* `discrete` become `rational` (still encoded using exact arithmetic, as opposed to the new `int` type)
* New power operator `pow(v, i)` over discrete variables, with `v` an integer or rational expression, and `i` an integer expression (possibly involving integer variables)

### Result
* IMITATOR now attempts to generate a result (`.res`) file even in the case of an error such as parsing error of the model or of the property

### Syntax changes in the model
* New syntax for the initial state definition, with a separation between the discrete and the continuous parts (the former syntax remains accepted for backward-compatibility, but a warning is triggered).

### Syntax changes in the property
* `property := #synth AcceptingCycle` now allowed as a shortcut for `property := #synth CycleThrough(accepting)`

### Export
* New translation to the [JANI](https://jani-spec.org/) interchange format

### Benchmarks
* Entire refactoring of the benchmarks library [[AMP21]](https://www.doi.org/10.1007/978-3-030-79379-1_3)

### Bug fixing:
* Fixed an issue met in v3.0 when a division by 0 is encountered.
* Solved a bug (introduced in v3.0) related to variable automatic removal in the initial state definition
* Solved a bug that crashed the tool when an included file was not found


*******************************************************
## release 3 (2021-01-12) Cheese
MAJOR RELEASE

### Calling paradigm
The calling paradigm switches from a single file with numerous options (some of them compulsory) to *two* files and a few options (none of them compulsory):
* a model file, containing the model
* a property file, containing the property (and optionally the projection onto some parameters)
The model syntax is almost entirely backward-compatible with (at least) the IMITATOR 2.10.1 syntax.
The property syntax is entirely new.

### Major features
* New variants of the NDFS-based accepting cycle synthesis with several exploration strategies [[AAPP21]](https://www.doi.org/10.1007/978-3-030-72016-2_17)

### Syntax improvement
* IMITATOR now supports multi-rate clocks with a syntax of the form `flow {x' = 1, y' = 2.5, z' = -3/5}`

### Syntax changes in the model
* Terminate support for HyTech-style comments (`-- comment`) and C-style comments (`/* comment */`)
* Terminate support for some HyTech-style optional syntax (regions declaration…)
* Many backward-compatible useless syntactic features now raise warnings (e.g., using `while` instead of `invariant`, using `Wait {}`…)

### Options
* Most options changed since version 2.x
* Some options were discarded, and are now part of the property syntax
* Result is always output to a file by default; use `-no-output-result` to disable

### Export
* By default, all files are now exported in the current directory instead of the model directory. Option `-output-prefix` can still be used to change the directory.

### Versioning
* The build number is discontinued (and replaced by the sole short Git hash)

### Benchmarks
* Created a new `Unsolvable` section in the `benchmarks` repository, containing very simple models for which IMITATOR cannot (yet) provide a result.



*******************************************************
## release 2.12.1 (2019-10-01)
minor release

### Bug fixing:
* Fix bugs for option `-counterexample`
* Fix issues in NDFS-based accepting cycle synthesis [[NPP18]](https://www.doi.org/10.1109/ICECCS2018.2018.00009), and options names


*******************************************************
## release 2.12 (2019-08-22) Butter Lobster
MAJOR RELEASE

### Bug fixing:
* Fixed a "+ 0" that appeared in some of the text and graphical outputs

### Major features
* New accepting cycle synthesis (`-mode AccLoopSynth`)
* New NDFS-based accepting cycle synthesis (`-mode AccLoopSynthNDFS`) with several exploration strategies [[NPP18]](https://www.doi.org/10.1109/ICECCS2018.2018.00009). Additional option `-counterexample` terminates the analysis as soon as one cycle is found.
* In mode `EF -counterexample`, IMITATOR outputs an example of parameter valuation, and a full concrete run, with a graphical representation of the clocks and discrete variables evolution. An absolute time clock (initially set to 0 and never reset) named `global_time` needs to be defined in the model. [EXPERIMENTAL]
* New check-syntax mode (`-mode checksyntax`), that simply checks the syntax and terminates without doing any analysis.

### Minor features
* Removed the creation and deletion of an (unused) script when generating graphics

### Syntax improvement
* `if`-`then`-`else` conditions are now allowed in updates. Example syntax: `when x = 2 sync a do {if l <= 10 then x := 0, l := 0 else (i := 2) end} goto l1;`
* Partial model inclusion is now possible thanks to keyword `#include`. Notably, properties can now be included from other files.

### Options
* Removed the `-merge-before` option (that was not documented nor properly tested)

### Export
* The result in the terminal tries to be more precise about what the synthesized constraints actually guarantees
* Added a (tentative) translation to Uppaal (option `-PTA2Uppaal`); see limitations in the user manual
* In the graphical state space and states description, the projection onto a selected set of parameters is added if the model contains `projectresult(…)`
* In the text files, state and transition descriptions are now ordered by increasing order
* The cartography and tile graphics are now larger (1024x1024), and the margins are a bit reduced

### Internal
* Changed the internal representation of the state space: (state, action, state) is now (state, edge, state)


*******************************************************
## release 2.11 (2019-03-01) Butter Kouign-aman
MAJOR RELEASE

### Major features
* New algorithm for optimal reachability with priority queue (queue-based, with priority to the earliest successor for the selection of the next state)
* Removed the `Gc.major()` instruction, that used to require a huge time for large models, while not bringing any benefit for smaller models

### Minor features
* Using the syntax `initially <loc name>` in the beginning of a PTA (which is not taken into account) now raises a warning.
* In translation mode, some metrics (numbers of automata, actions, variables, locations…) are printed on the terminal.
* New option to avoid the inclusion check in EFsynth: useful when very large parameter constraints are manipulated

### Bug fixing:
* Correct errors in displaying PTA (graphics in PDF, PNG, etc.): arithmetic expressions on discrete variables did not print correctly
* Fix a bug (or an unclarity) in the options for trace set generation

### Syntax improvement
* The prime (`'`) in transition updates becomes optional; the `=` in transition updates becomes `:=` (backward-compatibility remains ensured until further notices) That is, an update `x' = 0` becomes `x := 0`
* `while [invariant condition] wait{}` becomes `invariant [invariant condition]` (backward-compatibility remains ensured until further notices)

### Export
* The graphical state space now comes in .pdf instead of .jpg for better readability
* Some metrics are printed in the PTA graphical exports (PTA2PDF, PTA2PNG, PTA2JPG)
* The trace set now depicts the automaton name in addition to the location name (for example: `automaton: location` instead of `location`)


*******************************************************
## release 2.10.4 (2018-07-02)
minor release

### Bug fixing:
* Fix a bug (or an unclarity) in the options for trace set generation

### Major feature
* Removed the `Gc.major()` instruction, that used to require a huge time for large models, while not bringing any benefit for smaller models

### Export
* The graphical state space now comes in .pdf instead of .jpg for better readability


*******************************************************
## release 2.10.3 (2018-05-22)
minor release

### Bug fixing:
* Correct errors in displaying PTA (graphics in PDF, PNG, etc.): arithmetic expressions on discrete variables did not print correctly


*******************************************************
## release 2.10.2 (2018-04-06)
minor release

### Minor features:
* New option to avoid the inclusion check in EFsynth: useful when very large parameter constraints are manipulated


*******************************************************
## release 2.10.1 (2018-02-23)
minor release

### Syntax improvement
* The prime (`'`) in transition updates becomes optional; the `=` in transition updates becomes `:=` (backward-compatibility remains ensured until further notices) That is, an update `x' = 0` becomes `x := 0`
* `while [invariant condition] wait{}` becomes `invariant [invariant condition]` (backward-compatibility remains ensured until further notices)


*******************************************************
## release 2.10 (2018-02-21) Butter Jellyfish
MAJOR RELEASE

### Major features:
* New algorithms EFmin and EFmax synthesizing the min (resp. max) valuation of a single given parameter for which a given state is reachable
* Arithmetic expressions are now supported in discrete updates; this includes in particular the use of divisions (clock updates are not impacted, and remain linear only)
* Distributed version of the non-Zeno cycle emptiness checking [still in an experimental phase]
* The GrML syntax (in and out) is now discontinued, as CosyVerif is not maintained anymore, and the next version may not support GrML.

### Minor features:
* New option to convert exact-valued discrete variables into (possibly approximated) floats in all outputs
* Reintroduced the `-counterexample` option (stops the analysis as soon as a counterexample is found) for EF and PRP
* New options `-PTA2PNG` and `-PTA2PDF`, to convert the model as a graphics in PNG and PDF formats, respectively
* New verbose mode `experiments`, displaying slightly more information than the standard verbosity
* New option `-romeo` (April 1st 2017 feature)… Try it :wink: :fish:

### Display:
* Simplified the terminal output by IMITATOR by removing most technical (states, memory, statistics) information. The former information can be retrieved using the new mode `-verbose experiments`.

### Bug fixing
* Fixed a bug in the parsing module that crashed IMITATOR when a discrete was assigned to an undeclared variable

### Internal
* Better separation between the discrete variables and other variables in guards, so as to first check whether the discrete part is satisfiable (which is cheap) before checking the continuous part (which is expensive)


*******************************************************
## release 2.9.1 (2017-04-01)
MINOR RELEASE

### Major feature:
* The GrML syntax (in and out) is now discontinued, as CosyVerif is not maintained anymore, and the next version will not support GrML.

### Minor feature:
* New option `-romeo` (April 1st 2017 feature)… Try it :wink: :fish:


*******************************************************
## release 2.9 (2017-03-23) Butter Incaberry
MAJOR RELEASE

### Major
* New algorithm LoopSynthesis (option `-mode LoopSynth`): synthesizes valuations for which there exists an infinite run in the model.
* New algorithms for non-Zeno cycle emptiness checking: with CUB-detection (`-mode NZCUBcheck`) or transformation to a CUB-PTA (`-mode NZCUBtrans`) [still in an experimental phase]
* EFsynth rewritten to output a single, disjunctive constraint (instead of a list of constraints as until version 2.8); Legacy version still usable using option `-mode EFold`
* PRP and PRPC now disconnected from IM and BC: new syntax (`-mode PRP` and `-mode PRPC`), and result in the form of a single good and/or bad constraint.

### Minor features
* Variables not used in a model apart from resets (or updates) are simply removed entirely, so as to speed up the execution. If that behavior is not the one intended by the user, this automatic removal can be disabled using the new option `-no-var-autoremove`.
* (Limited) colored terminal output :-)

### Bug fixing
* Solved a bug for EF / PRP when the initial state is already bad
* Solved a bug in the equality and inclusion check of non-convex parameter constraints (wrong PPL function used)

### Export
* PDFC now outputs both an under-approximated good constraint and an under-approximated bad constraint (instead of an over-approximated good constraint for the latter); so presentation of the result changes compared to version 2.8
* Field `State space nature` becomes `Constraint nature` in result (`.res`) files
* Add L/U status to result (`.res`) files
* The file name for the graphical cartography becomes `model_cart.png` for any algorithm (instead of `model_cart_ef.png`, `model_cart_im.png` and so on).

### Install and compile
* To compile, now use `build.sh` to compile IMITATOR (in non-distributed mode) and `build-patator.sh` to compile PaTATOR (distributed mode)

### Internal
* In cartography algorithms, the list of tiles is now externalized to a dedicated manager (that can store a list of tiles, or a single good/bad constraints)
* New experimental version of PRPC using an external compositional verifier; hidden option `-mode coverlearning`


*******************************************************
## release 2.8 (2016-08-13) Butter Ham
MAJOR RELEASE

### Major
* New algorithm: parametric deadlock-freeness checking
* Allow for more complex unreachable states in property definition: several locations in several IPTAs can be tested, and discrete variables can be compared with `<`, `<=`, `=`, `>=`, `>` and intervals; all this can be mixed using and/or operators.
* Automatic detection of L/U-PTAs, U-PTAs and L-PTAs
* Standardized output for the results of IMITATOR (including a notion of validity of the constraint output)

### Bug fixing
* Corrected a problem in the generation of the observer patterns
* Solved an inconsistency in the property syntax of the `always sequence` observer pattern
* Remove three observer patterns that were allowed in the input syntax but not implemented
* Correct the depth-limit option behavior (that used to compute one step further than requested)
* All error messages now displayed on stderr (instead of stdout)

### Options
* Added an option `-output-trace-set-verbose` to print trace set with all information contained in the `.states` file (i.e., the constraint and the constraint projected onto the parameters)
* New options `-cart-tiles-limit` and `-cart-time-limit` that operate on the cartography algorithms

### Graphics
* Added a visualization of the bad states on the trace set (using red color + a frowney)

### Misc
* Memory information now displayed at the end of the execution even in `-verbose standard`
* Memory information now added to the `.res` files

### Export
* Result file (`.res`) much more nicely structured. Statistics better integrated both in the terminal output and in the `.res` files
* Added an export to HyTech (option `-PTA2HyTech`)
* Removed the automatically generated observer from the PTA export (option `-PTA2JPG`)
* When exporting to jpg (option `-PTA2JPG`), the urgent locations are now colored in yellow, and the nosync action (no synchronization name) are now dashed

### Versioning
* Now IMITATOR integrates the hash number of GitHub in all outputs

### Install and compile
* Using `strip` the binary is now (much) smaller

### Internal
* Entire reorganization of the core of IMITATOR: all algorithms rewritten in an object-oriented fashion
* Added a mode to regenerate the IMITATOR input file (helpful to test input/output models)
* `Automaton.ml` becomes `Location.ml` (and new module `Automaton.ml` created); and reorganization of numerous other modules
* Build number only incremented when a flag file `iamadeveloper` is present in the imitator root directory
* `examples` directory becomes `benchmarks`
* New experimental version of EFsynth (usable with a hidden option)
* Non-regression tests now implemented in an external Python script
* Comparator of efficiency with former versions now implemented in an external Python script


*******************************************************
## release 2.7.3 (2015-10-28)
INTERMEDIATE RELEASE

### Bug fixing
* Corrected a problem in the generation of the observer patterns

### Internal
* `examples` directory becomes `benchmarks`
* `Automaton.ml` becomes `Location.ml` (and new module `Automaton.ml` created)


*******************************************************
## release 2.7.2 (2015-09-30)
INTERMEDIATE RELEASE

### Major
* Allow for more complex unreachable states in property definition: several locations in several IPTA can be tested, and discrete variables can be compared with `<`, `<=`, `=`, `>=`, `>` and intervals; all this can be mixed using and/or operators.
* Detection of L/U-PTA, U-PTA and L-PTA

### Bug fixing
* Solved an inconsistency in the property syntax of the `always sequence` observer pattern
* Remove three observer patterns that were allowed in the input syntax but not implemented

### Options
* Added an option `-output-trace-set-verbose` to print trace set with all information contained in the `.states` file (i.e., the constraint and the constraint projected onto the parameters)

### Graphics
* Added a visualization of the bad states on the trace set (using red color + a frowney)
* Removed the automatically generated observer from the PTA export


*******************************************************
## release 2.7 (2015-07-22) Butter Guéméné

### Major algorithmic features since 2.6.1
* Added EF-synthesis (option `-mode EF`): synthesize all parameter valuations such that a given state is reachable [[AHV93]](https://www.doi.org/10.1145/167088.167242) [[JLR15]](https://www.doi.org/10.1109/TSE.2014.2357445)
* Added PRP and PRPC algorithms (option `-PRP`) [[ALNS15]](https://www.doi.org/10.1007/978-3-319-17524-9_5)
* Added a distributed version of the behavioral cartography (option `-distributed`), with several distribution schemes:
	* static [[ACN15]](https://www.doi.org/10.1007/978-3-319-25423-4_21)
	* sequential [[ACE14]](https://www.doi.org/10.1145/2642769.2642784)
	* random [[ACE14]](https://www.doi.org/10.1145/2642769.2642784)
	* shuffle [[ACN15]](https://www.doi.org/10.1007/978-3-319-25423-4_21)
	* dynamic [[ACN15]](https://www.doi.org/10.1007/978-3-319-25423-4_21)
	* unsupervised / unsupervised multi-threaded (unpublished, EXPERIMENTAL)

### Syntax improvement
* Urgent locations now handled natively (keyword: `urgent loc`)
* Constant now supported in the input model

### Options
* Add two new verbose modes: `mute` and `warning-only`
* Add export to LaTeX/TikZ
* The result (text) can be exported to a file (option `-output-result`)
* Time elapsing can be applied either at the beginning (new mode, option `-no-time-elapsing`) or at the end (classical IMITATOR semantics) of the computation of a new state
* New options to specify min/max values for x/y axes for the graphical output of the cartography
* Removed `-counterex` option (stops the analysis as soon as a counterexample is found)

### Changes in options syntax
* mode `reachability` becomes mode `statespace`
* `-cart` becomes `-output-cart`
* `-fancy` becomes true by default; to disable it, use `-output-trace-set-nodetails`
* `-log-prefix` becomes `-output-prefix`
* `-with-dot` becomes `-output-trace-set`
* `-with-graphics-source` becomes `-output-graphics-source`
* `-with-log` becomes `-output-states`
* `-with-parametric-log` is removed (becomes always true when option `-output-states` is enabled)

### Interfacing with CosyVerif (and GrML support)
* Add EF mode support to the CosyVerif interface (using options `-fromGrML` and `-cosyProp`)
* Fix a bug in the GrML output (action labels were removed when exporting to GrML)
* Add the initial constraint to both the GrML input and the GrML output

### Misc
* Add suffix `-pta.jpg` to PTA2JPG files
* In BC mode: try to reduce the number of constraint (inclusion test in both directions)
* Warning displayed if parameters are not constrained to be >= 0 in the model
* Improve printing floating numbers (in time and memory statistics)

### Versioning
* Now IMITATOR has a build number (since May 2013)
* Now IMITATOR has a version name and a version logo (based on traditional Breton "galettes" ingredients): 2.7 is "Butter Guéméné"

### Install and compile
* Main binary now in lower case (`imitator`)
* Two versions:
	* non-distributed (`sh compile.sh`), that creates a static binary `imitator`, or
	* distributed (`sh compile-distr.sh`), that creates a dynamic binary `patator`

### Internal
* Moved to OCaml 4.01.0
* Replaced `Makefile` with `_oasis` 0.3
* Refactored constraints
* Renamed `Graph.ml` with `StateSpace.ml`
* Much refactoring of the code, in particular in `Cartography.ml`



*******************************************************
## release 2.6.2 (2014-05-06)
PRIVATE RELEASE

### Major features:
* Added EF-synthesis (option `-EF`): synthesize all parameter valuations such that a given state is reachable
* Added a distributed version of IMITATOR

### Minor features:

### Changes in options syntax:
* `reachability` becomes `statespace`

### Misc
* Improve printing floating numbers (in time and memory statistics)
* Now IMITATOR has a build number

### Internal
* replaced `Makefile` with `_oasis`
* Refactored constraints
* Renamed `Graph.ml` with `StateSpace.ml`
* Much refactoring of the code, in particular in `Cartography.ml`


*******************************************************
## release 2.6.1.1 (2013-08-02)
PRIVATE RELEASE

Nothing really new, just a backup before refactoring constraints


*******************************************************
## release 2.6.1 (2013-05-01)

### Major features:
* dynamic clock elimination now implemented (option `-dynamic`)

### Minor features:
* tile nature now written in the source file for plot in border cartography (available with option `-with-graphics-source`)
* GrML input / output is no longer "experimental", and now fully integrated
* graphic cartography is now `.png` instead of `.ps`

### Bug correction:
* Correction of the memory used in 64 bits (before, gave the same number as in 32 bits)


*******************************************************
## release 2.6.0 (2013-02-25)

### Major features:
* Added observer patterns
* Changed and improved bad state definition (that was not really used anyway)
* The random cartography is temporarily suspended

### Minor features:
* Cartography can color in green and red according to the bad state definition

### Ongoing work:
* Added a new cartography mode with border detection (ongoing work)
* Added dynamic clock elimination (ongoing work)
* Added branch and bound (ONGOING WORK)

### Syntax in options:
* `-IMorig` becomes `-IMK`
* `-debug` becomes `-verbose`
* `-with-dot-source` becomes `-with-graphics-source`
* Merging option becomes (again?) `-merge`
* Added `-bab` option (for branch and bound) [work in progress]
* Added `-counterex` option (stops the analysis as soon as a counterexample is found) [work in progress]

### Syntax in model:
* region declaration not compulsory anymore (that was only used for Hytech backward compatibility)
* bad state definition added
* Not everything allowed anymore at the end of the file! (before, everything was allowed for backward compatibility with HyTech) Now need to add (optional) keyword `end` first

### Misc
* output files for model `XXXX.imi` are now `XXXX.ext` instead of `XXXX.imi.ext`

### Internal
* Refactored BC code
* The bounds for V0 are now NumConst (not "int" anymore)


*******************************************************
## release 2.5.0 (2012-08-21)

* Added arbitrary clock updates (not only to 0).
* Partial code refactoring.
* Options: Merging is still not enabled by default; inverted options `-no-log` and `-no-log` to `-with-dot` and `-with-log`. `-post-limit` becomes `-depth-limit`.
* Added experimental and ongoing features (import from and export to GML, depth first exploration).

*******************************************************
## release 2.4 (2012-02-20)

* Added support for stopwatches [[CL00]](https://doi.org/10.1007/3-540-44618-4_12) with new keyword `stop`. Syntax, semantics and computation time are backward-compatible for models without stopwatches.

*******************************************************
## release 2.375 (2012-02-20)
PRIVATE RELEASE

* Added merging of states [[AFS12]](https://doi.org/10.1007/978-3-642-28891-3_10), several optimizations; added and changed input options

*******************************************************
## release 2.36 (2011-11-20)
PRIVATE RELEASE

* Fully removed `X'` and `d` variables: large gain in both memory and time

*******************************************************
## release 2.35 (2011-11-17)
PRIVATE RELEASE

* Starting removing `X` and `d` variables
* Addition of an optional optimization of the inverse method algorithm
* Several optimizations to speed up post image computation, including a simple but efficient one for programs with discrete variables

*******************************************************
## release 2.3 (2011-06-06)

* Addition of variants of the inverse method algorithm
* Addition of an optional optimization of the inverse method algorithm
* Several optimizations to speed up post image computation

*******************************************************
## release 2.2 (2010-07-30)

* Several optimizations to speed up post image computation
* More detailed presentation of reachability graph
* Graphical output of behavioral cartography
* Refactoring of source code to improve maintainability

*******************************************************
## release 2.1 (2010-05-31)

* Replaced APRON and POLKA libraries by PPL (Parma Polyhedra Library)

*******************************************************
## release 2.0 (2010-05-17)

* Tool completely rewritten in OCaml
* Polyhedra handled thanks to APRON

*******************************************************
## release 1.0 (2008)

First version (in Python)
*******************************************************
