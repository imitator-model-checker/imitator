(lang dune 3.8)

(implicit_transitive_deps false)

(using menhir 2.1)

(name IMITATOR)

(version 3.4)

(generate_opam_files true)

(source
 (github imitator-model-checker/imitator))

(authors "Étienne André et al.")

(maintainers "Étienne André et al.")

(license "GPL-3.0-only")

(package
 (name IMITATOR)
 (synopsis
  "A parametric timed model checker performing synthesis of timing parameters for distributed real-time systems")
 (description
  "IMITATOR is a software tool for parametric verification and robustness analysis of real-time systems with parameters. It relies on the formalism of networks of parametric timed automata, augmented with rational-valued global variables, stopwatches, multi-rate clocks, and some other useful features.")
 (depends
  (alcotest :with-test)
  (fmt :with-test)
  (ocaml
   (>= 4.08))
  dune
  menhir
  mlgmp
  ppl
  extlib
  fileutils)
 (tags
  ("model-checking" "parametric timed automata")))
