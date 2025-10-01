(lang dune 3.6)

(name IMITATOR)

(version 3.4)

(generate_opam_files true)

(source
 (github imitator-model-checker/imitator))

(authors "\195\137tienne Andr\195\169 et al.")

(maintainers "\195\137tienne Andr\195\169 et al.")

(license "GPL-3.0")

(package
 (name IMITATOR)
 (synopsis "A parametric timed model checker performing synthesis of timing parameters for distributed real-time systems")
 (description "IMITATOR is a software tool for parametric verification and robustness analysis of real-time systems with parameters. It relies on the formalism of networks of parametric timed automata, augmented with rational-valued global variables, stopwatches, multi-rate clocks, and some other useful features.")
 (depends
  (alcotest :with-test)
  (ocaml
   (>= 4.08))
  dune
  oasis
  mlgmp
  ppl
  extlib
  fileutils)
 (tags
  ("model-checking" "parametric timed automata")))
