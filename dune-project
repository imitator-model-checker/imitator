(lang dune 3.6)

(name IMITATOR)

(generate_opam_files true)

(source
 (github imitator-model-checker/imitator))

(authors "\195\137tienne Andr\195\169 et al.")

(license "GPL-3.0")

(package
 (name IMITATOR)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml>=4.08 dune mlgmp ppl)
 (tags
  ("model-checking" "parametric timed automata")))
