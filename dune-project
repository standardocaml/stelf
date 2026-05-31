(lang dune 3.20)

(name stelf)

(version 1.0.1)

(generate_opam_files true)

(source
 (github standardocaml/stelf))

(authors "Asher Frost <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(pin
 (url "file://basis")
 (package
  (name basis)))

(package
 (allow_empty)
 (name stelf)
 (synopsis "A port of Twelf to OCaml")
 (description
  "STELF (System for the Typed Edinburgh Logical Framework) is a port of Twelf to OCaml. It provides a framework for defining and reasoning about formal systems, such as programming languages and logics.")
 (depends
  (ocaml
   (>= 5.0.0))
  basis
  lwt
  containers
  base
  angstrom
  (alcotest :with-test)
  (alcotest-lwt :with-test)
  (bisect-ppx :with-dev)
  (odoc :with-doc)
  lambda-term
  fmt
  (logs
   (>= 3.0.0)))
 (tags
  ("logic" "language" stelf)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
