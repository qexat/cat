(lang dune 3.16)

(name cat)

(generate_opam_files true)

(source
 (github qexat/cat))

(authors "Qexat")

(license LICENSE)

(documentation https://github.com/qexat/cat/)

(package
 (name cat)
 (synopsis "Category theory in OCaml")
 (description "A library defining the category theory concepts in OCaml.")
 (depends ocaml dune))
