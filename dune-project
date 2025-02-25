(lang dune 3.17)
(using menhir 3.0)

(name lang)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Sofia Rodrigues <sofia@algebraic.dev>")

(maintainers "Sofia Rodrigues <sofia@algebraic.dev>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lang)
 (synopsis "A compiler")
 (description "A compiler")
 (depends
   ocaml
   ppx_expect
   ppx_deriving
   sedlex
   ppx_expect
   menhirLib
   (alcotest :with-test))
 (tags ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
