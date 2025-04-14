(lang dune 3.16)

(using menhir 3.0)

(name ssr_typechecker)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Lorenzo Pace")

(maintainers "Lorenzo Pace")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ssr_typechecker)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
