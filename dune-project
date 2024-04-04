(lang dune 3.14)

(name gofer_compiler)

(generate_opam_files true)

(source
 (github DisownedWheat/GoferCompiler))

(authors "Rob Larnach")

(maintainers "Rob Larnach")

(license MIT)

(documentation https://github.com/DisownedWheat/GoferCompiler)

(package
 (name gofer_compiler)
 (synopsis "Compiler for the Gofer language that targets Golang")
 (description "Compiler for the Gofer language that targets Golang")
 (depends ocaml dune)
 (tags
  (topics compiler gofer golang)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
