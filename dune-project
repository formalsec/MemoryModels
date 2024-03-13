(lang dune 3.2)

(name memory_models) ; 

(generate_opam_files true) 

(source
  (github formalsec/MemoryModels))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(package
  (name memory_models)
  (synopsis "A short synopsis")
  (description "A longer description")
  (depends 
    (ocaml (>= "4.14.0")) 
    dune
    encoding)
  (tags
    (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project