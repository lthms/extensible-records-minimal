(lang dune 3.0)

(generate_opam_files true)

(license mpl-2.0)

(authors "Thomas Letan <lthms@soap.coffee>")

(maintainers "Thomas Letan <lthms@soap.coffee>")

(version 1)

(package
 (name extensible-records-minimal)
 (synopsis
  "Minimal example of OCaml extensible records implemented with dmap")
 (depends dmap))