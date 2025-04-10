(lang dune 3.16)

(name rustica)

(generate_opam_files true)

(source
 (github yanji-tech/rustica-compiler))

(authors "Yanji Tech")

(maintainers "Yanji Tech Team")

(license MulanPSL-2.0)

(documentation https://github.com/yanji-tech/rustica-compiler/blob/main/README.md)

(package
 (name rustica)
 (synopsis "Rustica Compiler")
 (description "\
The Rustica compiler is written in OCaml with Compiler-as-a-Server design \
for web computing (web IDEs, serverless, edge computing).")
 (depends ocaml dune)
 (tags (compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
