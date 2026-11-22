(lang dune 3.24)

(name recipebook)

(generate_opam_files true)

(source
 (github mindi888/CS3110-Project))

(authors "Alicia He" "Claire Yan" "Daisy Yin"  "Jennifer Wu" "Mindi Hu" )

(license LICENSE)

(package
 (name recipebook)
 (synopsis "A personalized recipe book that recommends recipes from a few questions")
 (depends
  ocaml
  dune
  yojson
  (ounit2 :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
