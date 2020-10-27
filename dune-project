(lang dune 2.0)
(using menhir 2.0)

(generate_opam_files true)

(name iec_checker)
(version 0.0.3)
(authors "Georgy Komarov")
(license LGPL-3)
(source (github jubnzv/iec-checker))
(maintainers "Georgy Komarov <jubnzv@gmail.com>")

(package
  (name iec_checker)
  (synopsis "Static analysis of IEC 61131-3 programs"))
