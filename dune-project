(lang dune 2.0)
(using menhir 2.0)

(generate_opam_files true)

(name iec_checker)
(version 0.0.4)
(authors "Georgiy Komarov")
(license  LGPL-3.0-or-later)
(source (github jubnzv/iec-checker))
(maintainers "Georgiy Komarov <jubnzv@gmail.com>")

(package
  (name iec_checker)
  (synopsis "Static analysis of IEC 61131-3 programs")
  (depends
        (ocaml (>= 4.08))
        (core :build)
        (clap :build)
        (menhir (and :build (= 20210929)))
        (menhirLib (and :build (= 20210929)))
        (ppx_deriving :build)
        (ppx_deriving_yojson :build)
        (ppx_fields_conv :build)
        (ppx_jane :build)
        (ppx_variants_conv :build)
        (ppxlib :build)
        (re :build)
        (xmlm :build)
        (yojson :build)))
