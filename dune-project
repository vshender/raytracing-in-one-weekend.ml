(lang dune 3.4)

(name raytracing-in-one-weekend)

(source
  (github vshender/raytracing-in-one-weekend.ml))
(homepage "https://github.com/vshender/raytracing-in-one-weekend.ml")

(authors "Vadzim Shender <vadzim@shender.org>")
(maintainers "Vadzim Shender <vadzim@shender.org>")

(license "ISC")

(generate_opam_files false)

(package
 (name raytracing-in-one-weekend)
 (synopsis "Ray Tracing in One Weekend implemented in OCaml")
 (description "A Ray Tracer based on Peter Shirley's 'Ray Tracing in One Weekend' book")
 (depends
  (ocaml (>= 4.13.0))
  (alcotest :with-test)))
