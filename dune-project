(lang dune 2.5)
(name reddit_api)
(version 0.1.0)

(generate_opam_files true)

(source (github leviroth/reddit_api))
(license MIT)
(authors "Levi Roth")
(maintainers "levimroth@gmail.com")

(package
 (name reddit_api)
 (synopsis "An OCaml wrapper for Reddit's API")
 (depends
  (dune (and (>= 2.0) :build))
  (async (= v0.14.0))
  (async_ssl (= v0.14.0))
  (cohttp-async (>= 2.5.4))
  (core (= v0.14.0))
  (angstrom (>= v0.14.0))
  (faraday (>= v0.7.0))))
