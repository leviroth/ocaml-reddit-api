(lang dune 2.9)

(generate_opam_files true)

(name reddit_api)

(version 0.2.1)

(source (github leviroth/ocaml-reddit-api))

(license MIT)

(authors "Levi Roth")

(maintainers levimroth@gmail.com)

(package
  (name     reddit_api_kernel)
  (synopsis "OCaml types for Reddit's API")
  (depends
    (cohttp      (>= 5.0.0))
    (core_kernel (>= v0.15.0))
    (core        (>= v0.15.0))
    (expect_test_helpers_async (and (>= v0.15.0) :with-test))
    (jsonaf (>= v0.15.0))
    (odoc :with-doc)
    (ppx_jane (>= v0.15.0))))

(package
  (name reddit_api_async)
  (synopsis "Async connection and utility functions for Reddit's API")
  (depends
    (async        (>= v0.15.0))
    (async_ssl    (>= v0.15.0))
    (cohttp-async (>= 5.0.0))
    (core         (>= v0.15.0))
    (expect_test_helpers_async (and (>= v0.15.0) :with-test))
    (jsonaf            (>= v0.15.0))
    (ppx_jane          (>= v0.15.0))
    (reddit_api_kernel (=  :version))
    (sequencer_table   (>= v0.15.0))))
