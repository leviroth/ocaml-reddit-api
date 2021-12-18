(lang dune 2.8)

(generate_opam_files true)

(name reddit_api)

(version 0.1.1)

(source (github leviroth/ocaml-reddit-api))

(license MIT)

(authors "Levi Roth")

(maintainers levimroth@gmail.com)

(package
  (name     reddit_api_kernel)
  (synopsis "OCaml types for Reddit's API")
  (depends
    (odoc :with-doc)
    (cohttp      (>= 5.0.0))
    (core_kernel (>= v0.14.0))
    (ezjsonm     (>= 1.0))))

(package
  (name reddit_api_async)
  (synopsis "Async connection and utility functions for Reddit's API")
  (depends
    (odoc :with-doc)
    (expect_test_helpers_async (and (>= v0.14.0) :with-test))
    (async             (>= v0.14.0))
    (async_ssl         (>= v0.14.0))
    (cohttp-async      (>= 5.0.0))
    (core              (>= v0.14.0))
    (ezjsonm           (>= 1.0))
    (reddit_api_kernel (=  :version))
    (sequencer_table   (>= v0.14.0))))
