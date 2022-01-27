(define-module (gn packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io))

(define-public rust-handlegraph-0.7
  (package
    (inherit rust-handlegraph-0.3)
    (name "rust-handlegraph")
    (version "0.7.0-alpha.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "handlegraph" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1frlcdwhycjvizb0gfb0v36vxjdi0jxagl2l2v6dzdjxpaawv9rs"))))
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-boomphf" ,rust-boomphf-0.5)
        ("rust-bstr" ,rust-bstr-0.2)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-gfa" ,rust-gfa-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-succinct" ,rust-succinct-0.5))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.9)
        ("rust-rand" ,rust-rand-0.7))))))

(define-public rust-clap-for-jrep
  (package
    (name "rust-clap")
    (version "2.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nf6ld3bims1n5vfzhkvcb55pdzh04bbhzf8nil5vvw05nxzarsh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-clap-derive" ,rust-clap-derive-3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-os-str-bytes" ,rust-os-str-bytes-2)
        ("rust-strsim" ,rust-strsim-0.10)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-ansi-term" ,rust-ansi-term-0.11)
        ("rust-terminal-size" ,rust-terminal-size-0.1)
        ("rust-textwrap" ,rust-textwrap-0.12)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-vec-map" ,rust-vec-map-0.8)
        ("rust-yaml-rust" ,rust-yaml-rust-0.4))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-version-sync" ,rust-version-sync-0.8))))
    (home-page "https://clap.rs/")
    (synopsis "Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured
Command Line Argument Parser.")
    (license (list license:expat license:asl2.0))))

(define-public jrep
  (package
    (name "jrep")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joshua-laughner/jrep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0syvlc93w26v856hp5l8ik615dfrvax6hdfzw5kqhaww3siqjaj9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-clap" ,rust-clap-for-jrep)
        ("rust-exitcode" ,rust-exitcode-1)
        ("rust-term" ,rust-term-0.7)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/joshua-laughner/jrep/")
    (synopsis "grep for Jupyter notebooks")
    (description
"@code{jrep} is @code{grep} for Jupyter notebooks.  It is a command line
program that can search across multiple notebooks for specific text,
but limit itself to certain types of cells, source text, output data,
or any combination.")
    (license license:gpl3+)))

(define-public notebook-tools
  (let ((commit "a9db1f4f90f6df72d28bf1235ca16b988d7b86be")
        (revision "0"))
    (package
      (name "notebook-tools")
      (version commit)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/CADLabs/notebook-tools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0mmvqjfcsa6fq12rpay9w6ra1q8ijhmm1raqzi4d70y7wsbd20lw"))))
      (build-system cargo-build-system)
      (arguments
       `(#:cargo-inputs
         (("rust-clap" ,rust-clap-3)
          ("rust-exitcode" ,rust-exitcode-1)
          ("rust-term" ,rust-term-0.7)
          ("rust-regex" ,rust-regex-1)
          ("rust-serde" ,rust-serde-1)
          ("rust-serde-json" ,rust-serde-json-1))))
      (home-page "https://github.com/CADLabs/notebook-tools")
      (synopsis "Rust CLI tools for manipulation of Jupyter Notebooks")
      (description "Rust CLI tools for manipulation of Jupyter Notebooks.")
      (license #f)))) ; There is no license.

