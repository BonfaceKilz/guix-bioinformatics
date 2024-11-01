(define-module (gn packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python))

(define-public rust-assert-cmd-0.12
  (package
    (name "rust-assert-cmd")
    (version "0.12.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "assert-cmd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xfn2spazxk3ljj9q3250a24gndja9vwa0h0rnbccdrbd4ncyvwk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-escargot" ,rust-escargot-0.5)
        ("rust-predicates" ,rust-predicates-1)
        ("rust-predicates-core" ,rust-predicates-core-1)
        ("rust-predicates-tree" ,rust-predicates-tree-1)
        ("rust-wait-timeout" ,rust-wait-timeout-0.2))))
    (home-page "https://github.com/assert-rs/assert_cmd")
    (synopsis "Test CLI Applications.")
    (description "Test CLI Applications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bgzip-0.2
  (package
    (name "rust-bgzip")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bgzip" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kssq4hp8csg27rhggabpfiyn9xp5rh5b8al63dghk11vqs7hk5j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-flate2" ,rust-flate2-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-csv" ,rust-csv-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/informationsea/bgzip-rs")
    (synopsis "Rust implementation of bgzip")
    (description "Rust implementation of bgzip")
    (license license:expat)))

(define-public rust-boomphf-0.5
  (package
    (name "rust-boomphf")
    (version "0.5.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "boomphf" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0braniw72g9yq5006sfgc1g8d4317bb524c694jw6nggizrvg3sf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-log" ,rust-log-0.4)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-wyhash" ,rust-wyhash-0.5))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-quickcheck" ,rust-quickcheck-1))))
    (home-page "https://github.com/10XGenomics/rust-boomphf")
    (synopsis "Scalable and Efficient Minimal Perfect Hash Functions")
    (description "This package provides a Rust implementation of
@url{https://arxiv.org/abs/1702.03154, fast and scalable minimal perfect hashing
for massive key sets}.  It generates an @acronym{MPHF, minimal perfect hash
functions} for a collection of hashable objects.")
    (license license:expat)))

(define-public rust-cuckoofilter-0.5
  (package
    (name "rust-cuckoofilter")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cuckoofilter" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "067fkr9dc118rqddr72xdldq05d31yyipvvyrmj9yrrik52ah45q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Not packaging dependencies
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ;("rust-clippy" ,rust-clippy-0.0.302)
        ;("rust-farmhash" ,rust-farmhash-1)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-derive" ,rust-serde-derive-1))
       #:cargo-development-inputs
       (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "http://axiom.co")
    (synopsis "Cuckoo Filter: Practically Better Than Bloom")
    (description "Cuckoo Filter: Practically Better Than Bloom")
    (license license:expat)))

(define-public rust-gfa-0.6
  (package
    (name "rust-gfa")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gfa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ghmy4r0324s6vvmj9nmh326346nkwm7nybnpcpswnjvf02b85gw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-0.2)
        ("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-nom" ,rust-nom-5)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/chfi/rs-gfa")
    (synopsis
     "Library for working with graphs in the GFA (Graphical Fragment Assembly) format")
    (description
     "Library for working with graphs in the GFA (Graphical Fragment Assembly) format")
    (license license:expat)))

(define-public rust-handlegraph-0.7.0-alpha.9
  (package
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
    (build-system cargo-build-system)
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
        ("rust-rand" ,rust-rand-0.7))))
    (home-page "https://github.com/chfi/rs-handlegraph")
    (synopsis "Library for use in variation graphs")
    (description
     "This package provides a Rust implementation of VG handle graph.")
    (license license:expat)))

(define-public rust-handlegraph-0.3
  (package
    (inherit rust-handlegraph-0.7.0-alpha.9)
    (name "rust-handlegraph")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "handlegraph" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sj100w4lpj7798pws85qrfrzsily5hhzh6j118rwf56sgic1yml"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-0.2)
        ("rust-gfa" ,rust-gfa-0.6))))))

(define-public rust-kstring-1
  (package
    (name "rust-kstring")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "kstring" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1r4n9fa5scikqvl736nxghcfa6s3b07xz61w43hyzs2qb3wmd3nk"))))
    (build-system cargo-build-system)
    (arguments
     `(;#:skip-build? #t     ; Uses unstable features.
       #:cargo-inputs
       (("rust-document-features" ,rust-document-features-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-static-assertions" ,rust-static-assertions-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/cobalt-org/kstring")
    (synopsis "String optimized for map keys")
    (description "Key String provides a Rust package optimized for map keys.")
    (license (list license:expat license:asl2.0))))

(define-public rust-line-drawing-0.8
  (package
    (name "rust-line-drawing")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "line-drawing" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18a940s3mv8w5prpb99sdxykzhrvzrnymw3hvd7wisnkgbr11jqm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-traits" ,rust-num-traits-0.2))
       #:cargo-development-inputs
       (("rust-bresenham" ,rust-bresenham-0.1)
        ("rust-image" ,rust-image-0.23)
        ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/expenses/line_drawing")
    (synopsis
     "A collection of line-drawing algorithms for use in graphics and video games.")
    (description
     "This package provides a collection of line-drawing algorithms for use in
graphics and video games.")
    (license license:expat)))

(define-public rust-lodepng-3
  (package
    (name "rust-lodepng")
    (version "3.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lodepng" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1f5d9bva17sq7npw9qh7h6sh02k0ycrjx5hr147q0jv4m0qd2970"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             ;; Not all files included.
             "--skip=test::read_icc")
       #:cargo-inputs
       (("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-fallible-collections" ,rust-fallible-collections-0.4)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-rgb" ,rust-rgb-0.8))))
    (home-page "https://lib.rs/crates/lodepng")
    (synopsis
     "Reading and writing PNG files without system dependencies. Pure Rust port of LodePNG.")
    (description
     "Reading and writing PNG files without system dependencies.  Pure Rust port of
@code{LodePNG}.")
    (license license:zlib)))

(define-public rust-pyo3-0.14
  (package
    (name "rust-pyo3")
    (version "0.14.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pyo3" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j70b9bkncgpgnfxjxyvp4mk40rp55lk6qmacxm5c2k78y9hy41m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-hashbrown" ,rust-hashbrown-0.11)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-indoc" ,rust-indoc-0.3)
        ("rust-inventory" ,rust-inventory-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-num-complex" ,rust-num-complex-0.4)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-paste" ,rust-paste-0.1)
        ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.14)
        ("rust-pyo3-macros" ,rust-pyo3-macros-0.14)
        ("rust-serde" ,rust-serde-1)
        ("rust-unindent" ,rust-unindent-0.1))
       #:cargo-development-inputs
       (("rust-assert-approx-eq" ,rust-assert-approx-eq-1)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-proptest" ,rust-proptest-0.10)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-trybuild" ,rust-trybuild-1))))
    (inputs (list python))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Rust bindings for the Python interpreter")
    (description
     "This package provides Rust bindings for Python, including tools for
creating native Python extension modules.  Running and interacting with
Python code from a Rust binary is also supported.")
    (license license:asl2.0)))

(define-public rust-pyo3-build-config-0.14
  (package
    (name "rust-pyo3-build-config")
    (version "0.14.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pyo3-build-config" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13vkcl49b2x81azb613ss256k8pazrfc4fy4ny8pzgdciirn2afi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Build configuration for the PyO3 ecosystem")
    (description "Build configuration for the PyO3 ecosystem")
    (license license:asl2.0)))

(define-public rust-pyo3-macros-0.14
  (package
    (name "rust-pyo3-macros")
    (version "0.14.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pyo3-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a4fh24c5q85f31n2rwbqrai2bjprf9kzh6xvpgj8j3hblhwa2zw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pyo3-macros-backend" ,rust-pyo3-macros-backend-0.14)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Proc macros for PyO3 package")
    (description "Proc macros for PyO3 package")
    (license license:asl2.0)))

(define-public rust-pyo3-macros-backend-0.14
  (package
    (name "rust-pyo3-macros-backend")
    (version "0.14.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pyo3-macros-backend" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ps068jqyq1275zxxbzn6hyz9lkfz35az8waj6mzlji2jg2kyqki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.14)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Code generation for PyO3 package")
    (description "Code generation for PyO3 package")
    (license license:asl2.0)))

(define-public rust-quick-csv-0.1
  (package
    (name "rust-quick-csv")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "quick-csv" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "131k8zzlplk2h62wz813jbvm0sk7v3mixwhhq34y9lmp3mqbgx7d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustc-serialize" ,rust-rustc-serialize-0.3))))
    (home-page "https://github.com/tafia/quick-csv")
    (synopsis "quick csv reader and decoder")
    (description "quick csv reader and decoder")
    (license license:expat)))

(define-public rust-succinct-0.5
  (package
    (name "rust-succinct")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "succinct" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0654c9gq50x7djyf25zbzz3d2pc4x3z21wmjj3qbr6d9h4hbd63p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-num-traits" ,rust-num-traits-0.2))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.9))))
    (home-page "https://github.com/tov/succinct-rs")
    (synopsis "Succinct data structures for Rust")
    (description "This package provides succinct data structures for Rust.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-trycmd-0.12
  (package
    (name "rust-trycmd")
    (version "0.12.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trycmd" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1rwa5nzq8c5zg7lqmpkf7hyib415yxshd9amp911y8w1zss4s38p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-concolor" ,rust-concolor-0.0.8)
        ("rust-content-inspector" ,rust-content-inspector-0.2)
        ("rust-difflib" ,rust-difflib-0.4)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-escargot" ,rust-escargot-0.5)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-humantime" ,rust-humantime-2)
        ("rust-humantime-serde" ,rust-humantime-serde-1)
        ("rust-normalize-line-endings" ,rust-normalize-line-endings-0.3)
        ("rust-os-pipe" ,rust-os-pipe-1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-schemars" ,rust-schemars-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-shlex" ,rust-shlex-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-toml-edit" ,rust-toml-edit-0.12)
        ("rust-wait-timeout" ,rust-wait-timeout-0.2)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-yansi" ,rust-yansi-0.5))))
    (home-page "https://github.com/assert-rs/trycmd")
    (synopsis "Snapshot testing for a herd of CLI tests")
    (description "Snapshot testing for a herd of CLI tests")
    (license (list license:expat license:asl2.0))))

(define-public rust-toml-edit-0.12
  (package
    (name "rust-toml-edit")
    (version "0.12.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml-edit" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0wx4wd849bmkqj0gdi041gmpfpvlyhy2ha4zpin69yw9d9npl8cl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Not all inputs packaged
       ;#:tests? #f          ; Skip tests for now
       #:cargo-inputs
       (("rust-combine" ,rust-combine-4)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-kstring" ,rust-kstring-1)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ;("rust-fs-snapshot" ,rust-fs-snapshot-0.1)
        ;("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-toml" ,rust-toml-0.5)
        ;("rust-toml-test-harness" ,rust-toml-test-harness-0.3)
        )))
    (home-page "https://github.com/ordian/toml_edit")
    (synopsis "Yet another format-preserving TOML parser.")
    (description "Yet another format-preserving TOML parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-vcf-0.6
  (package
    (name "rust-vcf")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vcf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dc0p00a19rpmhrqcshrn2qg5l716b5s1fy8vpd3p32bw77vpbs0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included
       #:cargo-inputs
       (("rust-nom" ,rust-nom-7)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-flate2" ,rust-flate2-1))))
    (home-page "https://github.com/informationsea/vcf-rs")
    (synopsis "Rust implmentation of VCF parser")
    (description "This package provides a rust implmentation of a VCF parser.")
    (license license:asl2.0)))

(define-public rust-gsl-sys
  (package
    (name "rust-gsl-sys")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "GSL-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17dx066l1pbjwp9syjkzqb6fiajyb4wc814zqdfrj807rh6nfxs5"))))
    (build-system cargo-build-system)
    (native-inputs
     (list gsl))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
	("rust-pkg-config"  ,rust-pkg-config-0.3))))
    (home-page "https://github.com/GuillaumeGomez/rust-GSL")
    (synopsis "A rust binding for the GSL (the GNU scientific library)")
    (description "This is the FFI counter-part of the Rust GSL crate. It is better to use the GSL crate directly rather than this one (unless something is missing the Rust binding!).")
    (license license:expat)))

(define-public rust-gsl
  (package
    (name "rust-gsl")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "GSL" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kpiivagrsk9ags7d4k2521jwix0lqgnd3i8ayj3dfniszvcmgn9"))))
    (build-system cargo-build-system)
    (native-inputs
     (list gsl))
    (arguments
     `(#:tests? #false
       #:cargo-inputs
       (("GSL-sys" ,rust-gsl-sys)
	("paste"  ,rust-paste-1))))
    (home-page "https://github.com/GuillaumeGomez/rust-GSL")
    (synopsis "A rust binding for the GSL (the GNU scientific library)")
    (description "This package is wrapper for gsl")
    (license license:gpl3+)))

(define-public gn-rust-correlation
  (let ((commit "a22381e9830bac9a6848449279d258397217e251")
        (revision "0"))
    (package
      (name "gn-rust-correlation")
      (version (git-version "0.1.4" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/Alexanderlacuna/correlation_rust.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1cb197p8zmgxymzs9b7bygs0kg67809z6gc2svza909sxr9ac5ps"))))
      (build-system cargo-build-system)
      (native-inputs
       (list gsl))
      (arguments
       `(#:cargo-inputs 
         (("rust-serde" ,rust-serde-1)
	  ("rust-serde-json" ,rust-serde-json-1)
	  ("GSL" ,rust-gsl)
	  ("rust-assert-approx-eq"  ,rust-assert-approx-eq-1))
	 #:cargo-development-inputs
	 (("rust-criterion" ,rust-criterion-0.3))))
      (home-page "https://github.com/Alexanderlacuna/correlation_rust")
      (synopsis "Re-implementation of genenetwork/correlation in Rust")
      (description "Re-implementation of genenetwork/correlation in Rust")
      (license #f))))
