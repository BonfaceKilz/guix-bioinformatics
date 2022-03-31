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

(define-public rust-clap-3.1
  (package
    (name "rust-clap")
    (version "3.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "08q1hkksfixybnrwrpm44xq028wbn9yr2hnzrax9hihyq8v39jfq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Skip the tests for now
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-clap-derive" ,rust-clap-derive-3.1)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-os-str-bytes" ,rust-os-str-bytes-6)
        ("rust-regex" ,rust-regex-1)
        ("rust-strsim" ,rust-strsim-0.10)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-terminal-size" ,rust-terminal-size-0.1)
        ("rust-textwrap" ,rust-textwrap-0.15)
        ("rust-unicase" ,rust-unicase-2)
        ("rust-yaml-rust" ,rust-yaml-rust-0.4))
       ;#:cargo-development-inputs
       ;(("rust-criterion" ,rust-criterion-0.3)
       ; ("rust-lazy-static" ,rust-lazy-static-1)
       ; ("rust-regex" ,rust-regex-1)
       ; ("rust-rustversion" ,rust-rustversion-1)
       ; ("rust-trybuild" ,rust-trybuild-1)
       ; ("rust-trycmd" ,rust-trycmd-0.12))
       ))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
      "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      "This package provides a simple to use, efficient, and full-featured Command Line
      Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-3.1
  (package
    (name "rust-clap-derive")
    (version "3.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "05mz2y6k73wc1gvv9r4mllfqslzvlwkvx77lk7769ag1xlwd15fs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-heck" ,rust-heck-0.4)
        ("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_derive")
    (synopsis
      "Parse command line argument by defining a struct, derive crate.")
    (description
      "Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-os-str-bytes-6
  (package
    (name "rust-os-str-bytes")
    (version "6.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "os-str-bytes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0r5z5xds2wzzqlqjaw96dpjsz5nqyzc1rflm4mh09aa32qyl88lf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr-2)
        ("rust-print-bytes" ,rust-print-bytes-0.5)
        ("rust-uniquote" ,rust-uniquote-3))
       ;#:cargo-development-inputs
       ;(("rust-getrandom" ,rust-getrandom-0.2))
       ))
    (home-page "https://github.com/dylni/os_str_bytes")
    (synopsis
     "Utilities for converting between byte sequences and platform-native strings")
    (description
     "Utilities for converting between byte sequences and platform-native strings")
    (license (list license:expat license:asl2.0))))

(define-public rust-textwrap-0.15
  (package
    (name "rust-textwrap")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "textwrap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1yw513k61lfiwgqrfvsjw1a5wpvm0azhpjr2kr0jhnq9c56is55i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Skip tests for now
       #:cargo-inputs
       (("rust-hyphenation" ,rust-hyphenation-0.8)
        ("rust-smawk" ,rust-smawk-0.3)
        ("rust-terminal-size" ,rust-terminal-size-0.1)
        ("rust-unicode-linebreak" ,rust-unicode-linebreak-0.1)
        ("rust-unicode-width" ,rust-unicode-width-0.1))
       ;#:cargo-development-inputs
       ;(("rust-criterion" ,rust-criterion-0.3)
       ; ("rust-lipsum" ,rust-lipsum-0.8)
       ; ("rust-termion" ,rust-termion-1)
       ; ("rust-unic-emoji-char" ,rust-unic-emoji-char-0.9)
       ; ("rust-version-sync" ,rust-version-sync-0.9))
       ))
    (home-page "https://github.com/mgeisler/textwrap")
    (synopsis
      "Powerful library for word wrapping, indenting, and dedenting strings")
    (description
      "Powerful library for word wrapping, indenting, and dedenting strings")
    (license license:expat)))

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

(define-public rust-smawk-0.3
  (package
    (name "rust-smawk")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smawk" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0hv0q1mw1r1brk7v3g4a80j162p7g1dri4bdidykrakzfqjd4ypn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ndarray" ,rust-ndarray-0.14))
       ;#:cargo-development-inputs
       ;(("rust-num-traits" ,rust-num-traits-0.2)
       ; ("rust-rand" ,rust-rand-0.8)
       ; ("rust-rand-chacha" ,rust-rand-chacha-0.3)
       ; ("rust-version-sync" ,rust-version-sync-0.9))
       ))
    (home-page "https://github.com/mgeisler/smawk")
    (synopsis "Functions for finding row-minima in a totally monotone matrix.")
    (description
      "This package provides functions for finding row-minima in a totally monotone
matrix.")
    (license license:expat)))

(define-public rust-unicode-linebreak-0.1
  (package
    (name "rust-unicode-linebreak")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-linebreak" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0grq6bsn967q4vpifld53s7a140nlmpq5vy8ghgr73f4n2mdqlis"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/axelf4/unicode-linebreak")
    (synopsis "Implementation of the Unicode Line Breaking Algorithm")
    (description "Implementation of the Unicode Line Breaking Algorithm")
    (license license:asl2.0)))

(define-public rust-lipsum-0.8
  (package
    (name "rust-lipsum")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lipsum" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0sn5k0hgx099x2qdx0xlx8a5b74sfc55qnbyrhnh72baqxqp5vj2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand" ,rust-rand-0.8)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3))
       ;#:cargo-development-inputs
       ;(("rust-version-sync" ,rust-version-sync-0.9))
       ))
    (home-page "https://github.com/mgeisler/lipsum/")
    (synopsis
      "Lipsum is a lorem ipsum text generation library. Use this if you need
some filler text for your application.

The text is generated using a simple Markov chain, which you can also
instantiate to generate your own pieces of pseudo-random text.
")
    (description
      "Lipsum is a lorem ipsum text generation library.  Use this if you need some
filler text for your application.

The text is generated using a simple Markov chain, which you can also
instantiate to generate your own pieces of pseudo-random text.")
    (license license:expat)))

(define-public rust-print-bytes-0.5
  (package
    (name "rust-print-bytes")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "print-bytes" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0d4i9y3jx1chi6w97a8rgdbwm9g3cppr53rw53zl6fcaq31qx0b6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; Skip tests for now
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))
       ;#:cargo-development-inputs
       ;(("rust-os-str-bytes" ,rust-os-str-bytes-4))
       ))
    (home-page "https://github.com/dylni/print_bytes")
    (synopsis "Print bytes as losslessly as possible")
    (description "Print bytes as losslessly as possible")
    (license (list license:expat license:asl2.0))))

(define-public rust-uniquote-3
  (package
    (name "rust-uniquote")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uniquote" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1bkl0n41yvs415mqny4b434kr456ysnb3dhic1zrrzppwx95jvxa"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dylni/uniquote")
    (synopsis "Quote strings for clear display in output")
    (description "Quote strings for clear display in output")
    (license (list license:expat license:asl2.0))))

(define-public rust-ndarray-0.14
  (package
    (name "rust-ndarray")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndarray" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "011wqzmrd9gpfcfvy1xfbskqfiahn96pmi2d0r9x34d682amq3bc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-approx" ,rust-approx-0.4)
        ;("rust-blas-src" ,rust-blas-src-0.6)
        ("rust-cblas-sys" ,rust-cblas-sys-0.1)
        ("rust-matrixmultiply" ,rust-matrixmultiply-0.2)
        ("rust-num-complex" ,rust-num-complex-0.3)
        ("rust-num-integer" ,rust-num-integer-0.1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rawpointer" ,rust-rawpointer-0.2)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-serde" ,rust-serde-1))
       ;#:cargo-development-inputs
       ;(("rust-approx" ,rust-approx-0.4)
       ; ("rust-defmac" ,rust-defmac-0.2)
       ; ("rust-itertools" ,rust-itertools-0.9)
       ; ("rust-quickcheck" ,rust-quickcheck-0.9))
       ))
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis
      "An n-dimensional array for general elements and for numerics. Lightweight array views and slicing; views support chunking and splitting.")
    (description
      "An n-dimensional array for general elements and for numerics.  Lightweight array
views and slicing; views support chunking and splitting.")
    (license (list license:expat license:asl2.0))))

(define-public rust-version-sync-0.9
  (package
    (name "rust-version-sync")
    (version "0.9.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version-sync" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1w0v20p6k13yhfmgmcwhgy3371znyqcn83lhrf47swq7xhf81l4r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-semver" ,rust-semver-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/mgeisler/version-sync")
    (synopsis
      "Crate for ensuring that version numbers in README files and other files are kept in sync with the crate version.")
    (description
      "Crate for ensuring that version numbers in README files and other files are kept
in sync with the crate version.")
    (license license:expat)))

(define-public rust-os-pipe-1
  (package
    (name "rust-os-pipe")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "os-pipe" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0mczqmqrkzmln4xg5ki1gwgykf4dsii0h4p7fxf667889ysz54ic"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/oconnor663/os_pipe.rs")
    (synopsis "a cross-platform library for opening OS pipes")
    (description "a cross-platform library for opening OS pipes")
    (license license:expat)))

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
     `(#:cargo-inputs
       (("rust-combine" ,rust-combine-4)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-kstring" ,rust-kstring-1)
        ("rust-serde" ,rust-serde-1))
       ;#:cargo-development-inputs
       ;(("rust-criterion" ,rust-criterion-0.3)
       ; ("rust-fs-snapshot" ,rust-fs-snapshot-0.1)
       ; ("rust-pretty-assertions" ,rust-pretty-assertions-1)
       ; ("rust-serde-json" ,rust-serde-json-1)
       ; ("rust-toml" ,rust-toml-0.5)
       ; ("rust-toml-test-harness" ,rust-toml-test-harness-0.3))
       ))
    (home-page "https://github.com/ordian/toml_edit")
    (synopsis "Yet another format-preserving TOML parser.")
    (description "Yet another format-preserving TOML parser.")
    (license (list license:expat license:asl2.0))))

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
     `(#:cargo-inputs
       (("rust-document-features" ,rust-document-features-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-static-assertions" ,rust-static-assertions-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/cobalt-org/kstring")
    (synopsis "Key String: optimized for map keys")
    (description "Key String: optimized for map keys")
    (license (list license:expat license:asl2.0))))

(define-public rust-document-features-0.2
  (package
    (name "rust-document-features")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "document-features" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "121wr2bd8a4s5i5yrxjz8c5amw2l69xmqqma86x6y4xmcgyhj75h"))))
    (build-system cargo-build-system)
    (home-page "https://slint-ui.com")
    (synopsis
      "Extract documentation for the feature flags from comments in Cargo.toml")
    (description
      "Extract documentation for the feature flags from comments in Cargo.toml")
    (license (list license:expat license:asl2.0))))

