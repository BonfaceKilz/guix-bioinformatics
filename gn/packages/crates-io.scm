(define-module (gn packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages maths))

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

(define-public rust-handlegraph-0.7
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
    (inherit rust-handlegraph-0.7)
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

;; replace fields with those from upstream
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
     `(#:cargo-inputs
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
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-trybuild" ,rust-trybuild-1)
        ("rust-trycmd" ,rust-trycmd-0.12))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
      "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      "This package provides a simple to use, efficient, and full-featured Command Line
      Argument Parser")
    (license (list license:expat license:asl2.0))))

;; ready to upstream, WITH rust-clap-derive
;; replace fields with those from upstream.
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
     `(#:skip-build? #t     ; Not all inputs packaged
       ;#:tests? #f          ; Skip tests for now
       #:cargo-inputs
       (("rust-hyphenation" ,rust-hyphenation-0.8)
        ("rust-smawk" ,rust-smawk-0.3)
        ("rust-terminal-size" ,rust-terminal-size-0.1)
        ("rust-unicode-linebreak" ,rust-unicode-linebreak-0.1)
        ("rust-unicode-width" ,rust-unicode-width-0.1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ;("rust-lipsum" ,rust-lipsum-0.8)
        ("rust-termion" ,rust-termion-1)
        ;("rust-unic-emoji-char" ,rust-unic-emoji-char-0.9)
        ("rust-version-sync" ,rust-version-sync-0.9))))
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
  (let ((commit "b82a93b691bb86e9dcf4ed30a4011a159857bba5")
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
            "03jqxnv3f8gc8bd1sz4jgrzhrgisd7jwsayvn0njys7i0a715ps3"))))
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
