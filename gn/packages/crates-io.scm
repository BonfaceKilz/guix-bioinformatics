(define-module (gn packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages rust))


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
@url(https://arxiv.org/abs/1702.03154, fast and scalable minimal perfect hashing
 for massive key sets}.  It generates an @acronym{MPHF, minimal perfect hash
 functions} for a collection of hashable objects.")
    (license license:expat)))

(define-public rust-gfa-0.10
  (package
    (name "rust-gfa")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfa" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1hadm6vfjwyqw41bqci18wb4wv80rydmrag7a5c02pdp1gid14fw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-bstr" ,rust-bstr-0.2)
        ("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-memmap" ,rust-memmap-0.7)
        ("rust-nom" ,rust-nom-5)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/chfi/rs-gfa")
    (synopsis "Library for working with graphs in the GFA format")
    (description
     "This package proides a Rust llibrary for working with graphs in the
@acronym(GFA, Graphical Fragment Assembly} format.")
    (license license:expat)))

(define-public rust-handlegraph-0.7
  ;; After this commit try the next actual release.
  (let ((commit "a7ca8da640ae442ffd3ef4f5f9f358cd37c7deee")
        (revision "1"))
    (package
      (inherit rust-handlegraph-0.3)
      (name "rust-handlegraph")
      (version (git-version "0.7.0-alpha.9" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/chfi/rs-handlegraph")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "19cxgysxam79k6skanh4q4hafi4aglzcmg5hmm3vfnrzbz9q6js0"))))
      (arguments
       `(#:rust ,rust-1.47      ; or later
         #:cargo-inputs
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
          ("rust-rand" ,rust-rand-0.7)))))))

(define-public rust-quick-csv-0.1
  (package
    (name "rust-quick-csv")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quick-csv" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "131k8zzlplk2h62wz813jbvm0sk7v3mixwhhq34y9lmp3mqbgx7d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rustc-serialize" ,rust-rustc-serialize-0.3))))
    (home-page "https://github.com/tafia/quick-csv")
    (synopsis "Quick csv reader and decoder")
    (description "This package provides a quick csv reader and decoder in Rust.")
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
    (description "Succinct data structures for Rust.1")
    (license (list license:expat license:asl2.0))))

(define-public rust-wyhash-0.5
  (package
    (name "rust-wyhash")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wyhash" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "15f26hvx6nyp4d6iswha7rm3psidxa2k2iab1f1aqgsyq9iy3xms"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand-core" ,rust-rand-core-0.6))))
    (home-page "https://github.com/eldruin/wyhash-rs")
    (synopsis "Rust implementation of the WyHash algorithm")
    (description
     "This package provides a Rust implementation of the WyHash fast portable
non-cryptographic hashing algorithm and random number generator.")
    (license (list license:expat license:asl2.0))))
