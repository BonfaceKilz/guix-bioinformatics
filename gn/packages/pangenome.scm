;; Pangenome module

(define-module (gn packages pangenome)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io)
  #:use-module (gn packages crates-io))


(define-public gfautil
  (package
    (name "gfautil")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfautil" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0cgiis9v1nd4m7zxvgsz8jf8ijv4d8fa5wb7cpnjshksb8z7xh69"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-gfa" ,rust-gfa-0.6)
        ("rust-handlegraph" ,rust-handlegraph-0.3)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-structopt" ,rust-structopt-0.3))))
    (home-page "https://github.com/chfi/rs-gfa-utils")
    (synopsis "Command line tools for working with GFA files")
    (description
     "This package provides command line tools for working with @acronym{GFA,
Graphical Fragment Assembly} files and related formats.")
    (license license:expat)))
