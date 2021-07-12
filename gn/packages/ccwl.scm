(define-module (gn packages ccwl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages skribilo))

(define-public ccwl
  (package
    (name "ccwl")
    (version "0.1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/arunisaac/ccwl"
                                 "/releases/download/v" version
                                 "/ccwl-" version ".tar.lz"))
             (sha256
              (base32 "0zj8rsmh82ip9ngq65is3yfbpn9y0acil4llwrr4jw2fiyyji7x1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")))   ;to prevent guild warnings
    (propagated-inputs
     `(("guile" ,guile-3.0)))
    (native-inputs
     `(("guile" ,guile-3.0)
       ("lzip" ,lzip)
       ("pkg-config" ,pkg-config)
       ;; For the documentation:
       ("cwltool" ,cwltool)
       ("graphviz" ,graphviz)
       ("skribilo" ,skribilo)))
    (home-page "https://ccwl.systemreboot.net/")
    (synopsis "Concise Common Workflow Language")
    (description
     "The @acronym{Concise Common Workflow Languager, ccwl} is a concise syntax
to express CWL workflows.  It is implemented as an @acronym{Embedded Domain
Specific Language, EDSL} in the Scheme programming language, a minimalist
dialect of the Lisp family of programming languages.  @code{ccwl} is a compiler
to generate CWL workflows from concise descriptions in ccwl.")
    (license license:gpl3+)))
