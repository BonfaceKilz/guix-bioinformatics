(define-module (gn packages cwl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics))


(define-public cwl-runner
  (package
    (name "cwl-runner")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cwl_runner" version))
        (sha256
         (base32
          "0011am2xqwchysdznayrmwhg4bfjl4wlq6m4k20z1m7gccyzjgw0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("cwltool" ,cwltool)))
    (home-page
     "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis "Common workflow language reference implementation")
    (description
     "Common workflow language alternate entry point to allow cwl-runner 
script as an implementation-agnostic script interpreter.") 
     (license license:asl2.0)))
