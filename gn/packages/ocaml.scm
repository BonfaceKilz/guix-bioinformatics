(define-module (gn packages ocaml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ocaml)
  #:use-module (gnu packages)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages maths))

;; This is the last version supported by ocaml-4.07.
(define-public ocaml4.07-lwt-5.5.0
  (package-with-ocaml4.07
    (package
      (inherit ocaml-lwt)
      (name "ocaml-lwt")
      (version "5.5.0")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ocsigen/lwt")
                 (commit version)))
          (file-name (git-file-name name version))
          (sha256 (base32
                   "1jbjz2rsz3j56k8vh5qlmm87hhkr250bs2m3dvpy9vsri8rkzj9z"))))
      (properties '()))))

(define-public ocaml4.07-gsl-1
  (package-with-ocaml4.07
   (package
     (inherit ocaml-gsl)
     (version "1.19.3")
     (source (origin
               (method url-fetch)
               (uri (string-append "https://github.com/mmottl/gsl-ocaml"
                                   "/releases/download/v"
                                   version "/gsl-ocaml-" version ".tar.gz"))
               (sha256
                (base32
                 "0nzp43hp8pbjqkrxnwp5lgjrabxayf61h18fjaydi0s5faq6f3xh"))))
     (build-system ocaml-build-system)
     (inputs
      (list gsl-static))
     (native-inputs
      (list ocamlbuild))
     (arguments '())
     (propagated-inputs '()))))

(define-public build-with-ocaml4.07
  (package-input-rewriting/spec `(("ocaml4.07-lwt" . ,(const ocaml4.07-lwt-5.5.0)))))
