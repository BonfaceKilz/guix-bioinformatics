(define-module (gn packages ocaml)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system ocaml)
  #:use-module (gnu packages)
  #:use-module (gnu packages ocaml))

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

(define-public build-with-ocaml4.07
  (package-input-rewriting/spec `(("ocaml4.07-lwt" . ,(const ocaml4.07-lwt-5.5.0)))))
