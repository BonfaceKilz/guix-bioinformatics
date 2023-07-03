(define-module (gn packages notebooks)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages))

(define-public nb-upload
  (let ((commit "b2853028a2ef2b1670074aa08c9e25d799ba4cb4")
        (revision "2"))
    (package
      (name "nb-upload")
      (version (git-version "20220414" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://git.genenetwork.org/jgart/nb-upload")
                (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
           (base32
            "1wdghsq4wj1bgcvxx604q7piknbwapj7facc1xfn1fqvw0cra7wg"))))
         (build-system python-build-system)
         (arguments
           (list #:tests? #f)) ; There are no tests.
         (inputs
           (list python-requests
                 python-yaspin
                 python-rich))
         (synopsis "Upload notebooks via CLI")
         (description
"@code{nb-upload} allows a user to upload notebooks to a @code{nb}
instance.")
         (home-page "https://git.genenetwork.org/jgart/nb-upload/")
         (license license:unlicense))))

(define-public sbcl-nb
  (let ((commit "bb9bb6cd2ac8e9aac48e97a4c68b4b9811aa817a")
        (revision "0"))
    (package
      (name "sbcl-nb")
      (version (git-version "20220414" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://git.genenetwork.org/jgart/nb")
                (commit commit)))
          (sha256
           (base32 "03wnfb2g9b9kawd9iw5hsdkzdlj0r7pkzwshhjiwn1cc04rqdi5m"))
          (file-name (git-file-name name commit))))
      (build-system asdf-build-system/sbcl)
      (arguments
        `(#:tests? #f)) ; There are no tests.
      (inputs
        (list sbcl-ningle
              sbcl-clack
              sbcl-drakma
              sbcl-find-port
              sbcl-jzon
              sbcl-legit
              sbcl-cl-interpol))
      (home-page "https://git.genenetwork.org/jgart/nb/")
      (synopsis "Launching interactive notebooks in GNU Guix containers")
      (description
"{cl-nb} is a Common Lisp system and API for launching interactive
notebooks in GNU Guix containers.")
      (license license:unlicense))))

(define-public cl-nb
  (sbcl-package->cl-source-package sbcl-nb))

(define-public ecl-nb
  (sbcl-package->ecl-package sbcl-nb))

