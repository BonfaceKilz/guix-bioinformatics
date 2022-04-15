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

