(define-module (gn packages binderlite)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system python))

(define-public python-jgart-giturlparse
  ;; https://github.com/nephila/giturlparse/pull/41
  (let ((commit "719f4c8e642718121f5a7b91ae8160b0041d31f9")
        (revision "0"))
    (package
      (inherit python-giturlparse)
      (name "python-jgart-giturlparse")
      (version (git-version "20211107" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://git.genenetwork.org/jgart/giturlparse")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1nyp0gw48cmnkcccgvq14adsykf9sk0z34x3j2myfm9g9cg9d669"))))
          (arguments (list #:tests? #f))
          (description
"Provides an updated clean_data function for the github platform.  See
@url{https://github.com/nephila/giturlparse/pull/41}.  @code{binderlite}
will be rewritten in Common Lisp.  If we stay with Python I recommend
switching this library out for @code{python-furl} or similar."))))

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

