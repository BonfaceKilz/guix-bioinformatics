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
  (let ((commit "7bf2ea910e88e3b5651daff943493ba95fc78e08")
        (revision "0"))
    (package
      (name "nb-upload")
      (version (git-version "20220407" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://git.genenetwork.org/jgart/nb-upload")
                (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
           (base32
            "0q7vvzjirrbls3wm6hhzfi2msdai65h9rdjaq6i215a3p135a9m2"))))
         (build-system python-build-system)
         (arguments
           (list #:tests? #f)) ; There are no tests.
         (inputs (list python-requests))
         (synopsis "Upload notebooks via CLI")
         (description
"@code{nb-upload} allows a user to upload notebooks to a @code{nb}
instance.")
         (home-page "https://git.genenetwork.org/jgart/nb-upload/")
         (license license:unlicense))))

