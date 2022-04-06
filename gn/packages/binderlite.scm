(define-module (gn packages binderlite)
  #:use-module (gnu packages)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
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

