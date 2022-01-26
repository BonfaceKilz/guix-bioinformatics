(define-module (gn packages lisp)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19))

(define-public sbcl-rollbar
  (let ((commit "fbaf644e3a0b077f6853d25874de6a5827b4094c")
        (revision "0"))
    (package
      (name "sbcl-rollbar")
      (version "0.0.2")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/adventuring/rollbar.lisp")
                (commit commit)))
          (sha256
           (base32 "01lax9qkb4xcd56ck88ickgpisw30zwg0s3y7rm6cnxv4qgamhzg"))
          (file-name (git-file-name name version))))
      (build-system asdf-build-system/source) ; FIXME?
      (inputs
        (list sbcl-alexandria
              sbcl-drakma
              sbcl-jonathan
              sbcl-trivial-backtrace))
      (home-page "https://github.com/adventuring/rollbar.lisp")
      (synopsis "Rollbar.com interface for Common Lisp")
      (description
  "Rollbar.com is a service for collecting automated telemetry (ie, bug
  reports, mostly) through their web service.")
      (license license:bsd-3))))

