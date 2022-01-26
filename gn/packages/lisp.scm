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

(define-public sbcl-snakes
  (let ((commit "8c7eae579bb24539dbd584a81a1049f3d3ff8bf8")
        (revision "0"))
    (package
      (name "sbcl-snakes")
      (version "0.4.2")
      (source 
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/BnMcGn/snakes")
                (commit commit)))
          (sha256
           (base32 "1ibp919qcpm6kg67b507kpjzdlhpdjr7vkh9vabln3a75k8lnlsg"))
          (file-name (git-file-name name commit))))
      (build-system asdf-build-system/sbcl)
      (inputs
        (list sbcl-alexandria 
              sbcl-cl-cont 
              sbcl-cl-utilities 
              sbcl-closer-mop
              sbcl-fiveam
              sbcl-iterate))
      (arguments
       `(#:asd-files '("snakes.asd")))
      (home-page "https://github.com/BnMcGn/snakes")
      (synopsis "Python-like generators for Common Lisp")
      (description
  "Python style generators for Common Lisp. Includes a port of itertools.")
      (license license:expat))))

(define-public cl-snakes
  (sbcl-package->cl-source-package sbcl-snakes))

(define-public ecl-snakes
  (sbcl-package->ecl-package sbcl-snakes))

(define-public sbcl-defstar
  (let ((commit "132829dac9f84fa7202a0c5793aa6accb8d2662a")
        (revision "0"))
    (package
      (name "sbcl-defstar")
      (version "0.4.2")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/lisp-maintainers/defstar")
                (commit commit)))
          (sha256
           (base32 "0n6m3aqvdfnsrhlhqjcy72d1i55lbkjg13ij5c7vw003p1n78wxi"))
          (file-name (git-file-name name commit))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-files '("defstar.asd")))
      (home-page "https://github.com/lisp-maintainers/defstar")
      (synopsis "Type declarations for defun et all")
      (description
"@code{defstar} is a collection of Common Lisp macros that can be used in
place of @code{defun}, @code{defmethod}, @code{defgeneric}, @code{defvar},
@code{defparameter}, @code{flet}, @code{labels}, @code{let}* and
@code{lambda}. Each macro has the same name as the form it replaces,
with a star added at the end.")
      (license license:gpl3))))

(define-public cl-defstar
  (sbcl-package->cl-source-package sbcl-defstar))

(define-public ecl-defstar
  (sbcl-package->ecl-package sbcl-defstar))

(define-public sbcl-2am
  (let ((commit "1d2fd21bbd8f26ec91b962705cab098dd7b5f11c")
        (revision "0"))
    (package
      (name "sbcl-2am")
      (version "0.4.2")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://gitlab.common-lisp.net/dkochmanski/2am")
                (commit commit)))
          (sha256
           (base32 "0zgx4ymyzvfg44z36yr4l87cd9mprajd7sycr2zc67ab6330rynf"))
          (file-name (git-file-name name commit))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-files '("2am.asd")))
      (home-page "https://gitlab.common-lisp.net/dkochmanski/2am")
      (synopsis "Small testing framework based on 1am")
      (description
"Small testing framework for Common Lisp.  The entire API consists of:
@code{test}, @code{is}, @code{signals}, @code{finishes}, @code{run},
suite and @code{setf} suite.")
      (license license:expat))))

(define-public cl-2am
  (sbcl-package->cl-source-package sbcl-2am))

(define-public ecl-2am
  (sbcl-package->ecl-package sbcl-2am))

(define-public sbcl-fwoar-lisputils
  (let ((commit "7093110d43e91bc1685ad430da99785d9ec61dae")
        (revision "0"))
    (package
      (name "sbcl-fwoar-lisputils")
      (version "0.4.2")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/fiddlerwoaroof/fwoar.lisputils")
                (commit commit)))
          (sha256
           (base32 "0wfy4k2xpnadzy6yfn5iavc8hv1m688942sc1yp8xkdm7pk0mvvi"))
          (file-name (git-file-name name commit))))
      (build-system asdf-build-system/source) ; FIXME
      (home-page "https://github.com/fiddlerwoaroof/fwoar.lisputils")
      (synopsis "Hodgepodge of useful Lisp snippets")
      (description "A hodgepodge of useful Lisp snippets.")
      (license license:expat))))

