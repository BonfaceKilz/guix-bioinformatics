(define-module (gn packages globus)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public python-globus-sdk
  (package
    (name "python-globus-sdk")
    (version "3.37.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "globus-sdk" version))
              (sha256
               (base32
                "19w3pjzfycaqvvr11nq8c91i6pkkkic95yf170hr39dwj70lrkc7"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cryptography python-pyjwt python-requests
                             python-typing-extensions))
    (home-page "https://github.com/globus/globus-sdk-python")
    (synopsis "Globus SDK for Python")
    (description "@code{python-globus-sdk} provides a convenient Pythonic interface to
Globus APIs.")
    (license license:asl2.0)))
