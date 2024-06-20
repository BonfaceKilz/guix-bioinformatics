(define-module (gn packages python-web)
  ;; core guix packages
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  ;; packages modules
  #:use-module (gnu packages django)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto))

(define-public python-authlib
  (package
   (name "python-authlib")
   (version "1.2.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "Authlib" version))
            (sha256
             (base32
              "178ycfypdv1hy4zjm09rmysxakmwch2n6a5wypwmx4d5hc4fi8sg"))))
   (build-system python-build-system)
   (native-inputs (list python-httpx
			python-flask
			python-django
			python-werkzeug
			python-starlette
			python-sqlalchemy
			python-pycryptodomex))
   (propagated-inputs (list python-cryptography))
   (arguments `(#:tests? #f)) ;; temporarily deactivate tests
   (home-page "https://authlib.org/")
   (synopsis
    "The ultimate Python library in building OAuth and OpenID Connect servers and clients.")
   (description
    "The ultimate Python library in building OAuth and OpenID Connect servers and
clients. It is designed from low level specifications implementations to high
level frameworks integrations, to meet the needs of everyone.")
   (license license:bsd-3)))
