(define-module (gn packages globus)
  #:use-module (gnu packages check)
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

(define-public python-click-type-test
  (package
    (name "python-click-type-test")
    (version "0.0.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "click-type-test" version))
              (sha256
               (base32
                "1i3z7akiz7s8jy6x0vzrak88m55ac1spq88vziwryzr7355y3hgq"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest))
    (propagated-inputs (list python-click))
    (home-page "https://github.com/sirosen/click-type-test")
    (synopsis "Test that type annotations match click parameter types")
    (description "@code{python-click-type-test} allows you to test that your click
options and arguments match your type annotations.")
    (license license:expat)))

(define-public globus-cli
  (package
    (name "globus-cli")
    (version "3.25.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "globus-cli" version))
              (sha256
               (base32
                "05div2psajmqdy9md804q4x6ha4yfp4w6yrxz0ynsq3i62a6cl5v"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))
    (propagated-inputs
     (list python-click
           python-cryptography
           python-globus-sdk
           python-jmespath
           python-packaging
           python-requests
           python-typing-extensions))
    (home-page "https://docs.globus.org/cli")
    (synopsis "Globus CLI")
    (description "@code{globus-cli} provides a command-line interface to
Globus APIs.")
    (license license:asl2.0)))
