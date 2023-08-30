(define-module (gn packages gn-auth)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)

  ;; Packages from guix
  #:use-module (gnu packages check)

  #:use-module (gnu packages django)

  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)

  #:use-module (gnu packages databases)

  ;; Packages from guix-bioinformatics
  #:use-module (gn packages python-web))

(define-public gn-auth
  (package
    (name "gn-auth")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/genenetwork/gn-auth.git")
	     (commit "6b4e800ad2b642d3dce80cdcf84a102aad64fcfd")))
       (hash
	(content-hash
	 (base32
	  "036k561kvzb6nwr1k1pd9fi20v4d7cnj5jdps6a30yjprfvj8y4l")))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       #~(modify-phases %standard-phases
	   (replace 'check
	     (lambda* (#:key tests? #:allow-other-keys)
	       (when tests?
		 (invoke "pytest" "-k" "unit_test")))))))
    (native-inputs
     (list python-mypy
	   python-pytest
	   python-pylint
	   python-hypothesis
	   python-pytest-mock
           python-mypy-extensions))
    (propagated-inputs
     (list gunicorn
	   python-flask
	   python-redis
	   python-authlib
	   python-pymonad
	   yoyo-migrations
	   python-bcrypt ;; remove after removing all references
	   python-mysqlclient
	   python-argon2-cffi
	   python-email-validator))
    (home-page "https://github.com/genenetwork/gn-auth")
    (synopsis "Authentication and Authorisation server for GeneNetwork services")
    (description "Authentication and Authorisation server for GeneNetwork services.")
    (license license:agpl3+)))
