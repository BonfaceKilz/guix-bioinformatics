(define-module (gn packages cwl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gn packages python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml))

(define-public python-pydot ;; can be updated in GNU Guix - this is a copy
  (package
    (name "python-pydot")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydot" version))
       (sha256
        (base32
         "00az4cbf8bv447lkk9xi6pjm7gcc7ia33y4pm71fwfwis56rv76l"))))
    (build-system python-build-system)
    (native-inputs
     ;; For tests.
     `(("python-chardet" ,python-chardet)))
    (propagated-inputs
     `(("python-pyparsing" ,python-pyparsing)))
    (home-page "https://github.com/erocarrera/pydot")
    (synopsis "Python interface to Graphviz's DOT language")
    (description
     "Pydot provides an interface to create, handle, modify and process
graphs in Graphviz's DOT language, written in pure Python.")
    (license license:expat)))


(define-public cwltool
  (let ((commit "78fe9d41ee5a44f8725dfbd7028e4a5ee42949cf")
        (revision "1"))
    (package
    (name "cwltool")
    (version "3.0.20201117141248")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/common-workflow-language/cwltool.git")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1awf99n7aglxc5zszrlrv6jxp355jp45ws7wpsgjlgcdv7advn0w"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-version-restrictions
           (lambda _
             (substitute* "setup.py"
               (("== 1.5.1") ">=1.5.1"))
             #t))
         (add-after 'unpack 'modify-tests
           (lambda _
             ;; Tries to connect to the internet.
             (delete-file "tests/test_udocker.py")
             (delete-file "tests/test_http_input.py")
             (substitute* "tests/test_load_tool.py"
               (("def test_load_graph_fragment_from_packed")
                "@pytest.mark.skip(reason=\"Disabled by Guix\")\ndef test_load_graph_fragment_from_packed"))
             (substitute* "tests/test_examples.py"
               (("def test_env_filtering")
                "@pytest.mark.skip(reason=\"Disabled by Guix\")\ndef test_env_filtering"))
             ;; Tries to use cwl-runners.
             (substitute* "tests/test_examples.py"
               (("def test_v1_0_arg_empty_prefix_separate_false")
                "@pytest.mark.skip(reason=\"Disabled by Guix\")\ndef test_v1_0_arg_empty_prefix_separate_false"))
             #t)))))
    (propagated-inputs
     `(("git" ,git)
       ("python-argcomplete" ,python-argcomplete)
       ("python-bagit" ,python-bagit)
       ("python-coloredlogs" ,python-coloredlogs)
       ("python-mypy-extensions" ,python-mypy-extensions)
       ("python-prov" ,python-prov)
       ("python-pydot" ,python-pydot)
       ("python-psutil" ,python-psutil)
       ("python-rdflib" ,python-rdflib)
       ("python-requests" ,python-requests)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-schema-salad" ,python-schema-salad)
       ("python-setuptools" ,python-setuptools)
       ("python-shellescape" ,python-shellescape)
       ("python-typing-extensions" ,python-typing-extensions)
       ;; Not listed as needed but seems to be necessary:
       ("node" ,node)
       ("python-cachecontrol" ,python-cachecontrol-0.11)
       ("python-dateutil" ,python-dateutil)
       ("python-lxml" ,python-lxml)
       ("python-networkx" ,python-networkx)))
    (native-inputs
     `(("python-arcp" ,python-arcp)
       ("python-humanfriendly" ,python-humanfriendly)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)))
    (home-page
     "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis "Common workflow language reference implementation")
    (description
     "Common workflow language reference implementation.")
    (license license:asl2.0))))

(define-public cwl-runner
  (package
    (name "cwl-runner")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cwl_runner" version))
        (sha256
         (base32
          "0011am2xqwchysdznayrmwhg4bfjl4wlq6m4k20z1m7gccyzjgw0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("cwltool" ,cwltool)))
    (home-page
     "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis "Common workflow language reference implementation")
    (description
     "Common workflow language alternate entry point to allow cwl-runner 
script as an implementation-agnostic script interpreter.") 
     (license license:asl2.0)))
