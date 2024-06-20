(define-module (gn packages cwl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages node)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
  #:use-module (gn packages python))


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

;; python-toil tightly integrates with cwltool using it as a library. So,
;; create a library version of cwltool where inputs become propagated inputs.
(define-public python-cwltool
  (package
    (inherit cwltool)
    (name "python-cwltool")
    (inputs
     (list node))
    (propagated-inputs
     (modify-inputs (package-inputs cwltool)
       (delete "node")))))

(define-public python-toil
  (package
    (name "python-toil")
    (version "6.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "toil" version))
              (sha256
               (base32
                "0hwaihnncsfxw0sf3iigvgw6nylrb56lpk8qjadkgazr98dsp4ha"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-optional-features
            (lambda _
              (substitute* "requirements-cwl.txt"
                ;; Loosen version restrictions on ruamel.yaml.
                (("ruamel.yaml[<>=.,[:digit:]]*\n") "ruamel.yaml\n")
                ;; Remove optional dependency on galaxy-util and
                ;; galaxy-tool-util. TODO: Package these and restore these
                ;; dependencies.
                (("galaxy-util[^\n]*") "")
                (("galaxy-tool-util[^\n]*") ""))
              ;; Disable optional wdl, wes and mesos features. We don't yet
              ;; have dependencies packaged for them. TODO: Package these
              ;; dependencies and enable these features.
              (substitute* "setup.py"
                (("\"wdl\",") "")
                (("toil-cwl-runner = toil.cwl.cwltoil:main \\[cwl\\]")
                 "toil-cwl-runner = toil.cwl.cwltoil:main")
                (("'toil-wdl-runner = toil.wdl.wdltoil:main \\[wdl\\]',") "")
                (("'toil-wes-cwl-runner = toil.server.cli.wes_cwl_runner:main \\[server\\]',") "")
                (("'_toil_mesos_executor = toil.batchSystems.mesos.executor:main \\[mesos\\]',") "")))))))
    (propagated-inputs
     (list python-addict
           python-cachecontrol
           python-configargparse
           python-cwltool
           python-dateutil
           python-dill
           python-docker
           python-enlighten
           python-psutil
           python-pypubsub
           python-pytz
           python-pyyaml
           python-requests
           python-ruamel.yaml
           python-typing-extensions-4.10
           python-urllib3))
    (home-page "https://github.com/DataBiosphere/toil")
    (synopsis "Scalable, efficient and cross-platform workflow engine")
    (description "Toil is a scalable, efficient, cross-platform pipeline
management system, written entirely in Python, and designed around the
principles of functional programming.")
    (license (list license:asl2.0    ;; main license
                   license:expat)))) ;; src/toil/batchSystems/lsf.py and
                                     ;; src/toil/batchSystems/lsfHelper.py
