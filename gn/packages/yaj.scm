(define-module (gn packages yaj)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages web)
  #:use-module (gn packages web))

(define-public python-elasticsearch
  (package
    (name "python-elasticsearch")
    (version "6.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "elasticsearch" version))
        (sha256
         (base32
          "1gpgb2vxl1q7hqrxxdlmfp9dxcl5wg3k78vxk2lka1cjaqd7mzw0"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nosexcover" ,python-nosexcover)
       ("python-pyaml" ,python-pyaml)
       ("python-requests" ,python-requests)))
    (propagated-inputs
     `(("urllib3" ,python-urllib3)))
    (arguments
     ;; tests require the test_elasticsearch module but it is not distributed.
     `(#:tests? #f))
    (home-page "https://github.com/elastic/elasticsearch-py")
    (synopsis "Low-level client for Elasticsearch")
    (description "Official low-level client for Elasticsearch.  Its goal is to
provide common ground for all Elasticsearch-related code in Python; because of
this it tries to be opinion-free and very extendable.")
    (license license:expat)))

(define-public yaj
  (let ((md5 "93e745e9c"))
    (package
    (name "yaj")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "http://biogems.info/genenetwork2-2.0-a8fcff4.svg") ; any old file
       (file-name (string-append name "-" md5))
       (sha256
        (base32 "0rir1mcn3a8i1mbw3ppgnjl7wg71mapljik7n3v5i8j5ic95mqr5"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))
    (inputs `(("sassc" ,sassc)))
    (propagated-inputs
     `(("python" ,python)
       ("python-elasticsearch" ,python-elasticsearch)
       ("python-flask" ,python-flask)
       ("python-jinja2" ,python-jinja2)
       ("python-mako" ,python-mako)
       ("python-markdown" ,python-markdown)
       ("python-misaka" ,python-misaka)
       ("python-pygit2" ,python-pygit2)
       ("web-bootstrap" ,web-bootstrap)
       ("sassc" ,sassc)
       ))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share")))
             (write target)
             (mkdir-p target)
             ; (copy-recursively (assoc-ref %build-inputs "source") target)
             #t))))

    (home-page "http://github.com/pjotrp/yaj/")
    (synopsis "Yet another journal")
    (description "YAJ.")
    (license license:agpl3+))))
