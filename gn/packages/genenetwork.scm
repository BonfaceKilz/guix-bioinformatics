;; Bioinformatics module

(define-module (gn packages genenetwork)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix graph)
  #:use-module (guix scripts graph)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages search)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gn packages bioinformatics)
  #:use-module (gn packages crates-io)
  #:use-module (gn packages gemma)
  #:use-module (gn packages javascript)
  #:use-module (gn packages python)
  #:use-module (gn packages statistics)
  #:use-module (gn packages web)
  #:use-module (gn packages python-web)
  #:use-module (srfi srfi-1))


(define-public rust-qtlreaper
  (let ((commit "2e7fed6d45b0b602d80fa2a55835f96ef1cba9e3")
        (revision "1"))
    (package
      (name "rust-qtlreaper")
      (version "0.1.4")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/chfi/rust-qtlreaper.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0h70aalsplmc6xn1w7ha102n3bsi3gqkbnbrjvjm2za37c07gv0g"))))
      (build-system cargo-build-system)
      (arguments
       `(#:cargo-inputs
         (("rust-rand" ,rust-rand-0.6)
          ("rust-structopt" ,rust-structopt-0.2)
          ("rust-rayon" ,rust-rayon-1)
          ("rust-serde" ,rust-serde-1)
          ("rust-serde-json" ,rust-serde-json-1)
          ("rust-ndarray" ,rust-ndarray-0.12))
         #:phases
         (modify-phases %standard-phases
           ;; Test results vary based on the machine running them.
           (replace 'check
             (lambda _
               (or (assoc-ref %standard-phases 'check)
                   (begin
                     (substitute* "src/geneobject.rs"
                       ;; array![Genotype::Unk, Genotype::Unk, Genotype::Pat]
                       (("0.3421367343627405") "0.3421367343627406")
                       ;; array![Genotype::Unk, Genotype::Unk, Genotype::Unk]
                       (("-0.3223330030526561") "-0.32233300305265566"))
                     (assoc-ref %standard-phases 'check)))
               #t)))))
      (home-page "https://github.com/chfi/rust-qtlreaper")
      (synopsis "Reimplementation of genenetwork/QTLReaper in Rust")
      (description "Reimplementation of genenetwork/QTLReaper in Rust")
      (license #f))))

; Tests on the upstream python-pengouin package are broken. So, we
; create this temporary workaround.
(define python-pingouin-without-tests
 (package
   (inherit python-pingouin)
   (arguments
    (substitute-keyword-arguments (package-arguments python-pingouin)
      ((#:tests? _ #f) #f)))))

(define-public genenetwork3
  (let ((commit "6bb4a5f05c1a2c96b7da1780ae4a1d70c7cc4afb")
        (revision "5"))
    (package
      (name "genenetwork3")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/genenetwork/genenetwork3.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "13nvi2gwwb0shra0d9rsfhppa9ky4bhgh1zgb34790ax0g9lwa97"))))
      (inputs
       (list python-click))
      (native-inputs
       `(("python-hypothesis" ,python-hypothesis)
	 ("python-mypy" ,python-mypy)
	 ("python-mypy-extensions" ,python-mypy-extensions)
         ("python-pylint" ,python-pylint)
         ("python-pytest" ,python-pytest)
         ("python-pytest-mock" ,python-pytest-mock)))
      (propagated-inputs
       `(("gemma-wrapper" ,gemma-wrapper)
	 ("python-wrapper" ,python-wrapper)
	 ("csvdiff" ,csvdiff)
	 ("gn-rust-correlation" ,gn-rust-correlation)
	 ;; Replace use of bcrypt with argon below
	 ("python-bcrypt" ,python-bcrypt)
	 ("python-argon2-cffi" ,python-argon2-cffi)
	 ("python-flask" ,python-flask)
	 ("python-flask-cors" ,python-flask-cors)
	 ;; Not working in Python > 3.8
	 ;; python-ipfshttpclient
	 ("python-lmdb" ,python-lmdb)
	 ("python-mysqlclient" ,python-mysqlclient)
	 ("python-numpy" ,python-numpy)
	 ("python-pandas" ,python-pandas)
	 ;; python-pingouin << build failing
	 ("python-pingouin-without-tests" ,python-pingouin-without-tests)
	 ("python-plotly" ,python-plotly)
	 ("python-pyld" ,python-pyld)
	 ("python-scikit-learn" ,python-scikit-learn)
	 ("python-pymonad" ,python-pymonad)
	 ("python-redis" ,python-redis)
	 ("python-requests" ,python-requests)
	 ("python-scipy" ,python-scipy)
	 ("python-authlib" ,python-authlib)
	 ("python-sparqlwrapper" ,python-sparqlwrapper)
	 ("python-email-validator" ,python-email-validator)
	 ("python-xapian-bindings" ,python-xapian-bindings)
	 ("r-optparse" ,r-optparse)
	 ("r-qtl" ,r-qtl)
	 ("r-rjson" ,r-rjson)
	 ("r-stringi" ,r-stringi)
	 ("r-wgcna" ,r-wgcna)
	 ("r-ctl" ,r-ctl)
	 ("rust-qtlreaper" ,rust-qtlreaper)
	 ("diffutils" ,diffutils)))
      (build-system python-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (replace 'check
                   (lambda* (#:key tests? #:allow-other-keys)
                     (when tests?
                       (invoke "pytest" "-k" "unit_test")))))))
      (home-page "https://github.com/genenetwork/genenetwork3")
      (synopsis "GeneNetwork3 API for data science and machine learning.")
      (description "GeneNetwork3 API for data science and machine learning.")
      (license license:agpl3+))))

(define-public genenetwork2
  (let ((commit "1e81c74ee8752ad2913b4e610ba7575638755385")
        (revision "4"))
    (package
      (name "genenetwork2")
      (version (git-version "3.11" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/genenetwork/genenetwork2.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "18g4dd3h737slqmp1ql2jqqzmzql3mx3b4xy72d7qwhwwhc7hcgv"))))
      (native-inputs
       (list graphviz))
      (propagated-inputs
       (list genenetwork3
             parallel
             coreutils
             git
             which
             grep
             r
             r-ctl
             r-qtl
             r-wgcna
             redis
             mariadb
             gemma
             gemma-wrapper
             plink-ng-gn
             rust-qtlreaper
             gn-rust-correlation
             glibc-utf8-locales
             nginx
             python-wrapper
             python-pillow
             python-coverage
             python-configparser
             python-flask
             gunicorn
             python-autopep8
             python-cssselect
             python-flask-debugtoolbar
             python-htmlgen
             python-ijson
             python-jinja2
             python-pytest
             python-pytest-mock
             python-sqlalchemy
             python-setuptools
             python-scipy
             python-lxml
             python-mysqlclient
             python-mypy
             python-numpy
             python-pandas
             python-pylint
             python-pymonad
             python-redis
             python-requests
             python-simplejson
             python-markdown
             python-rdflib
             python-authlib
             python-flask-session
             python-rpy2
             python-beautifulsoup4
             python-xlsxwriter
             javascript-twitter-post-fetcher
             javascript-cytoscape
             javascript-cytoscape-panzoom
             javascript-cytoscape-qtip
             javascript-chroma
             javascript-d3-tip
             javascript-jscolor
             javascript-colorbox
             javascript-jszip
             js-jstat
             js-md5
             js-parsley
             javascript-plotly
             javascript-typeahead
             js-underscore
	     javascript-uikit
             js-smart-time-ago
             javascript-nouislider
             javascript-purescript-genome-browser
             javascript-ckeditor
             javascript-datatables
             javascript-datatables-scroller
             javascript-datatables-buttons
             javascript-datatables-buttons-bootstrap
             javascript-datatables-plugins
             javascript-datatables-col-reorder
             javascript-datatables-col-resize
             javascript-datatables-buttons-styles
             javascript-shapiro-wilk
             javascript-underscore-string
	     javascript-htmx
             javascript-qtip2
             javascript-d3js
             javascript-nvd3
             javascript-bootstrap
             javascript-jquery
             javascript-zxcvbn
             javascript-jquery-ui
             javascript-jquery-cookie
             ; javascript-xterm -- disabled until we know what to do with it, not working on production
             ; javascript-xterm-style
             ; javascript-xterm-addon-fit
             javascript-font-awesome))
      (inputs
       (list javascript-colorbox))
      (build-system python-build-system)
      (arguments
       (list #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'reset-gzip-timestamps)
                 (add-after 'unpack 'fix-paths-to-static-files
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     ;; Set absolute store paths to installed static files.
                     (substitute* "gn2/wqflask/marker_regression/display_mapping_results.py"
                       (("\\./gn2/wqflask/static/fonts")
                        (string-append (site-packages inputs outputs)
                                       "/gn2/wqflask/static/fonts")))
                     (substitute* "gn2/wqflask/views.py"
                       (("\\./gn2/wqflask/static/gif/error")
                        (string-append (site-packages inputs outputs)
                                       "/gn2/wqflask/static/gif/error")))))
                 (add-after 'unpack 'fix-paths-scripts
                   (lambda _
                     (substitute* "bin/genenetwork2"
                       (("/usr/bin/env") (which "env"))
                       (("python ") (string-append (which "python3") " "))
                       (("readlink") (which "readlink"))
                       (("dirname") (which "dirname"))
                       (("basename") (which "basename"))
                       (("cat") (which "cat"))
                       (("echo") (which "echo"))
                       (("redis-server") (which "redis-server"))
                       (("git") (which "git"))
                       (("grep") (which "grep"))
                       (("rm") (which "rm"))
                       (("which") (which "which")))))
                 (add-before 'install 'fix-paths
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let* ((plink2cmd (string-append (assoc-ref inputs "plink-ng-gn") "/bin/plink2"))
                            (gemmacmd (string-append (assoc-ref inputs "gemma") "/bin/gemma")))
                       (substitute* '("gn2/default_settings.py")
                              (("^PLINK_COMMAND =.*") (string-append "PLINK_COMMAND = \"" plink2cmd "\"\n" ))
                              (("^GEMMA_COMMAND =.*") (string-append "GEMMA_COMMAND = \"" gemmacmd "\"\n" )))))))))
       (home-page "http://genenetwork.org/")
       (synopsis "Full genenetwork services")
       (description "Genenetwork installation sumo.")
       (license license:agpl3+))))

(define-public gn-uploader
  (let ((commit "6ced7085193affa636f229e72dc19175a3a06cfe")
	(version "0.0.1"))
    (package
      (name "gn-uploader")
      (version (string-append version "-" (string-take commit 8)))
      (source
       (origin
	 (method git-fetch)
	 (uri (git-reference
	       (url "https://git.genenetwork.org/gn-uploader")
	       (commit commit)))
	 (hash
	  (content-hash
	   (base32
	    "09sy7kxdrf44m2yr4cqn0nx070mdnqb76888ghrwpqzgd2phfhjp")))))
      (build-system python-build-system)
      (arguments
       (list
	#:phases
	#~(modify-phases %standard-phases
	    (replace 'check
	      (lambda* (#:key tests? #:allow-other-keys)
		(when tests?
		  (invoke "pytest" "-m" "unit_test")))))))
      (native-inputs
       (list python-mypy
	     python-pylint
	     python-pytest
	     python-hypothesis))
      (propagated-inputs
       (list gunicorn
	     python-redis
	     python-flask
	     python-pyyaml
	     python-jsonpickle
	     python-mysqlclient))
      (synopsis "GeneNetwork Quality Control Application")
      (description
       "gn-uploader is a service allowing upload of new data into GeneNetwork,
 that does quality control for the data files that is being uploaded to ensure
 it fulfils all conditions before it can be accepted.")
      (home-page "https://git.genenetwork.org/gn-uploader")
      (license license:agpl3+))))

(define-public gn-auth
  (package
    (name "gn-auth")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/genenetwork/gn-auth.git")
	     (commit "cd6eebd85d32feb289ccecd1aee45fb40b03d77f")))
       (hash
	(content-hash
	 (base32
	  "06slj3z110mbc46qncdvyi29y85zkqxl76n89dxjp5wjfqxw0nsl")))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
	  (replace 'check
	    (lambda* (#:key tests? #:allow-other-keys)
	      (when tests?
		(invoke "pytest" "-k" "unit_test")))))))
    (native-inputs
     (list python-hypothesis
           python-mypy
           python-mypy-extensions
           python-pylint
           python-pytest
           python-pytest-mock))
    (propagated-inputs
     (list gunicorn
           python-argon2-cffi
           python-authlib
           python-email-validator
           python-flask
           python-flask-cors
           python-mysqlclient
           python-pymonad
           python-redis
           yoyo-migrations))
    (home-page "https://github.com/genenetwork/gn-auth")
    (synopsis "Authentication and Authorisation server for GeneNetwork services")
    (description "Authentication and Authorisation server for GeneNetwork services.")
    (license license:agpl3+)))

;; ./pre-inst-env guix download http://files.genenetwork.org/raw_database/db_webqtl_s.zip
;; 0sscjh0wml2lx0mb43vf4chg9gpbfi7abpjxb34n3kyny9ll557x

(define-public genenetwork2-files-small
  (let ((pfff "xx"))
    (package
      (name "genenetwork2-files-small")
      (version "1.0")
      (source
       (origin
         (method url-fetch)
         (uri "http://files.genenetwork.org/data_files/gn2_data_s-20160303-C9E672ECED1F51B915DE419B5B2C524E.tar.lz4")
         (file-name (string-append name "-" pfff))
         (sha256
          (base32 "058ymx3af6abdhdxyxj0i9qfvb6v7j091frjpp6jh4ahks7r23lj"))))
      (build-system trivial-build-system)
      (native-inputs `(("lz4" ,lz4)
                       ("tar" ,tar)
                       ("source" ,source)))

      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (let* ((out (assoc-ref %outputs "out"))
                (name "gn2_data_s")
                (tarfn (string-append name ".tar"))
                (targetdir (string-append out "/share/genenetwork2/")))
           (begin
             (use-modules (guix build utils))
             (let ((source (assoc-ref %build-inputs "source"))
                   (lz4unpack (string-append (assoc-ref %build-inputs "lz4") "/bin/lz4"))
                   (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar")))
               (and
                (zero? (system* lz4unpack source "-d" tarfn))
                (zero? (system* tar "xf" tarfn))
                (mkdir-p targetdir)
                (copy-recursively name targetdir)))))))
      (home-page "http://genenetwork.org/")
      (synopsis "Small file archive to run on genenetwork")
      (description "Genenetwork genotype and mapping files.")
      (license license:agpl3+))))

(define-public genenetwork2-database-small
  (let ((md5 "93e745e9c"))
    (package
    (name "genenetwork2-database-small")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri "http://files.genenetwork.org/raw_database/db_webqtl_s.zip")
       (file-name (string-append name "-" md5))
       (sha256
        (base32 "0sscjh0wml2lx0mb43vf4chg9gpbfi7abpjxb34n3kyny9ll557x"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))

    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
                   (and (mkdir "db")
                        (zero? (system* unzip source "-d" "db"))
                        (chdir "db"))))))
    (home-page "http://genenetwork.org/")
    (synopsis "Small database to run on genenetwork")
    (description "Genenetwork installation + database.")
    (license license:agpl3+))))



(define (genenetwork-graph)
  (with-imported-modules '((guix build utils))
    (gexp->derivation "genenetwork-graph"
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-1))

          (define (python-version package)
            (let* ((version     (last (string-split
                                        package
                                        #\-)))
                   (components  (string-split version #\.))
                   (major+minor (take components 2)))
              (string-join major+minor ".")))

          (let ((html-file (string-append #$output
                                          "/lib/python"
                                          (python-version #$python)
                                          "/site-packages/wqflask"
                                          "/dependency-graph.html")))
            (mkdir-p (dirname html-file))
            (call-with-output-file html-file
              (lambda (port)
                (format
                  port "~a"
                  #$(call-with-output-string
                     (lambda (p)
                       (with-output-to-port p
                         (lambda ()
                           (run-with-store
                             (open-connection)
                             (export-graph
                              (list genenetwork2)
                              p
                              #:node-type %package-node-type
                              #:backend %d3js-backend))))))))))))))

;(define (computed-genenetwork-graph)
;  (with-imported-modules '((guix build utils))
;    (computed-file "genenetwork-graph"
;      #~(begin
;          (use-modules (guix build utils))
;            (call-with-output-file #$output
;              (lambda (port)
;                (format
;                  port "~a"
;                  #$(call-with-output-string
;                      (lambda (p)
;                        (with-output-to-port p
;                          (lambda ()
;                            (run-with-store
;                              (open-connection)
;                              (export-graph
;                                (list genenetwork1)
;                                p
;                                #:node-type %package-node-type
;                                #:backend %d3js-backend)))))))))))))

(define (dag-svg-file)
  (with-imported-modules '((guix build utils))
    (gexp->derivation "dag-svg-file"
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-1))

        (define (python-version package)
          (let* ((version     (last (string-split
                                      package
                                      #\-)))
                 (components  (string-split version #\.))
                 (major+minor (take components 2)))
            (string-join major+minor ".")))

          (let* ((dest-dir (string-append #$output
                                          "/lib/python"
                                          (python-version #$python)
                                          "/site-packages/wqflask"))
                 (dot-file (string-append dest-dir "/dependency-graph.dot"))
                 (svg-file (string-append dest-dir "/dependency-graph.svg")))
            (mkdir-p dest-dir)
            (call-with-output-file dot-file
              (lambda (port)
                (format
                  port "~a"
                  #$(call-with-output-string
                     (lambda (p)
                       (with-output-to-port p
                         (lambda ()
                           (run-with-store
                             (open-connection)
                             (export-graph
                              (list genenetwork2)
                              p
                              #:node-type %package-node-type
                              #:backend %graphviz-backend)))))))))
            (invoke #+(file-append graphviz "/bin/dot")
                    "-Tsvg" "-o" svg-file dot-file))))))

;(define-public genenetwork2-combined
;  (directory-union
;    "genenetwork2"
;    (list genenetwork2 genenetwork-graph dag-svg-file)))
