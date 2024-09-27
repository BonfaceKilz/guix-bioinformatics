;; Bioinformatics module

(define-module (gn packages genenetwork)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git)
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
  #:use-module (gn packages node)
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
  (let ((commit "21d6aab173c949447aad3a72b3118d96d78d2183")
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
           "1g9xd6dm18ir1gd8z4pnlycjx9vn98d7qcvmk6i9801n8zb76i87"))))
      (inputs
       (list python-click))
      (native-inputs
       (list python-hypothesis
             python-mypy
             python-mypy-extensions
             python-pylint))
      (propagated-inputs
       (list csvdiff
             diffutils
             gemma-wrapper
             gn-rust-correlation
             python-argon2-cffi
             python-authlib
             python-bcrypt ;; Replace use of bcrypt with argon
             python-biopython
             python-email-validator
             python-flask
             python-flask-cors
             python-lmdb
             python-mysqlclient
             python-numpy
             python-pandas
             python-pingouin-without-tests
             python-plotly
             python-pyld
             python-pymonad
             python-pytest
             python-pytest-mock
             python-redis
             python-requests
             python-scikit-learn
             python-scipy
             python-sparqlwrapper
             python-wrapper
             python-xapian-bindings
             r
             r-ctl
             r-optparse
             r-qtl
             r-rjson
             r-stringi
             r-wgcna
             rust-qtlreaper))
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
  (let ((commit "ae572dfe805defc4d40d173325178f5b08c080bf")
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
                  "0d93p4wpc5icikq395975y4bcw95j17x5m5f91r4gxrf8v79s0kb"))))
      (native-inputs
       (list graphviz))
      (propagated-inputs
       (list genenetwork3
             coreutils
             gemma
             gemma-wrapper
             git
             glibc-utf8-locales
             gn-rust-correlation
             grep
             gunicorn
	     javascript-ace
             javascript-bootstrap
             javascript-chroma
             javascript-ckeditor
             javascript-colorbox
             javascript-cytoscape
             javascript-cytoscape-panzoom
             javascript-cytoscape-qtip
             javascript-d3-tip
             javascript-d3js ;; very old
             javascript-d3js-7
             javascript-d3panels
             javascript-datatables
             javascript-datatables-buttons
             javascript-datatables-buttons-bootstrap
             javascript-datatables-buttons-styles
             javascript-datatables-col-reorder
             javascript-datatables-col-resize
             javascript-datatables-plugins
             javascript-datatables-scroller
	     javascript-diff
	     javascript-diff2html
	     javascript-diff2html-ui
             javascript-font-awesome
             javascript-htmx
	     javascript-highlight
	     javascript-highlight-ui
             javascript-jquery
             javascript-jquery-cookie
             javascript-jquery-ui
             javascript-jscolor
             javascript-jszip
             javascript-linkify
             javascript-linkify-html
	     javascript-marked
	     javascript-marked-highlight
             javascript-nouislider
             javascript-nvd3
             javascript-plotly
             javascript-purescript-genome-browser
             javascript-qtip2
             javascript-shapiro-wilk
             javascript-twitter-post-fetcher
             javascript-typeahead
             javascript-uikit
             javascript-underscore-string
             javascript-zxcvbn
             js-jstat
             js-md5
             js-parsley
             js-smart-time-ago
             js-underscore
             mariadb
             nginx
             parallel
             plink-ng-gn
             python-authlib
             python-autopep8
             python-beautifulsoup4
             python-configparser
             python-coverage
             python-cssselect
             python-flask
             python-flask-debugtoolbar
             python-flask-session
             python-htmlgen
             python-ijson
             python-jinja2
             python-lxml
             python-markdown
             python-mypy
             python-mysqlclient
             python-numpy
             python-pandas
             python-pillow
             python-pylint
             python-pymonad
             python-pytest
             python-pytest-mock
             python-rdflib
             python-redis
             python-requests
             python-rpy2
             python-scipy
             python-setuptools
             python-simplejson
             python-sqlalchemy
             python-wrapper
             python-xlsxwriter
             r
             r-ctl
             r-qtl
             r-wgcna
             redis
             rust-qtlreaper
             which
             ; javascript-xterm -- disabled until we know what to do with it, not working on production
             ; javascript-xterm-style
             ; javascript-xterm-addon-fit
             ))
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
                                       "/gn2/wqflask/static/gif/error")))
		     (substitute* "gn2/wqflask/app_errors.py"
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

(define-public genenetwork3-stable
  (let ((commit "e5569c3bb1c0d59ff7142273c5f51fe19f06cfe8")
        (revision "1"))
    (package
     (inherit genenetwork3)
     (name "genenetwork3-stable")
     (version (string-append (git-version "3.11" revision commit)))
     (arguments
       (list
         #:tests? #f
         #:phases
           #~(modify-phases %standard-phases
             (add-before 'build 'update-paths
               (lambda _
               (for-each (lambda (fn)
                  (substitute* (string-append "gn3/" fn)
                     (("scripts/")
                      (string-append #$output "/scripts/"))))
                      '("api/rqtl.py"
                        "computations/wgcna.py"
                        "computations/ctl.py"
                        "api/general.py"))))
             (add-before 'install 'install-scripts
               (lambda _
                 (begin
                  (mkdir (string-append #$output "scripts"))
                  (for-each (lambda (fn)
                    (install-file fn
                      (string-append #$output "/scripts")))
                      '("scripts/rqtl_wrapper.R"
                        "scripts/ctl_analysis.R"
                        "scripts/wgcna_analysis.R"
                        ))))))))

     (source
      (git-checkout
       (url "https://github.com/genenetwork/genenetwork3")
       (branch "prod"))))))

(define-public genenetwork2-stable
  (let ((commit "95e634ca90d52922812b93df162686b348288651")
        (revision "1"))
    (package
     (inherit genenetwork2)
     (name "genenetwork2-stable")
     (version (string-append "stable-" (git-version "3.12" revision commit)))
     (source
      (git-checkout
       (url "https://github.com/genenetwork/genenetwork2")
       (branch "prod")))
     (arguments
       (list
         #:tests? #f
         #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'update-font-paths
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (for-each (lambda (fn)
                       (substitute* (string-append "gn2/" fn)
                                    (("\\./gn2/wqflask/static/fonts/")
                                     (string-append (site-packages inputs outputs) "/gn2/wqflask/static/fonts/"))))
                     '("utility/Plot.py"
                       "wqflask/marker_regression/display_mapping_results.py"))))
     ))))))

(define-public gn-uploader
  (let ((commit "60fde66e02dba842b20fa126ff3b2ed9ec2638e6")
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
         "1q04viyf7d0q30k3424hrzsh9wxhhgs7hywlhzl3m68jki4zq1i7")))))
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
            python-authlib
            python-pymonad
            python-jsonpickle
            python-mysqlclient
            python-flask-session
            javascript-jquery
            javascript-bootstrap
            node-resumablejs))
     (synopsis "GeneNetwork Quality Control Application")
     (description
      "gn-uploader is a service allowing upload of new data into GeneNetwork,
 that does quality control for the data files that is being uploaded to ensure
 it fulfils all conditions before it can be accepted.")
     (home-page "https://git.genenetwork.org/gn-uploader")
     (license license:agpl3+))))

(define-public gn-auth
  (let ((commit "12edc160df0ead9ac1ae4e62a44d49582e063021")
        (revision "01"))
    (package
      (name "gn-auth")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.genenetwork.org/gn-auth")
               (commit commit)))
         (hash
          (content-hash
           (base32
            "11kixqv3rwfaad43bcqyh085gb0jfcpvzrlx66grkk73cqydgyzl")))))
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
             python-blinker
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
      (license license:agpl3+))))

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
