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
  (let ((commit "f52247c15f3694f3dd5fd0fd79c3e15376137e07"))
    (package
      (name "genenetwork3")
      (version (git-version "0.1.0" "3" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/genenetwork/genenetwork3.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0ac0dr8dny65x4xvm8gw6ap3g8g0j933ipy9116idcws31rk2adk"))))
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
  (let ((commit "bfe557dc1e537dc78a82a30817ecf2ca3004d978"))
    (package
      (name "genenetwork2")
      (version (git-version "3.11" "2" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/genenetwork/genenetwork2.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1bn0j0fpk4hcicgfird62x5wq2n6lj4rs1ggw69dcxyf4qdxbk5d"))))
      (native-inputs
       (list graphviz))
      (propagated-inputs
       `(("genenetwork3" ,genenetwork3)
         ("parallel" ,parallel) ;; GNU parallel
         ("coreutils" ,coreutils)
         ("git" ,git)
         ("which" ,which)
         ("grep" ,grep)
         ("r" ,r)
         ("r-ctl" ,r-ctl)
         ("r-qtl" ,r-qtl)
         ("r-wgcna" ,r-wgcna)
         ("redis" ,redis)
         ("mariadb" ,mariadb)
         ("gemma" ,gemma-gn2)
         ("gemma-wrapper" ,gemma-wrapper)
         ("plink-ng-gn" ,plink-ng-gn)
         ("rust-qtlreaper" ,rust-qtlreaper)
	 ("gn-rust-correlation" ,gn-rust-correlation)
         ("glibc-utf8-locales" ,glibc-utf8-locales)
         ("nginx" ,nginx)
         ("python" ,python-wrapper)
         ("python-pillow" ,python-pillow)
         ("python-coverage" ,python-coverage)
         ("python-configparser" ,python-configparser) ;; maintenance/scripts
         ("python-flask" ,python-flask)
         ("gunicorn" ,gunicorn)
         ("python-autopep8" ,python-autopep8)
         ("python-cssselect" ,python-cssselect)
         ("python-flask-debugtoolbar" ,python-flask-debugtoolbar)
         ("python-htmlgen" ,python-htmlgen)
         ("python-ijson" ,python-ijson)
         ("python-jinja2" ,python-jinja2)
         ("python-pytest" ,python-pytest)
         ("python-pytest-mock" ,python-pytest-mock)
         ("python-sqlalchemy" ,python-sqlalchemy)
         ("python-setuptools" ,python-setuptools)
         ("python-scipy" ,python-scipy)
         ("python-lxml" ,python-lxml)
         ("python-mysqlclient" ,python-mysqlclient)
         ("python-mypy" ,python-mypy)
         ("python-numpy" ,python-numpy)
         ("python-pandas" ,python-pandas)
         ("python-pylint" ,python-pylint)
         ("python-pymonad" ,python-pymonad)
         ("python-redis" ,python-redis)
         ("python-requests" ,python-requests)
         ("python-simplejson" ,python-simplejson)
         ("python-markdown" ,python-markdown)
         ("python-rdflib" ,python-rdflib)
	 ("python-authlib" ,python-authlib)
	 ("python-flask-session" ,python-flask-session)
         ;; TODO: Get rid of Python R bindings
         ("python-rpy2" ,python-rpy2)
         ("python-beautifulsoup4" ,python-beautifulsoup4)
         ;; Disable for now. Build fails on Penguin2
         ;; ("python-flask-socketio" ,python-flask-socketio)
         ("python-xlsxwriter" ,python-xlsxwriter)
         ;; All the external js dependencies
         ("javascript-twitter-post-fetcher" ,javascript-twitter-post-fetcher)
         ("javascript-cytoscape" ,javascript-cytoscape)
         ("javascript-panzoom" ,javascript-cytoscape-panzoom)
         ("javascript-qtip" ,javascript-cytoscape-qtip)
         ("javascript-chroma" ,javascript-chroma)
         ("javascript-d3-tip" ,javascript-d3-tip)
         ("javascript-jscolor" ,javascript-jscolor)
         ("javascript-colorbox" ,javascript-colorbox)
         ("javascript-jszip" ,javascript-jszip)
         ("js-jstat" ,js-jstat)
         ("js-md5" ,js-md5)
         ("js-parsley" ,js-parsley)
         ("javascript-plotly" ,javascript-plotly)
         ("javascript-typeahead" ,javascript-typeahead)
         ("js-underscore" ,js-underscore)
         ("js-smart-time-ago" ,js-smart-time-ago)
         ("javascript-nouislider" ,javascript-nouislider)
         ("javascript-purescript-genome-browser" ,javascript-purescript-genome-browser)
         ("javascript-ckeditor" ,javascript-ckeditor)
         ("javascript-datatables" ,javascript-datatables)
         ("javascript-datatables-scroller" ,javascript-datatables-scroller)
         ("javascript-datatables-buttons" ,javascript-datatables-buttons)
         ("javascript-datatables-buttons-bootstrap" ,javascript-datatables-buttons-bootstrap)
         ("javascript-datatables-plugins" ,javascript-datatables-plugins)
         ("javascript-datatables-col-reorder" ,javascript-datatables-col-reorder)
         ("javascript-datatables-col-resize" ,javascript-datatables-col-resize)
         ("javascript-datatables-buttons-styles" ,javascript-datatables-buttons-styles)
         ("javascript-shapiro-wilk" ,javascript-shapiro-wilk)
         ("javascript-underscore-string" ,javascript-underscore-string)
	 ("javascript-htmx" ,javascript-htmx)
         ("javascript-qtip2" ,javascript-qtip2)
         ("javascript-d3js" ,javascript-d3js)
         ("javascript-nvd3" ,javascript-nvd3)
         ("javascript-bootstrap" ,javascript-bootstrap)
         ("javascript-jquery" ,javascript-jquery)
         ("javascript-zxcvbn" ,javascript-zxcvbn)
         ("javascript-jquery-ui" ,javascript-jquery-ui)
         ("javascript-jquery-cookie" ,javascript-jquery-cookie)
         ("javascript-xterm" ,javascript-xterm)
         ("javascript-xterm-style" ,javascript-xterm-style)
         ("javascript-xterm-addon-fit",javascript-xterm-addon-fit)
         ("javascript-font-awesome" ,javascript-font-awesome)))
      (inputs
       `(("javascript-colorbox" ,(package-source javascript-colorbox))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f        ; no 'setup.py test'
         #:phases
         (modify-phases %standard-phases
            (delete 'reset-gzip-timestamps)
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
                  (("which") (which "which")))
                #t))
            ; (add-after 'unpack 'patch-javascript
            ;  (lambda* (#:key inputs #:allow-other-keys)
            ;    (let ((colorbox (assoc-ref inputs "javascript-colorbox"))
            ;          (gn2 "/share/genenetwork2/javascript/"))
            ;      (delete-file-recursively "wqflask/wqflask/static/packages/colorbox")
            ;      (copy-recursively colorbox "wqflask/wqflask/static/packages/colorbox")
            ;      #t)))
            (add-before 'install 'fix-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (let* (
                       ; (datafiles (string-append (assoc-ref inputs "genenetwork2-files-small") "/share/genenetwork2"))
                       ; (pylmmcmd (string-append (assoc-ref inputs "pylmm-gn2") "/bin/pylmm_redis"))
                       (plink2cmd (string-append (assoc-ref inputs "plink-ng-gn") "/bin/plink2"))
                       (gemmacmd (string-append (assoc-ref inputs "gemma") "/bin/gemma"))
                       )

                  (substitute* '("etc/default_settings.py")
                    ; (("^GENENETWORK_FILES +=.*") (string-append "GENENETWORK_FILES = \"" datafiles "\"\n" ))
                    ; (("^PYLMM_COMMAND =.*") (string-append "PYLMM_COMMAND = \"" pylmmcmd "\"\n" ))
                    (("^PLINK_COMMAND =.*") (string-append "PLINK_COMMAND = \"" plink2cmd "\"\n" ))
                    (("^GEMMA_COMMAND =.*") (string-append "GEMMA_COMMAND = \"" gemmacmd "\"\n" ))
                    )
                  )))
            ; (add-after 'install 'generate-graph
            ;  (lambda* (#:key inputs outputs #:allow-other-keys)
            ;    (call-with-output-file
            ;        (string-append
            ;         (assoc-ref outputs "out")
            ;         "/lib/python"
            ;         (python-version (assoc-ref inputs "python"))
            ;         "/site-packages"
            ;         "/wqflask/dependency-graph.html")
            ;      (lambda (port)
            ;        (format
            ;         port "~a"
            ;         ,(call-with-output-string
            ;            (lambda (p)
            ;              (with-output-to-port p
            ;                (lambda ()
            ;                  (run-with-store
            ;                      (open-connection)
            ;                    (export-graph
            ;                     (list this-package)
            ;                     p
            ;                     #:node-type %package-node-type
            ;                     #:backend %d3js-backend)))))))))))
            ;(add-after 'install 'generate-dag-svg-file
            ;  (lambda* (#:key inputs outputs #:allow-other-keys)
            ;    (let* ((output-dir
            ;            (string-append
            ;             (assoc-ref outputs "out")
            ;             "/lib/python"
            ;             (python-version (assoc-ref inputs "python"))
            ;             "/site-packages/wqflask/"))
            ;           (dot-file
            ;            (string-append
            ;             output-dir
            ;             "dependency-graph.dot"))
            ;           (svg-file
            ;            (string-append
            ;             output-dir
            ;             "dependency-graph.svg")))
            ;      (begin
            ;        (call-with-output-file
            ;            dot-file
            ;          (lambda (port)
            ;            (format
            ;             port "~a"
            ;             ,(call-with-output-string
            ;                (lambda (p)
            ;                  (with-output-to-port p
            ;                    (lambda ()
            ;                      (run-with-store
            ;                          (open-connection)
            ;                        (export-graph
            ;                         (list this-package)
            ;                         p
            ;                         #:node-type %package-node-type
            ;                         #:backend %graphviz-backend)))))))))
            ;        (invoke "dot" "-Tsvg" "-o" svg-file dot-file)))))

            ;; TODO: Use this to replace the two previous phases.
            ;(add-after 'install 'install-generated-files
            ;  (lambda* (#:key inputs outputs #:allow-other-keys)
            ;    (let ((output-dir
            ;           (string-append
            ;            (assoc-ref outputs "out")
            ;            "/lib/python"
            ;            (python-version (assoc-ref inputs "python"))
            ;            "/site-packages/wqflask/")))
            ;      (install-file (string-append %dag-svg-file "/dependency-graph.dot") output-dir)
            ;      (install-file (string-append %dag-svg-file "/dependency-graph.svg") output-dir)
            ;      (install-file (string-append %genenetwork-graph "/dependency-graph.html") output-dir)
            ;      #t)))

            #!
            (add-after 'install 'generate-dependency-file
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (call-with-output-file
                    (string-append
                     (assoc-ref outputs "out")
                     "/lib/python"
                     (python-version (assoc-ref inputs "python"))
                     "/site-packages"
                     "/wqflask/DEPENDENCIES.md"
                     )
                  (lambda (port)
                    (format
                     port "
#### System Inputs (generated from ~a package defined in ~a)
|Name | Description |
|-----|-------------|
~a
"
                     ,(package-name this-package)
                     (string-append
                      "[genenetwork.scm]"
                      "(http://git.genenetwork.org/"
                      "guix-bioinformatics/guix-bioinformatics"
                      "/src/branch/master/gn/packages/"
                      "genenetwork.scm)")
                     ,(apply
                       string-append
                       (map
                        (lambda (input)
                          (let* ((pkg (cadr input))
                                 (name (package-name pkg))
                                 (version (package-version pkg))
                                 (home-page (package-home-page pkg))
                                 (description (package-synopsis pkg)))
                            (string-append
                             "| **[" name "](" home-page ")** v"
                             version"| "
                             description " |\n")))
                        (package-propagated-inputs this-package))))))))
                        !# 
                        )))
      (home-page "http://genenetwork.org/")
      (synopsis "Full genenetwork services")
      (description "Genenetwork installation sumo.")
      (license license:agpl3+))))

(define-public gn-auth
  (package
    (name "gn-auth")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/genenetwork/gn-auth.git")
	     (commit "acd1a3d29900cff70ccc82dbe8ad1dc4b4c0fe8e")))
       (hash
	(content-hash
	 (base32
	  "19c0qshj511w4rhfbj5d9gvcscnbsjhk6xnr10ajjvr61w42qmq4")))))
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
     `(("python-mypy" ,python-mypy)
       ("python-pytest" ,python-pytest)
       ("python-pylint" ,python-pylint)
       ("python-hypothesis" ,python-hypothesis)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-mypy-extensions" ,python-mypy-extensions)))
    (propagated-inputs
     `(("gunicorn" ,gunicorn)
       ("python-flask" ,python-flask)
       ("python-redis" ,python-redis)
       ("python-authlib" ,python-authlib)
       ("python-pymonad" ,python-pymonad)
       ("yoyo-migrations" ,yoyo-migrations)
       ("python-bcrypt" ,python-bcrypt) ;; remove after removing all references
       ("python-mysqlclient" ,python-mysqlclient)
       ("python-argon2-cffi" ,python-argon2-cffi)
       ("python-email-validator" ,python-email-validator)))
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
