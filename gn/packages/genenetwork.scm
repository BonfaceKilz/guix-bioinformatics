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
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xml)
  #:use-module (past packages python)
  #:use-module (past packages web)
  #:use-module (gn packages bioinformatics)
  #:use-module (gn packages twint)
  #:use-module (gn packages databases)
  #:use-module (gn packages elixir)
  #:use-module (gn packages gemma)
  #:use-module (gn packages golang)
  #:use-module (gn packages javascript)
  #:use-module (gn packages phewas)
  #:use-module (gn packages python)
  #:use-module (gn packages python24)
  #:use-module (gn packages statistics)
  #:use-module (gn packages web)
  #:use-module (srfi srfi-1))


(define-public python2-qtlreaper
  (let ((commit "442c217b90393380a8634ff8636b44992f5c53dd"))
  (package
    (name "python2-qtlreaper")
    (version (string-append "1.11-gn2-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   ;; (url "https://github.com/genenetwork/genenetwork2.git")
                   (url "https://github.com/pjotrp/QTLreaper.git")
                   (commit commit)))
             (file-name (string-append name "-" (string-take commit 7)))
             (sha256
              (base32
               "1rrbm1ap2zzyjxmrs9aa1d18sgiba5dhj1fmkl7wmab06jv3j1hm"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test' really!
    (home-page "http://qtlreaper.sourceforge.net/")
    (synopsis "Scan expression data for QTLs")
    (description
     "Batch-oriented version of WebQTL. It requires, as input,
expression data from members of a set of recombinant inbred lines and
genotype information for the same lines.  It searches for an
association between each expression trait and all genotypes and
evaluates that association by a permutation test.  For the permutation
test, it performs only as many permutations as are necessary to define
the empirical P-value to a reasonable precision. It also performs
bootstrap resampling to estimate the confidence region for the
location of a putative QTL.")
    (license license:gpl2+))))

(define-public python24-qtlreaper
  (let ((commit "442c217b90393380a8634ff8636b44992f5c53dd"))
    (package
      (name "python24-qtlreaper")
      (version (git-version "1.11" "gn1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       ;; (url "https://github.com/genenetwork/genenetwork2.git")
                       (url "https://github.com/pjotrp/QTLreaper.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1rrbm1ap2zzyjxmrs9aa1d18sgiba5dhj1fmkl7wmab06jv3j1hm"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2.4
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'make-max-markername-size-larger
             (lambda _
               (substitute* "Src/dataset.c"
                 (("512") "2048"))
               #t))
           (replace 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "test/runtest.py"))))))
      (native-inputs
       `(("python24-setuptools" ,python24-setuptools)))
      (home-page "http://qtlreaper.sourceforge.net/")
      (synopsis "Scan expression data for QTLs")
      (description
       "Batch-oriented version of WebQTL. It requires, as input,
expression data from members of a set of recombinant inbred lines and
genotype information for the same lines.  It searches for an
association between each expression trait and all genotypes and
evaluates that association by a permutation test.  For the permutation
test, it performs only as many permutations as are necessary to define
the empirical P-value to a reasonable precision. It also performs
bootstrap resampling to estimate the confidence region for the
location of a putative QTL.")
      (license license:gpl2+))))

;; Reintroduced python2-gunicorn because we are running GN with python2
;; right now. Please keep it until we migrate to Python3 fully!

(define-public python-gunicorn-gn
  (package
    (name "python-gunicorn-gn")
    (version "19.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "gunicorn" version))
        (sha256
         (base32
          "1wzlf4xmn6qjirh5w81l6i6kqjnab1n1qqkh7zsj1yb6gh4n49ps"))))
    (build-system python-build-system)
    (inputs
     `(("python-mock" ,python-mock)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-verion-restrictions
           (lambda _
             (substitute* "requirements_test.txt"
               (("coverage.*") "coverage\n")
               (("pytest.*") "pytest\n")
               (("pytest-cov.*") "pytest-cov\n"))
             #t)))))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://gunicorn.org")
    (synopsis "WSGI HTTP Server for UNIX")
    (description "Gunicorn 'Green Unicorn' is a Python WSGI HTTP Server for
UNIX.  It's a pre-fork worker model ported from Ruby's Unicorn project.  The
Gunicorn server is broadly compatible with various web frameworks, simply
implemented, light on server resource usage, and fairly speedy.")
    (license license:expat)))

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

(define-public gfautil
  (package
    (name "gfautil")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfautil" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0cgiis9v1nd4m7zxvgsz8jf8ijv4d8fa5wb7cpnjshksb8z7xh69"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-gfa" ,rust-gfa-0.6)
        ("rust-handlegraph" ,rust-handlegraph-0.3)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-structopt" ,rust-structopt-0.3))))
    (home-page "https://github.com/chfi/rs-gfa-utils")
    (synopsis "Command line tools for working with GFA files")
    (description
     "This package provides command line tools for working with @acronym{GFA,
Graphical Fragment Assembly} files and related formats.")
    (license license:expat)))

(define-public genenetwork3
  (let ((commit "487e50e8f8304a65e7af3759c13256f921efb8be"))
    (package
      (name "genenetwork3")
      (version (string-append "0.0.1-guix-" (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/genenetwork/genenetwork3.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rm8pvl5x1350aivk7m2symdsf3iiacl4pnaz1q8fgxpr154qw93"))))
      (propagated-inputs `(("coreutils" ,coreutils)
                           ("gemma-wrapper" ,gemma-wrapper)
                           ("csvdiff" ,go-github-com-aswinkarthik-csvdiff)
                           ("python" ,python-wrapper)
                           ("python-bcrypt" ,python-bcrypt)
                           ("python-flask" ,python-flask)
                           ("python-ipfshttpclient" ,python-ipfshttpclient)
                           ("python-mypy" ,python-mypy)
                           ("python-mypy-extensions" ,python-mypy-extensions)
                           ("python-mysqlclient" ,python-mysqlclient)
                           ("python-numpy" ,python-numpy)
                           ("python-pylint" ,python-pylint)
                           ("python-redis" ,python-redis)
                           ("python-requests" ,python-requests)
                           ("python-scipy" ,python-scipy)
                           ("python-sqlalchemy-stubs"
                            ,python-sqlalchemy-stubs)
                           ("r-optparse" ,r-optparse)
                           ("r-qtl" ,r-qtl)
                           ("r-stringi" ,r-stringi)))
      (build-system python-build-system)
      (home-page "https://github.com/genenetwork/genenetwork3")
      (synopsis "GeneNetwork3 API for data science and machine learning.")
      (description "GeneNetwork3 API for data science and machine learning.")
      (license license:agpl3+))))

(define-public genenetwork2
  (let ((commit "84cbf35adbb15c79638372d108308edb05f12683"))
    (package
      (inherit python2-genenetwork2)
      (name "genenetwork2")
      (version (string-append "3.11-guix-" (string-take commit 7) ))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/genenetwork/genenetwork2.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1402g129ghfh0xwfxjj1i7gbib2yl9rahf55caj7b1psy24ys87x"))))
      (native-inputs
       `(("graphviz" ,graphviz)))
      (propagated-inputs
       (let ((inputs (package-propagated-inputs python2-genenetwork2)))
         `(,@(fold
              alist-delete inputs
              (map car
                   (filter (lambda (x)
                             (let ((name (car x)))
                               (or (string-prefix? "python2" name)
                                   (string-prefix? "python-2" name)
                                   (string=? "python" name))))
                           inputs)))
           ("genenetwork3" ,genenetwork3)
           ("parallel" ,parallel) ;; GNU parallel
           ("python" ,python-wrapper)
           ("python-pillow" ,python-pillow)
           ("python-coverage" ,python-coverage)
           ("python-configparser" ,python-configparser) ;; maintenance/scripts
           ("python-flask" ,python-flask)
           ("gunicorn" ,gunicorn)
           ("python-autopep8" ,python-autopep8)
           ("python-cssselect" ,python-cssselect)
           ("python-elasticsearch" ,python-elasticsearch)
           ("python-htmlgen" ,python-htmlgen)
           ("python-jinja2" ,python-jinja2)
           ("python-sqlalchemy" ,python-sqlalchemy)
           ("python-flask-sqlalchemy" ,python-flask-sqlalchemy)
           ("python-setuptools" ,python-setuptools)
           ("python-scipy" ,python-scipy)
           ("python-lxml" ,python-lxml)
           ("python-mechanize" ,python-mechanize)
           ("python-mysqlclient" ,python-mysqlclient)
           ("python-mypy" ,python-mypy)
           ("python-numpy" ,python-numpy)
           ("python-pandas" ,python-pandas)
           ("python-parameterized" ,python-parameterized)
           ("python-passlib" ,python-passlib)
           ("python-pylint" ,python-pylint)
           ("python-redis" ,python-redis)
           ("python-requests" ,python-requests)
           ("python-simplejson" ,python-simplejson)
           ("python-pyyaml" ,python-pyyaml)
           ("python-markdown" ,python-markdown)
           ("python-rdflib" ,python-rdflib)
           ;; TODO: Get rid of Python R bindings
           ("python-rpy2" ,python-rpy2-next)
           ("python-beautifulsoup4" ,python-beautifulsoup4)
           ;; Disable for now. Build fails on Penguin2
           ;; ("python-flask-socketio" ,python-flask-socketio)
           ("python-xlsxwriter" ,python-xlsxwriter))))
      (arguments
       (let ((python (specification->package "python-wrapper"))
             (args (package-arguments python2-genenetwork2)))
         (substitute-keyword-arguments args
           ((#:python _) python)
           ((#:phases phases)
            `(modify-phases ,phases
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
               (add-after 'install 'generate-graph
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (call-with-output-file
                       (string-append
                        (assoc-ref outputs "out")
                        "/lib/python3.8/site-packages"
                        "/wqflask/dependency-graph.html")
                     (lambda (port)
                       (format
                        port "~a"
                        ,(call-with-output-string
                           (lambda (p)
                             (with-output-to-port p
                               (lambda ()
                                 (run-with-store
                                     (open-connection)
                                   (export-graph
                                    (list this-package)
                                    p
                                    #:node-type %package-node-type
                                    #:backend %d3js-backend)))))))))))
               (add-after 'install 'generate-dag-svg-file
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((output-dir
                           (string-append
                            (assoc-ref outputs "out")
                            "/lib/python3.8/site-packages/wqflask/"))
                          (dot-file
                           (string-append
                            output-dir
                            "dependency-graph.dot"))
                          (svg-file
                           (string-append
                            output-dir
                            "dependency-graph.svg")))
                     (begin
                       (call-with-output-file
                           dot-file
                         (lambda (port)
                           (format
                            port "~a"
                            ,(call-with-output-string
                               (lambda (p)
                                 (with-output-to-port p
                                   (lambda ()
                                     (run-with-store
                                         (open-connection)
                                       (export-graph
                                        (list this-package)
                                        p
                                        #:node-type %package-node-type
                                        #:backend %graphviz-backend)))))))))
                       (invoke "dot" "-Tsvg" "-o" svg-file dot-file)))))
               (add-after 'install 'generate-dependency-file
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (call-with-output-file
                       (string-append
                        (assoc-ref outputs "out")
                        "/lib/python3.8/site-packages"
                        "/wqflask/DEPENDENCIES.md")
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
                           (package-propagated-inputs this-package))))))))))))))))

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



(define-public python-reaper
  (let ((commit "63391333a6619771277bfffa9bd9d33811fa0d28"))
    (package
     (name "python-reaper")
     (version (string-append "0.0.1-"
                             (string-take commit 7)))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                      (url "https://github.com/fredmanglis/reaper.git")
                      (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1rq2qn0vrqd8k676yy8drm0zxzkj065ywhxjl0j1n2r25zifay7r"))))
     (build-system python-build-system)
     (arguments
      `(#:tests? #f))
     (home-page "https://github.com/fredmanglis/reaper")
     (synopsis "Parser for .geno files")
     (description "Parser for .geno files.  It replaces the Python2 library
written in C")
     (license license:agpl3+))))


(define-public genenetwork1
  (let ((commit "acf65ac9ae4be395c07c1629758f7408bf4eab5f") ; June 3, 2020
        (revision "2"))
    (package
      (name "genenetwork1")
      (version (git-version "0.0.0" revision commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/genenetwork/genenetwork1.git")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0xmmmjyvh80yd8b0cjrwpdmxl8k9zj5ly65r2g9aygx74njsp4fi"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("ghostscript" ,ghostscript)
         ("graphviz" ,graphviz)
         ("wget" ,wget)))
      (propagated-inputs
       `(("python" ,python-2.4)
         ("httpd-with-mod-python" ,httpd22-with-mod-python)
         ("python-direct" ,python24-direct)
         ("python-htmlgen-GN1" ,python24-htmlgen-GN1)
         ("python-json-GN1" ,python24-json-GN1)
         ("python-mysqlclient" ,python24-mysqlclient)
         ("python-numarray" ,python24-numarray)
         ("python-piddle" ,python24-piddle)
         ("python-pp-GN1" ,python24-pp-GN1)
         ("python-pyx" ,python24-pyx)
         ("python-pyxlwriter" ,python24-pyxlwriter)
         ("python-qtlreaper" ,python24-qtlreaper)
         ("python-rpy2" ,python24-rpy2)
         ("python-svg-GN1" ,python24-svg-GN1)))
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (add-after 'patch-generated-file-shebangs 'patch-more-files
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((piddle (assoc-ref inputs "python-piddle")))
                 (substitute* "web/webqtl/networkGraph/networkGraphUtils.py"
                   (("/usr/local/bin/neato") (which "neato"))
                   (("/usr/local/bin/circo") (which "circo"))
                   (("/usr/local/bin/twopi") (which "twopi"))
                   (("/usr/local/bin/fdp") (which "fdp"))
                   (("ps2pdf") (which "ps2pdf")))
                 (substitute* "web/webqtl/maintainance/addRif.py"
                   (("rm ") (string-append (which "rm") " "))
                   (("wget ") (string-append (which "wget") " "))
                   (("gunzip") (which "gunzip")))
                 (substitute* "web/webqtl/misc/editHtmlPage.py"
                   (("/bin/cp") (which "cp")))
                 (substitute* "web/webqtl/geneWiki/AddGeneRIFPage.py"
                   (("touch") (which "touch")))
                 (substitute* '("web/webqtl/maintainance/addRif.py"
                                "web/webqtl/networkGraph/networkGraphPage.py"
                                "web/webqtl/utility/svg.py")
                   (("/usr/bin/(env )?python") (which "python")))
                 (substitute* "web/webqtl/base/webqtlConfigLocal.py"
                   (("PythonPath.*")
                    (string-append "PythonPath = '" (which "python") "'\n"))
                   (("PIDDLE_FONT_PATH.*/lib")
                    (string-append "PIDDLE_FONT_PATH = '" piddle "/lib"))))
               #t))
           (add-after 'patch-generated-file-shebangs 'changes-for-deployed-service
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out    (assoc-ref outputs "out")))
                 (substitute* "web/webqtl/base/webqtlConfigLocal.py"
                   ;; Where GN1 is located: (GNROOT)
                   (("/gnshare/gn") out)
                   ;; Where the database is located: (sql_host)
                   (("tux01.uthsc.edu") "127.0.0.1"))
                   (substitute* '("web/webqtl/maintainance/QTL_Reaper_cal_lrs.py")
                     (("128\\.169\\.5\\.59") "localhost"))
                 ;; This directory (TMPDIR) is expected to be writable by apache.
                 ;; /tmp is private inside the container.
                 (symlink "/tmp" "web/tmp")
                 ;; IMGDIR is expected to be writable.
                 (symlink "/tmp" "web/image")
                 (system "chmod 0777 web/tmp")
                 ;; More writable locations:
                 (substitute* (list "web/webqtl/collection/ExportSelectionDetailInfoPage.py"
                                    "web/webqtl/pairScan/DirectPlotPage.py"
                                    "web/webqtl/updateTrait/DataUpdatePage.py"
                                    "web/webqtl/utility/Plot.py")
                   (("/gnshare/gn/web/debug_file.txt") "/tmp/debug_file.txt"))
                 ;; We mount the genotypes folder (GENODIR) in the OS-config and
                 ;; provide the symlink to that location from the package.
                 ;; And now the directory is magically available!
                 (symlink "/gnshare/gn/web/genotypes" "web/genotypes")
                 (substitute* "web/webqtl/base/webqtlConfig.py"
                   (("http://www.genenetwork.org") "http://gn1-test.genenetwork.org"))
                 ;; Inside the gn1 container, there's some conflict when
                 ;; importing the user module, therefore, as a hack, rename
                 ;; user to useralt
                 (mkdir "web/webqtl/useralt")
                 (copy-recursively "web/webqtl/user" "web/webqtl/useralt")
                 (substitute* '("web/webqtl/main.py")
                   (("from user import") "from useralt import"))
                 #t)))
           (add-after 'unpack 'use-local-links
             (lambda _
               (substitute* '("web/javascript/menu_items.js"
                              "web/webqtl/maintainance/updateMenuJS.py")
                 (("http://(www|gn1).genenetwork.org") ""))

               ;; Move this file out of the way while patching files.
               (rename-file "web/infoshare/manager/MDB-Free/index.html"
                            "web/infoshare/manager/MDB-Free/index.htm")
               (substitute* (cons*
                              "web/webqtl/base/indexBody.py"
                              "web/webqtl/submitTrait/BatchSubmitPage.py"
                              (find-files "web" "\\.html"))
                 ((".*base href.*") "")
                 (("(HREF|href)=\\\"http://(www.)?genenetwork.org")
                  "href=\""))
               ;; Move this file back to its original location.
               (rename-file "web/infoshare/manager/MDB-Free/index.htm"
                            "web/infoshare/manager/MDB-Free/index.html")

               (substitute* (cons*
                              "web/humanCross.html"
                              "web/webqtl/base/indexBody.py"
                              "web/whats_new.html"
                              (find-files "web/dbdoc" "\\.html"))
                 (("src=\\\"http://www.genenetwork.org") "src=\""))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (home-page "http://www.genenetwork.org/webqtl/main.py")
      (synopsis
       "Combined database and data analysis software resource for systems genetics")
      (description "GeneNetwork is a group of linked data sets and tools used to
study complex networks of genes, molecules, and higher order gene function and
phenotypes.  GeneNetwork combines more than 25 years of legacy data generated by
hundreds of scientists together with sequence data (SNPs) and massive
transcriptome data sets (expression genetic or eQTL data sets).  The
@dfn{quantitative trait locus} (QTL) mapping module that is built into GN is
optimized for fast on-line analysis of traits that are controlled by
combinations of gene
variants and environmental factors.  GeneNetwork can be used to study humans,
mice (BXD, AXB, LXS, etc.), rats (HXB), Drosophila, and plant species (barley
and Arabidopsis).  Most of these population data sets are linked with dense
genetic maps (genotypes) that can be used to locate the genetic modifiers that
cause differences in expression and phenotypes, including disease susceptibility.")
      (license license:agpl3+))))
