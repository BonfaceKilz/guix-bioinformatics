(define-module (gn packages python24)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (gn packages databases)
  #:use-module (gn packages python)
  #:use-module (past packages python)
  #:use-module (past packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python-xyz)
  ; #:use-module (gnu packages tls)
  #:use-module (srfi srfi-1))

(define (default-python2.4)
    "Return the default Python-2.4 package."
      ;; Lazily resolve the binding.
        (let ((python (resolve-interface '(past packages python))))
              (module-ref python 'python-2.4)))

;; We borrow this from (guix build-system python) since we cannot refer to it
;; with the magic '@@' symbol since Guix has switched to guile-3.0.
(define* (package-with-explicit-python python old-prefix new-prefix
                                       #:key variant-property)
  "Return a procedure of one argument, P.  The procedure creates a package with
the same fields as P, which is assumed to use PYTHON-BUILD-SYSTEM, such that
it is compiled with PYTHON instead.  The inputs are changed recursively
accordingly.  If the name of P starts with OLD-PREFIX, this is replaced by
NEW-PREFIX; otherwise, NEW-PREFIX is prepended to the name.

When VARIANT-PROPERTY is present, it is used as a key to search for
pre-defined variants of this transformation recorded in the 'properties' field
of packages.  The property value must be the promise of a package.  This is a
convenient way for package writers to force the transformation to use
pre-defined variants."
  (define package-variant
    (if variant-property
        (lambda (package)
          (assq-ref (package-properties package)
                    variant-property))
        (const #f)))

  (define (transform p)
    (cond
     ;; If VARIANT-PROPERTY is present, use that.
     ((package-variant p)
      => force)

     ;; Otherwise build the new package object graph.
     ((eq? (package-build-system p) python-build-system)
      (package
        (inherit p)
        (location (package-location p))
        (name (let ((name (package-name p)))
                (string-append new-prefix
                               (if (string-prefix? old-prefix name)
                                   (substring name
                                              (string-length old-prefix))
                                   name))))
        (arguments
         (let ((python (if (promise? python)
                           (force python)
                           python)))
           (ensure-keyword-arguments (package-arguments p)
                                     `(#:python ,python))))))
     (else p)))

  (define (cut? p)
    (or (not (eq? (package-build-system p) python-build-system))
        (package-variant p)))

  (package-mapping transform cut?))

(define package-with-python24
  (package-with-explicit-python (delay (default-python2.4))
                                "python-" "python24-"
                                #:variant-property 'python24-variant))

(define (strip-python24-variant p)
  (package
    (inherit p)
    (properties (alist-delete 'python24-variant (package-properties p)))))

(define-public python24-htmlgen
  (package
    (name "python24-htmlgen")
    (version "0.9")
    ;(version "0.99")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "htmlgen" version))
        (sha256
         (base32
          "14xzjgwdqgs1vs5mq7mg3w48snvgb77yywv64mg8k6qhapmnafdw"))))
          ;"1kbn6jcbf2mpb9f8hm5gcsipy7habqrq4794lpdbzm5mqxlclmnl"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4))
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)))
    (propagated-inputs
     `(("python24-asserts" ,python24-asserts)))
    (home-page "https://github.com/srittau/python-htmlgen")
    (synopsis "Python HTML 5 Generator")
    (description "Python-htmlgen is a library to generate HTML from classes.")
    (license license:expat)))

(define-public python24-asserts
  (package
    (name "python24-asserts")
    (version "0.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "asserts" version))
        (sha256
         (base32
          "05ffy111giwv6sqx97vzzsvcra0gxzx2ilv16gyw135v583frxbn"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4))
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)))
    (home-page "https://github.com/srittau/python-asserts")
    (synopsis "Stand-alone Assertions for Python")
    (description "Stand-alone Assertions for Python")
    (license license:expat)))

(define-public python24-pil
  (package
    (inherit python2-pil1)
    (name "python24-pil")
    (arguments
     (substitute-keyword-arguments (package-arguments python2-pil1)
       ((#:python _) python-2.4)))))

(define-public python24-piddle
  (package
    (inherit python2-piddle-gn)
    (name "python24-piddle")
    (arguments
     (substitute-keyword-arguments (package-arguments python2-piddle-gn)
       ((#:python _) python-2.4)))
    (native-inputs `(("python24-setuptools" ,python24-setuptools)))
    (propagated-inputs
     `(("python24-pil" ,python24-pil)))))

;; Apparently this is the library which mimics python-2.6+'s json library
(define-public python24-simplejson
  (let ((base (package-with-python24 python-simplejson)))
    (package
      (inherit base)
      (version "2.0.9") ; last version to officially support python2.4
      (source
        (origin
          (method url-fetch)
          (uri (pypi-uri "simplejson" version))
          (sha256
           (base32
            "1vlkxibal9ljabybawnqr3jh6f6g21c5pbrzl65z9vwbfxhg9kdb"))))
      (native-inputs
       `(("python24-setuptools" ,python24-setuptools)
         ,@(package-native-inputs base))))))

(define-public python24-parallel
  (package
    (inherit python-parallel)
    (name "python24-parallel")
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:tests? #f))))  ; no tests

(define GN1-thirdparty-sources
  (origin
    (method url-fetch/tarbomb)
    (uri "http://ipfs.genenetwork.org/ipfs/QmTPwYT2pehdxdG1TiHEzVzLgbeuhJ4utXShuz3twA84AB/thirdparty.tgz")
    (file-name "GN1-thirdparty")
    (sha256
     (base32
      "0nnp6g412hjfrcn3k2yrfb14sxv06k0149whc7qmv678nyj5zhfa"))))

(define-public python24-json-GN1
  (package
    (name "python24-json-GN1")
    (version "GN1")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/json/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/json" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public python24-svg-GN1
  (package
    (name "python24-svg-GN1")
    (version "1.0")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/svg/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/svg" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-4)))

(define-public python24-htmlgen-GN1
  (package
    (name "python24-htmlgen-GN1")
    (version "2.5")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/htmlgen/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/htmlgen" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-2))) ; I'm not actually sure, checked HTMLgen.py

(define-public python24-pyx-GN1
  (package
    (name "python24-pyx-GN1")
    (version "0.8")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/pyx/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/pyx" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl2+)))

(define-public python24-pyxlwriter-GN1
  (package
    (name "python24-pyxlwriter-GN1")
    (version "0.4a3")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/pyXLWriter/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/pyXLWriter" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public python24-pp-GN1
  (package
    (name "python24-pp-GN1")
    (version "1.5.7")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "thirdparty/pp-1.5.7") #t)))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; 1.2.3 exactly. There was an API change between 1.2.3 and 1.2.5.
;; https://stackoverflow.com/questions/21740359/python-mysqldb-typeerror-not-all-arguments-converted-during-string-formatting/53563287#53563287
(define-public python24-mysqlclient
  (package
    (name "python24-mysqlclient")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "MySQL-python" version))
        (sha256
         (base32
          "0vkyg9dmj29hzk7fy77f42p7bfj28skyzsjsjry4wqr3z6xnzrkx"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs tests? #:allow-other-keys)
             (if tests?
               (begin
                 (mkdir-p "/tmp/mysqld")
                 (call-with-output-file "/tmp/my.cnf"
                   (lambda (p)
                     (format p
                             "[mysqld]~@
                             datadir = /tmp/mysqld~@
                             port = 3306~@
                             user = nixbld~@
                             #character-set-server = utf8mb4~@
                             socket = /tmp/mysqld/mysql.sock~%")))
                 (setenv "TESTDB" "/tmp/my.cnf")
                 ;; mysql-5.0 puts mysqld in libexec
                 (setenv "PATH" (string-append (getenv "PATH") ":"
                                               (assoc-ref inputs "mysql") "/libexec"))
                 (system "mysqld --defaults-file=/tmp/my.cnf &")
                 (sleep 5)
                 (invoke "mysqladmin" "-S" "/tmp/mysqld/mysql.sock" "variables")
                 (invoke "mysql" "-S" "/tmp/mysqld/mysql.sock"
                         "-e" "'create database mysqldb_test charset utf8;'"))
               #t))))
       #:tests? #f))    ; TODO: Run the test suite
    (native-inputs
     `(("mysql" ,mysql-5.0) ; Best supported version according to the README.
       ("python-nose" ,python24-nose)
       ("python-setuptools" ,python24-setuptools)))
    (inputs
     `(("openssl" ,openssl-1.0)
       ("zlib" ,zlib)))
    (home-page "http://mysql-python.sourceforge.net/")
    (synopsis "Python interface to MySQL")
    (description "MySQLdb is an interface to the popular MySQL database server
for Python.  The design goals are:
@itemize
@item with Python database API version 2.0
@item Thread-safety
@item Thread-friendliness (threads will not block each other)
@item Compatibility with MySQL-3.23 and later
@end itemize")
    (license license:gpl2+)))

;; It seems this isn't the correct DIRECT binary
(define-public python24-direct
  (package
    (name "python24-direct")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "DIRECT" version))
        (sha256
         (base32
          "1d4cf9hynlr42hq0k8w7j4qi45rkrgil332sh0hymklxgmyi21h5"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; Files are autogenerated using f2py.
            (delete-file "src/direct-f2pywrappers.f")
            (delete-file "src/directmodule.c")
            #t))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "DIRECT/__init__.py"
               (("\\.direct") "direct"))
             #t))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "test"
                 (invoke "python" "test_direct.py")))
             #t))
         (add-after 'install 'copy-library
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion
               (string-append (assoc-ref outputs "out")
                              "/lib/python2.4/site-packages")
               (copy-file "DIRECT/direct.so" "direct.so"))
             #t))
         (replace 'install-license-files
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "COPYRIGHT"
                           (string-append (assoc-ref outputs "out")
                                          "/share/doc/" ,name "-" ,version))
             #t)))))
    (propagated-inputs
     `(("python24-numpy" ,python24-numpy-1.2)))
    (inputs
     `(("gfortran" ,gfortran)))
    (native-inputs
     `(("python24-nose" ,python24-nose)
       ("python24-setuptools" ,python24-setuptools)))
    ;; Also seems to be here: https://github.com/amitibo/pydirect
    (home-page "http://code.google.com/p/pydirect/")
    (synopsis "Python wrapper to the DIRECT algorithm")
    (description
     "DIRECT is a method to solve global bound constraint optimization problems
and was originally developed by D. R. Jones, C. D. Perttunen and B. E. Stuckmann.
@code{pydirect} is a python wrapper around DIRECT.  It enables using DIRECT from
the comfort of the great Python scripting language.
The @code{pydirect} package uses the fortan implementation of DIRECT written by
Joerg.M.Gablonsky, DIRECT Version 2.0.4.  More information on the DIRECT
algorithm can be found in Gablonsky's
@url{http://repository.lib.ncsu.edu/ir/bitstream/1840.16/3920/1/etd.pdf,
thesis}.")
    (license license:expat)))

; env IPFS_PATH=/export/ipfs/ ipfs add direct.so
; added QmYUZiuAP6DJeubu69JqvRWSsn53qCZCS3FkRWgTowtWkA direct.so
; penguin2:~/tmp$ env IPFS_PATH=/export/ipfs/ ipfs pin add QmYUZiuAP6DJeubu69JqvRWSsn53qCZCS3FkRWgTowtWkA
; pinned QmYUZiuAP6DJeubu69JqvRWSsn53qCZCS3FkRWgTowtWkA recursively

(define-public python24-direct-gn
  (package
    (name "python24-direct-gn")
    (version "GN")
    (source (origin
              (method url-fetch)
              (uri "http://ipfs.genenetwork.org/ipfs/QmYUZiuAP6DJeubu69JqvRWSsn53qCZCS3FkRWgTowtWkA")
              (file-name "direct.so")
              (sha256
               (base32
                "0kj11dbi25k0wvyxxsylx7dsc7wm7rja799fymklkdd8h561la4i"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("direct.so" "lib/python2.4/site-packages/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-file (assoc-ref inputs "source") "direct.so")
             #t))
         (add-after 'unpack 'patchelf
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Writable so we can use patchelf, executable so it is useful.
             ;; After installation the writable bit is stripped off.
             (chmod "direct.so" #o777)
             (invoke "patchelf"
                     "--set-rpath"
                     (string-append (assoc-ref inputs "gcc:lib") "/lib")
                     "direct.so")
             #t)))))
    (inputs
     `(("gcc:lib" ,gcc "lib")))
    (native-inputs
     `(("patchelf" ,patchelf)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))
