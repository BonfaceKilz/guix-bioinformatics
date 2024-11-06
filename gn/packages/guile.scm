(define-module (gn packages guile)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system gnu)
  #:use-module ((gnu packages autotools) #:select (libltdl autoconf automake libtool))
  #:use-module ((gnu packages databases) #:select (mariadb))
  #:use-module ((gnu packages compression) #:select (zlib))
  #:use-module ((gnu packages perl) #:select (perl))
  #:use-module ((gnu packages texinfo) #:select (texinfo))
  #:use-module (guix utils)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages package-management)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages bioinformatics) #:select (gemma))
  #:use-module ((gnu packages certs) #:select (nss-certs))
  #:use-module ((gnu packages guile) #:select (guile-json-4 guile-3.0 guile-2.2 guile-readline))
  #:use-module ((gnu packages guile-xyz) #:select (guile-dbi guile-dbd-mysql guile-fibers guile-redis guile-hashing guile-commonmark))
  #:use-module ((gnu packages parallel) #:select (parallel))
  #:use-module ((gnu packages perl) #:select (perl))
  #:use-module ((gnu packages tls) #:select (guile-gnutls openssl)))

;; Lifted from:
;; https://lists.gnu.org/archive/html/artanis/2023-03/txtNk7zz7rJAN.txt
(define-public guile3-dbi
  (package
    (name "guile3-dbi")
    (version "2.1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/guile-dbi")
                    (commit (string-append "guile-dbi-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "123m4j82bi60s1v95pjh4djb7bh6zdwmljbpyg7zq8ni2gyal7lw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%gnu-build-system-modules)
       #:configure-flags
       (list (string-append
              "--with-guile-site-dir=" %output "/share/guile/site/"
              (target-guile-effective-version (assoc-ref %build-inputs "guile"))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             ;; The upstream Git repository contains all the code, so change
             ;; to the directory specific to guile-dbi.
             (chdir "guile-dbi")))
         (add-after 'install 'patch-extension-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dbi.scm (string-append out "/share/guile/site/"
                                            (target-guile-effective-version
                                             (assoc-ref inputs "guile"))
                                            "/dbi/dbi.scm"))
                    (ext (string-append out "/lib/libguile-dbi")))
               (substitute* dbi.scm (("libguile-dbi") ext))))))))
    (inputs
     (list libltdl))
    (native-inputs
     (list autoconf automake libtool perl texinfo guile-3.0))
    (synopsis "Guile database abstraction layer")
    (home-page "https://github.com/opencog/guile-dbi")
    (description
     "guile-dbi is a library for Guile that provides a convenient interface to
SQL databases.  Database programming with guile-dbi is generic in that the same
programming interface is presented regardless of which database system is used.
It currently supports MySQL, Postgres and SQLite3.")
    (license license:gpl2+)
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_DBD_PATH")
            (files '("lib")))))))

(define-public guile3-dbd-mysql
  (package
    (inherit guile3-dbi)
    (name "guile3-dbd-mysql")
    (arguments
     (substitute-keyword-arguments (package-arguments guile3-dbi)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'chdir
             (lambda _
               ;; The upstream Git repository contains all the code, so change
               ;; to the directory specific to guile-dbd-mysql.
               (chdir "guile-dbd-mysql")))
           (add-after 'chdir 'patch-src
             (lambda _
               (substitute* "configure.ac"
                 (("mariadbclient") "mariadb"))
               (substitute* "src/guile-dbd-mysql.c"
                 (("<mariadb/") "<mysql/"))))
           (delete 'patch-extension-path)))))
    (inputs
     (modify-inputs (package-inputs guile3-dbi)
       (prepend `(,mariadb "dev")
                `(,mariadb "lib")
                zlib)))
    (native-inputs
     (modify-inputs (package-native-inputs guile3-dbi)
       (prepend guile3-dbi		; only required for headers
                )))
    (synopsis "Guile DBI driver for MySQL")
    (description "@code{guile-dbi} is a library for Guile that provides a
convenient interface to SQL databases.  This package implements the interface
for MySQL.")
    (license license:gpl2+)))

(define-public gn-guile
  (let ((commit "4623225b0adb0846a4c2e879a33b31884d2e5f05")
	(revision "0"))
    (package
      (name "gn-guile")
      (version (git-version "4.0.0" revision commit))
      (source (origin
		(method git-fetch)
		(uri (git-reference
		      (url "https://git.genenetwork.org/gn-guile/")
		      (commit commit)))
		(file-name (string-append name "-" version))
		(sha256
		 (base32 "1l8q2yi8dqjglv54v2d6yqd498yabhhz193i25kwxwsk6cv9xkyz"))))
      (build-system guile-build-system)
      (propagated-inputs
       (list guile-3.0 guile3-dbi guile3-dbd-mysql guile-fibers guile-gnutls guile-readline
	     guile-commonmark guile-redis openssl nss-certs gemma parallel guile-hashing))
      (arguments
       (list
	#:phases
	#~(modify-phases %standard-phases
	    ;; When using the guile-build-system, guild doesn't
	    ;; correctly set the GUILE_LOAD_PATH for the various guile
	    ;; packages in the build phase leading to build failures.
	    (add-before 'build 'augment-GUILE_LOAD_PATH
	      (lambda* (#:key outputs #:allow-other-keys)
		(let* ((guile-version (target-guile-effective-version)))
		  ;; guild uses this: "\\.(scm|sls)$" regexp to try and
		  ;; compile all scm files in this repository.  We don't
		  ;; need to compile guix.scm and manifest.scm.
		  (delete-file "guix.scm")
		  (delete-file "manifest.scm")
		  (setenv "GUILE_LOAD_PATH"
			  (string-append
			   (format
			    #f "~{~a:~}"
			    (map (lambda (package)
				   (format #f "~a/share/guile/site/~a"
					   package guile-version))
				 (list #$guile3-dbi #$guile-fibers #$guile-commonmark #$guile-json-4 #$guile-hashing)))
			   #$(getenv "GUILE_LOAD_PATH")))))))))
      (home-page "https://git.genenetwork.com/gn-guile")
      (synopsis "Next generation GN code in guile")
      (description "Use of guile.")
      (license license:gpl3))))
