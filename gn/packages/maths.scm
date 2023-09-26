(define-module (gn packages maths)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages texinfo)
  #:use-module (gn packages gnulib)
  #:use-module (srfi srfi-1))

(define-public octave-3.4.3
  (package
    (inherit octave-cli)
    (name "octave")
    (version "3.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://gnu/octave/octave-"
                            version ".tar.gz"))
        (sha256
         (base32
          "04lh3crzwpramvfvxq34n2r29pmfl37rssj853nz9a3pyq7wrrir"))
        (patches (search-patches "gnulib-gets.patch"
                                 "octave-nested-class.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments octave-cli)
       ((#:configure-flags cf)
        `(cons "--enable-docs=no" ; docs fail to build
               ,cf))
       ((#:tests? _ #f) #f) ; tests hang
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-configure-script
             (lambda _
               (substitute* '("configure"
                              "src/DLD-FUNCTIONS/__delaunayn__.cc"
                              "src/DLD-FUNCTIONS/__voronoi__.cc"
                              "src/DLD-FUNCTIONS/convhulln.cc")
                 (("qhull/qhull.h") "libqhull/libqhull.h")
                 (("qhull/qhull_a.h") "libqhull/qhull_a.h"))
               #t))
           (add-after 'unpack 'update-gnulib
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((gnulib (assoc-ref inputs "gnulib")))
                 (install-file (string-append gnulib "/lib/fseeko.c") "libgnu")
                 #t)))
           (replace 'configure-makeinfo
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/help.cc"
                 (("\"makeinfo\"")
                  (string-append
                    "\"" (assoc-ref inputs "texinfo") "/bin/makeinfo\"")))
               #t))))))
    (native-inputs
     `(("gnulib" ,(package-source gnulib))
       ("gcc-5" ,gcc-5)
       ,@(fold alist-delete (package-native-inputs octave-cli)
               '("lzip"))))
    (inputs
     `(("glpk" ,glpk-4.48)
       ("gperf" ,gperf)
       ,@(fold alist-delete (package-inputs octave-cli)
               ;; suitesparse provides cholmod_common_struct, may need older version
               '("glpk" "suitesparse"))))))

;; 4.49 is the last version with _glp_lpx_simplex exported
;; 4.49 is the version where all lpx_ routines were removed
(define-public glpk-4.48
  (package
    (inherit glpk)
    (version "4.48")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/glpk/glpk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1cddqsdcfwavdklg7hsfifppsry81dx3c17wzk6r22mjjpwcihmb"))))))

(define-public suitesparse-3.5.0
  (package
    (inherit suitesparse)
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://faculty.cse.tamu.edu/davis/SuiteSparse/SuiteSparse-"
             version ".tar.gz"))
       (sha256
        (base32
         "0npn7c1j5qag5m2r0cmh3bwc42c1jk8k2yg2cfyxlcrp0h7wn4rc"))))
    (arguments
     (substitute-keyword-arguments (package-arguments suitesparse)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'unpack-metis
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((metis (assoc-ref inputs "metis-source")))
                 (invoke "tar" "xvf" metis)
                 ;; backported from 4.0.3
                 (substitute* (find-files "metis-4.0")
                              (("log2") "ilog2")))
               #t))
           (add-after 'unpack 'fix-source
             (lambda _
               (substitute* "UFconfig/Makefile"
                 (("Lib/") ""))
               ;; octave-3.4.3 expects it to be built with -fPIC
               (substitute* "UFconfig/UFconfig.mk"
                 (("-O3") "-O3 -fPIC"))
               #t))
           (add-before 'install 'prepare-directories
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (mkdir-p (string-append out "/lib"))
                 (mkdir-p (string-append out "/include")))
               #t))))))
    (native-inputs
     `(("metis-source" ,(origin
                          (method url-fetch)
                          (uri "http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/OLD/metis-4.0.1.tar.gz")
                          (sha256
                           (base32
                            "0lnkdfdrmmyy67h356bgdc06acvmcr26av9kdvqlws12znrr5iv0"))))
       ,@(package-native-inputs suitesparse)))
    (inputs
     `(,@(fold alist-delete (package-inputs suitesparse)
               '("metis"))))))

(define-public gsl-x86-64-v2
  (package/inherit gsl
    (name "gsl-x86-64-v2")
    (arguments
     (substitute-keyword-arguments (package-arguments gsl)
       ((#:make-flags flags #~'())
        #~(append (list "CFLAGS=-march=x86-64-v2"
                        "CXXFLAGS=-march=x86-64-v2")
                  #$flags))
       ((#:configure-flags flags #~'())
        #~(append (list (string-append "--libdir=" #$output
                                       "/lib/glibc-hwcaps/x86-64-v2"))
                  #$flags))
       ;; The building machine can't necessarily run the code produced.
       ((#:tests? _ #t) #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-extra-files
              (lambda _
                (delete-file-recursively (string-append #$output "/bin"))
                (delete-file-recursively (string-append #$output "/include"))))))))
    (supported-systems '("x86_64-linux"))
    (properties `((hidden? . #t)
                  (tunable? . #f)))))

(define-public gsl-x86-64-v3
  (package/inherit gsl
    (name "gsl-x86-64-v3")
    (arguments
     (substitute-keyword-arguments (package-arguments gsl)
       ((#:make-flags flags #~'())
        #~(append (list "CFLAGS=-march=x86-64-v3"
                        "CXXFLAGS=-march=x86-64-v3")
                  #$flags))
       ((#:configure-flags flags #~'())
        #~(append (list (string-append "--libdir=" #$output
                                       "/lib/glibc-hwcaps/x86-64-v3"))
                  #$flags))
       ;; The building machine can't necessarily run the code produced.
       ((#:tests? _ #t) #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-extra-files
              (lambda _
                (delete-file-recursively (string-append #$output "/bin"))
                (delete-file-recursively (string-append #$output "/include"))))))))
    (supported-systems '("x86_64-linux"))
    (properties `((hidden? . #t)
                  (tunable? . #f)))))

(define-public gsl-x86-64-v4
  (package/inherit gsl
    (name "gsl-x86-64-v4")
    (outputs '("out" "static"))
    (arguments
     (substitute-keyword-arguments (package-arguments gsl)
       ((#:make-flags flags #~'())
        #~(append (list "CFLAGS=-march=x86-64-v4"
                        "CXXFLAGS=-march=x86-64-v4")
                  #$flags))
       ((#:configure-flags flags #~'())
        #~(append (list (string-append "--libdir=" #$output
                                       "/lib/glibc-hwcaps/x86-64-v4"))
                  #$flags))
       ;; The building machine can't necessarily run the code produced.
       ((#:tests? _ #t) #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-extra-files
              (lambda _
                (delete-file-recursively (string-append #$output "/bin"))
                (delete-file-recursively (string-append #$output "/include"))))))))
    (supported-systems '("x86_64-linux"))
    (properties `((hidden? . #t)
                  (tunable? . #f)))))

;; This copy of gsl will automatically use the libraries that target the
;; x86_64 psABI which the hardware supports.
(define-public gsl-hwcaps
  (package/inherit gsl
    (name "gsl-hwcaps")
    (arguments
     (substitute-keyword-arguments (package-arguments gsl)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'install-optimized-libraries
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((hwcaps "/lib/glibc-hwcaps"))
                  (copy-recursively
                    (string-append (assoc-ref inputs "gsl-x86-64-v2")
                                   hwcaps "/x86-64-v2")
                    (string-append #$output hwcaps "/x86-64-v2"))
                  (copy-recursively
                    (string-append (assoc-ref inputs "gsl-x86-64-v3")
                                   hwcaps "/x86-64-v3")
                    (string-append #$output hwcaps "/x86-64-v3"))
                  (copy-recursively
                    (string-append (assoc-ref inputs "gsl-x86-64-v4")
                                   hwcaps "/x86-64-v4")
                    (string-append #$output hwcaps "/x86-64-v4")))))))))
    (native-inputs
     (modify-inputs (package-native-inputs gsl)
                    (append gsl-x86-64-v2
                            gsl-x86-64-v3
                            gsl-x86-64-v4)))
    (properties `((tunable? . #f)))))
