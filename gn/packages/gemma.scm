(define-module (gn packages gemma)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gn packages shell)
  #:use-module (srfi srfi-1))


(define-public openblas-git
  (let ((commit "63b03efc2af332c88b86d4fd8079d00f4b439adf"))
    (package
    (name "openblas-git")
    (version (string-append "0.3.10-git-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/xianyi/OpenBLAS.git")
                   (commit commit)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "174id98ga82bhz2v7sy9yj6pqy0h0088p3mkdikip69p9rh3d17b"))))
    (build-system gnu-build-system)
    (arguments

     `(
       #:tests? #f  ;no "check" target

       ;; DYNAMIC_ARCH is only supported on x86.  When it is disabled and no
       ;; TARGET is specified, OpenBLAS will tune itself to the build host, so
       ;; we need to disable substitutions.
       #:substitutable?
        ,(let ((system (or (%current-target-system) (%current-system))))
           (or (string-prefix? "x86_64" system)
               (string-prefix? "i686" system)
               (string-prefix? "mips" system)
               (string-prefix? "aarch64" system)))
               ; BINARY=64 NO_WARMUP=0 GEMM_MULTITHREAD_THRESHOLD=4 USE_THREAD=1 NO_AFFINITY=0 NO_LAPACK=0 NUM_THREADS=64
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "SHELL=bash"
             "NUM_THREADS=64"
             "BINARY=64"
             "NO_WARMUP=0"
             "GEMM_MULTITHREAD_THRESHOLD=4"
             "USE_THREAD=1"
             "NO_AFFINITY=0"
             "NO_LAPACK=0"    ; use OpenBlas LAPACK
             "COMMON_PROF=0"  ; disable profiling
             "DEBUG=0"

             ;; Build the library for all supported CPUs.  This allows
             ;; switching CPU targets at runtime with the environment variable
             ;; OPENBLAS_CORETYPE=<type>, where "type" is a supported CPU type.
             ;; Unfortunately, this is not supported on non-x86 architectures,
             ;; where it leads to failed builds.
             ,@(let ((system (or (%current-target-system) (%current-system))))
                 (cond
                  ; ((or (string-prefix? "x86_64" system)
                  ;     (string-prefix? "i686" system))
                  ((string-prefix? "x86_64" system)
                   '("DYNAMIC_ARCH=1"))
                  ;; On MIPS we force the "SICORTEX" TARGET, as for the other
                  ;; two available MIPS targets special extended instructions
                  ;; for Loongson cores are used.
                  ((string-prefix? "mips" system)
                   '("TARGET=SICORTEX"))
                  ;; On aarch64 force the generic 'armv8-a' target
                  ((string-prefix? "aarch64" system)
                   '("TARGET=ARMV8"))
                  (else '()))))
       ;; no configure script
       #:phases (alist-delete 'configure %standard-phases)))
    (inputs
     `(("gfortran" ,gfortran)
       ("gfortran:lib" ,gfortran "lib")))
    (native-inputs
     `(("cunit" ,cunit)
       ("perl" ,perl)))
    (home-page "http://www.openblas.net/")
    (synopsis "Platform optimized BLAS library based on GotoBLAS")
    (description
     "OpenBLAS is a BLAS library forked from the GotoBLAS2-1.13 BSD version.")
    (license license:bsd-3))))


(define-public gemma-gn2 ; Version used in GeneNetwork. Guix candidate - generic openblas version
  (let ((commit "f623b4775d47b2c2697a4925bd044412a7db49d3"))
  (package
    (name "gemma-gn2")
    (version (string-append "0.98.3-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/GEMMA")
                   (commit commit)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "1p8a7kkfn1mmrg017aziy544aha8i9h6wd1x2dk3w2794wl33qb7"))))
    ; guix environment -C guix --ad-hoc gcc-toolchain gdb gsl openblas zlib bash ld-wrapper perl vim which
    (inputs `(
              ("gsl" ,gsl)
              ("shunit2" ,shunit2)
              ("openblas" ,openblas)
              ("zlib" ,zlib)
              ))
    (native-inputs ; for running tests
     `(("perl" ,perl)
       ("which" ,which)
       ))

    (build-system gnu-build-system)
    (arguments
     `(#:phases
        (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'bin-mkdir
                     (lambda _
                       (mkdir-p "bin")
                       ))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                           (let ((out (assoc-ref outputs "out")))
                             (install-file "bin/gemma" (string-append out "/bin"))))))
       ; #:tests? #f
       #:parallel-tests? #f))
    (home-page "http://www.xzlab.org/software.html")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description "Genome-wide Efficient Mixed Model Association (GEMMA)
provides a standard linear mixed model resolver with application in
genome-wide association studies (GWAS).")

    (license license:gpl3))))

(define-public gemma-gn2-git ; openblas optimized
  (package
   (inherit gemma-gn2)
   (name "gemma-gn2-git")
   (inputs `(
             ("gsl" ,gsl)
             ("shunit2" ,shunit2)
             ("openblas" ,openblas-git)
             ("zlib" ,zlib)
             ))
    (arguments
     `(
       #:phases
        (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'bin-mkdir
                     (lambda _
                       (mkdir-p "bin")
                       ))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                           (let ((out (assoc-ref outputs "out")))
                             (install-file "bin/gemma" (string-append out "/bin"))))))
       ; #:tests? #f
       #:parallel-tests? #f))
   ))

(define-public gemma-gn2-dev ; Dev version for GeneNetwork. Not a Guix candidate
  (let ((commit "f623b4775d47b2c2697a4925bd044412a7db49d3"))
  (package
    (name "gemma-gn2-dev")
    (version (string-append "0.98.3-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/GEMMA")
                   (commit commit)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "0xm4cjfzrcb1wc8lw9p1727spica8rlzwnvjkp7ackn1g7zmzhw0"))))
    ; guix environment -C guix --ad-hoc gcc-toolchain gdb gsl openblas zlib bash ld-wrapper perl vim which
    (inputs `(
              ("gsl" ,gsl)
              ("shunit2" ,shunit2)
              ("openblas" ,openblas)
              ("zlib" ,zlib)
              ))
    (native-inputs ; for running tests
     `(("perl" ,perl)
       ("which" ,which)
       ))

    (build-system gnu-build-system)
    (arguments
     `(#:phases
        (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'bin-mkdir
                     (lambda _
                       (mkdir-p "bin")
                       ))
         (replace 'build
                  (lambda* (#:key inputs #:allow-other-keys)
                    (invoke "make" "debug" "-j" (number->string (parallel-job-count)))))
         (replace 'check
                  (lambda* (#:key inputs #:allow-other-keys)
                    (invoke "make" "fast-check" )))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                           (let ((out (assoc-ref outputs "out")))
                             (install-file "bin/gemma" (string-append out "/bin"))))))
       ; #:tests? #f
       #:parallel-tests? #f))
    (home-page "http://www.xzlab.org/software.html")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description "Genome-wide Efficient Mixed Model Association (GEMMA)
provides a standard linear mixed model resolver with application in
genome-wide association studies (GWAS).")

    (license license:gpl3))))



(define-public gemma-wrapper
  (package
    (name "gemma-wrapper")
    (version "0.98.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-gemma-wrapper" version))
       (sha256
        (base32
         "0wysg7s64vyzm19j2hvxyflbszx00d9diw9zwzbqirzagzmqbfwk"))))
    (build-system ruby-build-system)
    (inputs `(("gemma-gn2" ,gemma-gn2-git)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'build 'set-gemma-path
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
                     (substitute* "bin/gemma-wrapper"
                      ; (("gemma_command = ENV['GEMMA_COMMAND']")
                      (("gemma_command = ENV.*")
                       (string-append "gemma_command = '" (which "gemma") "'")))
                     ))))))
    (synopsis
     "Gemma wrapper for LOCO and caching")
    (description "Gemma wrapper")
    (home-page "https://rubygems.org/gems/bio-gemma-wrapper")
    (license license:gpl3)))

(define-public gemma-dev-env
  (let ((md5 "93e745e9c"))
    (package
    (name "gemma-dev-env")
    (version "0.98")
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
     `(("binutils" ,binutils) ; for ld
       ("gemma-gn2" ,gemma-gn2-git)
       ; ("gemma-wrapper" ,gemma-wrapper)
       ("gcc" ,gcc-7)
       ("gdb" ,gdb)
       ("gfortran:lib" ,gfortran "lib")
       ("glibc" ,glibc) ; for crt1.o
       ("gsl" ,gsl)
       ("eigen" ,eigen)
       ("linux-libre-headers" ,linux-libre-headers)
       ("openblas" ,openblas-git)
       ("shunit2" ,shunit2)
       ("zlib" ,zlib)
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

    (home-page "http://github.com/genetics-statistics/")
    (synopsis "GEMMA development environment imports build tools, gemma-wrapper and faster-lmm-d")
    (description "Gemma-development")
    (license license:gpl3))))


(define-public faster-lmm-d-dev ; incomplete, just creates build environment
  (let ((commit "68e22043ce0ca348cbc4f3bdd015e036ba9ac5f2"))
    (package
     (name "faster-lmm-d-dev")
     (version (string-append "0.0.1-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/genetics-statistics/faster_lmm_d.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0awcsrmpq1zbcy145l5ssmsxzfq5dq9yr0901hk977lmjf99gxng"))))
     (build-system trivial-build-system)
     (propagated-inputs
      `(("openblas" ,openblas)
        ("gsl" ,gsl)
        ; ("lapack" ,lapack)
        ("dub" ,dub)
        ("gcc" ,gcc-7)
        ("gdb" ,gdb)
        ; ("glibc" ,glibc)
        ("binutils" ,binutils) ; for ld linker
        ("gfortran:lib" ,gfortran "lib")
        ; ("make" ,make)
        ("ldc" ,ldc)
        ("shunit2" ,shunit2)
        ("linux-libre-headers" ,linux-libre-headers)
        ("openblas" ,openblas-git)
        ))
     (inputs
     `(
        ("which" ,which)
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
     (home-page "https://github.com/pjotrp")
     (synopsis "Build system for faster-lmm-d")
     (description
      ".")
     (license license:gpl3))))
