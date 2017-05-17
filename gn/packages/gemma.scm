(define-module (gn packages gemma)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  ;; #:use-module (guix build-system ruby)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (gn packages statistics)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages check)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages node)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages bootstrap)
  #:use-module (gn packages ldc)
  #:use-module (gn packages ldc)
  #:use-module (gn packages shell)
  #:use-module (srfi srfi-1))

(define-public gemma-git ; guix candidate
  (let ((commit "5675bdf0422f63f81752bb1fcf7b436bc30bf9b7"))
  (package
    (name "gemma-git")
    (version (string-append "0.95.2a" commit ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/xiangzhou/GEMMA")
                   (commit commit)))
             (file-name (string-append name "-" commit))
             (sha256
              (base32
               "0qc8dx8m4cqggnin2vsbc9l91ay6h04l0mpjv8z0wcickqb4lby6"))))
    (inputs `(
              ("gsl" ,gsl)
              ("lapack" ,lapack)
              ("zlib" ,zlib)
              ))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '(" FORCE_DYNAMIC=1")
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
       #:tests? #t))
    (home-page "")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description "GEMMA is the software implementing the Genome-wide
Efficient Mixed Model Association algorithm for a standard linear
mixed model and some of its close relatives for genome-wide
association studies (GWAS).")
    (license license:gpl3))))

(define-public gemma
  (package
   (name "gemma")
   (version "0.95a") ; this build fails
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/xiangzhou/GEMMA/archive/v"
                                version ".tar.gz"))
            (sha256
             (base32
              "1h0fqs45gy0n1nr76pzzagrv1cbcp7zaz34ljqb09jljc7c3lw9p"))))
   (inputs `(("gsl" ,gsl)
             ("lapack" ,lapack)
             ("zlib" ,zlib)))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags '("FORCE_DYNAMIC=1")
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
      #:tests? #f)) ; no tests included
   (home-page "")
   (synopsis "Tool for genome-wide efficient mixed model association")
   (description "GEMMA is software implementing the Genome-wide
Efficient Mixed Model Association algorithm for a standard linear
mixed model and some of its close relatives for genome-wide
association studies (GWAS).")
   (license license:gpl3)))
