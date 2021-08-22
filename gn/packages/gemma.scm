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
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gn packages shell)
  #:use-module (srfi srfi-1))


;; See also the recent gemma.scm in the GEMMA repo!
(define-public gemma-gn2 ; guix candidate - currently uses generic
                         ; openblas version and genenetwork github repo

  (let ((commit "140667153f7ad601af2761e0856c396ce2808a78"))
  (package
    (name "gemma-gn2")
    (version (string-append "0.98.4-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/GEMMA")
                   (commit commit)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "0km2m3xhq8a5d6nv7n9r0jmxhc2fy8f3q88mfqarmcqybbyplsd4"))))
    (inputs `(
              ("gsl" ,gsl)
              ("shunit2-old" ,shunit2-old)
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
    (home-page "https://github.com/genetics-statistics/GEMMA")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description "Genome-wide Efficient Mixed Model Association (GEMMA)
provides a standard linear mixed model resolver with application in
genome-wide association studies (GWAS).")

    (license license:gpl3))))

(define-public gemma-gn2-git ; typically use latest for genenetwork,
                             ; may switch off tests and (maybe)
                             ; openblas optimized
  (package
   (inherit gemma-gn2)
   (name "gemma-gn2-git")
   (inputs `(
             ("gsl" ,gsl)
             ("shunit2-old" ,shunit2-old)
             ("openblas" ,openblas)
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


(define-public gemma-wrapper
  (package
    (name "gemma-wrapper")
    (version "0.99.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-gemma-wrapper" version))
       (sha256
        (base32
         "032f4mwb584vcqqlzp61dqqlb16cp68y6iqpbjhxzx7x3qkfps59"))))
    (build-system ruby-build-system)
    (inputs `(
      ("gemma-gn2" ,gemma-gn2-git)
      ("parallel" ,parallel) ;; gnu parallel
    ))
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
