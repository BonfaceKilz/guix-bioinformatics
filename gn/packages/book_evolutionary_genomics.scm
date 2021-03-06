;; Module that goes with the 'Evolutionary Genomics' book

(define-module (gn packages book_evolutionary_genomics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages version-control)

  #:use-module (gn packages bioinformatics)
  #:use-module (gn packages javascript)
  #:use-module (gn packages python)
  #:use-module (srfi srfi-1))

(define-public r-gener ;; poor implementation
  (package
    (name "r-gener")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       ; (uri (bioconductor-uri "GeneR" version))
       (uri "http://www.bioconductor.org/packages//2.7/bioc/src/contrib/GeneR_2.20.0.tar.gz")
       (sha256
        (base32
         "1qrrq5lrm2wvx3vlas6s84spwnlaji7jaascljcr9078ww8vmjxp"))))
    (build-system r-build-system)
    (home-page "http://bioconductor.org/packages/GeneR")
    (synopsis "Package manipulating nucleotidic sequences (Embl, Fasta, GenBank)")
    (description
     ".")
    (license license:expat))) ; CeCILL-2.0

(define-public r-soap ;; obsolete package and fails to build
  (package
    (name "r-soap")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://sourceforge.net/projects/rsoap/files/RSOAP/1.1.4/RSOAP-1.1.4.tar.gz/download")
       (sha256
        (base32
         "0vggycbjnjpx2c4q4wgaxfd0ig3prw31gdw9djjkbmzc3crbhj8j"))))
    (propagated-inputs
     `(("python" ,python)
       ("r" ,r)
       ))
    (build-system r-build-system)
    (home-page "https://sourceforge.net/projects/rsoap/")
    (synopsis "SOAP server for R")
    (description
     ".")
    (license license:expat))) ; CeCILL-2.0

(define-public book-evolutionary-genomics
  (let ((commit "53a7aefefcedd52b94b38195f8daae94c07d89db"))
    (package
    (name "book-evolutionary-genomics")
    (version (string-append "0.2.1" "-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/pjotrp/Cross-language-interfacing.git")
                   (commit commit)))
             (file-name (string-append name "-" version))
             (sha256
              (base32
               "00amdba2vjwxjhrikaym2xacszhcmiqyc6iknlhdbi86ccmf5qbp"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))
    (propagated-inputs
     `(("python" ,python)
       ("python-biopython" ,python-biopython)
       ; ("python-rserve" ,python-rserve)
       ("r" ,r)
       ("r-gener" ,r-gener)
       ("r-biostrings" ,r-biostrings)
       ("python-cffi" ,python-cffi)
       ("python-rpy2" ,python-rpy2)
       ("r-rserve" ,r-rserve)
       ("ruby" ,ruby)
       ("ruby-ffi" ,ruby-ffi)
       ("bioruby" ,bioruby)
       ("perl" ,perl)
       ("bioperl-minimal" ,bioperl-minimal)
       ("emboss" ,emboss)
       ))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/book-evolutionary-genomics")))
             (write target)
             (mkdir-p target)
             (copy-recursively (assoc-ref %build-inputs "source") target)
             #t))))

    (home-page "http://github.com/pjotrp/")
    (synopsis "Packages for Evolutionary Genomics book")
    (description "More later...")
    (license license:agpl3+)))
  )
