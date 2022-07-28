(define-module (gn packages cran)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages statistics))

(define-public r-tictoc
  (package
    (name "r-tictoc")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "tictoc" version))
        (sha256
         (base32
          "1zp2n8k2ax2jjw89dsri268asmm5ry3ijf32wbca5ji231y0knj7"))))
    (build-system r-build-system)
    (home-page "http://github.com/collectivemedia/tictoc")
    (synopsis "Functions for timing R scripts")
    (description
     "This package provides the timing functions @code{tic} and @code{toc} that
can be nested.  One can record all timings while a complex script is running,
and examine the values later.  It is also possible to instrument the timing call
with custom callbacks.  In addition, this package provides class 'Stack',
implemented as a vector, and class 'List', implemented as a list, both of whic
support operations 'push', 'pop', 'first', 'last' and 'clear'.")
    (license license:asl2.0)))

(define-public r-bracer
  (package
    (name "r-bracer")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "bracer" version))
              (sha256
               (base32 "0q47p43957nlaqsz0hhbica2hlar95c2i8avgq024yhccik5nx6f"))))
    (properties `((upstream-name . "bracer")))
    (build-system r-build-system)
    (propagated-inputs (list r-stringr))
    (home-page "https://trevorldavis.com/R/bracer/")
    (synopsis "Brace Expansions")
    (description
     "Performs brace expansions on strings.  Made popular by Unix shells, brace
expansion allows users to concisely generate certain character vectors by taking
a single string and (recursively) expanding the comma-separated lists and
double-period-separated integer and character sequences enclosed within braces
in that string.  The double-period-separated numeric integer expansion also
supports padding the resulting numbers with zeros.")
    (license license:expat)))
