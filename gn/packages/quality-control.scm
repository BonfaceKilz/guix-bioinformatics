(define-module (gn packages quality-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages))

(define-public libcsv
  (let ((commit "b1d5212831842ee5869d99bc208a21837e4037d5")
        (revision "0"))
    (package
      (name "libcsv")
      (version commit)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/rgamble/libcsv")
           (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "0nni9y0prz8lh3dywbha52yfg8qcf3dn0nb7iinfwlrmmamfyzg2"))))
      (build-system gnu-build-system)
      (arguments
        `(#:make-flags
          (list (string-append "CC=" ,(cc-for-target)))))
      (home-page "https://github.com/rgamble/libcsv")
      (synopsis "CSV library written in pure ANSI C")
      (description
"CSV library written in pure ANSI C that can read and write CSV data.")
      (license license:lgpl2.1+))))

(define-public sbcl-qc
  (let ((commit "a0d8c807fa3f817f977a64f02f5f8936044050ad")
        (revision "4"))
    (package
      (name "sbcl-qc")
      (version (git-version "20220331" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://git.genenetwork.org/jgart/qc")
                (commit commit)))
          (sha256
           (base32 "12i9s1ipqarvq6gsjqycfm8d3ym7dv7vn3nxb5c6214xrd4mmzjj"))
          (file-name (git-file-name name commit))))
      (build-system asdf-build-system/sbcl)
      (arguments
        `(#:tests? #f ; Need to fix tests.
          #:asd-files '("qc.asd")))
      (native-inputs
        (list sbcl-1am))
      (inputs
        (list sbcl-alexandria
              sbcl-split-sequence))
      (home-page "https://git.genenetwork.org/jgart/qc/")
      (synopsis "Quality control of delimited files")
      (description
"@code{cl-qc} is a Common Lisp library for linting the following in
delimited text files:
@itemize
@item Whitespace
@item Floating-point number well-formedness
@item GeneChip Platform ID checks for Affymetrix, Illumina, and others
@end itemize\n")
      (license license:unlicense))))

(define-public cl-qc
  (sbcl-package->cl-source-package sbcl-qc))

(define-public ecl-qc
  (sbcl-package->ecl-package sbcl-qc))


;;  The file
;;  #P"/tmp/guix-build-sbcl-qc-uploads-20220301-0.76f870e.drv-0/source/strains.csv"
;;  does not exist:
;;    No such file or directory
#;(define-public sbcl-qc-uploads
  (let ((commit "76f870efad964bf680bf633e272094082008e69c")
        (revision "0"))
    (package
      (name "sbcl-qc-uploads")
      (version (git-version "20220301" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://git.genenetwork.org/jgart/qc-uploads")
                (commit commit)))
          (sha256
           (base32 "1nzcdqjwh1xpr92d01fvlmdysxizyklaid10zrxbi12jmdydapil"))
          (file-name (git-file-name name commit))))
      (build-system asdf-build-system/sbcl)
      ;; needs install-file for strains.tsv. This require more testing.
      ;; todo: see for an example: https://github.com/interactive-ssr/issr-server/blob/master/guix.scm#L46
      (arguments
       `(#:tests? #f ; There are no tests yet.
         #:asd-files '("qc-uploads.asd")))
      (inputs
        (list sbcl-alexandria
              sbcl-ningle
              sbcl-clack
              sbcl-cl-css
              sbcl-cl-who
              sbcl-cl-fad
              sbcl-qc ; packaged in guix-bioinformatics
              sbcl-woo))
      (home-page "https://git.genenetwork.org/jgart/qc-uploads/")
      (synopsis "Web UI for qc")
      (description 
"@code{qc-uploads} provides a web UI for @code{qc}.")
      (license license:expat))))

#;(define-public cl-qc-uploads
  (sbcl-package->cl-source-package sbcl-qc-uploads))

#;(define-public ecl-qc-uploads
  (sbcl-package->ecl-package sbcl-qc-uploads))

