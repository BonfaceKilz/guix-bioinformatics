(define-module (gn packages quality-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
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

