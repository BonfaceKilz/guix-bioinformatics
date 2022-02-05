(define-module (gn packages gemini)
  #:use-module (gnu packages guile)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public tissue
  (package
    (name "tissue")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.systemreboot.net/tissue")
                    (commit "20af13095723a44f58720670f3e98f04f7bf50b6")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rahq7adhlx9krs31im2x3lsm0307xw12865nik2nwccqmg171k1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "prefix=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs (list guile-3.0))
    (home-page "https://tissue.systemreboot.net")
    (synopsis "Text based issue tracker")
    (description "tissue is a text based issue tracker.")
    (license license:gpl3+)))
