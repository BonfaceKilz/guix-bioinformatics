(define-module (gn packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages syncthing)
  #:use-module (srfi srfi-1))

(define-public go-github-com-oneofone-xxhash
  (package
    (name "go-github-com-oneofone-xxhash")
    (version "1.2.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/OneOfOne/xxhash")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0f98qk83l2fhpclvrgyxsa9b8m4pipf11fah85bnjl01wy4lvybw"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/OneOfOne/xxhash"))
    (home-page "https://github.com/OneOfOne/xxhash")
    (synopsis "xxhash")
    (description
      "This is a native Go implementation of the excellent @url{https://github.com/Cyan4973/xxHash,xxhash}* algorithm, an extremely fast non-cryptographic Hash algorithm, working at speeds close to RAM limits.")
    (license license:asl2.0)))

(define-public go-github-com-aswinkarthik-csvdiff
  (package
    (name "go-github-com-aswinkarthik-csvdiff")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/aswinkarthik/csvdiff")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0cd1ikxsypjqisfnmr7zix3g7x8p892w77086465chyd39gpk97b"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/aswinkarthik/csvdiff"))
    (propagated-inputs
      `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ("go-github-com-stretchr-testify"
         ,go-github-com-stretchr-testify)
        ("go-github-com-spf13-cobra"
         ,go-github-com-spf13-cobra)
        ("go-github-com-spf13-afero"
         ,go-github-com-spf13-afero)
        ("go-github-com-spaolacci-murmur3"
         ,go-github-com-spaolacci-murmur3)
        ("go-github-com-mattn-go-colorable"
         ,go-github-com-mattn-go-colorable)
        ("go-github-com-fatih-color"
         ,go-github-com-fatih-color)
        ("go-github-com-cespare-xxhash"
         ,go-github-com-cespare-xxhash)
        ("go-github-com-oneofone-xxhash"
         ,go-github-com-oneofone-xxhash)))
    (home-page
      "https://github.com/aswinkarthik/csvdiff")
    (synopsis "csvdiff")
    (description
      "This package provides a fast diff tool for comparing csv files.")
    (license
      license:expat)))
