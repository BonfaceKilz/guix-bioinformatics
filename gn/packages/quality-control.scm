(define-module (gn packages quality-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages))

(define-public utf-8-lineseparator
  (package
    (name "utf-8-lineseparator")
    (version "cj3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pflanze/utf-8-lineseparator")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xnbcanqn5jr965gw9195ij6hz04clfm77m5776dysn9nykn20w1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "utf-8-lineseparator" bin)))))))
    (home-page "https://github.com/pflanze/utf-8-lineseparator")
    (synopsis "Line ending detection library")
    (description
     "@code{utf-8-lineseparator} provides a tool to efficiently check text
(CSV) files for valid UTF-8 use, and to report which line endings
they use.")
    (license license:expat)))

