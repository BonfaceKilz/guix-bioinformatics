(define-module (gn packages gemini)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages skribilo) #:prefix guix:)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public skribilo-latest
  (let ((commit "621eb1945aec8f26f5aee4bdf896f2434e145182")
        (revision "1"))
    (package
      (inherit guix:skribilo)
      (name "skribilo")
      (version (git-version "0.9.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.systemreboot.net/skribilo")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16rdcvszl9x183y32hjdwns0lkrvkmwd2fsshymspb12k4cxj6i4"))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gettext" ,gnu-gettext)
         ,@(package-native-inputs guix:skribilo))))))

(define-public tissue
  (let ((commit "85811ab5e6dfbfbcd772eba8a2219061db4c3991")
        (revision "0"))
    (package
      (name "tissue")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.systemreboot.net/tissue")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1wmdrv0lr7s96nm1invf4gc6c1fq1y1mfgqxy6l0h32fh5sx87gq"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "prefix=" %output)
                            "GUILE_AUTO_COMPILE=0")
         #:modules (((guix build guile-build-system)
                     #:select (target-guile-effective-version))
                    ,@%gnu-build-system-modules)
         #:imported-modules ((guix build guile-build-system)
                             ,@%gnu-build-system-modules)
         #:phases
         (modify-phases %standard-phases
           (replace 'patch-source-shebangs
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "bin/tissue"
                 (("^exec guile")
                  (string-append "exec " (assoc-ref inputs "guile") "/bin/guile")))))
           (replace 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* (list "bin/tissue" "tissue/git.scm" "tissue/issue.scm")
                 (("\"git\"")
                  (string-append "\"" (assoc-ref inputs "git-minimal") "/bin/git\"")))))
           (add-after 'install 'wrap
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (effective-version (target-guile-effective-version)))
                 (wrap-program (string-append out "/bin/tissue")
                   `("GUILE_LOAD_PATH" prefix
                     (,(string-append out "/share/guile/site/" effective-version)
                      ,(getenv "GUILE_LOAD_PATH")))
                   `("GUILE_LOAD_COMPILED_PATH" prefix
                     (,(string-append out "/lib/guile/" effective-version "/site-ccache")
                      ,(getenv "GUILE_LOAD_COMPILED_PATH"))))))))))
      (inputs (list git-minimal guile-3.0))
      (propagated-inputs
       (list skribilo-latest))
      (home-page "https://tissue.systemreboot.net")
      (synopsis "Text based issue tracker")
      (description "tissue is a text based issue tracker.")
      (license license:gpl3+))))
