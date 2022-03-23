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

(define-public skribilo-with-gemtext-reader
  (let ((commit "183eb122e17a9ab6c95409d1cb1803727cf85717")
        (revision "0"))
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
                  "0fz106df75fc1d6hy55nim2j6zhq94msg47bd96sp0hm25n8maca"))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gettext" ,gnu-gettext)
         ,@(package-native-inputs guix:skribilo))))))

(define-public tissue
  (let ((commit "17d101b2f97edc8574528d5f05dd952921b67027")
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
                  "0g2f4y6x8wam5367cfpd00nm0swnk3b33rgwcs3rl0cxn2hvj610"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "prefix=" %output)
                            "GUILE_AUTO_COMPILE=0")
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* (list "bin/tissue" "tissue/issue.scm" "tissue/web.scm")
                 (("\"git\"")
                  (string-append "\"" (assoc-ref inputs "git-minimal") "/bin/git\""))))))))
      (inputs (list git-minimal guile-3.0))
      (propagated-inputs
       (list skribilo-with-gemtext-reader))
      (home-page "https://tissue.systemreboot.net")
      (synopsis "Text based issue tracker")
      (description "tissue is a text based issue tracker.")
      (license license:gpl3+))))
