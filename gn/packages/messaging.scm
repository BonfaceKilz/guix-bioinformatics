(define-module (gn packages messaging)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages matrix)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gn packages python))

(define-public weechat-matrix
  (package
    (name "weechat-matrix")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/poljar/weechat-matrix")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1iv55n4k05139f7jzkhczgw4qp6qwilrvfsy3c6v2m1kxffj12d3"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((weechat-python (string-append (assoc-ref outputs "out")
                                                   "/share/weechat/python")))
               ;; Avoid circular import by renaming the matrix module to
               ;; weechat_matrix.
               (substitute* (cons "main.py"
                                  (append (find-files "matrix")
                                          (find-files "tests")))
                 (("from matrix") "from weechat_matrix")
                 (("import matrix") "import weechat_matrix"))
               ;; Install python modules.
               (invoke "make" "install-lib"
                       (string-append "INSTALLDIR="
                                      (site-packages inputs outputs)
                                      "/weechat_matrix"))
               ;; Extend PYTHONPATH to find installed python modules.
               (add-installed-pythonpath inputs outputs)
               ;; Augment sys.path so that dependencies are found.
               (substitute* "main.py"
                 (("import os\n" all)
                  (apply string-append
                         all
                         "import sys\n"
                         (map (lambda (path)
                                (string-append "sys.path.append('" path "')\n"))
                              (string-split (getenv "PYTHONPATH") #\:)))))
               ;; Install script.
               (mkdir-p weechat-python)
               (copy-file "main.py"
                          (string-append weechat-python "/matrix.py")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (inputs
     `(("python-matrix-nio" ,python-matrix-nio)
       ("python-pygments" ,python-pygments)
       ("python-pyopenssl" ,python-pyopenssl)
       ("python-webcolors" ,python-webcolors)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/poljar/weechat-matrix")
    (synopsis "Weechat Matrix protocol script")
    (description "@code{weechat-matrix} is a Python plugin for Weechat that lets
Weechat communicate over the Matrix protocol.")
    (license license:isc)))

(define-public weechat-wee-slack
  (package
    (name "weechat-wee-slack")
    (version "2.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wee-slack/wee-slack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xfklr0gsc9jgxfyrrb2j756lclz9g8imcb0pk0xgyj8mhsw23zk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Augment sys.path so that dependencies are found.
             (substitute* "wee_slack.py"
               (("import sys\n" all)
                (apply string-append
                       all
                       (map (lambda (path)
                              (string-append "sys.path.append('" path "')\n"))
                            (string-split (getenv "PYTHONPATH") #\:)))))
             ;; Install script.
             (install-file "wee_slack.py"
                           (string-append (assoc-ref outputs "out")
                                          "/share/weechat/python"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (inputs
     `(("python-websocket-client" ,python-websocket-client)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/wee-slack/wee-slack")
    (synopsis "Weechat Slack script")
    (description "@code{weechat-wee-slack} is a WeeChat native client for
Slack.  It provides supplemental features only available in the web/mobile
clients such as synchronizing read markers, typing notification, threads (and
more)!  It connects via the Slack API, and maintains a persistent websocket
for notification of events.")
    (license license:expat)))
