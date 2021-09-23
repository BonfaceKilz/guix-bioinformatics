(define-module (gn packages images)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  )

(define-public kaleido
  (package
    (inherit ungoogled-chromium)
    (name "kaleido")
    (version "0.2.1")
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments ungoogled-chromium)
       ((#:modules modules %gnu-build-system-modules)
        `((srfi srfi-1)
          ,@modules))
       ((#:tests? _ #f) #f)             ; TODO: enable after successfully building.
       ((#:configure-flags flags)
        `(append
           ;; First we modify the inherited configure-flags.
           (fold delete ,flags
                 '(
                   "use_pulseaudio=true"
                   "link_pulseaudio=true"
                   "use_vaapi=true"
                   "rtc_use_pipewire=true"
                   "rtc_link_pipewire=true"
                   ))
           (list
             "import(\"//build/args/headless.gn\")"
             "enable_nacl=false"
             "is_component_build=false"
             "symbol_level=0"
             "blink_symbol_level=0"
             "is_debug=false"
             )
           ))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'unpack-kaleido-source
             (lambda* (#:key inputs #:allow-other-keys)
               ;(invoke "tar" "xvf" (assoc-ref inputs "kaleido-source")
               ;        "--strip-components=1")
               (copy-recursively (assoc-ref inputs "kaleido-source") ".")
               ))
           (add-before 'configure 'prepare-kaleido-build-environment
             (lambda _
               ;; /repos/linux_scripts/build_kaleido_docker
               (with-output-to-file "repos/kaleido/version"
                 (lambda _
                   (format #t "~a~%" ,version)))
               ;(mkdir-p "out/Kaleido_linux")
               ;(with-output-to-file "out/Kaleido_linux/args.gn"
               ;  (lambda _
               ;    (format #t
               ;            "import(\"//build/args/headless.gn\")~@
               ;            enable_nacl=false~@
               ;            is_component_build=false~@
               ;            ~@
               ;            symbol_level=0~@
               ;            blink_symbol_level=0~@
               ;            is_debug=false~@
               ;            ~@
               ;            target_cpu=\"~a\"~%"
               ;            "x64")))   ; x86_64 -> x64, aarch64 -> arm64, armhf -> arm
               ))
           (replace 'configure
             (lambda* (#:key configure-flags #:allow-other-keys)
               (let ((args (string-join configure-flags " ")))
                 ;; Generate ninja build files.
                 (invoke "gn" "gen" "out/Release"
                         (string-append "--args=" args)
                         )

                 ;; Print the full list of supported arguments as well as
                 ;; their current status for convenience.
                 ;(format #t "Dumping configure flags...\n")
                 ;(invoke "gn" "args" "out/Kaleido_linux" "--list")
                 )))
           (replace 'build
             (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
               (mkdir-p "headless/app")
               (copy-recursively "repos/kaleido/cc" "headles/app")

               (invoke "ninja" "-C" "out/Release"
                       "-j" (if parallel-build?
                                (number->string (parallel-job-count))
                                "1")
                       "kaleido")

               (mkdir-p "repos/build/kaleido_minimal/bin")
               (install-file "out/Release/kaleido" "repos/build/kaleido_minimal/bin")
               (copy-recursively "out/Release/swiftshader/" "repos/build/kaleido_minimal/bin")
               (install-file "repos/kaleido/version" "repos/build/kaleido_minimal/")

               (invoke "python3" "setup.py" "package")
               (invoke "python3" "setup.py" "package_source")
               ))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (with-directory-excursion "repos/kaleido/tests"
                   (invoke "pytest" "-s" "test_py/")))))
           ))
        ))
    (native-inputs
     `(
       ("gn" ,gn)
       ("poppler" ,poppler)
       ("python" ,python)
       ("python-pandas" ,python-pandas)
       ("python-plotly" ,python-plotly)
       ("python-pytest" ,python-pytest)
       ("kaleido-source"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/plotly/Kaleido")
                  (commit (string-append "v" version))))
           (file-name (git-file-name name version))
           (sha256
            (base32 "0p7vddwha4bmb0ihbvjgqwhwa7xmm7xnh2hff5r2xzz64rjcz47x"))))
       ,@(package-native-inputs ungoogled-chromium)))
    (inputs
     `(
       ,@(package-inputs ungoogled-chromium)))
    (home-page "https://github.com/plotly/Kaleido")
    (synopsis "Static image export for web-based visualization libraries")
    (description "Kaleido is a library for generating static images (e.g. png,
svg, pdf, etc.) for web-based visualization libraries, with a particular focus
on eliminating external dependencies.  The project's initial focus is on the
export of @code{plotly.js} images from Python for use by @code{plotly.py}, but
it is designed to be relatively straight-forward to extend to other web-based
visualization libraries, and other programming languages.")
    (license license:expat)))
