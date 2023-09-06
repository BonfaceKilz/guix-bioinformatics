(define-module (gn packages julia)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system julia)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gn packages cran)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages julia-jll)
  #:use-module (gnu packages julia-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (ice-9 match))

(define S specification->package)

(define-public julia-visuals
  (let ((commit "e8e2b601f40a76c8f20f0ddfe80c56257dd9a294")
        (revision "3"))
    (package
      (name "julia-visuals")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sens/visuals")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "0lm9yhk0mq5cvvkcbsgcjc1y7fzhr8qz2nxn38cy1zdxd8vfknsx"))
                (patches (search-patches "julia-visuals-remove-manifests.diff"))))
      (build-system julia-build-system)
      (arguments
       `(#:tests? #f    ; no test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'link-depot)     ; Not really needed on this package.
           (add-after 'unpack 'patch-source
             (lambda* (#:key inputs #:allow-other-keys)
               (chmod "runpluto.sh" #o555)  ; it starts as #o444
               (substitute* "runpluto.sh"
                 ;; The arguments don't pass through the wrapper so we hardcode the port.
                 (("\\$\\{1\\}") "4343"))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; Copied from the Dockerfile.
                 (for-each
                  (lambda (file)
                    (copy-recursively file (string-append out "/" file)))
                  (list "plutoserver"
                        "environment.yml"
                        "setup.py"
                        "runpluto.sh"
                        "runsliderserver.sh"
                        "notebooks"
                        "Project.toml")))))
           (add-after 'install 'skip-julia-cairomakie
             (lambda* (#:key outputs #:allow-other-keys)
               (with-directory-excursion
                 (string-append (assoc-ref outputs "out") "/notebooks")
                 (delete-file "bayes.jl")
                 (delete-file "disease-testing.jl"))))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; Do we need to wrap this with PYTHONPATH too?
                 (wrap-script (string-append out "/runpluto.sh")
                  `("PATH" ":" prefix (,(string-append (assoc-ref inputs "julia") "/bin")
                                        ,(string-append (assoc-ref inputs "coreutils") "/bin")))
                  `("JULIA_LOAD_PATH" ":" prefix (,(getenv "JULIA_LOAD_PATH")))))))
           (add-after 'install 'create-runpluto
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (with-output-to-file (string-append out "/runpluto")
                   (lambda ()
                     (format #t "#!~a --no-auto-compile
!#
(setenv \"JULIA_LOAD_PATH\" \"~a\")
(setenv \"PATH\" \"~a\")
(zero? (system*
         \"~a\"
         \"--project=/home/jovyan\"
         \"--optimize=0\"
         \"-e\" \"import Pluto;
         Pluto.run(
            host=\\\"0.0.0.0\\\",
            port=4343,
            launch_browser=false,
            require_secret_for_open_links=false;
            require_secret_for_access=false)\"))\n"
                             (search-input-file inputs "/bin/guile")
                             (getenv "JULIA_LOAD_PATH")
                             (dirname (search-input-file inputs "/bin/yes"))
                             (search-input-file inputs "/bin/julia"))))
                   (chmod (string-append out "/runpluto") #o555))))
           (add-after 'install 'create-runsliderserver
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (with-output-to-file (string-append out "/runsliderserver")
                   (lambda ()
                     (format #t "#!~a --no-auto-compile
!#
(setenv \"JULIA_LOAD_PATH\" \"~a\")
(setenv \"PATH\" \"~a\")
(zero? (system*
         \"~a\"
         \"--optimize=0\"
         \"-e\" \"import PlutoSliderServer;
         PlutoSliderServer.run_directory(
            \\\"~a/notebooks/\\\",
            SliderServer_port=4343,
            SliderServer_host=\\\"0.0.0.0\\\")\"))\n"
                             (search-input-file inputs "/bin/guile")
                             (getenv "JULIA_LOAD_PATH")
                             (dirname (search-input-file inputs "/bin/yes"))
                             (search-input-file inputs "/bin/julia")
                             out)))
                   (chmod (string-append out "/runsliderserver") #o555))))
           (replace 'precompile
             (lambda _
               (invoke "julia" "-e" "\"import Pkg; Pkg.instantiate(); Pkg.status(); Pkg.precompile()\""))))))

      (propagated-inputs
       `(;; from setup.py
         ("python-jupyter-server-proxy"
          ,(@ (gn packages python) python-jupyter-server-proxy-1))))

      (inputs
       (list ;julia-cairomakie           ; 0.8.13
             julia-distributions        ; 0.25.76
             ;julia-interactiveutils    ; stdlib
             julia-latexstrings         ; 1.3.0
             ;julia-markdown            ; stdlib
             julia-optim                ; 1.7.2
             julia-plots                ; 1.35.3
             julia-pluto                ; 0.19.11
             julia-plutosliderserver    ; *
             julia-plutoui              ; 0.7.46
             julia-prettytables         ; 2.1.0
             julia-quadgk               ; 2.5.0
             julia-roots                ; 2.0.3
             (@ (gnu packages guile) guile-3.0)))   ; for wrap-script
      (home-page "https://github.com/sens/visuals")
      (synopsis "Visualizations using Pluto.jl notebooks")
      (description "Visualizations using Pluto.jl notebooks.")
      (license #f))))

(define-public julia-liteqtl
  (let ((commit "696218524a57a4ab5a933e516850c010915650d5")
        (revision "1"))
    (package
      (name "julia-liteqtl")
      ;(version (git-version "0.2.1" revision commit))
      (version "0.2.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/senresearch/LiteQTL.jl")
                       ;(commit commit)))
                       (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "0hincjnr56wpcgnd9r1m939gng8ac46zw1fv99ij920w3dpxwhq3"))))
      (build-system julia-build-system)
      (propagated-inputs
       `(;("julia-csv" ,julia-csv)
         ;("julia-cuda" ,julia-cuda)
         ("julia-dataframes" ,julia-dataframes)
         ("julia-distributions" ,julia-distributions)
         ("julia-docstringextensions" ,julia-docstringextensions)))
      (native-inputs
       `(("julia-documenter" ,julia-documenter)
         ("julia-safetestsets" ,julia-safetestsets)
         ;("r" ,r-minimal)
         ;("r-data-table" ,r-data-table)
         ;("r-parallel" ,r-parallel)    ; what is this package called?
         ;("r-qtl" ,r-qtl)
         ;("r-qtl2" ,r-qtl2)
         ;("r-tidyverse" ,r-tidyverse)
         ))
      (home-page "https://github.com/senresearch/LiteQTL.jl")
      (synopsis "Julia package for eQTL genome scans near real-time")
      (description "LiteQTL is a package that runs whole genome QTL scans near
real-time, utilizing the computation power of GPU.  LiteQTL uses new algorithms
that enables near-real time whole genome QTL scans for up to 1 million traits.
By using easily parallelizable operations including matrix multiplication,
vectorized operations, and element-wise operations, our method is about 300
times faster than a R/qtl linear model genome scan using 16 threads.")
      (license license:expat))))

;; contains bundled libraries?
(define-public julia-packagecompiler
  (package
    (name "julia-packagecompiler")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLang/PackageCompiler.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ks7g8cvyc81yj6knkrdcxkkm3rdw61jx1h3nqn2n289p5xxfv26"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Test suite needs a copy of the Julia package repository.
    (native-inputs
     `(("julia-example" ,julia-example)))
    (home-page "https://github.com/JuliaLang/PackageCompiler.jl")
    (synopsis "Compile your Julia Package")
    (description "PackageCompiler is a Julia package with two main purposes:
@itemize
@item Creating custom sysimages for reduced latency when working locally with
packages that has a high startup time.
@item Creating \"apps\" which are a bundle of files including an executable that
can be sent and run on other machines without Julia being installed on that machine.
@end itemize")
    (license license:expat)))

(define-public julia-flxqtl
  (package
    (name "julia-flxqtl")
    (version "0.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/senresearch/FlxQTL.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "06mzbiv8bp0hgkz2i67ax63xcs5wkbyw80cz3s9snwd1426v2r41"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f
       ))
    (propagated-inputs
     `(
       ("julia-distributions" ,julia-distributions)
       ;("julia-lossfunctions" ,julia-lossfunctions)
       ("julia-pyplot" ,julia-pyplot)
       ;("julia-revise" ,julia-revise)
       ("julia-staticarrays" ,julia-staticarrays)
       ("julia-statsbase" ,julia-statsbase)
       ))
    (home-page "https://github.com/senresearch/FlxQTL.jl")
    (synopsis "QTL analysis tool by multivariate mixed linear model")
    (description "FlxQTL.jl is a a package for a multivariate linear mixed model
based QTL analysis tool that supports incorporating information from trait
covariates such as time or different environments.  The package supports
computation of one-dimensional and two-dimensional multivariate genome scans,
visualization of genome scans, support for @acronym{LOCO,
leave-one-chromosome-out}, computation of kinship matrices, and support for
distributed computing.")
    (license license:gpl3)))

(define-public julia-conda
  (package
    (name "julia-conda")
    (version "1.5.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPy/Conda.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "01qxrv3xv7b979j760iyn28w3hlls29b2parjginwi81jbzr1vgd"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f  ; Tests involve downloading Conda packages.
       #:phases
       (modify-phases %standard-phases
         ;(delete 'precompile)
         (add-after 'link-depot 'hardcode-conda-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/Conda.jl"
               (("bin_dir\\(ROOTENV\\), \"conda\"") (string-append "\"" (assoc-ref inputs "conda") "/bin/\", \"conda\"")))))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;(setenv "CONDA_JL_HOME" (string-append (assoc-ref inputs "conda") ))
             ;(setenv "CONDA_JL_VERSION" "3")
             ;(setenv "CONDA_JL_USE_MINIFORGE" "false")
             ;(display (getcwd))
             ;(setenv "HOME" (getcwd))
             ;(invoke "conda" "create" "--offline" "--use-local" "-n" "conda_jl" "python" "conda")
             ;(invoke "ls")
             ;(invoke "pwd")
             ;(with-output-to-file "deps/deps.jl"
             ;  (lambda () #t))
             ;(invoke "touch" "deps/deps.jl")
             ;(invoke "julia" "-e" "using Pkg; Pkg.add(\"Conda\")")
             ;(invoke "julia" "-e" "using Pkg; Pkg.build(\"Conda\")")
             ;; From .travis.yml
             ;(setenv "CONDA_JL_VERSION" "3")
             ;(setenv "CONDA_JL_USE_MINIFORGE" "false")
             #t)))
       ))
    (native-inputs
     `(
       ("conda" ,(S "conda"))
       ;("python" ,(S "python-wrapper"))
       ))
    (propagated-inputs
     `(
       ("julia-json" ,julia-json)
       ("julia-versionparsing" ,julia-versionparsing)
       ))
    (home-page "https://github.com/JuliaPy/Conda.jl")
    (synopsis "Conda managing Julia binary dependencies")
    (description "This package allows one to use @code{conda} as a cross-platform binary provider for Julia for other Julia packages, especially to install binaries that have complicated dependencies like Python.")
    (license license:expat)))

(define-public julia-distributions
  (package
    (name "julia-distributions")
    (version "0.25.80")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/Distributions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0nqlnkh8grxfm8d1mivi7dnrvb31bznj9s540a10d2v396ikfggn"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))        ; Some failed tests
    (propagated-inputs
     `(("julia-chainrulescore" ,julia-chainrulescore)
       ("julia-densityinterface" ,julia-densityinterface)
       ("julia-fillarrays" ,julia-fillarrays)
       ("julia-pdmats" ,julia-pdmats)
       ("julia-quadgk" ,julia-quadgk)
       ("julia-specialfunctions" ,julia-specialfunctions)
       ("julia-statsbase" ,julia-statsbase)
       ("julia-statsfuns" ,julia-statsfuns)))
    (native-inputs
     `(("julia-calculus" ,julia-calculus)
       ("julia-chainrulestestutils" ,julia-chainrulestestutils)
       ("julia-finitedifferences" ,julia-finitedifferences)
       ("julia-forwarddiff" ,julia-forwarddiff)
       ("julia-json" ,julia-json)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-stablerngs" ,julia-stablerngs)
       ("julia-staticarrays" ,julia-staticarrays)))
    (home-page "https://github.com/JuliaStats/Distributions.jl")
    (synopsis "Probability distributions and associated functions")
    (description "Julia package for probability distributions and associated
functions.  Particularly, @code{Distributions} implements:
@enumerate
@item Moments (e.g mean, variance, skewness, and kurtosis), entropy, and other
properties
@item Probability density/mass functions (pdf) and their logarithm (logpdf)
@item Moment generating functions and characteristic functions
@item Sampling from population or from a distribution
@item Maximum likelihood estimation
@end enumerate")
    (license license:expat)))

(define-public julia-fromfile
  (package
    (name "julia-fromfile")
    (version "0.1.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Roger-luo/FromFile.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0jn394294d4s1fqsfiwpiy6dzlca4ciy54l2xviwyzhvixmlfrwn"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f))            ; Tests require network access
    (propagated-inputs
     (list julia-requires))
    (home-page "https://github.com/Roger-luo/FromFile.jl")
    (synopsis "Julia enhancement proposal for implicit per file module in Julia")
    (description "This package exports a macro @code{@@from}, which can be used
to import objects from files.")
    (license license:expat)))

(define-public julia-git
  (package
    (name "julia-git")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaVersionControl/Git.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0bznzg360cbvbzzpsdkin4dm2v980sb5pv58gy1bp3j9j8bj38h6"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-network-tests
           (lambda _
             (substitute* "test/runtests.jl"
               (("testset.*Git.jl.*" all)
                (string-append all " return\n"))))))))
    (propagated-inputs
     (list julia-git-jll))
    (home-page "https://github.com/JuliaVersionControl/Git.jl")
    (synopsis "Use command-line Git in your Julia packages")
    (description "Git.jl allows you to use command-line Git in your Julia
packages.  You do not need to have Git installed on your computer, and neither
do the users of your packages!")
    (license license:expat)))

(define-public julia-git-jll
  (package
    (name "julia-git-jll")
    (version "2.36.1+2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/Git_jll.jl")
               (commit (string-append "Git-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1n8k363q2mw5qfkagbbqpn4djx181qbn05l7brp3spspvf5aslgm"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f  ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
               (lambda (wrapper)
                 (substitute* wrapper
                   (("generate_wrapper_header.*")
                    (string-append
                      "generate_wrapper_header(\"Git\", \""
                      (assoc-ref inputs "git-minimal") "\")\n"))))
               ;; There's a Julia file for each platform, override them all
               (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     (list git-minimal))
    (propagated-inputs
     (list julia-jllwrappers
           julia-expat-jll
           julia-openssl-jll
           ;julia-libcurl-jll
           julia-libiconv-jll
           ;julia-pcre2-jll
           julia-zlib-jll))
    (home-page "https://github.com/JuliaBinaryWrappers/Git_jll.jl")
    (synopsis "Git library wrappers")
    (description "This package provides a wrapper for the git library.")
    (license license:expat)))

(define-public julia-terminalloggers
  (package
    (name "julia-terminalloggers")
    (version "0.1.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLogging/TerminalLoggers.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "01l9qsjljwa9pkpvf1pafmbrncjddwdw3737pf44y9q5hrp4hbr7"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-leftchildrightsiblingtrees
           julia-progresslogging))
    (home-page "https://github.com/JuliaLogging/TerminalLoggers.jl")
    (synopsis "Logging sinks and utilites for interactive terminals")
    (description "TerminalLoggers provides a logger type @code{TerminalLogger}
which can format your log messages in a richer way than the default
@code{ConsoleLogger} which comes with the julia standard @code{Logging}
library.")
    (license license:expat)))

(define-public julia-roots
  (package
    (name "julia-roots")
    (version "2.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Roots.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "17d62r8pib3cp73az00iz8nbnd5j0y3dl1mrmcssnj4ln8043056"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-expensive-tests
           (lambda _
             (substitute* "test/runtests.jl"
               ((".*test_composable.*") "")))))))
    (propagated-inputs
     (list julia-commonsolve
           julia-setfield))
    (native-inputs
     (list julia-benchmarktools
           julia-forwarddiff
           julia-json
           julia-specialfunctions))
    (home-page "http://juliamath.github.io/Roots.jl/")
    (synopsis "Root finding functions for Julia")
    (description "This package contains simple routines for finding roots, or
zeros, of scalar functions of a single real variable using floating-point
math.")
    (license license:expat)))

(define-public julia-commonsolve
  (package
    (name "julia-commonsolve")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/CommonSolve.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1w05fp02g5cmqzqp96hcpriwjpqx61sl481rj92gf4y4xpinmdf5"))))
    (build-system julia-build-system)
    (home-page "https://docs.sciml.ai/CommonSolve/stable")
    (synopsis "Common solve function for scientific machine learning")
    (description "This holds the common @code{solve}, @code{init},
@code{step!}, and @code{solve!} commands.  By using the same definition, solver
libraries from other completely different ecosystems can extend the functions
and thus not clash with SciML if both ecosystems export the @code{solve}
command.  The rules are that you must dispatch on one of your own types.")
    (license license:expat)))

(define-public julia-setfield
  (package
    (name "julia-setfield")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jw3126/Setfield.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0vdn9s639f120560599m4lhqkk0nsx7qa87fv2mixs1faaihsf6l"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))        ; Skip tests, not all dependencies packaged.
    (propagated-inputs
     (list julia-constructionbase
           julia-macrotools
           julia-staticarrayscore))
    (home-page "https://github.com/jw3126/Setfield.jl")
    (synopsis "Update deeply nested immutable structs")
    (description "Update deeply nested immutable structs.")
    (license license:expat)))

(define-public julia-staticarrayscore
  (package
    (name "julia-staticarrayscore")
    (version "1.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/StaticArraysCore.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0m573mxvf0y9h7y8rxrk35haphhqisplx159r77g30qvw7zwpgar"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaArrays/StaticArraysCore.jl")
    (synopsis "Interface package for StaticArrays.jl")
    (description
     "This package provides an interface package for @code{StaticArrays.jl}.")
    (license license:expat)))

(define-public julia-progresslogging
  (package
    (name "julia-progresslogging")
    (version "0.1.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLogging/ProgressLogging.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ysixnl7wjdykaka892jrhsay2aivpch83h8dnfs43z2xynf9sqn"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/JuliaLogging/ProgressLogging.jl")
    (synopsis "Define progress logs")
    (description "ProgressLogging.jl is a package for defining progress logs.
It can be used to report progress of a loop/loops with time-consuming body.")
    (license license:expat)))

(define-public julia-leftchildrightsiblingtrees
  (package
    (name "julia-leftchildrightsiblingtrees")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaCollections/LeftChildRightSiblingTrees.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1rijr0yqmbzhlk668v4rqw5xmxxzqvgziasyqrd6z837376mfyy6"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-abstracttrees))
    (home-page "https://github.com/JuliaCollections/LeftChildRightSiblingTrees.jl")
    (synopsis "Representation of a tree with arbitrary number of children/node")
    (description "A left child, right sibling tree (frequently abbreviated as
\"LCRS\") is a rooted tree data structure that allows a parent node to have
multiple child nodes.  Rather than maintain a list of children (which requires
one array per node), instead it is represented as a binary tree, where the
\"left\" branch is the first child, whose \"right\" branch points to its first
sibling.")
    (license license:expat)))

;; ready to upstream
(define-public julia-densityinterface
  (package
    (name "julia-densityinterface")
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/DensityInterface.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "10yr69lndh4jdyhjnpm421zvbw8v48bimxjawz05lqkd7k4w4lw6"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-inversefunctions))
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/JuliaMath/DensityInterface.jl")
    (synopsis "Interface for mathematical/statistical densities")
    (description "This package defines an interface for mathematical/statistical
densities and objects associated with a density in Julia.")
    (license license:expat)))

(define-public julia-plots
  (package
    (name "julia-plots")
    (version "1.35.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/Plots.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "005za8a8al822xr0cz4yi1rm23qj1lzy9iw93vvhwq76x3n8bfpy"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f  ; for now
       ))
    (propagated-inputs
     (list
       ;julia-contour
       julia-ffmpeg
       julia-fixedpointnumbers
       julia-gr
       julia-geometrybasics
       julia-jlfzf
       julia-json
       ;julia-latexify
       julia-measures
       julia-nanmath
       julia-plotthemes
       julia-plotutils
       julia-recipesbase
       julia-recipespipeline
       julia-reexport
       julia-relocatablefolders
       julia-requires
       julia-scratch
       julia-showoff
       julia-snoopprecompile
       julia-statsbase
       julia-unicodefun
       julia-unzip))
    (native-inputs
     (list
       ;julia-distributions
       julia-fileio
       ;julia-gtk
       ;julia-hdf5
       julia-imagemagick
       ;julia-images
       julia-offsetarrays
       ;julia-pgfplotsx
       ;julia-plotlyjs
       ;julia-rdatasets
       julia-stablerngs
       julia-staticarrays
       ;julia-statsplots
       julia-testimages
       ;julia-unicodeplots
       ;julia-visualregressiontests
       ))
    (home-page "http://docs.juliaplots.org/")
    (synopsis "Powerful convenience for Julia visualizations and data analysis")
    (description "Plots is a plotting API and toolset.")
    (license license:expat)))

(define-public julia-snoopprecompile
  (package
    (name "julia-snoopprecompile")
    (version "1.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/SnoopCompile.jl")
               (commit "v2.10.0")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "14nnqknswjzlk786x4r7xc7898nmfk7ijfchrc09i81qi2d0mmcn"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:tests? #f      ; Most tests fail in the build environment.
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "SnoopPrecompile"))))))
    (propagated-inputs
     (list julia-preferences))
    (home-page "https://timholy.github.io/SnoopCompile.jl/dev/")
    (synopsis "Effectively precompile code needed by your package")
    (description "SnoopPrecompile is a small dependency used to effectively
precompile code needed by your package, particularly on Julia 1.8 and higher.")
    (license license:expat)))

(define-public julia-jlfzf
  (package
    (name "julia-jlfzf")
    (version "0.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Moelf/JLFzf.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0fvcx80lzj0xc4imb9h0iybj8s0cazjc1wn9h8xncih9aln25is4"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f))        ; no tests
    (propagated-inputs
     (list julia-fzf-jll
           julia-pipe))
    (home-page "https://github.com/Moelf/JLFzf.jl")
    (synopsis "Julia bind to fzf fuzzy finder")
    (description "Julia bind to fzf fuzzy finder.")
    (license license:expat)))

(define-public julia-pipe
  (package
    (name "julia-pipe")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/oxinabox/Pipe.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "01anm76nfmcajcbr2piji0rknisf5wwhimfndwaadkqkcjv7k4xn"))))
    (build-system julia-build-system)
    (home-page "https://github.com/oxinabox/Pipe.jl")
    (synopsis "Enhancement to Julia piping syntax")
    (description "This package provides an enhancement to Julia piping syntax.")
    (license license:expat)))

(define-public julia-fzf-jll
  (package
    (name "julia-fzf-jll")
    (version "0.35.1+0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/juliabinarywrappers/fzf_jll.jl")
               (commit (string-append "fzf-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0psl1lsvl6sza69g7qh8ha2ir89fibzid4ka3wzmjgh0r4yjfnw0"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
               (lambda (wrapper)
                 (substitute* wrapper
                   (("generate_wrapper_header.*")
                   (string-append
                    "generate_wrapper_header(\"fzf\", \""
                    (assoc-ref inputs "fzf") "\")\n"))))
               ;; There's a Julia file for each platform, override them all
               (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     (list (@ (gnu packages terminals) fzf)))
    (propagated-inputs
     (list julia-jllwrappers))
    (home-page "https://github.com/JuliaBinaryWrappers/fzf_jll.jl")
    (synopsis "Fzf library wrappers")
    (description "This package provides a wrapper for fzf.")
    (license license:expat)))

(define-public julia-unicodefun
  (package
    (name "julia-unicodefun")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SimonDanisch/UnicodeFun.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "11hch4zpi8qby8pcqfvxnx88y45lxan4ind9kj2d0pgw614zkpg0"))))
    (build-system julia-build-system)
    (home-page "https://github.com/SimonDanisch/UnicodeFun.jl")
    (synopsis "Transform text into unicode symbols")
    (description "This package offers a unicode transformation library.")
    (license license:expat)))

(define-public julia-unzip
  (package
    (name "julia-unzip")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/bramtayl/Unzip.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0i4rkvyd8br024s0sgs3crpi6za2f51m42whkx0jd22wiwg675vb"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/bramtayl/Unzip.jl")
    (synopsis "")
    (description "")
    (license license:expat)))

;; Versions after 0.3.11 need a newer version of julia-http
(define-public julia-plutosliderserver
  (package
    (name "julia-plutosliderserver")
    (version "0.3.11")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPluto/PlutoSliderServer.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1aqc836glwqqs1pd9173sbjwjns3zv197xqdqlkp2xgrkjiw2yzg"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f      ; Tests require network access
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-source
           (lambda _
             (setenv "GITHUB_ACTIONS" "false")
             ;; Rip out betterfilewatching, it depends on a rust package.
             (substitute* "src/PlutoSliderServer.jl"
               ((".*watch_folder.*") ""))
             ;; Remove GitHubActions
             (substitute* "src/PlutoSliderServer.jl"
               ((".*GitHubActions.*") "")))))))
    (propagated-inputs
     (list julia-abstractplutodingetjes
           ;julia-betterfilewatching    ; should be disabled?
           julia-configurations
           julia-fromfile
           julia-git
           ;julia-githubactions         ; can be disabled
           julia-http
           julia-json
           julia-pluto
           julia-terminalloggers))
    (home-page "https://github.com/JuliaPluto/PlutoSliderServer.jl")
    (synopsis "Web server to run parts of a Pluto.jl notebook")
    (description "This package provides a web server to run just the
@code{@@bind} parts of a @code{Pluto.jl} notebook.")
    (license license:unlicense)))

;; TODO: unbundle javascript calls to cdn.jsdelivr.net
;; The '-src' commit needs some nodejs packages to build the frontend.
(define-public julia-pluto
  (package
    (name "julia-pluto")
    (version "0.19.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fonsp/Pluto.jl")
               ;(commit (string-append "v" version "-src"))))
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         ;(base32 "0h9sz4mpf3a4k0f5fblbb6j07wdhrnarxajrn0wz6zsq6w30x6yj"))))
         (base32 "1qml5id3fi4r6n7hrqvfjwdvp3zp8xfckir3mw7vwswmkc56jzpq"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f      ; Test suite tries to download the package registry.
       ;#:phases
       #;(modify-phases %standard-phases
         #;(add-before 'check 'pre-check
           (lambda _
             (setenv "PLUTO_TEST_ONLY_COMPILETIMES" "true")))
         (add-after 'link-depot 'dont-check-for-upgrades
           (lambda _
             (substitute* "frontend/components/welcome/Welcome.js"
               ((".*new_update_message.*") "")))))))
    (propagated-inputs
     (list julia-configurations
           julia-fuzzycompletions
           julia-http
           julia-hypertextliteral
           julia-mimes
           julia-msgpack
           julia-precompilesignatures
           julia-relocatablefolders
           julia-tables
           julia-uris))
    (native-inputs
     (list julia-dataframes
           julia-offsetarrays
           julia-timeroutputs))
    (home-page "https://github.com/fonsp/Pluto.jl")
    (synopsis "Simple reactive notebooks for Julia")
    (description "A Pluto notebook is made up of small blocks of Julia code
(cells) and together they form a reactive notebook.  When you change a variable,
Pluto automatically re-runs the cells that refer to it.  Cells can even be
placed in arbitrary order - intelligent syntax analysis figures out the
dependencies between them and takes care of execution.")
    (license license:expat)))

(define-public julia-plutoui
  (package
    (name "julia-plutoui")
    (version "0.7.51")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fonsp/PlutoUI.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0hqaa8wx7mia3krdwhj2yf8aa4a8h4r09j16dxn7nyc0zcz8hgb2"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-abstractplutodingetjes
           julia-colortypes
           julia-fixedpointnumbers
           julia-hyperscript
           julia-hypertextliteral
           julia-iocapture
           julia-json
           julia-mimes
           julia-reexport
           julia-uris))
    (home-page "https://github.com/fonsp/PlutoUI.jl")
    (synopsis "Helper package for Julia Pluto")
    (description "This package helps to make @code{html\"<input>\"} a bit more
native to Julia.  Use it with the @code{@@bind} macro in Pluto.")
    (license license:unlicense)))

(define-public julia-http-1.5
  (package
    (inherit julia-http)
    (name "julia-http")
    (version "1.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaWeb/HTTP.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qcfixsq65g8hdimygam7cd8nvcz6w7nzkkjk98mvf65dcby4593"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'disable-network-tests
            (lambda _
              #;(substitute* "test/runtests.jl"
                (("\"async.jl") "# \"async.jl")
                (("\"client.jl") "# \"client.jl"))
              #;(substitute* "test/aws4.jl"
                (("@testset.*HTTP.request with AWS authentication.*" all)
                 (string-append all "return\n")))
              #;(substitute* "test/insert_layers.jl"
                (("@testset.*Inserted final layer runs handler.*" all)
                 (string-append all "return\n")))
              #;(substitute* "test/multipart.jl"
                (("@testset \"Setting of Content-Type.*" all)
                 (string-append all "return\n"))
                (("@testset \"Deprecation of .*" all)
                 (string-append all "return\n")))
              #;(substitute* "test/websockets.jl"
                (("@testset.*External Host.*" all)
                 (string-append all "return\n")))
              #;(substitute* "test/messages.jl"
                (("@testset.*Read methods.*" all)
                 (string-append all "return\n"))
                (("@testset.*Body - .*" all)
                 (string-append all "return\n"))
                (("@testset.*Write to file.*" all)
                 (string-append all "return\n")))
              #;(substitute* "test/cookies.jl"
                (("@testset.*Set-Cookie casing.*" all)
                 (string-append all "return\n")))
#t)))))
    (propagated-inputs
     (list julia-codeczlib
           julia-inifile
           julia-loggingextras
           julia-mbedtls
           julia-openssl
           julia-simplebufferstream
           julia-uris))
    ;; required for tests
    (native-inputs
     (list julia-bufferedstreams
           julia-json))))

(define-public julia-http-1.0
  (package
    (inherit julia-http)
    (name "julia-http")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaWeb/HTTP.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15bqf0gv4qz3990cv1s2nj9mjqaka1pkhzymgwc19wxqvacwvwqr"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'disable-network-tests
            (lambda _
              (substitute* "test/runtests.jl"
                (("\"async.jl") "# \"async.jl")
                (("\"client.jl") "# \"client.jl")
                (("\"websockets") "# \"websockets")
                (("\"server") "# \"server"))
              (substitute* "test/multipart.jl"
                (("@testset \"Setting of Content-Type.*" all)
                 (string-append all "return\n"))
                (("@testset \"Deprecation of .*" all)
                 (string-append all "return\n")))
              (substitute* "test/messages.jl"
                (("@testset.*Read methods.*" all)
                 (string-append all "return\n"))
                (("@testset.*Body - .*" all)
                 (string-append all "return\n"))
                (("@testset.*Write to file.*" all)
                 (string-append all "return\n")))
              (substitute* "test/cookies.jl"
                (("@testset.*Set-Cookie casing.*" all)
                 (string-append all "return\n")))
)))))
    (propagated-inputs
     (list julia-codeczlib
           julia-inifile
           julia-loggingextras
           julia-mbedtls
           julia-simplebufferstream
           julia-uris))
    ;; required for tests
    (native-inputs
     (list julia-bufferedstreams
           julia-json))))

(define-public julia-simplebufferstream
  (package
    (name "julia-simplebufferstream")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/SimpleBufferStream.jl")
               ;; First commit after 1.1.0 with a license
               (commit "80c9854d5d9ea921da6f619624989fa30e83b8be")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05c4d73ki4cp38g66ljxwbl2d0dni3w05r8xsd6g1v63x2rqqbgn"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'link-depot 'adjust-tests
             (lambda _
               (substitute* "test/runtests.jl"
                 ;; Tests fail when build machine is too *fast*.
                 (("0\\.01") "0.001")
                 ;; Don't create circular dependencies with http
                 (("using HTTP.*") "")
                 (("@testset.*HTTP.jl.*" all)
                  (string-append all "return\n"))))))))
    (home-page "https://github.com/JuliaPackaging/SimpleBufferStream.jl")
    (synopsis "What Base.BufferStream should be")
    (description "This is what I wish Base.BufferStream was.")
    (license license:expat)))

(define-public julia-openssl
  (package
    (name "julia-openssl")
    (version "1.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaWeb/OpenSSL.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1s2h4qh9y3alhkf18p4pjqp77mvsb47qagmk68pq0wsx8r3hzhzx"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'disable-network-tests
            (lambda _
              (substitute* "test/runtests.jl"
                ;; No /etc/ssl/certs/ca-certificates.crt in the build environment.
                (("@testset.*(ReadPEMCert|X509Certificate|X509Store).*" all)
                 (string-append all "return\n"))
                ;; No network connection
                (("@testset.*(HttpsConnect|ClosedStream|NoCloseStream).*" all)
                 (string-append all "return\n"))
                ;; undefined symbol.
                ;; XXX: THIS BREAKS THE PACKAGE!
                (("@testset.*ErrorTaskTLS.*" all)
                 (string-append all "return\n"))))))))
    (propagated-inputs
     (list julia-bitflags
           julia-openssl-jll))
    (home-page "https://github.com/JuliaWeb/OpenSSL.jl")
    (synopsis "Openssl Julia bindings")
    (description "This package provides Openssl Julia bindings.")
    (license license:expat)))

#;(define-public julia-openssl-jll
  (package
    (name "julia-openssl-jll")
    (version "3.0.8+0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/OpenSSL_jll.jl")
               (commit (string-append "OpenSSL-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1d9kcx7a3yv3rgkggq7h436sfjafr66pq8d0lmlcjxdpl46hx3j7"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
               (lambda (wrapper)
                 (substitute* wrapper
                   (("lib64") "lib")
                   (("generate_wrapper_header.*")
                   (string-append
                    "generate_wrapper_header(\"OpenSSL\", \""
                    (assoc-ref inputs "openssl") "\")\n"))))
               ;; There's a Julia file for each platform, override them all
               (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     (list (@ (gnu packages tls) openssl)))
    (propagated-inputs
     (list julia-jllwrappers))
    (home-page "https://github.com/JuliaBinaryWrappers/OpenSSL_jll.jl")
    (synopsis "Openssl library wrappers")
    (description "This package provides a wrapper for openssl.")
    (license license:expat)))

#;(define-public julia-openssl-jll-1.1
  (package
    (name "julia-openssl-jll")
    (version "1.1.21+0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/OpenSSL_jll.jl")
               (commit (string-append "OpenSSL-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05wgsvy0iviffkcq4w1dg8gaafyxknnk26c1gpcgma5yhfia8yfs"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
               (lambda (wrapper)
                 (substitute* wrapper
                   (("generate_wrapper_header.*")
                   (string-append
                    "generate_wrapper_header(\"OpenSSL\", \""
                    (assoc-ref inputs "openssl") "\")\n"))))
               ;; There's a Julia file for each platform, override them all
               (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     (list (@ (gnu packages tls) openssl-1.1)))
    (propagated-inputs
     (list julia-jllwrappers))
    (home-page "https://github.com/JuliaBinaryWrappers/OpenSSL_jll.jl")
    (synopsis "Openssl library wrappers")
    (description "This package provides a wrapper for openssl.")
    (license license:expat)))

(define-public julia-bitflags
  (package
    (name "julia-bitflags")
    (version "0.1.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jmert/BitFlags.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wphfpwqm7bsff68d6h9yhhxrh1lq2shyl0afbzp0h5czf6qi2xr"))))
    (build-system julia-build-system)
    (home-page "https://github.com/jmert/BitFlags.jl")
    (synopsis "Enum-like type for bit flag option values")
    (description "@code{BitFlag.jl} provides an @code{Enum}-like type for bit
flag option values.")
    (license license:expat)))

(define-public julia-relocatablefolders
  (package
    (name "julia-relocatablefolders")
    (version "0.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/RelocatableFolders.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "116f98y2w5cqkv2fvddmv54422cpby6d7q1dndgqh2rdlkpb44iw"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-scratch))
    (home-page "https://github.com/JuliaPackaging/RelocatableFolders.jl")
    (synopsis "Reference packages in their project directory")
    (description "An alternative to the @code{@@__DIR__} macro.  Packages that
wish to reference paths in their project directory run into issues with
relocatability when used in conjunction with @code{PackageCompiler}.")
    (license license:expat)))

(define-public julia-scratch
  (package
    (name "julia-scratch")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/Scratch.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0s7dmqsskq5vy7sk6pxng5vrznrn6msg7xf8a9kj3a03b617pgap"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))        ; Tries to download the Julia registry.
    (home-page "https://github.com/JuliaPackaging/Scratch.jl")
    (synopsis "Scratch spaces for all your persistent mutable data needs")
    (description "This repository implements the scratch spaces API for
package-specific mutable containers of data.  These spaces can contain
datasets, text, binaries, or any other kind of data that would be convenient to
store in a location specific to your package.  As compared to Artifacts, these
containers of data are mutable.  Because the scratch space location on disk is
not very user-friendly, scratch spaces should, in general, not be used for a
storing files that the user must interact with through a file browser.  In that
event, packages should simply write out to disk at a location given by the
user.  Scratch spaces are designed for data caches that are completely managed
by a package and should be removed when the package itself is uninstalled.")
    (license license:expat)))

(define-public julia-hypertextliteral
  (package
    (name "julia-hypertextliteral")
    (version "0.9.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPluto/HypertextLiteral.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "06nzgxrl1aac9bqb37l2lak2aicp9h7fd1ijcva9pq80z0d74a8a"))))
    (build-system julia-build-system)
    (arguments (list #:tests? #f))      ; Disable tests until all inputs are pacakged.
    (propagated-inputs
     (list julia-tricks))
    (native-inputs
     (list julia-documenter
           ;julia-faker
           julia-hyperscript
           ;julia-narrativetest
           ))
    (home-page "https://juliapluto.github.io/HypertextLiteral.jl/stable/")
    (synopsis "Julia library for the string interpolation of HTML and SVG")
    (description "HypertextLiteral is a Julia package for generating HTML, SVG,
and other SGML tagged content.  It works similar to Julia string interpolation,
only that it tracks hypertext escaping needs and provides handy conversions
dependent upon context.")
    (license license:expat)))

;; This can be removed with julia-1.10
(define-public julia-tricks
  (package
    (name "julia-tricks")
    (version "0.1.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/oxinabox/Tricks.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0870hiqpl14wpr2v16wl6fw89r469yjrchv6gn4pfw2irw3nkjip"))))
    (build-system julia-build-system)
    (home-page "https://github.com/oxinabox/Tricks.jl")
    (synopsis "Cunning tricks though the julia compiler internals")
    (description "Tricks.jl is an particularly cunning package that does tricks
with the Julia edge system.")
    (license license:expat)))

(define-public julia-registryinstances
  (package
    (name "julia-registryinstances")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/GunnarFarneback/RegistryInstances.jl")
               (commit "2796d959014475bc19e2dfa174179cdf02642d28")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "09926dy2s4wcml6s2hrbq1j1r1456d61fvk5fma4sbr9qsvpyyd0"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))        ; TODO:   Got exception outside of a @test
    (propagated-inputs
     (list julia-lazilyinitializedfields))
    (home-page "https://github.com/GunnarFarneback/RegistryInstances.jl")
    (synopsis "Access the information in installed Julia registries")
    (description "Julia's package manager stores package metadata in registries,
which consist of TOML files in a directory structure.")
    (license license:expat)))

(define-public julia-lazilyinitializedfields
  (package
    (name "julia-lazilyinitializedfields")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/KristofferC/LazilyInitializedFields.jl")
               (commit "53ed8cbe78b2048105a0e0b355294e7f024e3d14")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1jwpka27d873cyn0vxrg7h7ns7fmlqlyx5h38cw6zvfbdhkrmi09"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/KristofferC/LazilyInitializedFields.jl")
    (synopsis "Handle lazily initialized fields")
    (description "A package for handling lazily initialized fields.")
    (license license:expat)))

(define-public julia-mimes
  (package
    (name "julia-mimes")
    (version "0.1.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaWeb/MIMEs.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0vgc2q2mgbrm16px7cbqsrga9l99djlb1ayg5k1djb0mbnprjajk"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaWeb/MIMEs.jl")
    (synopsis "MIME information: filetype, encoding, gzip")
    (description "A small package to transform between file extensions and MIME
types, with bonus features.")
    (license license:expat)))

(define-public julia-loggingextras
  (package
    (name "julia-loggingextras")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLogging/LoggingExtras.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0mc0bbr2wsa809q74hg4npdw2xcni4xl40zz50i6djwnxq88yl07"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaLogging/LoggingExtras.jl")
    (synopsis "Composable Loggers for the Julia Logging StdLib")
    (description "LoggingExtras is designs around allowing you to build
arbitrarily complicated systems for \"log plumbing\".  That is to say basically
routing logged information to different places.  It is built around the idea of
simple parts which are composed together, to allow for powerful and flexible
definition of your logging system.")
    (license license:expat)))

(define-public julia-precompilesignatures
  (package
    (name "julia-precompilesignatures")
    (version "3.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rikhuijzer/PrecompileSignatures.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "11rjdx8n3q10iis7ryzbvlvgdpi6kd2h53qp7lac02yx6rsgsfx1"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; cycle with Pluto.jl.
    ;(native-inputs
    ; (list julia-pluto))
    (home-page "https://github.com/rikhuijzer/PrecompileSignatures.jl")
    (synopsis "Generate precompile directives by reading method signatures")
    (description "This package reads all method signatures in a package and
generates precompile directives for any concrete signature that it can find.")
    (license license:expat)))

(define-public julia-precompiletools
  (package
    (name "julia-precompiletools")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLang/PrecompileTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "124qzflwnn34d8x8vz3cmj2m4a55mg5qf8i8jdcwyyrnag3si7zr"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))        ; Tries to download the Julia registry.
    (propagated-inputs
     (list julia-preferences))
    (home-page "https://github.com/JuliaLang/PrecompileTools.jl")
    (synopsis "Reduce time-to-first-execution of Julia code")
    (description "PrecompileTools allows you to reduce the latency of the first
execution of Julia code.  It is applicable for package developers and for
\"ordinary users\" in their personal workflows.")
    (license license:expat)))

(define-public julia-timeroutputs
  (package
    (name "julia-timeroutputs")
    (version "0.5.23")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/KristofferC/TimerOutputs.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "031m7d837cw4c7iz4arrm1a1ppqihhj5jsldvm7z1bc72jxgfrcv"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-exprtools))
    (home-page "https://github.com/KristofferC/TimerOutputs.jl")
    (synopsis "Formatted output of timed sections in Julia")
    (description "TimerOutputs is a small Julia package that is used to generate
formatted output from timings made in different sections of a program.  It's
main functionality is the @code{@@timeit} macro, similar to the @code{@@time}
macro in Base except one also assigns a label to the code section being timed.
Multiple calls to code sections with the same label (and in the same \"scope\")
will accumulate the data for that label.  After the program has executed, it is
possible to print a nicely formatted table presenting how much time,
allocations and number of calls were made in each section.  The output can be
customized as to only show the things you are interested in.")
    (license license:expat)))

(define-public julia-memoize
  (package
    (name "julia-memoize")
    (version "0.4.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaCollections/Memoize.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0mfavihgdmh6zzjx5lzbigmgsa50pa9ik9gk2gq9wam26prkmkba"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-macrotools))
    (home-page "https://github.com/JuliaCollections/Memoize.jl")
    (synopsis "Memoize macro for Julia")
    (description "Easy memoization for Julia.")
    (license license:bsd-0)))

(define-public julia-abstractplutodingetjes
  (package
    (name "julia-abstractplutodingetjes")
    (version "1.1.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPluto/AbstractPlutoDingetjes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "03grnv26n3qdadb2awv3d0rs655l5hhmh7h43v949za9aaaz89j3"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))        ; No tests for this package.
    (home-page "https://docs.juliahub.com/AbstractPlutoDingetjes/UHbnu/")
    (synopsis "Abstract package for creating widgets in Pluto.jl")
    (description "An abstract package to be implemented by packages/people who
create widgets (or other dingetjes) for Pluto.")
    (license license:unlicense)))

(define-public julia-hyperscript
  (package
    (name "julia-hyperscript")
    (version "0.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaWeb/Hyperscript.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0flap3a9p6fr768kjfk1lkyfgjqcyhzvzl7in7gjszfy04pqj55w"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaWeb/Hyperscript.jl")
    (synopsis "Lightweight DOM representation for Julia")
    (description "Hyperscript is a package for working with HTML, SVG, and CSS
in Julia.  When using this library you automatically get:
@enumerate
@item A concise DSL for writing HTML, SVG, and CSS.
@item Flexible ways to combine DOM pieces together into larger components.
@item Safe and automatic HTML-escaping.
@item Lightweight and optional support for scoped CSS.
@item Lightweight and optional support for CSS unit arithmetic.
@end enumerate")
    (license license:expat)))

;; ready to upstream
(define-public julia-statsfuns
  (package
    (name "julia-statsfuns")
    (version "0.9.18")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/StatsFuns.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1y71gz4skp6hxw8k5vjbjayplxmdfh3m3yjfw4ggi0azav6c9hrk"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-chainrulescore
           julia-inversefunctions
           julia-irrationalconstants
           julia-logexpfunctions
           julia-reexport
           julia-rmath
           julia-specialfunctions))
    (native-inputs
     (list julia-chainrulestestutils
           julia-forwarddiff))
    (home-page "https://github.com/JuliaStats/StatsFuns.jl")
    (synopsis "Mathematical functions related to statistics")
    (description "This package provides a collection of mathematical constants
and numerical functions for statistical computing.")
    (license license:expat)))

;; ready to upstream
(define-public julia-inversefunctions
  (package
    (name "julia-inversefunctions")
    (version "0.1.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/InverseFunctions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05g9f6i735x7syfr56l4yf4fy71kgdisjc6cfxi4jkf46iq86a69"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/JuliaMath/InverseFunctions.jl")
    (synopsis "Interface for function inversion in Julia")
    (description
     "This package provides an interface to invert functions in Julia.")
    (license license:expat)))

;; ready to upstream
(define-public rmath-for-julia-rmath-jll
  ;; More recent commits fix various build issues
  (let ((commit "5c5dfd6baca358103fbb47cc03dc0ecee04fb1ff")
        (revision "1"))
    (package
      (name "rmath-julia")
      (version (git-version "0.3.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/JuliaStats/Rmath-julia")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "04lf8gfnfcppckk9d7hss0ja91yxaax6qz1gzqya9w0shjr386s5"))))
      (build-system julia-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'precompile)     ; No Project.toml.
           (delete 'link-depot)     ; Not really needed on this package.
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "julia" "test.jl"))))
           (add-before 'install 'build
             (lambda _
               (invoke "make")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "src/libRmath-julia.so" (string-append out "/lib"))))))))
      (home-page "https://github.com/JuliaStats/Rmath-julia")
      (synopsis "Rmath library from R")
      (description "This is a slightly modified version of the standalone Rmath
library from R, built to be used with the @code{Rmath.jl} Julia package.
The main difference is that it is built to allow defining custom random number
generating functions via C function pointers (see @code{include/callback.h}).
When using the library, these should be defined before calling any of the random
functions.")
      (properties '((hidden? . #t)))
      (license license:gpl2))))

;; ready to upstream
(define-public julia-rmath
  (package
    (name "julia-rmath")
    (version "0.7.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/Rmath.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0cam16ff4v2fl7c9j1wx2ahgjhwba9mk2q6qv3zdknnnqj6w664s"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'link-to-librmath-directly
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((rmath    (assoc-ref inputs "rmath"))
                    (librmath (string-append rmath "/lib/libRmath-julia.so")))
               (substitute* "src/Rmath.jl"
                 (("libRmath\\)") (string-append "\"" librmath "\")")))))))))
    (propagated-inputs
     `(("julia-rmath-jll" ,julia-rmath-jll)))
    (inputs
     `(("rmath" ,rmath-for-julia-rmath-jll)))
    (native-inputs
     `(("rmath" ,rmath-for-julia-rmath-jll)))
    (home-page "https://github.com/JuliaStats/Rmath.jl")
    (synopsis "Emulate R's d-p-q-r functions for probability distributions")
    (description "This package provides an archive of functions that emulate
R's d-p-q-r functions for probability distributions.  It is a wrapper around
@code{rmath} for Julia.")
    (license license:expat)))

;; ready to upstream
(define-public julia-rmath-jll
  (package
    (name "julia-rmath-jll")
    (version "0.3.0+0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/Rmath_jll.jl")
               (commit (string-append "Rmath-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1idshxhj60bvra4i1xxpyh2prx1lq9nfrvlc47b68jixfpfkxrlg"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f  ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
              (lambda (wrapper)
                (substitute* wrapper
                  (("generate_wrapper_header.*")
                   (string-append
                     "generate_wrapper_header(\"Rmath\", \""
                     (assoc-ref inputs "rmath") "\")\n"))))
              ;; There's a Julia file for each platform, override them all
              (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     `(("rmath" ,rmath-for-julia-rmath-jll)))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)))
    (home-page "https://github.com/JuliaBinaryWrappers/Rmath_jll.jl")
    (synopsis "Rmath library wrappers")
    (description "This package provides a wrapper for Rmath.")
    (license license:expat)))

;; ready to upstream
(define-public julia-doublefloats
  (package
    (name "julia-doublefloats")
    (version "1.1.25")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/DoubleFloats.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0cd3wxa2q77d9cxmi8gp5hpc95bfdf195mw03p6r1z543sfcfg0d"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-genericlinearalgebra
           julia-polynomials
           julia-quadmath
           julia-requires
           julia-specialfunctions))
    (native-inputs
     (list julia-genericlinearalgebra
           julia-genericschur
           julia-specialfunctions))
    (home-page "https://github.com/JuliaMath/DoubleFloats.jl")
    (synopsis "Extended precision float and complex types")
    (description "This package provides a math library with extended precision
floats and complex types.")
    (license license:expat)))

;; ready to upstream
;; This package depends on TimeZones.jl.
(define-public julia-polynomials
  (package
    (name "julia-polynomials")
    (version "2.0.24")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Polynomials.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wpnk8m4l3ai7ggiim23xlvaxgbsjhprk614pva2pb4xi2jmclgq"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-intervals
           julia-mutablearithmetics
           julia-recipesbase))
    (native-inputs
     (list julia-dualnumbers
           julia-offsetarrays
           julia-specialfunctions
           julia-staticarrays))
    (home-page "https://github.com/JuliaMath/Polynomials.jl")
    (synopsis "Polynomial manipulations in Julia")
    (description "This package provides basic arithmetic, integration,
differentiation, evaluation, and root finding over dense univariate
polynomials.")
    (license license:expat)))

;; This package depends on TimeZones.jl.
(define-public julia-intervals
  (package
    (name "julia-intervals")
    (version "1.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/invenia/Intervals.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1nj40fjx0q3gszq0r8h6scnzyldp68yv1y2lpmmbxraf47644q0n"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; TODO: Fix! broken with timezone stuff
    (propagated-inputs
     (list julia-infinity
           julia-recipesbase
           julia-timezones))
    (native-inputs
     (list julia-documenter
           julia-imagemagick
           julia-infinity
           ;julia-plots
           ;julia-visualregressiontests
           ))
    (home-page "https://github.com/invenia/Intervals.jl")
    (synopsis "Non-iterable ranges")
    (description "This package defines:

                     AbstractInterval, along with its subtypes Interval and AnchoredInterval, and also Bound.")
    (license license:expat)))

(define-public julia-infinity
  (package
    (name "julia-infinity")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cjdoris/Infinity.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1941lwvrdjnrynigzixxin3chpg1ba6xplvcwc89x0f6z658hwmm"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:tests? #f           ; TODO: Fix tests!
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'remove-timezones.jl
            (lambda _
              (substitute* "test/runtests.jl"
                (("using TimeZones.*") "")
                ((".*infextendedtime.*") "")))))))
    (propagated-inputs
     (list julia-requires))
    (native-inputs
     (list julia-compat))
    (home-page "https://docs.juliahub.com/Infinity/")
    (synopsis "Representation of infinity in Julia")
    (description "This package provides representations for infinity and
negative infinity in Julia.")
    (license license:expat)))

;; TODO: There is talk upstream about separating out the timezone data into a
;; separate package which can allow this to actually be packaged in a sane way.
;; As of 1.7.1 there are 257 items in Artifact.toml
;; https://github.com/JuliaTime/TimeZones.jl/issues/359
;; Versions after 1.5.x introduce a dependency on julia-inlinestrings, which needs help with its test suite.
(define-public julia-timezones
  (package
    (name "julia-timezones")
    (version "1.5.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaTime/TimeZones.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1r6vnhfykw4zmpq1nahv9zflsd4gnpdj84admnhd0pnbrfwv8prw"))))
    (build-system julia-build-system)
    (arguments
     (list
       ;; Tests attempt to download timezone information
       ;; In its current form this is basically a source-only package.
       #:tests? #f))
    (propagated-inputs
     (list
       ;julia-inlinestrings
       julia-mocking
       julia-recipesbase))
    (home-page "https://github.com/JuliaTime/TimeZones.jl")
    (synopsis "IANA time zone database access for Julia")
    (description "IANA time zone database access for the Julia programming
language.  @code{TimeZones.jl} extends the Date/DateTime support for Julia to
include a new time zone aware @code{TimeType: ZonedDateTime}.")
    (license license:expat)))

(define-public julia-inlinestrings
  (package
    (name "julia-inlinestrings")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStrings/InlineStrings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fwbnjrsig0fbi9vjn8apbn186xvzhk7vnw3g661q3g8f2j0snp9"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:tests? #f      ; TODO: Fix skipping the test in question
       ;#:phases
       ;#~(modify-phases %standard-phases
       ;    (add-after 'unpack 'adjust-tests
       ;      (lambda _
       ;        (substitute* "test/runtests.jl"
       ;          ;; Need to skip lines 165, 166?
       ;          ((".*a,\".*") "")  ; 165, 167
       ;          ((".*a__\".*") "") ; 166, 168
       ;          ))))
       ))
    (propagated-inputs
     (list julia-parsers))
    (home-page "https://github.com/JuliaStrings/InlineStrings.jl")
    (synopsis "Fixed-width string types for Julia")
    (description "This package provides fixed-width string types for
facilitating certain string workflows in Julia.")
    (license license:expat)))

;; ready to upstream, if we want
(define-public julia-lazyartifacts
  (package
    (name "julia-lazyartifacts")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/LazyArtifacts.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0qln5pq2zm68wgm104cxyviiql8xkv7rf68hivar2b7x2a8vwnk0"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaPackaging/LazyArtifacts.jl")
    (synopsis "LazyArtifacts support for older versions of Julia")
    (description "This is a wrapper package meant to bridge the gap for packages
that want to use the @code{LazyArtifacts} stdlib as a dependency within packages
that still support Julia versions older than 1.6.")
    (license license:expat)))

(define-public julia-visualregressiontests
  (package
    (name "julia-visualregressiontests")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/VisualRegressionTests.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0fsqm89dqrn9bd466v79544hcd5ljn5ikg6x94hfcashjwa5y0g2"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))   ; GTK.jl not packaged yet
    (propagated-inputs
     `(("julia-colortypes" ,julia-colortypes)
       ("julia-colorvectorspace" ,julia-colorvectorspace)
       ("julia-fileio" ,julia-fileio)
       ;("julia-imagefiltering" ,julia-imagefiltering)
       ("julia-imagemagick" ,julia-imagemagick)
       ;("julia-quartzimageio" ,julia-quartzimageio)
       ("julia-requires" ,julia-requires)))
    ;(native-inputs
    ; `(;("julia-gtk" ,julia-gtk)
    ;   ("julia-plots" ,julia-plots)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://github.com/JuliaPlots/VisualRegressionTests.jl")
    (synopsis "Automated integrated regression tests for graphics libraries")
    (description "Easy regression testing for visual packages.  Automated tests compare similarity between a newly generated image and a reference image using the Images package.  While in interactive mode, the tests can optionally pop up a Gtk GUI window showing a side-by-side comparison of the test and reference image, and then optionally overwrite the reference image with the test image.  This allows for straightforward regression testing of image data, even when the \"correct\" images change over time.")
    (license license:expat)))

;; https://github.com/JuliaPackaging/Yggdrasil/tree/7e9ec714d786c4c841a80bdf75b84570c5bda7a1/E/EarCut
(define-public julia-earcut-jll
  ;; The only release tag contains just a license file.
  (let ((commit "b234ae0c064af12eb5482c7474a64af8be0f511e")
        (revision "1"))
  (package
    (name "julia-earcut-jll")
    (version (git-version "2.1.5+1" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaBinaryWrappers/EarCut_jll.jl")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x1zjmsm4kcccwhd1fmnz0w4m0f2na1d2vcc0pj2cf5ccprx7miw"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f                      ; no runtests.jl
       #:phases
       (modify-phases %standard-phases
         (add-after 'link-depot 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
              (lambda (wrapper)
                (substitute* wrapper
                  (("generate_wrapper_header.*")
                   (string-append
                    "generate_wrapper_header(\"EarCut\", \""
                    (assoc-ref inputs "earcut") "\")\n"))))
              ;; There's a Julia file for each platform, override them all.
              (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs                             ;required by artifacts
     `(("earcut" ,earcut-for-julia-earcut-jll)))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)))
    (home-page "https://github.com/JuliaBinaryWrappers/EarCut_jll.jl")
    (synopsis "")
    (description "")
    (license license:expat))))

(define-public earcut-for-julia-earcut-jll
  (package
    (name "earcut-for-julia-earcut-jll")
    (version "2.2.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mapbox/earcut.hpp")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "19qpmpz2sh80lqpvvn08r0k28s3v0vlzb72kg3b56rcwxxknkk5l"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (substitute* "CMakeLists.txt"
              ((".*add_subdirectory.*") ""))))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let ((out (assoc-ref %outputs "out"))
             (source (assoc-ref %build-inputs "source"))
             (cwrapper.cpp (assoc-ref %build-inputs "cwrapper.cpp")))
         (begin
           (use-modules (guix build utils))
           (setenv "PATH"
                   (string-append
                     (assoc-ref %build-inputs "gcc-toolchain") "/bin:"
                     (assoc-ref %build-inputs "xz") "/bin"))
           (invoke
             (string-append (assoc-ref %build-inputs "tar") "/bin/tar") "xvf" source)
           (copy-file
             (string-append ,name "-" ,version "-checkout/include/mapbox/earcut.hpp")
             "earcut.h")
           (copy-file cwrapper.cpp "cwrapper.cpp")
           (invoke ,(cxx-for-target)
                  "-std=c++11"
                  "-fPIC"
                  "-shared"
                  "-o" "libearcut.so"
                  "cwrapper.cpp")
           (install-file "libearcut.so"
                         (string-append out "/lib"))))))
    (inputs
     `(("gcc-toolchain" ,(S "gcc-toolchain"))
       ("cwrapper.cpp"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/JuliaPackaging/Yggdrasil/7e9ec714d786c4c841a80bdf75b84570c5bda7a1/E/EarCut/bundled/cwrapper.cpp")
           (file-name (string-append "cwrapper-for-earcut-for-julia-earcut-jll-" version ".cpp"))
           (sha256
            (base32 "0yf0r20306qsk1yrls5cz34vpfiazfdqc44f3mqmi76yq1ii0n09"))))))
    (native-inputs
     `(("tar" ,(S "tar"))
       ("xz" ,(S "xz"))))
    (home-page "https://github.com/mapbox/earcut.hpp")
    (synopsis "Header version of EarCut.js")
    (description "")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public julia-binaryprovider
  (package
    (name "julia-binaryprovider")
    (version "0.5.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/BinaryProvider.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00kin10n3fv5352fx3a4wh8l581702iqqhfz2sng773hkljndi9v"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f  ; test suite attempts to download packages
       ))
    (inputs
     `(
       ("busybox" ,(S "busybox"))
       ))
    (home-page "https://github.com/JuliaPackaging/BinaryProvider.jl")
    (synopsis "binary provider for Julia")
    (description "Packages are installed to a @code{Prefix}; a folder that acts similar to the @code{/usr/local} directory on Unix-like systems, containing a @code{bin} folder for binaries, a @code{lib} folder for libraries, etc... @code{Prefix} objects can have tarballs @code{install()}'ed within them, @code{uninstall()}'ed from them, etc...")
    (license license:expat)))
