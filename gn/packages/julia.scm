(define-module (gn packages julia)
  #:use-module ((guix licenses) #:prefix license:)
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
  #:use-module (gnu packages video)
  #:use-module (ice-9 match))

(define S specification->package)

(define-public julia-visuals
  (let ((commit "e7d670eb045a9f8e3a839476dc166318da7fe9dc")
        (revision "1"))
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
                 (base32 "15hshm5qrig5qbj02xy4ji79kfc72n93nna5nvxkhvb8gw3vvx07"))))
      (build-system julia-build-system)
      (arguments
       `(#:tests? #f    ; no test suite
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda* (#:key inputs #:allow-other-keys)
               (chmod "runpluto.sh" #o555)  ; it starts as #o444
               (substitute* "runpluto.sh"
                 ;; The arguments don't pass through the wrapper so we hardcode the port.
                 (("\\$\\{1\\}") "4343"))
               #t))
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
                         "notebooks"
                         "Project.toml"
                         "Manifest.toml"))
                 #t)))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; Do we need to wrap this with PYTHONPATH too?
                 (wrap-script (string-append out "/runpluto.sh")
                   `("PATH" ":" prefix (,(string-append (assoc-ref inputs "julia") "/bin")
                                        ,(string-append (assoc-ref inputs "coreutils") "/bin")))
                   `("JULIA_LOAD_PATH" ":" prefix (,(getenv "JULIA_LOAD_PATH"))))
                 #t)))
           (replace 'precompile
             (lambda _
               (invoke "julia" "-e" "\"import Pkg; Pkg.instantiate(); Pkg.status(); Pkg.precompile()\"")
               #t)))))
      (propagated-inputs
       `(;; from setup.py
         ("python-jupyter-server-proxy"
          ,(@ (gn packages python) python-jupyter-server-proxy-1))))
      (inputs
       `(("julia-distributions" ,julia-distributions)
         ("julia-latexstrings" ,julia-latexstrings)
         ("julia-optim" ,julia-optim)
         ("julia-plots" ,julia-plots)
         ("julia-pluto" ,julia-pluto)
         ("julia-plutoui" ,julia-plutoui)
         ("guile" ,(@ (gnu packages guile) guile-3.0))))    ; for wrap-script
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
         (add-after 'unpack 'hardcode-conda-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/Conda.jl"
               (("bin_dir\\(ROOTENV\\), \"conda\"") (string-append "\"" (assoc-ref inputs "conda") "/bin/\", \"conda\"")))
             #t))
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
    (version "0.25.11")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/Distributions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0n5xgdpzrpb4s0g23rjggk7c7x8677hbhq0sam7xbw9mn2w79m7n"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))        ; Some failed tests
    (propagated-inputs
     `(("julia-fillarrays" ,julia-fillarrays)
       ("julia-pdmats" ,julia-pdmats)
       ("julia-quadgk" ,julia-quadgk)
       ("julia-specialfunctions" ,julia-specialfunctions)
       ("julia-statsbase" ,julia-statsbase)
       ("julia-statsfuns" ,julia-statsfuns)))
    (native-inputs
     `(("julia-calculus" ,julia-calculus)
       ("julia-finitedifferences" ,julia-finitedifferences)
       ("julia-forwarddiff" ,julia-forwarddiff)
       ("julia-json" ,julia-json)
       ("julia-stablerngs" ,julia-stablerngs)
       ("julia-staticarrays" ,julia-staticarrays)))
    (home-page "https://github.com/JuliaStats/Distributions.jl")
    (synopsis "Probability distributions and associated functions")
    (description "Julia package for probability distributions and associated
functions.  Particularly, @code{Distributions} implements:
@enumerate
@enum Moments (e.g mean, variance, skewness, and kurtosis), entropy, and other
properties
@enum Probability density/mass functions (pdf) and their logarithm (logpdf)
@enum Moment generating functions and characteristic functions
@enum Sampling from population or from a distribution
@enum Maximum likelihood estimation
@end enumerate")
    (license license:expat)))

;; ready to upstream
;; if the test suite passes
(define-public julia-optim
  (package
    (name "julia-optim")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/Optim.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0bv281n999kmjlp9p3vl4vv4phdl17z4gdpvkjzxsyk6dvcg2nrf"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; TODO: Fix test
    (propagated-inputs
     `(("julia-compat" ,julia-compat)
       ("julia-fillarrays" ,julia-fillarrays)
       ("julia-linesearches" ,julia-linesearches)
       ("julia-nlsolversbase" ,julia-nlsolversbase)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-parameters" ,julia-parameters)
       ("julia-positivefactorizations" ,julia-positivefactorizations)
       ("julia-statsbase" ,julia-statsbase)))
    (native-inputs
     `(("julia-measurements" ,julia-measurements)
       ("julia-optimtestproblems" ,julia-optimtestproblems)
       ("julia-recursivearraytools" ,julia-recursivearraytools)))
    (home-page "https://github.com/JuliaNLSolvers/Optim.jl")
    (synopsis "Optimization functions for Julia")
    (description "@code{Optim.jl} is a package for univariate and multivariate
optimization of functions.")
    (license license:expat)))

(define-public julia-plots
  (package
    (name "julia-plots")
    (version "1.16.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/Plots.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ipv9408r4czsx1j01sv2f7ww52b329jc0v79npn4x2mf827nsik"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f  ; for now
       ))
    (propagated-inputs
     `(
       ;("julia-contour" ,julia-contour)
       ("julia-ffmpeg" ,julia-ffmpeg)
       ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
       ("julia-gr" ,julia-gr)
       ("julia-geometrybasics" ,julia-geometrybasics)
       ("julia-json" ,julia-json)
       ;("julia-latexify" ,julia-latexify)
       ("julia-measures" ,julia-measures)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-plotthemes" ,julia-plotthemes)
       ("julia-plotutils" ,julia-plotutils)
       ("julia-recipesbase" ,julia-recipesbase)
       ("julia-recipespipeline" ,julia-recipespipeline)
       ("julia-reexport" ,julia-reexport)
       ("julia-requires" ,julia-requires)
       ("julia-scratch" ,julia-scratch)
       ("julia-showoff" ,julia-showoff)
       ("julia-statsbase" ,julia-statsbase)
       ))
    (native-inputs
     `(
       ;("julia-distributions" ,julia-distributions)
       ("julia-fileio" ,julia-fileio)
       ;("julia-gtk" ,julia-gtk)
       ;("julia-hdf5" ,julia-hdf5)
       ("julia-imagemagick" ,julia-imagemagick)
       ;("julia-images" ,julia-images)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ;("julia-pgfplotsx" ,julia-pgfplotsx)
       ;("julia-plotlyjs" ,julia-plotlyjs)
       ;("julia-rdatasets" ,julia-rdatasets)
       ("julia-stablerngs" ,julia-stablerngs)
       ("julia-staticarrays" ,julia-staticarrays)
       ;("julia-statsplots" ,julia-statsplots)
       ("julia-testimages" ,julia-testimages)
       ;("julia-unicodeplots" ,julia-unicodeplots)
       ;("julia-visualregressiontests" ,julia-visualregressiontests)
       ))
    (home-page "http://docs.juliaplots.org/")
    (synopsis "Powerful convenience for Julia visualizations and data analysis")
    (description "Plots is a plotting API and toolset.")
    (license license:expat)))

;; TODO: unbundle javascript calls to cdn.jsdelivr.net
(define-public julia-pluto
  (package
    (name "julia-pluto")
    (version "0.15.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fonsp/Pluto.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1jsvqi33rsj8izm9pb0r4gjzb5xd01dxri8xp95h84kd0rdliirr"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f      ; Test suite fails to load HTTP.jl.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-check-for-upgrades
           (lambda _
             (substitute* "frontend/components/Welcome.js"
               (("local_index !== -1") "false"))
             #t))
         (add-after 'unpack 'skip-network-tests
           (lambda _
             (substitute* "test/runtests.jl"
               ;; Attempts to update the package registry.
               ((".*Basic.jl.*") ""))
             #t)))))
    (propagated-inputs
     `(("julia-configurations" ,julia-configurations)
       ("julia-fuzzycompletions" ,julia-fuzzycompletions)
       ("julia-http" ,julia-http)
       ("julia-msgpack" ,julia-msgpack)
       ("julia-tableiointerface" ,julia-tableiointerface)
       ("julia-tables" ,julia-tables)))
    (native-inputs
     `(("julia-dataframes" ,julia-dataframes)
       ("julia-offsetarrays" ,julia-offsetarrays)))
    (home-page "https://github.com/fonsp/Pluto.jl")
    (synopsis "Simple reactive notebooks for Julia")
    (description "A Pluto notebook is made up of small blocks of Julia code
(cells) and together they form a reactive notebook.  When you change a variable,
Pluto automatically re-runs the cells that refer to it.  Cells can even be
placed in arbitrary order - intelligent syntax analysis figures out the
dependencies between them and takes care of execution.")
    (license license:expat)))

;; ready to upstream, wait on Pluto.jl?
(define-public julia-plutoui
  (package
    (name "julia-plutoui")
    (version "0.7.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fonsp/PlutoUI.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0p159b4m0nxbz36ll5kf082vb806n2f26ma145pbhp749aldzplp"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-json" ,julia-json)
       ("julia-reexport" ,julia-reexport)
       ("julia-suppressor" ,julia-suppressor)))
    (home-page "https://github.com/fonsp/PlutoUI.jl")
    (synopsis "Helper package for Julia Pluto")
    (description "This package helps to make @code{html\"<input>\"} a bit more
native to Julia.  Use it with the @code{@@bind} macro in Pluto.")
    (license license:expat)))

;; ready to upstream
(define-public julia-statsfuns
  (package
    (name "julia-statsfuns")
    (version "0.9.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/StatsFuns.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1zl46p9gbx9xkjnnpd45csshqvq2i94mxw10karpr8xkx8msyk3k"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-test-suite
           (lambda _
             (substitute* "test/misc.jl"
               (("test logmvbeta\\(1") "test_nowarn logmvbeta(1"))
             #t)))))
    (propagated-inputs
     `(("julia-logexpfunctions" ,julia-logexpfunctions)
       ("julia-rmath" ,julia-rmath)
       ("julia-specialfunctions" ,julia-specialfunctions)))
    (native-inputs
     `(("julia-forwarddiff" ,julia-forwarddiff)))
    (home-page "https://github.com/JuliaStats/StatsFuns.jl")
    (synopsis "Mathematical functions related to statistics")
    (description "This package provides a collection of mathematical constants
and numerical functions for statistical computing.")
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
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "julia" "test.jl"))
               #t))
           (add-before 'install 'build
             (lambda _
               (invoke "make")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "src/libRmath-julia.so" (string-append out "/lib"))
                 #t))))))
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
         (add-after 'unpack 'link-to-librmath-directly
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((rmath    (assoc-ref inputs "rmath"))
                    (librmath (string-append rmath "/lib/libRmath-julia.so")))
               (substitute* "src/Rmath.jl"
                 (("libRmath\\)") (string-append "\"" librmath "\")")))
               #t))))))
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
         (add-after 'unpack 'override-binary-path
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
(define-public julia-linesearches
  (package
    (name "julia-linesearches")
    (version "7.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/LineSearches.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qc4la07w6s1xhcyd0hvbnpr31zc1a2ssgyybc8biv5m00g0dnr0"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-optim-tests
           (lambda _
             (substitute* "test/examples.jl"
               ;; Prevent a cycle with Optim.jl.
               (("^    SKIPFILE.*") "")
               (("^    #SKIPFILE") "    SKIPFILE"))
             #t)))))
    (propagated-inputs
     `(("julia-nlsolversbase" ,julia-nlsolversbase)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-parameters" ,julia-parameters)))
    (native-inputs
     `(("julia-doublefloats" ,julia-doublefloats)
       ("julia-optimtestproblems" ,julia-optimtestproblems)))
    (home-page "https://github.com/JuliaNLSolvers/LineSearches.jl")
    (synopsis "Line search methods for optimization and root-finding")
    (description "This package provides an interface to line search algorithms
implemented in Julia.")
    (license license:expat)))

;; ready to upstream
(define-public julia-doublefloats
  (package
    (name "julia-doublefloats")
    (version "1.1.23")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/DoubleFloats.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0qdkcmjknlan25dbgqw101zvgb5ly8v4pf1xikj6k2x96k8c6c8g"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-genericlinearalgebra" ,julia-genericlinearalgebra)
       ("julia-polynomials" ,julia-polynomials)
       ("julia-quadmath" ,julia-quadmath)
       ("julia-requires" ,julia-requires)
       ("julia-specialfunctions" ,julia-specialfunctions)))
    (native-inputs
     `(("julia-genericlinearalgebra" ,julia-genericlinearalgebra)
       ("julia-genericschur" ,julia-genericschur)
       ("julia-specialfunctions" ,julia-specialfunctions)))
    (home-page "https://github.com/JuliaMath/DoubleFloats.jl")
    (synopsis "Extended precision float and complex types")
    (description "This package provides a math library with extended precision
floats and complex types.")
    (license license:expat)))

;; ready to upstream
(define-public julia-polynomials
  (package
    (name "julia-polynomials")
    (version "2.0.14")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Polynomials.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "07yb98fm9yhhan0n3iwmd9dz7zpj4kal99z5h1px1q5zpmjn68xa"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-intervals" ,julia-intervals)
       ("julia-mutablearithmetics" ,julia-mutablearithmetics)
       ("julia-recipesbase" ,julia-recipesbase)))
    (native-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-specialfunctions" ,julia-specialfunctions)
       ("julia-staticarrays" ,julia-staticarrays)))
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
     `(("julia-recipesbase" ,julia-recipesbase)
       ("julia-timezones" ,julia-timezones)))
    (native-inputs
     `(("julia-documenter" ,julia-documenter)
       ;("julia-imagemagick" ,julia-imagemagick)
       ;("julia-infinity" ,julia-infinity)
       ;("julia-plots" ,julia-plots)
       ;("julia-visualregressiontests" ,julia-visualregressiontests)
       ))
    (home-page "https://github.com/invenia/Intervals.jl")
    (synopsis "Non-iterable ranges")
    (description "This package defines:

                     AbstractInterval, along with its subtypes Interval and AnchoredInterval, and also Bound.")
    (license license:expat)))

;; TODO: Keep this in sync with tzdata in base.scm
;; Package can use more work
;; Need to figure out how to generate Artifacts.toml from tzdata package
(define-public julia-timezones
  (package
    (name "julia-timezones")
    (version "1.5.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaTime/TimeZones.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0f6rk1g4ffj4r6g8hfy0ygk4vyppibywkxgixhbgnc09w8y0009d"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f    ; Tests attempt to download timezone information
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tzdata
           (lambda* (#:key inputs #:allow-other-keys)
             ;(substitute* "src/tzdata/TZData.jl"
             ;  (("(COMPILED_DIR = ).*" _ key)
             ;   (string-append key "\"" (assoc-ref inputs "tzdata") "/share/zoneinfo\"")))
             (substitute* "test/runtests.jl"
               (("2016j") "2021a")
               ((".*download.jl.*") "")
               )
             (make-file-writable "Artifacts.toml")
             (with-output-to-file "Artifacts.toml"
               (lambda _
                 (format #t "[tzdata2021a]~@
                         git-tree-sha1 = \"6d94ada27957590cbd0d7678f5ae711232a4d714\"~@
                         lazy = true~@
                         ~@
                         [[tzdata2021a.download]]~@
                         sha256 = \"39e7d2ba08c68cbaefc8de3227aab0dec2521be8042cf56855f7dc3a9fb14e08\"~@
                         url = \"file://~a\"~%"
                         (assoc-ref inputs "tzdata-src"))))
             #t))
         )))
    (propagated-inputs
     `(
       ("julia-lazyartifacts" ,julia-lazyartifacts)
       ("julia-mocking" ,julia-mocking)
       ("julia-recipesbase" ,julia-recipesbase)
       ))
    (native-inputs
     `(
       ;("julia-compat" ,julia-compat)
       ;("julia-timezones" ,julia-timezones)
       ;("curl" ,(@ (gnu packages curl) curl-minimal))
       ;("tzdata" ,(@ (gnu packages base) tzdata))
       ("tzdata-src" ,(package-source (@ (gnu packages base) tzdata)))
       ))
    (home-page "https://juliahub.com/docs/Infinity/")
    (synopsis "IANA time zone database access for the Julia programming language")
    (description "IANA time zone database access for the Julia programming language. TimeZones.jl extends the Date/DateTime support for Julia to include a new time zone aware TimeType: ZonedDateTime.")
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
  ;; Only release tag contains just a license file.
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
         (add-after 'unpack 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
              (lambda (wrapper)
                (substitute* wrapper
                  (("generate_wrapper_header.*")
                   (string-append
                    "generate_wrapper_header(\"EarCut\", \""
                    (assoc-ref inputs "earcut") "\")\n"))))
              ;; There's a Julia file for each platform, override them all
              (find-files "src/wrappers/" "\\.jl$"))
             #t)))))
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
              ((".*add_subdirectory.*") ""))
            #t))))
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
