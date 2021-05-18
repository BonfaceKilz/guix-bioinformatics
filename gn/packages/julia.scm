(define-module (gn packages julia)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system julia)
  #:use-module (gn packages cran)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages julia-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (ice-9 match))

(define-public julia-liteqtl
  (let ((commit "321a9e0aa87fb4524bec8278e64de76d1a4072b0")
        (revision "1"))
    (package
      (name "julia-liteqtl")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/senresearch/LiteQTL.jl")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01fn552cafb73g86cg9ii9c0ch2llarzaz54zr66qsfd97kl6ify"))))
      (build-system julia-build-system)
      (propagated-inputs
       `(
         ;("julia-csv" ,julia-csv)
         ;("julia-cuda" ,julia-cuda)
         ("julia-dataframes" ,julia-dataframes)
         ("julia-docstringextensions" ,julia-docstringextensions)
         ))
      (native-inputs
       `(
         ("julia-documenter" ,julia-documenter)
         ("julia-safetestsets" ,julia-safetestsets)
         ;("r" ,r-minimal)
         ;("r-data-table" ,r-data-table)
         ;("r-mice" ,r-mice)
         ;("r-qtl" ,r-qtl)
         ;("r-qtl2" ,r-qtl2)
         ;("r-tidyverse" ,r-tidyverse)
         ))
      (home-page "https://github.com/senresearch/LiteQTL.jl")
      (synopsis "Julia package for eQTL genome scans near real-time")
      (description "LiteQTL is a package that runs whole genome QTL scans near real-time, utilizing the computation power of GPU.
LiteQTL uses new algorithms that enables near-real time whole genome QTL scans for up to 1 million traits. By using easily parallelizable operations including matrix multiplication, vectorized operations, and element-wise operations, our method is about 300 times faster than a R/qtl linear model genome scan using 16 threads.")
      (license license:expat))))

(define-public julia-packagecompiler
  (package
    (name "julia-packagecompiler")
    (version "1.2.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLang/PackageCompiler.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1yyvqfix7ys3p24hyxi75r6ifyw0mplbry8rvd0vp0h77g6q357i"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "PackageCompiler"
       ))
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
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/senresearch/FlxQTL.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0lzf4vmbjc8zfqsw7a697gza4dxchq5jqp876567ywla3d1f3sl0"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "PackageCompiler"
       ))
    (propagated-inputs
     `(
       ;("julia-distributions" ,julia-distributions)
       ;("julia-lossfunctions" ,julia-lossfunctions)
       ("julia-pyplot" ,julia-pyplot)
       ;("julia-revise" ,julia-revise)
       ;("julia-staticarrays" ,julia-static-arrays)
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

(define-public julia-pyplot
  (package
    (name "julia-pyplot")
    (version "2.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPy/PyPlot.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0lvnraw8i851xnlfyd8d1p1bp8nrr1s9z56fw6znlmakpjjwny39"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "PackageCompiler"
       ))
    (propagated-inputs
     `(
       ("julia-pycall" ,julia-pycall)
       ("julia-latexstrings" ,julia-latexstrings)
       ("julia-colors" ,julia-colors)
       ("julia-versionparsing" ,julia-versionparsing)
       ))
    (home-page "https://github.com/JuliaPy/PyPlot.jl")
    (synopsis "Plotting for Julia based on matplotlib.pyplot")
    (description "This module provides a Julia interface to the Matplotlib plotting library from Python, and specifically to the @code{matplotlib.pyplot} module.  PyPlot uses the Julia PyCall package to call Matplotlib directly from Julia with little or no overhead (arrays are passed without making a copy).")
    (license license:expat)))

(define-public julia-pycall
  (package
    (name "julia-pycall")
    (version "1.92.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPy/PyCall.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "07r99ni6nkxpyrp3wsb5qg4jxz7i2r08dyqbiffy2zm3g0bn88jq"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "PackageCompiler"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-conda
           (lambda _
             (substitute* "Project.toml"
               ((".*Conda.*") ""))
             (substitute* "src/PyCall.jl"
               (("import Conda") ""))
             #t))
         (add-after 'unpack 'set-python
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((python (assoc-ref inputs "python")))
               ;(substitute* "deps/find_libpython.py"
               ;  (("/usr/bin/env python")
                  ;(string-append python "/bin/python3"))
               ;   "/usr/bin/env python3")
                 ;(("return (path|None)")
                 ; (string-append "return \"" python "/lib/libpython3.so\""))
               ;   )
               ;(substitute* "deps/buildutils.jl"
               ;  (("\\$python \\$script")
               ;   (string-append python "/bin/python3 $script"))
               ;  (("pythonenv\\(cmd\\)")
               ;   (string-append python "/lib/libpython3.so"))
               ;  )
               ;(substitute* "src/startup.jl"
               ;  (("dlopen\\(libpython")
               ;   (string-append "dlopen(\"" python "/lib/libpython3.so\""))
               ;  (("pyversion_build")
               ;  ;(string-append "\"" ,(package-version python) "\""))
               ;  ;,(package-version python))
               ;  ,(version-major+minor (package-version python)))
               ;  (("PYTHONHOME")
               ;  (string-append "\"" python "\""))
               ;  )
               ;(setenv "SHLIB_SUFFIX" ".so")
               (setenv "PYCALL_DEBUG_BUILD" "yes")
               (setenv "JULIA_PKGEVAL" "false")
               (with-output-to-file "deps/deps.jl"
                 (lambda _
                   (format #t ;"ENV[\"PYTHON\"]=\"~a/bin/python3\"~%"
                           "ENV[\"PYTHONHOME\"]=\"~a\"~%"
                           ;python
                           python)))
               #t)))
         )
       ))
    (propagated-inputs
     `(
       ;("julia-conda" ,julia-conda)
       ("julia-macrotools" ,julia-macrotools)
       ("julia-versionparsing" ,julia-versionparsing)
       ("python" ,python)
       ))
    (native-inputs
     `(
       ;("python-numpy" ,(@ (gnu packages python-xyz) python-numpy))
       ))
    (home-page "https://github.com/JuliaPy/PyCall.jl")
    (synopsis "Call Python functions from the Julia language")
    (description "This package provides the ability to directly call and fully interoperate with Python from the Julia language.  You can import arbitrary Python modules from Julia, call Python functions (with automatic conversion of types between Julia and Python), define Python classes from Julia methods, and share large data structures between Julia and Python without copying them.")
    (license license:expat)))

(define-public julia-versionparsing
  (package
    (name "julia-versionparsing")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaInterop/VersionParsing.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "060s72dsnpavgilf7f7315lw2sn4npk8lkndmj6bg7i23hppiwva"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaInterop/VersionParsing.jl")
    (synopsis "Flexible VersionNumber parsing in Julia")
    (description "The @code{VersionParsing} package implements flexible parsing of version-number strings into Julia's built-in @code{VersionNumber} type, via the @code{vparse(string)} function.  Unlike the @code{VersionNumber(string){ constructor, @code{vparse(string)} can handle version-number strings in a much wider range of formats than are encompassed by the semver standard. This is useful in order to support @code{VersionNumber} comparisons applied to \"foreign\" version numbers from external packages.")
    (license license:expat)))

(define-public julia-conda
  (package
    (name "julia-conda")
    (version "1.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPy/Conda.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1v0plrhx9765kzynjdbgrxg5yv0nl40mklyl0z0p06ifvn927q77"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "PackageCompiler"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;(setenv "CONDA_JL_HOME" (string-append (assoc-ref inputs "conda") ))
             ;(setenv "CONDA_JL_VERSION" "3")
             ;(display (getcwd))
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
       ("conda" ,(@ (gnu packages package-management) conda))
       ("python" ,(@ (gnu packages python) python-wrapper))
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

(define-public julia-latexstrings
  (package
    (name "julia-latexstrings")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/stevengj/LaTeXStrings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "117z27krcf8fydgp6mb0pgn75r4gng9qs7v90qb4bqzsry3faadp"))))
    (build-system julia-build-system)
    (native-inputs
     `(("julia-documenter" ,julia-documenter-0.24)))
    (home-page "https://github.com/stevengj/LaTeXStrings.jl")
    (synopsis "Input and display of LaTeX equation strings")
    (description "This is a small package to make it easier to type LaTeX equations in string literals in the Julia language.")
    (license license:expat)))

(define-public julia-distributions
  (package
    (name "julia-distributions")
    (version "0.25.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/Distributions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1rxv9cml5r8gp0qgqdp61hqbgvv55ncyfzbim4iqfmbaj4ji9fmp"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ))
    (propagated-inputs
     `(
       ;("julia-fillarrays" ,julia-fillarrays)
       ;("julia-distributed" ,julia-distributed)
       ("julia-pdmats" ,julia-pdmats)
       ;("julia-quadgk" ,julia-quadgk)
       ;("julia-specialfunctions" ,julia-specialfunctions)
       ("julia-statsbase" ,julia-statsbase)
       ("julia-statsfuns" ,julia-statsfuns)
       ))
    (home-page "https://github.com/JuliaStats/Distributions.jl")
    (synopsis "probability distributions and associated functions")
    (description "Julia package for probability distributions and associated functions. Particularly, Distributions implements:
@enumerate
@enum Moments (e.g mean, variance, skewness, and kurtosis), entropy, and other properties
@enum Probability density/mass functions (pdf) and their logarithm (logpdf)
@enum Moment generating functions and characteristic functions
@enum Sampling from population or from a distribution
@enum Maximum likelihood estimation
@end enumerate")
    (license license:expat)))

(define-public julia-documenter
  (package
    (name "julia-documenter")
    (version "0.26.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/Documenter.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1d4mdjc56w0hrc50qia361zfp8zapq163cqgagkbbjn0k83zp21x"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; LoadError: UndefVarError: iocapture not defined
    (propagated-inputs
     `(("julia-docstringextensions" ,julia-docstringextensions)
       ("julia-iocapture" ,julia-iocapture)
       ("julia-json" ,julia-json)))
    (native-inputs
     `(("julia-documentermarkdown" ,julia-documentermarkdown)))
    (home-page "https://juliadocs.github.io/Documenter.jl")
    (synopsis "Documentation generator for Julia")
    (description "This package provides a documentation generator for Julia.")
    (license license:expat)))

(define-public julia-documenter-0.24
  (package
    (inherit julia-documenter)
    (name "julia-documenter")
    (version "0.24.11")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/Documenter.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0s3di48bwc08lhsqhqkgsxg01fr39vp3j1hbnswcaq7f90v6lqhn"))))
    (arguments
     `(#:tests? #f  ; Some tests fail
       ))
    (propagated-inputs
     `(("julia-docstringextensions" ,julia-docstringextensions)
       ("julia-json" ,julia-json)))
    (native-inputs
     `(("julia-documentermarkdown" ,julia-documentermarkdown)))
    (properties '((hidden? . #t)))))

(define-public julia-documenter-0.22
  (package
    (inherit julia-documenter)
    (name "julia-documenter")
    (version "0.22.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/Documenter.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1z8b7267y7yn5nx8sjwkmc0ph97vmv42q52jg7s89ghqb9xx3wv5"))))
    (arguments
     `(#:tests? #f))    ; Some tests require network.
    (propagated-inputs
     `(("julia-docstringextensions" ,julia-docstringextensions)
       ("julia-json" ,julia-json)))
    (native-inputs `())
    (properties '((hidden? . #t)))))

(define-public julia-documentermarkdown
  (package
    (name "julia-documentermarkdown")
    (version "0.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/DocumenterMarkdown.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "11l7yrifg8pdr4q6h75zydfw5i8vww07p5bci5mi8gwwcpi3jksb"))))
    (build-system julia-build-system)
    (propagated-inputs
      ;; Cycle with julia-documenter in later versions.
     `(("julia-documenter" ,julia-documenter-0.22)))
    (home-page "https://github.com/JuliaDocs/DocumenterMarkdown.jl")
    (synopsis "Documenter's Markdown")
    (description "his package enables the Markdown / MkDocs backend of
@code{Documenter.jl}.")
    (license license:expat)))

(define-public julia-statsbase
  (package
    (name "julia-statsbase")
    (version "0.33.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/StatsBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "02y4pm5yvg713a2pn970bbcfkrn2h133rxbxk1da18svhqw3czhi"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-dataapi" ,julia-dataapi)
       ("julia-datastructures" ,julia-datastructures)
       ("julia-missings" ,julia-missings)
       ("julia-sortingalgorithms" ,julia-sortingalgorithms)
       ("julia-statsapi" ,julia-statsapi)))
    (native-inputs
     `(("julia-stablerngs" ,julia-stablerngs)))
    (home-page "https://github.com/JuliaStats/StatsBase.jl")
    (synopsis "Basic statistics for Julia")
    (description "StatsBase.jl is a Julia package that provides basic support
for statistics.  Particularly, it implements a variety of statistics-related
functions, such as scalar statistics, high-order moment computation, counting,
ranking, covariances, sampling, and empirical density estimation.")
    (license license:expat)))

(define-public julia-dataapi
  (package
    (name "julia-dataapi")
    (version "1.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/DataAPI.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "14sfvkz169zcbap3gdwpj16qsap783h86fd07flfxk822abam11w"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaData/DataAPI.jl")
    (synopsis "data-focused namespace for packages to share functions")
    (description "This package provides a namespace for data-related generic function definitions to solve the optional dependency problem; packages wishing to share and/or extend functions can avoid depending directly on each other by moving the function definition to DataAPI.jl and each package taking a dependency on it.")
    (license license:expat)))

(define-public julia-sortingalgorithms
  (package
    (name "julia-sortingalgorithms")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaCollections/SortingAlgorithms.jl")
               ;; Tagging releases is hard:
               ;; https://github.com/JuliaCollections/SortingAlgorithms.jl/issues/41#issuecomment-840587380
               (commit "aa2b98d384ddd132aae0219e68fb63b92513cb35")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "13zbx18psxrg4fvkqgp0m7g484vrama2xm6902bbls30801hgljg"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; cycle with StatsBase.jl
    (propagated-inputs
     `(("julia-datastructures" ,julia-datastructures)))
    ;(native-inputs
    ; `(("julia-statsbase" ,julia-statsbase)))
    (home-page "https://github.com/JuliaCollections/SortingAlgorithms.jl")
    (synopsis "Extra sorting algorithms extending Julia's sorting API")
    (description "The SortingAlgorithms package provides three sorting
algorithms that can be used with Julia's standard sorting API: heapsort,
timsort and radixsort.")
    (license license:expat)))

(define-public julia-optim
  (package
    (name "julia-optim")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/Optim.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1nmc4979dim5s630b5wskkjg141yz9655qag7i5m8f4p2cq4b2dp"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ))
    (propagated-inputs
     `(
       ("julia-compat" ,julia-compat)
       ("julia-fillarrays" ,julia-fillarrays)
       ("julia-linesearches" ,julia-linesearches)
       ("julia-nlsolversbase" ,julia-nlsolversbase)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-parameters" ,julia-parameters)
       ("julia-positivefactorizations" ,julia-positivefactorizations)
       ("julia-statsbase" ,julia-statsbase)
       ))
    (native-inputs
     `(
       ("julia-optimtestproblems" ,julia-optimtestproblems)
       ("julia-recursivearraytools" ,julia-recursivearraytools)
       ))
    (home-page "https://github.com/JuliaNLSolvers/Optim.jl")
    (synopsis "Optimization functions for Julia")
    (description "Optim.jl is a package for univariate and multivariate optimization of functions.")
    (license license:expat)))

(define-public julia-nlsolversbase
  (package
    (name "julia-nlsolversbase")
    (version "7.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/NLSolversBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0n8qh5a2ghjx1j70zxn0hmh8gzpa46kmjg8di879y9974bfk0f98"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-diffresults" ,julia-diffresults)
       ("julia-finitediff" ,julia-finitediff)
       ("julia-forwarddiff" ,julia-forwarddiff)))
    (native-inputs
     `(("julia-optimtestproblems" ,julia-optimtestproblems)
       ("julia-recursivearraytools" ,julia-recursivearraytools)))
    (home-page "https://github.com/JuliaNLSolvers/NLSolversBase.jl")
    (synopsis "Optimization and equation solver software in JuliaNLSolvers")
    (description "This package aims at establishing common ground for Optim.jl,
LineSearches.jl, and NLsolve.jl.  The common ground is mainly the types used to
hold objective related callables, information about the objectives, and an
interface to interact with these types.")
    (license license:expat)))

(define-public julia-finitediff
  (package
    (name "julia-finitediff")
    (version "2.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDiff/FiniteDiff.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ndazn02wn8ddwgjh1i32y7pbaqpw06f42ccilz5ya78cyrjhq2m"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; We don't want to run all the tests, the Downstream tests try
             ;; to download the package registry.
             (setenv "GROUP" "Core")
             #t)))))
    (propagated-inputs
     `(("julia-arrayinterface" ,julia-arrayinterface)
       ("julia-requires" ,julia-requires)
       ("julia-staticarrays" ,julia-staticarrays)))
    (native-inputs
     `(("julia-bandedmatrices" ,julia-bandedmatrices)
       ("julia-blockbandedmatrices" ,julia-blockbandedmatrices)
       ("julia-safetestsets" ,julia-safetestsets)))
    (home-page "https://github.com/JuliaDiff/FiniteDiff.jl")
    (synopsis "Calculations of gradients, Jacobians, and Hessians")
    (description "This package is for calculating derivatives, gradients,
Jacobians, Hessians, etc. numerically.  This library is for maximizing speed
while giving a usable interface to end users in a way that specializes on array
types and sparsity.")
    (license license:expat)))

(define-public julia-safetestsets
  (let ((commit "e553edc4c753344d38349304b9ff5483c3b8ff21")
        (revision "1"))
    (package
      (name "julia-safetestsets")
      (version (git-version "0.0.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/YingboMa/SafeTestsets.jl")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1fb1dfdmiw2ggx60hf70954xlps0r48fcb3k3dvxynlz7ylphp96"))))
      (build-system julia-build-system)
      (arguments
       `(#:julia-package-name "SafeTestsets"))
      (native-inputs
       `(("julia-staticarrays" ,julia-staticarrays)))
      (home-page "https://github.com/YingboMa/SafeTestsets.jl")
      (synopsis "Put Julia's testset in a module")
      (description "@code{safetestset} puts @code{testset} into a module.")
      (license license:expat))))

(define-public julia-arrayinterface
  (package
    (name "julia-arrayinterface")
    (version "3.1.14")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/ArrayInterface.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0w99fas8kkqm5qy9jqjp1aw8aygpdb823fmgyjgv9dvi3g10j5q3"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-ifelse" ,julia-ifelse)
       ("julia-requires" ,julia-requires)
       ("julia-static" ,julia-static)))
    (native-inputs
     `(("julia-aqua" ,julia-aqua)
       ("julia-bandedmatrices" ,julia-bandedmatrices)
       ("julia-blockbandedmatrices" ,julia-blockbandedmatrices)
       ("julia-ifelse" ,julia-ifelse)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-staticarrays" ,julia-staticarrays)))
    (home-page "https://github.com/JuliaArrays/ArrayInterface.jl")
    (synopsis "Base array interface primitives")
    (description "The purpose of this library is to solidify extensions to the
current AbstractArray interface, which are put to use in package ecosystems like
DifferentialEquations.jl.  Since these libraries are live, this package will
serve as a staging ground for ideas before they are merged into Base Julia.  For
this reason, no functionality is exported so that if such functions are added
and exported in a future Base Julia, there will be no issues with the upgrade.")
    (license license:expat)))

(define-public julia-ifelse
  (package
    (name "julia-ifelse")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sciml/ifelse.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wrw842r8708fryf2ihp9mkmdrg27saa9nix2c31vs995k2fgr9w"))))
    (build-system julia-build-system)
    (home-page "https://github.com/sciml/ifelse.jl")
    (synopsis "Function form of the if-else conditional statement")
    (description "This package provides a convenient function form of the conditional ifelse.  It is similar to @code{Core.ifelse} but it is extendable.")
    (license license:expat)))

(define-public julia-plots
  (package
    (name "julia-plots")
    (version "1.14.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/Plots.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0gpry538c3159ngf5kc86rsd5dpj8nh77g9kah0asahw46dz9sm8"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "SortingAlgorithms"
       ))
    (propagated-inputs
     `(
       ;("julia-compat" ,julia-compat)
       ;("julia-fillarrays" ,julia-fillarrays)
       ;("julia-linesearches" ,julia-linesearches)
       ;("julia-nlsolversbase" ,julia-nlsolversbase)
       ;("julia-nanmath" ,julia-nanmath)
       ;("julia-parameters" ,julia-parameters)
       ;("julia-positivefactorizations" ,julia-positivefactorizations)
       ;("julia-statsbase" ,julia-statsbase)
       ))
    (native-inputs
     `(
       ("julia-imagemagick" ,julia-imagemagick)
       ))
    (home-page "http://docs.juliaplots.org/")
    (synopsis "Powerful convenience for Julia visualizations and data analysis")
    (description "Plots is a plotting API and toolset.")
    (license license:expat)))

(define-public julia-imagemagick
  (package
    (name "julia-imagemagick")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/ImageMagick.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "05vzv4jsj3l9pv6yrix28hlw7wnag0mqdfjwv8shn4x71hcfxl1p"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "SortingAlgorithms"
       ))
    (propagated-inputs
     `(
       ("julia-fileio" ,julia-fileio)
       ;("julia-fillarrays" ,julia-fillarrays)
       ;("julia-linesearches" ,julia-linesearches)
       ;("julia-nlsolversbase" ,julia-nlsolversbase)
       ;("julia-nanmath" ,julia-nanmath)
       ;("julia-parameters" ,julia-parameters)
       ;("julia-positivefactorizations" ,julia-positivefactorizations)
       ;("julia-statsbase" ,julia-statsbase)
       ))
    (native-inputs
     `(
       ;("julia-imagemagick" ,julia-imagemagick)
       ))
    (home-page "https://github.com/JuliaIO/ImageMagick.jl")
    (synopsis "Thin Wrapper for the library ImageMagick")
    (description "This package provides a wrapper around ImageMagick version 6.  It was split off from Images.jl to make image I/O more modular.")
    (license license:expat)))

(define-public julia-fileio
  (package
    (name "julia-fileio")
    (version "1.8.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/FileIO.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0qbh93ys16h8p28sf8h556lzj7771ylhisqmla7y5yxwg4nqwkim"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "SortingAlgorithms"
       ))
    (propagated-inputs
     `(
       ("julia-requires" ,julia-requires)
       ;("julia-fillarrays" ,julia-fillarrays)
       ;("julia-linesearches" ,julia-linesearches)
       ;("julia-nlsolversbase" ,julia-nlsolversbase)
       ;("julia-nanmath" ,julia-nanmath)
       ;("julia-parameters" ,julia-parameters)
       ;("julia-positivefactorizations" ,julia-positivefactorizations)
       ;("julia-statsbase" ,julia-statsbase)
       ))
    (native-inputs
     `(
       ("julia-filepathsbase" ,julia-filepathsbase)
       ))
    (home-page "https://github.com/JuliaIO/FileIO.jl")
    (synopsis "Main Package for IO, loading all different kind of files")
    (description "FileIO aims to provide a common framework for detecting file formats and dispatching to appropriate readers/writers.  The two core functions in this package are called load and save, and offer high-level support for formatted files (in contrast with julia's low-level read and write).  To avoid name conflicts, packages that provide support for standard file formats through functions named load and save are encouraged to register with FileIO.")
    (license license:expat)))

(define-public julia-filepathsbase
  (package
    (name "julia-filepathsbase")
    (version "0.9.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rofinn/FilePathsBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "136wm4ik6isrdanmpi4gdr1qw0qhr15i925qzjxbawk5hnyzwng9"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "SortingAlgorithms"
       ))
    (propagated-inputs
     `(
       ;("julia-requires" ,julia-requires)
       ;("julia-fillarrays" ,julia-fillarrays)
       ;("julia-linesearches" ,julia-linesearches)
       ;("julia-nlsolversbase" ,julia-nlsolversbase)
       ;("julia-nanmath" ,julia-nanmath)
       ;("julia-parameters" ,julia-parameters)
       ;("julia-positivefactorizations" ,julia-positivefactorizations)
       ;("julia-statsbase" ,julia-statsbase)
       ))
    (native-inputs
     `(
       ("julia-jlso" ,julia-jlso)
       ))
    (home-page "https://github.com/rofinn/FilePathsBase.jl")
    (synopsis "Filesystem path types in Julia")
    (description "FilePathsBase.jl provides a type based approach to working with filesystem paths in julia.")
    (license license:expat)))

(define-public julia-jlso
  (package
    (name "julia-jlso")
    (version "2.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/invenia/JLSO.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1x00mrn4njvkhjns4g8bzjj40g4n6slaxlpsbbccalyabs9sz6id"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "SortingAlgorithms"
       ))
    (propagated-inputs
     `(
       ("julia-bson" ,julia-bson)
       ;("julia-fillarrays" ,julia-fillarrays)
       ;("julia-linesearches" ,julia-linesearches)
       ;("julia-nlsolversbase" ,julia-nlsolversbase)
       ;("julia-nanmath" ,julia-nanmath)
       ;("julia-parameters" ,julia-parameters)
       ;("julia-positivefactorizations" ,julia-positivefactorizations)
       ;("julia-statsbase" ,julia-statsbase)
       ))
    (native-inputs
     `(
       ;("julia-jlso" ,julia-jlso)
       ))
    (home-page "https://github.com/invenia/JLSO.jl")
    (synopsis "Julia Serialized Object (JLSO) file format for storing checkpoint data")
    (description "JLSO is a storage container for serialized Julia objects.  Think of it less as a serialization format but as a container, that employs a serializer, and a compressor, handles all the other concerns including metadata and saving.  Such that the serializer just needs to determine how to turn a julia object into a streamVector{UInt8}, and the compressor just needs to determine how to turn one stream of UInt8s into a smaller one (and the reverse).")
    (license license:expat)))

(define-public julia-bson
  (package
    (name "julia-bson")
    (version "0.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/BSON.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1l5608ma2ys7v2gpcqbiv9mwfc6yrlqkihrfx1pf7fgv5llhd4fn"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "SortingAlgorithms"
       ))
    (propagated-inputs
     `(
       ;("julia-bson" ,julia-bson)
       ;("julia-fillarrays" ,julia-fillarrays)
       ;("julia-linesearches" ,julia-linesearches)
       ;("julia-nlsolversbase" ,julia-nlsolversbase)
       ;("julia-nanmath" ,julia-nanmath)
       ;("julia-parameters" ,julia-parameters)
       ;("julia-positivefactorizations" ,julia-positivefactorizations)
       ;("julia-statsbase" ,julia-statsbase)
       ))
    (native-inputs
     `(
       ("julia-dataframes" ,julia-dataframes)
       ))
    (home-page "https://github.com/JuliaIO/BSON.jl")
    (synopsis "Binary JSON serialisation format")
    (description "@code{BSON.jl} is a Julia package for working with the Binary
JSON serialisation format.  It can be used as a general store for Julia data
structures.")
    (license license:expat)))

(define-public julia-dataframes
  (package
    (name "julia-dataframes")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/DataFrames.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ab03l9q9vmc176711hp0adc456fphh0d762fv6hcvzvhms4xjkz"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "SortingAlgorithms"
       ))
    (propagated-inputs
     `(
       ("julia-reexport" ,julia-reexport)
       ("julia-sortingalgorithms" ,julia-sortingalgorithms)
       ))
    (native-inputs
     `(
       ;("julia-dataframes" ,julia-dataframes)
       ))
    (home-page "https://dataframes.juliadata.org/stable/")
    (synopsis "In-memory tabular data")
    (description "Tools for working with tabular data in Julia.")
    (license license:expat)))

(define-public julia-parameters
  (package
    (name "julia-parameters")
    (version "0.12.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mauro3/Parameters.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0b8lawi7kcws4axfsdf023gyxca15irl648ciyi1kw3wghz3pfi2"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-orderedcollections" ,julia-orderedcollections)
       ("julia-unpack" ,julia-unpack)))
    (home-page "https://github.com/mauro3/Parameters.jl")
    (synopsis "Types with default field values, keyword constructors and (un-)pack macros")
    (description "This is a package I use to handle numerical-model parameters, thus the name.  However, it should be useful otherwise too.")
    (license license:expat)))

(define-public julia-unpack
  (package
    (name "julia-unpack")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mauro3/UnPack.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "066v1px72zidnvhl0rczhh07rcfwvli0jx5nprrgyi1dvj3mps2a"))))
    (build-system julia-build-system)
    (home-page "https://github.com/mauro3/UnPack.jl")
    (synopsis "Pack and Unpack macros for Julia")
    (description "The @code{@@unpack} and @code{@@pack!} macros work to unpack
types, modules, and dictionaries.")
    (license license:expat)))

;; TODO: unbundle javascript calls to cdn.jsdelivr.net
(define-public julia-pluto
  (package
    (name "julia-pluto")
    (version "0.14.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fonsp/Pluto.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1dvgrj0likniafs06hrwfndbshqr5khdqdyylganc1m81652rz5x"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Many tests need network connectivity or a browser.
    (propagated-inputs
     `(("julia-configurations" ,julia-configurations)
       ("julia-fuzzycompletions" ,julia-fuzzycompletions)
       ("julia-http" ,julia-http)
       ("julia-msgpack" ,julia-msgpack)
       ("julia-tableiointerface" ,julia-tableiointerface)))
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
    (description "A tiny package to make html\"<input>\" a bit more Julian.  Use
it with the @code{@@bind} macro in Pluto.")
    (license license:expat)))

(define-public julia-configurations
  (package
    (name "julia-configurations")
    (version "0.15.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Roger-luo/Configurations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1dz1h64nqgcv6ai70pfv2dv4mqx9rqmh08196k7j73bqlc6r00w1"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; cannot find test/option.toml
    (propagated-inputs
     `(("julia-crayons" ,julia-crayons)
       ("julia-exproniconlite" ,julia-exproniconlite)
       ("julia-orderedcollections" ,julia-orderedcollections)
       ("julia-toml" ,julia-toml)))
    (home-page "https://configurations.rogerluo.dev/stable")
    (synopsis "Options & Configurations made easy")
    (description "Configurations is a Julia Language package.")
    (license license:expat)))

;; XXX: Part of base Julia as of 1.6+
(define-public julia-toml
  (package
    (name "julia-toml")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLang/TOML.jl")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "15qmgy3jpyw6h938kg2fc9h896rbskdjgaimj118p3mg4mln4gci"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaLang/TOML.jl")
    (synopsis "TOML parser for TOML 1.0 written in Julia")
    (description "TOML v1.0.0 parser for Julia.")
    (license license:expat)))

(define-public julia-expronicon
  (package
    (name "julia-expronicon")
    (version "0.6.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Roger-luo/Expronicon.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0lbzfn1li2ph02z6hl5286bj6bf17g63vfp6qn4cz40d760fcw8a"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ))
    (propagated-inputs
     `(
       ;("julia-mlstyle" ,julia-mlstyle)
       ))
    (home-page "https://expronicon.rogerluo.dev/")
    (synopsis "Collective tools for metaprogramming on Julia Expr")
    (description "Collective tools for metaprogramming on Julia Expr.")
    (license license:expat)))

(define-public julia-exproniconlite
  (package
    (name "julia-exproniconlite")
    (version "0.6.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Roger-luo/ExproniconLite.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "02zm5r3fi7zx4fnr2ikxpapb9rvmnqvklrfyd4j9418q1mryh04l"))))
    (build-system julia-build-system)
    (native-inputs
     `(("julia-documenter" ,julia-documenter)))
    (home-page "https://expronicon.rogerluo.dev/")
    (synopsis "Collective tools for metaprogramming on Julia Expr")
    (description "no fancy pattern matching, no dependencies, Light-weight Expronicon for low latency.

                 this package is generated by Expronicon, please refer to Expronicon for any issues")
    (license license:expat)))

(define-public julia-crayons
  (package
    (name "julia-crayons")
    (version "4.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/KristofferC/Crayons.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0v3zhjlnb2914bxcj4myl8pgb7m31p77aj2k1bckmqs96jdph10z"))))
    (build-system julia-build-system)
    (home-page "https://github.com/KristofferC/Crayons.jl")
    (synopsis "Colored and styled strings for terminals")
    (description "Crayons is a package that makes it simple to write strings in different colors and styles to terminals.  It supports the 16 system colors, both the 256 color and 24 bit true color extensions, and the different text styles available to terminals.")
    (license license:expat)))

(define-public julia-fuzzycompletions
  (package
    (name "julia-fuzzycompletions")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JunoLab/FuzzyCompletions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "07sv88c472n6w4x7diy952igbcfm1s104ysnnvprld83312siw06"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JunoLab/FuzzyCompletions.jl")
    (synopsis "Fuzzy completion provider for Julia")
    (description "FuzzyCompletions provides fuzzy completions for a Julia runtime session.")
    (license license:expat)))

(define-public julia-tableiointerface
  (package
    (name "julia-tableiointerface")
    (version "0.1.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lungben/TableIOInterface.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0p2fi9jbyfg2j6rysv4if7dx8qw2mssb04i75j1zq607j8707kvn"))))
    (build-system julia-build-system)
    (home-page "https://github.com/lungben/TableIOInterface.jl")
    (synopsis "File formats based on file extensions")
    (description "Tiny package for determination of tabular file formats based on file extensions.

                 It is intended to be the base both for TableIO.jl and for the Pluto.jl tabular data import functionality.")
    (license license:expat)))

(define-public julia-tables
  (package
    (name "julia-tables")
    (version "1.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/Tables.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1q0wh4031zdp40k44jaix19pzy6cnwsa2p0zfz6799jbyqkg4kr1"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ))
    (propagated-inputs
     `(
       ("julia-dataapi" ,julia-dataapi)
       ("julia-datavalueinterfaces" ,julia-datavalueinterfaces)
       ("julia-datavalues" ,julia-datavalues)
       ("julia-tabletraits" ,julia-tabletraits)
       ("julia-queryoperators" ,julia-queryoperators)
       ))
    (native-inputs
     `(
       ;("julia-imagemagick" ,julia-imagemagick)
       ))
    (home-page "https://github.com/JuliaData/Tables.jl")
    (synopsis "Interface for tables in Julia")
    (description "The Tables.jl package provides simple, yet powerful interface functions for working with all kinds tabular data.")
    (license license:expat)))

(define-public julia-datavalueinterfaces
  (package
    (name "julia-datavalueinterfaces")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/DataValueInterfaces.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0g2wj6q7jj956nx6g7dk8x7w1c4l2xcmnr1kq5x8s8fild9kslg8"))))
    (build-system julia-build-system)
    (home-page "https://github.com/queryverse/DataValueInterfaces.jl")
    (synopsis "")
    (description "This package allows a few \"forward\" definitions for the DataValues.jl package that other packages can utilize for integration without having to take direct dependencies.")
    (license license:expat)))

(define-public julia-tabletraits
  (package
    (name "julia-tabletraits")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/TableTraits.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "08ssb2630wm6j8f2qa985mn2vfibfm5kjcn4ayl2qkhfcyp8daw4"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-iteratorinterfaceextensions" ,julia-iteratorinterfaceextensions)))
    (home-page "https://github.com/queryverse/TableTraits.jl")
    (synopsis "Traits for julia tables")
    (description "TableTraits defines a generic interface for tabular data.")
    (license license:expat)))

(define-public julia-iteratorinterfaceextensions
  (package
    (name "julia-iteratorinterfaceextensions")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/IteratorInterfaceExtensions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1slpay1dhja8f9gy6z7b3psgvgcknn963dvfqqakvg1grk9ppa09"))))
    (build-system julia-build-system)
    (home-page "https://github.com/queryverse/IteratorInterfaceExtensions.jl")
    (synopsis "Traits for julia iterators")
    (description "IteratorInterfaceExtensions defines a small number of extensions to the iterator interface.")
    (license license:expat)))

(define-public julia-datavalues
  (package
    (name "julia-datavalues")
    (version "0.4.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/DataValues.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "15j3hrqq6nazn533bfsvg32xznacbzsl303j1qs48av59ppnvhhv"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-known-failing-tests
           (lambda _
             ;; See upstream report:
             ;; https://github.com/queryverse/DataValues.jl/issues/83
             (substitute* "test/array/test_reduce.jl"
               ((".*DataValue\\(mapreduce.*") "")
               ((".*DataValue\\(method\\(f.*") ""))
             #t)))))
    (propagated-inputs
     `(("julia-datavalueinterfaces" ,julia-datavalueinterfaces)))
    (home-page "https://github.com/queryverse/DataValues.jl")
    (synopsis "Missing values for julia")
    (description "This package provides the type DataValue that is used to represent missing data.")
    (license license:expat)))

(define-public julia-queryoperators
  (package
    (name "julia-queryoperators")
    (version "0.9.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/QueryOperators.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "06zm4cbn3x49lbpgshhdfvvmgz066qkc8q0d57igm5p8bcp6js22"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ))
    (propagated-inputs
     `(
       ("julia-datastructures" ,julia-datastructures)
       ("julia-iteratorinterfaceextensions" ,julia-iteratorinterfaceextensions)
       ("julia-tableshowutils" ,julia-tableshowutils)
       ))
    (native-inputs
     `(
       ;("julia-imagemagick" ,julia-imagemagick)
       ))
    (home-page "https://github.com/queryverse/QueryOperators.jl")
    (synopsis "Query operators for julia")
    (description "This package contains the underlying query operators that are exposed to users in Query.jl.")
    (license license:expat)))

(define-public julia-tableshowutils
  (package
    (name "julia-tableshowutils")
    (version "0.2.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/TableShowUtils.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1x1ivz35xbc38x4zm720a6wycqb5gl8dfsxz83ss5gc5rdrcxm2j"))))
    (build-system julia-build-system)
    (arguments
     `(#:julia-package-name "TableShowUtils"))
    (propagated-inputs
     `(("julia-datavalues" ,julia-datavalues)
       ("julia-json" ,julia-json)))
    (home-page "https://github.com/queryverse/TableShowUtils.jl")
    (synopsis "helpers for implementing show for TableTraits.jl types")
    (description "This package provides some common helper functions that make it easier to implement various Base.show functions for types that participate in the TableTraits.jl ecosystem.")
    (license license:expat)))

(define-public julia-missings
  (package
    (name "julia-missings")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/Missings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "131ma44yvswvj85jdjhm37frzfz46cc60lwj65a9jcmgc77dshsm"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-dataapi" ,julia-dataapi)))
    (home-page "https://github.com/JuliaData/Missings.jl")
    (synopsis "Missing value support for Julia")
    (description "Part of Julia since 1.0")
    (license license:expat)))

(define-public julia-statsapi
  (package
    (name "julia-statsapi")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com.cnpmjs.org/JuliaStats/StatsAPI.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1k1c3s7i5wzkz4r9fyy4gd7wb97p0qgbc7bmaajm16zqipfmy2bv"))))
    (build-system julia-build-system)
    (home-page "https://juliastats.org/")
    (synopsis "Statistics-focused namespace for packages to share functions")
    (description "This package provides a namespace for data-related generic function definitions to solve the optional dependency problem; packages wishing to share and/or extend functions can avoid depending directly on each other by moving the function definition to StatsAPI.jl and each package taking a dependency on it.")
    (license license:expat)))

(define-public julia-stablerngs
  (package
    (name "julia-stablerngs")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaRandom/StableRNGs.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1cw4wc38qbgmrrx0jjwjhynnarrzjkh0yyz242zj272brbci7p1r"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaRandom/StableRNGs.jl")
    (synopsis "Julia RNG with stable streams")
    (description "This package intends to provide a simple RNG with stable streams, suitable for tests in packages which need reproducible streams of random numbers across Julia versions. Indeed, the Julia RNGs provided by default are documented to have non-stable streams (which for example enables some performance improvements).")
    (license license:expat)))

(define-public julia-pdmats
  (package
    (name "julia-pdmats")
    (version "0.11.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/PDMats.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1gyhfjmb0qlqgx2398b356cph25bnpjagcslckv41bzyf8pg3ybl"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaStats/PDMats.jl")
    (synopsis "Uniform Interface for positive definite matrices of various structures")
    (description "PDMats.jl supports efficient computation on positive definite matrices of various structures.  In particular, it provides uniform interfaces to use positive definite matrices of various structures for writing generic algorithms, while ensuring that the most efficient implementation is used in actual computation.")
    (license license:expat)))

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
    (propagated-inputs
     `(
       ("julia-logexpfunctions" ,julia-logexpfunctions)
       ("julia-rmath" ,julia-rmath)
       ("julia-specialfunctions" ,julia-specialfunctions)
       ))
    (home-page "https://github.com/JuliaStats/StatsFuns.jl")
    (synopsis "Mathematical functions related to statistics")
    (description "This package provides a collection of mathematical constants and numerical functions for statistical computing.")
    (license license:expat)))

(define-public julia-logexpfunctions
  (package
    (name "julia-logexpfunctions")
    (version "0.2.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/LogExpFunctions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1dv4ghd23f96276wcc9ibkazbgx0y0xkr8na6z82r3b786vz974a"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-docstringextensions" ,julia-docstringextensions)))
    (native-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)))
    (home-page "https://github.com/JuliaStats/LogExpFunctions.jl")
    (synopsis "special functions based on `log` and `exp`")
    (description "Various special functions based on log and exp moved from StatsFuns.jl into a separate package, to minimize dependencies. These functions only use native Julia code, so there is no need to depend on librmath or similar libraries.")
    (license license:expat)))

(define-public rmath-julia
  (package
    (name "rmath-julia")
    (version "0.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/Rmath-julia")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "11a6h3wwmpnb2d55pkm6av111b3pxlvxfnbz8b0n77afpllgb8j2"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f  ; Test not defined, tests not often run upstream.
       #:phases
       (modify-phases %standard-phases
         (delete 'precompile)
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
    (description "This is a slightly modified version of the standalone Rmath library from R, built to be used with the Rmath.jl Julia package.

                 The main difference is that it is built to allow defining custom random number generating functions via C function pointers (see include/callback.h). When using the library, these should be defined before calling any of the random functions.")
    (properties '((hidden? . #t)))
    (license license:gpl2)))

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
     `(;#:tests? #f  ; Test not defined
       #:phases
       (modify-phases %standard-phases
         ;(add-after 'unpack 'patch-source
         ;  (lambda _
         ;    ;; see upstream julia bug
         ;    (substitute* "src/Rmath.jl"
         ;      (("libRmath\\)") "libRmath_path)"))
         ;    #t))
         )
       ))
    (propagated-inputs
     `(
       ("julia-rmath-jll" ,julia-rmath-jll)
       ))
    (native-inputs
     `(
       ;("julia-offsetarrays" ,julia-offsetarrays)
       ))
    (home-page "https://github.com/JuliaStats/Rmath.jl")
    (synopsis "functions that emulate R's d-p-q-r functions for probability distributions")
    (description "

                 Archive of functions that emulate R's d-p-q-r functions for probability distributions.")
    (license license:expat)))

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
     `(("rmath" ,rmath-julia)))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)))
    (home-page "https://github.com/JuliaBinaryWrappers/Rmath_jll.jl")
    (synopsis "Rmath library wrappers")
    (description "This package provides a wrapper for Rmath.")
    (license license:expat)))

(define-public julia-static
  (package
    (name "julia-static")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/Static.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "01rbiysrkaca03gh55rc5zykkp63bhzaqgrxxj88lnisrbzmf0d2"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-ifelse" ,julia-ifelse)))
    (native-inputs
     `(("julia-aqua" ,julia-aqua)))
    (home-page "https://github.com/SciML/Static.jl")
    (synopsis "Static types useful for dispatch and generated functions")
    (description "Static defines a limited set of statically parameterized types
and a common interface that is shared between them.")
    (license license:expat)))

(define-public julia-aqua
  (package
    (name "julia-aqua")
    (version "0.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaTesting/Aqua.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0zcvrwnyhh2kr4d2xv7ps8dh7byw78dx6yb1m9m4dblgscn5kypb"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaTesting/Aqua.jl")
    (synopsis "Automated quality assurance for Julia packages")
    (description "@acronym{Aqua.jl, Auto QUality Assurance for Julia packages},
provides functions to run a few automatable checks for Julia packages.")
    (license license:expat)))

(define-public julia-bandedmatrices
  (package
    (name "julia-bandedmatrices")
    (version "0.16.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/BandedMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "105y5d3208s0byk3p0469sfy79lhjpdblk6karbwj8x7hl26na00"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ))
    (propagated-inputs
     `(
       ("julia-arraylayouts" ,julia-arraylayouts)
       ("julia-fillarrays" ,julia-fillarrays)
       ))
    (native-inputs
     `(
       ;("julia-blockbandedmatrices" ,julia-blockbandedmatrices)
       ("julia-genericlinearalgebra" ,julia-genericlinearalgebra)
       ))
    (home-page "https://github.com/JuliaMatrices/BandedMatrices.jl")
    (synopsis "Julia package for representing banded matrices")
    (description "This package supports representing banded matrices by only
the entries on the bands.")
    (license license:expat)))

(define-public julia-blockbandedmatrices
  (package
    (name "julia-blockbandedmatrices")
    (version "0.10.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/BlockBandedMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0q9ni4pgdkb00jb42fdzlhx745852xx2666vr96k0c4l0cn5mi0y"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(
       ("julia-arraylayouts" ,julia-arraylayouts)
       ("julia-bandedmatrices" ,julia-bandedmatrices)
       ("julia-blockarrays" ,julia-blockarrays)
       ("julia-fillarrays" ,julia-fillarrays)
       ("julia-matrixfactorizations" ,julia-matrixfactorizations)
       ))
    (home-page "https://github.com/JuliaMatrices/BlockBandedMatrices.jl")
    (synopsis "Block-banded matrices and banded-block-banded matrices")
    (description "This package supports representing block-banded and
banded-block-banded matrices by only storing the entries in the non-zero bands.
A @code{BlockBandedMatrix} is a subtype of @code{BlockMatrix} of BlockArrays.jl
whose layout of non-zero blocks is banded.")
    (license license:expat)))

(define-public julia-blockarrays
  (package
    (name "julia-blockarrays")
    (version "0.15.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/BlockArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "15nd493bfkx92ihnr8dj8mb155dj44iqw266igv0qr5q0wad2bfr"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-arraylayouts" ,julia-arraylayouts)
       ("julia-fillarrays" ,julia-fillarrays)))
    (native-inputs
     `(("julia-lazyarrays" ,julia-lazyarrays)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-staticarrays" ,julia-staticarrays)))
    (home-page "https://github.com/JuliaArrays/BlockArrays.jl")
    (synopsis "BlockArrays for Julia")
    (description "A block array is a partition of an array into blocks or
subarrays.  This package has two purposes.  Firstly, it defines an interface for
an @code{AbstractBlockArray} block arrays that can be shared among types
representing different types of block arrays.  The advantage to this is that it
provides a consistent API for block arrays.
Secondly, it also implements two different type of block arrays that follow the
@code{AbstractBlockArray} interface.  The type @code{BlockArray} stores each
block contiguously while the type @code{PseudoBlockArray} stores the full matrix
contiguously.  This means that @code{BlockArray} supports fast non copying
extraction and insertion of blocks while @code{PseudoBlockArray} supports fast
access to the full matrix to use in in for example a linear solver.")
    (license license:expat)))

(define-public julia-arraylayouts
  (package
    (name "julia-arraylayouts")
    (version "0.7.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/ArrayLayouts.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "01725v4jp8h8zwn85splw907r206h1hnp205pchmzjin7h4659xz"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-fillarrays" ,julia-fillarrays)))
    (home-page "https://github.com/JuliaMatrices/ArrayLayouts.jl")
    (synopsis "describing array layouts and more general fast linear algebra")
    (description "This package implements a trait-based framework for describing
array layouts such as column major, row major, etc. that can be dispatched to
appropriate BLAS or optimised Julia linear algebra routines.  This supports a
much wider class of matrix types than Julia's in-built @code{StridedArray}.")
    (license license:expat)))

(define-public julia-lazyarrays
  (package
    (name "julia-lazyarrays")
    (version "0.21.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/LazyArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0l427zghlpwcr6zcfy28sw3wm3y5zm0ckl008cgf2pwrpjfrahyw"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-arraylayouts" ,julia-arraylayouts)
       ("julia-fillarrays" ,julia-fillarrays)
       ("julia-macrotools" ,julia-macrotools)
       ("julia-matrixfactorizations" ,julia-matrixfactorizations)
       ("julia-staticarrays" ,julia-staticarrays)))
    (native-inputs
     `(("julia-tracker" ,julia-tracker)))
    (home-page "https://github.com/JuliaArrays/LazyArrays.jl")
    (synopsis "Lazy arrays and linear algebra")
    (description "This package supports lazy analogues of array operations like
@code{vcat}, @code{hcat}, and multiplication.  This helps with the
implementation of matrix-free methods for iterative solvers.")
    (license license:expat)))

(define-public julia-matrixfactorizations
  (package
    (name "julia-matrixfactorizations")
    (version "0.8.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/MatrixFactorizations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "14c6w1vhyf4pi4454pdp6ryczsxn9pgjg99fg9bkdj03xg5fsxb8"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-failing-test
           (lambda _
             ;; Tests with math functions are hard.
             (substitute* "test/test_ul.jl"
               (("@test @inferred\\(logdet") "@test @test_nowarn(logdet")
               ;; Also skip the REPL test.
               (("test String") "test_nowarn String"))
             #t)))))
    (propagated-inputs
     `(("julia-arraylayouts" ,julia-arraylayouts)))
    (home-page "https://github.com/JuliaMatrices/MatrixFactorizations.jl")
    (synopsis "Julia package to contain non-standard matrix factorizations")
    (description "A Julia package to contain non-standard matrix factorizations.
At the moment it implements the QL, RQ, and UL factorizations, a combined
Cholesky factorization with inverse, and polar decompositions.  In the future it
may include other factorizations such as the LQ factorization.")
    (license license:expat)))

(define-public julia-tracker
  (package
    (name "julia-tracker")
    (version "0.2.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/Tracker.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1s4mdywbp7nli7z985fqaj1rs4i6d92b1jx3lhg0qhk1s5wc0v8j"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-adapt" ,julia-adapt)
       ("julia-diffrules" ,julia-diffrules)
       ("julia-forwarddiff" ,julia-forwarddiff)
       ("julia-macrotools" ,julia-macrotools)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-nnlib" ,julia-nnlib)
       ("julia-requires" ,julia-requires)
       ("julia-specialfunctions" ,julia-specialfunctions)))
    (native-inputs
     `(("julia-pdmats" ,julia-pdmats)))
    (home-page "https://github.com/FluxML/Tracker.jl")
    (synopsis "Flux's ex AD")
    (description "Flux's old AD, now replaced with Zygote.")
    (license license:expat)))

(define-public julia-nnlib
  (package
    (name "julia-nnlib")
    (version "0.7.19")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/NNlib.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "163v7hkmbxxgnq7qigmqjdqcdywi2njxbh54w8v0hf4bddnalbba"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; skip so we can drop CUDA.jl
    (propagated-inputs
     `(("julia-adapt" ,julia-adapt)
       ("julia-chainrulescore" ,julia-chainrulescore)
       ("julia-requires" ,julia-requires)))
    (native-inputs
     `(("julia-chainrulestestutils" ,julia-chainrulestestutils)
       ;("julia-cuda" ,julia-cuda)
       ("julia-stablerngs" ,julia-stablerngs)
       ("julia-zygote" ,julia-zygote)))
    (home-page "https://github.com/FluxML/NNlib.jl")
    (synopsis "Neural Network primitives with multiple backends")
    (description "This package will provide a library of functions useful for
machine learning, such as softmax, sigmoid, convolutions and pooling.  It
doesn't provide any other \"high-level\" functionality like layers or AD.")
    (license license:expat)))

(define-public julia-genericlinearalgebra
  (package
    (name "julia-genericlinearalgebra")
    (version "0.2.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLinearAlgebra/GenericLinearAlgebra.jl/")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ndwypa397z3pwzdgc3s9plaqlqf63g3d4px5pvym5psgr6lnm3l"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; come back to this later
    (native-inputs
     `(("julia-quaternions" ,julia-quaternions)))
    (home-page "https://github.com/JuliaLinearAlgebra/GenericLinearAlgebra.jl/")
    (synopsis "Generic numerical linear algebra")
    (description "The purpose of this package is partly to extend linear algebra functionality in base to cover generic element types, e.g. @code{BigFloat} and @code{Quaternion}, and partly to be a place to experiment with fast linear algebra routines written in Julia (except for optimized BLAS). It is my hope that it is possible to have implementations that are generic, fast, and readable.")
    (license license:expat)))

(define-public julia-quaternions
  (package
    (name "julia-quaternions")
    (version "0.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGeometry/Quaternions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1zhynyvchc50hywws2jznpkwydr3njh8cv84d2ylyabhcwwmil9s"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-dualnumbers" ,julia-dualnumbers)))
    (home-page "https://github.com/JuliaGeometry/Quaternions.jl")
    (synopsis "Julia module with quaternion and dual-quaternion functionality")
    (description "Quaternions are best known for their suitability as representations of 3D rotational orientation. They can also be viewed as an extension of complex numbers.")
    (license license:expat)))

(define-public julia-dualnumbers
  (package
    (name "julia-dualnumbers")
    (version "0.6.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDiff/DualNumbers.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "05vr5wbzqpchnb96b3pmn67x196mbfnkv7r9bdlz3gm56if4awk5"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; fail hard on test/runtests.jl:4
    (propagated-inputs
     `(("julia-calculus" ,julia-calculus)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-specialfunctions" ,julia-specialfunctions)))
    (home-page "https://github.com/JuliaDiff/DualNumbers.jl")
    (synopsis "representing dual numbers and for performing dual algebra")
    (description "The @code{DualNumbers} Julia package defines the @code{Dual} type to represent dual numbers, and supports standard mathematical operations on them. Conversions and promotions are defined to allow performing operations on combinations of dual numbers with predefined Julia numeric types.")
    (license license:expat)))

(define-public julia-optimtestproblems
  (package
    (name "julia-optimtestproblems")
    (version "2.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/OptimTestProblems.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "10h47x5ws42pkqjccimaz0yxfvz41w0yazq6inamfk4lg5g2g3d9"))))
    (build-system julia-build-system)
    (arguments
     `(#:julia-package-name "OptimTestProblems"))
    (home-page "https://github.com/JuliaNLSolvers/OptimTestProblems.jl")
    (synopsis "A collection of test problems for optimization problems")
    (description "The purpose of this package is to provide test problems for
JuliaNLSolvers packages.")
    (license license:expat)))

(define-public julia-recursivearraytools
  (package
    (name "julia-recursivearraytools")
    (version "2.11.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/RecursiveArrayTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "12z7w1wxjjcy5rnjd1bj5bmpdspv5ix6ifq0ql67d32vlghiyn3h"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Don't pull in OrdinaryDiffEq.jl
    (propagated-inputs
     `(("julia-arrayinterface" ,julia-arrayinterface)
       ("julia-docstringextensions" ,julia-docstringextensions)
       ("julia-requires" ,julia-requires)
       ("julia-recipesbase" ,julia-recipesbase)
       ("julia-staticarrays" ,julia-staticarrays)
       ("julia-zygoterules" ,julia-zygoterules)))
    (native-inputs
     `(
       ;("julia-forwarddiff" ,julia-forwarddiff)
       ;("julia-nlsolve" ,julia-nlsolve)
       ;("julia-ordinarydiffeq" ,julia-ordinarydiffeq)
       ("julia-unitful" ,julia-unitful)
       ;("julia-zygote" ,julia-zygote)
       ))
    (home-page "https://github.com/SciML/RecursiveArrayTools.jl")
    (synopsis "Tools for handling objects like arrays of arrays and deeper nestings")
    (description "RecursiveArrayTools.jl is a set of tools for dealing with
recursive arrays like arrays of arrays.")
    (license license:expat)))

(define-public julia-recipesbase
  (package
    (name "julia-recipesbase")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/RecipesBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1b6m5rz6wprj30rwvlxz4r1jv5gl0ay0f52kfmy2w7lqly7zhap5"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaPlots/RecipesBase.jl")
    (synopsis "Define transformation recipes on user types")
    (description "This package implements handy macros @code{@@recipe} and @code{@@series} which will define a custom transformation and attach attributes for user types. Its design is an attempt to simplify and generalize the summary and display of types and data from external packages. With no extra dependencies and minimal code, package authors can describe visualization routines that can be used as components in more complex visualizations.")
    (license license:expat)))

(define-public julia-ordinarydiffeq
  (package
    (name "julia-ordinarydiffeq")
    (version "5.53.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/OrdinaryDiffEq.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0xxn7ga5ii3v2430aj9b7mpiiwjw8vvip8afdyq04rni07d0cpmx"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ))
    (propagated-inputs
     `(
       ("julia-adapt" ,julia-adapt)
       ("julia-arrayinterface" ,julia-arrayinterface)
       ("julia-diffeqbase" ,julia-diffeqbase)
       ("julia-docstringextensions" ,julia-docstringextensions)
       ("julia-reexport" ,julia-reexport)
       ;("julia-recipesbase" ,julia-recipesbase)
       ;("julia-staticarrays" ,julia-staticarrays)
       ;("julia-zygoterules" ,julia-zygoterules)
       ))
    (native-inputs
     `(
       ;("julia-forwarddiff" ,julia-forwarddiff)
       ;("julia-nlsolve" ,julia-nlsolve)
       ;("julia-ordinarydiffeq" ,julia-ordinarydiffeq)
       ("julia-safetestsets" ,julia-safetestsets)
       ;("julia-zygote" ,julia-zygote)
       ))
    (home-page "https://github.com/SciML/OrdinaryDiffEq.jl")
    (synopsis "High performance differential equation solvers for ordinary differential equations")
    (description "OrdinaryDiffEq.jl is a component package in the DifferentialEquations ecosystem. It holds the ordinary differential equation solvers and utilities.")
    (license license:expat)))

(define-public julia-diffeqbase
  (package
    (name "julia-diffeqbase")
    (version "6.61.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/DiffEqBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "16nwsw08gl17zwqw5jqg3r8b42pgzbd6m2na7c8yvvswy9s3sygl"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ))
    (propagated-inputs
     `(
       ("julia-arrayinterface" ,julia-arrayinterface)
       ;("julia-diffeqbase" ,julia-diffeqbase)
       ;("julia-docstringextensions" ,julia-docstringextensions)
       ;("julia-iterativesolvers" ,julia-iterativesolvers)
       ;("julia-reexport" ,julia-reexport)
       ;("julia-recipesbase" ,julia-recipesbase)
       ;("julia-staticarrays" ,julia-staticarrays)
       ;("julia-zygoterules" ,julia-zygoterules)
       ))
    (native-inputs
     `(
       ;("julia-forwarddiff" ,julia-forwarddiff)
       ;("julia-nlsolve" ,julia-nlsolve)
       ;("julia-ordinarydiffeq" ,julia-ordinarydiffeq)
       ("julia-safetestsets" ,julia-safetestsets)
       ;("julia-zygote" ,julia-zygote)
       ))
    (home-page "https://github.com/SciML/DiffEqBase.jl")
    (synopsis "Base library for shared types and functionality for defining differential equation and scientific machine learning (SciML) problems")
    (description "DiffEqBase.jl is a component package in the DiffEq ecosystem.  It holds the common types and utility functions which are shared by other component packages in order to reduce the size of dependencies.  This is so that the packages for the common interface do not require one another, allowing users to use the functionality of individual packages if they so please.")
    (license license:expat)))

(define-public julia-positivefactorizations
  (package
    (name "julia-positivefactorizations")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/PositiveFactorizations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wxy6ak7f3hvibcgc8q88cgkf9zvi649mmjy1zlkx1qk80hgvz23"))))
    (build-system julia-build-system)
    (native-inputs
     `(("julia-forwarddiff" ,julia-forwarddiff)
       ("julia-reversediff" ,julia-reversediff)))
    (home-page "https://github.com/timholy/PositiveFactorizations.jl")
    (synopsis "Positive-definite \"approximations\" to matrices")
    (description "PositiveFactorizations is a package for computing a positive
definite matrix decomposition (factorization) from an arbitrary symmetric input.
The motivating application is optimization (Newton or quasi-Newton methods), in
which the canonical search direction -H/g (H being the Hessian and g the
gradient) may not be a descent direction if H is not positive definite.")
    (license license:expat)))

;; Ready to upstream
(define-public julia-reversediff
  (package
    (name "julia-reversediff")
    (version "1.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDiff/ReverseDiff.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wrr6sqj2xl9grkvdp88rw3manxy9vbx28zq2wssya5ns1xabsnl"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-diffresults" ,julia-diffresults)
       ("julia-diffrules" ,julia-diffrules)
       ("julia-forwarddiff" ,julia-forwarddiff)
       ("julia-functionwrappers" ,julia-functionwrappers)
       ("julia-macrotools" ,julia-macrotools)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-specialfunctions" ,julia-specialfunctions)
       ("julia-staticarrays" ,julia-staticarrays)))
    (native-inputs
     `(("julia-difftests" ,julia-difftests)
       ("julia-fillarrays" ,julia-fillarrays)))
    (home-page "https://github.com/JuliaDiff/ReverseDiff.jl")
    (synopsis "Reverse Mode Automatic Differentiation for Julia")
    (description "ReverseDiff is a fast and compile-able tape-based reverse mode
@acronym{AD, automatic differentiation}, that implements methods to take
gradients, Jacobians, Hessians, and higher-order derivatives of native Julia
functions (or any callable object, really).")
    (license license:expat)))

(define-public julia-functionwrappers
  (package
    (name "julia-functionwrappers")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yuyichao/FunctionWrappers.jl/")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02jilpjr7px6138dx2w7ixricvfgsxqdk84d9dgviranibhnjcxa"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))        ; TODO: Fix test failure
    (home-page "https://github.com/yuyichao/FunctionWrappers.jl/")
    (synopsis "Type stable and efficient wrapper of arbitrary functions")
    (description "This package provides type stable and efficient wrapper of arbitrary functions.")
    (license license:expat)))

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
     `(#:tests? #f))    ; Tests require Optim.jl, creating a cycle.
    (propagated-inputs
     `(("julia-doublefloats" ,julia-doublefloats)
       ("julia-nlsolversbase" ,julia-nlsolversbase)
       ("julia-nanmath" ,julia-nanmath)
       ("julia-parameters" ,julia-parameters)))
    (native-inputs
     `(("julia-doublefloats" ,julia-doublefloats)
       ;("julia-optim" ,julia-optim)
       ("julia-optimtestproblems" ,julia-optimtestproblems)))
    (home-page "https://github.com/JuliaNLSolvers/LineSearches.jl")
    (synopsis "Line search methods for optimization and root-finding")
    (description "This package provides an interface to line search algorithms
implemented in Julia.")
    (license license:expat)))

(define-public julia-doublefloats
  (package
    (name "julia-doublefloats")
    (version "1.1.21")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/DoubleFloats.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0bpx2y05mmnk77lsg3gnxcjvag5h75nk5pyv0xrw53a8b62ja57y"))))
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

(define-public julia-polynomials
  (package
    (name "julia-polynomials")
    (version "2.0.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Polynomials.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0mz7ls281d6166w9808lwgn007dsk8pqi4qmdf0jiiipy5a0a4ji"))))
    (build-system julia-build-system)
    ;(arguments
    ; `(#:tests? #f))
    (propagated-inputs
     `(
       ("julia-intervals" ,julia-intervals)
       ("julia-mutablearithmetics" ,julia-mutablearithmetics)
       ;("julia-recipesbase" ,julia-recipesbase)
       ))
    (native-inputs
     `(
       ("julia-specialfunctions" ,julia-specialfunctions)
       ))
    (home-page "https://github.com/JuliaMath/Polynomials.jl")
    (synopsis "Polynomial manipulations in Julia")
    (description "This package provides basic arithmetic, integration,
differentiation, evaluation, and root finding over dense univariate
polynomials.")
    (license license:expat)))

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
     `(
       ("julia-documenter" ,julia-documenter)
       ("julia-infinity" ,julia-infinity)
       ("julia-recipesbase" ,julia-recipesbase)
       ))
    (native-inputs
     `(
       ))
    (home-page "https://github.com/invenia/Intervals.jl")
    (synopsis "Non-iterable ranges")
    (description "This package defines:

                     AbstractInterval, along with its subtypes Interval and AnchoredInterval, and also Bound.")
    (license license:expat)))

(define-public julia-infinity
  (package
    (name "julia-infinity")
    (version "0.2.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cjdoris/Infinity.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1bw7p88l8svb7455srz0jmw8ap17r2wwgz5y02vr9s8cg4lbsps5"))))
    (build-system julia-build-system)
    ;(arguments
    ; `(#:tests? #f))
    (propagated-inputs
     `(
       ("julia-compat" ,julia-compat)
       ("julia-requires" ,julia-requires)
       ("julia-timezones" ,julia-timezones)
       ))
    (native-inputs
     `(
       ("julia-compat" ,julia-compat)
       ("julia-timezones" ,julia-timezones)
       ))
    (home-page "https://juliahub.com/docs/Infinity/")
    (synopsis "Representation of infinity in Julia")
    (description "This package provides representations for infinity and negative infinity in Julia.")
    (license license:gpl3)))

;; TODO: Keep this in sync with tzdata in base.scm
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

(define-public julia-mocking
  (package
    (name "julia-mocking")
    (version "0.7.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/invenia/Mocking.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "10jz716v6i3gpd403rmcrip6cncjl9lqr12cdl321x1994a5g8ck"))))
    (build-system julia-build-system)
    ;(arguments
    ; `(#:tests? #f))
    (propagated-inputs
     `(
       ("julia-exprtools" ,julia-exprtools)
       ;("julia-recipesbase" ,julia-recipesbase)
       ))
    (native-inputs
     `(
       ;("julia-compat" ,julia-compat)
       ;("julia-timezones" ,julia-timezones)
       ))
    (home-page "https://github.com/invenia/Mocking.jl")
    (synopsis "Julia function calls to be temporarily overloaded for purpose of testing")
    (description "Allows Julia function calls to be temporarily overloaded for purpose of testing.")
    (license license:expat)))

(define-public julia-exprtools
  (package
    (name "julia-exprtools")
    (version "0.1.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com.cnpmjs.org/invenia/ExprTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lwxi9fx9farf1jdv42gv43xs3f3i3js2xnvr5gf6d0xfx0g6b6a"))))
    (build-system julia-build-system)
    ;(arguments
    ; `(#:tests? #f))
    (propagated-inputs
     `(
       ("julia-recipesbase" ,julia-recipesbase)
       ))
    (native-inputs
     `(
       ;("julia-compat" ,julia-compat)
       ;("julia-timezones" ,julia-timezones)
       ))
    (home-page "https://github.com.cnpmjs.org/invenia/ExprTools.jl")
    (synopsis "Light-weight expression manipulation tools")
    (description "ExprTools provides tooling for working with Julia expressions during metaprogramming. This package aims to provide light-weight performant tooling without requiring additional package dependencies.")
    (license license:expat)))

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
    ;(arguments
    ; `(#:tests? #f))
    (propagated-inputs
     `(
       ;("julia-recipesbase" ,julia-recipesbase)
       ))
    (native-inputs
     `(
       ;("julia-compat" ,julia-compat)
       ;("julia-timezones" ,julia-timezones)
       ))
    (home-page "https://github.com/JuliaPackaging/LazyArtifacts.jl")
    (synopsis "LazyArtifacts support for older versions of Julia")
    (description "This is a wrapper package meant to bridge the gap for packages that want to use the LazyArtifacts stdlib as a dependency within packages that still support Julia versions older than 1.6.")
    (license license:expat)))

(define-public julia-mutablearithmetics
  (package
    (name "julia-mutablearithmetics")
    (version "0.2.19")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jump-dev/MutableArithmetics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1zjfq5sh0rc941pbc9kvnr6a2lpr4yd276mw62vbncbz9jg52rrg"))))
    (build-system julia-build-system)
    ;(arguments
    ; `(#:tests? #f))
    (propagated-inputs
     `(
       ("julia-offsetarrays" ,julia-offsetarrays)
       ))
    (native-inputs
     `(
       ;("julia-sparsearrays" ,julia-sparsearrays)
       ))
    (home-page "https://github.com/jump-dev/MutableArithmetics.jl")
    (synopsis "Interface for arithmetics on mutable types in Julia")
    (description "MutableArithmetics (MA for short) is a Julia package which allows:

                     for mutable types to implement mutable arithmetics;
                         for algorithms that could exploit mutable arithmetics to exploit them while still being completely generic.
                         ")
    (license license:mpl2.0)))

(define-public julia-quadmath
  (package
    (name "julia-quadmath")
    (version "0.5.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Quadmath.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "051biw4b9zni7cmh2f1yzifp1v8wazlfxrdz4p44lyd1wba6379w"))))
    (build-system julia-build-system)
    ;(arguments
    ; `(#:tests? #f))
    (propagated-inputs
     `(
       ("julia-requires" ,julia-requires)
       ))
    (native-inputs
     `(
       ("julia-specialfunctions" ,julia-specialfunctions)
       ))
    (home-page "https://github.com/JuliaMath/Quadmath.jl")
    (synopsis "Float128 and libquadmath for the Julia language")
    (description "This is a Julia interface to libquadmath, providing a Float128 type corresponding to the IEEE754 binary128 floating point format.")
    (license license:expat)))

(define-public julia-genericschur
  (package
    (name "julia-genericschur")
    (version "0.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/RalphAS/GenericSchur.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0kklc2niylvynhq0v49kdmy58m9jmr5jxjf287k1wr9r81fya3sz"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; TODO: Fix later
    (propagated-inputs
     `(
       ;("julia-requires" ,julia-requires)
       ))
    (native-inputs
     `(
       ;("julia-specialfunctions" ,julia-specialfunctions)
       ))
    (home-page "https://github.com/RalphAS/GenericSchur.jl")
    (synopsis "Julia package for Schur decomposition of matrices with generic element types")
    (description "The Schur decomposition is the workhorse for eigensystem analysis of dense matrices. The diagonal eigen-decomposition of normal (especially Hermitian) matrices is an important special case, but for non-normal matrices the Schur form is often more useful.")
    (license license:expat)))
