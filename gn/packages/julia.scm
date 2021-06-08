(define-module (gn packages julia)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system julia)
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
       `(;("julia-distributions" ,julia-distributions)
         ;("julia-interactiveutils" ,julia-interactiveutils) ; Part of stdlib as of XXXX
         ("julia-latexstrings" ,julia-latexstrings)
         ;("julia-markdown" ,julia-markdown)                 ; Part of stdlib as of XXXX
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
         ;("julia-distributions" ,julia-distributions)
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
      (description "LiteQTL is a package that runs whole genome QTL scans near real-time, utilizing the computation power of GPU.
LiteQTL uses new algorithms that enables near-real time whole genome QTL scans for up to 1 million traits. By using easily parallelizable operations including matrix multiplication, vectorized operations, and element-wise operations, our method is about 300 times faster than a R/qtl linear model genome scan using 16 threads.")
      (license license:expat))))

;; contains bundled libraries?
(define-public julia-packagecompiler
  (package
    (name "julia-packagecompiler")
    (version "1.2.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLang/PackageCompiler.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1l0sc7dcx5zabvkjfzqjmni47c5w8fr5c6phkdcgrb1vqp90xvkx"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Tries to contact package repository.
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
       ("conda" ,(S "conda"))
       ("python" ,(S "python-wrapper"))
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

;; ready to upstream
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
         (base32 "117z27krcf8fydgp6mb0pgn75r4gng9qs7v90qb4bqzsry3faadp"))))
    (build-system julia-build-system)
    (native-inputs
     `(("julia-documenter" ,julia-documenter)))
    (home-page "https://github.com/stevengj/LaTeXStrings.jl")
    (synopsis "Input and display of LaTeX equation strings")
    (description "This is a small package to make it easier to type LaTeX
equations in string literals in the Julia language.")
    (license license:expat)))

(define-public julia-distributions
  (package
    (name "julia-distributions")
    (version "0.25.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/Distributions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0p3998sh667f1bskd011z9hfdkbdw5kgh9n1771jx4madxscy7dq"))))
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
       ("julia-statsfuns" ,julia-statsfuns)    ; fix Rmath.jl
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

;; TODO: Unbundle extra assets? assets/html/
;; see also: js inputs: src/Writers/HTMLWriter.jl
;;           latex inputs: src/Writers/LaTeXWriter.jl
;;           needs pip: src/Deps.jl
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
    (propagated-inputs
     `(("julia-docstringextensions" ,julia-docstringextensions)
       ;; TODO: Switch to julia-iocapture after 0.27.
       ("julia-iocapture" ,julia-iocapture-0.1)
       ("julia-json" ,julia-json)))
    (native-inputs
     `(("git" ,(S "git-minimal"))
       ("julia-documentermarkdown" ,julia-documentermarkdown)))
    (home-page "https://juliadocs.github.io/Documenter.jl")
    (synopsis "Documentation generator for Julia")
    (description "This package provides a documentation generator for Julia.")
    (license license:expat)))

;; Upstream with julia-documenter
(define-public julia-iocapture-0.1
  (package
    (inherit julia-iocapture)
    (name "julia-iocapture")
    (version "0.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/IOCapture.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0wm8pag5mk46064h3qpvgz8m63138104rq0smx1za7lh7j32925h"))))))

;; ready to upstream with julia-documenter
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
     `(#:tests? #f      ; Some tests require network.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-javascript-downloads
           (lambda _
             ;; This isn't problematic because we only use
             ;; this package for bootstrapping.
             (substitute* '("assets/html/documenter.js"
                            "assets/html/search.js"
                            "src/Writers/HTMLWriter.jl")
               (("https.*(min|css|js)$") ""))
             #t)))))
    (propagated-inputs
     `(("julia-docstringextensions" ,julia-docstringextensions)
       ("julia-json" ,julia-json)))
    (native-inputs `())
    (properties '((hidden? . #t)))))

;; ready to upstream; depends on julia-documenter
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
    (inputs
     ;; We don't want to propagate the bootstrap version.
     ;; Cycle with Documenter.jl in later versions.
     `(("julia-documenter" ,julia-documenter-0.22)))
    (home-page "https://github.com/JuliaDocs/DocumenterMarkdown.jl")
    (synopsis "Documenter's Markdown")
    (description "his package enables the Markdown / MkDocs backend of
@code{Documenter.jl}.")
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
     `(#:tests? #f  ; TODO: Fix test
       ))
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
     `(
       ("julia-optimtestproblems" ,julia-optimtestproblems)
       ("julia-recursivearraytools" ,julia-recursivearraytools)
       ))
    (home-page "https://github.com/JuliaNLSolvers/Optim.jl")
    (synopsis "Optimization functions for Julia")
    (description "Optim.jl is a package for univariate and multivariate optimization of functions.")
    (license license:expat)))

;; ready to upstream
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

;; ready to upstream
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
         (base32 "0ndazn02wn8ddwgjh1i32y7pbaqpw06f42ccilz5ya78cyrjhq2m"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; We don't want to run all the tests, the Downstream tests
             ;; try to download the package registry.
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

;; ready to upstream
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
current @code{AbstractArray} interface, which are put to use in package
ecosystems like @code{DifferentialEquations.jl}.  Since these libraries are
live, this package will serve as a staging ground for ideas before they are
merged into Base Julia.  For this reason, no functionality is exported so that
if such functions are added and exported in a future Base Julia, there will be
no issues with the upgrade.")
    (license license:expat)))

(define-public julia-plots
  (package
    (name "julia-plots")
    (version "1.15.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/Plots.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1bw76zzrq4zlwglhr7nkr1h0w0wl1i49rp35nnbbdqkdj46bz52y"))))
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
       ;("julia-fileio" ,julia-fileio)
       ;("julia-gtk" ,julia-gtk)
       ;("julia-hdf5" ,julia-hdf5)
       ("julia-imagemagick" ,julia-imagemagick)
       ;("julia-images" ,julia-images)
       ;("julia-libgit2" ,julia-libgit2)
       ;("julia-offsetarrays" ,julia-offsetarrays)
       ;("julia-pgfplotsx" ,julia-pgfplotsx)
       ;("julia-plotlyjs" ,julia-plotlyjs)
       ;("julia-rdatasets" ,julia-rdatasets)
       ;("julia-stablerngs" ,julia-stablerngs)
       ;("julia-staticarrays" ,julia-staticarrays)
       ;("julia-statsplots" ,julia-statsplots)
       ;("julia-testimages" ,julia-testimages)
       ;("julia-unicodeplots" ,julia-unicodeplots)
       ;("julia-visualregressiontests" ,julia-visualregressiontests)
       ))
    (home-page "http://docs.juliaplots.org/")
    (synopsis "Powerful convenience for Julia visualizations and data analysis")
    (description "Plots is a plotting API and toolset.")
    (license license:expat)))

;; ready to upstream
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
         (base32 "05vzv4jsj3l9pv6yrix28hlw7wnag0mqdfjwv8shn4x71hcfxl1p"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-failing-test
           (lambda _
             ;; These tests try to download from the imagemagick.org
             (substitute* "test/runtests.jl"
               ((".*readremote\\.jl.*") ""))
             ;; Tests with the color gray are hard.
             (substitute* "test/constructed_images.jl"
               (("test (b == aa)" _ test) (string-append "test_nowarn " test))
               (("test (B == map)" _ test) (string-append "test_nowarn " test)))
             #t)))))
    (propagated-inputs
     `(("julia-fileio" ,julia-fileio)
       ("julia-imagecore" ,julia-imagecore)
       ("julia-imagemagick-jll" ,julia-imagemagick-jll)))
    (native-inputs
     `(("julia-colors" ,julia-colors)
       ("julia-colorvectorspace" ,julia-colorvectorspace)
       ("julia-imagemetadata" ,julia-imagemetadata)
       ("julia-imageshow" ,julia-imageshow)
       ("julia-imagetransformations" ,julia-imagetransformations)
       ("julia-indirectarrays" ,julia-indirectarrays)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-zipfile" ,julia-zipfile)))
    (home-page "https://github.com/JuliaIO/ImageMagick.jl")
    (synopsis "Thin wrapper for ImageMagick")
    (description "This package provides a wrapper around ImageMagick version 6.
It was split off from @code{Images.jl} to make image I/O more modular.")
    (license license:expat)))

;; ready to upstream
(define-public julia-imagecore
  (package
    (name "julia-imagecore")
    (version "0.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageCore.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0d844lrilw7zjpax8zr5272a9s292wg4qk53mvm0n88yai598zd6"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with ImageMagick.jl
    (propagated-inputs
     `(("julia-abstractffts" ,julia-abstractffts)
       ("julia-colors" ,julia-colors)
       ("julia-colorvectorspace" ,julia-colorvectorspace)
       ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
       ("julia-graphics" ,julia-graphics)
       ("julia-mappedarrays" ,julia-mappedarrays)
       ("julia-mosaicviews" ,julia-mosaicviews)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-paddedviews" ,julia-paddedviews)
       ("julia-reexport" ,julia-reexport)))
    ;(native-inputs
    ; `(("julia-aqua" ,julia-aqua)
    ;   ("julia-colorvectorspace" ,julia-colorvectorspace)
    ;   ("julia-documenter" ,julia-documenter)
    ;   ("julia-fftw" ,julia-fftw)
    ;   ("julia-imageinterminal" ,julia-imageinterminal)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-referencetests" ,julia-referencetests)
    ;   ("julia-statistics" ,julia-statistics)))
    (home-page "https://github.com/JuliaImages/ImageCore.jl")
    (synopsis "Julia types for representing images")
    (description "@code{ImageCore} is the lowest-level component of the system
of packages designed to support image processing and computer vision.")
    (license license:expat)))

(define-public julia-fileio
  (package
    (name "julia-fileio")
    (version "1.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/FileIO.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15q4bmya5bn17pxfq1nbs1y05y723zb711ps0q8164c086yrj6d9"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f  ; skip for now
       #:phases
       (modify-phases %standard-phases
         (delete 'reset-gzip-timestamps))
       ))
    (propagated-inputs
     `(("julia-requires" ,julia-requires)))
    (native-inputs
     `(
       ;("julia-filepathsbase" ,julia-filepathsbase)
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
     `(#:tests? #f  ; not all dependants packaged
       ))
    (propagated-inputs
     `(("julia-dataapi" ,julia-dataapi)
       ("julia-invertedindices" ,julia-invertedindices)
       ("julia-missings" ,julia-missings)
       ("julia-pooledarrays" ,julia-pooledarrays)
       ("julia-prettytables" ,julia-prettytables)
       ("julia-reexport" ,julia-reexport)
       ("julia-sortingalgorithms" ,julia-sortingalgorithms)
       ("julia-tabletraits" ,julia-tabletraits)))
    (native-inputs
     `(
       ;("julia-categoricalarrays" ,julia-categoricalarrays)
       ;("julia-combinatorics" ,julia-combinatorics)
       ("julia-datastructures" ,julia-datastructures)
       ("julia-datavalues" ,julia-datavalues)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-unitful" ,julia-unitful)))
    (home-page "https://dataframes.juliadata.org/stable/")
    (synopsis "In-memory tabular data")
    (description "Tools for working with tabular data in Julia.")
    (license license:expat)))

;; TODO: unbundle javascript calls to cdn.jsdelivr.net
(define-public julia-pluto
  (package
    (name "julia-pluto")
    (version "0.14.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fonsp/Pluto.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0b2g3j78kpkayhrm3am855cc5kjb3w73ygcvjbvhz2p5i1ivji7b"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f      ; Many tests need network connectivity or a browser.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-check-for-upgrades
           (lambda _
             (substitute* "frontend/components/Welcome.js"
               (("local_index !== -1") "false"))
             #t)))))
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

;; autogenerated package?
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
    (native-inputs
     `(
       ("julia-forwarddiff" ,julia-forwarddiff)
       ))
    (home-page "https://github.com/JuliaStats/StatsFuns.jl")
    (synopsis "Mathematical functions related to statistics")
    (description "This package provides a collection of mathematical constants and numerical functions for statistical computing.")
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
    ;(version "0.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/Rmath.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0cam16ff4v2fl7c9j1wx2ahgjhwba9mk2q6qv3zdknnnqj6w664s"))))
         ;(base32 "1745xajy5c8hdcy1hgi2rr9lrapr55hp0jm2dcb1ksyskvm5drsr"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f  ; Test not defined
       #:phases
       (modify-phases %standard-phases
         ;(add-after 'unpack 'patch-source
         ;  (lambda _
         ;    ;; see upstream julia bug
         ;    ;; ERROR: LoadError: InitError: UndefVarError: libRmath_path not defined
         ;    (substitute* "src/Rmath.jl"
         ;      (("libRmath\\)") "libRmath_path)"))
         ;    #t))
         )
       ))
    (propagated-inputs
     `(
       ("julia-rmath-jll" ,julia-rmath-jll)
       ;("julia-rmath-jll" ,julia-rmath-jll-0.2)
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
                  ;(("libRmath-julia") "libRmath")
                  (("generate_wrapper_header.*")
                   (string-append
                     "generate_wrapper_header(\"Rmath\", \""
                     (assoc-ref inputs "rmath") "\")\n"))))
              ;; There's a Julia file for each platform, override them all
              (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     `(
       ;; It wants the custom rmath.
       ("rmath" ,rmath-julia)
       ;("rmath" ,(S "rmath-standalone"))
       ))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)))
    (home-page "https://github.com/JuliaBinaryWrappers/Rmath_jll.jl")
    (synopsis "Rmath library wrappers")
    (description "This package provides a wrapper for Rmath.")
    (license license:expat)))

(define-public julia-rmath-jll-0.2
  (package
    (name "julia-rmath-jll")
    (version "0.2.2+2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/Rmath_jll.jl")
               (commit (string-append "Rmath-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "13wvx4n0ai7bsda3rvlw8xbqwdbdwhjijbgjgl0k2yzq5l8x5dmh"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f ; no runtests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-binary-path
           (lambda* (#:key inputs #:allow-other-keys)
             (map
               (lambda (wrapper)
                 (substitute* wrapper
                   (("artifact\"Rmath\"")
                    (string-append "\"" (assoc-ref inputs "rmath") "\""))))
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

;; ready to upstream
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
    (description "ReverseDiff.jl is a fast and compile-able tape-based reverse
mode @acronym{AD, automatic differentiation}, that implements methods to take
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
               (url "https://github.com/yuyichao/FunctionWrappers.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02jilpjr7px6138dx2w7ixricvfgsxqdk84d9dgviranibhnjcxa"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))        ; TODO: Fix test failure
    (home-page "https://github.com/yuyichao/FunctionWrappers.jl")
    (synopsis "Type stable and efficient wrapper of arbitrary functions")
    (description "This package provides type stable and efficient wrapper of arbitrary functions.")
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
     `(("julia-polynomials" ,julia-polynomials)
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
    (propagated-inputs
     `(("julia-intervals" ,julia-intervals)
       ("julia-mutablearithmetics" ,julia-mutablearithmetics)
       ("julia-recipesbase" ,julia-recipesbase)))
    (native-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-specialfunctions" ,julia-specialfunctions)))
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

;; ready to upstream
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
    (propagated-inputs
     `(("julia-compat" ,julia-compat)
       ("julia-requires" ,julia-requires)
       ("julia-timezones" ,julia-timezones)))
    (native-inputs
     `(("julia-compat" ,julia-compat)
       ("julia-timezones" ,julia-timezones)))
    (home-page "https://juliahub.com/docs/Infinity/")
    (synopsis "Representation of infinity in Julia")
    (description "This package provides representations for infinity and
negative infinity in Julia.")
    (license license:gpl3)))

;; TODO: Keep this in sync with tzdata in base.scm
;; Package can use more work
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

;; ready to upstream
(define-public julia-mosaicviews
  (package
    (name "julia-mosaicviews")
    (version "0.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/MosaicViews.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "04fgxghyb7n2ji76xkb1r1fjhzsdbgmp5wsfyyn3yjcsdqbyp8pz"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with ImageCore.jl
    (propagated-inputs
     `(("julia-mappedarrays" ,julia-mappedarrays)
       ("julia-paddedviews" ,julia-paddedviews)
       ("julia-stackviews" ,julia-stackviews)))
    ;(native-inputs
    ; `(("julia-colorvectorspace" ,julia-colorvectorspace)
    ;   ("julia-imagecore" ,julia-imagecore)))
    (home-page "https://github.com/JuliaArrays/MosaicViews.jl")
    (synopsis
     "Lazily view a 3D or 4D array as an expanded 2D array as a matrix of slices")
    (description "When visualizing images, it is not uncommon to provide a 2D
view of different image sources.  For example, comparing multiple images of
different sizes, getting a preview of machine learning dataset.  This package
aims to provide easy-to-use tools for such tasks.")
    (license license:expat)))

;; ready to upstream
(define-public julia-stackviews
  (package
    (name "julia-stackviews")
    (version "0.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/StackViews.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fwiaxdpx1z9dli3jr8kyraych0jbdiny3qklynf0r13px25r6i7"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-doctest
           (lambda _
             (substitute* "test/runtests.jl"
               ((".*doctest.*") ""))
             #t)))))
    (propagated-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)))
    (native-inputs
     `(("julia-aqua" ,julia-aqua)
       ("julia-documenter" ,julia-documenter)))
    (home-page "https://github.com/JuliaArrays/StackViews.jl")
    (synopsis "no more catcat")
    (description "StackViews provides only one array type: @code{StackView}.
There are multiple ways to understand @code{StackView}:
@itemize
@item inverse of @code{eachslice}
@item @code{cat} variant
@item view object
@item lazy version of @code{repeat} special case
@end itemize")
    (license license:expat)))

(define-public julia-referencetests
  (package
    (name "julia-referencetests")
    (version "0.9.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaTesting/ReferenceTests.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0mm6bjhs8a21pippww6b08b5frmnb9m6k8xrszrwq9zhc879zpc9"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; Cycle with ImageCore.jl through ImageMagick.jl.
    (propagated-inputs
     `(
       ;("julia-deepdiffs" ,julia-deepdiffs)
       ("julia-distances" ,julia-distances)
       ("julia-fileio" ,julia-fileio)
       ("julia-imagecore" ,julia-imagecore)
       ;("julia-imageinterminal" ,julia-imageinterminal)
       ))
    ;(native-inputs
    ; `(("julia-csvfiles" ,julia-csvfiles)
    ;   ("julia-dataframes" ,julia-dataframes)
    ;   ("julia-gr" ,julia-gr)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-imagetransformations" ,julia-imagetransformations)
    ;   ("julia-plots" ,julia-plots)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://juliatesting.github.io/ReferenceTests.jl/latest/")
    (synopsis "Utility package for comparing data against reference files")
    (description "@code{ReferenceTests.jl} is a Julia package that adds a couple
of additional macros to your testing toolbox.  In particular, it focuses on
functionality for testing values against reference files, which in turn the
package can help create and update if need be.")
    (license license:expat)))

;; ready to upstream
(define-public julia-imagemetadata
  (package
    (name "julia-imagemetadata")
    (version "0.9.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageMetadata.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0iv154ms370xgcr56bwsjl13iwmy671cbxjl9ld5yfj85pclcwi1"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-axisarrays" ,julia-axisarrays)
       ("julia-imageaxes" ,julia-imageaxes)
       ("julia-imagecore" ,julia-imagecore)
       ("julia-indirectarrays" ,julia-indirectarrays)))
    (native-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-simpletraits" ,julia-simpletraits)
       ("julia-unitful" ,julia-unitful)))
    (home-page "https://github.com/JuliaImages/ImageMetadata.jl")
    (synopsis "Julia package for images having metadata")
    (description "ImageMetadata is a simple package providing utilities for
working with images that have metadata attached.  For example, you might want to
associate an image with the date on which the picture was taken, or an MRI scan
with patient data, or an astronomical image with sky coordinates and information
about the detector used to acquire the image.")
    (license license:expat)))

;; ready to upstream
(define-public julia-imageaxes
  (package
    (name "julia-imageaxes")
    (version "0.6.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageAxes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15zqxani1jjh8849s7rdps6b6prqdwv8yxx893y536vkpk7i07qd"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-axisarrays" ,julia-axisarrays)
       ("julia-imagecore" ,julia-imagecore)
       ("julia-reexport" ,julia-reexport)
       ("julia-simpletraits" ,julia-simpletraits)))
    (native-inputs
     `(("julia-unitful" ,julia-unitful)))
    (home-page "https://github.com/JuliaImages/ImageAxes.jl")
    (synopsis "Julia package for giving \"meaning\" to the axes of an image")
    (description "This small package supports the representation of images as
@code{AxisArrays} to endow the axes with \"meaning,\" and makes programming with
such arrays easy via traits.")
    (license license:expat)))

;; ready to upstream
(define-public julia-axisarrays
  (package
    (name "julia-axisarrays")
    (version "0.4.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/AxisArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "079rj7wvh9ks293g2ih1yah5k0sg8wazw08z3vg2bxj4s16wr64p"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-rangearrays" ,julia-rangearrays)
       ("julia-intervalsets" ,julia-intervalsets)
       ("julia-itertools" ,julia-itertools)))
    (native-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-unitful" ,julia-unitful)))
    (home-page "http://juliaarrays.github.io/AxisArrays.jl/latest/")
    (synopsis "Arrays where each dimension can have a named axis with values")
    (description "This package for the Julia language provides an array type
(the AxisArray) that knows about its dimension names and axis values.  This
allows for indexing by name without incurring any runtime overhead.  This
permits one to implement algorithms that are oblivious to the storage order of
the underlying arrays.  AxisArrays can also be indexed by the values along their
axes, allowing column names or interval selections.")
    (license license:expat)))

;; ready to upstream
(define-public julia-intervalsets
  (package
    (name "julia-intervalsets")
    (version "0.5.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/IntervalSets.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0gsz89cd3iygbl5qr389k9vwpg7w1nk0s90g25nsmk34y9hifxag"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-ellipsisnotation" ,julia-ellipsisnotation)))
    (native-inputs
     `(("julia-offsetarrays" ,julia-offsetarrays)))
    (home-page "https://github.com/JuliaMath/IntervalSets.jl")
    (synopsis "Interval Sets for Julia")
    (description "This package is intended to implement a \"minimal\" foundation
for intervals upon which other packages might build.  In particular, we
encourage type-piracy for the reason that only one interval package can
unambiguously define the @code{..} and @code{±} operators.")
    (license license:expat)))

;; ready to upstream
(define-public julia-ellipsisnotation
  (package
    (name "julia-ellipsisnotation")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ChrisRackauckas/EllipsisNotation.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0py46kxl702r8pw3v7x4cqllf7yc91b0dr7vb60xh2qi7d6y3jc7"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-arrayinterface" ,julia-arrayinterface)))
    (home-page "https://github.com/ChrisRackauckas/EllipsisNotation.jl")
    (synopsis "Elipsis notation implementation")
    (description "This implements the notation @code{..} for indexing arrays.
It's similar to the Python @code{...} in that it means \"all of the columns
before (or after)\".")
    (license license:expat)))

;; ready to upstream
(define-public julia-imagetransformations
  (package
    (name "julia-imagetransformations")
    (version "0.8.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageTransformations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0i8gw68hljshsy9wdl5mrpbb31irhmayqyglsxi7jwm88iy9pxhm"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; cycle with ImageMagick.jl
    (propagated-inputs
     `(("julia-axisalgorithms" ,julia-axisalgorithms)
       ("julia-colorvectorspace" ,julia-colorvectorspace)
       ("julia-coordinatetransformations" ,julia-coordinatetransformations)
       ("julia-imagecore" ,julia-imagecore)
       ("julia-identityranges" ,julia-identityranges)
       ("julia-interpolations" ,julia-interpolations)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-rotations" ,julia-rotations)
       ("julia-staticarrays" ,julia-staticarrays)))
    ;(native-inputs
    ; `(("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-referencetests" ,julia-referencetests)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://github.com/JuliaImages/ImageTransformations.jl")
    (synopsis "Geometric transformations on images for Julia")
    (description "This package provides support for image resizing, image
rotation, and other spatial transformations of arrays.")
    (license license:expat)))

;; ready to upstream
(define-public julia-coordinatetransformations
  (package
    (name "julia-coordinatetransformations")
    (version "0.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGeometry/CoordinateTransformations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15zbkn32v7xlz7559s0r5a0vkwmjwsswxaqpzijly4lky4jnp33d"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-staticarrays" ,julia-staticarrays)))
    (native-inputs
     `(("julia-documenter" ,julia-documenter)
       ("julia-forwarddiff" ,julia-forwarddiff)
       ("julia-unitful" ,julia-unitful)))
    (home-page "https://github.com/JuliaGeometry/CoordinateTransformations.jl")
    (synopsis "A fresh approach to coordinate transformations")
    (description "@code{CoordinateTransformations} is a Julia package to manage
simple or complex networks of coordinate system transformations.
Transformations can be easily applied, inverted, composed, and differentiated
(both with respect to the input coordinates and with respect to transformation
parameters such as rotation angle).  Transformations are designed to be
light-weight and efficient enough for, e.g., real-time graphical applications,
while support for both explicit and automatic differentiation makes it easy to
perform optimization and therefore ideal for computer vision applications such
as SLAM (simultaneous localization and mapping).")
    (license license:expat)))

(define-public julia-testimages
  (package
    (name "julia-testimages")
    (version "1.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/TestImages.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lnfsmx33qspyvxw0cykwh7il8xykjpcw1080sisn95ngz2qhdmy"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; cycle with ImageMagick.jl
    (propagated-inputs
     `(
       ("julia-axisarrays" ,julia-axisarrays)
       ("julia-colortypes" ,julia-colortypes)
       ("julia-fileio" ,julia-fileio)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-stringdistances" ,julia-stringdistances)
       ))
    ;(native-inputs
    ; `(("julia-colors" ,julia-colors)
    ;   ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
    ;   ("julia-imagecontrastadjustment" ,julia-imagecontrastadjustment)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-ometiff" ,julia-ometiff)
    ;   ("julia-referencetests" ,julia-referencetests)))
    (home-page "https://testimages.juliaimages.org/")
    (synopsis "Standard test images for Julia")
    (description "This package provides a convenient Julia interface for loading standard named test images and example images for the internal usage in JuliaImages.  This can be used in conjunction with the Images package.")
    (license license:expat)))

(define-public julia-interpolations
  (package
    (name "julia-interpolations")
    (version "0.13.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Interpolations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "06wh4fc7hy20kq9iipk3w8v50vd09k7vkii43k8z1vw036f6l7x3"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; some of the tests are flakey
    (propagated-inputs
     `(("julia-axisalgorithms" ,julia-axisalgorithms)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-ratios" ,julia-ratios)
       ("julia-staticarrays" ,julia-staticarrays)
       ("julia-woodburymatrices" ,julia-woodburymatrices)))
    (native-inputs
     `(
       ;("julia-offsetarrays" ,julia-offsetarrays)
       ;("julia-unitful" ,julia-unitful)
       ))
    (home-page "https://github.com/JuliaMath/Interpolations.jl")
    (synopsis "Fast, continuous interpolation of discrete datasets")
    (description "This package implements a variety of interpolation schemes for
the Julia language.  It has the goals of ease-of-use, broad algorithmic support,
and exceptional performance.")
    (license license:expat)))

;; ready to upstream
(define-public julia-imageshow
  (package
    (name "julia-imageshow")
    (version "0.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageShow.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1b5fdj6bh6fsg36wnjgkxnwqz1s6n65pm3067a3c1g61ngfm9zqr"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f))    ; cycle with ImageMagick.jl
    (propagated-inputs
     `(("julia-fileio" ,julia-fileio)
       ("julia-imagecore" ,julia-imagecore)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-requires" ,julia-requires)
       ("julia-stackviews" ,julia-stackviews)))
    ;(native-inputs
    ; `(("julia-imagedistances" ,julia-imagedistances)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-suppressor" ,julia-suppressor)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://github.com/JuliaImages/ImageShow.jl")
    (synopsis
     "Inline graphical display of images in Julia graphical environments")
    (description "This package provides three non-exported functions
@code{play}/@code{explore} and @code{gif} to interpret your 3D image or 2D
images as either a video sequence or a gif.")
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
    ;(arguments
    ; `(#:tests? #f))   ; GTK.jl not packaged yet
    (propagated-inputs
     `(
       ;("julia-colortypes" ,julia-colortypes)
       ;("julia-colorvectorspace" ,julia-colorvectorspace)
       ;("julia-fileio" ,julia-fileio)
       ;("julia-imagefiltering" ,julia-imagefiltering)
       ;("julia-imagemagick" ,julia-imagemagick)
       ;("julia-quartzimageio" ,julia-quartzimageio)
       ;("julia-requires" ,julia-requires)
       ))
    (native-inputs
     `(
       ;("julia-gtk" ,julia-gtk)
       ;("julia-plots" ,julia-plots)
       ;("julia-testimages" ,julia-testimages)
       ))
    (home-page "https://github.com/JuliaPlots/VisualRegressionTests.jl")
    (synopsis "Automated integrated regression tests for graphics libraries")
    (description "Easy regression testing for visual packages.  Automated tests compare similarity between a newly generated image and a reference image using the Images package.  While in interactive mode, the tests can optionally pop up a Gtk GUI window showing a side-by-side comparison of the test and reference image, and then optionally overwrite the reference image with the test image.  This allows for straightforward regression testing of image data, even when the \"correct\" images change over time.")
    (license license:expat)))

(define-public julia-geometrybasics
  (package
    (name "julia-geometrybasics")
    (version "0.3.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGeometry/GeometryBasics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wsx287i1hlzkw7ljfc929ssla6b4qn647nsa3j32v2f8gzd86ag"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-earcut
           (lambda _
             (substitute* '("Project.toml"
                            "src/GeometryBasics.jl")
               ((".*EarCut.*") ""))
             #t)))
     ))
    (propagated-inputs
     `(
       ;("julia-earcut-jll" ,julia-earcut-jll)
       ("julia-itertools" ,julia-itertools)
       ("julia-staticarrays" ,julia-staticarrays)
       ("julia-structarrays" ,julia-structarrays)
       ("julia-tables" ,julia-tables)
       ))
    (native-inputs
     `(
       ("julia-offsetarrays" ,julia-offsetarrays)
       ))
    (home-page "https://github.com/JuliaGeometry/GeometryBasics.jl")
    (synopsis "Basic Geometry Types")
    (description "This package aims to offer a standard set of Geometry types, which easily work with metadata, query frameworks on geometries and different memory layouts.  The aim is to create a solid basis for Graphics/Plotting, finite elements analysis, Geo applications, and general geometry manipulations - while offering a julian API, that still allows performant C-interop.")
    (license license:expat)))

;; ready to upstream
(define-public julia-structarrays
  (package
    (name "julia-structarrays")
    (version "0.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/StructArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0i1h3pbjp04dwic786yjnx81ifppgcbdysvgjs00cd9zmpn3xnqw"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-dataapi" ,julia-dataapi)
       ("julia-tables" ,julia-tables)))
    (native-inputs
     `(("julia-documenter" ,julia-documenter)
       ("julia-offsetarrays" ,julia-offsetarrays)
       ("julia-pooledarrays" ,julia-pooledarrays)
       ("julia-typedtables" ,julia-typedtables)
       ("julia-weakrefstrings" ,julia-weakrefstrings)))
    (home-page "https://github.com/JuliaArrays/StructArrays.jl")
    (synopsis "Efficient implementation of struct arrays in Julia")
    (description "This package introduces the type @code{StructArray} which is
an @code{AbstractArray} whose elements are @code{struct} (for example
@code{NamedTuples}, or @code{ComplexF64}, or a custom user defined
@code{struct}).  While a @code{StructArray} iterates @code{structs}, the layout
is column based (meaning each field of the @code{struct} is stored in a separate
@code{Array}).")
    (license license:expat)))

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
     `(("earcut" ,earcut)))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)))
    (home-page "https://github.com/JuliaBinaryWrappers/EarCut_jll.jl")
    (synopsis "")
    (description "")
    (license license:expat))))

(define-public earcut
  (package
    (name "earcut")
    (version "0.12.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mapbox/earcut.hpp")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lfvh7shr82g10z3ydw7rll80nyi8nba41ykkgrghh95gvr6m3k7"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (substitute* "CMakeLists.txt"
              ((".*add_subdirectory.*") ""))
            #t))))
    (build-system cmake-build-system)
    (arguments
     `(;#:tests? #f
       #:configure-flags '("-DEARCUT_BUILD_BENCH=OFF"
                           "-DEARCUT_BUILD_VIZ=OFF"
                           )
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./tests"))
             #t))
         ;; no install target, but no shared library either
         ;(replace 'install
         ;  (lambda* (#:key outputs #:allow-other-keys)
         ;    (let ((out (assoc-ref outputs "out")))
         )
       ))
    (propagated-inputs
     `(
       ))
    (inputs
     `(
       ;("glfw" ,(@ (gnu packages gl) glfw))    ; for VIZ
       ))
    (native-inputs
     `(
       ;("boost" ,(@ (gnu packages boost) boost))   ; not needed for tests?
       ))
    (home-page "https://github.com/mapbox/earcut.hpp")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public julia-recipespipeline
  (package
    (name "julia-recipespipeline")
    (version "0.3.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/RecipesPipeline.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0wa342m2d9k4kihr6g9i0wpbsipp0n11kh9jmlw4pc5msmz4rxr0"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f  ; Cycle with Plots.jl
       ))
    (propagated-inputs
     `(
       ("julia-nanmath" ,julia-nanmath)
       ("julia-plotutils" ,julia-plotutils)
       ("julia-recipesbase" ,julia-recipesbase)
       ))
    (native-inputs
     `(
       ;("julia-distributions" ,julia-distributions)
       ))
    (home-page "http://juliaplots.org/RecipesPipeline.jl/dev/")
    (synopsis "Utilities for processing recipes")
    (description "This package was factored out of Plots.jl to allow any other plotting package to use the recipe pipeline.  In short, the extremely lightweight RecipesBase.jl package can be depended on by any package to define \"recipes\": plot specifications of user-defined types, as well as custom plot types.  RecipePipeline.jl contains the machinery to translate these recipes to full specifications for a plot.")
    (license license:expat)))

;; ready to upstream
(define-public julia-gr
  (package
    (name "julia-gr")
    (version "0.57.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jheinen/GR.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0hwzxwnak3sixm8jlm2zz6578gn713sbbznq49s11h38n0aczjx2"))))
    (build-system julia-build-system)
    (propagated-inputs
     `(("julia-gr-jll" ,julia-gr-jll)))
    (home-page "https://github.com/jheinen/GR.jl")
    (synopsis "Plotting for Julia based on GR")
    (description "This module provides a Julia interface to GR, a framework for
visualisation applications.")
    (license license:expat)))

;; TODO: Unbundle fonts
(define-public gr-framework
  (package
    (name "gr-framework")
    (version "0.57.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sciapp/gr")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05qch57acgj6bs1l634wczj0agj2v0b3j221iyk47zqhbimhk45y"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "3rdparty")
            #t))
        ))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))    ; no test target
    (inputs
     `(("cairo" ,(S "cairo"))
       ("ffmpeg" ,(S "ffmpeg"))
       ("freetype" ,(@ (gnu packages fontutils) freetype))
       ("glfw" ,(S "glfw"))
       ("libjpeg-turbo" ,(@ (gnu packages image) libjpeg-turbo))
       ("libpng" ,(@ (gnu packages image) libpng))
       ("libtiff" ,(S "libtiff"))
       ("libx11" ,(@ (gnu packages xorg) libx11))
       ("libxft" ,(@ (gnu packages xorg) libxft))
       ("libxt" ,(@ (gnu packages xorg) libxt))
       ("pixman" ,(S "pixman"))
       ("qtbase" ,(@ (gnu packages qt) qtbase))
       ("qhull" ,(@ (gnu packages maths) qhull))
       ("zlib" ,zlib)))
    (home-page "https://gr-framework.org/")
    (synopsis "Graphics library for visualisation applications")
    (description "GR is a universal framework for cross-platform visualization applications.  It offers developers a compact, portable and consistent graphics library for their programs.  Applications range from publication quality 2D graphs to the representation of complex 3D scenes.  GR is essentially based on an implementation of a @acronym{GKS, Graphical Kernel System}.  As a self-contained system it can quickly and easily be integrated into existing applications (i.e. using the @code{ctypes} mechanism in Python or @code{ccall} in Julia).")
    (license license:expat)))

;; ready to upstream
(define-public julia-gr-jll
  (package
    (name "julia-gr-jll")
    (version "0.57.2+0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/GR_jll.jl")
               (commit (string-append "GR-v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fqm531s5pm8q2rqz0gmrbj2qsivmc6x04sgn8gzfpz9jrmglbzq"))))
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
                      "generate_wrapper_header(\"GR\", \""
                      (assoc-ref inputs "gr-framework") "\")\n"))))
               ;; There's a Julia file for each platform, override them all
               (find-files "src/wrappers/" "\\.jl$")))))))
    (inputs
     `(("gr-framework" ,gr-framework)))
    (propagated-inputs
     `(("julia-jllwrappers" ,julia-jllwrappers)
       ("julia-bzip2-jll" ,julia-bzip2-jll)
       ("julia-cairo-jll" ,julia-cairo-jll)
       ("julia-ffmpeg-jll" ,julia-ffmpeg-jll)
       ("julia-fontconfig-jll" ,julia-fontconfig-jll)
       ("julia-glfw-jll" ,julia-glfw-jll)
       ("julia-jpegturbo-jll" ,julia-jpegturbo-jll)
       ("julia-libpng-jll" ,julia-libpng-jll)
       ("julia-libtiff-jll" ,julia-libtiff-jll)
       ("julia-pixman-jll" ,julia-pixman-jll)
       ("julia-qt5base-jll" ,julia-qt5base-jll)
       ("julia-zlib-jll" ,julia-zlib-jll)))
    (home-page "https://github.com/JuliaBinaryWrappers/GR_jll.jl")
    (synopsis "GR framework library wrappers")
    (description "This package provides a wrapper for the GR framework.")
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
