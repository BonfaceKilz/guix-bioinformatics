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

(define-public julia-lmgpu
  (let ((commit "e9e95b5fa46f1905ca1ff32a3684a2616a7e482c")
        (revision "1"))
    (package
      (name "julia-lmgpu")
      (version (git-version "0.1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ChelseaTrotter/LMGPU.jl")
                       (commit commit)))
                ;(file-name (git-file-name name version))
                (file-name "LMGPU")
                (sha256
                 (base32
                  "1ddx2np1lakw1l2dclpcaihxd0fcj6bjxsvaxr6g5brxjqk5j7b1"))))
      (build-system julia-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; This is a super ugly hack. Some JULIA environment variable should
           ;; be tuned so it can find the artifact directory.
           (add-after 'unpack 'symlink-zlib-into-artifact-directory
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((julia-dir (string-append (assoc-ref outputs "out")
                                               "/share/julia")))
                 (mkdir-p julia-dir)
                 (symlink
                   (string-append (assoc-ref inputs "julia-zlib-jll")
                                  "/share/julia/artifacts")
                   (string-append julia-dir "/artifacts")))
               #t))
           ;(add-after 'precompile 'check
           ;  (lambda* (#:key outputs #:allow-other-keys)
           ;    (let* ((out (assoc-ref outputs "out"))
           ;           (builddir (string-append out "/share/julia/")))
           ;      (setenv "JULIA_LOAD_PATH"
           ;              (string-append builddir "packages/" ":"
           ;                             (or (getenv "JULIA_LOAD_PATH")
           ;                                 "")))
           ;      (setenv "HOME" (getcwd))
           ;      (invoke "julia" "test/runtests.jl"))))
           )))
      (native-inputs
       `(("r" ,r-minimal)
         ("r-mice" ,r-mice)
         ("r-qtl2" ,r-qtl2)
         ("r-tictoc" ,r-tictoc)
         ("r-tidyverse" ,r-tidyverse)))
      (propagated-inputs
       `(("julia-zipfile" ,julia-zipfile)))
      (home-page "https://github.com/ChelseaTrotter/LMGPU.jl")
      (synopsis "")
      (description "")
      (license license:expat))))

(define-public julia-lmgpu-myapp
  (package
    (inherit julia-lmgpu)
    (name "julia-lmgpu-myapp")
    (source
      (origin (inherit (package-source julia-lmgpu))
              (file-name "MyApp")))
    (arguments
     (substitute-keyword-arguments (package-arguments julia-lmgpu)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'change-directory
             (lambda _
               (chdir "bin/MyApp") #t))))))
    (propagated-inputs
     `(("julia-lmgpu" ,julia-lmgpu)
       ,@(package-propagated-inputs julia-lmgpu)))
    (native-inputs
     `(("julia-packagecompiler" ,julia-packagecompiler)))))

(define-public julia-zipfile
  (package
    (name "julia-zipfile")
    (version "0.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fhs/ZipFile.jl")
               (commit (string-append "v" version))))
        ;(file-name (git-file-name name version))
        (file-name "ZipFile")
        (sha256
         (base32
          "1fpvlhfqg5kgq5vchlf8dyc73r6dzki0dz7plddc3bnr0ld00rlw"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This is a super ugly hack. Some JULIA environment variable should
         ;; be tuned so it can find the artifact directory.
         (add-after 'unpack 'symlink-zlib-into-artifact-directory
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((julia-dir (string-append (assoc-ref outputs "out")
                                             "/share/julia")))
               (mkdir-p julia-dir)
               (symlink
                 (string-append (assoc-ref inputs "julia-zlib-jll")
                                "/share/julia/artifacts")
                 (string-append julia-dir "/artifacts")))
             #t)))))
    (propagated-inputs
     `(("julia-zlib-jll" ,julia-zlib-jll)))
    (home-page "https://github.com/fhs/ZipFile.jl")
    (synopsis "Read/Write ZIP archives in Julia")
    (description "This module provides support for reading and writing ZIP
archives in Julia.")
    (license license:expat)))

(define-public julia-zlib-jll
  (package
    (name "julia-zlib-jll")
    (version "1.2.11+9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/Zlib_jll.jl")
               (commit (string-append "Zlib-v" version))))
        ;(file-name (git-file-name name version))
        (file-name "Zlib_jll")
        (sha256
         (base32
          "0m9n8dp4bwhkyjag1szmhz02k0bxzm4ka2ia2jh8crnd1qi8w9dz"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'symlink-zlib-into-artifact-directory
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((artifacts (string-append (assoc-ref outputs "out")
                                             "/share/julia/artifacts")))
               (mkdir-p artifacts)
               (symlink
                 (assoc-ref inputs "zlib")
                 ;; from git-tree-sha1 in Artifacts.toml
                 (string-append
                   artifacts
                   ,(match (%current-system)
                      ("x86_64-linux" "/7846a2956a213715c2c76632f3461cef87d9d545")
                      ("i686-linux" "/c8456cbd00982236828623bbc63f21b9b7b03821")
                      ("armhf-linux" "/748c38025b5596a5005a87ac2b9476603cf8615b")
                      ("aarch64-linux" "/3dd0c7cd5424c8746a1a32034ba1b10458f20b3b")
                      (_ "/UNSUPPORTED")))))
             #t)))))
    (native-inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/JuliaBinaryWrappers/Zlib_jll.jl")
    (synopsis "Autogenerated package constructed using BinaryBuilder.jl")
    (description "This is an autogenerated package constructed using
@url{https://github.com/JuliaPackaging/BinaryBuilder.jl, BinaryBuilder.jl}.")
    (license license:expat)))

(define-public julia-packagecompiler
  (package
    (name "julia-packagecompiler")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLang/PackageCompiler.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1s9xc17i308fdpyvkz1w6qb1h7yncdr2jgk1szfvygxd6yzkv1b4"))))
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
          "000000000000000000000000000000000parjginwi81jbzr1vgd"))))
    (build-system julia-build-system)
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "PackageCompiler"
       ))
    (propagated-inputs
     `(
       ;("julia-fillarrays" ,julia-fillarrays)
       ;("julia-distributed" ,julia-distributed)
       ;("julia-pdmats" ,julia-pdmats)
       ;("julia-quadgk" ,julia-quadgk)
       ;("julia-specialfunctions" ,julia-specialfunctions)
       ;("julia-statsbase" ,julia-statsbase)
       ;("julia-statsfuns" ,julia-statsfuns)
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
     `(;#:tests? #f  ; LoadError: UndefVarError: iocapture not defined
       ))
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

(define-public julia-docstringextensions
  (package
    (name "julia-docstringextensions")
    (version "0.8.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/DocStringExtensions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1fazv87f0j6hw03frx0gqgq9qpjbddqgccm9998a3329wrrs6gwd"))))
    (build-system julia-build-system)
    (home-page "https://juliadocs.github.io/DocStringExtensions.jl/latest")
    (synopsis "Extensions for Julia's docsystem")
    (description "This package provides extensions for Julia's docsystem.")
    (license license:expat)))

(define-public julia-iocapture
  (package
    (name "julia-iocapture")
    (version "0.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/IOCapture.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ajlfh8f1g23bx5f8h70nrgr0zfwxaqnpxlka8l4qhjmnfqxl43a"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaDocs/IOCapture.jl")
    (synopsis "Capture standard output and error streams")
    (description "Provides the @code{IOCapture.capture(f)} function, captures
the standard output and standard error, and returns it as a string, together
with the return value.")
    (license license:expat)))

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
    (arguments
     `(;#:tests? #f
       ;#:julia-package-name "PackageCompiler"
       ))
    (propagated-inputs
     `(
       ("julia-dataapi" ,julia-dataapi)
       ("julia-datastructures" ,julia-datastructures)
       ;("julia-missings" ,julia-missings)
       ("julia-sortingalgorithms" ,julia-sortingalgorithms)
       ;("julia-statsapi" ,julia-statsapi)
       ))
    (home-page "https://github.com/JuliaStats/StatsBase.jl")
    (synopsis "Basic statistics for Julia")
    (description "StatsBase.jl is a Julia package that provides basic support for statistics. Particularly, it implements a variety of statistics-related functions, such as scalar statistics, high-order moment computation, counting, ranking, covariances, sampling, and empirical density estimation.")
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
    (version "0.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaCollections/SortingAlgorithms.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1nz96sccgl6h6aknck59gmy1yrzx356kk9z68svj2g6yialprv1j"))))
    (build-system julia-build-system)
    (arguments
     `(#:tests? #f
       #:julia-package-name "SortingAlgorithms"
       ))
    (propagated-inputs
     `(
       ("julia-datastructures" ,julia-datastructures)
       ))
    (native-inputs
     `(
       ;("julia-statsbase" ,julia-statsbase)
       ))
    (home-page "https://github.com/JuliaCollections/SortingAlgorithms.jl")
    (synopsis "extra sorting algorithms extending Julia's sorting API")
    (description "The SortingAlgorithms package provides three sorting algorithms that can be used with Julia's standard sorting API: heapsort, timsort and radixsort.")
    (license license:expat)))
