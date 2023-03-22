(define-module (gn packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module ((gnu packages python-xyz) #:hide (python2-six))
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages time)
  #:use-module (past packages python27)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (srfi srfi-1))

(define-public python-rpy2-2.9
  (package
    (inherit python-rpy2)
    (name "python-rpy2")
    (version "2.9.6b0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rpy2" version))
       (sha256
        (base32
         "1vqgw42a51l162gyg8qbx1xakxbj29riyb6azzv89f260w865k0d"))))
        (arguments
         `(#:tests? #f))))

(define-public python2-rpy2
  (package
    (name "python2-rpy2")
    (version "2.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rpy2" version))
       (sha256
        (base32
         "0nhan2qvrw7b7gg5zddwa22kybdv3x1g26vkd7q8lvnkgzrs4dga"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; apparently incompatible with Python 3
       #:tests? #f))
    (propagated-inputs
     `(("python2-six" ,python2-six)
       ("python2-singledispatch" ,python2-singledispatch)))
    (inputs
     `(("readline" ,readline)
       ("icu4c" ,icu4c)
       ("pcre" ,pcre)
       ("r-minimal" ,r-minimal)
       ("r-survival" ,r-survival)))
    (native-inputs
     `(("zlib" ,zlib)))
    (home-page "http://rpy.sourceforge.net/")
    (synopsis "Python interface to the R language")
    (description "rpy2 is a redesign and rewrite of rpy.  It is providing a
low-level interface to R from Python, a proposed high-level interface,
including wrappers to graphical libraries, as well as R-like structures and
functions.")
    (license license:gpl3+)))

(define-public python-subprocess32 ; guix candidate
  (package
    (name "python-subprocess32")
    (version "3.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "subprocess32" version))
        (sha256
         (base32
          "1hr5fan8i719hmlmz73hf8rhq74014w07d8ryg7krvvf6692kj3b"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;; No tests.
    (home-page "https://pypi.python.org/pypi/subprocess32")
    (synopsis
      "Python subprocess32.")
    (description
      "Python subprocess32.")
    (license license:gpl2))
)

(define-public python-inotify ; guix candidate
(package
  (name "python-inotify")
  (version "0.2.9")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "inotify" version))
      (sha256
        (base32
          "043sbm9q8ca4fhn19knwpsxgzfgm5ik75m0bl7dp9hjy6p3v3zzn"))))
  (build-system python-build-system)
  (propagated-inputs
      `(("inotify-tools" ,inotify-tools)))
  (arguments `(#:tests? #f)) ;; No tests.
  (home-page "https://pypi.python.org/pypi/inotify")
  (synopsis
    "Python inotify.")
  (description
    "Python inotify.")
  (license license:gpl2))
)

(define-public python2-inotify
  (package-with-python2 python-inotify))

(define-public python2-flask
  (let ((base (package-with-python2 python-flask)))
    (package
      (inherit base)
      (arguments
       `(#:tests? #f)))))

(define-public python2-werkzeug
  (let ((base (package-with-python2 python-werkzeug)))
    (package
      (inherit base)
      (arguments
       `(#:tests? #f)))))

(define-public python2-flask-sqlalchemy
  (package-with-python2 python-flask-sqlalchemy))

(define-public python2-xlsxwriter
  (package-with-python2 python-xlsxwriter))

(define-public python-rserve
  (package
   (name "python-rserve")
   (version "0.91")
   (source
    (origin
     (method url-fetch)
                                        ; https://pypi.python.org/packages/27/e8/b23e0e3d38dadc721947bc9f4b6f1b3e5e1e6c26ac67d8ad88376c5555a0/pyRserve-0.9.1.tar.gz#md5=6da1978f908dd7bdc2d22ee5c29238c0
     (uri (string-append
           "https://pypi.python.org/packages/27/e8/b23e0e3d38dadc721947bc9f4b6f1b3e5e1e6c26ac67d8ad88376c5555a0/pyRserve-0.9.1.tar.gz"))
     (sha256
      (base32
       "162dg7d0ni035b75qskrjdzd1yyxwnvybcv115aiapcvyfw2vbsm"))))
   (build-system python-build-system)
   (propagated-inputs
    `(
      ("python-numpy" ,python-numpy)
      ))
   (inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-pytest" ,python-pytest)
      ))
   (home-page "https://pypi.python.org/pypi/pyRserve")
   (synopsis
    "RServe.")
   (description
    ".")
   (license license:expat)))

(define-public python2-rserve
  (package-with-python2 python-rserve))

(define-public python-setuptools-old
  (package
    (name "python-setuptools-old")
    (version "40.6.0")
    (source
     (origin
      (method url-fetch)
      (uri "https://files.pythonhosted.org/packages/37/1b/b25507861991beeade31473868463dad0e58b1978c209de27384ae541b0b/setuptools-40.6.3.zip"
             )
      (sha256
       (base32
        "1y085dnk574sxw9aymdng9gijvrsbw86hsv9hqnhv7y4d6nlsirv"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Remove included binaries which are used to build self-extracting
          ;; installers for Windows.
          ;; TODO: Find some way to build them ourself so we can include them.
          (for-each delete-file (find-files "setuptools" "^(cli|gui).*\\.exe$"))
          #t))))
    (build-system python-build-system)
    ;; FIXME: Tests require pytest, which itself relies on setuptools.
    ;; One could bootstrap with an internal untested setuptools.
    (arguments
     `(#:tests? #f))
    (home-page "https://pypi.python.org/pypi/setuptools")
    (synopsis
     "Library designed to facilitate packaging Python projects")
    (description
     "Setuptools is a fully-featured, stable library designed to facilitate
packaging Python projects, where packaging includes:
Python package and module definitions,
distribution package metadata,
test hooks,
project installation,
platform-specific details,
Python 3 support.")
    ;; TODO: setuptools now bundles the following libraries:
    ;; packaging, pyparsing, six and appdirs. How to unbundle?
    (license (list license:psfl        ; setuptools itself
                   license:expat       ; six, appdirs, pyparsing
                   license:asl2.0      ; packaging is dual ASL2/BSD-2
                   license:bsd-2))))


(define-public python-avro ; guix ready - used by CWL
(package
  (name "python-avro")
  (version "1.8.2")
  (source
    (origin
      (method url-fetch)
        (uri (pypi-uri "avro" version))
      (sha256
        (base32
          "0nabn1hzj1880qsp7fkg7923c0xdqk4i35s15asmy2xp604f97lg"))))
  (build-system python-build-system)
  (inputs
    `(("python-setuptools" ,python-setuptools)))
  (home-page "http://hadoop.apache.org/avro")
  (synopsis
    "Avro is a serialization and RPC framework.")
  (description
    "Avro is a serialization and RPC framework.")
  (license #f)))

(define-public python2-avro
  (package-with-python2 python-avro))


(define-public python2-htmlgen-gn ; guix obsolete
  (package
    (name "python2-htmlgen-gn")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              ;; http://files.genenetwork.org/software/contrib/htmlgen-2.2.2-gn.tar.gz
              (uri (string-append
                     "https://files.genenetwork.org/software/htmlgen-2.2.2-gn.tar.gz"))
              (sha256
               (base32
                "1lwsk56rymhrma46cbyh3g64ksmq1vsih3qkrc2vh0lpba825y7r"))
              ;;(patches (list
              ;;          (search-patch "python2-htmlgen-Applied-Deb-patch.patch")
              ;;          (search-patch "python2-htmlgen-Fix-test-for-random.patch")
              ))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'modernize-imports
           (lambda _
             (substitute* "HTMLgen.py"
               (("whrandom") "random"))
             (substitute* "HTMLcalendar.py"
               (("import regex") "import re as regex"))
             (substitute* "HTMLutil.py"
               (("import string, regex") "import re as regex\nimport string"))
             (substitute* "HTMLtest.py"
               (("import string, regex, regsub") "import re as regex\nimport string")
               (("regsub.split") "re.split"))
             #t))
         (replace 'build
           (lambda _
             (invoke "python" "-m" "compileall" ".")))
         (replace 'check
           (lambda* (#:key (tests? '()) #:allow-other-keys)
             (if tests?
               (invoke "python" "HTMLtest.py")
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/python2.7/site-packages/htmlgen/")))
               ;; Install libs and headers.
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "." "\\.py[c]?$"))
               #t))))))
    (home-page "https://packages.debian.org/unstable/python/python-htmlgen")
    (synopsis "Genenetwork version of Python2 HTMLgen (defunkt project)")
    (description #f)
    (license #f)))

(define-public python2-htmlgen-2.2
  (package
    (name "python2-htmlgen-2.2")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/h/htmlgen/htmlgen_" version ".orig.tar.gz"))
              (sha256
               (base32
                 "186kb434q6z84g31ysvzi4kcfcvl3pmm57k4jlng4ccgls94x6wb"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'modernize-imports
           (lambda _
             (substitute* "HTMLgen.py"
               (("whrandom") "random"))
             (substitute* "HTMLcalendar.py"
               (("import regex") "import re as regex"))
             (substitute* "HTMLutil.py"
               (("import string, regex") "import re as regex\nimport string"))
             (substitute* "HTMLtest.py"
               (("import string, regex, regsub") "import re as regex\nimport string")
               (("regsub.split") "re.split"))
             #t))
         (replace 'build
           (lambda _
             (invoke "python" "-m" "compileall" ".")))
         (replace 'check
           (lambda _
             (invoke "python" "HTMLtest.py")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/python2.7/site-packages/htmlgen/")))
               ;; Install libs and headers.
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "." "\\.py[c]?$"))
               #t))))))
    (home-page "https://packages.debian.org/unstable/python/python-htmlgen")
    (synopsis "Genenetwork version of Python2 HTMLgen (defunkt project)")
    (description #f)
    (license #f)))

(define-public python2-pil1-gn ; guix obsolete
  (package
    (name "python2-pil1") ; works with GN2
    (version "1.1.6")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   ; "http://files.genenetwork.org/software/contrib/Imaging-"
                   "http://effbot.org/downloads/Imaging-1.1.6.tar.gz"))
             (sha256
              (base32
               "141zidl3s9v4vfi3nsbg42iq1lc2a932gprqr1kij5hrnn53bmvx"))
    (modules '((guix build utils)))
    (snippet
     ;; Adapt to newer freetype. As the package is unmaintained upstream,
     ;; there is no use in creating a patch and reporting it.
     '(substitute* "_imagingft.c"
                   (("freetype/")
                    "freetype2/freetype/")))))
    (build-system python-build-system)
    (inputs
      `(("freetype" ,freetype)
        ("libjpeg" ,libjpeg-turbo)
        ("libtiff" ,libtiff)
        ("python2-setuptools" ,python2-setuptools)
        ("zlib" ,zlib)))
    (arguments
     ;; Only the fork python-pillow works with Python 3.
     `(#:python ,python-2
       #:tests? #f ; no check target
       #:phases
         (alist-cons-before
          'build 'configure
          ;; According to README and setup.py, manual configuration is
          ;; the preferred way of "searching" for inputs.
          ;; lcms is not found, TCL_ROOT refers to the unavailable tkinter.
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((jpeg (assoc-ref inputs "libjpeg"))
                  (zlib (assoc-ref inputs "zlib"))
                  (tiff (assoc-ref inputs "libtiff"))
                  (freetype (assoc-ref inputs "freetype")))
              (substitute* "setup.py"
                (("JPEG_ROOT = None")
                 (string-append "JPEG_ROOT = libinclude(\"" jpeg "\")"))
                (("ZLIB_ROOT = None")
                 (string-append "ZLIB_ROOT = libinclude(\"" zlib "\")"))
                (("TIFF_ROOT = None")
                 (string-append "TIFF_ROOT = libinclude(\"" tiff "\")"))
                (("FREETYPE_ROOT = None")
                 (string-append "FREETYPE_ROOT = libinclude(\""
                                freetype "\")")))))
          %standard-phases)))
    (home-page "http://www.pythonware.com/products/pil/")
    (synopsis "Python Imaging Library")
    (description "The Python Imaging Library (PIL) adds image processing
capabilities to the Python interpreter.")
    (license (license:x11-style
               "file://README"
               "See 'README' in the distribution."))))


(define-public python2-piddle-gn ; guix obsolete
  (package
    (name "python2-piddle-gn")
    (version "1.0.15-gn")
    (source (origin
     (method url-fetch)
     (uri (string-append
           "https://files.genenetwork.org/software/piddle-1.0.15-gn.tgz"))
     (sha256
      (base32
       "05gjnn31v7p0kh58qixrpcizcxqf3b7zv4a5kk8nsmqwgxh0c6gq"))))

    (build-system python-build-system)
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (propagated-inputs
     `(("python2-pil1" ,python2-pil1-gn)))
    (arguments
     `(
       #:python ,python-2
       #:tests? #f   ; no 'setup.py test' really!
    ))
    (home-page #f)
    (synopsis "Canvas drawing library for python2 (old!)")
    (description #f)
    (license #f)))


(define-public python2-pil1 ; guix obsolete
  (package
    (name "python2-pil1") ; this version is NOT used by genenetwork
    (version "1.1.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://effbot.org/downloads/Imaging-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "04aj80jhfbmxqzvmq40zfi4z3cw6vi01m3wkk6diz3lc971cfnw9"))
    (modules '((guix build utils)))
    (snippet
     ;; Adapt to newer freetype and lcms.  As the package is unmaintained
     ;; upstream, there is no use in creating a patch and reporting it.
     '(begin (substitute* "_imagingft.c"
               (("freetype/")
                "freetype2/freetype/"))
             (substitute* '("setup.py"
                            "_imagingcms.c")
               (("lcms.h") "lcms2.h"))))))
    (build-system python-build-system)
    (inputs
     `(("freetype" ,freetype)
       ("lcms" ,lcms) ; not fully supported
       ("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("tcl" ,tcl)
       ("zlib" ,zlib)))
    (arguments
     ;; Only the fork python-pillow works with Python 3.
     `(#:python ,python-2
       #:tests? #f ; no check target
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((freetype (assoc-ref inputs "freetype"))
                   (jpeg (assoc-ref inputs "libjpeg"))
                   (lcms (assoc-ref inputs "lcms"))
                   (tcl (assoc-ref inputs "tcl"))
                   (tiff (assoc-ref inputs "libtiff"))
                   (zlib (assoc-ref inputs "zlib")))
               (substitute* "setup.py"
                 (("FREETYPE_ROOT .*")
                  (string-append "FREETYPE_ROOT = libinclude(\"" freetype "\")\n"))
                 (("JPEG_ROOT .*")
                  (string-append "JPEG_ROOT = libinclude(\"" jpeg "\")\n"))
                 (("LCMS_ROOT .*")
                  (string-append "LCMS_ROOT = libinclude(\"" lcms "\")\n"))
                 (("^TCL_ROOT .*")
                  (string-append "TCL_ROOT = libinclude(\"" tcl "\")\n"))
                 (("TIFF_ROOT .*")
                  (string-append "TIFF_ROOT = libinclude(\"" tiff "\")\n"))
                 (("ZLIB_ROOT .*")
                  (string-append "ZLIB_ROOT = libinclude(\"" zlib "\")\n"))))
             #t)))))
    (home-page "http://www.pythonware.com/products/pil/")
    (synopsis "Python Imaging Library")
    (description "The @dfn{Python Imaging Library} (PIL) adds image processing
capabilities to your Python interpreter.  This library supports many file
formats, and provides powerful image processing and graphics capabilities.

NOTE: This package is superseded by python-pillow")
    (license (license:x11-style
               "file://README"
               "See 'README' in the distribution."))))

; upstream version won't work with gn2
(define-public python2-piddle
  (package
    (name "python2-piddle")
    (version "1.0.15")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/piddle/piddle/"
                            version "/piddle-" version ".zip"))
        (sha256
         (base32
          "0jaxfsrcgqb5cf2wznxnpdws5khlrdixmg85lrhq2zl9cy6dfdya"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:use-setuptools? #f
       #:tests? #f)) ; tests are interactive
    (native-inputs
     `(("unzip" ,unzip)))
    (propagated-inputs
     `(("python2-pil" ,python2-pil1-gn)))
    (home-page "http://www.strout.net/info/coding/python/piddle/")
    (synopsis "Plug-In Drawing, Does Little Else")
    (description "PIDDLE is designed for vector graphics -- i.e., drawing of
primitives such as lines and ellipses, rather than manipulation of individual
pixels.")
    (license license:gpl2+)))

(define-public python2-numarray ; guix: obsolete lib
  (package
    (name "python2-numarray")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/numpy/numarray-" version ".tar.gz"
             ))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x1i4j7yni7k4p9kjxs1lgln1psdmyrz65wp2yr35yn292iw2vbg"))))
    (build-system python-build-system)
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "python" "setup.py" "build"))))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; Build and install the Python bindings.  The underlying
                    ;; C++ library is apparently not meant to be installed.
                    (let ((out (assoc-ref outputs "out")))
                      (system* "python" "setup.py" "install"
                               (string-append "--prefix=" out))))))
       #:tests? #f))   ; no 'setup.py test' really!
    (home-page "http://www.numpy.org/")
    (synopsis "Numerical library array processing of numbers, strings, records and objects")
    (description
     "Numarray is an (OBSOLETE) array processing package designed to
efficiently manipulate large multi-dimensional arrays. Numarray is
modelled after Numeric and features c-code generated from python
template scripts, the capacity to operate directly on arrays in files,
and improved type promotions. Numarray provides support for
manipulating arrays consisting of numbers, strings, records, or
objects using the same basic infrastructure and syntax.  Numarray is
now part of the numpy package, though some legacy software still uses
the older versions.")
    (license license:gpl2))) ; actualy PyRAF http://www.stsci.edu/resources/software_hardware/pyraf/LICENSE

(define-public python-htmlgen
  (package
    (name "python-htmlgen")
    (version "2.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/srittau/python-htmlgen")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "11hfx5x3jg4hyfxzav6ypsb57mahb5nk6qzg4zn1pyy1lilllqj6"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-asserts" ,python-asserts)))
    (synopsis "Python HTML 5 Generator")
    (description "This is a python library for generating html from classes.")
    (home-page "https://github.com/srittau/python-htmlgen")
    (license license:expat)))

(define-public python2-htmlgen
  (package-with-python2 python-htmlgen))

(define-public python-asserts
  (package
    (name "python-asserts")
    (version "0.10.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/srittau/python-asserts")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "10lzdbhyl1s1fpq34prhi288wcigrk0z4hphql20pyjxx6yla8ya"))))
    (build-system python-build-system)
    (synopsis "Stand-alone Assertions for Python")
    (description "This is a python library that can provide assertions in a
stand-alone manner.")
    (home-page "https://github.com/srittau/python-asserts")
    (license license:expat)))

(define-public python-version
(let ((commit "e5aadc720bb74c535f29e5a2de5cd9697efe8d7c"))
(package
  (name "python-version")
  (version "0.1.2")
  (source
    (origin
      (method git-fetch)
      (uri (git-reference
      ; (url "https://github.com/genenetwork/pylmm.git")
        (url "https://github.com/keleshev/version.git") ; version not in pypi
        (commit commit)))
      (file-name (string-append name "-" commit))
      (sha256
        (base32
          "1rc8kf72v180qlygkh1y0jwv2fxqpx7n97bqfhbwgnn31iwai9g3"))))
  (build-system python-build-system)
  (propagated-inputs
    `(
    ("python-more-itertools" ,python-more-itertools)
    ("python-pytest" ,python-pytest)))
  (home-page "http://github.com/halst/version")
  (synopsis "Implementation of semantic version")
  (description
    "Implementation of semantic version")
  (license license:expat)
)))

(define-public python2-pp
  (package
    (name "python2-pp")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
             "http://www.parallelpython.com/downloads/pp/pp-" version ".zip"))
        (sha256
         (base32
          "0qkxcyclz3vgwpl6xvsrg76q59dj0wwy8qx15567bafv659ypyb1"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:use-setuptools? #f
       #:tests? #f)) ; no tests
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.parallelpython.com")
    (synopsis "Parallel and distributed programming for Python")
    (description "PP is a python module which provides mechanism for parallel
execution of python code on SMP (systems with multiple processors or cores) and
clusters (computers connected via network).")
    (license license:bsd-3)))

(define GN1-thirdparty-sources
  (origin
    (method url-fetch/tarbomb)
    (uri "https://files.genenetwork.org/software/thirdparty.tgz")
    (file-name "GN1-thirdparty")
    (sha256
     (base32
      "0nnp6g412hjfrcn3k2yrfb14sxv06k0149whc7qmv678nyj5zhfa"))))

(define-public python2-json-GN1
  (package
    (name "python2-json-GN1")
    (version "GN1")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.7/site-packages/json/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/json" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public python2-pyx-GN1
  (package
    (name "python2-pyx-GN1")
    (version "0.8")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.7/site-packages/pyx/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/pyx" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl2+)))

(define-public python2-pyxlwriter
  (package
    (name "python2-pyxlwriter")
    (version "0.4a3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/pyxlwriter/pyxlwriter/"
                            version "/pyXLWriter-" version ".zip"))
        (sha256
         (base32
          "1kfsi6la9y53rwayszgayfmkjfknpp650v69a0hwd1fcfk1df735"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:use-setuptools? #f
       #:tests? #f)) ; no tests
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://sourceforge.net/projects/pyxlwriter/")
    (synopsis "Python library for generating Excel compatible spreadsheets")
    (description "PyXLWriter is a Python library for generating Excel compatible
spreadsheets.  It's a port of John McNamara's Perl @code{Spreadsheet::WriteExcel}
module version 1.01 to Python.  It allows writing of Excel compatible
spreadsheets without the need for COM objects.")
    (license license:lgpl2.1+)))

(define-public python2-svg-GN1
  (package
    (name "python2-svg-GN1")
    (version "1.0")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.7/site-packages/svg/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/svg" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-4)))

(define-public python-admiral
  (package
    (name "python-admiral")
    (version "0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "admiral" version))
        (sha256
         (base32
          "1b2zjgyz94ld5wr7s4cm4x5sxijx3w0dmd7r2cq1s8iqjzz6rd1x"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))      ; No tests
    (propagated-inputs
     `(("python-humanfriendly" ,python-humanfriendly)))
    (home-page "https://github.com/nspies/admiral")
    (synopsis
     "Simple python high-performance computing cluster batch submission")
    (description
     "Simple python high-performance computing cluster batch submission.")
    (license #f)))      ; No license in repository.

(define-public python2-admiral
  (package-with-python2 python-admiral))

(define-public python-pbr-1.6.0
  (package
    (inherit python-pbr)
    (name "python-pbr")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pbr" version))
        (sha256
         (base32
          "1lg1klrczvzfan89y3bl9ykrknl3nb01vvai37fkww24apzyibjf"))))))

(define-public python-arvados-python-client
  (package
    (name "python-arvados-python-client")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "arvados-python-client" version))
        (sha256
         (base32 "1j08aykj0v2z2bqwr5nfnbjgc1yzdnfdafcnxbf2jbwqh8kx7zc9"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (substitute* "setup.py"
              ;; Don't set a maximum version of pycurl.
              (("(pycurl >=([[:digit:]]+\\.?)+),.*" _ pycurl)
               (string-append pycurl "',\n")))))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))    ; tests not included?
    (propagated-inputs
     (list python-ciso8601
           python-future
           python-google-api-client
           python-google-api-core-1
           python-google-auth-1
           python-httplib2
           python-protobuf
           python-pycurl
           python-pyparsing-2.4.7       ; < 3
           python-ruamel.yaml
           python-ws4py))
    (native-inputs
     (list python-mock
           python-pbr-1.6.0
           python-pyyaml))
    (home-page "https://arvados.org")
    (synopsis "Arvados client library")
    (description "This package provides the arvados module, an API client for
Arvados.  It also includes higher-level functions to help you write Crunch
scripts, and command-line tools to store and retrieve data in the Keep storage
server.")
    (license license:asl2.0)))

(define-public python-pyshex
  (package
    (name "python-pyshex")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "PyShEx" version))
        (sha256
         (base32 "1l3hprspx5l4zl28p1m79mwfxy5c86601jq3x3damyi7zr2lsp1w"))))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (setenv "SKIP_EXTERNAL_URLS" "true")
               (invoke "python" "-m" "unittest" "-k"
                       "test_sparql_options")))))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cfgraph
           python-chardet
           python-pyshexc
           python-rdflib-shim
           python-requests
           python-shexjsg
           python-sparqlslurper
           python-sparqlwrapper
           python-urllib3))
    (native-inputs
     (list python-pbr python-unittest2))
    (home-page "https://github.com/hsolbrig/PyShEx")
    (synopsis "Python ShEx Implementation")
    (description "This package provides a python ShEx Implementation.")
    (license license:asl2.0)))

(define-public python-pyshexc
  (package
    (name "python-pyshexc")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "PyShExC" version))
        (sha256
         (base32 "1lq4lf0nal1v1d3vbyrr1hfkhmhphy06dyqhyw7b5zls9dfrga9m"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f  ; It isn't clear how the tests expect to succeed.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "unittest" "discover" "-s" "tests")))))))
    (propagated-inputs
     (list python-antlr4-python3-runtime
           python-chardet
           python-jsonasobj
           python-pyjsg
           python-rdflib-shim
           python-shexjsg))
    (native-inputs
     (list python-pbr
           python-requests
           python-yadict-compare))
    (home-page "https://github.com/shexSpec/grammar/tree/master/parsers/python")
    (synopsis "Python ShExC Parser")
    (description "This package converts the @dfn{Shape Expression Compact}
(ShExC) into @dfn{Python JSON Schema Binding} (pyjsg) objects.")
    (license license:asl2.0)))

(define-public python-antlr4-python3-runtime
  (package
    (name "python-antlr4-python3-runtime")
    (version "4.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "antlr4-python3-runtime" version))
        (sha256
         (base32 "06w8fz73rk8vzjz9rydfk56g4mbqpyl81yhypc14jab886dlc97j"))))
    (build-system python-build-system)
    (home-page "https://www.antlr.org/")
    (synopsis "ANTLR runtime for Python")
    (description "This package provides the ANTLR runtime for Python.")
    (license license:bsd-3)))

(define-public python-jsonasobj
  (package
    (name "python-jsonasobj")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jsonasobj" version))
        (sha256
         (base32
          "1yj8y3k3fb7lk043f1zhmhb2lzjlfpnxajb92rpxmjzja93yxx0y"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f   ; Tests not included in release tarball.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "setup.py"
               (("yadict_compare") "['dict_compare"))
             #t)))))
    (native-inputs
     `(("python-yadict-compare" ,python-yadict-compare)))
    (home-page "https://github.com/hsolbrig/jsonasobj")
    (synopsis "JSON as python objects")
    (description
     "This package provides an extension to the core python json library that
treats name/value pairs as first class attributes whenever possible.")
    (license license:asl2.0)))

(define-public python-shexjsg
  (package
    (name "python-shexjsg")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ShExJSG" version))
        (sha256
         (base32 "1dnhpk6n6vzadkv13y7r6y2mi1pzv4y19vmxh91k9ykpqngn4ypi"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (if tests?
               (begin (add-installed-pythonpath inputs outputs)
                      ;; Tries to download files from the internet.
                      (substitute* "tests/test_shexj.py"
                        (("skipIf\\(False") "skipIf(True"))
                      (invoke "python" "-m" "unittest"))))))))
    (propagated-inputs
     (list python-pyjsg))
    (native-inputs
     (list python-jsonasobj
           python-pbr
           python-rdflib-shim
           python-requests
           python-yadict-compare))
    (home-page "https://github.com/hsolbrig/ShExJSG")
    (synopsis "Astract Syntax Tree for the ShEx 2.0 language")
    (description "This package provides an astract syntax tree for the
ShEx 2.0 language.")
    (license license:cc0)))

(define-public python-yadict-compare
  (package
    (name "python-yadict-compare")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "yadict_compare" version))
        (sha256
         (base32
          "1kkcw82cp6mf3jailckd9gya4r7wjyz4gc5azsj2njj4wqn081rw"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (with-directory-excursion "test"
               (invoke "python" "-m" "unittest" "discover" "-s" "test")))))))
    (native-inputs
     `(("python-unittest2" ,python-unittest2)))
    (home-page "https://github.com/hsolbrig/dict_compare")
    (synopsis "Dictionary comparison tool with filtering and reporting")
    (description
     "A dictionary comparison tool that allows the injection of filters and
handles recursion and lists.")
    (license license:bsd-3)))

(define-public python-pyjsg
  (package
    (name "python-pyjsg")
    (version "0.11.10")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "PyJSG" version))
        (sha256
         (base32 "1ylbx2pc06qsvb8cqnr8nysvmw55f8nkm05ybcwjpyik53zy7mjb"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; From tox.ini
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (for-each
                 (lambda (dir)
                   (invoke "python" "-m" "unittest" "discover" "-s" dir))
                 '("tests/test_issues"
                   ;"tests/test_basics"
                   "tests/test_jsglib"
                   "tests/test_parser_impl"
                   "tests/test_python_generator"
                   "tests_standalone"
                   "tests_standalone_2"))))))))
    (propagated-inputs
     (list python-antlr4-python3-runtime
           python-jsonasobj))
    (native-inputs
     (list python-pbr
           python-requests
           python-unittest2
           python-yadict-compare))
    (home-page "https://github.com/hsolbrig/pyjsg")
    (synopsis "Python JSON Schema Grammar bindings")
    (description
     "A tool to create Python classes that represent JSON objects defined in JSG.")
    (license license:asl2.0)))

(define-public python-sparql-slurper
  (package
    (name "python-sparql-slurper")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sparql_slurper" version))
        (sha256
         (base32
          "1m9jlck7ny5dyr762l4xrsn7ll4v48fccjkm062ihhvhbsjf0iil"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))  ; Tests require network access.
    (propagated-inputs
     `(("python-pbr" ,python-pbr)
       ("python-rdflib" ,python-rdflib)
       ("python-sparqlwrapper" ,python-sparqlwrapper)))
    (home-page "https://github.com/hsolbrig/sparql_slurper")
    (synopsis "SPARQL Slurper for rdflib")
    (description "This package provides a SPARQL Slurper for rdflib.")
    (license license:asl2.0)))

(define-public python-sparqlslurper
  (package
    (name "python-sparqlslurper")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sparqlslurper" version))
              (sha256
               (base32 "0ipma74dr5jvsxwaa9al147mn9vv3v5r9lb9hajm4qgwcjqfp0lj"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f  ; Tests try to use the internet.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "unittest")))))))
    (propagated-inputs
     (list python-rdflib-shim
           python-sparqlwrapper))
    (native-inputs
     (list python-pbr python-unittest2))
    (home-page "http://github.com/hsolbrig/sparqlslurper")
    (synopsis "SPARQL Slurper for rdflib")
    (description "This package provides an implementation of a
@code{rdflibGraph} that acts as a cache for a SPARQL endpoint.  SPARQL Slurper
translates the @code{Graph.triples()} function into the corresponding SPARQL
query and resolves it against an endpoint.")
    (license license:cc0)))

(define-public python2-ruamel.ordereddict
  (package
    (name "python2-ruamel.ordereddict")
    (version "0.4.14")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ruamel.ordereddict" version))
        (sha256
         (base32
          "0jldrcanmz0284r2y6kbi82vslkaf5hf5q90v7rqxcdjdv952418"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (if tests?
               (begin (add-installed-pythonpath inputs outputs)
                      (invoke "python" "test/testordereddict.py"))
               #t))))))
    (home-page "https://sourceforge.net/projects/ruamel-ordereddict/")
    (synopsis "Version of dict that keeps keys in insertion resp. sorted order")
    (description
     "This package provides a version of dict that keeps keys in insertion resp.
sorted order.")
    (license license:expat)))

(define-public python-pytest-5
  (package
    (inherit python-pytest)
    (name "python-pytest")
    (version "5.4.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest" version))
              (sha256
               (base32
                "1n67lk8iwlsmfdm8663k8l7isllg1xd3n9p1yla7885szhdk6ybr"))))
    (build-system python-build-system)
    (native-inputs
     (modify-inputs (package-native-inputs python-pytest)
                    (prepend python-argcomplete python-requests)))))

(define-public python-pytest-4
  (package
    (inherit python-pytest)
    (version "4.6.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest" version))
        (sha256
         (base32
          "0ls3pqr86xgif6bphsb6wrww9r2vc7p7a2naq8zcq8115wwq5yjh"))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-pytest)
       ((#:tests? _ #f) #f)))
    (native-inputs
     `(("python-argcomplete" ,python-argcomplete)
       ("python-atomicwrites" ,python-atomicwrites)
       ("python-requests" ,python-requests)
       ,@(package-native-inputs python-pytest)))))

(define-public python-pandas-plink
  (package
    (name "python-pandas-plink")
    (version "2.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://files.pythonhosted.org/packages/6f/de/48ccae952383ff3c9d227e06d6968bbf9923d509e40490f734baf2efb8b5/pandas_plink-" version ".tar.gz"))
        (sha256
         (base32
          "08wgrlv38nvsjcaw806fwy6bfl0h9swvr5x8nqx5xcsn8r04lsjq"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))
    (propagated-inputs
     `(("python-cffi" ,python-cffi)
       ("python-dask" ,python-dask)
       ("python-deprecated" ,python-deprecated)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-tqdm" ,python-tqdm)
       ("python-xarray" ,python-xarray)
       ("python-zstandard" ,python-zstandard)))
    (home-page
     "https://github.com/limix/pandas-plink")
    (synopsis
     "Read PLINK files into Pandas data frames")
    (description
     "Read PLINK files into Pandas data frames")
    (license license:expat)))

(define-public python-pytest-regressions
  (package
   (name "python-pytest-regressions")
   (version "2.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pytest-regressions" version))
     (sha256
      (base32
       "0227957vmy93pdaayakwwc2zawndbh203d2yc9nbss36i3s8xs3f"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-pytest" ,python-pytest)
      ("python-pytest-datadir" ,python-pytest-datadir)
      ("python-pyyaml" ,python-pyyaml)))
   (native-inputs
    `(("python-matplotlib" ,python-matplotlib)
      ("python-numpy" ,python-numpy)
      ("python-pandas" ,python-pandas)
      ("python-pillow" ,python-pillow)
      ("python-pre-commit" ,python-pre-commit)
      ("python-restructuredtext-lint"
       ,python-restructuredtext-lint)
      ("python-tox" ,python-tox)))
   (inputs
    `(("python-setuptools-scm" ,python-setuptools-scm)))
   (home-page
    "https://github.com/ESSS/pytest-regressions")
   (synopsis
    "Easy to use fixtures to write regression tests.")
   (description
    "Easy to use fixtures to write regression tests.")
   (license license:expat)))

(define-public python-pre-commit
  (package
   (name "python-pre-commit")
   (version "2.7.1")
   (source
    (origin
     (method url-fetch)
     (uri
     "https://files.pythonhosted.org/packages/bd/b4/e5d78c83533dd116596d05bdc8a82372b44ce7e7bef0ba7f0c6b33307613/pre_commit-2.7.1.tar.gz")
     (sha256
      (base32
       "0w2a104yhbw1z92rcwpq0gdjsxvr2bwx5ry5xhlf2psnfkjx6ky5"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-cfgv" ,python-cfgv)
      ("python-identify" ,python-identify)
      ("python-importlib-metadata"
       ,python-importlib-metadata)
      ("python-importlib-resources"
       ,python-importlib-resources)
      ("python-nodeenv" ,python-nodeenv)
      ("python-pyyaml" ,python-pyyaml)
      ("python-toml" ,python-toml)
      ("python-virtualenv" ,python-virtualenv)))
    (arguments
     `(#:tests? #f  ; suprise test failures
       ))
   (home-page
    "https://github.com/pre-commit/pre-commit")
   (synopsis
    "A framework for managing and maintaining multi-language pre-commit hooks.")
   (description
    "A framework for managing and maintaining multi-language pre-commit hooks.")
   (license license:expat)))

;; Latest version. Upstream once v2.x.x becomes a stable release candidate
(define-public python-mistune-2.0.0a5
  (package
    (inherit python-mistune)
    (name "python-mistune-2.0.0a5")
    (version "2.0.0a5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mistune" version))
       (sha256
        (base32
         "1vc0rd50wbny0qdjjc7z14xvjdsfcmzavx3njxpxr2dvhx3b6j79"))))))

(define-public python-engineio
  (package
    (name "python-engineio")
    (version "3.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-engineio" version))
       (sha256
        (base32
         "0jzhrq9n8mvndl42zv448jiak89dg8wcdvi7ynkvdn82lxm3rcrn"))))
    (build-system python-build-system)
    ;; No tests
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page
     "http://github.com/miguelgrinberg/python-engineio/")
    (synopsis "Engine.IO server")
    (description "Engine.IO server")
    (license license:expat)))

(define-public python-socketio
  (package
    (name "python-socketio")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-socketio" version))
       (sha256
        (base32
         "0npjklf76flavqlsijvz07h5ndvzwj1s5jjvla757i19q2xqz39m"))))
    (build-system python-build-system)
    ;; No tests
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-engineio" ,python-engineio)
       ("python-six" ,python-six)))
    (home-page
     "http://github.com/miguelgrinberg/python-socketio/")
    (synopsis "Socket.IO server")
    (description "Socket.IO server")
    (license license:expat)))

(define-public python-varint
  (package
    (name "python-varint")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "varint" version))
       (sha256
        (base32
         "19ac46adalgva1chlh0rxv6cinpikxfd92kabbbfjpmcfwiw1v56"))))
    (build-system python-build-system)
    (home-page
     "http://github.com/fmoo/python-varint")
    (synopsis "Simple python varint implementation")
    (description
     "Simple python varint implementation")
    (license license:expat)))

(define-public python-multiaddr
  (package
    (name "python-multiaddr")
    (version "0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "multiaddr" version))
       (sha256
        (base32
         "1kqfmcbv8plpicbygwpdljin7n82iyxklc0w1ywxbhzdi58nkcih"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-base58" ,python-base58)
       ("python-idna" ,python-idna)
       ("python-netaddr" ,python-netaddr)
       ("python-six" ,python-six)
       ("python-varint" ,python-varint)))
    (home-page
     "https://github.com/multiformats/py-multiaddr")
    (synopsis
     "Python implementation of jbenet's multiaddr")
    (description
     "Python implementation of jbenet's multiaddr")
    (license license:expat)))

(define-public python-ipfshttpclient
  (package
    (name "python-ipfshttpclient")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipfshttpclient" version))
       (sha256
        (base32
         "14rnqk61fqa6c1ql412q723g7spgpv2pch96h7p8gb632hy07cgy"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bumpup-max-version
           (lambda _
             (substitute* "ipfshttpclient/client/__init__.py"
               (("VERSION_MAXIMUM   = \"0.7.0\"")
                "VERSION_MAXIMUM   = \"0.8.1\""))
             #t)))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-httpx" ,python-httpx)
       ("python-multiaddr" ,python-multiaddr)
       ("python-requests" ,python-requests)))
    (home-page
     "https://ipfs.io/ipns/12D3KooWEqnTdgqHnkkwarSrJjeMP2ZJiADWLYADaNvUb6SQNyPF/")
    (synopsis "Python IPFS HTTP CLIENT library")
    (description "Python IPFS HTTP CLIENT library")
    (license license:expat)))


(define-public python-jupyter-server-proxy
  (package
    (name "python-jupyter-server-proxy")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyter-server-proxy" version))
        (sha256
         (base32 "0vglj7v7wq73a9dya60q2vrxxpglg11w66l7crkzzrfpbblr814p"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-aiohttp" ,python-aiohttp)
       ("python-jupyter-server" ,python-jupyter-server)
       ("python-simpervisor" ,python-simpervisor)))
    (home-page "https://jupyter-server-proxy.readthedocs.io/")
    (synopsis "Jupyter server extension to supervise and proxy web services")
    (description
     "Jupyter Server Proxy lets you run arbitrary external processes (such as RStudio, Shiny Server, Syncthing, PostgreSQL, Code Server, etc) alongside your notebook server and provide authenticated web access to them using a path like @code{/rstudio} next to others like @code{/lab}.  Alongside the python package that provides the main functionality, the JupyterLab extension (@code{@@jupyterlab/server-proxy}) provides buttons in the JupyterLab launcher window to get to RStudio for example.")
    (license license:bsd-3)))

(define-public python-jupyter-server-proxy-1    ; bundled javascript?
  (package
    (name "python-jupyter-server-proxy")
    (version "1.6.0")
    (source
      (origin
        ;; Tests not included in release.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jupyterhub/jupyter-server-proxy")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "03yry0jz6xlvy28h3w514pw0q9w51lnr1lpcigqmhnf5x7g9bfyy"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; Running the test suite isn't fully documented.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; see .github/workflows/test.yaml and CONTRIBUTING.md
               (setenv "JUPYTER_TOKEN" "secret")
               (setenv "HOME" (getcwd))
               (invoke "jupyter" "serverextension" "enable" "jupyter_server_proxy")
               (system "jupyter-notebook --config=./tests/resources/jupyter_server_config.py &")
               (sleep 5)
               (invoke "pytest" "--verbose"))
             #t)))))
    (propagated-inputs
     `(("python-aiohttp" ,python-aiohttp)
       ("python-notebook" ,python-notebook)
       ("python-simpervisor" ,python-simpervisor)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://jupyter-server-proxy.readthedocs.io/")
    (synopsis "Jupyter server extension to supervise and proxy web services")
    (description
     "Jupyter Server Proxy lets you run arbitrary external processes (such as
RStudio, Shiny Server, Syncthing, PostgreSQL, Code Server, etc) alongside your
notebook server and provide authenticated web access to them using a path like
@code{/rstudio} next to others like @code{/lab}.  Alongside the python package
that provides the main functionality, the JupyterLab extension
(@code{@@jupyterlab/server-proxy}) provides buttons in the JupyterLab launcher
window to get to RStudio for example.")
    (license license:bsd-3)))

(define-public python-jupyter-server
  (package
    (name "python-jupyter-server")
    (version "1.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyter-server" version))
        (sha256
         (base32
          "0dqj51fj5ikklbl0gnb939pp80ngnzml5932mljn2lvplph7a34g"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-anyio" ,python-anyio)
       ("python-argon2-cffi" ,python-argon2-cffi)
       ("python-ipython-genutils" ,python-ipython-genutils)
       ("python-jinja2" ,python-jinja2)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-jupyter-core" ,python-jupyter-core)
       ("python-nbconvert" ,python-nbconvert)
       ("python-nbformat" ,python-nbformat)
       ("python-prometheus-client" ,python-prometheus-client)
       ("python-pyzmq" ,python-pyzmq)
       ("python-send2trash" ,python-send2trash)
       ("python-terminado" ,python-terminado)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)
       ("python-websocket-client" ,python-websocket-client)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-ipykernel" ,python-ipykernel)
       ("python-pytest" ,python-pytest)
       ; ("python-pytest-console-scripts" ,python-pytest-console-scripts) - gone missing
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-mock" ,python-pytest-mock)
       ; ("python-pytest-tornasync" ,python-pytest-tornasync) - gone missing
       ("python-requests" ,python-requests)))
    (home-page "https://jupyter.org")
    (synopsis
      "The backend—i.e. core services, APIs, and REST endpoints—to Jupyter web applications.")
    (description
      "The backend—i.e. core services, APIs, and REST endpoints—to Jupyter web applications.")
    (license #f)))

(define-public python-anyio
  (package
    (name "python-anyio")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "anyio" version))
        (sha256
         (base32 "03mdklsgm4ik7hqr0lnc7k085as4vib27hclsrcdh0yhm48hgqj3"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-idna" ,python-idna)
       ("python-sniffio" ,python-sniffio)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-hypothesis" ,python-hypothesis)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-trustme" ,python-trustme)
       ("python-uvloop" ,python-uvloop)))
    (home-page "")
    (synopsis
      "High level compatibility layer for multiple asynchronous event loop implementations")
    (description
      "High level compatibility layer for multiple asynchronous event loop implementations")
    (license license:expat)))

;; Waiting for this to be merged upstream then I'll remove it
(define-public python-flask-debugtoolbar
  (package
    (name "python-flask-debugtoolbar")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-DebugToolbar" version))
       (sha256
        (base32
         "1d5asdnk8bmh6m46pzg3i7677cjgdm9mlm3wcpk19q7dak9pjkiw"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-blinker" ,python-blinker)
       ("python-flask" ,python-flask)
       ("python-itsdangerous" ,python-itsdangerous)
       ("python-werkzeug" ,python-werkzeug)))
    (home-page
     "https://flask-debugtoolbar.readthedocs.io/")
    (synopsis
     "A toolbar overlay for debugging Flask applications.")
    (description
     "This extension adds a toolbar overlay to Flask applications containing
useful information for debugging.")
    (license license:bsd-3)))

(define-public python-requirements-parser
  (package
    (name "python-requirements-parser")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "requirements-parser" version))
        (sha256
          (base32 "1m2fgnyrh4vb5canm7cp30b04f7vh8869z6kb2gsw19dbj4ywqsr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
      `(("python-coverage" ,python-coverage)
        ("python-coveralls" ,python-coveralls)
        ("python-nose" ,python-nose)))
    (home-page "https://github.com/davidfischer/requirements-parser")
    (synopsis "Parses Pip requirement files")
    (description 
"This is a small Python module for parsing Pip requirement files.
The goal of the project is to parse everything in the Pip requirement
file format spec.")
    (license license:bsd-3)))

(define-public python-supervisor
  (package
    (name "python-supervisor")
    (version "4.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "supervisor" version))
        (sha256
          (base32 "18lpddhyl8ziy6lc0klassxpd8kdg8c0nhkrvz1k3ipfw4n5ip20"))))
    (build-system python-build-system)
    (native-inputs
      (list python-pytest python-pytest-cov))
    (arguments (list #:tests? #f)) ; fixme
    (home-page "http://supervisord.org/")
    (synopsis "A system for controlling process state under UNIX")
    (description
      "This package provides a system for controlling process state under UNIX")
    (license
      ;; https://github.com/Supervisor/supervisor/issues/1364
      (license:fsf-free (string-append "https://web.archive.org/web/20190211105114/"
                                       "http://www.repoze.org/LICENSE.txt")))))

(define-public python-rdflib-shim
  (package
    (name "python-rdflib-shim")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "rdflib_shim" version))
              (sha256
               (base32
                "03gwsjcinbyyqrhs2jfhs6mr7j69dfn5djihd0mv9al654gd2mfr"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "unittest" "discover" "-s" "tests")))))))
    (propagated-inputs
     (list python-rdflib python-rdflib-jsonld-0.6.1))
    (native-inputs
     (list python-pbr))
    (home-page "http://hsolbrig.github.io/rdflib-shim")
    (synopsis "Shim for rdflib 5 and 6 incompatibilities")
    (description "Shim for rdflib 5 and 6 incompatibilities")
    (license license:cc0)))

;; Don't inherit from python-rdflib-jsonld, that package is scheduled to be removed.
(define python-rdflib-jsonld-0.6.1
  (package
    (name "python-rdflib-jsonld")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rdflib-jsonld" version))
        (sha256
         (base32 "1q50h89zppdwnzk425cg6rsz5kdwhk3baclflx6hvy095qma99gd"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list python-nose))
    (propagated-inputs
     (list python-rdflib))
    (home-page "https://github.com/RDFLib/rdflib-jsonld")
    (synopsis "rdflib extension adding JSON-LD parser and serializer")
    (description "This package provides an rdflib extension adding JSON-LD
parser and serializer.")
    (license license:bsd-3)))
