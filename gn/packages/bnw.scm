(define-module (gn packages bnw)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (gn packages javascript)
  #:use-module (gn packages web))

(define-public bnw
  (let ((commit "f39cd7ba681262de5658cfe7b9b0d46c6000b4fa")
        (revision "2"))
    (package
      (name "bnw")
      (version (git-version "genenet8_initial_1.3" revision commit)) ; Mar 4, 2021
      (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/ziejd2/BNW")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1w5hf3pkp91lf697675hjmhir622nz49spx30d0vkvh4i791cpqh"))
               (modules '((guix build utils)))
               (snippet
                '(begin
                   (delete-file "var_lib_genenet_bnw/localscore/libRmath.so")
                   (delete-file-recursively "bnw-env")
                   (delete-file "var_lib_genenet_bnw/k-best/src/get_kbest_nets")
                   (delete-file "var_lib_genenet_bnw/k-best/src/get_kbest_parents")
                   (for-each delete-file (find-files "." "\\.dll$"))
                   (for-each delete-file (find-files "." "\\.o$"))
                   (for-each
                     (lambda (file)
                       (delete-file (string-append "sourcecodes/scripts/" file)))
                     ;; accordion.js, create_table.js and table.css are original files.
                     '(;"FileSaver.min.js"          ; sha256 mismatch
                       ;"canvas-toBlob.js"          ; sha256 mismatch
                       ;"cytoscape-dagre.min.js"    ; sha256 mismatch
                       ;"cytoscape-panzoom.js"      ; sha256 mismatch
                       ;"cytoscape.js-panzoom.css"  ; sha256 mismatch
                       ;"cytoscape.min.js"
                       ;"d3-selection-multi.v1.js"
                       ;"d3.v4.min.js"  ; some work needs to be done on our package
                       ;"dagre.js"                  ; sha256 mismatch
                       ;"font-awesome.css"          ; sha256 mismatch
                       ;"fontawesome-webfont.eot"   ; sha256 mismatch
                       ;"fontawesome-webfont.svg"   ; sha256 mismatch
                       ;"fontawesome-webfont.ttf"   ; sha256 mismatch
                       ;"fontawesome-webfont.woff"  ; sha256 mismatch
                       ;"jquery.min.js"             ; sha256 mismatch
                       ;"lodash.js"                 ; sha256 mismatch
                       ))
                   #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'patch-source
             (lambda _
               ;; sourcecodes/matrix.php     hardcodes $dir=/tmp/bnw
               (substitute* "sourcecodes/matrix.php"
                 (("/tmp/bnw") "/var/lib/genenet/bnw/"))
               #t))
           (add-after 'patch-source-shebangs 'patch-more-shebangs
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((graphviz (assoc-ref inputs "graphviz"))
                     (octave   (assoc-ref inputs "octave"))
                     (python   (assoc-ref inputs "python"))
                     (rmath    (assoc-ref inputs "rmath")))
                 (for-each (lambda (file)
                   (patch-shebang file
                     (list (string-append octave "/bin"))))
                   (find-files "sourcecodes/run_scripts" "^run"))
                 (substitute* (find-files "sourcecodes" "(^run|py$)")
                   (("/var/www/html/compbio/BNW_1.3/bnw-env/bin/python3")
                    (which "python3"))
                   (("/var/www/html/compbio/BNW_1.3/bnw-env/bin/python")
                    (which "python3")))
                 (substitute*
                   (append (find-files "sourcecodes" ".php")
                           (find-files "sourcecodes/run_scripts"))
                   (("/usr/bin/dot") (string-append graphviz "/bin/dot")))
                 (substitute* "var_lib_genenet_bnw/build.sh"
                   (("./localscore/libRmath.so")
                    (string-append rmath "/lib/libRmath.so")))
               #t)))
           (add-after 'patch-source-shebangs 'replace-javascript
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((jquery        (assoc-ref inputs "jquery"))
                     (awesome       (assoc-ref inputs "awesome"))
                     (cyto          (assoc-ref inputs "cytoscape"))
                     (cs-dagre      (assoc-ref inputs "cyto-dagre"))
                     (d3js-4        (assoc-ref inputs "d3js-4"))
                     (d3js-multi    (assoc-ref inputs "d3js-multi"))
                     (dagre         (assoc-ref inputs "dagre"))
                     (lodash        (assoc-ref inputs "lodash"))
                     (canvas-toblob (assoc-ref inputs "canvas-toblob"))
                     (filesaver     (assoc-ref inputs "filesaver"))
                     (panzoom       (assoc-ref inputs "panzoom"))
                     (js-path "/share/genenetwork2/javascript/"))
                 (substitute* "sourcecodes/layout_cyto.php"
                   (("https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.7.1/cytoscape.min.js")
                    "./scripts/cytoscape.min.js")
                   (("https://cdn.jsdelivr.net/npm/cytoscape-dagre@2.2.2/cytoscape-dagre.min.js")
                    "./scripts/cytoscape-dagre.min.js")
                   (("https://unpkg.com/dagre@0.7.4/dist/dagre.js")
                    "./scripts/dagre.js")
                   (("https://cdnjs.cloudflare.com/ajax/libs/cytoscape-panzoom/2.5.3/cytoscape.js-panzoom.css")
                    "./scripts/cytoscape.js-panzoom.css")
                   (("https://cdnjs.cloudflare.com/ajax/libs/cytoscape-panzoom/2.5.3/cytoscape-panzoom.js")
                    "./scripts/cytoscape-panzoom.js")
                   (("https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.0.3/css/font-awesome.css")
                    "./scripts/font-awesome.css")
                   (("https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js")
                    "./scripts/jquery.min.js")
                   (("https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.10/lodash.js")
                    "./scripts/lodash.js"))
                 (substitute* "sourcecodes/layout_svg_no.php"
                   (("http://d3js.org/d3.v4.min.js")
                    "./scripts/d3.v4.min.js")
                   (("http://d3js.org/d3-selection-multi.v1.js")
                    "./scripts/d3-selection-multi.v1.js")
                   (("https://cdn.rawgit.com/eligrey/canvas-toBlob.js/f1a01896135ab378aa5c0118eadd81da55e698d8/canvas-toBlob.js")
                    "./scripts/canvas-toBlob.js")
                   (("https://cdn.rawgit.com/eligrey/FileSaver.js/e9d941381475b5df8b7d7691013401e171014e89/FileSaver.min.js")
                    "./scripts/FileSaver.min.js"))
                 ;; Against Google's ToS to make available offline:
                 ;; https://developers.google.com/chart/interactive/faq?csw=1#offline
                 ;(substitute* '("sourcecodes/network_layout_evd.php"
                 ;               "sourcecodes/network_layout_evd_2.php"
                 ;               "sourcecodes/network_layout_inv.php"
                 ;               "sourcecodes/network_layout_inv_2.php")
                 ;  (("https://www.google.com/jsapi") "https://www.gstatic.com/charts/loader.js"))
                 )
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (copy-recursively "." out))
               #t))
           (add-after 'install 'install-javascript-libraries
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (scripts      (string-append out "/sourcecodes/scripts/"))
                      (fonts        (string-append out "/sourcecodes/fonts/"))
                      (jquery       (assoc-ref inputs "jquery"))
                      (awesome      (assoc-ref inputs "awesome"))
                      (cyto         (assoc-ref inputs "cytoscape"))
                      (cs-dagre     (assoc-ref inputs "cyto-dagre"))
                      (d3js-4       (assoc-ref inputs "d3js-4"))
                      (d3js-multi   (assoc-ref inputs "d3js-multi"))
                      (dagre        (assoc-ref inputs "dagre"))
                      (lodash       (assoc-ref inputs "lodash"))
                      (canvas-toblob (assoc-ref inputs "canvas-toblob"))
                      (filesaver    (assoc-ref inputs "filesaver"))
                      (panzoom      (assoc-ref inputs "panzoom"))
                      (js-path  "/share/genenetwork/javascript/")
                      (js-path2 "/share/genenetwork2/javascript/"))
                 (unless (file-exists? (string-append scripts "cytoscape.min.js"))
                   (symlink (string-append cyto js-path2 "cytoscape/cytoscape.min.js")
                            (string-append scripts "cytoscape.min.js")))
                 (unless (file-exists? (string-append scripts "cytoscape-dagre.min.js"))
                   (symlink (string-append cs-dagre "/share/javascript/cytoscape-dagre.min.js")
                            (string-append scripts "cytoscape-dagre.min.js")))
                 (unless (file-exists? (string-append scripts "dagre.js"))
                   (symlink (string-append dagre js-path2 "dagre/dagre.js")
                            (string-append scripts "dagre.js")))
                 (unless (file-exists? (string-append scripts "cytoscape.js-panzoom.css"))
                   (symlink (string-append panzoom js-path2 "cytoscape-panzoom/cytoscape.js-panzoom.css")
                            (string-append scripts "cytoscape.js-panzoom.css")))
                 (unless (file-exists? (string-append scripts "cytoscape-panzoom.js"))
                   (symlink (string-append panzoom js-path2 "cytoscape-panzoom/cytoscape-panzoom.js")
                            (string-append scripts "cytoscape-panzoom.js")))
                 (unless (file-exists? (string-append scripts "font-awesome.css"))
                   (symlink (string-append awesome "/share/web/font-awesomecss/font-awesome.css")
                            (string-append scripts "font-awesome.css"))
                   ;; font-awesome.css depends on the other font-awesome files,
                   ;; by default in the ../fonts/ folder. Because we remove the
                   ;; bundled (and minimally modified) version we have to make
                   ;; some adjustments, namely moving the font files.
                   (mkdir-p fonts)
                   (for-each
                     (lambda (font)
                       (symlink (string-append awesome "/share/web/font-awesomefonts"
                                               "/fontawesome-webfont." font)
                                (string-append fonts "fontawesome-webfont." font)))
                     '("eot" "woff" "woff2" "ttf" "svg")))

                 (unless (file-exists? (string-append scripts "jquery.min.js"))
                   (symlink (string-append jquery "/share/web/jquery/jquery.min.js")
                            (string-append scripts "jquery.min.js")))
                 (unless (file-exists? (string-append scripts "lodash.js"))
                   (symlink (string-append lodash js-path2 "lodash/lodash.js")
                            (string-append scripts "lodash.js")))
                 (unless (file-exists? (string-append scripts "d3.v4.min.js"))
                   (symlink (string-append d3js-4 js-path "d3js/d3.min.js")
                            (string-append scripts "d3.v4.min.js")))
                 (unless (file-exists? (string-append scripts "d3-selection-multi.v1.js"))
                   (symlink (string-append d3js-multi js-path "d3js-multi/d3-selection-multi.js")
                            (string-append scripts "d3-selection-multi.v1.js")))
                 (unless (file-exists? (string-append scripts "canvas-toBlob.js"))
                   (symlink (string-append canvas-toblob js-path "canvas-toblob/canvas-toBlob.js")
                            (string-append scripts "canvas-toBlob.js")))
                 (unless (file-exists? (string-append scripts "FileSaver.min.js"))
                   (symlink (string-append filesaver js-path2 "filesaver/FileSaver.min.js")
                            (string-append scripts "FileSaver.min.js")))
               #t)))
           (add-after 'install 'make-files-executable
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (for-each
                   (lambda (file)
                     (chmod file #o555))
                   (append (find-files out "\\.(sh|py)$")
                           (find-files
                             (string-append out "/sourcecodes/run_scripts/") "^run")))
                 #t)))
           (add-after 'make-files-executable 'wrap-executables
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each
                 (lambda (script)
                   (wrap-program script
                    `("PYTHONPATH" prefix (,(getenv "PYTHONPATH")))))
                 (find-files (string-append (assoc-ref outputs "out")
                                            "/sourcecodes/run_scripts") "^run"))
               (for-each
                 (lambda (script)
                   (wrap-program script
                    `("PATH" prefix (,(dirname (which "cut"))))))
                 (append
                   (find-files (string-append (assoc-ref outputs "out")
                                              "/sourcecodes") "\\.sh$")
                   (find-files (string-append (assoc-ref outputs "out")
                                              "/var_lib_genenet_bnw") "\\.sh$")))
               #t))
           (replace 'build
             (lambda* (#:key inputs #:allow-other-keys)
               (with-directory-excursion "var_lib_genenet_bnw"
                 (invoke "sh" "build.sh")))))))
      (inputs
       `(("graphviz" ,graphviz)
         ("octave" ,octave)
         ("python" ,python)
         ("python-plotly" ,python-plotly)
         ("rmath" ,rmath-standalone)
         ;; the javascript libraries:
         ("awesome" ,web-font-awesome)
         ("canvas-toblob" ,javascript-canvas-toblob)
         ("cyto-dagre" ,javascript-cytoscape-dagre)
         ("cytoscape" ,javascript-cytoscape)
         ("d3js-4" ,javascript-d3js-4)
         ("d3js-multi" ,javascript-d3js-multi)
         ("dagre" ,javascript-dagre)
         ("filesaver" ,javascript-filesaver)
         ("jquery" ,web-jquery)
         ("lodash" ,javascript-lodash)
         ("panzoom" ,javascript-cytoscape-panzoom)))
      (native-inputs
       ;; get_best_knets isn't buildable with anything newer than gcc-5.
       `(("gcc" ,(@ (gnu packages commencement) gcc-toolchain-5))))
      (home-page "http://compbio.uthsc.edu/BNW/")
      (synopsis "Bayesian Network Webserver")
      (description
       "This contains the code for the @dfn{Bayesian Network Webserver} (BNW).")
      (license (list license:gpl2
                     license:gpl2+
                     license:lgpl2.1+)))))
