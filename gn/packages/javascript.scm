(define-module (gn packages javascript)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages javascript)
  #:use-module (gn packages web)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system minify)
  #:use-module (guix build-system trivial)
  #:use-module ((srfi srfi-1) #:hide (zip)))

;; see color-make package for great example - also install-file and invoke

(define-public javascript-twitter-post-fetcher
  (let ((commit "8f9e667e917b3c581b100bf2ccc7157aadc5ff43") ; April 30, 2019
        (revision "1"))
    (package
      (name "javascript-twitter-post-fetcher")
      (version (git-version "18.0.2" revision commit)) ; April 3, 2018
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/jasonmayes/Twitter-Post-Fetcher.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0n935qmmd9gijkklrps8qimplbskcrijds3076zfd28afxrr96wp"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/" ,name))
                (source (assoc-ref %build-inputs "source")))
           (begin
             (use-modules (guix build utils))
             (copy-recursively source targetdir)
             (install-file (string-append source "/License.txt")
                           (string-append out "/share/doc/" ,name "-" ,version))))))
      (native-inputs `(("source" ,source)))
      (home-page "http://jasonmayes.com/projects/twitterApi/")
      (synopsis "Twitter post fetching")
      (description "Allows you to get your tweets displaying on your website
using JavaScript, without using new Twitter 1.1 API.")
      (license license:expat))))

(define-public javascript-cytoscape
  (package
    (name "javascript-cytoscape")
    (version "3.8.1") ; July 9, 2019
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cytoscape/cytoscape.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dnwvmghwq21g9ag5cj49l0wnyfc54kbsrj0byii5wbwljjg9826"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://js.cytoscape.org/")
    (synopsis "Graph theory (network) library for visualisation and analysis")
    (description "Cytoscape.js is a fully featured graph theory library.  Do you
need to model and/or visualise relational data, like biological data or social
networks? If so, Cytoscape.js is just what you need.

Cytoscape.js contains a graph theory model and an optional renderer to display
interactive graphs.  This library was designed to make it as easy as possible
for programmers and scientists to use graph theory in their apps, whether it's
for server-side analysis in a Node.js app or for a rich user interface.")
    (license license:expat)))

(define-public javascript-cytoscape-3.17
  (package
    (inherit javascript-cytoscape)
    (name "javascript-cytoscape")
    (version "3.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cytoscape/cytoscape.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fp4bv3gq9vj0517h61jvg7bxk8fyrhrq0jr1v4d0lzp164sgxln"))))))

(define-public javascript-cytoscape-2
  (package
    (inherit javascript-cytoscape)
    (name "javascript-cytoscape")
    (version "2.7.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cytoscape/cytoscape.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00y0h6kdkw2x6lyf9c16fxmg8iagfl77dz8nqb337v3ljifkb4z8"))))))

(define-public javascript-cytoscape-panzoom
  (package
    (name "javascript-cytoscape-panzoom")
    (version "2.5.3") ; August 21, 2018
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cytoscape/cytoscape.js-panzoom")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lzcsip6q44h14g5l4jciv5sfc7ilvg1yrd14lcji8mnq6akx16n"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape-panzoom"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/cytoscape-panzoom.js") targetdir)
           (install-file (string-append source "/cytoscape.js-panzoom.css") targetdir)))))
    (native-inputs `(("source" ,source)))
    ;; TODO: Add font-awsome?
    (propagated-inputs
     `(("javascript-cytoscape" ,javascript-cytoscape)
       ("jquery" ,web-jquery)))
    (home-page "https://github.com/cytoscape/cytoscape.js-panzoom/")
    (synopsis "Panzoom extension for Cytoscape.js")
    (description "This extension creates a widget that lets the user pan and
zoom about a Cytoscape.js graph.  This complements the built-in gesture support
for panning and zooming in Cytoscape.js by giving less savvy users a more
traditional UI -- similar to controls on map webapps.")
    (license license:expat)))

(define-public javascript-dagre
  (package
    (name "javascript-dagre")
    (version "0.8.4") ; Dec. 9, 2018
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dagrejs/dagre.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1svlvd66bnskicqb7kizx57s97z9lkxssh1g5sgymw7ddfdbhy5l"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/dagre"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://github.com/dagrejs/dagre")
    (synopsis "Directed graph layout for JavaScript")
    (description "Dagre is a JavaScript library that makes it easy to lay out
directed graphs on the client-side.")
    (license license:expat)))

(define-public javascript-cytoscape-dagre
  (package
    (name "javascript-cytoscape-dagre")
    (version "2.2.2") ; Sept. 26, 2018
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cytoscape/cytoscape.js-dagre")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0z0sh5q5cd0iirdyhlln83vmsvwn1sbh4zdmdh8k5hld075g4q64"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("cytoscape-dagre.js")))
    (propagated-inputs
     `(("javascript-cytoscape" ,javascript-cytoscape)
       ("javascript-dagre" ,javascript-dagre)))
    (home-page "https://github.com/cytoscape/cytoscape.js-dagre")
    (synopsis "Dagre layout for DAGs and trees for Cytoscape.js")
    (description "The dagre layout organises the graph using a @dfn{directed
acyclic graph} (DAG) system, written by Chris Pettitt.  It is especially
suitable for DAGs and trees.")
    (license license:expat)))

;; Author recommends using cytoscape-popper with tippy.js since qtip2 is no longer maintained
(define-public javascript-cytoscape-qtip
  (package
    (name "javascript-cytoscape-qtip")
    (version "2.7.1") ; May 4, 2017
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cytoscape/cytoscape.js-qtip")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1rdwdkx9j1lqzks8gi8ilkcbryswdx653569av4i74pv4j93v6zx"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape-qtip"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/cytoscape-qtip.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (propagated-inputs ; TODO: Add qtip
     `(("javascript-cytoscape" ,javascript-cytoscape)
       ("jquery" ,web-jquery)))
    (home-page "https://github.com/cytoscape/cytoscape.js")
    (synopsis "Cytoscape.js extension that wraps the QTip jQuery library")
    (description "Cytoscape.")
    (license license:expat)))

(define-public javascript-cytoscape-popper
  (package
    (name "javascript-cytoscape-popper")
    (version "1.0.4") ; Feb 13, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cytoscape/cytoscape.js-popper")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0nb59cimzp6zprk0czrfkc6id70ia2gg8drgmd55nf3yspn4b5rj"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape-popper"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/cytoscape-popper.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (propagated-inputs `(("javascript-cytoscape" ,javascript-cytoscape)))
    (home-page "https://github.com/cytoscape/cytoscape.js-popper")
    (synopsis "Cytoscape.js extension for integrating Popper.js")
    (description "Popper.js allows you to dynamically align a div, e.g. a
tooltip, to another element in the page.  This extension allows you to use
Popper.js on Cytoscape elements.  This allows you to create DOM elements
positioned on or around Cytoscape elements.  It is useful for tooltips and
overlays, for example.")
    (license license:expat)))

(define-public js-cytoscape-svg-vendor-0.3.1
  (package
    (name "js-cytoscape-svg-vendor")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://unpkg.com/cytoscape-svg@" version
                            "/cytoscape-svg.js"))
        (sha256
         (base32
          "0ic0m91a4a4517c6f8nwf649yyq6xma57rxp8cyk4261qhk70494"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out  (assoc-ref %outputs "out"))
                (dest (string-append out "//share/javascript")))
           (mkdir-p dest)
           (copy-file (assoc-ref %build-inputs "source")
                      (string-append dest "/cytoscape-svg.js"))
           #t))))
    (home-page "https://kinimesi.github.io/cytoscape-svg")
    (synopsis "Cytoscape.js extension to export the current graph view as an SVG")
    (description
     "A Cytoscape.js extension to export the current graph view as an SVG.")
    (license license:gpl3)
    (properties `((hidden? . #t)))))

(define-public javascript-qtip2
  (package
    (name "javascript-qtip2")
    (version "3.0.3") ; May 11, 2016
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/qTip2/qTip2.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "06lmmy6mqbfdgbbyjm0v2bl1ifdv03rh6vqnsmmkn1s56kd2qr62"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/qtip2"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "http://qtip2.com/")
    (synopsis "Pretty powerful tooltips")
    (description "The second generation of the advanced qTip plugin for the
ever popular jQuery framework.")
    (license license:expat)))

(define-public javascript-datatables
  (package
    (name "javascript-datatables")
    (version "1.10.19") ; June 22, 2018
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/DataTables/DataTables.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0p85xzcvfjdrs4nwj7lhnlw2dmyky0hkwy8bzjw2fdabmsrdpwyg"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTables"))
                (source (assoc-ref %build-inputs "source"))
                (media (string-append source "/media")))
           (copy-recursively media targetdir)))))
    (native-inputs `(("source" ,source)))
    (propagated-inputs `(("javascript-cytoscape" ,javascript-cytoscape)))
    (home-page "https://www.datatables.net/")
    (synopsis "Tables plug-in for jQuery")
    (description "DataTables is a table enhancing plug-in for the jQuery
Javascript library, adding sorting, paging and filtering abilities to plain HTML
tables with minimal effort.")
    (license license:expat)))

(define-public javascript-datatables-scroller-style
  (package
    (name "javascript-datatables-scroller-style")
    (version "2.0.3") ; Dec 12 , 2020
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/DataTables/Dist-DataTables-Scroller-DataTables.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "17lhgd8c6fxgh1b9rgln9pqljma2gz0whrjsixfd6jmiqn9gpqzk"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTablesExtensions/scrollerStyle/"))
                (source (assoc-ref %build-inputs "source")))
           (copy-recursively source targetdir)))))
    (propagated-inputs `(("javascript-datatables" ,javascript-datatables)))
    (home-page "https://datatables.net/extensions/scroller/scrollerStyles")
    (synopsis "This package contains distribution files required to style Scroller extension for DataTables.")
    (description "Scroller is a virtual rendering plug-in for DataTables which allows large datasets to be drawn on screen very quickly. Virtual rendering means is that only the visible portion of the table is drawn, while the scrolling container gives the visual impression that the whole table is visible, allowing excellent browser performance.")
    (license license:expat)))

(define-public javascript-datatables-scroller
  (package
    (name "javascript-datatables-scroller")
    (version "2.0.3") ; Dec 12,2020
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/DataTables/Dist-DataTables-Scroller.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "18pgy1zi6m2c4ycgylrszj2a8x9h2n8n7ymkwddcpk1ryq4n2l6q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTablesExtensions/scroller"))
                (source (assoc-ref %build-inputs "source")))
           (copy-recursively source targetdir)))))
    (propagated-inputs `(("javascript-datatables" ,javascript-datatables)
			 ("javascript-datatables-scroller-style", javascript-datatables-scroller-style)
			 ))
    (home-page "https://datatables.net/extensions/scroller/")
    (synopsis "This package contains distribution files for the Scroller extension for DataTables. Only the core software for this library is contained in this package - to be correctly styled, a styling package for Scroller must also be included.")
    (description "Scroller is a virtual rendering plug-in for DataTables which allows large datasets to be drawn on screen very quickly. Virtual rendering means is that only the visible portion of the table is drawn, while the scrolling container gives the visual impression that the whole table is visible, allowing excellent browser performance.")
    (license license:expat)))

(define-public javascript-xterm
  (package
    (name "javascript-xterm")
    (version "4.9.0") ; Jan 08 2021
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cdn.jsdelivr.net/npm/xterm@" version
                           "/lib/xterm.min.js"))
       (file-name (string-append "xterm.min" version ".js"))
       (sha256
        (base32
         "18smra546ws5fhnfdhj9m6yhvfjqdwx44jyb19q3az780ifwj2lz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir
                 (string-append out "/share/genenetwork2/javascript/xterm"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p targetdir)
           (copy-file source (string-append targetdir "/xterm.min.js"))))))
    (native-inputs `(("source" ,source)))
    (home-page "https://xtermjs.org/")
    (synopsis "Javascript library that allows apps to bring fully-featured terminals in browsers.")
    (description
     "Xterm.js is a front-end component written in TypeScript that lets applications bring fully-featured terminals to their users in the browser. It's used by popular projects such as VS Code, Hyper and Theia.")
    (license license:expat)))

(define-public javascript-xterm-style
  (package
    (inherit javascript-xterm)
    (name "javascript-xterm-style")
    (version "4.9.0") ; Jan 08 2021
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cdn.jsdelivr.net/npm/xterm@" version
                           "/css/xterm.min.css"))
       (file-name (string-append "xterm.min" version ".css"))
       (sha256
        (base32
         "0nc7ysmfngzy1q1x4sasgnd5x91pqc00lgkajynz2gdan1rwsfk3"))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir
                 (string-append out "/share/genenetwork2/javascript/xterm"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p targetdir)
           (copy-file source (string-append targetdir "/xterm.min.css"))))))
    (native-inputs `(("source" ,source)))
    (synopsis "Style sheets for xtermjs")
    (description "Style sheets for xtermjs")
    (license license:expat)))


(define-public javascript-xterm-addon-fit
  (package
    (inherit javascript-xterm)
    (name "javascript-xterm-addon-fit")
    (version "0.5.0") ; Nov 16 2021
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cdn.jsdelivr.net/npm/xterm-addon-fit@" version
                   "/lib/xterm-addon-fit.min.js"))
       (sha256
        (base32
         "0x47zj8wknisjjjqz6sasjn94nm803h5br8frypsbwhmdw6v9nak"))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir
                 (string-append out "/share/genenetwork2/javascript/xterm"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p targetdir)
           (copy-file source (string-append targetdir "xterm-addon-fit-min.js"))))))
    (native-inputs `(("source" ,source)))
    (synopsis "xterm terminal addon")
    (description "An addon for xterm.js that enables fitting the terminal's dimensions to a containing element")
    (license license:expat)))

(define-public javascript-datatables-buttons
  (package
    (name "javascript-datatables-buttons")
    (version "1.6.2") ; May 11, 2020
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/DataTables/Dist-DataTables-Buttons.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1iny8bsm4xx2w7psj7kf140bvkznlw5jvpdi2r7h6fzyrr5hd60x"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTablesExtensions/buttons"))
                (source (assoc-ref %build-inputs "source")))
           (copy-recursively source targetdir)))))
    (propagated-inputs `(("javascript-datatables" ,javascript-datatables)))
    (home-page "https://datatables.net/extensions/buttons/")
    (synopsis "This package contains distribution files for the Buttons extension for DataTables.")
    (description "The Buttons extension for DataTables provides a common set of options, API methods and styling to display buttons on a page that will interact with a DataTable. It also provides plug-ins for file export (HTML5 and Flash), print view and column visibility. Other libraries, such as Editor and Select also provide buttons specific to their use cases.")
    (license license:expat)))

(define-public javascript-datatables-buttons-styles
  (package
    (name "javascript-datatables-buttons-styles")
    (version "1.6.1") ; Oct 18, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/DataTables/Dist-DataTables-Buttons-DataTables.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "070xc2vgmb8zarhjjsxg4gsknrpxw81dwhbj2qjnbzpf5s2hxhkf"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTablesExtensions/buttonStyles"))
                (source (assoc-ref %build-inputs "source")))
           (copy-recursively source targetdir)))))
    (propagated-inputs `(("javascript-datatables" ,javascript-datatables)))
    (home-page "https://datatables.net/extensions/buttons/")
    (synopsis "This package contains distribution files required to style Buttons extension for DataTables.")
    (description "The Buttons extension for DataTables provides a common set of options, API methods and styling to display buttons on a page that will interact with a DataTable. It also provides plug-ins for file export (HTML5 and Flash), print view and column visibility. Other libraries, such as Editor and Select also provide buttons specific to their use cases.")
    (license license:expat)))

(define-public javascript-datatables-buttons-bootstrap
  (package
    (name "javascript-datatables-buttons-bootstrap")
    (version "1.6.2") ; May 11, 2020
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/DataTables/Dist-DataTables-Buttons-Bootstrap.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "090a9xrfsk40fqk1s1rjkm4j3nx08wz0lxvnqidd013ff7awaqxa"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTablesExtensions/buttonsBootstrap"))
                (source (assoc-ref %build-inputs "source")))
           (copy-recursively source targetdir)))))
    (propagated-inputs `(("javascript-datatables" ,javascript-datatables)))
    (home-page "https://datatables.net/extensions/buttons/")
    (synopsis "This package contains distribution files required to style Buttons extension for DataTables with styling for Bootstrap.")
    (description "The Buttons extension for DataTables provides a common set of options, API methods and styling to display buttons on a page that will interact with a DataTable. It also provides plug-ins for file export (HTML5 and Flash), print view and column visibility. Other libraries, such as Editor and Select also provide buttons specific to their use cases.")
    (license license:expat)))

(define-public javascript-datatables-plugins
  (package
    (name "javascript-datatables-plugins")
    (version "1.10.20") ; Oct 1, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/DataTables/Plugins.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05zni20863ml1711lfllljdfkb3k05h0kpqhkijkbp0bp7q0ak94"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTablesExtensions/plugins"))
                (source (assoc-ref %build-inputs "source")))
           (copy-recursively source targetdir)))))
    (propagated-inputs `(("javascript-datatables" ,javascript-datatables)))
    (home-page "https://datatables.net/plug-ins/")
    (synopsis "This package contains a collection of plug-ins for the jQuery DataTables table enhancer")
    (description "These plug-ins are feature enhancing for the DataTables library, adding extra options to core functionality such as additional sort algorithms, API methods and pagination controls. The plug-ins should not be confused with DataTables 'extensions' which are more significant software libraries which add additional features to DataTables.")
    (license license:expat)))

(define-public javascript-datatables-col-reorder
  (package
   (name "javascript-datatables-col-reorder")
    (version "1.5.2") ; Oct 1, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/DataTables/Dist-DataTables-ColReorder.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0y5d6wpk0qbgng66a3w63c994wxyawx62wswflf166a89vmaf1gj"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTablesExtensions/colReorder"))
                (source (assoc-ref %build-inputs "source")))
           (copy-recursively source targetdir)))))
    (propagated-inputs `(("javascript-datatables" ,javascript-datatables)))
    (home-page "https://datatables.net/extensions/colreorder/")
    (synopsis "This package contains distribution files for the ColReorder extension for DataTables.")
    (description "ColReorder adds the ability for the end user to be able to reorder columns in a DataTable through a click and drag operation. This can be useful when presenting data in a table, letting the user move columns that they wish to compare next to each other for easier comparison")
    (license license:expat)))

(define-public javascript-datatables-col-resize
  (package
   (name "javascript-datatables-col-resize")
    (version "0.0.10") ; June 26, 2015
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/Silvacom/colResize.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0s04r8jj88zfi1qj97afan3jygpwfm2m7lyazdkxm9adwsd52g8m"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTablesExtensions/colResize"))
                (source (assoc-ref %build-inputs "source")))
           (copy-recursively source targetdir)))))
    (propagated-inputs `(("javascript-datatables" ,javascript-datatables)))
    (home-page "https://github.com/Silvacom/colResize")
    (synopsis "A DataTables plugin for dynamic resizing of columns")
    (description "A DataTables plugin for dynamic resizing of columns")
    (license license:expat)))

(define-public javascript-lodash
  (package
    (name "javascript-lodash")
    (version "4.17.15") ; July 17, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lodash/lodash")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1hp04cg3b59j3dpnvzixd5p6wpv34mj2pnq8wp60csv3p2s0qk4y"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/lodash"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://lodash.com")
    (synopsis "JavaScript utility library delivering modularity, performance & extras")
    (description "Lodash makes JavaScript easier by taking the hassle out of
working with arrays, numbers, objects, strings, etc. Lodash's modular methods
are great for:
@enumerate
@item Iterating arrays, objects, & strings
@item Manipulating & testing values
@item Creating composite functions
@end enumerate")
    (license license:expat)))

(define-public javascript-nvd3
  (package
    (name "javascript-nvd3")
    (version "1.8.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/novus/nvd3")
              (commit (string-append "v" version))))
        (sha256
         (base32 "1fcqsac233616h52fm1xmj9z9glbi3hfxr6z617kix8p1vcp6g3g"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/nvd3"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/build")))
           (copy-recursively dist targetdir)))))
    (native-inputs
     `(("source" ,source)))
    (home-page "https://nvd3.org/")
    (synopsis "A reusable charting library written in d3.js")
    (description "This project is an attempt to build re-usable charts and chart components for d3.js without taking away the power that d3.js gives you. This is a very young collection of components, with the goal of keeping these components very customizable, staying away from your standard cookie cutter solutions.")
    (license license:asl2.0)))

(define-public javascript-d3js
  (package
    (name "javascript-d3js")
    (version "3.5.17")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/d3/d3/releases/download/v"
                            version "/d3.zip"))
        (sha256
         (base32 "1adxr5q90k0x6ndknjayq718wmqirc7j4hpfqw38wmcknmag429q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (name "d3js")
                (unzip (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip"))
                (targetdir (string-append out "/share/genenetwork2/javascript/" name))
                (source (assoc-ref %build-inputs "source")))
           (invoke unzip source)
           (install-file "d3.js" targetdir)
           (install-file "d3.min.js" targetdir)
           (install-file "LICENSE" (string-append out "/share/doc/d3js-" ,version))))))
    (native-inputs
     `(("source" ,source)
       ("unzip" ,unzip)))
    (home-page "https://d3js.org/")
    (synopsis "JavaScript library for visualizing data")
    (description "D3.js is a JavaScript library for manipulating documents based
on data.  D3 helps you bring data to life using HTML, SVG, and CSS.  D3's
emphasis on web standards gives you the full capabilities of modern browsers
without tying yourself to a proprietary framework, combining powerful
visualization components and a data-driven approach to DOM manipulation.")
    (license license:bsd-3)))

(define-public javascript-jquery
  (package
   (inherit web-jquery)
   (name "javascript-jquery")
   (version "1.10.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/jquery/jquery.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1k4prjchm9iyvnzcfdsrm12sp8vggjgl8kdvjzfc8a5w97wzbbpr"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (targetdir (string-append out "/share/genenetwork2/javascript/jquery"))
               (source (assoc-ref %build-inputs "source"))
               (dist (string-append source "/dist")))
          (copy-recursively dist targetdir)))))))

(define-public javascript-d3js-4
  (package
    (inherit javascript-d3js)
    (version "4.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/d3/d3/releases/download/v"
                            version "/d3.zip"))
        (sha256
         (base32 "06yqgyvkpvh0lg7rsh4hjzq72fylkd8ziqcf7yhgy510x0mqla19"))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (name "d3js")
                (unzip (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip"))
                (targetdir (string-append out "/share/genenetwork/javascript/" name))
                (source (assoc-ref %build-inputs "source")))
           (invoke unzip source)
           (install-file "d3.js" targetdir)
           (install-file "d3.min.js" targetdir)
           (install-file "LICENSE" (string-append out "/share/doc/d3js-" ,version))))))))

(define-public javascript-d3js-multi
  (package
    (name "javascript-d3js-multi")
    (version "1.0.1") ; Feb, 21, 2017
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/d3/d3-selection-multi/releases"
                            "/download/v" version "/d3-selection-multi.zip"))
        (sha256
         (base32 "0k89n15ggpzsvf7qflmsagkrrhigk6nksdyks0ccx3453gizbb4q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (name "d3js-multi")
                (unzip (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip"))
                (targetdir (string-append out "/share/genenetwork/javascript/" name))
                (source (assoc-ref %build-inputs "source")))
           (invoke unzip source)
           (install-file "d3-selection-multi.js" targetdir)
           (install-file "d3-selection-multi.min.js" targetdir)
           (install-file "LICENSE" (string-append out "/share/doc/" ,name "-" ,version))))))
    (native-inputs
     `(("source" ,source)
       ("unzip" ,unzip)))
    (home-page "https://d3js.org/")
    (synopsis "Multi-value syntax for d3-selection and d3-transition")
    (description "This module adds multi-value syntax to selections and
transitions, allowing you to set multiple attributes, styles or properties
simultaneously with more concise syntax.")
    (license license:bsd-3)))

(define-public javascript-canvas-toblob
  (let ((commit "f1a01896135ab378aa5c0118eadd81da55e698d8") ; May 26, 2016
        (revision "1"))
    (package
      (name "javascript-canvas-toblob")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/eligrey/canvas-toBlob.js")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1ly7qnavf9h26sgynccf00hf2ybdwyn0kvnl7i3milz3big02qdm"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out (assoc-ref %outputs "out"))
                  (name "canvas-toblob")
                  (targetdir (string-append out "/share/genenetwork/javascript/" name))
                  (source (assoc-ref %build-inputs "source")))
             (install-file (string-append source "/canvas-toBlob.js") targetdir)
             (install-file (string-append source "/LICENSE.md")
                           (string-append out "/share/doc/" ,name "-" ,version))))))
      (native-inputs
       `(("source" ,source)))
      (home-page "https://github.com/eligrey/canvas-toBlob.js/")
      (synopsis "canvas.toBlob() implementation")
      (description "canvas-toBlob.js implements the standard HTML5
@code{canvas.toBlob()} and @code{canvas.toBlobHD()} methods in browsers that do
not natively support it.")
      (license license:expat))))

(define-public javascript-filesaver
  (package
    (name "javascript-filesaver")
    (version "2.0.2") ; May 14, 2019
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/eligrey/FileSaver.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ij3vmv8n2ia9kbyih3g479rj68xrsiq7l9s29vv1bdmmk41lpf3"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/filesaver"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://github.com/eligrey/FileSaver.js")
    (synopsis "HTML5 saveAs() FileSaver implementation")
    (description "FileSaver.js is the solution to saving files on the
client-side, and is perfect for web apps that generates files on the client.")
    (license license:expat)))

(define-public js-filesaver-1.3.2
  (package
    (inherit js-filesaver)
    (name "js-filesaver")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eligrey/FileSaver.js")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02f34yr56i5fyppna52bhn6i9wj0zhvj4vp9vkg9v74yls1hdczz"))))
    (arguments
     `(#:javascript-files '("FileSaver.js")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-unminified-version
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "FileSaver.js"
                           (string-append (assoc-ref outputs "out")
                                          "/share/javascript"))
             #t)))))))

(define-public javascript-underscore
  (package
    (name "javascript-underscore")
    (version "1.12.0") ; Nov 24, 2020
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jashkenas/underscore.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1scvwaiziqfpi106z2bf5s9czd8mn8p8scgiyckrgjhaxw40acdh"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/underscore"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/underscore.js") targetdir)
           (install-file (string-append source "/underscore-min.js") targetdir)
           (install-file (string-append source "/underscore-min.js.map") targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://underscorejs.org")
    (synopsis "Utility-belt library for JavaScript")
    (description
     "Underscore is a JavaScript library that provides a whole mess of useful
functional programming helpers without extending any built-in objects.")
    (license license:expat)))

(define-public js-underscore
  (package
    (inherit javascript-underscore)
    (name "js-underscore")
    (arguments
     `(#:javascript-files '("underscore.js")))
    (build-system minify-build-system)))

(define-public javascript-smart-time-ago
  (let ((commit "055c3858997b12d44bf06c0fb9eb5847002cf973")
        (revision "1"))
    (package
      (name "javascript-smart-time-ago")
      (version (git-version "0.1.5" revision commit)) ; Feb, 21, 2014
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/pragmaticly/smart-time-ago.git")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0fj5vf3s3rj7ywvx1s4gh6z0yljn9ax75y2ikf1d41c0lzaxdpyd"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out (assoc-ref %outputs "out"))
                  (targetdir (string-append out "/share/genenetwork2/javascript/smart-time-ago"))
                  (source (assoc-ref %build-inputs "source")))
             (install-file (string-append source "/lib/timeago.js") targetdir)))))
      (native-inputs `(("source" ,source)))
      (home-page "http://pragmaticly.github.com/smart-time-ago/")
      (synopsis "jQuery library to update the relative timestamps")
      (description
       "Smart Time Ago is a little jQuery library to update the relative
timestamps in your document intelligently.  (e.g \"3 hours ago\").")
      (license license:expat))))

(define-public js-smart-time-ago
  (package
    (inherit javascript-smart-time-ago)
    (name "js-smart-time-ago")
    (arguments
     `(#:javascript-files '("lib/timeago.js")))
    (build-system minify-build-system)))

(define-public javascript-colorbox
  (package
    (name "javascript-colorbox")
    (version "1.4.36") ; Feb. 11, 2014
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jackmoore/colorbox.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0yjikn0mc1cmhcl3wbd5pjspi6n75swazsahm616xlra73qpagfn"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/colorbox"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/jquery.colorbox.js") targetdir)
           (install-file (string-append source "/jquery.colorbox-min.js") targetdir)
           ))))
    (native-inputs `(("source" ,source)))
    (home-page "http://www.jacklmoore.com/colorbox/")
    (synopsis "Lightweight customizable lightbox plugin for jQuery")
    (description
     "Colorbox is a lightweight customizable lightbox plugin for jQuery.")
    (license license:expat)))

(define-public javascript-bootstrap
  (package
   (inherit web-bootstrap-3)
   (name "javascript-bootstrap")
   (version "3.3.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/twbs/bootstrap.git")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1d70mhxx2pwp0hghjynz17a2s3vj6wj1mdg0sg9dgwkmlnbxv7jy"))))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (targetdir (string-append out "/share/genenetwork2/javascript/bootstrap"))
               (source (assoc-ref %build-inputs "source"))
               (docs (string-append source "/docs/assets/css/src"))
               (dist (string-append source "/dist")))
          (copy-recursively dist targetdir)
          (install-file (string-append docs "/docs.css")
                        (string-append targetdir "/css"))))))
   (native-inputs `(("source" ,source)))))

(define-public js-colorbox
  (package
    (inherit javascript-colorbox)
    (name "js-colorbox")
    (arguments
     `(#:javascript-files '("jquery.colorbox.js")))
    (build-system minify-build-system)))

(define-public javascript-nouislider
  (package
    (name "javascript-nouislider")
    (version "8.0.2") ; July 6, 2015
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/leongersen/noUiSlider.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ngfll2hr9w2q4869n0prfn66lcfyjshvhq4pgi0lb63xla8asfp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/nouislider"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/distribute")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://refreshless.com/nouislider/")
    (synopsis "Javascript range slider")
    (description
     "Nouislider is a lightweight JavaScript range slider with full touch support.")
    (license license:expat)))

(define-public js-nouislider
  (package
    (inherit javascript-nouislider)
    (name "js-nouislider")
    (arguments
     `(#:javascript-files '("distribute/nouislider.js")))
    (build-system minify-build-system)))

(define-public javascript-chroma
  (package
    (name "javascript-chroma")
    (version "1.1.1") ; Aug. 15, 2015
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gka/chroma.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "184bd2cddd9b5ynj2ygr5p7xkrrxnmnjyq5ljyw6g4aqqk4pb0mr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/chroma"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/chroma.js") targetdir)
           (install-file (string-append source "/chroma.min.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://vis4.net/chromajs/")
    (synopsis "Javascript library for color conversions")
    (description
     "chroma.js is a small-ish zero-dependency JavaScript library for all kinds
of color conversions and color scales.  chroma.js can also help you generate
nice colors using various methods, for instance to be used in color palette for
maps or data visualization.")
    (license (list license:bsd-3 license:asl2.0))))

(define-public js-chroma
  (package
    (inherit javascript-chroma)
    (name "js-chroma")
    (arguments
     `(#:javascript-files '("chroma.js")))
    (build-system minify-build-system)))

(define-public javascript-jscolor
  (package
    (name "javascript-jscolor")
    (version "2.0.5") ; April 26, 2018
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://jscolor.com/release/"
                           (version-major+minor version)
                           "/jscolor-" version ".zip"))
       (sha256
        (base32
         "1mjsr7vvark3glipvk5xxg3xsi88swwlkfmlkykwcwnrgw2hyq53"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/jscolor"))
                (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
                (source (assoc-ref %build-inputs "source")))
           (invoke unzip source)
           (install-file "jscolor.js" targetdir)
           ))))
    (native-inputs
     `(("source" ,source)
       ("unzip" ,unzip)))
    (home-page "http://jscolor.com")
    (synopsis "Javascript web color picker")
    (description
     "jscolor is a web color picker component that aims to be super easy both
for developers to install and for the end users to use.")
    (license license:gpl3)))

(define-public js-jscolor
  (package
    (inherit javascript-jscolor)
    (name "js-jscolor")
    (arguments
     `(#:javascript-files '("jscolor.js")
       #:phases
       (modify-phases %standard-phases
         ;; unpacking the zipbomb breaks javascript-jscolor
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "unzip" source))))))
    (build-system minify-build-system)))

(define-public javascript-jstat
  (package
    (name "javascript-jstat")
    (version "1.9.1") ; Sept. 2, 2019
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jstat/jstat.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0v69rkvgjykg2msjgpg38raypw7293jawlfxnicn86p2x0c57pzz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/jstat"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "http://jstat.github.io/")
    (synopsis "Javascript statistical library")
    (description
     "jStat provides native javascript implementations of statistical functions.
jStat provides more functions than most libraries, including the weibull,
cauchy, poisson, hypergeometric, and beta distributions.  For most
distributions, jStat provides the pdf, cdf, inverse, mean, mode, variance, and
a sample function, allowing for more complex calculations.")
    (license license:expat)))

(define-public js-jstat
  (package
    (inherit javascript-jstat)
    (name "js-jstat")
    (arguments `(#:javascript-files '("dist/jstat.js")))
    (build-system minify-build-system)))

(define-public javascript-ckeditor ; version 4
  (package
    (name "javascript-ckeditor")
    (version "4.13.0") ; Sept. 26, 2019
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cdn.ckeditor.com/" version
                           "/standard/ckeditor.js"))
       (file-name (string-append "ckeditor-" version ".js"))
       (sha256
        (base32
         "0cvf1qdva5h2dh8y10c9v7dxrd82siswxx7h6cq0mf46ssjdygd0"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir
                  (string-append out "/share/genenetwork2/javascript/ckeditor"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p targetdir)
           (copy-file source (string-append targetdir "/ckeditor.js"))))))
    (native-inputs `(("source" ,source)))
    (home-page "https://ckeditor.com/")
    (synopsis "Smart WYSIWYG HTML editor")
    (description
     "CKEditor is a proven, enterprise-grade WYSIWYG HTML editor with wide
browser compatibility, including legacy browsers.
@enumerate
@item Paste from Word and Excel, spell check, accessibility checker, tables.
@item Autocomplete, mentions, widgets, code snippets, emoji plugins.
@item Full control over content: HTML filtering, view source mode.
@item Great accessibility: WCAG 2.0 AA and Section 508 compliant.
@item Long-term support (LTS) until 2023.
@end enumerate")
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:mpl1.1)))) ; Any of these three

(define-public javascript-parsley
  (package
    (name "javascript-parsley")
    (version "2.9.1") ; May 28, 2019
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/guillaumepotier/Parsley.js")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0kb2hmnj9rry68qqmj0m4wjk0syrb0i3c2n31sr24y9m8pdr8nck"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/parsley"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://parsleyjs.org/")
    (synopsis "Javascript validation library")
    (description
     "Parsley is a javascript library which allows you to write in English your
requirements inside your form HTML tags.  It can also automatically detects
your forms' modifications and adapts its validation accordingly.")
    (license license:expat)))

(define-public javascript-jszip
  (package
    (name "javascript-jszip")
    (version "3.2.2") ; July 4, 2019
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Stuk/jszip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "185n341knjqmmpzs7gjv9xd2id2vy85l9in2q058hsz36lz3rpjz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/jszip"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (install-file (string-append dist "/jszip.js") targetdir)
           (install-file (string-append dist "/jszip.min.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://stuk.github.io/jszip/")
    (synopsis "JSZip is a javascript library for creating, reading and editing .zip files, with a lovely and simple API.")
    (description
     "JSZip is a javascript library for creating, reading and editing .zip files, with a lovely and simple API.")
    (license license:expat)))

(define-public javascript-shapiro-wilk
  (package
    (name "javascript-shapiro-wilk")
    (version "1.0.0") ; Sept 27, 2012
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rniwa/js-shapiro-wilk")
             (commit "451e6341832dc42de026b1d18ac0282da7f72a1e")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1azy5h30m7j86bq2zvwpkm5l4jgypbrp3bjlwa4z6la2h0c4l2nz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/shapiro-wilk"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/shapiro-wilk.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://stuk.github.io/jszip/")
    (synopsis "Shapiro-Wilk Test in JavaScript (ported from R)")
    (description
     "Shapiro-Wilk Test in JavaScript (ported from R)")
    (license license:expat)))

(define-public javascript-underscore-string
  (package
    (name "javascript-underscore-string")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/esamattis/underscore.string")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dzqcq4hb8wvhmaqh8bxmjgj3hh22hgjdvcrw9ipxsm1v74bl78i"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/underscore-string"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (install-file (string-append dist "/underscore.string.min.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "http://esamattis.github.com/underscore.string/")
    (synopsis "String manipulation helpers for javascript")
    (description
     "Javascript lacks complete string manipulation operations. This is an attempt to fill that gap. List of build-in methods can be found for example from Dive Into JavaScript. Originally started as an Underscore.js extension but is a full standalone library nowadays.")
    (license license:expat)))

(define-public js-parsley
  (package
    (inherit javascript-parsley)
    (name "js-parsley")
    (arguments `(#:javascript-files '("dist/parsley.js")))
    (build-system minify-build-system)))

(define-public javascript-plotly
  (package
    (name "javascript-plotly")
    (version "1.53.0") ; Mar 31, 2020
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/plotly/plotly.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0m8bfj8dxxmxa30z14w7r9ij5r2050zybkf6r85kkzbjyabgpfb4"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/plotly"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://plot.ly/javascript/")
    (synopsis "Javascript charting library")
    (description
     "Built on top of d3.js and stack.gl, plotly.js is a high-level, declarative
charting library. plotly.js ships with 20 chart types, including 3D charts,
statistical graphs, and SVG maps.")
    (license license:expat)))

(define-public javascript-typeahead
  (package
    (name "javascript-typeahead")
    (version "0.11.1") ; April 27, 2015
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/twitter/typeahead.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1aj070x1l43zyrv9r6az5mc6r8zzfc7ajqavi1fw85a7wgcwchcy"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/typeahead"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://twitter.github.io/typeahead.js")
    (synopsis "Javascript library for building typeaheads")
    (description
     "Inspired by twitter.com's autocomplete search functionality,
@code{typeahead.js} is a flexible JavaScript library that provides a strong
foundation for building robust typeaheads.
The @code{typeahead.js} library consists of 2 components: the suggestion engine,
Bloodhound, and the UI view, Typeahead.  The suggestion engine is responsible
for computing suggestions for a given query.  The UI view is responsible for
rendering suggestions and handling DOM interactions.  Both components can be
used separately, but when used together, they can provide a rich typeahead
experience.")
    (license license:expat)))

(define-public js-typeahead
  (package
    (inherit javascript-typeahead)
    (name "js-typeahead")
    (arguments `())
    (build-system minify-build-system)))

(define-public js-md5
  (package
    (name "js-md5")
    (version "2.10.0") ; Dec. 28, 2017
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blueimp/JavaScript-MD5")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "017y9gkhzdj7qa5fw8r4kkp9kf5s9cnz4y17dskkrwd2dnwwxg3z"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/javascript/"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/js/md5.min.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://github.com/blueimp/JavaScript-MD5")
    (synopsis "JavaScript MD5 implementation")
    (description
     "JavaScript MD5 implementation. Compatible with server-side environments like Node.js, module loaders like RequireJS, Browserify or webpack and all web browsers.")
    (license license:expat)))

(define d3-tip-js
  (let ((version "0.9.1"))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://cdnjs.cloudflare.com/ajax/libs/d3-tip/"
            version
            "/d3-tip.js"))
      (file-name "d3-tip.js")
      (sha256
       (base32
        "1y6vq5vs46k806cj1d6nr8z220ndl9bsycd01d8xfmhkwn3rn0x2")))))

(define-public javascript-d3-tip
  (package
    (name "javascript-d3-tip")
    (version "0.9.1") ; May 10, 2018
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/caged/d3-tip")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1v7kw5j73kgjidxzra2hi33wgcyqr4l26wybp1czmv1kndxqfyzw"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/d3-tip"))
                (source (assoc-ref %build-inputs "source"))
                (distfile (assoc-ref %build-inputs "distfile")))
           (mkdir-p targetdir)
           (copy-file distfile (string-append targetdir "/d3-tip.js"))
           (copy-file (string-append source "/examples/example-styles.css")
                      (string-append targetdir "/d3-tip.css"))))))
    (native-inputs `(("source" ,source)
                     ("distfile" ,d3-tip-js)
                     ))
    (home-page "http://labratrevenge.com/d3-tip")
    (synopsis "Tooltips for d3.js visualizations")
    (description
     "Tooltips for d3.js visualizations.")
    (license license:expat)))

(define-public javascript-purescript-genome-browser
  (package
    (name "javascript-purescript-genome-browser")
    (version "0.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BonfaceKilz/Dist-Purestript-Genome-Browser")
             (commit "93d45a55ca5053bb87b6d4627ae5c7d973c046ea")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mp47ms696pijd95agvfg32anyrg6rx0gxkhzm8423pqcfqfad5s"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/purescript-genome-browser"))
                (source (assoc-ref %build-inputs "source")))
           (copy-recursively source targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://github.com/chfi/purescript-genetics-browser")
    (synopsis "Dist files for the purescript genetics browser")
    (description
     "Dist files for the purescript genetics browser")
    (license license:expat)))

(define-public javascript-jquery-ui
  (package
    (name "javascript-jquery-ui")
    (version "1.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://jqueryui.com/resources/download/jquery-ui-" version ".zip"))
       (sha256
        (base32
         "0xsppsyjqvq8d00dklpj595czwh9fqlkyr1ab6hmk08pdnbn4dzp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/jquery-ui"))
                (source (assoc-ref %build-inputs "source")))
           (invoke (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")
                   source)
           (copy-recursively (string-append "jquery-ui-" ,version) targetdir)
           #t))))
    (native-inputs
     `(("source" ,source)
       ("unzip" ,unzip)))
    (home-page "https://jqueryui.com/")
    (synopsis "user interface interactions built on top of jquery")
    (description
     "jQuery UI is a curated set of user interface interactions, effects,
widgets, and themes built on top of the jQuery JavaScript Library.")
    (license license:expat)))

(define-public js-jquery-ui
  (package
    (inherit javascript-jquery-ui)
    (name "js-jquery-ui")
    (arguments `(#:javascript-files '("ui/jquery-ui.js")))
    (build-system minify-build-system)))

(define-public js-popper
  (package
    (name "js-popper")
    (version "2.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/popperjs/popper-core")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lzy981p9nja2l3xa2zvals6q31v3bzpxxa85yn9pm7wkj3vglf2"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("src/popper.js")))
    (home-page "https://popper.js.org/")
    (synopsis "Tooltip and popover positioning engine")
    (description
     "Given an element, such as a button, and a tooltip element describing it,
Popper will automatically put the tooltip in the right place near the button.")
    (license license:expat)))

(define-public js-popper-1.12.9
  (package
    (name "js-popper")
    (version "1.12.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/popperjs/popper-core")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j7gfb7agvvzxv09vq8yr5h04pls5a8md2qw62qyn20112p0hdzs"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("dist/umd/popper.js")))
    (home-page "https://popper.js.org/")
    (synopsis "Tooltip and popover positioning engine")
    (description
     "Given an element, such as a button, and a tooltip element describing it,
Popper will automatically put the tooltip in the right place near the button.")
    (license license:expat)))

(define-public javascript-zxcvbn
  (package
   (name "javascript-zxcvbn")
   (version "4.4.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://cdnjs.cloudflare.com/ajax/libs/zxcvbn/"
           version
           "/zxcvbn.js"))
     (file-name (string-append name ".js"))
     (sha256
      (base32 "0jhpzvgr3aly7m5wmcz759ssx6kgm3rrh2ax5psrgws5s8azqxv6"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((source (assoc-ref %build-inputs "source"))
               (out (assoc-ref %outputs "out"))
               (targetdir (string-append out "/share/genenetwork2/javascript/zxcvbn")))
          (mkdir-p targetdir)
          (copy-file source (string-append targetdir "/zxcvbn.js"))
          ))))
   (native-inputs `(("source" ,source)))
   (home-page "https://www.usenix.org/conference/usenixsecurity16/technical-sessions/presentation/wheeler")
   (synopsis "Low-Budget Password Strength Estimation")
   (description
    "zxcvbn is a password strength estimator inspired by password crackers. Through pattern matching and conservative estimation, it recognizes and weighs 30k common passwords, common names and surnames according to US census data, popular English words from Wikipedia and US television and movies, and other common patterns like dates, repeats (aaa), sequences (abcd), keyboard patterns (qwertyuiop), and l33t speak.")
   (license license:expat)))

(define-public javascript-jquery-cookie
  (package
   (name "javascript-jquery-cookie")
   (version "1.3.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/carhartl/jquery-cookie.git")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0a94jzk83pbsw3wd8pfa3g61yx2najhik0dzqr4nf1rsgmqbqhgc"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (let* ((out (assoc-ref %outputs "out"))
             (targetdir (string-append out "/share/genenetwork2/javascript/jquery-cookie"))
             (source (assoc-ref %build-inputs "source")))
        (begin
          (use-modules (guix build utils))
          (install-file (string-append source "/jquery.cookie.js")
                        targetdir)))))
   (native-inputs `(("source" ,source)))
   (home-page "http://jasonmayes.com/projects/twitterApi/")
   (synopsis "A simple, lightweight jQuery plugin for reading, writing and deleting cookies.")
   (description "A simple, lightweight jQuery plugin for reading, writing and deleting cookies. No longer maintained, superseded by JS Cookie: https://github.com/js-cookie/js-cookie")
   (license license:expat)))


;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32916 on why font-awesome
;; cannot be upstreamed
(define-public javascript-font-awesome
  (package
    (name "javascript-font-awesome")
    (version "5.15.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FortAwesome/Font-Awesome")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jin0qlf5lv4l9gj8qc1pp34mxyvyj6gma4qnjqiah1bzcfn635l"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/fontawesome"))
                (source (assoc-ref %build-inputs "source"))
		(js-dir (string-append source "/js"))
                (css-dir (string-append source "/css"))
                (fonts-dir (string-append source "/webfonts")))
           (copy-recursively css-dir (string-append targetdir "/css"))
	   (copy-recursively js-dir (string-append targetdir "/js"))
           (copy-recursively fonts-dir
                             (string-append targetdir "/webfonts"))))))
    (native-inputs `(("source" ,source)))
    (home-page "https://fontawesome.com/")
    (synopsis "Font that contains a rich iconset")
    (description "Font Awesome is a full suite of pictographic icons for easy scalable
vector graphics.")
    (license license:silofl1.1)))


(define-public javascript-htmx
  (package
    (name "javascript-htmx")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/bigskysoftware/htmx")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14m9wan8sp5lzblfzbi1hln621p7ld3npajxrhq1a19zm5bcrz3y"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("dist/htmx.js")))
    (home-page "https://htmx.org/")
    (synopsis "High Power Tools for HTML")
    (description
     "htmx allows you to access AJAX, CSS Transitions, WebSockets and Server Sent Events directly in HTML, using attributes, so you can build modern user interfaces with the simplicity and power of hypertext")
    (license license:expat)))
