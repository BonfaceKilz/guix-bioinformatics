;; Bioinformatics module

(define-module (gn past genenetwork1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix graph)
  #:use-module (guix scripts graph)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xml)
  #:use-module (past packages python)
  #:use-module (past packages web)
  #:use-module (gn packages bioinformatics)
  #:use-module (gn packages databases)
  #:use-module (gn packages elixir)
  #:use-module (gn packages gemma)
  #:use-module (gn packages javascript)
  #:use-module (gn packages phewas)
  #:use-module (gn packages python)
  #:use-module (gn packages python24)
  #:use-module (gn packages statistics)
  #:use-module (gn packages web)
  #:use-module (srfi srfi-1))


(define-public python2-qtlreaper
  (let ((commit "442c217b90393380a8634ff8636b44992f5c53dd"))
  (package
    (name "python2-qtlreaper")
    (version (string-append "1.11-gn2-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   ;; (url "https://github.com/genenetwork/genenetwork2.git")
                   (url "https://github.com/pjotrp/QTLreaper.git")
                   (commit commit)))
             (file-name (string-append name "-" (string-take commit 7)))
             (sha256
              (base32
               "1rrbm1ap2zzyjxmrs9aa1d18sgiba5dhj1fmkl7wmab06jv3j1hm"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test' really!
    (home-page "http://qtlreaper.sourceforge.net/")
    (synopsis "Scan expression data for QTLs")
    (description
     "Batch-oriented version of WebQTL. It requires, as input,
expression data from members of a set of recombinant inbred lines and
genotype information for the same lines.  It searches for an
association between each expression trait and all genotypes and
evaluates that association by a permutation test.  For the permutation
test, it performs only as many permutations as are necessary to define
the empirical P-value to a reasonable precision. It also performs
bootstrap resampling to estimate the confidence region for the
location of a putative QTL.")
    (license license:gpl2+))))

(define-public python24-qtlreaper
  (let ((commit "442c217b90393380a8634ff8636b44992f5c53dd"))
    (package
      (name "python24-qtlreaper")
      (version (git-version "1.11" "gn1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       ;; (url "https://github.com/genenetwork/genenetwork2.git")
                       (url "https://github.com/pjotrp/QTLreaper.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1rrbm1ap2zzyjxmrs9aa1d18sgiba5dhj1fmkl7wmab06jv3j1hm"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2.4
         #:phases
         (modify-phases %standard-phases
           (replace 'add-install-to-pythonpath
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (setenv "PYTHONPATH"
                       (string-append (site-packages inputs outputs) ":"
                                      (getenv "PYTHONPATH")))))
           (delete 'sanity-check)       ; Not applicable to python-2.4
           (add-after 'unpack 'make-max-markername-size-larger
             (lambda _
               (substitute* "Src/dataset.c"
                 (("512") "2048"))))
           (replace 'check
             (lambda* (#:key tests? inputs outputs #:allow-other-keys)
               (when tests?
                 (setenv "PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                                     (getenv "PYTHONPATH")))
                 (invoke "python" "test/runtest.py")))))))
      (native-inputs
       `(("python24-setuptools" ,python24-setuptools)))
      (home-page "http://qtlreaper.sourceforge.net/")
      (synopsis "Scan expression data for QTLs")
      (description
       "Batch-oriented version of WebQTL. It requires, as input,
expression data from members of a set of recombinant inbred lines and
genotype information for the same lines.  It searches for an
association between each expression trait and all genotypes and
evaluates that association by a permutation test.  For the permutation
test, it performs only as many permutations as are necessary to define
the empirical P-value to a reasonable precision. It also performs
bootstrap resampling to estimate the confidence region for the
location of a putative QTL.")
      (license license:gpl2+))))

(define-public genenetwork1
  (let ((commit "acf65ac9ae4be395c07c1629758f7408bf4eab5f") ; June 3, 2020
        (revision "2"))
    (package
      (name "genenetwork1")
      (version (git-version "0.0.0" revision commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/genenetwork/genenetwork1.git")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0xmmmjyvh80yd8b0cjrwpdmxl8k9zj5ly65r2g9aygx74njsp4fi"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("ghostscript" ,ghostscript)
         ("graphviz" ,graphviz)
         ("wget" ,wget)))
      (propagated-inputs
       `(("python" ,python-2.4)
         ("httpd-with-mod-python" ,httpd22-with-mod-python)
         ("python-direct" ,python24-direct-gn)
         ("python-htmlgen-GN1" ,python24-htmlgen-GN1)
         ("python-json-GN1" ,python24-json-GN1)
         ("python-mysqlclient" ,python24-mysqlclient)
         ("python-numarray" ,python24-numarray)
         ("python-piddle" ,python24-piddle)
         ("python-pp-GN1" ,python24-pp-GN1)
         ("python-pyx" ,python24-pyx)
         ("python-pyxlwriter" ,python24-pyxlwriter)
         ("python-qtlreaper" ,python24-qtlreaper)
         ("python-rpy2" ,python24-rpy2)
         ("python-svg-GN1" ,python24-svg-GN1)))
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (add-after 'patch-generated-file-shebangs 'patch-more-files
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((piddle (assoc-ref inputs "python-piddle")))
                 (substitute* "web/webqtl/networkGraph/networkGraphUtils.py"
                   (("/usr/local/bin/neato") (which "neato"))
                   (("/usr/local/bin/circo") (which "circo"))
                   (("/usr/local/bin/twopi") (which "twopi"))
                   (("/usr/local/bin/fdp") (which "fdp"))
                   (("ps2pdf") (which "ps2pdf")))
                 (substitute* "web/webqtl/maintainance/addRif.py"
                   (("rm ") (string-append (which "rm") " "))
                   (("wget ") (string-append (which "wget") " "))
                   (("gunzip") (which "gunzip")))
                 (substitute* "web/webqtl/misc/editHtmlPage.py"
                   (("/bin/cp") (which "cp")))
                 (substitute* "web/webqtl/geneWiki/AddGeneRIFPage.py"
                   (("touch") (which "touch")))
                 (substitute* '("web/webqtl/maintainance/addRif.py"
                                "web/webqtl/networkGraph/networkGraphPage.py"
                                "web/webqtl/utility/svg.py")
                   (("/usr/bin/(env )?python") (which "python")))
                 (substitute* "web/webqtl/base/webqtlConfigLocal.py"
                   (("PythonPath.*")
                    (string-append "PythonPath = '" (which "python") "'\n"))
                   (("PIDDLE_FONT_PATH.*/lib")
                    (string-append "PIDDLE_FONT_PATH = '" piddle "/lib"))))
               #t))
           (add-after 'patch-generated-file-shebangs 'changes-for-deployed-service
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out    (assoc-ref outputs "out")))
                 (substitute* "web/webqtl/base/webqtlConfigLocal.py"
                   ;; Where GN1 is located: (GNROOT)
                   (("/gnshare/gn") out)
                   ;; Where the database is located: (sql_host)
                   (("tux01.uthsc.edu") "127.0.0.1"))
                   (substitute* '("web/webqtl/maintainance/QTL_Reaper_cal_lrs.py")
                     (("128\\.169\\.5\\.59") "localhost"))
                 ;; This directory (TMPDIR) is expected to be writable by apache.
                 ;; /tmp is private inside the container.
                 (symlink "/tmp" "web/tmp")
                 ;; IMGDIR is expected to be writable.
                 (symlink "/tmp" "web/image")
                 (system "chmod 0777 web/tmp")
                 ;; More writable locations:
                 (substitute* (list "web/webqtl/collection/ExportSelectionDetailInfoPage.py"
                                    "web/webqtl/pairScan/DirectPlotPage.py"
                                    "web/webqtl/updateTrait/DataUpdatePage.py"
                                    "web/webqtl/utility/Plot.py")
                   (("/gnshare/gn/web/debug_file.txt") "/tmp/debug_file.txt"))
                 ;; We mount the genotypes folder (GENODIR) in the OS-config and
                 ;; provide the symlink to that location from the package.
                 ;; And now the directory is magically available!
                 (symlink "/gnshare/gn/web/genotypes" "web/genotypes")
                 (substitute* "web/webqtl/base/webqtlConfig.py"
                   (("http://www.genenetwork.org") "http://gn1-test.genenetwork.org"))
                 ;; Inside the gn1 container, there's some conflict when
                 ;; importing the user module, therefore, as a hack, rename
                 ;; user to useralt
                 (mkdir "web/webqtl/useralt")
                 (copy-recursively "web/webqtl/user" "web/webqtl/useralt")
                 (substitute* '("web/webqtl/main.py")
                   (("from user import") "from useralt import"))
                 #t)))
           (add-after 'unpack 'use-local-links
             (lambda _
               (substitute* '("web/javascript/menu_items.js"
                              "web/webqtl/maintainance/updateMenuJS.py")
                 (("http://(www|gn1).genenetwork.org") ""))

               ;; Move this file out of the way while patching files.
               (rename-file "web/infoshare/manager/MDB-Free/index.html"
                            "web/infoshare/manager/MDB-Free/index.htm")
               (substitute* (cons*
                              "web/webqtl/base/indexBody.py"
                              "web/webqtl/submitTrait/BatchSubmitPage.py"
                              (find-files "web" "\\.html"))
                 ((".*base href.*") "")
                 (("(HREF|href)=\\\"http://(www.)?genenetwork.org")
                  "href=\""))
               ;; Move this file back to its original location.
               (rename-file "web/infoshare/manager/MDB-Free/index.htm"
                            "web/infoshare/manager/MDB-Free/index.html")

               (substitute* (cons*
                              "web/humanCross.html"
                              "web/webqtl/base/indexBody.py"
                              "web/whats_new.html"
                              (find-files "web/dbdoc" "\\.html"))
                 (("src=\\\"http://www.genenetwork.org") "src=\""))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (home-page "http://www.genenetwork.org/webqtl/main.py")
      (synopsis
       "Combined database and data analysis software resource for systems genetics")
      (description "GeneNetwork is a group of linked data sets and tools used to
study complex networks of genes, molecules, and higher order gene function and
phenotypes.  GeneNetwork combines more than 25 years of legacy data generated by
hundreds of scientists together with sequence data (SNPs) and massive
transcriptome data sets (expression genetic or eQTL data sets).  The
@dfn{quantitative trait locus} (QTL) mapping module that is built into GN is
optimized for fast on-line analysis of traits that are controlled by
combinations of gene
variants and environmental factors.  GeneNetwork can be used to study humans,
mice (BXD, AXB, LXS, etc.), rats (HXB), Drosophila, and plant species (barley
and Arabidopsis).  Most of these population data sets are linked with dense
genetic maps (genotypes) that can be used to locate the genetic modifiers that
cause differences in expression and phenotypes, including disease susceptibility.")
      (license license:agpl3+))))
