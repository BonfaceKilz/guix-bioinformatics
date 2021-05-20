;; Bioinformatics module

(define-module (gn packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages)
  #:use-module (gn packages python)
  #:use-module (gn packages twint)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web))

(define-public contra
  (package
    (name "contra")
    (version "2.0.6")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "mirror://sourceforge/contra-cnv/CONTRA.V"
            (version-major+minor version) "/CONTRA.v" version ".tar.gz"))
      (sha256
       (base32
        "0agpcm2xh5f0i9n9sx1kvln6mzdksddmh11bvzj6bh76yw5pnw91"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (delete-file "BEDTools.v2.11.2.tar.gz") #t))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("python" ,python-2)
       ("r" ,r)
       ;; ("r-dnacopy" ,r-dnacopy) <-- missing in Pjotr's tree
       ("bedtools" ,bedtools)
       ("samtools" ,samtools)))
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build) ; We can use Guix's BEDtools instead.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/contra")))
               (copy-recursively "scripts" (string-append bin "/scripts"))
               (install-file "contra.py" bin)
               (install-file "baseline.py" bin)
               ;; There's only a pre-built PDF available.
               (install-file "CONTRA_User_Guide.2.0.pdf" doc))
             #t)))))
    (home-page "http://contra-cnv.sourceforge.net/")
    (synopsis "Tool for copy number variation (CNV) detection for targeted
resequencing data")
    (description "CONTRA is a tool for copy number variation (CNV) detection
for targeted resequencing data such as those from whole-exome capture data.
CONTRA calls copy number gains and losses for each target region with key
strategies including the use of base-level log-ratios to remove GC-content
bias, correction for an imbalanced library size effect on log-ratios, and the
estimation of log-ratio variations via binning and interpolation.  It takes
standard alignment formats (BAM/SAM) and outputs in variant call format
(VCF 4.0) for easy integration with other next generation sequencing analysis
package.")
    (license license:gpl3+)))

(define boost-delly
  (package (inherit boost)
    (name "boost-delly")
    (version "1.57.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "0rs94vdmg34bwwj23fllva6mhrml2i7mvmlb11zyrk1k5818q34i"))))))

(define-public delly
  (package
    (name "delly")
    (version "0.7.2")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/tobiasrausch/delly/archive/v"
            version ".tar.gz"))
      (sha256
       (base32 "173mmg43dbxqkyq0kiffz63xbmggr2kzd55mwxci9yfh5md1zprn"))
      (patches (list (search-patch "delly-use-system-libraries.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (inputs
     `(("boost" ,boost-delly) ; Use version 1.57.0 instead.
       ("htslib" ,htslib)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)))
    (arguments
     `(#:tests? #f ; There are no tests to run.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (replace 'install
           (lambda _
             (let ((bin (string-append (assoc-ref %outputs "out") "/bin")))
               (install-file "src/cov" bin)
               (install-file "src/delly" bin)
               (install-file "src/extract" bin)
               (install-file "src/iover" bin)
               (install-file "src/stats" bin)))))))
    (home-page "https://github.com/tobiasrausch/delly")
    (synopsis "Integrated structural variant prediction method")
    (description "Delly is an integrated structural variant prediction method
that can discover and genotype deletions, tandem duplications, inversions and
translocations at single-nucleotide resolution in short-read massively parallel
sequencing data.  It uses paired-ends and split-reads to sensitively and
accurately delineate genomic rearrangements throughout the genome.  Structural
variants can be visualized using Delly-maze and Delly-suave.")
    (license license:gpl3)))

(define-public freec
  (package
    (name "control-freec")
    (version "8.7")
    (source (origin
      (method url-fetch)
      (uri "http://bioinfo-out.curie.fr/projects/freec/src/FREEC_Linux64.tar.gz")
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "12sl7gxbklhvv0687qjhml1z4lwpcn159zcyxvawvclsrzqjmv0h"))))
    (build-system gnu-build-system)
    ;; The source code's filename indicates only a 64-bit Linux build.
    ;; We need to investigate whether this is true.
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There's no configure phase because there are no external
         ;; dependencies.
         (delete 'configure)
         ;; There are no tests.
         (delete 'check)
         (replace
          'unpack
          (lambda* (#:key source #:allow-other-keys)
            (and
             (zero? (system* "mkdir" "source"))
             (with-directory-excursion "source"
               (zero? (system* "tar" "xvf" source))))))
         (replace
          'build
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "source"
              (zero? (system* "make")))))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "source/freec" bin)))))))
    (home-page "http://bioinfo-out.curie.fr/projects/freec/")
    (synopsis "Tool for detection of copy-number changes and allelic imbalances
(including LOH) using deep-sequencing data")
    (description "Control-FREEC automatically computes, normalizes, segments
copy number and beta allele frequency (BAF) profiles, then calls copy number
alterations and LOH.  The control (matched normal) sample is optional for whole
genome sequencing data but mandatory for whole exome or targeted sequencing
data.  For whole genome sequencing data analysis, the program can also use
mappability data (files created by GEM). ")
    (license license:gpl2+)))

(define-public plink2
  (package
    (name "plink2")
    (version "1.90b3")
    (source
     (origin
      (method url-fetch)
      ;; https://github.com/chrchang/plink-ng/archive/v1.90b3.tar.gz
       (uri (string-append
             "https://github.com/chrchang/plink-ng/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "03fzib1al5qkr9vxv63wxmv6y2pfb1rmir0h8jpi72r87hczqjig"))
       (patches (list (search-patch "plink-ng-Makefile-zlib.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
                 (lambda _
                   (zero? (system* "make" "-f" "Makefile.std"))
                   ))
        (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (install-file "plink2" bin)
                      #t))))))
    (inputs
     `(("zlib" ,zlib)
       ("openblas" ,openblas)
       ;; ("atlas" ,atlas)
       ;; ("lapack" ,lapack)
       ("gfortran" ,gfortran)
       ))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://www.cog-genomics.org/plink2")
    (synopsis "Whole genome association analysis toolset")
    (description
     "PLINK is a whole genome association analysis toolset, designed to
perform a range of basic, large-scale analyses in a computationally efficient
manner.  The focus of PLINK is purely on analysis of genotype/phenotype data,
so there is no support for steps prior to this (e.g. study design and
planning, generating genotype or CNV calls from raw data).  Through
integration with gPLINK and Haploview, there is some support for the
subsequent visualization, annotation and storage of results.")
    ;; Code is released under GPLv2, except for fisher.h, which is under
    ;; LGPLv2.1+
    (license (list license:gpl2 license:lgpl2.1+))))

(define-public plink-ng-gn
  (let ((commit "5d1db4313ba0cc976562da233db4aced78975d10"))
  (package
    (name "plink-ng-gn")
    (version (string-append "1.90b3-" commit )) ; Aug 11, 2016
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/plink-ng.git")
                   (commit commit)))
             (file-name (string-append name "-" commit))
             (sha256
              (base32
               "1366li3ks9076bblvd1rpzkjq4j8f8f08lhga4c1ckrkil3xww4m"))))
            ;; no longer (patches (list (search-patch "plink-ng-Makefile-zlib-git.patch")))))
    (inputs
     `(("zlib" ,zlib)
       ("openblas" ,openblas)
       ;; ("atlas" ,atlas) ; openblas replaces atlas
       ("lapack" ,lapack)  ; lapack is disabled in GUIX openblas
       ;; ("gfortran" ,gfortran)
       ;; ("python" ,python-2)   ;; for tests - currently disabled
       ))
    (native-inputs
     `(("unzip" ,unzip)))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target. Some of the python-based tests fail
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
                 (lambda _
                   (zero? (system* "make" "-f" "Makefile.guix"))
                   ))
        (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (install-file "plink2" bin)
                      #t))))))
    (home-page "https://www.cog-genomics.org/plink2")
    (synopsis "Whole genome association analysis toolset")
    (description
     "PLINK is a whole genome association analysis toolset, designed to
perform a range of basic, large-scale analyses in a computationally efficient
manner.  The focus of PLINK is purely on analysis of genotype/phenotype data,
so there is no support for steps prior to this (e.g. study design and
planning, generating genotype or CNV calls from raw data).  Through
integration with gPLINK and Haploview, there is some support for the
subsequent visualization, annotation and storage of results.")
    (license license:gpl3+))))

(define-public pindel
  (package
   (name "pindel")
   (version "0.2.5b8")
   (source (origin
     (method git-fetch)
     (uri (git-reference
            (url "https://github.com/genome/pindel.git")
            (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "16a32fbgv1n58nfcxa1nyphrdrad80sgpinfa9p028n6plwycpww"))))
   (build-system gnu-build-system)
   (inputs
    `(("samtools" ,samtools)
      ("htslib" ,htslib)
      ("zlib" ,zlib)))
   (native-inputs
    `(("cppcheck" ,cppcheck)
      ("python" ,python-2)
      ("perl" ,perl)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'configure) ; There is no configure phase.
        ;; The build phase needs to run 'make' twice for the reasons described
        ;; below.
        (replace 'build
          (lambda* (#:key inputs #:allow-other-keys)
            ;; The first run creates a Makefile.local file.  Make will report
            ;; the failure to find Makefile.local, but we can ignore this error.
            (system* "make" (string-append "SAMTOOLS=" (assoc-ref inputs "samtools")))
            ;; The second run actually compiles the program.  Now Makefile.local
            ;; is available, and we should treat an exiting make with an error as
            ;; a true error.
            (invoke "make")))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "src/pindel" bin)
              (install-file "src/pindel2vcf" bin)
              (install-file "src/pindel2vcf4tcga" bin)
              (install-file "src/sam2pindel" bin))))
        ;; There are multiple test targets, so in order to run all
        ;; tests, we must run the separate make targets.
        (replace 'check
          (lambda _
            (for-each (lambda (target)
                        (invoke "make" target))
                      '("acceptance-tests" "coverage-tests" "cppcheck"
                        "functional-tests" "regression-tests")))))))
   (home-page "https://github.com/genome/pindel")
   (synopsis "Structural variants detector for next-gen sequencing data")
   (description "Pindel can detect breakpoints of large deletions, medium sized
insertions, inversions, tandem duplications and other structural variants at
single-based resolution from next-gen sequence data.  It uses a pattern growth
approach to identify the breakpoints of these variants from paired-end short
reads.")
   (license license:gpl3+)))

(define-public varscan
  (package
    (name "varscan")
    (version "2.4.1")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/dkoboldt/varscan/releases/download/v"
            version "/VarScan.v" version ".source.jar"))
      (sha256
       (base32 "0y45ympkza7qwcbcisg006286pwjbr5978n03hx5nvl09f0mapk8"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; build.xml does not exist
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda _
             (mkdir "source")
             (chdir "source")
             ;; Unpack the Java archive containing the source files.
             (invoke "jar" "xf" (assoc-ref %build-inputs "source"))
             ;; Remove existing compiled output.
             (with-directory-excursion "net/sf/varscan/"
               (for-each (lambda (file)
                           (delete-file file))
                         (find-files "." "^.java$" #:directories? #f)))
             #t))
         (replace 'build
           (lambda _
             ;; Compile the source files.
             (with-directory-excursion "net/sf/varscan/"
               (for-each (lambda (file)
                           (invoke "javac" file))
                         (find-files "." ".java$" #:directories? #f)))
             ;; Construct the new Java archive.
             (apply invoke "jar" "cfm"
                    (string-append "varscan-" ,version ".jar")
                    "META-INF/MANIFEST.MF"
                    (find-files "net/sf/varscan" ".java$"))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (string-append (assoc-ref outputs "out")
                                      "/share/java/varscan/")))
              (install-file (string-append "varscan-" ,version ".jar") out))
            #t)))))
    (home-page "https://dkoboldt.github.io/varscan/")
    (synopsis "Variant detection in massively parallel sequencing data")
    (description "Variant detection in massively parallel sequencing data.")
    ;; Free for non-commercial use by academic, government, and
    ;; non-profit/not-for-profit institutions
    (license license:non-copyleft)))

(define-public edirect-gn
  (deprecated-package "edirect-gn" edirect))

;; TODO: Unbundle zlib, bamtools, tclap
(define-public sniffles
  (package
   (name "sniffles")
   (version "1.0.11")
   (source (origin
     (method git-fetch)
     (uri (git-reference
            (url "https://github.com/fritzsedlazeck/Sniffles.git")
            (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0rkwqn1ycckfzrg2wdid4cqahq8q2jmmgi7vvl8qxgpsihqfbq0j"))))
   (build-system cmake-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (install-file (string-append "../source/bin/sniffles-core-"
                                           ,version "/sniffles")
                            (string-append out "/bin")))
            #t))
        (replace 'check
          (lambda _
            (with-directory-excursion "../source/test_set"
              (for-each make-file-writable (find-files "."))
              (invoke (string-append "../bin/sniffles-core-" ,version "/sniffles")
                      "-m" "reads_region.bam" "-v" "test.vcf")))))))
   (native-inputs
    `(("zlib" ,zlib)))
   (home-page "https://github.com/fritzsedlazeck/Sniffles")
   (synopsis "Structural variation caller using third generation sequencing")
   (description
    "Sniffles is a structural variation caller using third generation sequencing
(PacBio or Oxford Nanopore).  It detects all types of SVs (10bp+) using evidence
from split-read alignments, high-mismatch regions, and coverage analysis.")
   (license license:expat)))

;; TODO: Unbundle Complete-Striped-Smith-Waterman-Library
(define-public ngmlr
  (package
   (name "ngmlr")
   (version "0.2.7")
   (source (origin
     (method git-fetch)
     (uri (git-reference
            (url "https://github.com/philres/ngmlr.git")
            (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0lmsy8w0kxbyfnrln7lxgmnx3d82sv2b20n2yw5742rvfhq1v31n"))))
   (build-system cmake-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'patch-source-shebangs 'patch-more-tools
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((bed (assoc-ref inputs "bedtools"))
                  (sam (assoc-ref inputs "samtools")))
              (substitute* (find-files "test" "\\.sh$")
                (("bedtools") (string-append bed "/bin/bedtools"))
                (("samtools") (string-append sam "/bin/samtools")))
              #t)))
        (replace 'check
          (lambda _
            (with-directory-excursion "../source"
              (invoke "sh" "test/test_travis.sh")))))))
   (native-inputs
    `(("bedtools" ,bedtools)
      ("samtools" ,samtools)))
   (inputs
    `(("zlib" ,zlib)))
   (home-page "https://github.com/philres/ngmlr")
   (synopsis "Long-read mapper designed to align PacBio or Oxford Nanopore")
   (description
    "NGMLR is a long-read mapper designed to align PacBio or Oxford Nanopore
(standard and ultra-long) to a reference genome with a focus on reads that span
structural variations.")
   (license license:expat)))

(define-public svim
  (package
   (name "svim")
   (version "1.2.0")
   (source (origin
     (method git-fetch)
     (uri (git-reference
            (url "https://github.com/eldariont/svim.git")
            (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "08j02in9jbq41b67dna1apnc3y30i37v44d1khml1xlx0iga720s"))))
   (build-system python-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda _
            (invoke "python3" "-m" "unittest" "discover" "-s" "src/"))))))
   (propagated-inputs
    `(("python-matplotlib" ,python-matplotlib)
      ("python-numpy" ,python-numpy)
      ("python-pysam" ,python-pysam)
      ("python-scipy" ,python-scipy)
      ("minimap2" ,minimap2)
      ("ngmlr" ,ngmlr)
      ("samtools" ,samtools)))
   (home-page "https://github.com/eldariont/svim")
   (synopsis "Structural Variant Identification Method using Long Reads")
   (description
    "SVIM (pronounced SWIM) is a structural variant caller for long reads.  It
is able to detect, classify and genotype five different classes of structural
variants.  Unlike existing methods, SVIM integrates information from across the
genome to precisely distinguish similar events, such as tandem and interspersed
duplications and novel element insertions.")
   (license license:gpl3)))

(define-public bamaddrg
  (let ((commit "3fccbf057eef21f6304fade6c306c5bb64158865") ; May 26, 2012
        (revision "1"))
    (package
      (name "bamaddrg")
      (version (git-version "0.0.0" revision commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ekg/bamaddrg.git")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "14hq66cc7f4cssagb6079fmd2i6hfr9vmpcw5vi5kzsqr3ifc5yk"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           ;; The Makefile wants to vendor bamtools' source so we mimic it.
           (replace 'build
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((bam (assoc-ref inputs "bamtools")))
                  (apply invoke
                         `("g++" "-O3"
                           ,(string-append "-I" bam "/include/bamtools")
                           ,(string-append "-L" bam "/lib/libbamtools.a")
                           "bamaddrg.cpp" "-o" "bamaddrg" "-lbamtools" "-lz")))
                #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "bamaddrg" bin)
               #t))))))
      (native-inputs
       `(("bamtools" ,bamtools)))
      (inputs
       `(("zlib" ,zlib)))
      (home-page "https://github.com/ekg/bamaddrg")
      (synopsis "Adds read groups to input BAM files, streams BAM output on stdout")
      (description
       "This is intended for use \"fixing up\" RG tags on the fly so that they
reflect the source file from which the aligment originated from.  This allows
the \"safe\" merging of many files from many individuals into one stream,
suitable for input into downstream processing systems such as freebayes (
population variant detector).")
      (license #f)))) ; no license listed

(define-public qctool
  (let ((changeset "73662f5f6e1e6efe75796bc64e342fb5d5d35e54") ; May 30, 2019
        (revision "1"))
    (package
      (name "qctool")
      (version (string-append "2.0.5-" revision "." (string-take changeset 7)))
      (source
        (origin
          (method hg-fetch)
          (uri (hg-reference
                 (url "https://bitbucket.org/gavinband/qctool")
                 (changeset changeset)))
          (file-name (string-append name "-" version "-checkout"))
          (sha256
           (base32 "0lcir6jdw1gsi1l0yrsyqgrb8dryxxw3gyncfx5bx34qbhd6f5dv"))))
      (build-system waf-build-system)
      (arguments
       `(#:python ,python-2
         #:tests? #f ; no check command
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'rename-waf
             (lambda _
               (rename-file "waf-1.5.18" "waf")
               #t)))))
      (native-inputs
       `(("readline" ,readline)
         ("zlib" ,zlib)))
      (inputs
       `(("lapack" ,lapack)
         ("openblas" ,openblas)))
      (home-page "https://www.well.ox.ac.uk/~gav/qctool_v2/")
      (synopsis "Quality control and analysis of gwas datasets")
      (description
       "QCTOOL is a command-line utility program for manipulation and quality
control of gwas datasets and other genome-wide data.  QCTOOL can be used
@enumerate
@item To compute per-variant and per-sample QC metrics.
@item To filter out samples or variants.
@item To merge datasets in various ways.
@item To convert dataset between file formats. (In particular QCTOOL can read
and write BGEN files, including full support for the BGEN v1.2 format that has
been used for the UK Biobank imputed data full release).
@item To manipulate datasets in various ways - e.g. by updating data fields or
aligning alleles to a reference sequence based on information in a strand file.
@item To annotate variants with information from BED files, sequence from FASTA
files, or with genetic map positions.
@item To compute LD metrics between variants.
@item To compare genotypes for individuals typed or imputed or phased in
different datasets.
@item To compute between-sample relatedness and principal components.
@item To compute 'genetic risk predictor' scores.
@end enumerate")
      (license (license:x11-style "https://www.boost.org/LICENSE_1_0.txt")))))

(define-public rn6-assembly-error-app
  (package
   (name "rn6-assembly-error-app")
   (version "0.12")
   (source (origin
     (method git-fetch)
     (uri (git-reference
            (url "https://github.com/chen42/rn6_assembly_error_app.git")
            (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0ilmn6w0l17041dlizf4dy4pqn26k7956k7fjx4fvssb525g4gi6"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out       (assoc-ref %outputs "out"))
               (targetdir (string-append out "/share/" ,name))
               (app       (string-append out "/bin/" ,name))
               (Rbin      (string-append (assoc-ref %build-inputs "r-min")
                                         "/bin/Rscript"))
               (convert   (string-append (assoc-ref %build-inputs "imagemagick")
                                         "/bin/convert"))
               (cp        (string-append (assoc-ref %build-inputs "coreutils")
                                         "/bin/cp"))
               (source    (assoc-ref %build-inputs "source")))
          (copy-recursively source targetdir)
          (substitute* (string-append targetdir "/server.r")
            ;; This version is ideal for deploying with the included PNGs.
            ;; But we want all of them, so we use a local copy in shepherd's $HOME.
            ;;(("./pngs") (string-append targetdir "/pngs"))
            (("./pngs") "/home/shepherd/rn6app/pngs")
            (("cp") cp)
            (("convert") convert))
          (mkdir-p (string-append out "/bin"))
          (call-with-output-file app
            (lambda (port)
              (format port
"#!~a
library(shiny)
setwd(\"~a\")
runApp(launch.browser=0, port=4202)~%\n"
              Rbin targetdir)))
          (chmod app #o555)
          #t))))
   (native-inputs `(("source" ,source)))
   (inputs
    `(("coreutils" ,coreutils-minimal)
      ("imagemagick" ,imagemagick)
      ("r-min" ,r-minimal)))
   (propagated-inputs
    `(("freetype" ,freetype)
      ("r" ,r)
      ("r-ggplot2" ,r-ggplot2)
      ("r-shiny" ,r-shiny)))
   (home-page "http://rn6err.opar.io/")
   (synopsis "Display potential assembly errors in rn6")
   (description
    "Display potential assembly errors in rn6.")
   (license license:expat)))

(define-public bxd-power-calculator-app
  (let ((commit "7cdd73daa9a7aa79af1de04dc314c325f9706fb8")
        (revision "1"))
    (package
      (name "bxd-power-calculator-app")
      (version (git-version "0.7" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/Dashbrook/BXD_power_calculator_app/")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "0vdfilzy78jalkh9w9xxvarnzgnlaz943crmhlds8bcrvwbmf6yh"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out       (assoc-ref %outputs "out"))
                  (targetdir (string-append out "/share/" ,name))
                  (app       (string-append out "/bin/" ,name))
                  (Rbin      (string-append (assoc-ref %build-inputs "r-min")
                                            "/bin/Rscript"))
                  (datasets  (assoc-ref %build-inputs "datasets"))
                  (source    (assoc-ref %build-inputs "source")))
             (copy-recursively source targetdir)
             (mkdir-p (string-append out "/bin"))
             (call-with-output-file app
               (lambda (port)
                 (format port
"#!~a
library(shiny)
setwd(\"~a\")
runApp(launch.browser=0, port=3978)~%\n"
                 Rbin targetdir)))
             (chmod app #o555)
             (substitute* (string-append targetdir "/server.R")
               (("read.csv.*")
               (string-append "read.csv(url(\"file://" datasets "\"), header = TRUE)\n")))
             #t))))
      (native-inputs `(("source" ,source)))
      (propagated-inputs
       `(("r" ,r)
         ("r-data-table" ,r-data-table)
         ("r-dt" ,r-dt)
         ("r-dplyr" ,r-dplyr)
         ("r-ggplot2" ,r-ggplot2)
         ("r-rcolorbrewer" ,r-rcolorbrewer)
         ("r-shiny" ,r-shiny)))
      (inputs
       `(("r-min" ,r-minimal)
         ;; Also available from ipfs
         ;; ipfs get Qma3LWJBoks77btTmp6rn6jGSBcuBoPgcPCmofY2RRKEKf
         ("datasets" ,(origin
                        (method url-fetch)
                        (uri "https://web.archive.org/web/20191016132922/http://individual.utoronto.ca/D_Ashbrook/Effect_size_analysis_heritability_28th_Nov_2018_recalc.csv")
                        (sha256
                         (base32
                          "1ldr9infavd0vak8n8ry9smcnrir3xgs1bahmmx7n2csx4n6qx2x"))))))
      (home-page "https://dashbrook1.shinyapps.io/bxd_power_calculator_app/")
      (synopsis "Visualize probability (beta) of detecting a QTL")
      (description
       "The BXD power app seeks to provide a quick and easy graphical interface
for users to calculate the theortical power to detect an effect in a two parent
recombinant inbred population.  A power calculator such as this is needed as all
grants require a calculation of the applications power to detect the effect of
interest, and this app can provide values and figures for applicants to use.")
        (license license:gpl3))))

(define-public singlecellrshiny
  (let ((commit "bdca74f4819d11e8fe7b15d9ab91b853f6542f7a")
        (revision "3"))
    (package
     (name "singlecellrshiny")
     (version (git-version "0.0.0" revision commit))
     (source (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/genenetwork/singleCellRshiny")
              (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rxj933s9p9r7358vnp15f7ag6c0j65r4hgr8kyirfhmp1i8xdlw"))))
     (build-system trivial-build-system)
     (arguments
      `(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let* ((out       (assoc-ref %outputs "out"))
                 (targetdir (string-append out "/share/" ,name))
                 (app       (string-append out "/bin/" ,name))
                 (Rbin      (string-append (assoc-ref %build-inputs "r-min")
                                           "/bin/Rscript"))
                 (top1001   (assoc-ref %build-inputs "RobTop1001.csv"))
                 (celltypes (assoc-ref %build-inputs "CellTypes_RGC_Master_08Dec2018.csv"))
                 (800-H1    (assoc-ref %build-inputs "800-H1-H20-RNA-Seq.csv"))
                 (source    (assoc-ref %build-inputs "source")))
            (copy-recursively source targetdir)
            (substitute* (string-append targetdir "/app.R")
              ;; As seen in https://github.com/genenetwork/singleCellRshiny/commit/6b2a344dd0d02f65228ad8c350bac0ced5850d05.patch
              (("library\\(DT\\)") "library(DT)\nlibrary(multtest)"))
            (substitute* (string-append targetdir "/global.R")
              (("800-H1-H20-RNA-Seq-SingleCell-Retina-OMRF-03-29-19_FPKM_v2_SiamakPlay.csv") 800-H1)
              (("CellTypes_RGC_Master_08Dec2018.csv") celltypes)
              (("RobTop1001.csv") top1001)
              ;; As seen in https://github.com/genenetwork/singleCellRshiny/commit/6b2a344dd0d02f65228ad8c350bac0ced5850d05.patch
              (("dim\\(sc.object.1") "dim(sc.object"))
            (mkdir-p (string-append out "/bin"))
            (call-with-output-file app
              (lambda (port)
                (format port
"#!~a
library(shiny)
setwd(\"~a\")
runApp(launch.browser=0, port=4208)~%\n"
                Rbin targetdir)))
            (chmod app #o555)
            #t))))
     (inputs
      `(("r-min" ,r-minimal)
        ("RobTop1001.csv"
         ,(origin
            (method url-fetch)
            (uri "https://archive.org/download/celltypesrgcmaster08dec2018/RobTop1001.csv")
            (file-name "RobTop1001.csv")
            (sha256
             (base32 "0pa73kc1p8417sjvvvhp9xsbh2h8g7h85pnmm16mnv4wjalhq0gn"))))
        ("CellTypes_RGC_Master_08Dec2018.csv"
         ,(origin
            (method url-fetch)
            (uri "https://archive.org/download/celltypesrgcmaster08dec2018/CellTypes_RGC_Master_08Dec2018.csv")
            (file-name "CellTypes_RGC_Master_08Dec2018.csv")
            (sha256
             (base32 "0y411968np1f5g21iym9xc9yj5c1jsn94rpkwkxh9pw2z43gvghn"))))
        ("800-H1-H20-RNA-Seq.csv"
         ,(origin
            (method url-fetch)
            (uri "https://archive.org/download/celltypesrgcmaster08dec2018/800-H1-H20-RNA-Seq-SingleCell-Retina-OMRF-03-29-19_FPKM_v2_SiamakPlay.csv")
            (file-name "800-H1-H20-RNA-Seq-SingleCell-Retina-OMRF-03-29-19_FPKM_v2_SiamakPlay.csv")
            (sha256
             (base32 "1b1y4lfs8drypm04i1rypbmk67rdqgs27nfh05pwnv3sja2nanam"))))))
     (propagated-inputs
      `(("r" ,r)
        ("r-dt" ,r-dt)
        ("r-multtest" ,r-multtest)
        ("r-seurat" ,r-seurat)
        ("r-shiny" ,r-shiny)))
     (home-page "http://singlecell.opar.io/")
     (synopsis "RNA sequencing data analysis")
     (description
      "This is the R-Shiny programs to run some basic single cell RNA sequencing
(scRNA-seq) data analysis.")
     (license license:agpl3))))

(define-public seqwish
  (package
    (name "seqwish")
    (version "0.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ekg/seqwish.git")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1gp72cmi13hbkmwwhgckmxkbx8w644jc5l6dvvvxdbl6sk8xsi5r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((sdsl-lite      (assoc-ref inputs "sdsl-lite"))
                   (sufsort        (assoc-ref inputs "sufsort"))
                   (bsort          (assoc-ref inputs "bsort"))
                   (mmap_allocator (assoc-ref inputs "mmap-allocator"))
                   (tayweeargs     (assoc-ref inputs "tayweeargs-source"))
                   (gzipreader     (assoc-ref inputs "gzipreader-source"))
                   (mmmultimap     (assoc-ref inputs "mmmultimap-source"))
                   (iitii          (assoc-ref inputs "iitii-source"))
                   (ips4o          (assoc-ref inputs "ips4o-source")))
               (apply invoke "g++" "-o" "seqwish"
                      "-O3" "-g" "-std=c++14" "-fopenmp"
                      "-latomic" "-lz"
                      (string-append "-I" sdsl-lite "/include")
                      (string-append "-I" sdsl-lite "/include/sdsl")
                      (string-append "-I" bsort "/include")
                      (string-append "-I" tayweeargs)
                      (string-append "-I" gzipreader)
                      (string-append "-I" mmmultimap "/src")
                      (string-append "-I" iitii "/src")
                      (string-append "-I" mmap_allocator "/include")
                      (string-append "-I" ips4o)
                      (append
                        (find-files "src" ".")
                        (list
                          (string-append sdsl-lite "/lib/libsdsl.so")
                          (string-append sufsort "/lib/libdivsufsort.so")
                          (string-append sufsort "/lib/libdivsufsort64.so")
                          (string-append mmap_allocator "/lib/libmmap_allocator.a")
                          (string-append bsort "/lib/libbsort.a")))))))
         (replace 'check
           (lambda _
             ;; Add seqwish to the PATH for the tests.
             (setenv "PATH" (string-append (getcwd) ":" (getenv "PATH")))
             (with-directory-excursion "test"
               (invoke "make"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "seqwish" (string-append out "/bin")))
             #t)))))
    (inputs
     `(("bsort" ,ekg-bsort)
       ("mmap-allocator" ,ekg-mmap-allocator)
       ("openmpi" ,openmpi)
       ("sdsl-lite" ,sdsl-lite)
       ("sufsort" ,libdivsufsort)
       ("zlib" ,zlib)))
    (native-inputs
     `(("prove" ,perl)
       ("tayweeargs-source" ,(origin
                               (method git-fetch)
                               (uri (git-reference
                                      (url "https://github.com/Taywee/args.git")
                                      (commit "3de44ec671db452cc0c4ef86399b108939768abb")))
                               (file-name "tayweeargs-source-for-seqwish")
                               (sha256
                                (base32
                                 "1v8kq1gvl5waysrfp0s58881rx39mnf3ifdsl6pb3y3c4zaki2xh"))))
       ("gzipreader-source" ,(origin
                               (method git-fetch)
                               (uri (git-reference
                                      (url "https://github.com/gatoravi/gzip_reader.git")
                                      (commit "0ef26c0399e926087f9d6c4a56067a7bf1fc4f5e")))
                               (file-name "gzipreader-source-for-seqwish")
                               (sha256
                                (base32
                                 "1wy84ksx900840c06w0f1mgzvr7zsfsgxq1b0jdjh8qka26z1r17"))))
       ("mmmultimap-source" ,(origin
                               (method git-fetch)
                               (uri (git-reference
                                      (url "https://github.com/ekg/mmmultimap.git")
                                      (commit "88c734c36563048b0f3acc04dd8856f19e02b75f")))
                               (file-name "mmmultimap-source-for-seqwish")
                               (sha256
                                (base32
                                 "06mnf3bd32s3ngxkl573ylg2qsvlw80r1ksdwamx3fzxa1a5yls0"))))
       ("iitii-source" ,(origin
                          (method git-fetch)
                          (uri (git-reference
                                 (url "https://github.com/ekg/iitii.git")
                                 (commit "85209e07a3ee403fb6557387a7f897cd76be4406")))
                          (file-name "iitii-source-for-seqwish")
                          (sha256
                           (base32
                            "0sszvffkswf89nkbjmjg3wjwqvy2w0d3wgy3ngy33ma4sy4s025s"))))
       ("ips4o-source" ,(origin
                          (method git-fetch)
                          (uri (git-reference
                                 (url "https://github.com/SaschaWitt/ips4o.git")
                                 (commit "bff3ccf0bf349497f2bb10f825d160b792236367")))
                          (file-name "ips4o-source-for-seqwish")
                          (sha256
                           (base32
                            "0yjfvrkiwgmy5cn0a7b9j8jwc3zp0l8j4dl5n0jgz68pdnhlp96h"))))))
    (home-page "https://github.com/ekg/seqwish")
    (synopsis "Alignment to variation graph inducer")
    (description "Seqwish implements a lossless conversion from pairwise
alignments between sequences to a variation graph encoding the sequences and
their alignments.  As input we typically take all-versus-all alignments, but the
exact structure of the alignment set may be defined in an application specific
way.  This algorithm uses a series of disk-backed sorts and passes over the
alignment and sequence inputs to allow the graph to be constructed from very
large inputs that are commonly encountered when working with large numbers of
noisy input sequences.  Memory usage during construction and traversal is
limited by the use of sorted disk-backed arrays and succinct rank/select
dictionaries to record a queryable version of the graph.")
    (license license:expat)))

(define ekg-bsort
  (let ((commit "c3ab0d3308424030e0a000645a26d2c10a59a124")
        (revision "1"))
    (package
      (name "bsort")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ekg/bsort.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0dgpflzcp3vdhbjwbjw347czi86gyk73hxcwjdqnaqh5vg61bdb6"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f ; no test target
         #:out-of-source? #f
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "bin/bsort" (string-append out "/bin"))
                 (install-file "src/bsort.hpp" (string-append out "/include"))
                 (install-file "lib/libbsort.a" (string-append out "/lib")))
               #t)))))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:gpl2))))

(define ekg-mmap-allocator
  (let ((commit "ed61daf094de1c2e1adbe8306287ad52da5f0264")
        (revision "1"))
    (package
      (name "mmap-allocator")
      (version (git-version "0.10.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ekg/mmap_allocator.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1f30b2kpwwzh6333s0qi5samk458ghbnvyycf6rwx6n6j7xswhbw"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (delete 'configure) ; no configure script
           (add-before 'install 'pre-install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "Makefile"
                  (("HEADERS=") "HEADERS=mmappable_vector.h ")
                   (("/usr") out))
                 (mkdir-p (string-append out "/lib"))
                 (mkdir (string-append out "/include"))
                 #t))))
         #:test-target "test"))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:lgpl2.0+)))) ; README just says "lpgl".

;; TODO: Unbundle BBHash, concurrentqueue, parallel-hashmap zstr
(define-public graphaligner
  (package
   (name "graphaligner")
   (version "1.0.10")
   (source (origin
     (method url-fetch)
     (uri (string-append "https://github.com/maickrau/GraphAligner/files/"
                         "3879798/GraphAligner.tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0sk0cfjw44wslmlgplzwcqi0w4862vhf75p4x6syalvyi34pw3ck"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; no tests
      #:make-flags '("all")
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-source
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((sdsl (assoc-ref inputs "sdsl-lite")))
              (substitute* "makefile"
                (("VERSION .*") (string-append "VERSION = " ,version "\n"))))
            #t))
        (delete 'configure) ; no configure phase
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (for-each
                (lambda (program)
                  (install-file program (string-append out "/bin")))
                (find-files "bin" "."))
              (for-each
                (lambda (header)
                  (install-file header (string-append out "/include")))
                (find-files "src" "\\.h(pp)?$")))
            #t)))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("protobuf" ,protobuf "static")
      ("sdsl-lite" ,sdsl-lite)
      ("sparsehash" ,sparsehash)
      ("zlib" ,zlib "static")))
   (inputs
    `(("boost" ,boost-static)
      ("jemalloc" ,jemalloc)
      ("libdivsufsort" ,libdivsufsort)
      ("mummer" ,mummer)
      ("protobuf" ,protobuf)
      ("zlib" ,zlib)))
   (home-page "https://github.com/maickrau/GraphAligner")
   (synopsis "Seed-and-extend program for aligning  genome graphs")
   (description "Seed-and-extend program for aligning long error-prone reads to
genome graphs.  For a description of the bitvector alignment extension
algorithm, see
@url{https://academic.oup.com/bioinformatics/advance-article/doi/10.1093/bioinformatics/btz162/5372677
here}.")
   (license license:expat)))

(define-public mummer
  (package
    (name "mummer")
    (version "4.0.0beta2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/mummer4/mummer/releases/"
                            "download/v" version "/mummer-" version ".tar.gz"))
        (sha256
         (base32
          "14qvrmf0gkl4alnh8zgxlzmvwc027arfawl96i7jk75z33j7dknf"))))
    (build-system gnu-build-system)
    (inputs
     `(("gnuplot" ,gnuplot)
       ("perl" ,perl)))
    (home-page "http://mummer.sourceforge.net/")
    (synopsis "Efficient sequence alignment of full genomes")
    (description "MUMmer is a versatil alignment tool for DNA and protein sequences.")
    (license license:artistic2.0)))

(define-public diagnostic-slider
  (let ((commit "514d65d4982133e4869e578c5553fced4c6d506c")
        (revision "1"))
    (package
      (name "diagnostic-slider")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/sens/diagnostic-slider")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "04g8if32g8syg6v0bd3jjn05i3d394nx8i3ccl0883p8mlmdvlmx"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out       (assoc-ref %outputs "out"))
                  (targetdir (string-append out "/share/" ,name))
                  (app       (string-append out "/bin/" ,name))
                  (Rbin      (string-append (assoc-ref %build-inputs "r-min")
                                            "/bin/Rscript"))
                  (source    (assoc-ref %build-inputs "source")))
             (copy-recursively source targetdir)
             (mkdir-p (string-append out "/bin"))
             (call-with-output-file app
               (lambda (port)
                 (format port
"#!~a
library(shiny)
setwd(\"~a\")
runApp(launch.browser=0, port=4206)~%\n"
                         Rbin targetdir)))
               (chmod app #o555)
               #t))))
        (native-inputs
         `(("source" ,source)))
        (inputs
         `(("r-min" ,r-minimal)))
        (propagated-inputs
         `(("r" ,r)
           ("r-shiny" ,r-shiny)))
        (home-page "https://github.com/sens/diagnostic-slider")
        (synopsis "")
        (description
         "")
        (license #f))))

(define-public clustalw
  (package
   (name "clustalw")
   (version "2.1")
   (source (origin
     (method url-fetch)
     (uri "http://www.clustal.org/download/current/clustalw-2.1.tar.gz")
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "11llyj08liq0bg6vqan8728qjrbam3xhna2wd6g8rzdbhydhalp0"))))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
       (add-after
        'install 'post-install
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let* ((out     (assoc-ref outputs "out")))
            (rename-file (string-append out "/bin/clustalw2")
                         (string-append out "/bin/clustalw"))
                 ))))))
   (build-system gnu-build-system)
   (home-page "http://www.clustal.org/")
   (synopsis "")
   (description
    "")
   (license #f)))

(define-public bh20-seq-resource
  (let ((commit "ae4cb3c2cf7103bbc84f52618bb755d7ce25775b")
        (revision "3"))
    (package
      (name "bh20-seq-resource")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/arvados/bh20-seq-resource")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1k0gsz4yc8l5znanzd094g2jp40ksbpa9667zr31ayrjx6labz02"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (substitute* "setup.py"
                      (("py-dateutil") "python-dateutil"))
                    #t))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-arvados-python-client" ,python-arvados-python-client)
         ("python-dateutil" ,python-dateutil)
         ("python-flask" ,python-flask)
         ("python-magic" ,python-magic)
         ("python-pyyaml" ,python-pyyaml)
         ("python-pycurl" ,python-pycurl)
         ("python-pyshex" ,python-pyshex)
         ("python-redis" ,python-redis)
         ("python-ruaml.yaml" ,python38-ruaml.yaml-0.15.76)
         ("clustalw" ,clustalw)
         ("python-schema-salad" ,python-schema-salad)
         ("python-twint" ,python-twint)
         ;; and for the service
         ("python" ,python)
         ("gunicorn" ,gunicorn)))
      (native-inputs
       `(("git" ,(@ (gnu packages version-control) git))
         ("python-oauth2client" ,python-oauth2client)
         ("python-pytest" ,python-pytest-4)
         ("python-pytest-runner" ,python-pytest-runner-2)
         ("python-uritemplate" ,python-uritemplate)))
      (home-page "https://github.com/arvados/bh20-seq-resource")
      (synopsis
       "Tool to upload SARS-CoV-19 sequences and service to kick off analysis")
      (description "This repository provides a sequence uploader for the
COVID-19 Virtual Biohackathon's Public Sequence Resource project.  You can use
it to upload the genomes of SARS-CoV-2 samples to make them publicly and freely
available to other researchers.")
      (license license:asl2.0))))

(define-public python-scanpy-git
  (let ((commit "590d42309f9ed6550d7b887039990edfc1ac7648") ; April 22, 2020
        (revision "1"))
    (package
      (inherit python-scanpy)
      (name "python-scanpy-git")
      (version (git-version "1.4.6" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/theislab/scanpy")
                 (commit commit)))
          (file-name (git-file-name "python-scanpy" version))
          (sha256
           (base32 "0z3pk9vh4b7fqq7fs262i6z0pm1dnn6bf49a4r7r73k6gjj6namd"))))
      (arguments
       (substitute-keyword-arguments (package-arguments python-scanpy)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'build 'fix-build
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out"))
                       (pyv (python-version (assoc-ref inputs "python"))))
                   (substitute* "setup.py"
                     (("use_scm_version=True") "use_scm_version=False"))
                   (substitute* "scanpy/__init__.py"
                     (("__version__.*")
                      (string-append "__version__ = '" ,version "'\n")))
                   (mkdir-p
                     (string-append out "/lib/python" pyv "/site-packages"))
                   (setenv "PYTHONPATH"
                           (string-append out
                                          "/lib/python" pyv "/site-packages/:"
                                          (getenv "PYTHONPATH"))))
                 ;; These tests fail on this git revision
                 (delete-file "scanpy/tests/test_neighbors_key_added.py")
                 (delete-file "scanpy/tests/test_pca.py")
                 #t)))))))))

(define-public vg
  (package
    (name "vg")
    (version "1.32.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/vgteam/vg/releases/download/v"
                            version "/vg-v" version ".tar.gz"))
        (sha256
         (base32
          "0sqk2ymd5p1mpvsxaaz5vz3fdc8m9vd2l9307bd59603nijm8yzf"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; List all the options, makes it easier to try to remove them.
            ;(delete-file-recursively "deps/BBHash")
            ;(delete-file-recursively "deps/DYNAMIC")
            ;(delete-file-recursively "deps/FlameGraph")
            ;(delete-file-recursively "deps/backward-cpp")
            (delete-file-recursively "deps/bash-tap")
            ;(delete-file-recursively "deps/dozeu")
            (delete-file-recursively "deps/elfutils")
            ;(delete-file-recursively "deps/fastahack")
            ;(delete-file-recursively "deps/fermi-lite")
            ;(delete-file-recursively "deps/gbwt")
            (delete-file-recursively "deps/gbwt/deps")
            ;(delete-file-recursively "deps/gbwtgraph")
            (delete-file-recursively "deps/gbwtgraph/deps")
            ;(delete-file-recursively "deps/gcsa2")
            ;(delete-file-recursively "deps/gfakluge")
            ;(delete-file-recursively "deps/gssw")
            ;(delete-file-recursively "deps/ipso")
            (delete-file-recursively "deps/jemalloc")
            ;(delete-file-recursively "deps/libVCFH")
            ;(delete-file-recursively "deps/libbdsg")
            ;(delete-file-recursively "deps/libbdsg/bdsg/deps")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/BBHash")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/DYNAMIC")
            ;(delete-file-recursively "deps/libbdsg/bdsg/deps/hopscotch-map")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/libhandlegraph")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/pybind11")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/sdsl-lite")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/sparsepp")
            ;(delete-file-recursively "deps/libdeflate")
            ;(delete-file-recursively "deps/libhandlegraph")
            ;(delete-file-recursively "deps/libvgio")
            ;(delete-file-recursively "deps/libvgio/deps")
            (delete-file-recursively "deps/raptor")
            ;(delete-file-recursively "deps/sdsl-lite")
            (delete-file-recursively "deps/snappy")
            ;(delete-file-recursively "deps/sonLib")
            (delete-file-recursively "deps/sparsehash")
            ;(delete-file-recursively "deps/ssw")
            (delete-file-recursively "deps/sublinear-Li-Stephens/deps")
            (delete-file-recursively "deps/vcflib")
            (delete-file-recursively "deps/vowpal_wabbit")
            ;(delete-file-recursively "deps/xg")
            ;; Removing causes segfaults in the test suite
            ;(delete-file-recursively "deps/xg/deps")
            ;; libvgio doesn't search the correct include directory.
            (copy-recursively "deps/libhandlegraph/src/include/handlegraph"
                              "deps/libvgio/include/handlegraph")
            #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; no configure script
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               ;; PKG_CONFIG_DEPS needs to be substituted to actually link to everything.
               (("cairo jansson")
                "cairo jansson vcflib htslib sdsl-lite libvw raptor2 protobuf libelf libdw")

               ;; Skip the part where we link static libraries special. It doesn't like the changes we make
               (("-Wl,-B.*") "\n")

               (("\\$\\(CWD\\)/\\$\\(LIB_DIR\\)/libhts\\.a") "$(LIB_DIR)/libhts.a")
               ((" \\$\\(LIB_DIR\\)/libhts\\.a")
                (string-append " " (assoc-ref inputs "htslib") "/lib/libhts.so"))
               (("\\$\\(LIB_DIR\\)/pkgconfig/htslib\\.pc") "")

               ((" \\$\\(LIB_DIR\\)/libvcflib.a")
                (string-append " " (assoc-ref inputs "vcflib") "/lib/libvcflib.so"))
               ((" \\$\\(VCFLIB_DIR\\)/bin/vcf2tsv")
                (string-append " " (assoc-ref inputs "vcflib") "/bin/vcf2tsv"))

               ((" \\$\\(FASTAHACK_DIR\\)/bin/fastahack")
                (string-append " " (assoc-ref inputs "fastahack") "/bin/fastahack"))

               ((" \\$\\(LIB_DIR\\)/libsnappy.a")
                (string-append " " (assoc-ref inputs "snappy") "/lib/libsnappy.so"))

               ((" \\$\\(LIB_DIR\\)/libvw.a")
                (string-append " " (assoc-ref inputs "vowpal-wabbit") "/lib/libvw.so"))
               ((" \\$\\(LIB_DIR\\)/liballreduce.a")
                (string-append " " (assoc-ref inputs "vowpal-wabbit") "/lib/liballreduce.so"))

               ;; Only link against the libraries in the elfutils package.
               (("-ldwfl -ldw -ldwelf -lelf -lebl") "-ldw -lelf")
               ((" \\$\\(LIB_DIR\\)/libelf.a")
                (string-append " " (assoc-ref inputs "elfutils") "/lib/libelf.so"))
               ((" \\$\\(LIB_DIR\\)/libdw.a")
                (string-append " " (assoc-ref inputs "elfutils") "/lib/libdw.so"))

               ;; We need the Make.helper file in SDSL_DIR for gcsa2
               ;((" \\$\\(LIB_DIR\\)/libsdsl.a")
               ; (string-append " " (assoc-ref inputs "sdsl-lite") "/lib/libsdsl.so"))

               ((" \\$\\(LIB_DIR\\)/libdivsufsort.a")
                (string-append " " (assoc-ref inputs "libdivsufsort") "/lib/libdivsufsort.so"))
               ((" \\$\\(LIB_DIR\\)/libdivsufsort64.a")
                (string-append " " (assoc-ref inputs "libdivsufsort") "/lib/libdivsufsort64.so"))

               ((" \\$\\(LIB_DIR\\)/libjemalloc.a")
                (string-append " " (assoc-ref inputs "jemalloc") "/lib/libjemalloc.so"))

               ((" \\$\\(INC_DIR\\)/sparsehash")
                (string-append " " (assoc-ref inputs "sparsehash") "/include/sparsehash"))

               ((" \\$\\(INC_DIR\\)/raptor2")
                (string-append " " (assoc-ref inputs "raptor2") "/include/raptor2"))
               ((" \\$\\(LIB_DIR\\)/libraptor2.a")
                (string-append " " (assoc-ref inputs "raptor2") "/lib/libraptor2.so"))
               ((" \\$\\(BIN_DIR\\)/rapper")
                (string-append " " (assoc-ref inputs "raptor2") "/bin/rapper")))
             ;; vcf2tsv shows up in a couple of other places
             (substitute* "test/t/02_vg_construct.t"
               (("../deps/vcflib/bin/vcf2tsv") (which "vcf2tsv")))
             #t))
         (add-after 'unpack 'fix-hopscotch-dependency
           (lambda _
             (substitute* "Makefile"
               ;; The build directory for hopscotch_map-prefix.
               (("rm -Rf build && ") ""))
             ;; Don't try to download hopscotch_map from the internet.
             (substitute* "deps/DYNAMIC/CMakeLists.txt"
               ((".*GIT_REPOSITORY.*")
                "SOURCE_DIR \"../../libbdsg/bdsg/deps/hopscotch-map\"\n")
               ((".*BUILD_IN_SOURCE.*") ""))
             ;; We still need to copy it to the expected location.
             (copy-recursively
               "deps/libbdsg/bdsg/deps/hopscotch-map"
               "deps/DYNAMIC/build/hopscotch_map-prefix/src/hopscotch_map")
             #t))
         (add-after 'unpack 'adjust-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash-tap (assoc-ref inputs "bash-tap")))
               (substitute* (find-files "test/t" ".")
                 (("BASH_TAP_ROOT.*")
                  (string-append "BASH_TAP_ROOT=" bash-tap "/bin\n"))
                 ((".*bash-tap-bootstrap")
                  (string-append ". " bash-tap "/bin/bash-tap-bootstrap")))
               ;; Lets skip the 4 failing tests for now. They fail with our
               ;; bash-tap and the bundled one.
               (substitute* "test/t/02_vg_construct.t"
                 ((".*the graph contains.*") "is $(true) \"\" \"\"\n"))
               (substitute* '("test/t/07_vg_map.t"
                              "test/t/33_vg_mpmap.t")
                 ((".*node id.*") "is $(true) \"\" \"\"\n"))
               ;; Don't test the docs, we're not providing npm
               (substitute* "Makefile"
                 ((".*test-docs.*") ""))
               #t)))
         (add-after 'build 'build-manpages
           (lambda _
             (invoke "make" "man")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "bin/vg" (string-append out "/bin"))
               (install-file "lib/libvg.a" (string-append out "/lib"))
               (for-each
                 (lambda (file)
                   (install-file file (string-append out "/share/man/man1")))
                 (find-files "doc/man" "\\.1$"))
               #t))))
       #:test-target "test"))
    (native-inputs
     `(("asciidoctor" ,ruby-asciidoctor)
       ("bash-tap" ,bash-tap)
       ("bc" ,bc)
       ("cmake" ,cmake-minimal)
       ("jq" ,jq)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("samtools" ,samtools)
       ("util-linux" ,util-linux)
       ("which" ,which)
       ("xxd" ,xxd)))
    (inputs
     `(("boost" ,boost)
       ("cairo" ,cairo)
       ("curl" ,curl-minimal)
       ("elfutils" ,elfutils)
       ("fastahack" ,fastahack)
       ("htslib" ,htslib)
       ("jansson" ,jansson)
       ("jemalloc" ,jemalloc)
       ("libdivsufsort" ,libdivsufsort)
       ("ncurses" ,ncurses)
       ("protobuf" ,protobuf)
       ("raptor2" ,raptor2)
       ("sdsl-lite" ,sdsl-lite)
       ("smithwaterman" ,smithwaterman)
       ("snappy" ,snappy)
       ("sparsehash" ,sparsehash)
       ("tabixpp" ,tabixpp)
       ("vcflib" ,vcflib)
       ("vowpal-wabbit" ,vowpal-wabbit)
       ("zlib" ,zlib)))
    (home-page "https://www.biostars.org/t/vg/")
    (synopsis "Tools for working with genome variation graphs")
    (description "Variation graphs provide a succinct encoding of the sequences
of many genomes.  A variation graph (in particular as implemented in vg) is
composed of:
@enumerate
@item nodes, which are labeled by sequences and ids
@item edges, which connect two nodes via either of their respective ends
@item paths, describe genomes, sequence alignments, and annotations (such as
gene models and transcripts) as walks through nodes connected by edges
@end enumerate
This model is similar to sequence graphs that have been used in assembly and
multiple sequence alignment.")
    (license
      (list
        license:expat   ; main program
        license:bsd-2   ; deps/xg/deps/ips4o
        license:bsd-3   ; deps/sparsepp, deps/sonLib/C/{impl,inc}
        license:asl2.0  ; deps/sonLib/externalTools/quicktree_1.1, deps/structures
        license:gpl3+   ; all sdsl-lite copies
        license:zlib    ; deps/sonLib/externalTools/cutest
        license:boost1.0)))) ; catch.hpp

(define-public ucsc-genome-browser
  (package
    (name "ucsc-genome-browser")
    (version "413")
    (source (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://genome-source.gi.ucsc.edu/kent.git/")
             (commit (string-append "v" version "_base"))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1qcjhd4wcajik71z5347fw2sfhfkv0p6y7yldrrkmycw2qhqmpzn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:parallel-tests? #f ; not supported
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (add-before 'build 'pre-build
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Start by setting some variables.
               (chdir "src")
               (setenv "CC" ,(cc-for-target))
               (setenv "HOME" (getcwd))

               ;; And here we set the output directories
               (setenv "CGI_BIN" (string-append out "/cgi-bin"))
               (setenv "CGI_BIN_USER" (string-append out "/cgi-bin"))
               (setenv "DOCUMENTROOT" (string-append out "/html"))
               (setenv "DOCUMENTROOT_USER" (string-append out "/html"))
               (setenv "BINDIR" (string-append out "/bin"))

               ;; Now let's fix some errors:
               (mkdir-p (string-append out "/cgi-bin"))
               (substitute* "inc/cgi_build_rules.mk"
                  (("rm -f.*") ""))
               (substitute* (cons* "inc/cgi_build_rules.mk"
                                   (find-files "." "makefile"))
                  (("CGI_BIN\\}-\\$\\{USER") "CGI_BIN_USER"))

               ;; Force linking with freetype.
               (substitute* "inc/common.mk"
                 (("libpng-config --ldflags") "pkg-config --libs libpng freetype2")
                 (("libpng-config --I_opts") "pkg-config --cflags-only-I libpng freetype2")
                 (("\\$\\{HG_INC\\}" hg_inc) (string-append hg_inc " -DUSE_FREETYPE")))

               ;; Force the trash location.
               (substitute* (cons*
                              "utils/qa/showTracks"
                              "webBlat/webBlat.cfg"
                              "hg/js/hgTracks.js"
                              (find-files "." "\\.c$"))
                 ;; This line is specifically needed as-is.
                 (("\\.\\./trash") "/var/cache/genome"))

               #t)))

         (add-before 'check 'pre-check
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (triplet ,(gnu-triplet->nix-system (%current-system))))
               (setenv "PATH" (string-append (getenv "PATH")
                                             ":" out "/bin"))
               (setenv "MACHTYPE"
                       (string-take triplet (string-index triplet #\-)))
               (for-each make-file-writable
                         (find-files "utils/bedJoinTabOffset/tests"))
               (substitute* '("utils/bamToPsl/tests/makefile"
                              "utils/trackDbIndexBb/tests/makefile")
                 (("/cluster/bin/bedtools/bedtools") (which "bedtools")))

               ;; These tests fail intermittently:
               (substitute* "utils/vcfFilter/tests/makefile"
                 ((" testRenameNoGt ") " ")
                 ((" testMinAc1NoGt ") " "))

               ;; These tests can't find their database:
               (substitute* "hg/lib/tests/makefile"
                 ((" spDbTest ") " ")
                 ((" hdbTest ") " "))
               (substitute* "hg/lib/tests/genePredTests.mk"
                 ((" tableTests ") " ")
                 ((" compatTblTests ") " "))
               (substitute* "hg/lib/tests/pslReaderTests.mk"
                 ((" tableTests") " "))
               (substitute* "hg/lib/tests/makefile"
                 ((" annoGratorTest ") " ")
                 ((" customTrackTest ") " ")
                 ((" hgvsTest") " "))
               (substitute* "hg/autoSql/tests/makefile"
                 ((" dbLinkTest ") " ")
                 ((" symColsTest ") " "))
               (delete-file "hg/checkTableCoords/tests/makefile")
               (delete-file "hg/hgGetAnn/tests/makefile")
               (substitute* "hg/sqlToXml/makefile"
                 ((".*doTest.*") ""))
               (substitute* "hg/utils/genePredFilter/tests/makefile"
                 ((" gencodeHackDbTest") " "))
               (substitute* "hg/utils/refSeqGet/tests/makefile"
                 (("^test::.*") "test:: mkout\n"))
               (delete-file "hg/utils/vcfToHgvs/tests/makefile")
               (substitute* "hg/bedItemOverlapCount/tests/makefile"
                 ((".*RunTest.*") ""))

               ;; Depends on /cluster
               (substitute* "hg/liftOver/tests/makefile"
                 (("^test:.*") "test: mkdirs scaffoldEndBug\n"))
               (delete-file "hg/mouseStuff/netToAxt/tests/makefile")
               (substitute* "hg/pslToChain/tests/makefile"
                 ((" example1 ") " "))

               ;; Depends on /gbdb
               (delete-file "hg/mouseStuff/chainBridge/tests/makefile")

               ;; Depends on /hive
               (delete-file "hg/utils/genePredToProt/tests/makefile")

               ;; Unclear why this fails
               (delete-file "hg/utils/hgvsToVcf/tests/makefile")

               ;; Wants network
               (substitute* "hg/utils/hubCheck/tests/makefile"
                 (("^test::.*") "test:: one two\n"))
               #t)))

         ;; Install happens during the 'build phase.
         ;; Install the website files too
         (replace 'install
           (lambda _
             (invoke "make" "doc-install")
             #t))
         (add-after 'install 'create-hg-conf
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-output-to-file (string-append out "/cgi-bin/hg.conf")
                 (lambda ()
                   (display
                     "include /var/lib/genome/hg.conf\n")))
               #t)))
         (add-after 'install 'create-symlink
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "htdocs")
             ;; Fallback location for fonts.
             (symlink (string-append (assoc-ref inputs "gs-fonts")
                                     "/share/fonts/type1/ghostscript")
                      "htdocs/urw-fonts")
             #t)))))
    (inputs
     `(("freetype" ,freetype)
       ("libpng" ,libpng)
       ("mysql:dev" ,mariadb "dev")
       ("mysql:lib" ,mariadb "lib")
       ("openssl" ,openssl)
       ("perl" ,perl)
       ("python2" ,python-2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bedtools" ,bedtools)
       ("gs-fonts" ,gs-fonts)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("rsync" ,rsync)    ; For installing js files from the source checkout
       ("tcl" ,tcl)
       ("tcsh" ,tcsh)
       ("util-linux:lib" ,util-linux "lib")
       ("which" ,(@ (gnu packages base) which))))
    (home-page "https://www.genome.ucsc.edu/")
    (synopsis "Structural variants detector for next-gen sequencing data")
    (description
     "The UCSC Genome Browser provides a rapid and reliable display of any
requested portion of genomes at any scale, together with dozens of aligned
annotation tracks (known genes, predicted genes, ESTs, mRNAs, CpG islands,
assembly gaps and coverage, chromosomal bands, mouse homologies, and more).
Half of the annotation tracks are computed at UCSC from publicly available
sequence data.  The remaining tracks are provided by collaborators worldwide.
Users can also add their own custom tracks to the browser for educational or
research purposes.
The Genome Browser stacks annotation tracks beneath genome coordinate positions,
allowing rapid visual correlation of different types of information.  The user
can look at a whole chromosome to get a feel for gene density, open a specific
cytogenetic band to see a positionally mapped disease gene candidate, or zoom in
to a particular gene to view its spliced ESTs and possible alternative splicing.
The Genome Browser itself does not draw conclusions; rather, it collates all
relevant information in one location, leaving the exploration and interpretation
to the user.")
    (license (list
               license:bsd-0    ; kent/src/{utils,lib,inc,tabStorm,parasol,hg/ausoSql,hg/autoXml}
               license:bsd-3    ; these two for bundled htslib-1.3
               license:expat
               (license:non-copyleft
                 "https://www.genome.ucsc.edu/license/"
                 "Free for academic/non-profit/personal use only.")
               (license:non-copyleft    ; Blat, In-Silico PCR
                 "http://www.kentinformatics.com/index.html"
                 "Free for universities and non-profit institutions.")))))

(define-public bam2fastx
  (package
    (name "bam2fastx")
    (version "1.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/PacificBiosciences/bam2fastx")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0pyfmvh874w29kaq6gbb1bd86135qs2jc4f8giw98kxw1b2gjdh0"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dtests=true")))
    (inputs
     `(("boost" ,boost)
       ;("htslib" ,htslib)
       ("pbbam" ,pbbam)
       ("pbcopper" ,pbcopper)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-cram" ,python-cram)
       ("python-wrapper" ,python-wrapper)))
    (home-page "https://github.com/PacificBiosciences/bam2fastx")
    (synopsis "Converting and demultiplexing of PacBio BAM files into gzipped fasta and fastq files")
    (description "Conversion of PacBio BAM files into gzipped fasta and fastq files, including splitting of barcoded data.")
    (license license:bsd-3)))

(define-public pbcopper
  (package
    (name "pbcopper")
    (version "1.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/PacificBiosciences/pbcopper")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1pphklil5kn1ds796ch41bgvdf7yq03z6w5rgi572s8xg8k5b0xn"))))
    (build-system meson-build-system)
    (inputs
     `(("boost" ,boost)))
    (native-inputs
     `(("googletest" ,googletest)))
    (home-page "https://github.com/PacificBiosciences/pbcopper")
    (synopsis "Core C++ library for data structures, algorithms, and utilities")
    (description "The pbcopper library provides a suite of data structures, algorithms, and utilities for PacBio C++ applications.")
    (license license:bsd-3)))
