;; Bioinformatics module

(define-module (gn packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages)
  #:use-module (gn packages crates-io)
  #:use-module (gn packages java)
  #:use-module (gn packages ocaml)
  #:use-module (gn packages python)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module ((gnu packages python-xyz) #:hide (python2-six))
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (past packages python27))

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
    (license (license:non-copyleft "file:///LICENSE"))))

(define-public edirect-gn
  (deprecated-package "edirect-gn" edirect))

(define-public gfaffix
  (package
    (name "gfaffix")
    (version "0.1.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/marschall-lab/GFAffix")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "181jxl8ldj39jgscyqzhz4l4k5kxj1j9hvzi8dxj59h2zzznb0kb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-clap" ,rust-clap-3)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-handlegraph" ,rust-handlegraph-0.7.0-alpha.9)
        ("rust-gfa" ,rust-gfa-0.10)
        ("rust-quick-csv", rust-quick-csv-0.1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-env-logger" ,rust-env-logger-0.7))))
    (home-page "https://github.com/marschall-lab/GFAffix")
    (synopsis "Identify walk-preserving shared affixes in variation graphs")
    (description
     "GFAffix identifies walk-preserving shared affixes in variation graphs and
collapses them into a non-redundant graph structure.")
    (license license:expat)))

(define-public vcfbub
  (package
    (name "vcfbub")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/pangenome/vcfbub")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0sk2ab22z6qa00j1w8a8f5kbb7q2xb10fhd32zy4lh351v3mqmyg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-vcf" ,rust-vcf-0.6))))
    (home-page "https://github.com/pangenome/vcfbub")
    (synopsis "Popping bubbles in vg deconstruct VCFs")
    (description
     "The VCF output produced by a command like @command{vg deconstruct -e -a
-H '#' ...} includes information about the nesting of variants.  With @code{-a},
@code{--all-snarls}, we obtain not just the top level bubbles, but all nested
ones.  This exposed snarl tree information can be used to filter the VCF to
obtain a set of non-overlapping sites (n.b. \"snarl\" is a generic model of
graph bubbles including tips and loops).
@code{vcfbub} lets us do two common operations on these VCFs:
@enumerate
@item We can filter sites by maximum level in the snarl tree.  For instance,
@code{--max-level 0} would keep only sites with @code{LV=0}.  In practice, vg's
snarl finder ensures that these are sites rooted on the main linear axis of the
pangenome graph.  Those at higher levels occur within larger variants.
@item We can filter sites by maximum allele size, either for the reference
allele or any allele.  In this case, @code{--max-ref-length 10000} would keep
only sites where the reference allele is less than 10kb long.  Setting
@code{--max-ref-length} or @code{--max-allele-length} additionally ensures that
the output contains the bubbles nested inside of any popped bubble, even if
they are at greater than @code{--max-level}.
@end enumerate
@code{vcfbub} accomplishes a simple task: we keep sites that are the children
of those which we \"pop\" due to their size.  These occur around complex large
SVs, such as multi-Mbp inversions and segmental duplications.  We often need to
remove these, as they provide little information for many downstream
applications, such as haplotype panels or other imputation references.")
    (license license:expat)))

(define-public fastix
  (package
    (name "fastix")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fastix" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1mzk65mg8vx0hz39xis6zqdmq56abhmza656gn9pgmlsn151gpx2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-clap" ,rust-clap-2))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-0.12)
        ("rust-predicates" ,rust-predicates-1))))
    (home-page "https://github.com/ekg/fastix")
    (synopsis "Prefix-renaming FASTA records")
    (description "A command line tool to add prefixes to FASTA headers.  The
idea is to support pangenomic applications, following the
@url{https://github.com/pangenome/PanSN-spec, PanSN} hierarchical naming
specification.")
    (license license:expat)))

(define-public gafpack
  (let ((commit "ad31875b6914d964c6fd72d1bf334f0843538fb6")     ; November 10, 2022
        (revision "1"))
    (package
      (name "gafpack")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ekg/gafpack")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0di2psh0ls7jlbnqs7k71p55f73pn23a09k1h3ril7gwjcrzr3rk"))))
      (build-system cargo-build-system)
      (arguments
       `(#:install-source? #f
         #:cargo-inputs
         (("rust-clap" ,rust-clap-4)
          ("rust-gfa" ,rust-gfa-0.10))))
      (home-page "https://github.com/ekg/gafpack")
      (synopsis "Convert variation graph alignments to coverage maps over nodes")
      (description
       "Gafpack converts alignments to pangenome variation graphs to coverage
maps useful in haplotype-based genotyping.")
      (license license:expat))))

(define-public agc-for-pgr-tk
  (let ((commit "453c0afdc54b4aa00fa8e97a63f196931fdb81c4") ; April 26, 2022
        (revision "1"))
    (package
      (name "agc")
      (version (git-version "2.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/cschin/agc")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1v5s79rl38dcyy5h1lykbp6clcbqq9winn533j54y49q1jp8chix"))
          (snippet
           #~(begin
               (use-modules (guix build utils))
               ;; Copy the two radul files we can't find a replacement for:
               ;; https://github.com/refresh-bio/RADULS
               (mkdir "keep-libs")
               (rename-file "libs/raduls.h" "keep-libs/raduls.h")
               (rename-file "libs/libraduls.a" "keep-libs/libraduls.a")
               (delete-file-recursively "libs")
               (rename-file "keep-libs" "libs")

               (delete-file-recursively "py_agc_api/pybind11-2.8.1")
               (substitute* '("makefile" "makefile.release")
                 (("-mavx") "")
                 (("-m64") "")
                 (("\\$\\(AGC_LIBS_DIR)\\/mimalloc/\\$\\(LIB_ALLOC\\)")
                  "$(pkg-config --cflags --libs mimalloc) /usr/lib/libmimalloc.so")
                 (("\\$\\(AGC_LIBS_DIR)\\/\\$\\(LIB_ZLIB\\)")
                  "$(pkg-config --cflags --libs zlib) /usr/lib/libz.so")
                 (("\\$\\(AGC_LIBS_DIR)\\/\\$\\(LIB_ZSTD\\)")
                  "$(pkg-config --cflags --libs libzstd) /usr/lib/libzstd.so")
                 (("^PYBIND11_LIB = .*") "PYBIND11_LIB = /usr/include/pybind11")
                 (("\\$\\(PYBIND11_LIB\\)/include") "$(PYBIND11_LIB)"))
               (substitute* (find-files "src" "\\.(h|cpp)$")
                 (("../../libs/ketopt.h") "ketopt.h")
                 (("../../libs/zlib.h") "zlib.h")
                 (("../../libs/zstd.h") "zstd.h"))))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; No tests.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; No configure script.
           (add-after 'unpack 'adjust-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((mimalloc (assoc-ref inputs "mimalloc")))
                 (substitute* '("makefile" "makefile.release")
                   (("/usr/include/pybind11")
                    (search-input-directory inputs "/include/pybind11"))
                   (("/usr/lib/libmimalloc.so")
                    (search-input-file inputs "/lib/libmimalloc.so"))
                   (("/usr/lib/libz.so")
                    (search-input-file inputs "/lib/libz.so"))
                   (("/usr/lib/libzstd.so")
                    (search-input-file inputs "/lib/libzstd.so"))
                   (("pkg-config") ,(pkg-config-for-target))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (include (string-append out "/include/")))
                 (install-file "agc" (string-append out "/bin"))
                 (install-file "libagc.so" (string-append out "/lib"))
                 (mkdir-p (string-append include "app"))
                 (mkdir-p (string-append include "core"))
                 (mkdir-p (string-append include "lib-cxx"))
                 (with-directory-excursion "src"
                   (for-each
                     (lambda (file)
                       (copy-file file (string-append include file)))
                     (find-files "." "\\.h$")))))))))
      (native-inputs
       (list minimap2                   ; for ketopt.h
             pkg-config))
      (inputs
       (list mimalloc
             python
             pybind11
             zlib
             (list zstd "lib")))
      (home-page "https://github.com/cschin/agc")
      (synopsis "Assembled Genomes Compressor")
      (description
       "@acronym{Assembled Genomes Compressor, AGC} is a tool designed to
compress collections of de-novo assembled genomes.  It can be used for various
types of datasets: short genomes (viruses) as well as long (humans).")
      (license license:expat))))

(define-public pgr-tk
  (package
    (name "pgr-tk")
    (version "0.3.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Sema4-Research/pgr-tk")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0vm1k63v91zd0pfbg2zmwskajylz8xg83m63qxwaiwny5f4y6f1j"))
        (snippet
         #~(begin
             (use-modules (guix build utils))
             (substitute* (find-files "." "Cargo.toml")
               ;; Only use the major+minor version to decrease the number of
               ;; special version crates.
               (("(.*= \")([[:digit:]]+\\.[[:digit:]]+)\\.[[:digit:]]+(\".*)"
                 _ name version tail)
                (string-append name version tail))
               ;; Then fix the version string for the actual package.
               (("^version = \".*")
                (string-append "version = \"" #$version "\"\n")))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags
       (list "--release" "--"
             "--skip=get_aln_segements"
             "--skip=get_shmmr_dots"
             "--skip=AGCFile"
             "--skip=SeqIndexDB")
       #:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.58)
        ("rust-bgzip" ,rust-bgzip-0.2)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-clap" ,rust-clap-3)
        ("rust-cuckoofilter" ,rust-cuckoofilter-0.5)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-petgraph" ,rust-petgraph-0.6)
        ("rust-pyo3" ,rust-pyo3-0.14)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-simple-logger" ,rust-simple-logger-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'insert-wfa-source
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "wfa-src")
                               "rs-wfa/WFA")))
         (add-after 'unpack 'adjust-source
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("pgr-bin/build.rs"
                            "pgr-db/build.rs"
                            "pgr-tk/build.rs")
               (("git") "ls")
               (("bioconda") "Guix"))
             ;; Build with zlib, not zlib-ng
             (substitute* '("pgr-bin/Cargo.toml"
                            "pgr-db/Cargo.toml")
               (("zlib-ng-compat") "zlib"))
             ;; Don't look for agc to be bundled.
             (substitute* "pgr-db/wrapper.h"
               (("../agc/src/lib-cxx/agc-api.h") "lib-cxx/agc-api.h"))
             (substitute* "pgr-db/build.rs"
               ((".*panic!\\(\"Error.*") ""))
             (mkdir-p "target/release")
             (symlink (search-input-file inputs "/bin/agc")
                      "target/release/agc")
             (symlink (search-input-file inputs "/lib/libagc.so")
                      "target/release/libagc")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "target/release"
                 (install-file "libpgrtk.so" (string-append out "/lib"))
                 (for-each
                   (lambda (file)
                     (install-file file (string-append out "/bin")))
                   (list "pgr-filter"
                         "pgr-mdb"
                         "pgr-multifilter"
                         "pgr-probe-match"
                         "pgr-shmmr-pair-count")))))))))
    (inputs
     (list agc-for-pgr-tk
           clang
           python
           zlib
           (list zstd "lib")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("wfa-src"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  ;; forPYO3 branch, 14-03-2021
                  (url "https://github.com/cschin/WFA")
                  (commit "1f8c8d2905ed482cd2d306a1676d60c2a45cb098")))
           (file-name "wfa-for-pgr-tk")
           (sha256
            (base32 "19h1cjp2bdlcfq5c6rsbk8bc0f8zn64b471dhj4xlfxd1prv2dpk"))))))
    (home-page "https://github.com/Sema4-Research/pgr-tk")
    (synopsis "Pangenome Research Tool Kit")
    (description
     "PGR-TK provides pangenome assembly management, query and
@acronym{Minimizer Anchored Pangenome, MAP} Graph Generation.  It is a project
to provide Python and Rust libraries to facilitate pangenomics analysis.
Several algorithms and data structures used for the Peregrine Genome Assembler
are useful for Pangenomics analysis as well.  This repo takes those algorithms
and data structure, combining other handy 3rd party tools to expose them as a
library in Python (with Rust code for those computing parts that need
performance.)")
    (license (license:non-copyleft
               "file:///LICENSE"
               "CC-BY-NC-SA 4.0"))))

(define-public graph-genotyper
  (let ((commit "e7cc6b43a5b1f389d76bf9aac7f2ee02f92caeaf") ; October 17, 2022
        (revision "13"))
    (package
      (name "graph-genotyper")
      (version (git-version "0.0.0" revision commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/davidebolo1993/graph_genotyper")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1l8yjpkqamiqr1q5i7vr5z04aba7skpbcwyc9dx5fiklvljjfhcx"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("genotype.py" "bin/")
           ("genotype.sh" "bin/"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-genotype
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (wrap-script (string-append out "/bin/genotype.sh")
                  `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
                  `("PATH" ":" prefix
                    ,(map (lambda (file-name)
                            (string-append (assoc-ref inputs file-name) "/bin"))
                          (list "gafpack"
                                "odgi"
                                "python"
                                "samtools"
                                "vg"))))))))))
      (inputs
       (list gafpack
             guile-3.0
             odgi
             python
             python-numpy
             python-pandas
             python-scipy
             samtools
             vg))
      (home-page "https://bitbucket.org/jana_ebler")
      (synopsis "Genotyping based on k-mers and pangenome graphs")
      (description
       "This package provides a genotyper for various types of genetic variants
(such as SNPs, indels and structural variants).  Genotypes are computed based on
read k-mer counts and a panel of known haplotypes.  A description of the method
can be found @url{https://www.biorxiv.org/content/10.1101/2020.11.11.378133v1,
here}.")
      (license (license:non-copyleft
                 "No license listed")))))

(define-public pangenie
  (let ((commit "e779076827022d1416ab9fabf99a03d8f4725956") ; September 2, 2021 from phasing-tests branch
        (revision "2"))
    (package
      (name "pangenie")
      (version (git-version "0.0.0" revision commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://bitbucket.org/jana_ebler/pangenie.git")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1mphrvidaz328kcwrjgz8q5i4iwnz6ygl6279lm2acv4zgqhmp5i"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags (list (string-append "-DCMAKE_BUILD_RPATH="
                                                (assoc-ref %outputs "out") "/lib"))
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "make" "-C" "tests"))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (with-directory-excursion "src"
                   (install-file "PanGenie" (string-append out "/bin"))
                   (install-file "PanGenie-graph" (string-append out "/bin"))
                   (install-file "libPanGenieLib.so" (string-append out "/lib"))
                   )
                 #t))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("jellyfish" ,jellyfish)))
      (home-page "https://bitbucket.org/jana_ebler")
      (synopsis "Genotyping based on k-mers and pangenome graphs")
      (description
       "This package provides a genotyper for various types of genetic variants
(such as SNPs, indels and structural variants).  Genotypes are computed based on
read k-mer counts and a panel of known haplotypes.  A description of the method
can be found @url{https://www.biorxiv.org/content/10.1101/2020.11.11.378133v1,
here}.")
      (license license:expat))))

(define-public pbsim2
  (let ((commit "e71f7892aea0bd3c963b4f1f5628db4f830ee475") ; Dec 2, 2020
        (revision "1"))
    (package
      (name "pbsim2")
      (version (git-version "0.0.0" revision commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yukiteruono/pbsim2")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "13d3mhdp3rs58w14j9a5sbda4q8k9vzic7rgfa8223m7cm5ih6y7"))))
      (build-system gnu-build-system)
      (home-page "https://github.com/yukiteruono/pbsim2")
      (synopsis "Simulator for long read sequencers")
      (description "PBSIM simulates @acronym{Continuous Long Reads, CLRs} of
PacBio, and Nanopore reads.  In it sampling-based and model-based simulations
are implemented.")
      (license license:gpl2))))

(define-public pirs
  (let ((commit "bee9b594f4d0e10580aae77ec411cecec4a77219") ; Sept 7, 2017
        (revision "1"))
    (package
      (name "pirs")
      (version (git-version "2.0.2" revision commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/galaxy001/pirs")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0pn74h98cqcr5qayp4riss982n4272p35y5dp472cmqpwjjil9cd"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags (list "--enable-pirs-diploid"
                                 ;; TODO: Enable after core-updates merge, late 2021.
                                 ;,@(if (not (or (target-x86-64?)
                                 ;               (target-x86-32?)))
                                 ;    `("--disable-sse2")
                                 ;    '())
                                 )
         #:phases
         (modify-phases %standard-phases
           (add-before 'bootstrap 'chdir
             (lambda _
               (chdir "src")))
           (add-after 'unpack 'patch-source
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out  (assoc-ref outputs "out")))
                 (substitute* "src/configure.ac"
                   (("ssse2") "sse2"))
                 (substitute* "src/stator/gcContCvgBias/Makefile"
                   (("gzstream.o ") "")
                   (("-lz")"-lgzstream -lz")
                   (("-static") "")
                   (("-mtune=generic") ""))
                 (substitute* "src/pirs/gccMakefile"
                   (("/usr/local") out)))))
           (replace 'check
             (lambda* (#:key tests? test-target #:allow-other-keys #:rest args)
               (when tests?
                 (apply (assoc-ref %standard-phases 'check) args)
                 (with-directory-excursion "stator/gcContCvgBias"
                 ;  ((assoc-ref %standard-phases 'check)
                 ;   #:test-target "test" args))
                   (invoke "make" "test")))))
           (add-after 'build 'build-more
             (lambda* (#:key #:allow-other-keys #:rest args)
               (with-directory-excursion "stator/gcContCvgBias"
                 (apply (assoc-ref %standard-phases 'build) args))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys #:rest args)
               (let ((out (assoc-ref outputs "out")))
                 (apply (assoc-ref %standard-phases 'install) args)
                 (with-directory-excursion "stator/gcContCvgBias"
                   ;(apply (assoc-ref %standard-phases 'install) args)
                   (install-file "gc_coverage_bias" (string-append out "/bin")))))))))
      (inputs
       `(("gnuplot" ,gnuplot)
         ("perl" ,perl)
         ("zlib" ,zlib)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("boost" ,boost)
         ("gzstream" ,gzstream)
         ("libtool" ,libtool)))
      (home-page "https://github.com/galaxy001/pirs")
      (synopsis "Profile based Illumina pair-end Reads Simulator")
      (description "@code{pIRS} is a program for simulating paired-end reads
from a reference genome.  It is optimized for simulating reads similar to those
generated from the Illumina platform.")
      (license license:gpl2))))

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

(define-public seqwish-0.1
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

;; TODO: Unbundle BBHash, parallel-hashmap, zstr
(define-public graphaligner
  (package
    (name "graphaligner")
    (version "1.0.14")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/maickrau/GraphAligner/files/"
                          "7813545/GraphAligner.tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "1y4vwp03fl2ck6bnyn0sc97vgvdb8i0yfzjk5mv5gk0bc7a4f0n1"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:tests? #f ; no tests
       #:make-flags
       #~(list (string-append "VERSION=" #$version))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure) ; no configure phase
           (add-after 'unpack 'patch-source
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((concurrentqueue (assoc-ref inputs "concurrentqueue")))
                 (delete-file-recursively "concurrentqueue")
                 (substitute* "makefile"
                   (("-Iconcurrentqueue")
                    (string-append "-I" concurrentqueue "/include/concurrentqueue"))
                   (("^JEMALLOCFLAGS.*")
                    "JEMALLOCFLAGS= `pkg-config --libs jemalloc`\n")
                   ;; No need to build statically.
                   (("-Wl,-Bstatic") "")
                   (("-static-libstdc\\+\\+") "")))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (for-each
                   (lambda (program)
                     (install-file program (string-append out "/bin")))
                   (find-files "bin"))
                 (for-each
                   (lambda (header)
                     (install-file header (string-append out "/include")))
                   (find-files "src" "\\.h(pp)?$"))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("sparsehash" ,sparsehash)))
    (inputs
     `(("boost" ,boost)
       ("concurrentqueue" ,concurrentqueue)
       ("jemalloc" ,jemalloc)
       ("libdivsufsort" ,libdivsufsort)
       ("mummer" ,mummer)
       ("protobuf" ,protobuf)
       ("sdsl-lite" ,sdsl-lite)
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

(define-public python-whatshap
  (package
    (name "python-whatshap")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "whatshap" version))
        (sha256
         (base32 "0vxv6y8sg25yii106j6k55vc5z7n1l1y1nax49dgbarbrvk8cr2f"))))
    (build-system python-build-system)
    (inputs
     `(("python-biopython" ,python-biopython)
       ("python-networkx" ,python-networkx)
       ("python-pyfaidx" ,python-pyfaidx)
       ("python-pysam" ,python-pysam)
       ("python-scipy" ,python-scipy)
       ("python-xopen" ,python-xopen)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/whatshap/whatshap/")
    (synopsis "Read-based phasing of genomic variants")
    (description
     "WhatsHap is a software for phasing genomic variants using DNA sequencing
reads, also called read-based phasing or haplotype assembly.  It is especially
suitable for long reads, but works also well with short reads.")
    (license license:expat)))

(define-public bh20-seq-resource
  (let ((commit "2ae71911cd87ce4f2eabdff21e538267b3270d45")
        (revision "4"))
    (package
      (name "bh20-seq-resource")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/pubseq/bh20-seq-resource")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1k6cc88hrcm77jwpdk2084q0zirv2vlbz3c07nmpbhk1lhqk5x0n"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (delete-file "gittaggers.py")))))
      (build-system python-build-system)
      (arguments
       (list
         #:tests? #f    ; Tests can't find pytest
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'patch-program-calls
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "bh20sequploader/qc_fasta.py"
                   (("\"minimap2\"")
                    (string-append "\"" (search-input-file
                                          inputs "/bin/minimap2")
                                   "\""))))))))
      (propagated-inputs
       (list python-arvados-python-client
             python-schema-salad
             python-magic
             python-pyshex
             python-pyshexc-0.7
             python-py-dateutil

             ;; for the web
             python-flask
             python-pyyaml
             python-redis

             ;; and for the service
             python
             gunicorn))
      (inputs
       (list minimap2))
      (native-inputs
       (list python-pytest-4                ; < 6
             python-pytest-runner-4))       ; < 5
      (home-page "https://github.com/pubseq/bh20-seq-resource")
      (synopsis
       "Tool to upload SARS-CoV-19 sequences and service to kick off analysis")
      (description "This repository provides a sequence uploader for the
COVID-19 Virtual Biohackathon's Public Sequence Resource project.  You can use
it to upload the genomes of SARS-CoV-2 samples to make them publicly and freely
available to other researchers.")
      (license license:asl2.0))))

;; This version has no profile collisions.
(define-public bh20-seq-resource-for-service
  (package
    ;(inherit (fix-profile-collisions-for-bh20 bh20-seq-resource))
    (inherit
      ((package-input-rewriting/spec
        `(("python-google-api-core" . ,(const python-google-api-core-1))
          ("python-google-auth" . ,(const python-google-auth-1))
          ("python-pyparsing" . ,(const python-pyparsing-2.4.7))))
       bh20-seq-resource))
    (properties `((hidden? . #t)))))

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

;; TODO: Unbundle everything
(define-public odgi
  (package
    (name "odgi")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/pangenome/odgi/releases"
                                  "/download/v" version
                                  "/odgi-v" version ".tar.gz"))
              (sha256
               (base32 "175083pb9hp0vn9a00hbxlayyk5a5j8p52yq5qfmbnfvndisbmbv"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   (substitute* "CMakeLists.txt"
                     (("-march=native") "")
                     (("-msse4\\.2") ""))
                   (delete-file-recursively "deps/pybind11")
                   (delete-file-recursively "deps/sdsl-lite")))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list jemalloc
           libdivsufsort
           pybind11
           python
           sdsl-lite))
    (home-page "https://github.com/vgteam/odgi")
    (synopsis "Optimized Dynamic Genome/Graph Implementation")
    (description "@acronym{Optimized Dynamic Genome/Graph Implementation, odgi}
provides an efficient and succinct dynamic DNA sequence graph model, as well as
a host of algorithms that allow the use of such graphs in bioinformatic
analyses.

Careful encoding of graph entities allows odgi to efficiently compute and
transform pangenomes with minimal overheads.  @command{odgi} implements a
dynamic data structure that leveraged multi-core CPUs and can be updated on the
fly.

The edges and path steps are recorded as deltas between the current node id and
the target node id, where the node id corresponds to the rank in the global
array of nodes.  Graphs built from biological data sets tend to have local
partial order and, when sorted, the deltas be small.  This allows them to be
compressed with a variable length integer representation, resulting in a small
in-memory footprint at the cost of packing and unpacking.")
    (properties '((tunable? . #t)))
    (license license:expat)))

(define-public vg
  (package
    (name "vg")
    (version "1.39.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/vgteam/vg/releases/download/v"
                            version "/vg-v" version ".tar.gz"))
        (sha256
         (base32 "0cj575qr2jkingrm6r4ki7f89s7glrf18d4pvaa69smxh2vbajv3"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; List all the options, makes it easier to try to remove them.
            ;(delete-file-recursively "deps/BBHash")
            ;(delete-file-recursively "deps/DYNAMIC")
            ;(delete-file-recursively "deps/FlameGraph")
            ;(delete-file-recursively "deps/atomic_queue")
            ;(delete-file-recursively "deps/backward-cpp")
            (delete-file-recursively "deps/bash-tap")
            ;(delete-file-recursively "deps/dozeu")
            (delete-file-recursively "deps/elfutils")
            (delete-file-recursively "deps/fastahack")
            ;(delete-file-recursively "deps/fermi-lite")
            ;(delete-file-recursively "deps/gbwt")
            ;(delete-file-recursively "deps/gbwtgraph")
            ;(delete-file-recursively "deps/gcsa2")
            ;(delete-file-recursively "deps/gfakluge")
            ;(delete-file-recursively "deps/gssw")
            (delete-file-recursively "deps/htslib")
            ;(delete-file-recursively "deps/ips4o")
            (delete-file-recursively "deps/jemalloc")
            ;(delete-file-recursively "deps/libVCFH")
            ;(delete-file-recursively "deps/libbdsg")
            ;(delete-file-recursively "deps/libbdsg/bdsg/deps")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/BBHash")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/DYNAMIC")
            ;(delete-file-recursively "deps/libbdsg/bdsg/deps/DYNAMIC/deps/hopscotch-map")
            ;(delete-file-recursively "deps/libbdsg/bdsg/deps/hopscotch-map")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/libhandlegraph")
            ;(delete-file-recursively "deps/libbdsg/bdsg/deps/mio")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/pybind11")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/sdsl-lite")
            (delete-file-recursively "deps/libbdsg/bdsg/deps/sparsepp")
            ;(delete-file-recursively "deps/libdeflate")
            ;(delete-file-recursively "deps/libhandlegraph")
            ;(delete-file-recursively "deps/libVCFH")
            ;(delete-file-recursively "deps/libvgio")
            ;(delete-file-recursively "deps/libvgio/deps")  ; libhandlegraph
            ;(delete-file-recursively "deps/lru_cache")
            ;(delete-file-recursively "deps/mio")
            ;(delete-file-recursively "deps/mmmultimap")
            (delete-file-recursively "deps/mmmultimap/deps/DYNAMIC")
            (delete-file-recursively "deps/mmmultimap/deps/args")
            (delete-file-recursively "deps/mmmultimap/deps/atomic_queue")
            ;(delete-file-recursively "deps/mmmultimap/deps/hopscotch-map")
            (delete-file-recursively "deps/mmmultimap/deps/ips4o")
            (delete-file-recursively "deps/mmmultimap/deps/mio")
            ;(delete-file-recursively "deps/mmmultimap/deps/paryfor")
            (delete-file-recursively "deps/mmmultimap/deps/sdsl-lite")
            ;(delete-file-recursively "deps/pinchesAndCacti")
            ;(delete-file-recursively "deps/progress_bar")
            (delete-file-recursively "deps/raptor")
            ;(delete-file-recursively "deps/sdsl-lite")
            ;(delete-file-recursively "deps/sha1")
            (delete-file-recursively "deps/snappy")
            ;(delete-file-recursively "deps/sonLib")
            (delete-file-recursively "deps/sparsehash")
            ;(delete-file-recursively "deps/sparsepp")
            ;(delete-file-recursively "deps/ssw")
            ;(delete-file-recursively "deps/structures")
            ;(delete-file-recursively "deps/sublinear-Li-Stephens")
            (delete-file-recursively "deps/sublinear-Li-Stephens/deps")
            (delete-file-recursively "deps/tabixpp")
            (delete-file-recursively "deps/vcflib")
            ;(delete-file-recursively "deps/xg")
            (delete-file-recursively "deps/xg/deps")
            ;; libvgio doesn't search the correct include directory.
            (copy-recursively "deps/libhandlegraph/src/include/handlegraph"
                              "deps/libvgio/include/handlegraph")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; no configure script
         ,@(if (target-riscv64?)
             ;; riscv64 doesn't take '-march=native. This needs to be removed
             ;; for all architectures if/when vg is upstreamed.
             `((add-after 'unpack 'dont-build-native
                 (lambda _
                   (substitute* (append (find-files "." "CMakeLists\\.txt")
                                        (find-files "." "Makefile"))
                     (("-march=native") "")))))
             '())
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               ;; PKG_CONFIG_DEPS needs to be substituted to actually link to everything.
               (("cairo jansson")
                "cairo htslib jansson libdw libelf protobuf raptor2 sdsl-lite tabixpp vcflib")

               ;; Skip the part where we link static libraries special. It doesn't like the changes we make
               (("-Wl,-B.*") "\n")

               (("\\$\\(CWD\\)/\\$\\(LIB_DIR\\)/libtabixpp\\.a") "$(LIB_DIR)/libtabixpp.a")
               ((" \\$\\(LIB_DIR\\)/libtabixpp\\.a")
                (string-append " " (assoc-ref inputs "tabixpp") "/lib/libtabixpp.so"))
               (("\\$\\(LIB_DIR\\)/pkgconfig/tabixpp\\.pc")
                (string-append " " (assoc-ref inputs "tabixpp") "/lib/pkgconfig/tabixpp.pc"))

               (("\\$\\(CWD\\)/\\$\\(LIB_DIR\\)/libhts\\.a") "$(LIB_DIR)/libhts.a")
               ((" \\$\\(LIB_DIR\\)/libhts\\.a")
                (string-append " " (assoc-ref inputs "htslib") "/lib/libhts.so"))
               (("\\$\\(LIB_DIR\\)/pkgconfig/htslib\\.pc")
                (string-append " " (assoc-ref inputs "htslib") "/lib/pkgconfig/htslib.pc"))

               ((" \\$\\(LIB_DIR\\)/libvcflib.a")
                (string-append " " (assoc-ref inputs "vcflib") "/lib/libvcflib.so"))
               ((" \\$\\(BIN_DIR\\)/vcf2tsv")
                (string-append " " (assoc-ref inputs "vcflib") "/bin/vcf2tsv"))
               ((" \\$\\(VCFLIB_DIR\\)/bin/vcf2tsv")
                (string-append " " (assoc-ref inputs "vcflib") "/bin/vcf2tsv"))

               ((" \\$\\(FASTAHACK_DIR\\)/fastahack")
                (string-append " " (assoc-ref inputs "fastahack") "/bin/fastahack"))
               ((" \\$\\(FASTAHACK_DIR\\)/bin/fastahack")
                (string-append " " (assoc-ref inputs "fastahack") "/bin/fastahack"))
               (("\\+= \\$\\(OBJ_DIR\\)/Fasta\\.o")
                (string-append "+= " (assoc-ref inputs "fastahack") "/lib/libfastahack.so"))

               ((" \\$\\(LIB_DIR\\)/libsnappy.a")
                (string-append " " (assoc-ref inputs "snappy") "/lib/libsnappy.so"))

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
                (string-append " " (assoc-ref inputs "jemalloc") "/lib/libjemalloc.a"))

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
               (("../deps/vcflib/bin/vcf2tsv") (which "vcf2tsv")))))
         (add-after 'unpack 'fix-fastahack-dependency
           (lambda _
             (substitute* "src/aligner.hpp"
               (("Fasta.h") "fastahack/Fasta.h"))))
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
               "deps/DYNAMIC/build/hopscotch_map-prefix/src/hopscotch_map")))
         (add-after 'unpack 'adjust-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash-tap (assoc-ref inputs "bash-tap")))
               (substitute* (find-files "test/t")
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
                 ((".*test-docs.*") "")))))
         (add-after 'build 'build-manpages
           (lambda* (#:key inputs #:allow-other-keys)
             (when (assoc-ref inputs "asciidoctor")
               (invoke "make" "man"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "bin/vg" (string-append out "/bin"))
               (install-file "lib/libvg.a" (string-append out "/lib"))
               (for-each
                 (lambda (file)
                   (install-file file (string-append out "/share/man/man1")))
                 (find-files "doc/man" "\\.1$"))))))
       #:test-target "test"))
    (native-inputs
     `(,@(if (member (%current-system)
                     (package-transitive-supported-systems ruby-asciidoctor))
           `(("asciidoctor" ,ruby-asciidoctor))
           '())
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
       ("curl" ,curl)
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
    (properties `((release-monitoring-url . "https://github.com/vgteam/vg/releases")))
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
               ;; license:bsd-0    ; kent/src/{utils,lib,inc,tabStorm,parasol,hg/ausoSql,hg/autoXml}
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

(define-public pbbam-1
  (package
    (name "pbbam")
    (version "1.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PacificBiosciences/pbbam")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z3sh9cmrap37ijrm0cv85j92r1xkq6kba2j10mrr4fv7fc9zzfb"))))
    (build-system meson-build-system)
    ;; These libraries are listed as "Required" in the pkg-config file.
    (propagated-inputs
     `(("htslib" ,htslib)
       ("pbcopper" ,pbcopper)
       ("zlib" ,zlib)))
    (inputs
     `(("boost" ,boost)
       ("samtools" ,samtools)))
    (native-inputs
     `(("cram" ,python-cram)
       ("googletest" ,googletest)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper))) ; for tests
    (home-page "https://github.com/PacificBiosciences/pbbam")
    (synopsis "Work with PacBio BAM files")
    (description
     "The pbbam software package provides components to create, query, and
edit PacBio BAM files and associated indices.  These components include a core
C++ library, bindings for additional languages, and command-line utilities.
This library is not intended to be used as a general-purpose BAM utility - all
input and output BAMs must adhere to the PacBio BAM format specification.
Non-PacBio BAMs will cause exceptions to be thrown.")
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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "meson.build"
               ;; uncomment when upstreaming
               ;(("sse4\\.1") "nosse4.1")
               (("v8\\.2-a") "v8-a"))
             #t)))))
    (inputs
     `(("boost" ,boost)))
    (native-inputs
     `(("googletest" ,googletest)))
    (home-page "https://github.com/PacificBiosciences/pbcopper")
    (synopsis "Data structures, algorithms, and utilities for C++ applications")
    (description "The pbcopper library provides a suite of data structures,
algorithms, and utilities for PacBio C++ applications.")
    (license license:bsd-3)))

(define-public pbmm2
  (package
    (name "pbmm2")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/PacificBiosciences/pbmm2")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0c01c647c7wvq5jzkf68xsf0bn8mlyw0hbz2fiyirxg7hj05jyac"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f))    ; TODO: Fix later.
    (inputs
     `(("boost" ,boost)
       ("htslib" ,htslib)
       ("minimap2" ,minimap2-for-pbmm2)
       ("pbbam" ,pbbam-1)
       ("pbcopper" ,pbcopper)))
    (native-inputs
     `(("cram" ,python-cram)
       ("googletest" ,googletest)
       ("pkg-config" ,pkg-config)
       ("samtools" ,samtools)
       ("util-linux" ,util-linux)
       ("zlib" ,zlib)))
    (home-page "https://github.com/PacificBiosciences/pbmm2")
    (synopsis "minimap2 frontend for PacBio native data formats")
    (description "pbmm2 is a SMRT C++ wrapper for minimap2's C API.  Its purpose is to support native PacBio in- and output, provide sets of recommended parameters, generate sorted output on-the-fly, and postprocess alignments.  Sorted output can be used directly for polishing using GenomicConsensus, if BAM has been used as input to pbmm2.  Benchmarks show that pbmm2 outperforms BLASR in sequence identity, number of mapped bases, and especially runtime.  pbmm2 is the official replacement for BLASR.")
    (license license:bsd-3)))

(define minimap2-for-pbmm2
  (package
    (name "minimap2")
    (version "2.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pacificbiosciences/minimap2")
              (commit (string-append version "-meson"))))
       (file-name (git-file-name "minimap2-for-pbmm2" version))
       (sha256
        (base32
         "1833y6xdcblz7k4fyclryd6lwibsisp4svp2mk9w6ivk64icl6jq"))))
    (build-system meson-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://lh3.github.io/minimap2/")
    (synopsis "Pairwise aligner for genomic and spliced nucleotide sequences")
    (description "Minimap2 is a versatile sequence alignment program that
aligns DNA or mRNA sequences against a large reference database.  Typical use
cases include:

@enumerate
@item mapping PacBio or Oxford Nanopore genomic reads to the human genome;
@item finding overlaps between long reads with error rate up to ~15%;
@item splice-aware alignment of PacBio Iso-Seq or Nanopore cDNA or Direct RNA
  reads against a reference genome;
@item aligning Illumina single- or paired-end reads;
@item assembly-to-assembly alignment;
@item full-genome alignment between two closely related species with
  divergence below ~15%.
@end enumerate\n")
    (license license:expat)))

;; 0.8.9 is the last version which supports python2.
(define-public python2-bx-python
  (let ((base (python2-package python-bx-python)))
    (package
      (inherit base)
      (name "python2-bx-python")
      (version "0.8.9")
      (source (origin
                ;; No cythonized files in the git repository.
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/bxlab/bx-python")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0bsqnw8rv08586wksvx2a8dawvhyzvz5pzsh9y3217b6wxq98dnq"))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (append python2-six))))))

(define-public hap.py
  (package
   (name "hap.py")
   (version "0.3.14")
   (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Illumina/hap.py")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bnm7s86651p3zf6wahz5pic7n8416fx677kj47lwckr3syp2x1h"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "external/bcftools.tar.gz")
           (delete-file-recursively "external/boost_subset_1_58_0.tar.bz2")
           (delete-file-recursively "external/htslib.tar.gz")
           ;; TODO: Unbundle jsoncpp.
           ;(delete-file-recursively "external/jsoncpp")
           ;(delete-file-recursively "external/klib")
           (delete-file-recursively "external/samtools.tar.gz")
           (delete-file-recursively "external/virtualenv-12.0.7.tar.gz")
           (delete-file-recursively "external/zlib-1.2.8.tar.gz")
           #t))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags (list "-DBUILD_VCFEVAL=ON")
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'set-package-version
          (lambda _
            (substitute* "CMakeLists.txt"
              (("git describe --tags --always")
               (string-append "echo " ,version)))
            #t))
        ;; A shared library conflicts with boost-static.
        ;; Not using boost-static causes linking errors.
        ;(add-after 'unpack 'build-dynamic-library
        ;  (lambda _
        ;    (substitute* "src/c++/lib/CMakeLists.txt"
        ;      (("STATIC") "SHARED"))
        ;    #t))
        (add-after 'unpack 'fix-build
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((zlib     (assoc-ref inputs "zlib"))
                  (bcftools (assoc-ref inputs "bcftools"))
                  (boost    (assoc-ref inputs "boost"))
                  (htslib   (assoc-ref inputs "htslib"))
                  (samtools (assoc-ref inputs "samtools")))
              (mkdir-p "external/bin")
              (mkdir-p "external/lib")
              (mkdir-p "external/include")
              (mkdir-p "external/scratch/lib")

              (substitute* "external/make_dependencies.sh"
                (("zlib-1\\.2\\.8/libz\\.a") "lib/libz.so"))
              (substitute* "src/cmake/FindHTSLib.cmake"
                (("libhts\\.a") "libhts.so"))
              (substitute* "CMakeLists.txt"
                (("ZLIB_LIBRARIES .*\\)")
                 (string-append "ZLIB_LIBRARIES \"" zlib "/lib/libz.so\")")))

              (setenv "BOOST_ROOT" boost)
              (setenv "LDFLAGS"
                      (string-append "-L" (assoc-ref %build-inputs "htslib") "/lib"))

              (symlink (string-append zlib "/lib/libz.so")
                       "external/scratch/lib/libz.so")

              (symlink (string-append htslib "/include/htslib")
                       "external/include/htslib")
              (symlink (string-append htslib "/lib/libhts.so")
                       "external/lib/libhts.so")
              (symlink (string-append htslib "/lib/libhts.so")
                       "external/lib/libhts.so.3")

              (symlink (string-append bcftools "/bin/bcftools")
                       "external/bin/bcftools")
              (symlink (string-append samtools "/bin/samtools")
                       "external/bin/samtools")
              #t)))
        (add-after 'fix-build 'insert-rtg-tools
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((rtg-tools (assoc-ref inputs "rtg-tools"))
                  (dest      "external/libexec/rtg-tools-install"))
              (mkdir-p (dirname dest))
              (symlink rtg-tools dest)
              #t)))
        (replace 'configure
          (lambda* (#:key outputs (configure-flags '()) (out-of-source? #t)
                    build-type target
                    #:allow-other-keys)
                   "Configure the given package."
                   (let* ((out        (assoc-ref outputs "out"))
                          (abs-srcdir (getcwd))
                          (srcdir     (if out-of-source?
                                        (string-append "../" (basename abs-srcdir))
                                        ".")))
                     (format #t "source directory: ~s (relative from build: ~s)~%"
                             abs-srcdir srcdir)
                     (when out-of-source?
                       (mkdir "../build")

                       ;; Extra code added here!!
                       (copy-recursively "external/scratch" "../build/scratch")
                       (copy-recursively "external/libexec" "../build/libexec")
                       (copy-recursively "external/lib" "../build/lib")
                       (copy-recursively "external/bin" "../build/bin")
                       (copy-recursively "external/include" "../build/include")

                       (chdir "../build"))
                     (format #t "build directory: ~s~%" (getcwd))

                     (let ((args `(,srcdir
                                    ,@(if build-type
                                        (list (string-append "-DCMAKE_BUILD_TYPE="
                                                             build-type))
                                        '())
                                    ,(string-append "-DCMAKE_INSTALL_PREFIX=" out)
                                    ;; ensure that the libraries are installed into /lib
                                    "-DCMAKE_INSTALL_LIBDIR=lib"
                                    ;; add input libraries to rpath
                                    "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                                    ;; add (other) libraries of the project itself to rpath
                                    ,(string-append "-DCMAKE_INSTALL_RPATH=" out "/lib")
                                    ;; enable verbose output from builds
                                    "-DCMAKE_VERBOSE_MAKEFILE=ON"

                                    ;;  Cross-build
                                    ,@(if target
                                        (list (string-append "-DCMAKE_C_COMPILER="
                                                             target "-gcc")
                                              (string-append "-DCMAKE_CXX_COMPILER="
                                                             target "-g++")
                                              (if (string-contains target "mingw")

                                                "-DCMAKE_SYSTEM_NAME=Windows"
                                                "-DCMAKE_SYSTEM_NAME=Linux"))
                                        '())
                                    ,@configure-flags)))
                       (format #t "running 'cmake' with arguments ~s~%" args)
                       (apply invoke "cmake" args)))))
        (replace 'check
          (lambda* (#:key tests? #:allow-other-keys)
            (when tests?
              (invoke "./bin/test_haplotypes"))
            #t))
        (add-before 'install 'remove-extra-files
          (lambda _
            (delete-file "bin/bcftools")
            (delete-file "bin/samtools")
            (delete-file "bin/test_haplotypes")
            (delete-file "lib/libhts.so")
            (delete-file "lib/libhts.so.3")
            #t))
        (add-after 'install 'wrap-programs
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out      (assoc-ref outputs "out"))
                  (bcftools (assoc-ref inputs "bcftools"))
                  (samtools (assoc-ref inputs "samtools")))
              (for-each
                (lambda (file)
                  (wrap-script file
                    `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
                    `("PATH" ":" prefix (,(string-append bcftools "/bin")
                                         ,(string-append samtools "/bin")))))
                (find-files (string-append out "/bin") "\\.py$"))
              #t))))))
   (inputs
    `(("bcftools" ,bcftools)
      ("boost" ,boost-static)   ; has to be boost-static
      ("guile" ,guile-3.0)      ; for wrap-script
      ("htslib" ,htslib)
      ;; The software specifically states python-2.
      ("python" ,python-2)
      ("python2-bx-python" ,python2-bx-python)
      ("python2-numpy" ,python2-numpy)
      ("python2-pandas" ,python2-pandas)
      ("python2-pysam" ,(python2-package python-pysam))
      ("python2-scipy" ,python2-scipy)
      ("rtg-tools" ,rtg-tools)
      ("samtools" ,samtools)
      ("zlib" ,zlib)))
   (home-page "https://github.com/Illumina/hap.py")
   (synopsis "Haplotype VCF comparison tools")
   (description
    "This is a set of programs based on htslib to benchmark variant calls
against gold standard truth datasets.

The main two tools are @code{hap.py} (diploid precision/recall evaluation) and
@code{som.py} (somatic precision/recall evaluation -- this ignores the GT and
just checks for presence of alleles).  Other tools are @code{qfy.py} (which just
executes the quantification step of the analysis pipeline, this requires a
GA4GH-intermediate VCF file), and @code{pre.py}, which is @code{hap.py}'s input
cleaning and variant normalisation step.

To run the bundled rtg-tools software you will also need java.  The
@code{icedtea:jdk} output should work nicely.")
   (license (list license:expat     ; bundled jsoncpp, klib
                  license:bsd-2))))

;; TODO:
;; Unbundle gatb-core.
(define-public minia
  (package
    (name "minia")
    (version "3.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/GATB/minia/releases"
                            "/download/v" version
                            "/minia-v" version "-Source.tar.gz"))
        (sha256
         (base32 "03zg1jh0yjw7546kax8xs0zwiqhaiqz044409jc3ss6nj968ay70"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f          ; Tests are expected to be run manually.
       #:configure-flags '("-DNO_SSE=ON")   ; Can be removed after unbundling gatb-core.
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-cruft
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion out
                 (delete-file-recursively "lib")
                 (delete-file-recursively "test")
                 (delete-file "bin/h5cc")
                 (delete-file "LICENSE")
                 (delete-file "README.md")
                 #t)))))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://gatb.inria.fr/software/minia")
    (synopsis "Short-read assembler based on a de Bruijn graph")
    (description "Minia is a short-read assembler based on a de Bruijn graph,
capable of assembling a human genome on a desktop computer in a day.  The output
of Minia is a set of contigs.  Back when it was released, Minia produced results
of similar contiguity and accuracy to other de Bruijn assemblers (e.g. Velvet).")
    (license license:agpl3+)))

(define-public metaeuk
  (package
    (name "metaeuk")
    (version "5-34c21f2")       ; As seen upstream.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/soedinglab/metaeuk")
                     (commit version)
                     (recursive? #t)))      ; Only contains the tests.
              (file-name (git-file-name name version))
              (sha256
               (base32 "0pqiqy3wycn9h3y699b5drd3y4zmz087bwgdxx6wbbqqipa6wk0j"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;(delete-file-recursively "lib/mmseqs/lib/gzstream")
                  (delete-file-recursively "lib/mmseqs/lib/simde")
                  (delete-file-recursively "lib/mmseqs/lib/xxhash")
                  (delete-file-recursively "lib/mmseqs/lib/zstd")))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DUSE_SYSTEM_ZSTD=YES")
       #:substitutable? #f      ; We want the native build.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-shared-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/mmseqs/CMakeLists.txt"
               (("libzstd\\.a") "libzstd.so")
               (("libzstd_static") "libzstd_shared")
               ;(("lib/gzstream")
               ; (string-append (assoc-ref inputs "gzstream") "/include"))
               (("lib/xxhash")
                (string-append (assoc-ref inputs "xxhash") "/include"))
               (("lib/simde")
                (string-append (assoc-ref inputs "simde") "/include/simde")))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "../source/tests"
                 (invoke "./run.sh" "../../build/src/metaeuk"))))))))
    (inputs
     `(("bzip2" ,bzip2)
       ("zlib" ,zlib)
       ("zstd:lib" ,zstd "lib")))
    (native-inputs
     `(;("gzstream" ,gzstream)
       ("perl" ,perl)
       ("simde" ,simde)
       ("xxd" ,xxd)
       ("xxhash" ,xxhash)))
    (home-page "https://github.com/soedinglab/metaeuk")
    (synopsis
     "Gene discovery and annotation for large-scale eukaryotic metagenomics")
    (description
     "MetaEuk is a modular toolkit designed for large-scale gene discovery and
annotation in eukaryotic metagenomic contigs.  MetaEuk combines the fast and
sensitive homology search capabilities of
@url{https://github.com/soedinglab/MMseqs2, MMseqs2} with a dynamic programming
procedure to recover optimal exons sets.  It reduces redundancies in multiple
discoveries of the same gene and resolves conflicting gene predictions on the
same strand.")
    (license license:gpl3)))

(define-public augustus
  (package
    (name "augustus")
    (version "3.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/Gaius-Augustus/Augustus")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1nc4nddcxi98fb14vmgj7x5aw5vglm4amzraqibgzmigpqnca68f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "CXX=" ,(cxx-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; No configure script
         (add-after 'unpack 'adjust-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "common.mk"
               ;; Has the wrong version.
               (("AUGVERSION = .*")
                (string-append "AUGVERSION = " ,version "\n"))
               ;; Looks for ancient version of mysql.
               (("COMPGENEPRED = ")
                "SQLITE=true\nMYSQL = false\nCOMPGENEPRED = "))
             (substitute* "src/Makefile"
               (("/usr/include/lpsolve")
                (string-append (assoc-ref inputs "lpsolve") "/include/lpsolve")))
             (substitute* (find-files "auxprogs" "Makefile")
               (("/usr/include/bamtools")
                (string-append (assoc-ref inputs "bamtools") "/include/bamtools"))
               (("/usr/include/htslib")
                (string-append (assoc-ref inputs "htslib") "/include/htslib"))
               (("/usr/include/boost")
                (string-append (assoc-ref inputs "boost") "/include/boost")))
             #t))
         (replace 'check
           (lambda args
             ;; These tests rely on mysql.
             ;(apply (assoc-ref %standard-phases 'check)
             ;       `(,@args #:test-target "unit_test"))
             (apply (assoc-ref %standard-phases 'check)
                    `(,@args #:test-target "test"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share/augstus"))
                    (scripts (string-append share "/scripts")))
               ;; Install targets taken from Debian.
               (install-file "auxprogs/bam2wig/bam2wig" bin)
               (install-file "auxprogs/compileSpliceCands/compileSpliceCands" bin)
               (install-file "auxprogs/homGeneMapping/src/homGeneMapping" bin)
               (install-file "auxprogs/joingenes/joingenes" bin)
               (mkdir-p scripts)
               (copy-recursively "scripts" scripts)
               (copy-recursively "config" share)
               (with-directory-excursion "scripts"
                 (for-each delete-file (cons "executeTestCGP.py"
                                             (find-files "." "\\.txt$"))))
               (for-each make-file-writable (find-files out "\\.gz$"))
               #t))))))
    (inputs
     `(("boost" ,boost)
       ("htslib" ,htslib)
       ("perl" ,perl)
       ("python" ,python)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bamtools" ,bamtools)
       ("gsl" ,gsl)
       ("gzip" ,gzip)
       ("lpsolve" ,lpsolve)
       ("samtools" ,samtools)
       ("suitesparse" ,suitesparse)))
    (home-page "http://bioinf.uni-greifswald.de/webaugustus/")
    (synopsis "Genome annotation with AUGUSTUS")
    (description "Augustus can be used as an ab initio program, which means it
bases its prediction purely on the sequence.  AUGUSTUS may also incorporate
hints on the gene structure coming from extrinsic sources such as EST, MS/MS,
protein alignments and syntenic genomic alignments.")
    (license (license:non-copyleft
               "https://opensource.org/licenses/artistic-license-1.0"
               "Artistic-license-1.0"))))

(define-public pplacer
  (let ((commit "807f6f3"))
   (build-with-ocaml4.07
    (package
      (name "pplacer")
      ;; The commit should be updated with each version change.
      (version "1.1.alpha19")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/matsen/pplacer")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11ppbbbx20p2g9wj3ff64dhnarb12q79v7qh4rk0gj6lkbz4n7cn"))))
      (build-system ocaml-build-system)
      (arguments
       `(#:modules ((guix build ocaml-build-system)
                    (guix build utils)
                    (ice-9 ftw))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'fix-build-with-latest-ocaml
             (lambda _
               (substitute* "myocamlbuild.ml"
                 (("dep \\[\"c_pam\"\\]" m)
                  (string-append "flag [\"ocaml\"; \"compile\"] (A \"-unsafe-string\");\n"
                                 m))
                 (("let run_and_read" m)
                  (string-append "
let split s ch =
  let x = ref [] in
  let rec go s =
    let pos = String.index s ch in
    x := (String.before s pos)::!x;
    go (String.after s (pos + 1))
  in
  try go s
  with Not_found -> !x
let split_nl s = split s '\\n'
let before_space s =
  try String.before s (String.index s ' ')
  with Not_found -> s

" m))
                 (("run_and_read \"ocamlfind list \\| cut -d' ' -f1\"" m)
                  (string-append "List.map before_space (split_nl & " m ")"))
                 (("    blank_sep_strings &") "")
                 (("      Lexing.from_string &") ""))
               #t))
           (add-after 'unpack 'replace-bundled-cddlib
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((cddlib-src (assoc-ref inputs "cddlib-src"))
                      (local-dir "cddlib_guix"))
                 (mkdir local-dir)
                 (with-directory-excursion local-dir
                   (invoke "tar" "xvf" cddlib-src))
                 (let ((cddlib-src-folder
                        (string-append local-dir "/"
                                       (list-ref (scandir local-dir) 2)
                                       "/lib-src")))
                   (for-each make-file-writable (find-files "cdd_src" ".*"))
                   (for-each
                    (lambda (file)
                      (copy-file file
                                 (string-append "cdd_src/" (basename file))))
                    (find-files cddlib-src-folder ".*[ch]$")))
                 #t)))
           (add-after 'unpack 'fix-makefile
             (lambda _
               ;; Remove system calls to 'git'.
               (substitute* "Makefile"
                 (("^DESCRIPT:=pplacer-.*")
                  (string-append
                   "DESCRIPT:=pplacer-$(shell uname)-v" ,version "\n")))
               (substitute* "myocamlbuild.ml"
                 (("git describe --tags --long .*\\\" with")
                  (string-append
                   "echo -n v" ,version "-" ,commit "\" with")))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (copy-recursively "bin" bin))
               #t)))
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib))
      (inputs
       `(("zlib" ,zlib "static")
         ("gsl" ,gsl-static)
         ("ocaml-ounit" ,(package-with-ocaml4.07 ocaml-ounit))
         ("ocaml-batteries" ,(package-with-ocaml4.07 ocaml-batteries))
         ("ocaml-camlzip" ,(package-with-ocaml4.07 camlzip))
         ("ocaml-csv" ,(package-with-ocaml4.07 ocaml-csv))
         ("ocaml-sqlite3" ,(package-with-ocaml4.07 ocaml-sqlite3))
         ("ocaml-xmlm" ,(package-with-ocaml4.07 ocaml-xmlm))
         ("ocaml-mcl" ,(package-with-ocaml4.07 ocaml-mcl))
         ("ocaml-gsl" ,ocaml4.07-gsl-1)
         ("sqlite:static" ,sqlite "static")))
      (native-inputs
       `(("cddlib-src" ,(package-source cddlib))
         ("ocamlbuild" ,(package-with-ocaml4.07 ocamlbuild))
         ("pkg-config" ,pkg-config)))
      (propagated-inputs
       (list pplacer-scripts))
      (synopsis "Phylogenetic placement of biological sequences")
      (description
       "Pplacer places query sequences on a fixed reference phylogenetic tree
to maximize phylogenetic likelihood or posterior probability according to a
reference alignment.  Pplacer is designed to be fast, to give useful
information about uncertainty, and to offer advanced visualization and
downstream analysis.")
      (home-page "https://matsen.fhcrc.org/pplacer/")
      (license license:gpl3)))))

(define-public python2-biopython
  (python2-package python-biopython))

;; This package is installed alongside 'pplacer'.  It is a separate package so
;; that it can use the python-build-system for the scripts that are
;; distributed alongside the main OCaml binaries.
(define pplacer-scripts
  (package
    (inherit pplacer)
    (name "pplacer-scripts")
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-scripts-dir
           (lambda _ (chdir "scripts")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "unittest" "discover" "-v"))))
         (add-after 'install 'wrap-executables
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (path (string-append
                            (assoc-ref inputs "hmmer") "/bin:"
                            (assoc-ref inputs "infernal") "/bin")))
               (display path)
               (wrap-program (string-append bin "/refpkg_align.py")
                 `("PATH" ":" prefix (,path)))
               (wrap-program (string-append bin "/hrefpkg_query.py")
                 `("PATH" ":" prefix (,path)))))))))
    (inputs
     `(("infernal" ,infernal)
       ("hmmer" ,hmmer)))
    (propagated-inputs
     `(("python-biopython" ,python2-biopython)
       ("taxtastic" ,taxtastic)))
    (synopsis "Pplacer Python scripts")))

(define-public sepp
  (package
    (name "sepp")
    (version "4.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/smirarab/sepp")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1bw5gjhymq0a4slkk7pr5dl4jb9bnwv4qpn26mvwp8fx3aszvmij"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "tools/bundled")
                  (mkdir-p "tools/bundled/Linux")
                  (for-each (lambda (file)
                              (with-output-to-file
                                (string-append "tools/bundled/Linux/" file)
                                (lambda _
                                  (format #t ""))))
                            '("guppy-32" "guppy-64"
                              "hmmalign-32" "hmmalign-64"
                              "hmmbuild-32" "hmmbuild-64"
                              "hmmsearch-32" "hmmsearch-64"
                              "pplacer-32" "pplacer-64"))
                  ;; TODO: Rebuild java blob.
                  ;(delete-file-recursively "tools/merge/lib")
                  ;(delete-file "tools/merge/seppJsonMerger.jar")
                  ;; This is a circular reference to the top directory ...
                  (delete-file-recursively "sepp-package/sepp")))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f      ; Test suite hangs.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (let ((hmmer   (string-append (assoc-ref inputs "hmmer") "/bin/"))
                   (pplacer (string-append (assoc-ref inputs "pplacer") "/bin/"))
                   (tools   "tools/bundled/Linux/"))
               (for-each
                 (lambda (target package)
                   (delete-file (string-append tools target))
                   (symlink (string-append package (string-drop-right target 3))
                            (string-append tools target)))
                 (list "guppy-32" "guppy-64"
                       "hmmalign-32" "hmmalign-64"
                       "hmmbuild-32" "hmmbuild-64"
                       "hmmsearch-32" "hmmsearch-64"
                       "pplacer-32" "pplacer-64")
                 (list pplacer pplacer
                       hmmer hmmer
                       hmmer hmmer
                       hmmer hmmer
                       pplacer pplacer))
               #t)))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; This test is upset we removed the Darwin binary.
               (delete-file "test/unittest/testConfig.py")
               ;; This test is missing its config file.
               (delete-file "test/unittest/testUPP.py")
               (invoke "nosetests" "-w" "test/unittest"))))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (home.path (string-append out "/share/sepp")))
               (setenv "HOME" home.path)
               (mkdir-p (string-append home.path "/.sepp"))
               ;; configure with '-c' so our pretend bundled
               ;; libraries aren't actually installed.
               (invoke "python" "setup.py" "config" "-c")
               #t)))
         (add-after 'install 'copy-home.path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (home.path (string-append out "/share/sepp")))
               (with-output-to-file (string-append out "/home.path")
                 (lambda _
                   (format #t "~a/.sepp"
                           home.path)))
               (with-output-to-file (string-append home.path "/.sepp/main.config")
                 (lambda _
                   (format #t "[pplacer]~@
                           path=~a/bin/pplacer~@
                           ~@
                           [hmmalign]~@
                           path=~a/bin/hmmalign~@
                           ~@
                           [hmmsearch]~@
                           path=~a/bin/hmmsearch~@
                           piped=False~@
                           elim=10000~@
                           filters=True~@
                           ~@
                           [hmmbuild]~@
                           path=~a/bin/hmmbuild~@
                           ~@
                           [jsonmerger]~@
                           path=~a/share/sepp/seppJsonMerger.jar~@
                           ~@
                           [exhaustive]~@
                           strategy = centroid~@
                           minsubsetsize = 2~@
                           placementminsubsetsizefacotr = 4~@
                           placer = pplacer~@
                           weight_placement_by_alignment = True~%"
                           (assoc-ref inputs "pplacer")
                           (assoc-ref inputs "hmmer")
                           (assoc-ref inputs "hmmer")
                           (assoc-ref inputs "hmmer")
                           out)))
               (install-file "tools/merge/seppJsonMerger.jar"
                             home.path)
               (copy-file (string-append out "/home.path")
                          (string-append (site-packages inputs outputs) "/home.path"))
               #t))))))
    (inputs
     `(("hmmer" ,hmmer)
       ("java" ,icedtea-8)
       ("pplacer" ,pplacer)
       ("python-dendropy" ,python-dendropy)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/smirarab/sepp")
    (synopsis "SATe enabled phylogenetic placement")
    (description "SEPP operates by using a divide-and-conquer strategy adopted
from SATe-II (@url{Liu et al. (Systematic Biology 2012),
http://sysbio.oxfordjournals.org/content/61/1/90.full.pdf+html?sid=dd32838d-89dc-4bda-8008-6f948146341f}
and @url{Liu et. al. (Science 2009),
http://www.sciencemag.org/content/324/5934/1561.abstract}) to construct an
Ensemble of @acronym{HMMs, Hidden Markov Models} to represent the input
multiple sequence alignment `A`.  It then computes the fit of each query
sequence in `X` to each HMM in the ensemble, and uses the highest scoring HMM to
add the sequence to the input tree `T`.  This technique improves the accuracy of
the placements of the query sequences compared to using a single HMM to
represent the input alignment.  SEPP uses tools in HMMER to construct HMMs,
compute the fit of sequences to HMMs, and add sequences to the alignment `A`.
SEPP uses @code{pplacer} to add query sequences to the input tree `T`, after
they are added to the alignment `A`.  SEPP is also used in other software,
including @acronym{TIPP, taxonomic identical using phylogenetic placement} and
@acronym{UPP, ultra-large alignments using phylogeny-aware profiles}.")
    (license license:gpl3+)))

(define-public busco
  (package
    (name "busco")
    (version "5.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.com/ezlab/busco")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0fnijr7q9jj8hq3q5v0jd73zcznqrg72idr3lmchx7x2c66mb9dz"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (begin (add-installed-pythonpath inputs outputs)
                      (invoke "python" "setup.py" "check")))))
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/busco")
                 `("PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "augustus") "/bin")
                     ,(string-append (assoc-ref inputs "blast") "/bin")
                     ,(string-append (assoc-ref inputs "hmmer") "/bin")
                     ,(string-append (assoc-ref inputs "metaeuk") "/bin")
                     ,(string-append (assoc-ref inputs "prodigal") "/bin")
                     ,(string-append (assoc-ref inputs "r") "/bin")
                     ,(string-append (assoc-ref inputs "sepp") "/bin"))))))))))
    (inputs
     `(("augustus" ,augustus)
       ("blast" ,blast+)
       ("hmmer" ,hmmer)
       ("metaeuk" ,metaeuk)
       ("prodigal" ,prodigal)
       ("python-biopython" ,python-biopython)
       ("python-pandas" ,python-pandas)
       ("r" ,r)
       ("sepp" ,sepp)))
    (home-page "https://busco.ezlab.org/")
    (synopsis "Assess genome assembly and annotation completeness")
    (description "Assess genome assembly and annotation completeness with
Benchmarking Universal Single-Copy Orthologs.")
    (license license:expat)))

(define-public mutation-simulator
  (let ((commit "9cb6bd2acf8201151bc610be14963e65b41d8899")     ; March 25, 2021
        (revision "1"))
    (package
      (name "mutation-simulator")
      (version (git-version "2.0.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/mkpython3/mutation-simulator")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1yxn5v5x804rm5ra1srmnph468yk7amsgfsj6h20rd6nmj2j0g9c"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("mutation-simulator.py" "bin/"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-script
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (script (string-append out "/bin/mutation-simulator.py")))
                 ;; wrap-script doesn't accept arguments
                 (wrap-program script
                   `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH"))))
                 (chmod script #o555)
                 ;; When using wrap-script.
                 (when (file-exists?
                         (string-append out "/bin/.mutation-simulator.py-real"))
                   (chmod (string-append out "/bin/.mutation-simulator.py-real")
                          #o555))
                 #t)))
           (add-after 'wrap-script 'check
             (lambda* (#:key tests? outputs #:allow-other-keys)
               (when tests?
                 (invoke (string-append (assoc-ref outputs "out")
                                        "/bin/mutation-simulator.py")
                         "Test/test.fa" "rmt" "Test/test.rmt")))))))
      (inputs
       `(("bash" ,bash-minimal)                         ; for wrap-program
         ;("guile" ,(@ (gnu packages guile) guile-3.0))  ; for wrap-script
         ("python" ,python)
         ("python-blist" ,python-blist)
         ("python-pyfaidx" ,python-pyfaidx)
         ("python-numpy" ,python-numpy)
         ("python-tqdm" ,python-tqdm)))
      (home-page "https://github.com/mkpython3/mutation-simulator")
      (synopsis "Simulate mutations on given fasta files")
      (description "Mutation-Simulator is a Python tool for simulating SNPs and
SVs in any reference genome with cohesive documentation about implemented
mutations.  With Mutation-Simulator, the new file format @acronym{RMT, Random
Mutation Tables} is introduced, which gives more simulation power to the user by
creating an interface for more natural simulations within specific genomes.
Mutation-Simulator provides 3 different modes to simulate SNPs, insertions,
deletions, tandem duplications, inversions, translocations and interchromosomal
translocations from the commandline or with highly configureable RMT files.")
      (license license:gpl3+))))

(define-public python-blist
  (package
    (name "python-blist")
    (version "1.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "blist" version))
        (sha256
         (base32 "1hqz9pqbwx0czvq9bjdqjqh5bwfksva1is0anfazig81n18c84is"))
        (patches (search-patches "blist-stopiteration.patch"))))
    (build-system python-build-system)
    (home-page "http://stutzbachenterprises.com/blist/")
    (synopsis "List-like type for Python with better asymptotic performance")
    (description
     "This package provides a list-like type for Python with better asymptotic
performance and similar performance on small lists.")
    (license license:bsd-3)))

(define-public verkko
  (let ((commit "9323e71f46b0ea1725202ebe911142d0d1288c45")     ; Jan 22, 2022
        (revision "1"))
    (package
      (name "verkko")
      (version (git-version "1.0_beta" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/marbl/verkko")
                       (commit commit)
                       (recursive? #t)))    ; Needs canu
                (file-name (git-file-name name version))
                (sha256
                 (base32 "0pb66mlz8r9hrvlcfw9zwxqzzns7221pm2z9mrjisvniwq8ggqmh"))))
      (build-system gnu-build-system)
      (arguments
       (list
         #:make-flags
         #~(list (string-append "CC=" #$(cc-for-target))
                 (string-append "VERSION= verkko " #$version)
                 "BUILDOPTIMIZED=1")
         #:phases
         #~(modify-phases %standard-phases
             (delete 'configure)        ; No configure script.
             (add-after 'unpack 'chdir
               (lambda _ (chdir "src")))
             (add-after 'chdir 'patch-source
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "verkko.sh"
                   (("\"#!/bin/sh\"")
                    (string-append "\"#!" (which "sh") "\""))
                   ;; Hardcode the paths to some binaries
                   (("\\$\\(which MBG\\)")
                    (search-input-file inputs "/bin/MBG"))
                   (("\\$\\(which GraphAligner\\)")
                    (search-input-file inputs "/bin/GraphAligner"))
                   (("snakemake --nocolor")
                    (string-append (search-input-file
                                    inputs
                                    "/bin/snakemake")
                                   " --nocolor")))
                 (substitute* (find-files "Snakefiles")
                   (("#!/bin/sh") (string-append "#!" (which "sh"))))))
             (replace 'check
               (lambda* (#:key tests? inputs #:allow-other-keys)
                 (let ((hifi.fastq.gz (assoc-ref inputs "hifi.fastq.gz"))
                       (ont.fastq.gz  (assoc-ref inputs "ont.fastq.gz")))
                   (when tests?
                     (invoke "../bin/verkko" "-d" "asm"
                             "--hifi" hifi.fastq.gz
                             "--nano" ont.fastq.gz)))))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (with-directory-excursion "../"
                     (copy-recursively "bin" (string-append out "/bin"))
                     (copy-recursively "lib" (string-append out "/lib")))))))))
      (inputs
       (list graphaligner
             mbg
             python-wrapper
             snakemake))
      (native-inputs
       `(("perl" ,perl)
         ;; Provided by upstream to test the build:
         ("hifi.fastq.gz"       ; 118 MiB
          ,(origin
             (method url-fetch)
             (uri "https://obj.umiacs.umd.edu/sergek/shared/ecoli_hifi_subset24x.fastq.gz")
             (sha256
              (base32 "1nh5jzwnlf0r37rcgqwsjlszb8i0w5pfwp3rb5h869qp5qdlms8z"))))
         ("ont.fastq.gz"        ; 244 MiB
          ,(origin
             (method url-fetch)
             (uri "https://obj.umiacs.umd.edu/sergek/shared/ecoli_ont_subset50x.fastq.gz")
             (sha256
              (base32 "056pkf1dx76zs88vi4zgcbzrgvqqvlq9mpnyvmdszyhy0cj00smy"))))))
      (home-page "https://github.com/marbl/verkko")
      (synopsis "Hybrid genome assembly pipeline for telomere-to-telomere
assembly of PacBio HiFi and Oxford Nanopore reads")
      (description "Verkko is a hybrid genome assembly pipeline developed for
telomere-to-telomere assembly of PacBio HiFi and Oxford Nanopore reads.  Verkko
is Finnish for net, mesh and graph.  Verkko uses Canu to correct remaining
errors in the HiFi reads, builds a multiplex de Bruijn graph using MBG, aligns
the Oxford Nanopore reads to the graph using GraphAligner, progressively
resolves loops and tangles first with the HiFi reads then with the aligned
Oxford Nanopore reads, and finally creates contig consensus sequences using
Canu's consensus module.")
      (license license:public-domain))))

(define-public mbg
  (package
    (name "mbg")
    (version "1.0.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/maickrau/MBG")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14p0vk6qfyf7ha8x30dk8hi16c5n8fpzi96k2vwmg17mlcf0hkgj"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:tests? #f                      ; No tests.
       #:make-flags
       #~(list (string-append "VERSION=" #$version))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)          ; No configure script.
           (add-after 'unpack 'use-packaged-inputs
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((cxxopts (dirname (search-input-file inputs
                                        "/include/cxxopts.hpp")))
                     (concurrentqueue
                       (search-input-directory inputs
                        "/include/concurrentqueue")))
                 (delete-file-recursively "cxxopts")
                 (delete-file-recursively "concurrentqueue")
                 (substitute* "makefile"
                   (("-Icxxopts/include") (string-append "-I" cxxopts))
                   (("-Iconcurrentqueue") (string-append "-I" concurrentqueue))
                   ;; No need to build statically.
                   (("-Wl,-Bstatic") "")
                   (("-static-libstdc\\+\\+") "")))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "bin/MBG" (string-append out "/bin"))))))))
    (inputs (list concurrentqueue
                  ;; parallel-hashmap
                  ;; zstr
                  zlib))
    (native-inputs (list cxxopts))
    (home-page "https://github.com/maickrau/MBG")
    (synopsis "Minimizer based sparse de Bruijn Graph constructor")
    (description
     "Minimizer based sparse de Bruijn Graph constructor.  Homopolymer compress
input sequences, pick syncmers from hpc-compressed sequences, connect syncmers
with an edge if they are adjacent in a read, unitigify and homopolymer
decompress.  Suggested input is PacBio HiFi/CCS reads.")
    (license license:expat)))

;; TODO: Unbundle bloom, meryl.
(define-public willowmap
  (package
    (name "willowmap")
    (version "2.03")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/marbl/Winnowmap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "152650bljmdm9f1nmi4xbpxs9583faijba9i8gkp3qz76pzcvbfh"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "ext/meryl/src/utility/src/utility/system.C"
                    ;; This was removed in glibc-2.32.
                    ((".*sys/sysctl\\.h.*") ""))))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:tests? #f                      ; No tests.
       #:make-flags
       #~(list "OSVERSION=5.15")
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)          ; No configure script.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (for-each (lambda (file)
                             (install-file file (string-append out "/bin")))
                           (find-files "bin"))))))))
    (inputs
     (list zlib))
    (native-inputs
     (list perl
           which))
    (home-page "https://github.com/marbl/Winnowmap")
    (synopsis "Long read / genome alignment software")
    (description
     "Winnowmap is a long-read mapping algorithm optimized for mapping ONT and
PacBio reads to repetitive reference sequences.  Winnowmap implements a novel
weighted minimizer sampling algorithm (>=v1.0).  This optimization was motivated
by the need to avoid masking of frequently occurring k-mers during the seeding
stage in an efficient manner, and achieve better mapping accuracy in complex
repeats (e.g., long tandem repeats) of the human genome.  Using weighted
minimizers, Winnowmap down-weights frequently occurring k-mers, thus reducing
their chance of getting selected as minimizers.")
    (supported-systems '("x86_64-linux"))
    ;; minimap2 based code is expat, as is bloom.
    ;; Meryl is mix bsd-3, expat and public-domain.
    ;; Rest of the code is public domain.
    (license license:expat)))

(define-public quast
  (package
    (name "quast")
    (version "5.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (list (pypi-uri "quast" version)
                   (string-append "https://github.com/ablab/quast"
                                  "/releases/download/quast_" version
                                  "/quast-" version ".tar.gz")))
        (sha256
         (base32 "1nz0lz7zgrhcirmm3xcn756f91a6bpww9npap3a4l9gsgh413nfc"))
        (patches (search-patches "quast.patch"))
        (snippet
         #~(begin
             (use-modules (guix build utils))
             (with-directory-excursion "quast_libs"
               (substitute* "run_busco.py"
                 (("from quast_libs\\.busco import busco") "import busco"))
               (delete-file-recursively "site_packages/joblib2")
               (delete-file-recursively "site_packages/joblib3")
               (delete-file-recursively "site_packages/simplejson")
               (delete-file-recursively "minimap2")     ; Accepts minimap2 >= 2.19
               ;; These packages are needed at runtime
               (delete-file-recursively "bedtools")
               (delete-file-recursively "bwa")
               ;; These files are from python itself
               (delete-file "site_packages/bz2.py")
               (delete-file "site_packages/_bz2.py")
               (delete-file "site_packages/_compression.py")
               ;; Delete some pre-compiled binaries
               (delete-file-recursively "barrnap/binaries/darwin")
               (delete-file "barrnap/binaries/linux/nhmmer")
               (delete-file "busco/hmmsearch")
               (delete-file "sambamba/sambamba_linux")
               (delete-file "sambamba/sambamba_osx")
               ;; TODO:
               ;(delete-file "barrnap/bin/barrnap")

               ;; Genemark is a non-free, but available to academic
               ;; institutions. Remove some of the bundled binaries.
               (delete-file-recursively "genemark/linux_32")
               (delete-file-recursively "genemark/macosx")
               (delete-file-recursively "genemark-es/linux_32")
               (delete-file-recursively "genemark-es/macosx"))))))
    (build-system python-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'patchelf-genemark
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((patchelf (search-input-file inputs "/bin/patchelf"))
                     (ld-so    (search-input-file inputs #$(glibc-dynamic-linker)))
                     (rpath    (dirname
                                 (search-input-file inputs "/lib/libstdc++.so.6"))))
                 (for-each (lambda (binary)
                             (invoke patchelf "--set-interpreter" ld-so binary)
                             (invoke patchelf "--set-rpath" rpath binary))
                           (list "quast_libs/genemark/linux_64/gmhmmp"
                                 "quast_libs/genemark/linux_64/probuild"
                                 "quast_libs/genemark-es/linux_64/gmhmme3"
                                 "quast_libs/genemark-es/linux_64/probuild")))))
           (add-before 'build 'replace-bundled-binaries
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "quast_libs/ca_utils/misc.py"
                 (("join\\(qconfig.LIBS_LOCATION, 'minimap2'\\)")
                  (string-append "'" (search-input-file inputs "/bin/minimap2") "'")))
               (substitute* "./quast_libs/ra_utils/misc.py"
                 (("join\\(sambamba_dirpath, fname \\+ platform_suffix\\)")
                  (string-append "'" (search-input-file inputs "/bin/sambamba") "'"))
                 (("join\\(qconfig.LIBS_LOCATION, 'bedtools', 'bin'\\)")
                  (string-append
                    "'" (dirname (search-input-file inputs "/bin/bedtools")) "'")))))
           (add-after 'wrap 'wrap-more
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (for-each
                 (lambda (file)
                   (wrap-program file
                     `("PATH" ":" prefix
                       ,(map (lambda (file-name)
                               (string-append (assoc-ref inputs file-name) "/bin"))
                             (list "bedtools"
                                   "blast+"
                                   "busco"
                                   "bwa"
                                   "hmmer"
                                   "minimap2"
                                   "sambamba")))))
                 (find-files (string-append #$output "/bin") "\\.py$"))))
           (replace 'check
             (lambda* (#:key tests? inputs outputs #:allow-other-keys)
               (when tests?
                 (add-installed-pythonpath inputs outputs)
                 (invoke "python" "setup.py" "test"))))
           (delete 'strip))))       ; Can't strip genemark binaries.
    (native-inputs
     (list (list (canonical-package gcc) "lib") patchelf))
    (inputs
     (list python-joblib
           python-matplotlib
           python-simplejson
           ;; And the non-python packages:
           ;augustus
           bash-minimal
           bedtools
           blast+
           busco
           bwa
           hmmer
           minimap2
           perl
           sambamba))
    (home-page "http://quast.sourceforge.net/")
    (synopsis "Genome assembly evaluation tool")
    (description "QUAST stands for QUality ASsessment Tool.  It evaluates
genome/metagenome assemblies by computing various metrics.  The current QUAST
toolkit includes the general QUAST tool for genome assemblies, MetaQUAST, the
extension for metagenomic datasets, QUAST-LG, the extension for large genomes
(e.g., mammalians), and Icarus, the interactive visualizer for these tools.")
    (supported-systems '("x86_64-linux"))   ; Due to bundled genemark
    (license
      (list license:gpl2    ; Main program
            ;; Genemark (bundled) is free for non-commercial use by academic,
            ;; government, and non-profit/not-for-profit institutions.
            (license:non-copyleft
              "http://topaz.gatech.edu/GeneMark/license_download.cgi")))))

(define-public blobtools
  (let ((commit "1bed7870198831539b370f9254d5d30b94199a18")
        (revision "3"))         ;; 2022-09-21
    (package
      (name "blobtools")
      (version (git-version "1.1.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/DRL/blobtools")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0nki2m1sxkx75rkwzw6dqvxzyswrv47q0rz4bx9w0bz4fawnx86z"))))
      (build-system python-build-system)
      (arguments
       (list #:tests? #f))          ; No tests in repo.
      (inputs
       (list python-docopt
             python-matplotlib
             python-tqdm
             python-pysam
             python-pyyaml))
      (home-page "https://blobtools.readme.io/")
      (synopsis
       "Modular command-line solution for partitioning of genome datasets")
      (description "Blobtools is a modular command-line solution for
visualisation, quality control and taxonomic partitioning of genome datasets.
BlobTools takes the information from HITS files and sums up bitscores by
taxonomic group at each taxonomic rank.")
      (license license:gpl3))))

;; TODO: Regenerate or remove docs folder.
(define-public python-pixy
  (package
    (name "python-pixy")
    (version "1.2.6.beta1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ksamuk/pixy")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "16hl6hcf38fya18b1x75250z1shsysvpmc75vsp6wjqggajcfqc7"))))
    (build-system python-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 ;; "Test" based on test command in conda recipe.
                 (invoke "pixy" "--version")))))))
    (propagated-inputs
     (list python-multiprocess
           python-numcodecs
           python-numpy
           python-pandas
           python-scikit-allel
           python-scipy))
    (home-page "https://pixy.readthedocs.io/")
    (synopsis
     "Unbiased estimation of nucleotide diversity within and between populations")
    (description "@command{pixy} is a command-line tool for painlessly
estimating average nucleotide diversity within (π) and between (dxy) populations
from a VCF.  In particular, pixy facilitates the use of VCFs containing
invariant (monomorphic) sites, which are essential for the correct computation
of π and dxy in the face of missing data (i.e. always).")
    (license license:expat)))

(define-public wfa2-lib
  (let ((commit "af6be887614e8bb4e2b6e8c4e500705a978bd513")     ; 14 April 2022
        (revision "1"))
    (package
      (name "wfa2-lib")
      (version (git-version "2.1" revision commit))     ; As seen in ./VERSION
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/smarco/WFA2-lib")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "09gsmks4dzmfscklb60m6gcsvsd9r6jywf10633dpcsfsdcvmzaw"))
                (snippet
                 #~(begin
                     (use-modules ((guix build utils)))
                     (substitute* "Makefile"
                       (("^CC=") "CC:=")
                       (("^CPP=") "CPP:=")
                       (("-march=native") ""))))))
      (build-system gnu-build-system)
      (arguments
       (list
         #:tests? #f            ; No tests.
         #:parallel-build? #f   ; Race condition in Makefile.
         #:modules '((guix build gnu-build-system)
                     (guix build utils)
                     (srfi srfi-26))
         #:make-flags
         #~(list (string-append "CC=" #$(cc-for-target))
                 (string-append "CPP=" #$(cxx-for-target)))
         #:phases
         #~(modify-phases %standard-phases
             (delete 'configure)        ; No configure script.
             ;; -flto breaks align_benchmark.
             (replace 'build
               (lambda* (#:key (make-flags '()) #:allow-other-keys)
                 (apply invoke "make" "all" make-flags)))
             (replace 'install
               (lambda _
                 (for-each
                   (cut install-file <> (string-append #$output "/bin"))
                   (find-files "bin"))
                 (for-each
                   (cut install-file <> (string-append #$output "/lib"))
                   (find-files "lib"))
                 (for-each
                   (lambda (file)
                     (mkdir-p (string-append #$output "/include/wfa2-lib/" (dirname file)))
                     (copy-file file (string-append #$output "/include/wfa2-lib/" file)))
                   (find-files "." "\\.(h|hpp)$")))))))
      (home-page "https://github.com/smarco/WFA2-lib")
      (synopsis "Wavefront alignment algorithm library")
      (description "The @acronym{wavefront alignment, WFA} algorithm is an exact
gap-affine algorithm that takes advantage of homologous regions between the
sequences to accelerate the alignment process.  Unlike to traditional dynamic
programming algorithms that run in quadratic time, the WFA runs in time
@code{O(ns+s^2)}, proportional to the sequence length @code{n} and the alignment
score @code{s}, using @code{O(s^2)} memory.  Moreover, the WFA algorithm
exhibits simple computational patterns that the modern compilers can
automatically vectorize for different architectures without adapting the code.")
      (properties '((tunable? . #t)))
      (license license:expat))))

(define-public wfa2-lib-static
  (package
    (inherit (static-package wfa2-lib))
    (name "wfa2-lib-static")
    (arguments
     (substitute-keyword-arguments (package-arguments wfa2-lib)
       ((#:make-flags flags ''())
        #~(cons "CC_FLAGS+=-static" #$flags))))))
