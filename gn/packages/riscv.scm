;;;
;;; Commentary:
;;;
;;; This module is a temporary staging ground for Guix packages ported
;;; to RISC-V. They should be contributed upstream at the earliest.
;;;

;;;
;;; Code:
;;;

(define-module (gn packages riscv)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages maths))


;; Static hello, for testing
(define-public hello-static
  (package
    (inherit hello)
    (name "hello-static")
    (arguments
     (substitute-keyword-arguments (package-arguments hello)
       ((#:make-flags _ ''())
        `(list "CFLAGS=-static"))))))

;; Improvements to riscv support have been merged since the last release.
(define-public atomic-queue-git
  (let ((commit "7d75e9ed0359650224b29cdf6728c5fe0a19fffb")     ; 2022-03-11
        (revision "1"))
    (package
      (inherit atomic-queue)
      (name "atomic-queue")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/max0x7ba/atomic_queue")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dh8x0ikfwk0by5avwfv9gvr9ay6jy13yr66rvgw9wwyxmklz848")))))))

(define-public wfmash
  (let ((version "0.7.0")
        (commit "81b8292479648058c6986da808afba0eadcce8d0")
        (package-revision "26"))
    (package
      (name "wfmash")
      (version (git-version version package-revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ekg/wfmash.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0nfmbnmlk2ji5f651dkv0jl1h3d1lp2npldwhdiyylp96z3yz8zb"))
                (modules '((guix build utils)))
                (snippet '(begin
                            (delete-file-recursively "src/common/atomic_queue")
                            (substitute* "src/align/include/computeAlignments.hpp"
                              (("\"common/atomic_queue/atomic_queue.h\"")
                               "<atomic_queue/atomic_queue.h>"))))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'remove-x86-specific-compile-flags
             (lambda _
               (substitute* (find-files "." "CMakeLists\\.txt")
                 (("-mcx16") "")
                 (("-march=native") ""))
               (substitute* "src/common/dset64.hpp"
                 (("__x86_64__" all) (string-append all " && " all)))))
           ;; This stashes our build version in the executable
           (add-after 'unpack 'set-version
             (lambda _
               (mkdir "include")
               (with-output-to-file "include/wfmash_git_version.hpp"
                 (lambda ()
                   (format #t "#define WFMASH_GIT_VERSION \"~a\"~%" version)))))
           (replace 'check
             ;; Adapted from .github/workflows/test_on_push.yml
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (and
                   ;; This test takes 60 minutes on riscv64-linux.
                   ,@(if (not (target-riscv64?))
                       `((begin
                           ;; Test with a subset of the LPA dataset (PAF output)
                           (setenv "ASAN_OPTIONS" "detect_leaks=1:symbolize=1")
                           (setenv "LSAN_OPTIONS" "verbosity=0:log_threads=1")
                           (with-output-to-file "LPA.subset.paf"
                             (lambda _
                               (invoke "bin/wfmash"
                                       "../source/data/LPA.subset.fa.gz"
                                       "../source/data/LPA.subset.fa.gz"
                                       "-X" "-n" "10" "-T" "wflign_info.")))
                           (invoke "head" "LPA.subset.paf")))
                       '())
                   ;; This test takes about 5 hours on riscv64-linux.
                   ,@(if (not (target-riscv64?))
                       `((begin
                           ;; Test with a subset of the LPA dataset (SAM output)
                           (setenv "ASAN_OPTIONS" "detect_leaks=1:symbolize=1")
                           (setenv "LSAN_OPTIONS" "verbosity=0:log_threads=1")
                           (with-output-to-file "LPA.subset.sam"
                             (lambda _
                               (invoke "bin/wfmash"
                                       "../source/data/LPA.subset.fa.gz"
                                       "../source/data/LPA.subset.fa.gz"
                                       "-X" "-N" "-a" "-T" "wflign_info.")))
                           (with-output-to-file "LPA.subset.sam-view"
                             (lambda _
                               (invoke "samtools" "view" "LPA.subset.sam" "-bS")))
                           (with-output-to-file "LPA.subset.bam"
                             (lambda _
                               (invoke "samtools" "sort" "LPA.subset.sam-view")))
                           (invoke "samtools" "index" "LPA.subset.bam")
                           ;; samtools view LPA.subset.bam | head | cut -f 1-9
                           ;(invoke "samtools" "view" "LPA.subset.bam")
                           ;; There should be an easier way to do this with pipes.
                           (with-output-to-file "LPA.subset.bam-incr1"
                             (lambda _
                               (invoke "samtools" "view" "LPA.subset.bam")))
                           (with-output-to-file "LPA.subset.bam-incr2"
                             (lambda _
                               (invoke "head" "LPA.subset.bam-incr1")))
                           (invoke "cut" "-f" "1-9" "LPA.subset.bam-incr2")))
                       '())
                   ;; This test takes 60 minutes on riscv64-linux.
                   ,@(if (not (target-riscv64?))
                       `((begin
                           ;; Test with a subset of the LPA dataset,
                           ;; setting a lower identity threshold (PAF output)
                           (setenv "ASAN_OPTIONS" "detect_leaks=1:symbolize=1")
                           (setenv "LSAN_OPTIONS" "verbosity=0:log_threads=1")
                           (with-output-to-file "LPA.subset.p90.paf"
                             (lambda _
                               (invoke "bin/wfmash"
                                       "../source/data/LPA.subset.fa.gz"
                                       "../source/data/LPA.subset.fa.gz"
                                       "-X" "-p" "90" "-n" "10" "-T" "wflign_info.")))
                           (invoke "head" "LPA.subset.p90.paf")))
                       '())
                   (begin
                     ;; Test aligning short reads (500 bps) to a reference (SAM output)
                     (setenv "ASAN_OPTIONS" "detect_leaks=1:symbolize=1")
                     (setenv "LSAN_OPTIONS" "verbosity=0:log_threads=1")
                     (with-output-to-file "reads.500bps.sam"
                       (lambda _
                         (invoke "bin/wfmash"
                                 "../source/data/reference.fa.gz"
                                 "../source/data/reads.500bps.fa.gz"
                                 "-s" "0.5k" "-N" "-a")))
                     (with-output-to-file "reads.500bps.sam-view"
                       (lambda _
                         (invoke "samtools" "view" "reads.500bps.sam" "-bS")))
                     (with-output-to-file "reads.500bps.bam"
                       (lambda _
                         (invoke "samtools" "sort" "reads.500bps.sam-view")))
                     (invoke "samtools" "index" "reads.500bps.bam")
                     (with-output-to-file "reads.500bps.bam-view"
                       (lambda _
                         (invoke "samtools" "view" "reads.500bps.bam")))
                     (invoke "head" "reads.500bps.bam-view"))
                   (begin
                     ;; Test with few very short reads (255bps) (PAF output)
                     (setenv "ASAN_OPTIONS" "detect_leaks=1:symbolize=1")
                     (setenv "LSAN_OPTIONS" "verbosity=0:log_threads=1")
                     (with-output-to-file "reads.255bps.paf"
                       (lambda _
                         (invoke "bin/wfmash"
                                 "../source/data/reads.255bps.fa.gz"
                                 "../source/data/reads.255bps.fa.gz"
                                 "-X")))
                     (invoke "head" "reads.255bps.paf")))))))
         #:make-flags (list (string-append "CC=" ,(cc-for-target))
                            (string-append "CXX=" ,(cxx-for-target)))))
      (inputs (list atomic-queue
                    gsl
                    htslib
                    jemalloc
                    zlib))
      (native-inputs
       (list samtools))
      (synopsis "base-accurate DNA sequence alignments using WFA and mashmap2")
      (description "wfmash is a fork of MashMap that implements
base-level alignment using the wavefront alignment algorithm WFA. It
completes an alignment module in MashMap and extends it to enable
multithreaded operation. A single command-line interface simplfies
usage. The PAF output format is harmonized and made equivalent to that
in minimap2, and has been validated as input to seqwish.")
      (home-page "https://github.com/ekg/wfmash")
      (license license:expat))))
