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
  #:use-module (gnu packages autotools)
  #:use-module ((gnu packages bioinformatics) #:prefix guix:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages jemalloc)
  #:use-module ((gnu packages maths) #:prefix guix:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public htslib
  (package
    (inherit guix:htslib)
    (name "htslib")
    (inputs
     `(("bzip2" ,bzip2)
       ("xz" ,xz)
       ,@(package-inputs guix:htslib)))))

(define-public gsl
  (package
    (inherit guix:gsl)
    (name "gsl")
    (arguments
     (substitute-keyword-arguments (package-arguments guix:gsl)
       ((#:configure-flags _) `(list))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'force-bootstrap
             (lambda _
               (delete-file "configure")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))))

(define-public atomic-queue
  (package
    (name "atomic-queue")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/max0x7ba/atomic_queue")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ssff73wlvrsk2nma99dmvm0ijyzfr54jk37kxgpb694r7ajc90l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "include/atomic_queue"
                               (string-append (assoc-ref outputs "out")
                                              "/include/atomic_queue")))))))
    (home-page "https://github.com/max0x7ba/atomic_queue")
    (synopsis "C++ lockless queue")
    (description "@code{atomic-queue} provides C++14
multiple-producer-multiple-consumer lockless queues based on a circular buffer
with std::atomic.  The maximum queue size must be set at compile time. And,
there are no OS-blocking push/pop functions, thus making it suitable for
ultra-low latency applications.")
    (license license:expat)))

(define-public wfmash
  (let ((version "0.7.0")
        (commit "50a68f0d8c372e720d73e7fc9d90a0d0a4e54ef8")
        (package-revision "25"))
    (package
      (name "wfmash")
      (version (string-append version "+" (string-take commit 7) "-" package-revision))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ekg/wfmash.git")
                      (commit commit)
                      (recursive? #f)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xf742sxn7xvrcmn67zk9rv8zxl9nb9f72hbw8khdlz1qj3n00vp"))
                (modules '((guix build utils)))
                (snippet '(begin
                            (delete-file-recursively "src/common/atomic_queue")
                            (substitute* "src/align/include/computeAlignments.hpp"
                              (("\"common/atomic_queue/atomic_queue.h\"")
                               "<atomic_queue/atomic_queue.h>"))))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DBUILD_SHARED_LIBS=OFF")
         #:phases
         (modify-phases
             %standard-phases
           (add-after 'unpack 'remove-x86-specific-compile-flags
             (lambda _
               (substitute* (list "CMakeLists.txt"
                                  "src/common/wflign/CMakeLists.txt"
                                  "src/common/wflign/deps/WFAv2/CMakeLists.txt"
                                  "src/common/wflign/deps/wflambdav2/CMakeLists.txt")
                 (("-mcx16") ""))
               (substitute* (list "CMakeLists.txt"
                                  "src/common/wflign/CMakeLists.txt"
                                  "src/common/wflign/deps/WFAv2/CMakeLists.txt"
                                  "src/common/wflign/deps/wflambdav2/CMakeLists.txt")
                 (("-march=native") ""))
               (substitute* "src/common/dset64.hpp"
                 (("#error \"wfmash can only be built on an x86_64 machine \\(64-bit Intel/AMD\\)\"")
                  ""))))
           ;; This stashes our build version in the executable
           (add-after 'unpack 'set-version
             (lambda _
               (mkdir "include")
               (with-output-to-file "include/wfmash_git_version.hpp"
                 (lambda ()
                   (format #t "#define WFMASH_GIT_VERSION \"~a\"~%" version)))
               #t))
           (delete 'check))
         #:make-flags (list (string-append "CC=" ,(cc-for-target))
                            (string-append "CXX=" ,(cxx-for-target)))))
      (inputs (list atomic-queue
                    gsl
                    htslib
                    jemalloc
                    zlib))
      (synopsis "base-accurate DNA sequence alignments using WFA and mashmap2")
      (description "wfmash is a fork of MashMap that implements
base-level alignment using the wavefront alignment algorithm WFA. It
completes an alignment module in MashMap and extends it to enable
multithreaded operation. A single command-line interface simplfies
usage. The PAF output format is harmonized and made equivalent to that
in minimap2, and has been validated as input to seqwish.")
      (home-page "https://github.com/ekg/wfmash")
      (license license:expat))))
