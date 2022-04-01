;;;
;;; Commentary:
;;;
;;; This module contains statically linked executables meant for use
;;; with spike and gem5.
;;;

;;;
;;; Code:
;;;

(define-module (gn packages static)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages maths)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; Static hello, for testing
(define-public hello-static
  (package
    (inherit (static-package hello))
    (name "hello-static")))

;; A minimal version of htslib that does not depend on curl and openssl. This
;; reduces the number of higher order dependencies in static linking.
(define htslib-minimal
  (package
    (inherit htslib)
    (arguments
     (substitute-keyword-arguments (package-arguments htslib)
       ((#:configure-flags flags ''())
        ''())))
    (inputs
     (list bzip2 xz))))

(define-public wfmash-static
  (package
    (inherit wfmash)
    (name "wfmash-static")
    (arguments
     (substitute-keyword-arguments (package-arguments wfmash)
       ((#:configure-flags flags ''())
        `(cons* "-DCMAKE_EXE_LINKER_FLAGS=-static"
                "-DCMAKE_SKIP_RPATH=TRUE"
                ,flags))
       ((#:phases phases ''())
        #~(modify-phases #$phases
            ;; When static linking, we need to link against the entire
            ;; dependency tree, not just the direct first-order dependencies.
            (add-after 'unpack 'add-higher-order-dependencies
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("hts" all)
                   (string-append all " bz2 dl lzma"))
                  (("jemalloc" all)
                   ;; We add atomic because riscv64 has no lock-free atomic
                   ;; instructions.
                   (string-append all " atomic pthread")))))))))
    (inputs
     (list atomic-queue
           gsl-static
           htslib-minimal
           jemalloc
           (list zlib "static")

           ;; Second-order dependencies from htslib
           (list bzip2 "static")
           (list xz "static")))))
