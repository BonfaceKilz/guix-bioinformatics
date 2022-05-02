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
  #:use-module (gn packages fpga)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages virtualization)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

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

(define-public cva6
  (let ((commit "5f802b0520d91ee4a8c988fd83c66b0b48e75447")
        (revision "1"))
    (package
     (name "cva6")
     (version (git-version "4.2.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    ;; TODO: Use https://github.com/openhwgroup/cva6
                    ;; instead of the cornell-brg fork.
                    (url "https://github.com/cornell-brg/cva6")
                    (commit commit)
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06rwzgvkf6cb6lgwbk6gb41afqv30h4m1sb2piyb1j499znvvgq6"))))
     (build-system gnu-build-system)
     (arguments
      (list
       #:tests? #f
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda _
               (invoke "make" "verilate"
                       (string-append "-j" (number->string (parallel-job-count))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "work-ver/Variane_testharness"
                             (string-append (assoc-ref outputs "out")
                                            "/bin")))))))
     (inputs
      (list spike verilator-4.110))
     (home-page "https://docs.openhwgroup.org/projects/cva6-user-manual/")
     (synopsis "Application class 6-stage RISC-V CPU")
     (description "CVA6 is a 6-stage, single issue, in-order CPU which
implements the 64-bit RISC-V instruction set. It fully implements I,
M, A and C extensions as specified in Volume I: User-Level ISA V 2.3
as well as the draft privilege extension 1.10. It implements three
privilege levels M, S, U to fully support a Unix-like operating
system. Furthermore it is compliant to the draft external debug spec
0.13.

It has configurable size, separate TLBs, a hardware PTW and
branch-prediction (branch target buffer and branch history table). The
primary design goal was on reducing critical path length.")
     (license (list license:asl2.0
                    license:bsd-3
                    ;; TODO: Add Solderpad Hardware license.
                    )))))
