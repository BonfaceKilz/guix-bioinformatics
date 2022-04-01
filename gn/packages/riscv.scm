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
  #:use-module (gnu packages cpp)
  #:use-module (guix git-download)
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
