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
  #:use-module (guix build-system gnu)
  #:use-module (guix packages))

;; Static hello, for testing
(define-public hello-static
  (package
    (inherit (static-package hello))
    (name "hello-static")))
