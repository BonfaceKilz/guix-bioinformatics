;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Dennis Mungai <dmngaie@gmail.com>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gn packages hyphy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls))

;; TODO: Unbundle sqlite, gtest
(define-public hyphy ; guix: check
  (package
    (name "hyphy")
    (version "2.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/veg/hyphy.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fvmwg2rxkz3abhhldiir69vmyc4i85vdvy64bizgxd0g2k2bikm"))))
    (inputs
     `(("curl" ,curl)
       ("fftw-openmpi" ,fftw-openmpi)
       ("openmpi" ,openmpi)
       ("openssl" ,openssl)
       ("python" ,python-2)))
    (build-system cmake-build-system)
    (arguments
     '(#:make-flags '("MPI") ; Add "GTEST" for tests, currently fails to compile.
       #:configure-flags (list "-DCMAKE_BUILD_TYPE=Release"
                               (string-append "-DINSTALL_PREFIX="
                                              (assoc-ref %outputs "out")))
       #:tests? #f
       #:test-target "HYPHYGTEST"))
    (synopsis "hyphy: an open-source software package for the analysis
of genetic sequences using techniques in phylogenetics, molecular
evolution, and machine learning.")
    (description "HyPhy is an open-source software package for the
analysis of genetic sequences using techniques in phylogenetics,
molecular evolution, and machine learning.  It features a complete
graphical user interface (GUI) and a rich scripting language for
limitless customization of analyses. Additionally, HyPhy features
support for parallel computing environments (via message passing
interface (MPI)) and it can be compiled as a shared library and called
from other programming environments such as Python and R. ")
    (home-page "http://hyphy.org")
    (license license:expat)))
