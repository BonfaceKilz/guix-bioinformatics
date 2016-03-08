;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Dennis Mungai <dmngaie@gmail.com>
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

(define-module (gn packages pocl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages web)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages zip)  
  #:use-module (gnu packages linux))

(define-public pocl
(let ((commit "a6f377a"))
  (package
    (name "pocl")
    (version (string-append "v09rc-" commit ))
    (source (origin
              (method git-fetch)
              (uri (git-reference
              (url "https://github.com/pocl/pocl.git")
              (commit commit)))
              (file-name (string-append name "-" commit))
              (sha256
               (base32
                "0b1y8c2y0xx5sqfpkkvgmp02czgmq5immypgm4hhpmp512hcj38j"))))
(native-inputs `(("autoconf" ,autoconf)
                ("automake" ,automake)
                ("libtool" ,libtool)
                ("cmake" ,cmake)
                ("pkg-config" ,pkg-config)))
(inputs        `(("python" ,python-2)
               ("boost" ,boost)
               ("dbus" ,dbus)
               ("clang" ,clang)
               ("clang-runtime" ,clang-runtime)
               ("enca" ,enca)
               ("eudev" ,eudev)
               ("fftw-openmpi" ,fftw-openmpi)
               ("glew" ,glew)
               ("hwloc" ,hwloc)
               ("libcap" ,libcap)
               ("libjpeg" ,libjpeg)
               ("libltdl" ,libltdl)
               ("libtiff" ,libtiff)
               ("llvm" ,llvm-3.7.1)
               ("ocl-icd" ,ocl-icd)
               ("opencl-headers" ,opencl-headers)
               ("mesa-utils" ,mesa-utils)
               ("openmpi" ,openmpi)
               ("perl" ,perl)
               ("randrproto" ,randrproto)
               ("libxrandr" ,libxrandr)
               ("xineramaproto" ,xineramaproto)
               ("libxinerama" ,libxinerama)
               ("libxcursor" ,libxcursor)
               ("fftw-openmpi" ,fftw-openmpi)))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-icd")
     #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'rewrite-usr-bin
          (lambda _
                   (zero? (system* "./autogen.sh")))))))                
    (synopsis "pocl: Portable Computing Language (pocl) aims to become a MIT-licensed
     open source implementation of the OpenCL standard which can be easily adapted for
     new targets and devices, both for homogeneous CPU and heterogenous GPUs/accelerators.")
    (description "Portable Computing Language (pocl) aims to become a MIT-licensed
     open source implementation of the OpenCL standard which can be easily adapted for
     new targets and devices, both for homogeneous CPU and heterogenous GPUs/accelerators.")
    (home-page "http://portablecl.org/")
    (license license:gpl2))))
    
(define-public llvm-3.7.1
(package
    (name "llvm-3.7.1")
    (version "3.7.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://llvm.org/releases/"
                          version "/llvm-" version ".src.tar.xz"))
      (sha256
       (base32
        "1masakdp9g2dan1yrazg7md5am2vacbkb3nahb3dchpc1knr8xxy"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python-wrapper)
       ("perl"   ,perl)))
    (arguments
     `(#:configure-flags '("-DCMAKE_SKIP_BUILD_RPATH=FALSE"
                           "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE")))
    (home-page "http://www.llvm.org")
    (synopsis "Optimizing compiler infrastructure")
    (description
     "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.  It currently supports compilation of C and C++ programs, using
front-ends derived from GCC 4.0.1.  A new front-end for the C family of
languages is in development.  The compiler infrastructure includes mirror sets
of programming tools as well as libraries with equivalent functionality.")
    (license license:ncsa)))
    
(define-public ocl-icd
  (package
   (name "ocl-icd")
   (version "2.2.9")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://forge.imag.fr/frs/download.php/716/ocl-icd-"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1rgaixwnxmrq2aq4kcdvs0yx7i6krakarya9vqs7qwsv5hzc32hc"))))
    (inputs `(("zip" ,zip)
             ("autoconf" ,autoconf)
             ("automake" ,automake)
             ("ruby" ,ruby)
             ("libtool" ,libtool)
             ("opencl-headers" ,opencl-headers)
             ("libgcrypt" ,libgcrypt)))                                              
    (build-system gnu-build-system)
     (arguments
     '(#:phases (modify-phases %standard-phases
                    (add-after 'unpack `bootstrap
                      (lambda _
                        (zero? (system* "autoreconf" "-vfi")))))))    
    (home-page "https://forge.imag.fr/projects/ocl-icd/")
    (synopsis "OpenCL implementations are provided as ICD (Installable Client Driver).")
    (description "OpenCL implementations are provided as ICD (Installable Client Driver).
    An OpenCL program can use several ICD thanks to the use of an ICD Loader as provided by this project.
    This free ICD Loader can load any (free or non free) ICD")
    (license license:gpl2)))
    
 (define-public opencl-headers
(let ((commit "c1770dc"))
  (package
    (name "opencl-headers")
    (version (string-append "2.1-" commit ))
    (source (origin
              (method git-fetch)
              (uri (git-reference
              (url "https://github.com/KhronosGroup/OpenCL-Headers.git")
              (commit commit)))
              (file-name (string-append name "-" commit))
              (sha256
               (base32
                "0m9fkblqja0686i2jjqiszvq3df95gp01a2674xknlmkd6525rck"))))
    (propagated-inputs '())
    (inputs '())
    (native-inputs '())
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (copy-recursively "." (string-append
                                                 (assoc-ref outputs "out")
                                                 "/include/CL")))))))
    (synopsis "The Khronos OpenCL headers")
    (description "This package provides the Khronos OpenCL headers")
    (home-page "https://www.khronos.org/registry/cl/")
    (license license:gpl2))))
