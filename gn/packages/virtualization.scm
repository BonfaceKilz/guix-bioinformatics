(define-module (gn packages virtualization)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system scons)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public riscv-pk
  (package
    (name "riscv-pk")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/riscv-software-src/riscv-pk")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1cc0rz4q3a1zw8756b8yysw8lb5g4xbjajh5lvqbjix41hbdx6xz"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:out-of-source? #t
       ;#:configure-flags
       ;#~(list "--host=riscv64-linux-gnu")
       #:target "riscv64-linux-gnu"
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'force-install-directory
             (lambda _
               (substitute* "Makefile.in"
                 (("\\$\\(install_subdir\\)") "")))))))
    (native-inputs
     (if (not (string-prefix? "riscv64" (%current-system)))
       (list (cross-gcc "riscv64-linux-gnu")
             (cross-binutils "riscv64-linux-gnu"))
       '()))
    (home-page "https://github.com/riscv-software-src/riscv-pk")
    (synopsis "RISC-V Proxy Kernel")
    (description "The RISC-V Proxy Kernel, @code{pk}, is a lightweight
application execution environment that can host statically-linked RISC-V ELF
binaries.  It is designed to support tethered RISC-V implementations with
limited I/O capability and and thus handles I/O-related system calls by proxying
them to a host computer.  This package also contains the Berkeley Boot Loader,
@code{bbl}, which is a supervisor execution environment for tethered RISC-V
systems.  It is designed to host the RISC-V Linux port.")
    (license license:bsd-3)))

(define-public gem5
  (package
    (name "gem5")
    (version "21.2.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gem5.googlesource.com/public/gem5")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0wyns8cfkigzw3m21331m8ydc7akm8l2lp0kci4pq8jyzsanmjm3"))
        (snippet
          #~(begin
              (use-modules (guix build utils))
              ;; For reproducibility.
              (substitute* "src/base/date.cc"
                (("__DATE__") "\"1970-01-01\"")
                (("__TIME__") "\"00:00:00\""))))))
    (build-system scons-build-system)
    (arguments
     `(#:scons-flags
       (list "--verbose")
       #:tests? #f      ; TODO: How to run test suite?
       #:build-targets '("build/ARM/gem5.opt"
                         "build/MIPS/gem5.opt"
                         "build/NULL/gem5.opt"
                         "build/POWER/gem5.opt"
                         "build/RISCV/gem5.opt"
                         "build/SPARC/gem5.opt"
                         "build/X86/gem5.opt")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "SConstruct"
               ;; Scons does not use the environment variables by default,
               ;; but this substitution makes it do so.
               ;(("main = Environment")
               ; "main = Environment(ENV=os.environ)\nmain = Environment")
               ;; Force adding missing includes into the environment.
               (("main\\.Append\\(CPPPATH=\\[Dir\\('" all)
                (string-append
                  all (assoc-ref inputs "kernel-headers") "/include')])\n"
                  all (assoc-ref inputs "libpng") "/include')])\n"
                  all (assoc-ref inputs "zlib") "/include')])\n"
                  all)))
             (substitute* "ext/libelf/SConscript"
               (("m4env\\.Tool" all)
                (string-append
                  "m4env.Append(CPPPATH=[Dir('"
                  (assoc-ref inputs "kernel-headers")
                  "/include')])\n"
                  all)))
             (substitute* "ext/libelf/native-elf-format"
               (("cc") ,(cc-for-target)))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python3" "tests/run.py"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/")))
               (mkdir-p bin)
               (for-each
                 (lambda (arch)
                   (when (directory-exists?
                           (string-append "build/" (string-upcase arch)))
                     (copy-file
                       (string-append "build/" (string-upcase arch) "/gem5.opt")
                       (string-append bin "gem5-" arch ".opt"))
                     (copy-file
                       (string-append "build/" (string-upcase arch) "/gem5py")
                       (string-append bin "gem5py-" arch))
                     (copy-file
                       (string-append "build/" (string-upcase arch) "/gem5py_m5")
                       (string-append bin "gem5py_m5-" arch))))
                 (list "arm" "mips" "null" "power" "riscv" "sparc" "x86"))))))))
    (inputs
     (list gperftools
           libpng
           protobuf
           python
           python-pydot
           python-six
           zlib))
    (native-inputs
     (list boost
           m4
           pkg-config))
    (home-page "http://gem5.org/")
    (synopsis "Modular platform for computer-system architecture research")
    (description "The gem5 simulator is a modular platform for computer-system
architecture research, encompassing system-level architecture as well as
processor microarchitecture.")
    (license license:bsd-2)))

(define-public gem5-riscv
  (package
    (inherit gem5)
    (name "gem5-riscv")
    (arguments
     (substitute-keyword-arguments (package-arguments gem5)
       ((#:build-targets _)
        `(list "build/RISCV/gem5.opt"))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "build/RISCV/gem5.opt" bin)
                 (install-file "build/RISCV/gem5py" bin)
                 (install-file "build/RISCV/gem5py_m5" bin))))))))))
