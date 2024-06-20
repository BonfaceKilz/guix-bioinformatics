(define-module (gn packages virtualization)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system scons)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public gem5
  (package
    (name "gem5")
    (version "22.1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gem5.googlesource.com/public/gem5")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1kcdn8rawzhf88lcb2app8m1r1px4ba041kyl7xigslix7qs05k3"))
        (snippet
          #~(begin
              (use-modules (guix build utils))
              ;; For reproducibility.
              (substitute* "src/base/date.cc"
                (("__DATE__") "\"1970-01-01\"")
                (("__TIME__") "\"00:00:00\""))
              ;; Remove vendored pybind11.
              (substitute* "ext/sst/Makefile"
                (("-I../../ext/pybind11/include/")
                 "${shell pybind11-config --includes}"))
              (substitute* "SConstruct"
                ((".*pybind11.*") ""))
              (delete-file-recursively "ext/pybind11")))))
    (build-system scons-build-system)
    (arguments
     `(#:scons-flags
       (list "--verbose")
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
                  all (assoc-ref inputs "pybind11") "/include')])\n"
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
         ;; This uses the cached results from the previous 'build phase.
         ;; Move to after 'install and delete build dir first?
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys #:rest args)
             (when tests?
               (apply (assoc-ref %standard-phases 'build)
                      #:build-targets '("build/NULL/unittests.opt")
                      args))))
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
                 (list "arm" "mips" "null" "power" "riscv" "sparc" "x86")))))
         (add-after 'install 'install-configs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (dest (string-append out "/share/gem5/configs")))
               (copy-recursively "configs" dest))))
         (add-after 'install 'wrap-binaries
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                 (lambda (file)
                   (wrap-program file
                     `("GUIX_PYTHONPATH" ":" prefix
                       (,(getenv "GUIX_PYTHONPATH")))))
                 (find-files (string-append out "/bin")))))))))
    (inputs
     (list gperftools
           libpng
           protobuf
           pybind11
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
       ((#:build-targets _ '())
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
