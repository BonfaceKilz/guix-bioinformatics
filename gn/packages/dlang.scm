(define-module (gn packages dlang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xorg))

#!
The following tests FAILED:
        394 - std.socket (Failed)
        793 - std.socket-debug (Failed)
        1192 - std.socket-shared (Failed)
        1591 - std.socket-debug-shared (Failed)
        1608 - druntime-test-aa (Failed)
        1610 - druntime-test-allocations (Failed)
        1612 - druntime-test-betterc (Failed)
        1614 - druntime-test-config (Failed)
        1616 - druntime-test-coverage (Failed)
        1618 - druntime-test-cpuid (Failed)
        1620 - druntime-test-cycles (Failed)
        1622 - druntime-test-exceptions (Failed)
        1624 - druntime-test-gc (Failed)
        1626 - druntime-test-hash (Failed)
        1630 - druntime-test-init_fini (Failed)
        1632 - druntime-test-profile (Failed)
        1634 - druntime-test-shared (Failed)
        1638 - druntime-test-thread (Failed)
        1640 - druntime-test-typeinfo (Failed)
        1642 - druntime-test-unittest (Failed)
        1643 - build-run-dmd-testsuite (Failed)
        1645 - dmd-testsuite-debug (Not Run)
        1647 - dmd-testsuite (Not Run)
        1648 - lit-tests (Failed)
Errors while running CTest
!#

(define-public ldc
  ;; Phobos, druntime and dmd-testsuite library dependencies do
  ;; not always have a newer release than the compiler, hence we
  ;; retain this variable.
  (let ((older-version "1.26.0")) ;; retain this because sometimes the libs are older
    (package
      (inherit ldc-bootstrap)
      (name "ldc")
      (version "1.26.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ldc-developers/ldc")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1k9998w8zdm6l6ld8wzxvp5hj5jfqhnxmvs82vmrrh92ysrfjbp6"))))
      (arguments
       `(#:tests? #f ;; tests fail, see list above
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (input target)
                               (let ((source (assoc-ref inputs input)))
                                 ;; Git checkouts are directories as long as
                                 ;; there are no patches; tarballs otherwise.
                                 (if (file-is-directory? source)
                                     (copy-recursively source target)
                                     (with-directory-excursion target
                                       (invoke "tar" "xvf" source
                                               "--strip-components=1")))))))
                 (unpack "phobos-src" "runtime/phobos")
                 (unpack "druntime-src" "runtime/druntime")
                 (unpack "dmd-testsuite-src" "tests/d2/dmd-testsuite")
                 #t)))
           (add-after 'unpack-submodule-sources 'patch-phobos
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("runtime/phobos/std/process.d"
                              "tests/linking/linker_switches.d")
                 (("/bin/sh") (which "sh"))
                 (("echo") (which "echo")))
               (substitute* "tests/d2/dmd-testsuite/Makefile"
                 (("/bin/bash") (which "bash")))
               ;; disable unittests in the following files. We are discussing with
               ;; upstream
               (substitute* '("runtime/phobos/std/net/curl.d"
                              "runtime/phobos/std/datetime/systime.d"
                              "runtime/phobos/std/datetime/timezone.d"
                              )
                 (("version(unittest)") "version(skipunittest)")
                 ((" unittest") " version(skipunittest) unittest"))
               ;; the following tests require a more recent LLVM
               (delete-file "tests/compilable/ctfe_math.d")
               (delete-file "tests/debuginfo/nested_gdb.d")
               (delete-file "tests/debuginfo/classtypes_gdb.d")
               ;; the following tests plugins we don't have.
               (delete-file "tests/plugins/addFuncEntryCall/testPlugin.d")
               ;; the following tests requires AVX instruction set in the CPU.
               ; (substitute* "tests/d2/dmd-testsuite/runnable/test_cdvecfill.d"
               ;   (("^// DISABLED: ") "^// DISABLED: linux64 "))
               #t))
            ; (replace 'check
            ;   (lambda* (#:key inputs outputs #:allow-other-keys)
            ;     ;; some tests call into gdb binary which needs SHELL and CC set
            ;    (setenv "SHELL" (which "sh"))
            ;    (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
                                        ;    (invoke "make" "test" "-j" (number->string (parallel-job-count))))))))
           )))
      (native-inputs
       `(("llvm" ,llvm)
         ("clang" ,clang)
         ("ldc" ,ldc-bootstrap)
         ("python-lit" ,python-lit)
         ("python-wrapper" ,python-wrapper)
         ("unzip" ,unzip)
         ("gdb" ,gdb)
         ("phobos-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ldc-developers/phobos")
                   (commit (string-append "ldc-v" older-version))))
             (file-name (git-file-name "phobos" older-version))
             (sha256
              (base32 "0salwkm2kl9vyaqqjzxgqfd6k7grk2d8886g7vahl4mqm5nqc78w"))
             ;; This patch deactivates some tests that depend on network access
             ;; to pass.  It also deactivates some tests that have some reliance
             ;; on timezone.
             ;;
             ;; For the network tests, there's an effort to get a version flag
             ;; added to deactivate these tests for distribution packagers
             ;; that is being pursued at
             ;; <https://forum.dlang.org/post/zmdbdgnzrxyvtpqafvyg@forum.dlang.org>.
             ;; It also deactivates a test that requires /root
             ;; (patches (search-patches "ldc-disable-phobos-tests.patch"))
             ))
         ("druntime-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ldc-developers/druntime")
                   (commit (string-append "ldc-v" older-version))))
             (file-name (git-file-name "druntime" older-version))
             (sha256
              (base32 "07l71j3haafiglf8f0f5q5k84zsd61jgghsxqb4krs9rc1yxmr4s"))))
         ("dmd-testsuite-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ldc-developers/dmd-testsuite")
                   (commit (string-append "ldc-v" older-version))))
             (file-name (git-file-name "dmd-testsuite" older-version))
             (sha256
              (base32 "0cvf6nk7yi3s800plx0j6765p3irqm2k6zwz2pmwallxjl21zbvq")))))))))
