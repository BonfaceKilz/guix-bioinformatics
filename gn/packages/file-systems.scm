(define-module (gn packages file-systems)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public lizardfs
  (package
    (name "lizardfs")
    (version "3.12.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lizardfs/lizardfs")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0zk73wmx82ari3m2mv0zx04x1ggsdmwcwn7k6bkl5c0jnxffc4ax"))
        (patches (search-patches "lizardfs-issues-found-on-Fedora-34-using-GCC-11.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DENABLE_CLIENT_LIB=YES"
             "-DENABLE_TESTS=YES"
             "-DENABLE_STATIC=NO"
             "-DENABLE_VERBOSE_ASCIIDOC=YES"
             "-DENABLE_TCMALLOC=NO"
             ;"-DLIB_SUBDIR=lib" ; no 64 suffix
             ;; Some directories need to be changed
             ;"-DRUN_SUBDIR=/var/run/lizardfs"
             ;"-DDATA_SUBDIR=/var/lib/lizardfs"
             ;"-DETC_SUBDIR=/etc/lizardfs"
             "-DENABLE_UTILS=YES")
       ;; Tests involve setting up a lizardfs instance and run as root.
       ;; We will make do with just building the tests.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure-through-cmakelists.txt
           (lambda _
             ;; For some reason some configure flags don't work.
             (substitute* "CMakeLists.txt"
               (("\"64\"") "\"\"")
               (("var/run/mfs") "/var/run/lizardfs")
               (("var/lib/mfs") "/var/lib/lizardfs")
               (("etc/mfs") "/etc/lizardfs"))
             ;; Then adjust the install instructions.
             (substitute* "src/data/CMakeLists.txt"
               (("^install.*") ""))))
         (add-after 'unpack 'use-system-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gtest  (assoc-ref inputs "googletest"))
                   (spdlog (assoc-ref inputs "spdlog")))
               ;; Don't attempt to download any sources.
               (substitute* "cmake/Libraries.cmake"
                 (("ENABLE_TESTS") "DOWNLOAD_GTEST")
                 (("# Find GoogleTest" all)
                  (string-append "set(GTEST_NAME gtest)\n" all))
                 (("GTEST_INCLUDE_DIRS.*")
                  (string-append "GTEST_INCLUDE_DIRS " gtest "/include)\n"))
                 (("download_external\\(SPDLOG" all)
                  (string-append "if(DOWNLOAD_SPDLOG)\n" all))
                 (("\"spdlog-1\")")
                  (string-append "\"spdlog-1\")\n"
                                 "endif()\n"
                                 "find_package(spdlog CONFIG REQUIRED)"))
                 (("include_directories(external/spdlog-0.14.0/include)") ""))

               ;; Configure the location of the googletest sources.
               (copy-recursively gtest "external/googletest")
               (substitute* "external/CMakeLists.txt"
                 (("add_subdirectory.*") "add_subdirectory(googletest)\n"))

               ;; Compatability with spdlog1.2.0+
               ;; https://github.com/lizardfs/lizardfs/issues/774
               (substitute* "src/common/slogger.h"
                 (("spdlog/spdlog.h\"")
                  (string-append "spdlog/spdlog.h\"\n"
                                 "#include \"spdlog/sinks/rotating_file_sink.h\"\n"
                                 "#include \"spdlog/sinks/syslog_sink.h\"\n"
                                 "#include \"spdlog/sinks/stdout_color_sinks.h\"")))
               (substitute* "src/common/slogger.cc"
                 (("spdlog::syslog_logger\\(\"syslog\"\\);")
                  "spdlog::syslog_logger_mt(\"syslog\");"))

               ;; fix FTBFS with glibc-2.28; for makedev
               ;; https://github.com/lizardfs/lizardfs/issues/655
               (substitute* "src/chunkserver/iostat.h"
                 (("sys/stat.h>") "sys/stat.h>\n#include <sys/sysmacros.h>")))))
         (add-after 'install 'install-extras
           ;; This got broken by changing the directories above.
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion "../source"
             (let* ((out  (assoc-ref outputs "out"))
                    (etc  (string-append out "/etc"))
                    (var  (string-append out "/var/lib/lizardfs"))
                    (data "src/data"))
               (mkdir-p (string-append etc "/bash_completion.d"))
               (mkdir-p var)
               (copy-file (string-append data "/lizardfs.completion")
                          (string-append etc "/bash_completion.d/lizardfs"))
               (copy-file (string-append data "/metadata.mfs")
                          (string-append var "/metadata.mfs.empty"))
               (for-each
                 (lambda (file)
                   (copy-file file
                              (string-append etc "/" (basename file) ".dist")))
                 (find-files data "\\.cfg(\\.in)?$")))))))))
    (inputs
     (list bdb
           boost
           fuse-2
           linux-pam
           python-2
           spdlog
           zlib))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("googletest" ,(package-source googletest-1.7))
       ("pkg-config" ,pkg-config)))
    (home-page "https://lizardfs.com/")
    (synopsis "Distributed, scalable, fault-tolerant, highly available file system")
    (description
     "LizardFS makes files secure by keeping all the data in many replicas
spread over all available servers. It can be used also to build affordable
storage because it runs without any problems on commodity hardware.  Disk and
server failures are handled transparently and most of all without any downtime
or loss of data.  If storage requirements grow, it's possible to scale an
existing LizardFS installation just by adding new servers at any time, without
any downtime.  The system will automatically move data across to the newly
added servers, as it continuously takes care of balancing disk usage across all
connected nodes.")
    (license (list license:asl2.0   ; external/crcutil
                   license:gpl3))))

(define-public googletest-1.7
  ;; Source only package
  (hidden-package
    (package
      (inherit googletest)
      (version "1.7.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/google/googletest")
                       (commit (string-append "release-" version))))
                (file-name (git-file-name "googletest" version))
                (sha256
                 (base32
                  "1yqnzrr7bgnnnwq02c5gy559mmb52pap7m1h7zd6w58dj9yvg72n")))))))
