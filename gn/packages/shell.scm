(define-module (gn packages shell)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  )

(define-public shunit2-old ;; maintain old version for gemma and sambamba, for now
  (let ((commit "60dd60bcd1573befe38465010263ab242e55811d"))
    (package
      (name "shunit2-old")
      (version (string-append "2.0.4-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kward/shunit2.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32 "11savxc6qliqv25kv59qak6j7syjv95hbpmq1szn1mzn32g2gc25"))))
    (inputs `(
              ("coreutils" ,coreutils) ; for mktemp and od
              ))
      (build-system gnu-build-system)
      (arguments
       `(
         #:tests? #f ;; no test-suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (add-after 'unpack 'replace-binary-paths
                      (lambda _
                        (substitute* "source/2.0/src/shell/shunit2"
                                     (("/bin/sh") (which "sh"))
                                     (("exec mktemp") (string-append "exec " (which "mktemp")))
                                     (("/usr/bin/od") (which "od"))
                                     )#t))

           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (shunit2-exec (string-append bin "/shunit2")))
                 (write (file-exists? "source/2.0/src/shell/shunit2"))
                 (write (format #t "build directory: ~s~%" (getcwd)))
                 (mkdir-p bin)
                 (copy-file "source/2.0/src/shell/shunit2" shunit2-exec)
                 (chmod shunit2-exec #o555)
                 #t))))
         ))
      (home-page "https://code.google.com/archive/p/shunit2/")
      (synopsis "xUnit based unit testing for Unix shell scripts")
      (description
       "shUnit2 is a xUnit unit test framework for Bourne based shell
scripts, and it is designed to work in a similar manner to JUnit,
PyUnit, etc. If you have ever had the desire to write a unit test for
a shell script, shUnit2 can do the job.")
      (license license:lgpl2.0))))
