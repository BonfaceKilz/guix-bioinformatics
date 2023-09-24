;; Datastructures module

(define-module (gn packages datastructures)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages datastructures))

(define sdsl-lite-x86-64-v2
  (package/inherit sdsl-lite
    (name "sdsl-lite-x86-64-v2")
    (arguments
     (substitute-keyword-arguments (package-arguments sdsl-lite)
       ((#:configure-flags flags #~'())
        #~(append (list "-DCMAKE_CXX_FLAGS=-march=x86-64-v2"
                        "-DCMAKE_INSTALL_LIBDIR=lib/glibc-hwcaps/x86-64-v2"
                        (string-append "-DCMAKE_INSTALL_RPATH=" #$output
                                       "/lib/glibc-hwcaps/x86-64-v2"))
                  #$flags))
       ;; The building machine can't necessarily run the code produced.
       ((#:tests? _ #t) #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-extra-files
              (lambda _
                (delete-file-recursively (string-append #$output "/include"))))
            (replace 'install-static-library
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((lib "/lib/glibc-hwcaps/x86-64-v2/libsdsl.a"))
                  (mkdir-p (dirname (string-append #$output:static lib)))
                  (copy-file "lib/libsdsl_static.a"
                             (string-append #$output:static lib)))))))))
    (supported-systems '("x86_64-linux"))
    (properties `((hidden? . #t)))))

(define sdsl-lite-x86-64-v3
  (package/inherit sdsl-lite
    (name "sdsl-lite-x86-64-v3")
    (arguments
     (substitute-keyword-arguments (package-arguments sdsl-lite)
       ((#:configure-flags flags #~'())
        #~(append (list "-DCMAKE_CXX_FLAGS=-march=x86-64-v3"
                        "-DCMAKE_INSTALL_LIBDIR=lib/glibc-hwcaps/x86-64-v3"
                        (string-append "-DCMAKE_INSTALL_RPATH=" #$output
                                       "/lib/glibc-hwcaps/x86-64-v3"))
                  #$flags))
       ;; The building machine can't necessarily run the code produced.
       ((#:tests? _ #t) #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-extra-files
              (lambda _
                (delete-file-recursively (string-append #$output "/include"))))
            (replace 'install-static-library
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((lib "/lib/glibc-hwcaps/x86-64-v3/libsdsl.a"))
                  (mkdir-p (dirname (string-append #$output:static lib)))
                  (copy-file "lib/libsdsl_static.a"
                             (string-append #$output:static lib)))))))))
    (supported-systems '("x86_64-linux"))
    (properties `((hidden? . #t)))))

(define sdsl-lite-x86-64-v4
  (package/inherit sdsl-lite
    (name "sdsl-lite-x86-64-v4")
    (arguments
     (substitute-keyword-arguments (package-arguments sdsl-lite)
       ((#:configure-flags flags #~'())
        #~(append (list "-DCMAKE_CXX_FLAGS=-march=x86-64-v4"
                        "-DCMAKE_INSTALL_LIBDIR=lib/glibc-hwcaps/x86-64-v4"
                        (string-append "-DCMAKE_INSTALL_RPATH=" #$output
                                       "/lib/glibc-hwcaps/x86-64-v4"))
                  #$flags))
       ;; The building machine can't necessarily run the code produced.
       ((#:tests? _ #t) #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-extra-files
              (lambda _
                (delete-file-recursively (string-append #$output "/include"))))
            (replace 'install-static-library
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((lib "/lib/glibc-hwcaps/x86-64-v4/libsdsl.a"))
                  (mkdir-p (dirname (string-append #$output:static lib)))
                  (copy-file "lib/libsdsl_static.a"
                             (string-append #$output:static lib)))))))))
    (supported-systems '("x86_64-linux"))
    (properties `((hidden? . #t)))))

(define-public sdsl-lite-hwcaps
  (package/inherit sdsl-lite
    (name "sdsl-lite-hwcaps")
    (arguments
     (substitute-keyword-arguments (package-arguments sdsl-lite)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'install-optimized-libraries
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((hwcaps "/lib/glibc-hwcaps"))
                  (copy-recursively
                    (string-append (assoc-ref inputs "sdsl-lite-x86-64-v2")
                                   hwcaps "/x86-64-v2")
                    (string-append #$output hwcaps "/x86-64-v2"))
                  (copy-recursively
                    (string-append (assoc-ref inputs "sdsl-lite-x86-64-v3")
                                   hwcaps "/x86-64-v3")
                    (string-append #$output hwcaps "/x86-64-v3"))
                  (copy-recursively
                    (string-append (assoc-ref inputs "sdsl-lite-x86-64-v4")
                                   hwcaps "/x86-64-v4")
                    (string-append #$output hwcaps "/x86-64-v4")))))))))
    (native-inputs
     (modify-inputs (package-native-inputs sdsl-lite)
                    (append sdsl-lite-x86-64-v2
                            sdsl-lite-x86-64-v3
                            sdsl-lite-x86-64-v4)))
    (properties `((tunable? . #f)))))
