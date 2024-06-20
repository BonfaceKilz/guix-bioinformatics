(define-module (gn packages machine-learning)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages machine-learning))

(define-public tensorflow-native
  (package
    (inherit tensorflow)
    (name "tensorflow-native")
    (arguments
     (substitute-keyword-arguments (package-arguments tensorflow)
       ((#:substitutable? _ #f) #f)
       ((#:configure-flags flags)
        `(cons
           "-Dtensorflow_OPTIMIZE_FOR_NATIVE_ARCH=ON"
           (delete "-Dtensorflow_OPTIMIZE_FOR_NATIVE_ARCH=OFF"
                   ,flags)))))))

(define-public tensowflow-native-instead-of-tensorflow
  (package-input-rewriting/spec `(("tensorflow" . ,(const tensorflow-native)))))
