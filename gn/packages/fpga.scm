(define-module (gn packages fpga)
  #:use-module (gnu packages fpga)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public verilator-4.110
  (package
   (inherit verilator)
   (version "4.110")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (inherit (origin-uri (package-source verilator)))
                  (commit (string-append "v" version))))
            (sha256
             (base32
              "1lm2nyn7wzxj5y0ffwazhb4ygnmqf4d61sl937vmnmrpvdihsrrq"))))))
