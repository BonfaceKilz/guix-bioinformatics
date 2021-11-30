(define-module (gn services databases)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:export (virtuoso-configuration
            virtuoso-configuration-package
            virtuoso-configuration-http-server-port))

;;;
;;; Virtuoso
;;;

(define-maybe non-negative-integer)

(define (non-negative-integer? val)
  (and (integer? val)
       (not (negative? val))))

(define-configuration virtuoso-configuration
  (package
    (package virtuoso-ose)
    "The virtuoso package.")
  (http-server-port
   (maybe-non-negative-integer 'disabled)
   "The port on which to listen for HTTP connections."))

(define %virtuoso-accounts
  (list (user-group (name "virtuoso")
                    (system? #t))
        (user-account
         (name "virtuoso")
         (group "virtuoso")
         (system? #t)
         (comment "Virtuoso user")
         (home-directory "/var/lib/virtuoso")
         (shell (file-append shadow "/sbin/nologin")))))

(define (virtuoso-shepherd-service config)
  (shepherd-service
   (documentation "Run Virtuoso.")
   (provision '(virtuoso))
   (requirement '(networking))
   (start #~(make-forkexec-constructor
             (list #$(file-append (virtuoso-configuration-package config)
                                  "/bin/virtuoso-t")
                   "+foreground"
                   "+configfile"
                   #$(computed-file
                      "virtuoso.ini"
                      #~(call-with-output-file #$output
                          (lambda (port)
                            (when (not (eq? #$(virtuoso-configuration-http-server-port config)
                                            'disabled))
                              (format port "[HTTPServer]~%")
                              (format port "ServerPort = ~a~%"
                                      #$(virtuoso-configuration-http-server-port config)))))))
             #:directory "/var/lib/virtuoso"
             #:user "virtuoso"
             #:group "virtuoso"))
   (stop #~(make-kill-destructor))))

(define virtuoso-service-type
  (service-type
   (name 'virtuoso)
   (description "Run Virtuoso.")
   (extensions
    (list (service-extension account-service-type
                             (const %virtuoso-accounts))
          (service-extension shepherd-root-service-type
                             (compose list virtuoso-shepherd-service))))
   (default-value (virtuoso-configuration))))


