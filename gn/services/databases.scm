(define-module (gn services databases)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (virtuoso-configuration
            virtuoso-configuration?
            virtuoso-configuration-package
            virtuoso-configuration-http-server-ip
            virtuoso-configuration-http-server-port
            virtuoso-service-type))

;;;
;;; Virtuoso
;;;

(define-record-type* <virtuoso-configuration>
  virtuoso-configuration make-virtuoso-configuration
  virtuoso-configuration?
  (package virtuoso-configuration-package
           (default virtuoso-ose))
  (http-server-ip virtuoso-configuration-http-server-ip
                  (default "localhost"))
  (http-server-port virtuoso-configuration-http-server-port
                    (default 8890)))

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
                            (when (and #$(virtuoso-configuration-http-server-ip config)
                                       #$(virtuoso-configuration-http-server-port config))
                              (format port "[HTTPServer]~%")
                              (format port "ServerPort = ~a:~a~%"
                                      #$(virtuoso-configuration-http-server-ip config)
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


