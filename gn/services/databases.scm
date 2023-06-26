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
            virtuoso-configuration-state-directory
            virtuoso-configuration-server-ip
            virtuoso-configuration-server-port
            virtuoso-configuration-number-of-buffers
            virtuoso-configuration-maximum-dirty-buffers
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
  (state-directory virtuoso-configuration-state-directory
                   (default "/var/lib/virtuoso"))
  (server-ip virtuoso-configuration-server-ip
             (default "localhost"))
  (server-port virtuoso-configuration-server-port
               (default 1111))
  (dirs-allowed virtuoso-dirs-allowed
                (default "/var/genenetwork/virtuoso-data"))
  (number-of-buffers virtuoso-configuration-number-of-buffers
                     (default #f))
  (maximum-dirty-buffers virtuoso-configuration-maximum-dirty-buffers
                         (default #f))
  (http-server-ip virtuoso-configuration-http-server-ip
                  (default "localhost"))
  (http-server-port virtuoso-configuration-http-server-port
                    (default 8890)))

(define (virtuoso-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (for-each (lambda (file)
                    (chown file
                           (passwd:uid (getpw "virtuoso"))
                           (passwd:gid (getpw "virtuoso"))))
                  (find-files #$(virtuoso-configuration-state-directory config)
                              #:directories? #t)))))

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
   (start #~(make-forkexec-constructor
             (list #$(file-append (virtuoso-configuration-package config)
                                  "/bin/virtuoso-t")
                   "+foreground"
                   "+configfile"
                   #$(computed-file
                      "virtuoso.ini"
                      #~(call-with-output-file #$output
                          (lambda (port)
                            (when (and #$(virtuoso-configuration-server-ip config)
                                       #$(virtuoso-configuration-server-port config))
                              (format port "[Parameters]~%")
                              (format port "ServerPort = ~a:~a~%"
                                      #$(virtuoso-configuration-server-ip config)
                                      #$(virtuoso-configuration-server-port config)))
                            (when #$(virtuoso-dirs-allowed config)
                                  (format port "[DirsAllowed] = ~a~%"
                                          #$(virtuoso-dirs-allowed config)))
                            (when #$(virtuoso-configuration-number-of-buffers config)
                                  (format port "NumberOfBuffers = ~a~%"
                                          #$(virtuoso-configuration-number-of-buffers config)))
                            (when #$(virtuoso-configuration-maximum-dirty-buffers config)
                                  (format port "MaxDirtyBuffers = ~a~%"
                                          #$(virtuoso-configuration-maximum-dirty-buffers config)))
                            (when (and #$(virtuoso-configuration-http-server-ip config)
                                       #$(virtuoso-configuration-http-server-port config))
                              (format port "[HTTPServer]~%")
                              (format port "ServerPort = ~a:~a~%"
                                      #$(virtuoso-configuration-http-server-ip config)
                                      #$(virtuoso-configuration-http-server-port config)))))))
             #:directory #$(virtuoso-configuration-state-directory config)
             #:user "virtuoso"
             #:group "virtuoso"
             #:log-file "/var/log/virtuoso.log"))
   (stop #~(make-kill-destructor))))

(define virtuoso-service-type
  (service-type
   (name 'virtuoso)
   (description "Run Virtuoso.")
   (extensions
    (list (service-extension activation-service-type
                             virtuoso-activation)
          (service-extension account-service-type
                             (const %virtuoso-accounts))
          (service-extension shepherd-root-service-type
                             (compose list virtuoso-shepherd-service))))
   (default-value (virtuoso-configuration))))


