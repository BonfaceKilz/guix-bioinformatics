(define-module (gn services bnw-container))

(use-modules (gnu)
             (gn packages bnw)
             (guix build utils)
             (guix records)
             (ice-9 match))
(use-service-modules networking web)
(use-package-modules base)

(define-record-type* <bnw-configuration>
  bnw-configuration
  make-bnw-configuration
  bnw-configuration?
  (package          bnw-configuration-package       ; package
                    (default bnw))
  (deploy-directory bnw-deploy-directory            ; string
                    (default "/srv/http"))
  (port             bnw-configuration-port          ; list of strings
                    (default '("8881"))))

(define bnw-activation
  (match-lambda
    (($ <bnw-configuration> package _ port)
     #~(begin
         (let ((genenet "/var/lib/genenet/bnw")
               (php-fpm-uid (passwd:uid (getpw "php-fpm")))
               (php-fpm-gid (passwd:gid (getpw "php-fpm"))))
           (mkdir-p genenet)
           (copy-recursively #$(file-append package "/var_lib_genenet_bnw") genenet)
           (for-each (lambda (file-name)
                       (make-file-writable file-name)
                       (chown file-name php-fpm-uid php-fpm-gid))
                     (find-files genenet
                                 #:directories? #t)))))))

(define bnw-nginx-config
  (match-lambda
    (($ <bnw-configuration> package deploy-directory port)
     (list
       (nginx-server-configuration
         (server-name '("Bayesian Network"))
         (listen port)
         (root package)
         ;(root deploy-directory)
         (locations
           (list
             (nginx-php-location)
             ;(nginx-location-configuration
             ;  (uri "/sourcecodes/data/")
             ;  (body (list "alias /tmp/bnw/;")))
             )))))))

(define bnw-service-type
  (service-type
    (name 'bnw)
    (extensions
      (list
        (service-extension activation-service-type
                           bnw-activation)
        (service-extension nginx-service-type
                           bnw-nginx-config)
        ;; Make sure BNW doesn't get garbage collected.
        (service-extension profile-service-type
                           (compose list bnw-configuration-package))
        ;; Make sure php-fpm is instantiated.
        (service-extension php-fpm-service-type
                           (const #t))))
    (default-value (bnw-configuration))
    (description
     "Run a Bayesian Network Webserver.")))

(operating-system
  (host-name "bnw")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("does-not-matter"))))
  (file-systems %base-file-systems)
  ;; No firmware for VMs.
  (firmware '())
  ;; We don't need any packages inside the container.
  (packages (list coreutils))

  (services (list (service dhcp-client-service-type)
                  (service bnw-service-type
                           ;; The following is for testing:
                           (bnw-configuration
                             (port '("8888")))
                           ))))

;; guix system container -L ~/workspace/guix-past/modules/ -L ~/workspace/guix-bioinformatics/ ~/workspace/guix-bioinformatics/gn/services/bnw-container.scm --network
