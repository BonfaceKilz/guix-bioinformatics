(define-module (gn services pluto))

(use-modules (gnu)
             (gn packages julia)
             (guix modules)
             (guix records)
             (ice-9 match))
(use-service-modules shepherd)
(use-package-modules
  admin
  certs)

(define-record-type* <pluto-configuration>
  pluto-configuration
  make-pluto-configuration
  pluto-configuration?
  (package  pluto-configuration-package     ; package
            (default julia-visuals))
  ;; TODO: Currently port 4343 is hardcoded in the package definition.
  (port     pluto-configuration-port        ; integer
            (default 80)))

(define %julia-account
  (list (user-group
          (name "julia")
          (system? #t))
        (user-account
          (name "julia")
          (group "julia")
          (system? #t)
          (comment "Julia User")
          (home-directory "/home/jovyan")
          (shell (file-append shadow "/sbin/nologin")))))

(define pluto-shepherd-service
  (match-lambda
    (($ <pluto-configuration> package port)
     (with-imported-modules (source-module-closure
                              '((gnu build shepherd)
                                (gnu system file-systems)))
       (list (shepherd-service
               (provision '(pluto))
               (requirement '(networking))
               (modules '((gnu build shepherd)
                          (gnu system file-systems)))
               (start #~(make-forkexec-constructor/container
                          ;(list #$(file-append package "/runpluto.sh") #$port)
                          (list #$(file-append package "/runpluto"))
                          #:log-file "/var/log/pluto.log"
                          #:user "julia"
                          #:group "julia"
                          ;; This prevents the service from using /root as $HOME.
                          #:environment-variables '()
                          #:mappings (list (file-system-mapping
                                             (source "/home/jovyan")
                                             (target source)
                                             (writable? #t))
                                           (file-system-mapping
                                             (source "/etc/ssl")
                                             (target source)))))
               (stop  #~(make-kill-destructor))))))))

(define pluto-service-type
  (service-type
    (name 'pluto)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           pluto-shepherd-service)
        (service-extension account-service-type
                           (const %julia-account))))
    (default-value (pluto-configuration))
    (description
     "Run a Pluto Jupyter Webserver.")))

(operating-system
  (host-name "pluto")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("does-not-matter"))))
  (file-systems (list (file-system
                        (device "does-not-matter")
                        (mount-point "/")
                        (type "does-not-matter"))))
  ;; TODO: A more minimal kernel for use in a docker image
  ;; (kernel linux-libre-vm)
  ;; No firmware for VMs.
  (firmware '())
  (packages (list nss-certs))
  ;; For testing
  ;(packages (cons* nss-certs %base-packages))

  (setuid-programs '())

  (services (list (service pluto-service-type
                           (pluto-configuration
                             (port "4343"))))))

;; guix system container -L /path/to/guix-bioinformatics/ -L /path/to/guix-past/modules/ /path/to/guix-bioinformatics/gn/services/pluto.scm --network
;; For docker it isn't necessary to list the shared folders at build time.
;; guix system docker-image -L /path/to/guix-bioinformatics/ -L /path/to/guix-past/modules/ /path/to/guix-bioinformatics/gn/services/pluto.scm --network
;; Docker instructions:
;; docker load --input guix-docker-image.tar.gz
;; docker run -d --privileged --net=host --name pluto guix
