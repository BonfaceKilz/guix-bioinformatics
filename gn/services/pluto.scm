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
  (port     pluto-configuration-port        ; integer
            (default 80)))

(define %julia-account
  (list (user-group
          (name "julia")
          ;(system? #t)
          )
        (user-account
          (name "julia")
          (group "julia")
          ;(system? #t)
          (comment "Julia User")
          (home-directory "/home/jovyan")
          ;(shell (file-append shadow "/sbin/nologin"))
          )))

;(define pluto-activation
;  (match-lambda
;    (($ <ratspub-configuration> package)
;     #~(begin
;         (let ((nltk_data "/var/cache/nltk_data/tokenizers")
;               (data_dir "/export/ratspub"))
;           (unless (file-exists? "/export2/PubMed")
;             (mkdir-p "/export2/PubMed"))
;           (unless (file-exists? nltk_data)
;             (begin
;               ;; The correct way would be to use python-nltk to download the data
;               ;; python3 -m nltk.downloader -d /var/cache/nltk_data punkt
;               (mkdir-p nltk_data)
;               (chdir nltk_data)
;               (invoke #$(file-append unzip "/bin/unzip") "-q" #$%punkt.zip)))
;           (unless (file-exists? (string-append data_dir "/userspub.sqlite"))
;             (begin
;               (install-file #$(file-append package "/userspub.sqlite") data_dir)
;               (chmod (string-append data_dir "/userspub.sqlite") #o554))))))))

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
                          (list #$(file-append package "/runpluto.sh") #$port)
                          ;; Needs to run from the directory it is located in.
                          ;#:directory #$package
                          #:log-file "/var/log/pluto.log"
                          ;; We don't need to set TMPDIR because we're inside a container.
                          #:user "julia"
                          #:group "julia"
                          #:environment-variables
                          '(;"JULIA_PROJECT=/home/jovyan"
                            ;"JULIA_LOAD_PATH=/run/current-system/profile/share/julia/packages/"
                            )
                          #:mappings (list (file-system-mapping
                                             (source "/home/jovyan")
                                             (target source)
                                             (writable? #t))
                                           )
                          ))
               (stop  #~(make-kill-destructor))))))))

(define pluto-service-type
  (service-type
    (name 'pluto)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           pluto-shepherd-service)
        ;(service-extension activation-service-type
        ;                   ratspub-activation)
        (service-extension account-service-type
                           (const %julia-account))
        ;; Make sure we get all the dependencies of julia-visuals.
        (service-extension profile-service-type
                           (compose list pluto-configuration-package))))
    (default-value (pluto-configuration))
    (description
     "Run a Pluto Jupyter Webserver.")))

(operating-system
  (host-name "pluto")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "does-not-matter")))
  (file-systems (list (file-system
                        (device "does-not-matter")
                        (mount-point "/")
                        (type "does-not-matter"))))
  ;; TODO: A more minimal kernel for use in a docker image
  ;; (kernel linux-libre-vm)
  ;; No firmware for VMs.
  (firmware '())
  (packages ;(list nss-certs)
    %base-packages
            )

  (services (list (service pluto-service-type
                           (pluto-configuration
                             (port "4343"))))))

;; guix system container -L /path/to/guix-bioinformatics/ -L /path/to/guix-past/modules/ /path/to/guix-bioinformatics/gn/services/pluto.scm --network
;; For docker it isn't necessary to list the shared folders at build time.
;; guix system docker-image -L /path/to/guix-bioinformatics/ -L /path/to/guix-past/modules/ /path/to/guix-bioinformatics/gn/services/pluto.scm --network
;; Docker instructions:
;; docker load --input pluto-docker-image.tar.gz
;; docker run -d --privileged --net=host --name pluto guix
