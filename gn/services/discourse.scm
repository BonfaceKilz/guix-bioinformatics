(define-module (gn services discourse))

(use-modules (gnu)
             (gn packages ruby)
             (guix modules)
             (guix records)
             (ice-9 match))
(use-service-modules networking shepherd web)
(use-package-modules node ruby)

(define-record-type* <discourse-configuration>
  discourse-configuration
  make-discourse-configuration
  discourse-configuration?
  (package          discourse-configuration-package     ; package
                    (default discourse))
  (deploy-directory discourse-deploy-directory          ; string
                    (default "/srv/http"))
  (port             discourse-configuration-port        ; list of strings
                    (default '("3000")))
  (bundler          discourse-configuration-bundler     ; package
                    (default bundler))
  )

(define discourse-activation
  (match-lambda
    (($ <discourse-configuration> package deploy-directory port bundler)
     #~(begin
         (mkdir-p #$deploy-directory)
         (copy-recursively #$package #$deploy-directory)
         (for-each make-file-writable (find-files #$deploy-directory))
         (with-directory-excursion #$deploy-directory
           ;; copied from the discourse package.
           (substitute* "Gemfile"
             ;; Don't require specific versions of these gems
             (("6.0.3.3") (package-version ruby-rails))
             (("2.0.1") (package-version ruby-sassc))
             (("active_model_serializers.*") "active_model_serializers'\n")
             ;; Add tzinfo-data and figure out how to use non-Ruby version later
             (("active_model_serializers'")
              "active_model_serializers'\ngem 'tzinfo-data'")
             ;; ruby-cppjieba-rb never finishes the install phase with ruby-2.6
             ((".*cppjieba_rb.*") ""))
           (invoke #$(file-append node "/bin/npm") "install" "svgo")
           )
         ))))

(define discourse-service
  (match-lambda
    (($ <discourse-configuration> package deploy-directory port bundler)
     (with-imported-modules (source-module-closure
                              '((gnu build shepherd)
                                (gnu system file-systems)))
       (list (shepherd-service
               (provision '(discourse))
               (requirement '(networking))
               (modules '((gnu build shepherd)
                          (gnu system file-systems)))
               (start #~(make-forkexec-constructor/container
                          (list #$(file-append package "/server.py"))
                          ;; Needs to run from the directory it is located in.
                          #:directory #$deploy-directory
                          #:log-file "/var/log/discourse.log"
                          ;; We don't need to set TMPDIR because we're inside a container.
                          #:environment-variables
                          '(
                            ;"EDIRECT_PUBMED_MASTER=/export2/PubMed"
                            ;"NLTK_DATA=/var/cache/nltk_data"
                            ;"PERL_LWP_SSL_CA_FILE=/etc/ssl/certs/ca-certificates.crt"
                            )
                          ;#:mappings (list (file-system-mapping
                          ;                   (source "/export2/PubMed")
                          ;                   (target source)
                          ;                   (writable? #t))
                          ;                 (file-system-mapping
                          ;                   (source "/export/ratspub")
                          ;                   (target source)
                          ;                   (writable? #t))
                          ;                 (file-system-mapping
                          ;                   (source "/var/cache/nltk_data")
                          ;                   (target source))
                          ;                 (file-system-mapping
                          ;                   (source "/etc/ssl/certs")
                          ;                   (target source)))
                          ))
               (stop  #~(make-kill-destructor)))))
     )))

(define discourse-service-type
  (service-type
    (name 'discourse)
    (extensions
      (list
        (service-extension activation-service-type
                           discourse-activation)
        (service-extension shepherd-root-service-type
                           discourse-service)
        ;; Make sure discourse doesn't get garbage collected.
        (service-extension profile-service-type
                           (compose list discourse-configuration-package))
        ;; Make sure php-fpm is instantiated.
        (service-extension php-fpm-service-type
                           (const #t))))
    (default-value (discourse-configuration))
    (description
     "Run an instance of Discourse.")))

(operating-system
  (host-name "discourse")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("does-not-matter"))))
  (file-systems %base-file-systems)
  ;; No firmware for VMs.
  (firmware '())
  (packages (append
              (list node)
              %base-packages))

  (services (list (service dhcp-client-service-type)
                  (service discourse-service-type
                           ;; The following is for testing:
                           ;(discourse-configuration
                           ;  (port '("3333")))
                           ))))
