(define-module (gn services genenetwork1))

(use-modules (gnu)
             (past packages python)
             (past packages web)
             (gn packages genenetwork)
             (gn packages python24)
             (gn services gn1-httpd-config))
(use-service-modules web)

(operating-system
  (host-name "genenetwork")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("does-not-matter"))))
  (file-systems %base-file-systems)
  ;; No firmware for VMs
  (firmware '())
  (packages (cons* %base-packages))
  (services (list (service special-files-service-type
                           ;; The genotypes folder doesn't have it's shebangs patched.
                           `(("/usr/bin/python" ,(file-append python-2.4 "/bin/python"))))
                  (service httpd-service-type
                           (httpd-configuration
                             ;; Must be a httpd-2.2 variant.
                             (package httpd22-with-mod-python)
                             (config GN1-httpd-config))))))

;; guix system container -L /path/to/guix-past/modules/ -L /path/to/guix-bioinformatics/ /path/to/guix-bioinformatics/gn/services/genenetwork1.scm --network --expose=/gnshare/gn/web/genotypes
;; xdg-open http://localhost:8042
