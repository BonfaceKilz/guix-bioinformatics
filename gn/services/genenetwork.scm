(define-module (gn services genenetwork))

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
               (target "does-not-matter")))
  (file-systems %base-file-systems)
  ;; No firmware for VMs
  (firmware '())

  (packages (cons* python-2.4
                   python24-htmlgen-GN1
                   python24-json-GN1
                   python24-mysqlclient ; MySQLdb
                   python24-numarray
                   python24-piddle
                   python24-pp-GN1  ; not python24-parallel, we need an older version
                   python24-pyx
                   python24-pyxlwriter
                   python24-qtlreaper
                   python24-rpy2
                   python24-svg-GN1
                   %base-packages))

  (services (list (service special-files-service-type
                           ;; The genotypes folder doesn't have it's shebangs patched.
                           `(("/usr/bin/python" ,(file-append python-2.4 "/bin/python"))))
                  (service httpd-service-type
                           (httpd-configuration
                             ;; Must be a httpd-2.2 variant.
                             (package httpd22-with-mod-python)
                             (config GN1-httpd-config))))))

;; guix system container -L /path/to/guix-past/modules/ -L /path/to/guix-bioinformatics/ /path/to/guix-bioinformatics/gn/services/genenetwork.scm --network --expose=/gnshare/gn/web/genotypes
;; xdg-open http://localhost:8042
