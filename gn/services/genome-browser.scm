(define-module (gn services genome-browser))

(use-modules (gnu)
             (gn packages bioinformatics)
             )
(use-service-modules web)

(define %hg.conf
  (plain-file "hg.conf"
              (string-append "db.host=gbdb\n"
                             "db.user=admin\n"
                             "db.password=admin\n"
                             "db.trackDb=trackDb\n"
                             "defaultGenome=Human\n"
                             "central.db=hgcentral\n"
                             "central.host=gbdb\n"
                             "central.user=admin\n"
                             "central.password=admin\n"
                             "central.domain=\n"
                             "backupcentral.db=hgcentral\n"
                             "backupcentral.host=gbdb\n"
                             "backupcentral.user=admin\n"
                             "backupcentral.password=admin\n"
                             "backupcentral.domain=\n")))

;; TODO: create 'daily clean' mcron scripts.

(define ucsc-genome-browser-port 4321)

(operating-system
  (host-name "genome-browser")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "does-not-matter")))
  (file-systems %base-file-systems)
  ;; No firmware for VMs
  (firmware '())
  (packages (cons* %base-packages))
  (services
    (list (service httpd-service-type
                   (httpd-configuration
                     (config
                       (httpd-config-file
                         (document-root (file-append ucsc-genome-browser "/html"))
                         (listen (list (number->string ucsc-genome-browser-port)))
                         (modules
                           (cons*
                             (httpd-module
                               (name "cgid_module")
                               (file "modules/mod_cgid.so"))
                             (httpd-module
                               (name "include_module")
                               (file "modules/mod_include.so"))
                             %default-httpd-modules))
                         (extra-config (list "\
TypesConfig etc/httpd/mime.types
# same as 'listen' above
<VirtualHost *:" (number->string ucsc-genome-browser-port) ">
  XBitHack On
  DocumentRoot " ucsc-genome-browser "/html
  Alias /bin " ucsc-genome-browser "/bin
  Alias /htdocs " ucsc-genome-browser "/htdocs
  <Directory " ucsc-genome-browser "/html>
    Options +Includes
    SSILegacyExprParser on
  </Directory>

  ScriptAlias /cgi-bin/ " ucsc-genome-browser "/cgi-bin/
  <Directory " ucsc-genome-browser "/cgi-bin>
    AllowOverride None
    Options +ExecCGI -MultiViews +SymLinksIfOwnerMatch
    SetHandler cgi-script
    Require all granted
  </Directory>

  <Directory /var/www/html/trash>
    Options MultiViews
    AllowOverride None
    Order allow,deny
    Allow from all
  </Directory>
</VirtualHost>")))))))))

;; guix system container -L /path/to/guix-past/modules/ -L /path/to/guix-bioinformatics/ /path/to/guix-bioinformatics/gn/services/genome-browser.scm --network
;; xdg-open http://localhost:4321
