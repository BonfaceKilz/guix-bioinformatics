;; See dockerfile for some clarification about choices:
;; https://github.com/icebert/docker_ucsc_genome_browser/blob/master/Dockerfile
;; http://genome.ucsc.edu/goldenPath/help/mirrorManual.html
(define-module (gn services genome-browser))

(use-modules (gnu)
             (guix packages)
             (gn packages bioinformatics))
(use-service-modules
  databases
  networking
  web)
(use-package-modules
  bash
  databases
  ghostscript
  wget)

(define %hg.conf
  (mixed-text-file "hg.conf"
                   "browser.documentRoot=" ucsc-genome-browser "/html\n"
                   "db.host=localhost\n"
                   "db.user=readonly\n"
                   "db.password=access\n"
                   "db.trackDb=trackDb\n"
                   "gbdbLoc1=/gbdb/\n"
                   "gbdbLoc2=http://hgdownload.soe.ucsc.edu/gbdb/\n"
                   ;# To disable on-the-fly loading of mysql data, comment out these lines.
                   "slow-db.host=genome-mysql.soe.ucsc.edu\n"
                   "slow-db.user=genomep\n"
                   "slow-db.password=password\n"
                   "defaultGenome=Mouse\n"
                   "central.db=hgcentral\n"
                   "central.host=localhost\n"
                   "central.socket=/run/mysqld/mysqld.sock\n"   ; default for mysql service
                   "central.user=readwrite\n"
                   "central.password=update\n"
                   "central.domain=\n"
                   "backupcentral.db=hgcentral\n"
                   "backupcentral.host=localhost\n"
                   "backupcentral.user=readwrite\n"
                   "backupcentral.password=update\n"
                   "backupcentral.domain=\n"
                   "freeType=on\n"
                   "freeTypeDir=" gs-fonts "/share/fonts/type1/ghostscript\n"
                   ;"hgc.psxyPath=/hive/data/outside/GMT4.3.1/bin/psxy"
                   ;"hgc.ps2rasterPath=""/bin/ps2raster"
                   "hgc.ghostscriptPath=" ghostscript "/bin/gs\n"   ; needed?
                   "udc.cacheDir=/var/www/html/trash/udcCache\n"    ; default is /tmp/udcCache
                   ))

(define %startup-script
  (mixed-text-file "create_databases.sh"
                   wget "/bin/wget http://hgdownload.soe.ucsc.edu/admin/hgcentral.sql\n"
                   bash-minimal "/bin/sh " (package-source ucsc-genome-browser) "/src/product/ex.MySQLUserPerms.sh\n"
                   mariadb "/bin/mysql -e \"create database hgcentral;\" mysql\n"
                   mariadb "/bin/mysql hgcentral < hgcentral.sql\n"
                   coreutils-minimal "/bin/mkdir -p /var/www/html/trash\n"
                   coreutils-minimal "/bin/chown -R httpd:httpd /var/www\n"
                   ))

;; TODO:
;;  create 'daily clean' mcron scripts.
;;  move /var/www/html/trash to /gbdb/trash?
;;  Fix from main page:
;;      hgVai
;;      hgIntegrator
;;  from 'more tools'
;;      hgPhyloPlace

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
  (packages (cons* mariadb  ; for create-db script
                   %base-packages))
  (services
    (list (service mysql-service-type)
          (service special-files-service-type
                   `(("/root/create_hgcentral" ,%startup-script)
                     ("/var/lib/genome/hg.conf" ,%hg.conf)))
          (service inetd-service-type
                   (inetd-configuration
                     (entries
                       (list
                         (inetd-entry
                           (node "127.0.0.1")
                           (name "blat")    ; yes, it's named blat
                           (socket-type 'stream)
                           (protocol "tcp") ; probably?
                           (wait? #f)
                           (user "httpd:httpd") ; or dedicated user. Needs write access.
                           (program (file-append ucsc-genome-browser "/bin/gfServer"))
                           (arguments
                             '("gfServer" "dynserver" "/gbdb")))))))
          (syslog-service)  ; needed by inetd
          (service httpd-service-type
                   (httpd-configuration
                     (config
                       (httpd-config-file
                         (document-root (file-append ucsc-genome-browser "/html"))
                         (server-name "Genome_Browser")
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
# cgid.sock needs to be creatable, not in the store
ScriptSock /var/run/cgid.sock
# same as 'listen' above
<VirtualHost *:" (number->string ucsc-genome-browser-port) ">
  XBitHack On
  DocumentRoot " ucsc-genome-browser "/html
  Alias /bin " ucsc-genome-browser "/bin
  #Alias /cgi-bin " ucsc-genome-browser "/cgi-bin   # causes cgi scripts to fail to render
  Alias /htdocs " ucsc-genome-browser "/htdocs
  #Alias /trash /var/www/html/trash # this is wrong
  Alias /var/www/html/trash /var/www/html/trash
  <Directory " ucsc-genome-browser "/html>
    Options +Includes +FollowSymLinks +Indexes
    AllowOverride None
    <IfModule mod_authz_host.c>
      Require all granted
      SSILegacyExprParser on
    </IfModule>
  </Directory>

  ScriptAlias /cgi-bin " ucsc-genome-browser "/cgi-bin
  <Directory " ucsc-genome-browser "/cgi-bin>
    AllowOverride None
    Options +ExecCGI -MultiViews +SymLinksIfOwnerMatch +Includes +FollowSymlinks
    Order allow,deny
    #SetHandler cgi-script
    Require all granted
    Allow from all
  </Directory>

  <Directory /var/www/html/trash>
    Options MultiViews
    AllowOverride None
    Order allow,deny
    Allow from all
  </Directory>
</VirtualHost>")))))))))

;; guix system container -L /path/to/guix-past/modules/ -L /path/to/guix-bioinformatics/ /path/to/guix-bioinformatics/gn/services/genome-browser.scm --network --share=/path/to/gbdb=/gbdb
;; ALSO need to share in the external database?
;; Probably not, it falls back to http://hgdownload.soe.ucsc.edu/gbdb/
;; Can skip the %startup-script dance if /var/lib/mysql is stored outside of the container, but might need /var/www/html/trash too then.
;; xdg-open http://localhost:4321
