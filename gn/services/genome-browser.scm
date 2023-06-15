;; See dockerfile for some clarification about choices:
;; https://github.com/icebert/docker_ucsc_genome_browser/blob/master/Dockerfile
;; http://genome.ucsc.edu/goldenPath/help/mirrorManual.html
(define-module (gn services genome-browser))

(use-modules (gnu)
             (guix build utils)
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
                   ;"db.host=localhost\n"
                   ;"db.user=readonly\n"
                   ;"db.password=access\n"
                   "db.host=tux02\n"
                   "db.user=webqtlout\n"
                   "db.password=webqtlout\n"
                   "db.trackDb=trackDb\n"
                   ;"db.port=13306\n"
                   "gbdbLoc1=/gbdb/\n"
                   "gbdbLoc2=http://hgdownload.soe.ucsc.edu/gbdb/\n"
                   ;# To disable on-the-fly loading of mysql data, comment out these lines.
                   "slow-db.host=genome-mysql.soe.ucsc.edu\n"
                   "slow-db.user=genomep\n"
                   "slow-db.password=password\n"
                   "defaultGenome=Mouse\n"
                   "central.db=hgcentral\n"
                   ;"central.host=localhost\n"
                   ;"central.socket=/run/mysqld/mysqld.sock\n"   ; default for mysql service
                   ;"central.user=readwrite\n"
                   ;"central.password=update\n"
                   "central.host=tux02\n"
                   "central.user=webqtlout\n"
                   "central.password=webqtlout\n"
                   "central.domain=\n"
                   "backupcentral.db=hgcentral\n"
                   "backupcentral.host=tux02\n"
                   "backupcentral.user=webqtlout\n"
                   "backupcentral.password=webqtlout\n"
                   "backupcentral.domain=\n"
                   "freeType=on\n"
                   "freeTypeDir=" gs-fonts "/share/fonts/type1/ghostscript\n"
                   ;"hgc.psxyPath=" gmt "/bin/psxy"
                   ;"hgc.ps2rasterPath=" gmt "/bin/ps2raster"
                   "hgc.ghostscriptPath=" ghostscript "/bin/gs\n"   ; needed?
                   "udc.cacheDir=/var/cache/genome/udcCache\n"))    ; default is /tmp/udcCache

(define %startup-script
  (mixed-text-file "create_databases.sh"
                   wget "/bin/wget http://hgdownload.soe.ucsc.edu/admin/hgcentral.sql\n"
                   bash-minimal "/bin/sh " (package-source ucsc-genome-browser) "/src/product/ex.MySQLUserPerms.sh\n"
                   mariadb "/bin/mysql -e \"create database hgcentral;\" mysql\n"
                   mariadb "/bin/mysql hgcentral < hgcentral.sql\n"
                   coreutils-minimal "/bin/mkdir -p /var/cache/genome\n"
                   coreutils-minimal "/bin/chown -R httpd:httpd /var/www\n"))

;; This might be useful as a one-off shepherd job.
;(define %genome-activation-script
;  #~(begin
;      ;(unless (file-exists? "/var/lib/mysql/hgcentral")
;      ;  (invoke #$(file-append wget "/bin/wget") "http://hgdownload.soe.ucsc.edu/admin/hgcentral.sql" "-O" "/var/lib/mysql/hgcentral.sql")
;      ;  ;(invoke "sh" #$(file-append (package-source ucsc-genome-browser) "/src/product/ex.MySQLUserPerms.sh"))
;      ;  (invoke #$(file-append mariadb "/bin/mysql") "-e" "\"create database hgcentral;\"" "mysql")
;      ;  (invoke #$(file-append mariadb "/bin/mysql") "hgcentral" "<" "/var/lib/mysql/hgcentral.sql"))
;      (unless (file-exists? "/var/www/html/trash")
;        (mkdir-p "/var/www/html/trash"))
;      (unless (file-exists? "/var/cache/genome")
;        (mkdir-p "/var/cache/genome"))
;      (for-each (lambda (file)
;                  (chown file (passwd:uid "httpd") (passwd:gid "httpd")))
;                (append (find-files "/var/cache/genome" #:directories? #t)
;                        (find-files "/var/www/html/trash" #:directories? #t)))
;      (for-each (lambda (file)
;                  (chown file (passwd:uid "mysql") (passwd:gid "mysql")))
;                (find-files "/var/lib/mysql" #:directories? #t))))

;(define %one-month-files
;  (let ((%one-month (* 60 60 24 30)))
;    (find-files "/var/cache/genome" (lambda (file stat)
;                                      (> (stat:atime stat) %one-month)))))

;; TODO:
;;  create 'daily clean' mcron scripts. Only needed in /var/cache/genome grows too large.
;;  use rsync to make sure mouse genome data is kept up-to-date.

(define ucsc-genome-browser-port 4322)

(operating-system
  (host-name "genome-browser")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("does-not-matter"))))
  (file-systems %base-file-systems)
  ;; No firmware for VMs
  (firmware '())
  (packages (cons* mariadb  ; for create-db script, interacting with database if necessary
                   %base-packages))
  (services
    (list ;(service mysql-service-type
          ;         (mysql-configuration
          ;           (port 13306))) ; don't overlap with penguin2's mariadb
          (service special-files-service-type
                   `(;("/root/create_hgcentral" ,%startup-script)
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
          (service syslog-service-type)  ; needed by inetd
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
# More logs for more debugging.
LogFormat \"%h %l %u %t \\\"%r\\\" %>s %b\" common
CustomLog /var/log/httpd/combined_log common
#LogLevel debug
# cgid.sock needs to be creatable, not in the store
ScriptSock /var/run/cgid.sock
#XBitHack needs to not be inside the VirtualHost block.
XBitHack On
# same as 'listen' above
<VirtualHost *:" (number->string ucsc-genome-browser-port) ">
  DocumentRoot " ucsc-genome-browser "/html
  Alias /bin " ucsc-genome-browser "/bin
  #Alias /cgi-bin " ucsc-genome-browser "/cgi-bin   # causes cgi scripts to fail to render
  Alias /favicon.ico " ucsc-genome-browser "/html/faviconRR.ico
  Alias /htdocs " ucsc-genome-browser "/htdocs
  #Alias /htdocs " ucsc-genome-browser "/html
  Alias /var/www/html/trash /var/cache/genome
  Alias /var/cache/genome /var/cache/genome
  <Directory " ucsc-genome-browser "/html>
    Options +Includes
    #Options +Includes +FollowSymLinks +Indexes
    AllowOverride None
    <IfModule mod_authz_host.c>
      Require all granted
      SSILegacyExprParser on
    </IfModule>
  </Directory>

  ScriptAlias /cgi-bin " ucsc-genome-browser "/cgi-bin
  <Directory " ucsc-genome-browser "/cgi-bin>
    AllowOverride None
    Options +ExecCGI -MultiViews +SymLinksIfOwnerMatch
    #Options +ExecCGI -MultiViews +SymLinksIfOwnerMatch +Includes +FollowSymlinks
    Order allow,deny
    #SetHandler cgi-script
    Require all granted
    Allow from all
  </Directory>

  <Directory /var/cache/genome>
    Options MultiViews
    AllowOverride None
    Order allow,deny
    Allow from all
  </Directory>
  # Upstream default location, possibly expected by software
  <Directory /var/www/html/trash>
    Options MultiViews
    AllowOverride None
    Order allow,deny
    Allow from all
  </Directory>
</VirtualHost>")))))))))

;; guix system container -L /path/to/guix-past/modules/ -L /path/to/guix-bioinformatics/ /path/to/guix-bioinformatics/gn/services/genome-browser.scm --network --share=/export/efraimf/UCSC_Genome/gbdb=/gbdb --share=/export/efraimf/UCSC_Genome/var-lib-mysql=/var/lib/mysql --share=/export/efraimf/UCSC_Genome/var-cache-genome=/var/cache/genome --share=/export/efraimf/UCSC_Genome/var-cache-genome=/var/www/html/trash
;; xdg-open http://localhost:4322
