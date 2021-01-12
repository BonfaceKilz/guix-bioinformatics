(define-module (gn services gn1-httpd-config)
               #:export (%default-httpd22-modules
                         GN1-httpd-config))

(use-modules (gnu)
             (ice-9 match)
             (gn packages genenetwork)
             (past packages web))
(use-service-modules web)

(define %default-httpd22-modules
  (map (match-lambda
         ((name file)
          (httpd-module
           (name name)
           (file file))))
       '(("authn_file_module" "modules/mod_authn_file.so")
         ("authn_dbm_module" "modules/mod_authn_dbm.so")
         ("authn_anon_module" "modules/mod_authn_anon.so")
         ("authn_dbd_module" "modules/mod_authn_dbd.so")
         ("authn_default_module" "modules/mod_authn_default.so")
         ("authz_host_module" "modules/mod_authz_host.so")
         ("authz_groupfile_module" "modules/mod_authz_groupfile.so")
         ("authz_user_module" "modules/mod_authz_user.so")
         ("authz_dbm_module" "modules/mod_authz_dbm.so")
         ("authz_owner_module" "modules/mod_authz_owner.so")
         ("authz_default_module" "modules/mod_authz_default.so")
         ("auth_basic_module" "modules/mod_auth_basic.so")
         ("auth_digest_module" "modules/mod_auth_digest.so")
         ("dbd_module" "modules/mod_dbd.so")
         ("dumpio_module" "modules/mod_dumpio.so")
         ("reqtimeout_module" "modules/mod_reqtimeout.so")
         ("ext_filter_module" "modules/mod_ext_filter.so")
         ("include_module" "modules/mod_include.so")
         ("filter_module" "modules/mod_filter.so")
         ("substitute_module" "modules/mod_substitute.so")
         ("log_config_module" "modules/mod_log_config.so")
         ("logio_module" "modules/mod_logio.so")
         ("env_module" "modules/mod_env.so")
         ("mime_magic_module" "modules/mod_mime_magic.so")
         ("expires_module" "modules/mod_expires.so")
         ("headers_module" "modules/mod_headers.so")
         ("ident_module" "modules/mod_ident.so")
         ("setenvif_module" "modules/mod_setenvif.so")
         ("version_module" "modules/mod_version.so")
         ("ssl_module" "modules/mod_ssl.so")
         ("mime_module" "modules/mod_mime.so")
         ("dav_module" "modules/mod_dav.so")
         ("status_module" "modules/mod_status.so")
         ("autoindex_module" "modules/mod_autoindex.so")
         ("asis_module" "modules/mod_asis.so")
         ("info_module" "modules/mod_info.so")
         ("cgi_module" "modules/mod_cgi.so")
         ("dav_fs_module" "modules/mod_dav_fs.so")
         ("vhost_alias_module" "modules/mod_vhost_alias.so")
         ("negotiation_module" "modules/mod_negotiation.so")
         ("dir_module" "modules/mod_dir.so")
         ("imagemap_module" "modules/mod_imagemap.so")
         ("actions_module" "modules/mod_actions.so")
         ("speling_module" "modules/mod_speling.so")
         ("userdir_module" "modules/mod_userdir.so")
         ("alias_module" "modules/mod_alias.so")
         ("rewrite_module" "modules/mod_rewrite.so"))))

(define GN1-httpd-config
  (httpd-config-file
    (server-name "gn1-test.genenetwork.org")
    ;; Defaults to httpd, should be same as 'package' above to launch service.
    (server-root httpd22-with-mod-python)
    (user "nobody")
    (group "root")
    (pid-file "/tmp/guix-gn1/httpd-genenetwork1.pid")
    (error-log "/tmp/guix-gn1/httpd-genenetwork1-error-log")
    (listen '("8042"))
    (modules (cons*
               (httpd-module
                 (name "python_module")
                 (file "modules/mod_python.so"))
               %default-httpd22-modules))
    (extra-config (list "\
TypesConfig " httpd22-with-mod-python "/etc/httpd/mime.types
DefaultType application/octet-stream
# DocumentRoot MUST NOT be in the PythonPath. Because genenetwork1 must be in PythonPath we leave the document-root keyword above unset.
PythonPath \"sys.path+['/run/current-system/profile/lib/python2.4', '/run/current-system/profile/lib/python2.4/site-packages', '" genenetwork1 "/web/webqtl']\"
# same as 'listen' above
NameVirtualHost *:8042
<VirtualHost *:8042>
  DocumentRoot "genenetwork1 "/web/webqtl
  Alias /images "genenetwork1 "/web/images/
  Alias /javascript "genenetwork1 "/web/javascript/
  Alias /css "genenetwork1 "/web/css/
  <Directory "genenetwork1 "/web/images>
    AllowOverride None
    Order allow,deny
    Allow from all
  </Directory>
  <Directory "genenetwork1 "/web/javascript>
    AllowOverride None
    Order allow,deny
    Allow from all
  </Directory>
  <Directory "genenetwork1 "/web/css>
    AllowOverride None
    Order allow,deny
    Allow from all
  </Directory>
</VirtualHost>
<Directory " genenetwork1 "/web/webqtl>
  PythonOption session FileSession
  #what is the difference between these two?
  #AddHandler mod_python .py
  SetHandler python-program
  #publisher has more debug information
  PythonHandler " genenetwork1 "/web/webqtl/main.py
  #PythonHandler mod_python.publisher
  #PythonHandler mod_python.cgihandler
  # only while debugging:
  PythonDebug On
</Directory>
# only while debugging:
<Location /mpinfo>
  SetHandler python-program
  PythonHandler mod_python.testhandler
</Location>"))))
