(define-module (gn services gn-guile)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module ((gnu packages bioinformatics) #:select (gemma))
  #:use-module ((gnu packages guile) #:select (guile-json-4 guile-3.0))
  #:use-module ((gnu packages guile-xyz)
		#:select (guile-dbi guile-dbd-mysql guile-fibers guile-redis guile-hashing guile-commonmark))
  #:use-module ((gn packages guile) #:select (gn-guile))
  #:export (gn-guile-configuration
	    gn-guile-shepherd-service
	    gn-guile-service-type))

(define-maybe file-like)

(define-configuration gn-guile-configuration
  (guile
   (file-like guile)
   "guile package.")
  (gn-guile
   (file-like gn-guile)
   "gn-guile package.")
  (port
   (string "8091")
   "HTTP port in which gn-guile will run.")
  (home-directory
   (string "/var/lib/gn-guile")
   "Home directory for gn-guile.")
  (environment-variables
   (list '())
   "Environment variables to set for gn-guile")
  (no-serialization))

(define %gn-guile-accounts
  (list (user-group (name "gn-guile")
		    (system #t))
	(user-account
	 (name "gn-guile")
	 (group "gn-guile")
	 (system #t)
	 (comment "gn-guile user")
	 (home-directory "/var/lib/gn-guile")
	 (shell (file-append shadow "/sbin/nologin")))))

(define (gn-guile-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (for-each (lambda (file)
                    (chown file
                           (passwd:uid (getpw "gn-guile"))
                           (passwd:gid (getpw "gn-guile"))))
                  (find-files #$(gn-guile-configuration-home-directory config)
                              #:directories? #t)))))

(define (gn-guile-shepherd-service config)
  (match-record config <gn-guile-configuration>
		(gn-guile port environment-variables)
    (shepherd-service
     (documentation "Run gn-guile service.")
     (provision '(gn-guile))
     (modules `((gnu packages guile)
		,@%default-modules))
     (start #~(make-forkexec-constructor
	       (list #$(file-append guile "/bin/guile")
		     "-e"
		     "main"
		     #$(file-append gn-guile "/share/guile/site/3.0/web/webserver.scm")
		     #$port)
	       #:environment-variables
	       (list #$@environment-variables)
	       #:log-file "/var/log/gn-guile.log"))
     (stop #~(make-kill-destructor)))))

(define gn-guile-service-type
  (service-type
   (name 'gn-guile)
   (description "Run gn-guile.")
   (extensions
    (list (service-extension activation-service-type
                             gn-guile-activation)
	  (service-extension account-service-type
			     (const %gn-guile-accounts))
	  (service-extension shepherd-root-service-type
                             (compose list gn-guile-shepherd-service))))
   (default-value (gn-guile-configuration))))
