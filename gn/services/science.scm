(define-module (gn services science)
  #:export (munge-configuration
            munge-configuration?
            munge-service-type))

(use-modules (gnu)
             (guix records)
             (ice-9 match))
(use-service-modules shepherd)
(use-package-modules admin parallel)

(define %munge-accounts
  (list (user-group
          (name "munge")
          (id 900)
          (system? #t))
        (user-account
          (name "munge")
          (group "munge")
          (uid 900)
          (system? #t)
          (comment "Munge User")
          (home-directory "/var/lib/munge")
          (shell (file-append shadow "/sbin/nologin")))))

(define %slurm-accounts
  (list (user-group
          (name "slurm")
          (id 901)
          (system? #t))
        (user-account
          (name "slurm")
          (group "slurm")
          (uid 901)
          (system? #t)
          (comment "Slurm User")
          (home-directory "/var/lib/slurm"))))

(define-record-type* <munge-configuration>
  munge-configuration
  make-munge-configuration
  munge-configuration?
  (package      munge-configuration-package
                (default munge))
  (socket       munge-configuration-socket
                (default "/var/run/munge/munge.socket.2"))
  (pid-file     munge-configuration-pid-file
                (default "/var/run/munge/munged.pid"))
  (log-file     munge-configuration-log-file
                (default "/var/log/munge/munged.log"))
  (key          munge-configuration-key
                (default "/etc/munge/munge.key")))

(define-record-type* <slurm-configuration>
  slurm-configuration
  make-slurm-configuration
  slurm-configuration?
  (package      slurm-configuration-package
                (default slurm)))

(define (munge-activation config)
  "Return the activation GEXP for CONFIG for the munge service."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (rnrs bytevectors)
                     (rnrs io ports))
        (define %user (getpw "munge"))
        (let* ((homedir     (passwd:dir %user))
               (key         #$(munge-configuration-key config))
               (etc-dir     (dirname key))
               (run-dir     (dirname #$(munge-configuration-pid-file config)))
               (log-dir     (dirname #$(munge-configuration-log-file config))))
          (for-each (lambda (dir)
                      (unless (file-exists? dir)
                        (mkdir-p dir))
                      (chown dir (passwd:uid %user) (passwd:gid %user))
                      (chmod dir #o700))
                    (list homedir etc-dir log-dir))
          (unless (file-exists? key)
            ;; Borrowed from /dev/urandom in (gnu services base)
            (call-with-input-file "/dev/urandom"
              (lambda (urandom)
                (let ((buf (make-bytevector 1024)))
                  (get-bytevector-n! urandom buf 0 1024)
                  (call-with-output-file key
                    (lambda (seed)
                      (put-bytevector seed buf)))))))
          (chown key (passwd:uid %user) (passwd:gid %user))
          (chmod key #o400)
          (unless (file-exists? run-dir)
            (mkdir-p run-dir))
          (chown run-dir (passwd:uid %user) (passwd:gid %user))))))

(define (slurm-activation config)
  "Return the activation GEXP for CONFIG for the slurm service."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (unless (file-exists? "/var/lib/slurm")
          (mkdir-p "/var/lib/slurm"))
        (chown "/var/lib/slurm" (passwd:uid "slurm") (passwd:gid "slurm")))))

(define munge-shepherd-service
  (match-lambda
    (($ <munge-configuration> package socket pid-file log-file key)
     (list
       (shepherd-service
         (documentation "Munge server")
         (provision '(munge))
         (requirement '(loopback user-processes file-systems))
         (start #~(make-forkexec-constructor
                    (list #$(file-append package "/sbin/munged")
                          "--foreground"    ; "--force"
                          (string-append "--socket=" #$socket)
                          (string-append "--key-file=" #$key)
                          (string-append "--pid-file=" #$pid-file)
                          (string-append "--log-file=" #$log-file))
                    #:user "munge"
                    #:group "munge"
                    #:pid-file #$pid-file
                    #:log-file #$log-file))
         (stop #~(lambda _
                   (not (and
                          (list #$(file-append package "/sbin/munged")
                                (string-append "--socket=" #$socket)
                                "--stop")
                          ;; This seems to not be removed by default.
                          (delete-file (string-append #$socket ".lock"))))))
         (auto-start? #t))))))

(define munge-service-type
  (service-type
    (name 'munge)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           munge-shepherd-service)
        (service-extension activation-service-type
                           munge-activation)
        (service-extension account-service-type
                           (const %munge-accounts))
        (service-extension profile-service-type
                           (compose list munge-configuration-package))))
    (default-value (munge-configuration))
    (description
     "Run a munge service.")))
