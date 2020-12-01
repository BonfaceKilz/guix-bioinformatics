(define-module (gn services science)
  #:export (munge-configuration
            munge-configuration?
            munge-service-type

            slurm-configuration
            slurm-configuration?
            slurmd-service-type
            slurmdbd-service-type
            slurmctld-service-type))

(use-modules (gnu)
             (guix records)
             (ice-9 match))
(use-service-modules shepherd)
(use-package-modules admin parallel)

;; TODO: Make id/uid configurable
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

(define (munge-activation config)
  "Return the activation GEXP for CONFIG for the munge service."
  (with-imported-modules '((guix build utils))
    #~(begin
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
            (invoke #$(file-append (munge-configuration-package config)
                                 "/sbin/mungekey")
                    "--create"
                    (string-append "--bits=" (number->string (* 8 1024))) ; bits, not bytes
                    (string-append "--keyfile=" key)))
          (chown key (passwd:uid %user) (passwd:gid %user))
          (chmod key #o400)
          (unless (file-exists? run-dir)
            (mkdir-p run-dir))
          (chown run-dir (passwd:uid %user) (passwd:gid %user))))))

(define munge-shepherd-service
  (match-lambda
    (($ <munge-configuration> package socket pid-file log-file key)
     (list
       (shepherd-service
         (documentation "Munge server")
         (provision '(munge))
         (requirement '(loopback user-processes))
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
     "Run @url{https://dun.github.io/munge/,Munge}, an authentication service.")))

;; Initial documentation for upstreaming:
;@subsubheading Munge
;
;The following example describes a Munge service with the default configuration.
;
;@lisp
;(service munge-service-type)
;@end lisp
;
;@deftp {Data Type} munge-configuration
;Data type representing the configuration for the @code{munge-service-type}.
;
;@table @asis
;@item @code{package}
;Munge package to use for the service.
;
;@item @code{socket} (default "/var/run/munge/munge.socket.2")
;The socket Munge should use.
;
;@item @code{pid-file} (default "/var/run/munge/munged.pid")
;The PID file which Munge should use.
;
;@item @code{log-file} (default "/var/log/munge/munged.log")
;The location of the log file Munge should write to.
;
;@item @code{key} (default "/etc/munge/munge.key")
;The location of the shared key Munge should use.  Since this a shared secret key between the different nodes it should not be added to the store.
;
;@end table
;@end deftp


;; TODO: Make id/uid configurable
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

(define-record-type* <slurm-configuration>
  slurm-configuration
  make-slurm-configuration
  slurm-configuration?
  ;; As I understand it, all the services depend on also running slurmd on
  ;; that machine.  Therefore it makes sense to have one config section with
  ;; "common" and "extended" options.  With all the possible options and
  ;; versions we only cover the ones which affect the services.
  (package              slurm-configuration-package
                        (default slurm))
  (slurm-conf-file      slurm-configuration-slurm-conf-file
                        (default "/etc/slurm/slurm.conf"))
  (slurmd-log-file      slurm-configuration-slurmd-log-file
                        (default "/var/log/slurm/slurmd.log"))
  (slurmd-pid-file      slurm-configuration-slurmd-pid-file
                        (default "/var/run/slurm/slurmd.pid"))

  (slurmd-spooldir      slurm-configuration-slurmd-spooldir
                        (default "/var/spool/slurmd"))

  (run-slurmctld?       slurm-configuration-run-slurmctld
                        (default #f))
  (slurmctld-log-file   slurm-configuration-slurmctld-log-file
                        (default "/var/log/slurm/slurmctld.log"))
  (slurmctld-pid-file   slurm-configuration-slurmctld-pid-file
                        (default "/var/run/slurm/slurmctld.pid"))

  (run-slurmdbd?        slurm-configuration-run-slurmdbd
                        (default #f))
  (slurmdbd-conf-file   slurm-configuration-slurmdbd-conf-file
                        (default "/etc/slurm/slurmdbd.conf"))
  (slurmdbd-pid-file    slurm-configuration-slurmdbd-pid-file
                        (default "/var/run/slurm/slurmdbd.pid")))


(define (slurm-activation config)
  "Return the activation GEXP for CONFIG for the slurm service."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define %user (getpw "slurm"))
        (let ((homedir     (passwd:dir %user))
              (spooldir    #$(slurm-configuration-slurmd-spooldir config))
              (logdir      (dirname #$(slurm-configuration-slurmd-log-file config)))
              (piddir      (dirname #$(slurm-configuration-slurmd-pid-file config))))
          (for-each (lambda (dir)
                      (unless (file-exists? dir)
                        (mkdir-p dir))
                      (chown dir (passwd:uid %user) (passwd:gid %user)))
                    (list homedir spooldir piddir logdir)))
        ;; /etc/slurm/slurm.conf needs to exist.
        (file-exists? #$(slurm-configuration-slurm-conf-file config)))))

(define slurmd-shepherd-service
  (match-lambda
    (($ <slurm-configuration> package slurm-conf-file slurmd-log-file slurmd-pid-file)
     (list
       (shepherd-service
         (documentation "Slurmd server")
         (provision '(slurmd))
         (requirement '(loopback munge))
         (start #~(make-forkexec-constructor
                    (list #$(file-append package "/sbin/slurmd")
                          "-L" #$slurmd-log-file
                          "-f" #$slurm-conf-file)
                    #:pid-file #$slurmd-pid-file))
         (stop #~(make-kill-destructor)))))))

(define slurmctld-shepherd-service
  (match-lambda
    (($ <slurm-configuration> package slurm-conf-file _ _ _ run-slurmctld? slurmctld-log-file slurmctld-pid-file)
     (list
       (shepherd-service
         (documentation "Slurmctld server")
         (provision '(slurmctld))
         (requirement '(loopback munge))
         (start #~(make-forkexec-constructor
                    (list #$(file-append package "/sbin/slurmctld")
                          "-L" #$slurmctld-log-file
                          "-f" #$slurm-conf-file)
                    #:pid-file #$slurmctld-pid-file))
         (stop #~(make-kill-destructor))
         (auto-start? run-slurmctld?))))))

(define (slurmdbd-activation config)
  "Test the Slurmdbd configration exists."
  (file-exists?
    (slurm-configuration-slurmdbd-conf-file config)))

(define slurmdbd-shepherd-service
  (match-lambda
    (($ <slurm-configuration> package _ _ _ _ _ _ _ run-slurmdbd? slurmdbd-conf-file slurmdbd-pid-file)
     (list
       (shepherd-service
         (documentation "Slurmdbd server")
         (provision '(slurmdbd))
         (requirement '(loopback munge))
         (start #~(make-forkexec-constructor
                    (list #$(file-append package "/sbin/slurmdbd"))
                    #:pid-file #$slurmdbd-pid-file))
         (stop #~(make-kill-destructor))
         (auto-start? run-slurmdbd?))))))

(define (slurm-services-to-run config)
  (append (slurmd-shepherd-service config)
          (if (slurm-configuration-run-slurmctld? config)
            (slurmctld-shepherd-service config)
            '())
          (if (slurm-configuration-run-slurmdbd? config)
            (slurmdbd-shepherd-service config)
            '())))

(define (slurm-activations-to-run config)
  (append (slurm-activation config)
          (if (slurm-configuration-run-slurmctld? config)
            (slurmctld-activation config)
            '())
          (if (slurm-configuration-run-slurmdbd? config)
            (slurmdbd-activation config)
            '())))

(define slurmd-service-type
  (service-type
    (name 'slurmd)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           ;(cons slurmd-shepherd-service
                           ;      slurmdbd-shepherd-service))
                           slurmd-shepherd-service)
                           ;slurm-services-to-run)
        (service-extension activation-service-type
                           ;(append slurm-activation
                           ;      slurmdbd-activation))
                           slurm-activation)
                           ;slurm-activations-to-run)
        (service-extension account-service-type
                           (const %slurm-accounts))
        (service-extension profile-service-type
                           (compose list slurm-configuration-package))))
    (default-value (slurm-configuration))
    (description
     "Run @url{https://slurm.schedmd.com/slurm.html,Slurm}, a workflow manager service.")))

(define slurmdbd-service-type
  (service-type
    (name 'slurmdbd)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           slurmdbd-shepherd-service)
        (service-extension activation-service-type
                           slurmdbd-activation)))
    (default-value (slurm-configuration))
    (description
      ;; TODO: Fix for slurmdbd or integrate with slurm(d).
     "Run @url{https://slurm.schedmd.com/slurm.html,Slurm}, a workflow manager service.")))

(define slurmctld-service-type
  (service-type
    (name 'slurmctld)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           slurmctld-shepherd-service)))
    (default-value (slurm-configuration))
    (description
      ;; TODO: Fix for slurmctld or integrate with slurm(d).
     "Run @url{https://slurm.schedmd.com/slurm.html,Slurm}, a workflow manager service.")))
