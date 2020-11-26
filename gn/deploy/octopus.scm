(define-module (gn deploy octopus))

(use-modules (gnu)
             (gn services science)
             )
(use-service-modules ssh web)
(use-package-modules certs tmux screen vim)

(operating-system
  (host-name "octopus")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sda")
               (terminal-outputs '(console serial))))
  (kernel-arguments '("console=tty1" "console=ttyS0,115200n8"))

  (file-systems
    (append (list
              (file-system
                (device "/dev/sda3")
                (mount-point "/")
                (type "ext4")
                (options "errors=remount-ro"))
              ;(file-system
              ;  (device "/dev/sdb1")
              ;  (mount-point "/mnt/sdb1")
              ;  (type "xfs")
              ;  (flags '(no-exec no-dev no-atime))
              ;  (options "rw,nodiratime,largeio,inode64")
              ;  (create-mount-point? #t))
              ;(file-system
              ;  (device "octopus01:/home")
              ;  (mount-point "/home")
              ;  (type "nfs"))
              )
              %base-file-systems))

  (swap-devices '("/dev/sda2"))
  ;; No firmware needed
  (firmware '())

  (users (cons*
           (user-account
             (name "efraimf")
             (comment "Efraim Flashner")
             (uid 1000)
             (group "users")
             (supplementary-groups '("wheel")))
           (user-account
             (name "wrk")
             (comment "Pjotr Prins")
             (uid 502)
             (group "users")
             (supplementary-groups '("wheel")))
           %base-user-accounts))


  (packages (cons*
              nss-certs
              screen
              tmux
              vim
              %base-packages))

  (services
    (append (list
              ;; This conflicts with everything when testing in a VM.
              ;(agetty-service
              ;  (agetty-configuration
              ;    (extra-options '("-L"))
              ;    (baud-rate "115200")
              ;    (term "vt100")
              ;    (tty "ttyS0")))

              (service openssh-service-type)

              (service munge-service-type)
              )
            %base-services)))
