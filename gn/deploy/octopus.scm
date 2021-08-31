(define-module (gn deploy octopus))

(use-modules (gnu)
             (gn services file-systems)
             (gn services science)
             (srfi srfi-26))
(use-service-modules networking ssh sysctl web)
(use-package-modules parallel shells)

(define %efraimf-ssh-pubkey
  (plain-file "efraim-id_rsa.pub"
              "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDUCDY8ZKFF/ln0yzDt3CNmKz3cT4wzNv9bzCKvOBXcL0O7JtPWwqgLlZgmMHfzhzgReAkHcrt+Gdsyduzm/s9Y8c6QpyfaH6uoDwjfoOs6GrAjZaOXmAdncf+9HZEAy/IrygQ1YFRu6BvYogsdhhtN+O6IXBuvQQDRzldHs53Y53DK06Nrs19vAPwELXcDxcx1FvO+/L9nT8RHkI1Z0ucgTS+F/BWXl8+mh89r4j+4IRpZXOuCD0DrW5rgEE1EygF2dVdWZQESi23gU5Mt6vnmysXzwixB7j6I+xTih8LH4pz7hewEx6754e/cs9Gm7ZtfXKfXUt6+GtsBSBF3ULKl efraimf@octopus01"))


(operating-system
  (host-name "octopus")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sda"))
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
              (file-system
                (device "octopus01:/export")
                (mount-point "/export")
                (type "nfs")
                (mount? #f)    ; VM can't find octopus01
                (check? #f))(file-system
                (device "octopus01:/home")
                (mount-point "/home")
                (type "nfs")
                (mount? #f)    ; VM can't find octopus01
                (check? #f)))
              %base-file-systems))

  (swap-devices '("/dev/sda2"))
  ;; No firmware needed
  (firmware '())

  (users (cons*
           (user-account
             (name "wrk")
             (comment "Pjotr Prins")
             (uid 502)
             (group "users")
             (supplementary-groups '("wheel" "kvm")))
           (user-account
             (name "efraimf")
             (comment "Efraim Flashner")
             (uid 1000)
             (group "users")
             (supplementary-groups '("wheel" "kvm")))
           (user-account
             (name "erikg")
             (comment "Erik Garrison")
             (uid 1001)
             (group "users")
             (shell (file-append zsh "/bin/zsh")))
           (user-account
             (name "hchen")
             (comment "Hao Chen")
             (uid 1002)
             (group "users"))
           %base-user-accounts))


  (packages (append
              (map (cut specification->package <>)
                     '("nss-certs"
                       "screen" "tmux"
                       "vim"
                       "htop"))
              %base-packages))

  (services
    (append (list
              (service openssh-service-type
                       (openssh-configuration
                         (authorized-keys
                           `(("efraimf" ,%efraimf-ssh-pubkey)))))

              (service sysctl-service-type
                       (sysctl-configuration
                         (settings '(("vm.overcommit_memory" . "2")
                                     ("vm.overcommit_ratio" . "90")))))

              (service munge-service-type)
              (service slurm-service-type
                       (slurm-configuration
                         (package slurm-18.08)
                         (SlurmdLogFile "/var/log/slurmd.log")
                         (SlurmctldLogFile "/var/log/slurmctld.log")
                         (ClusterName "linux")
                         (SlurmUser "slurm")
                         (SlurmctldHost '("octopus"))
                         (DbdHost "localhost")
                         (StorageType "accounting_storage/none")
                         (slurm-extra-content
                           (string-append
                             "StateSaveLocation=/var/spool/slurmd/ctld    # default /var/spool\n"
                             "ReturnToService=1               # default 0\n"
                             "DebugFlags=NO_CONF_HASH         # default empty\n"
                             "SelectType=select/cons_res      # default select/linear\n"
                             "SelectTypeParameters=CR_CPU     # default 0\n"
                             "# COMPUTE NODES\n"
                             "NodeName=octopus CPUs=1 Boards=1 SocketsPerBoard=1 CoresPerSocket=1 ThreadsPerCore=1 RealMemory=1024\n"
                             "PartitionName=debug Nodes=ALL Default=YES MaxTime=INFINITE State=UP"))
                         (cgroup-extra-content
                           (string-append
                             "CgroupAutomount=yes     # default no\n"
                             "ConstrainCores=yes      # default no\n"
                             "MaxRAMPercent=95        # default 100"))
                         (slurmdbd-extra-content
                           (string-append
                             "LogFile=/var/log/slurmdbd.log   # default none, syslog"))
                         (run-slurmdbd? #t)
                         (run-slurmctld? #t)))

              (service lizardfs-service-type
                       (lizardfs-configuration
                         (mfsmetalogger-config
                           (lizardfs-mfsmetalogger-config-file
                             (master-host "octopus")))
                         (mfschunkserver-config
                           (lizardfs-mfschunkserver-config-file
                             (master-host "octopus")))
                         (mfshdd-config
                           (lizardfs-mfshdd-config-file
                             (disks-to-use (list "/mnt"))))
                         (mfsmaster-config
                           (lizardfs-mfsmaster-config-file
                             (personality "master")
                             (master-host "octopus")))
                         (mfsexports-config
                           (plain-file "mfsexports.cfg"
                                       "* / rw\n"))
                         (run-mfsmetalogger-service? #t)
                         ))

              (service dhcp-client-service-type)
              (service openntpd-service-type))
            %base-services))

  (name-service-switch %mdns-host-lookup-nss))
