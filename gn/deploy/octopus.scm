(define-module (gn deploy octopus))

(use-modules (gnu)
             (gn services science)
             (gn packages parallel))
(use-service-modules networking ssh web)
(use-package-modules certs tmux screen vim)

(define %slurm.conf
  (plain-file "slurm.conf"
              (string-append
"ClusterName=linux
ControlMachine=octopus

SlurmUser=slurm
#SlurmdUser=root
SlurmctldPort=6817
SlurmdPort=6818
AuthType=auth/munge
StateSaveLocation=/var/spool/slurmd/ctld
SlurmdSpoolDir=/var/spool/slurmd
SwitchType=switch/none
MpiDefault=none
SlurmctldPidFile=/var/run/slurm/slurmctld.pid
SlurmdPidFile=/var/run/slurm/slurmd.pid
ProctrackType=proctrack/pgid
ReturnToService=1

# TIMERS
InactiveLimit=0
MinJobAge=300
KillWait=30
#

# LOGGING
SlurmctldDebug=3
SlurmctldLogFile=/var/log/slurm/slurmctld.log
SlurmdDebug=3
SlurmdLogFile=/var/log/slurm/slurmd.log
JobCompType=jobcomp/none

# COMPUTE NODES
NodeName=octopus CPUs=1 Boards=1 SocketsPerBoard=1 CoresPerSocket=1 ThreadsPerCore=1 RealMemory=1024
PartitionName=debug Nodes=ALL Default=YES MaxTime=INFINITE State=UP")))

(define %cgroup.conf
  (plain-file "cgroup.conf"
              (string-append
"###
# Slurm cgroup support configuration file
###
CgroupAutomount=yes
ConstrainCores=yes
#")))

(define %slurmdbd.conf
  (plain-file "slurmdbd.conf"
              (string-append
"AuthType=auth/munge
AuthInfo=/var/run/munge/munge.socket.2
DbdHost=localhost
StorageHost=localhost
StorageType=accounting_storage/none
StorageUser=slurm
PidFile=/var/run/slurm/slurmdbd.pid
LogFile=/var/log/slurm/slurmdbd.log
SlurmUser=slurm")))

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
              ;  (type "nfs")
              ;  (mount? #f)    ; is this necessary?
              ;  (check? #f))
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
             (supplementary-groups '("wheel" "kvm")))
           (user-account
             (name "wrk")
             (comment "Pjotr Prins")
             (uid 502)
             (group "users")
             (supplementary-groups '("wheel" "kvm")))
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

              (service openssh-service-type
                       (openssh-configuration
                         (authorized-keys
                           `(("efraimf" ,(local-file "/home/efraimf/.ssh/id_rsa.pub"))))))

              (service munge-service-type)
              (service slurmd-service-type
                       (slurm-configuration
                         (package slurm-18.08)))
              (service slurmdbd-service-type
                       (slurm-configuration
                         (package slurm-18.08)
                         (run-slurmdbd? #t)))
              (service slurmctld-service-type
                       (slurm-configuration
                         (package slurm-18.08)
                         (run-slurmctld? #t)))

              ;; Some slurm configuration files
              (simple-service 'slurm-conf etc-service-type
                              `(("slurm/slurm.conf" ,%slurm.conf)
                                ("slurm/cgroup.conf" ,%cgroup.conf)
                                ("slurm/slurmdbd.conf" ,%slurmdbd.conf)))

              (service dhcp-client-service-type)
              (service openntpd-service-type))
            %base-services)))
