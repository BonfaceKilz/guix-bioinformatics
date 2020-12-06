(define-module (gn deploy octopus))

(use-modules (gnu)
             (gn services science)
             (srfi srfi-26))
(use-service-modules networking ssh web)
(use-package-modules parallel shells)

(define %efraimf-ssh-pubkey
  (plain-file "id_rsa.pub"
              "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDUCDY8ZKFF/ln0yzDt3CNmKz3cT4wzNv9bzCKvOBXcL0O7JtPWwqgLlZgmMHfzhzgReAkHcrt+Gdsyduzm/s9Y8c6QpyfaH6uoDwjfoOs6GrAjZaOXmAdncf+9HZEAy/IrygQ1YFRu6BvYogsdhhtN+O6IXBuvQQDRzldHs53Y53DK06Nrs19vAPwELXcDxcx1FvO+/L9nT8RHkI1Z0ucgTS+F/BWXl8+mh89r4j+4IRpZXOuCD0DrW5rgEE1EygF2dVdWZQESi23gU5Mt6vnmysXzwixB7j6I+xTih8LH4pz7hewEx6754e/cs9Gm7ZtfXKfXUt6+GtsBSBF3ULKl efraimf@octopus01"))

(define %slurm.conf
  (plain-file "slurm.conf"
              (string-append
"# Defaults are commented out, otherwise noted at the end of the line
# Values are from example in the man page or from Debian
ClusterName=linux       # no default, suggests lowercase
#ControlMachine=octopus  # defunct, use SlurmctldHost
SlurmctldHost=octopus   # no default, falls back to next SlurmctldHost in list

SlurmUser=slurm         # default root, not recommended
#SlurmctldPort=6817
#SlurmdPort=6818
#AuthType=auth/munge
StateSaveLocation=/var/spool/slurmd/ctld    # default /var/spool
#SlurmdSpoolDir=/var/spool/slurmd
#SwitchType=switch/none
#MpiDefault=none
#SlurmctldPidFile=/var/run/slurmctld.pid
#SlurmdPidFile=/var/run/slurmd.pid
ProctrackType=proctrack/pgid    # default proctrack/cgroup
ReturnToService=1       # default 0

DebugFlags=NO_CONF_HASH # default empty
# TIMERS
SlurmctldTimeout=300    # default 120
#SlurmdTimeout=300
#InactiveLimit=0
#MinJobAge=300
#KillWait=30
#WaitTime=0
#

# LOGGING
#SlurmctldDebug=3
SlurmctldLogFile=/var/log/slurmctld.log     # default none, syslog
#SlurmdDebug=3
SlurmdLogFile=/var/log/slurmd.log           # default none, syslog
#JobCompType=jobcomp/none

# COMPUTE NODES
NodeName=octopus CPUs=1 Boards=1 SocketsPerBoard=1 CoresPerSocket=1 ThreadsPerCore=1 RealMemory=1024
PartitionName=debug Nodes=ALL Default=YES MaxTime=INFINITE State=UP")))

(define %cgroup.conf
  (plain-file "cgroup.conf"
              (string-append
"###
# Slurm cgroup support configuration file
###
CgroupAutomount=yes     # default no
ConstrainCores=yes      # default no
#")))

(define %slurmdbd.conf
  (plain-file "slurmdbd.conf"
              (string-append
"#AuthType=auth/munge
#AuthInfo=/var/run/munge/munge.socket.2
DbdHost=localhost       # must be specified
StorageHost=localhost   # unclear, must be specified?
StorageType=accounting_storage/none     # must be specified
StorageUser=slurm       # unclear
#PidFile=/var/run/slurmdbd.pid
LogFile=/var/log/slurmdbd.log       # default none, syslog
SlurmUser=slurm         # default root, not recommended")))

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
                           `(("efraimf" ,%efraimf-ssh-pubkey)))))

              (service munge-service-type)
              (service slurm-service-type
                       (slurm-configuration
                         (package slurm-18.08)
                         (slurmd-log-file "/var/log/slurmd.log")
                         (slurmctld-log-file "/var/log/slurmctld.log")
                         (run-slurmdbd? #t)
                         (run-slurmctld? #t)))

              ;; Some slurm configuration files
              (simple-service 'slurm-conf etc-service-type
                              `(("slurm/slurm.conf" ,%slurm.conf)
                                ("slurm/cgroup.conf" ,%cgroup.conf)
                                ("slurm/slurmdbd.conf" ,%slurmdbd.conf)))

              (service dhcp-client-service-type)
              (service openntpd-service-type))
            %base-services)))
