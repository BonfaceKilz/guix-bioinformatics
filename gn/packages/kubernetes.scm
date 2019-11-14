(define-module (gn packages kubernetes)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages rsync))

(define-public kubernetes
  (package
    (name "kubernetes")
    (version "1.16.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kubernetes/kubernetes.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v4dhlpvi8gkc26zaxdfypng8b1f2lwm6hjz2amvq8mh49j5x7ld"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "k8s.io/kubernetes"
       #:install-source? #f
       #:tests? #f ; Skip tests for now.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'build 'prepare-build
           (lambda _
             (with-directory-excursion "src/k8s.io/kubernetes"
               (substitute* '("build/root/Makefile"
                              "build/root/Makefile.generated_files"
                              "build/pause/Makefile")
                 (("/bin/bash") (which "bash"))))
             #t))
         (replace 'build
           (lambda _
             (with-directory-excursion "src/k8s.io/kubernetes"
               ;; Cannot find go-bindata otherwise.
               (setenv "PATH" (string-append (getcwd) "/_output/bin:"
                                             (getenv "PATH")))
               (invoke "make"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "src/k8s.io/kubernetes"
                 ;; This saves more than 350MiB.
                 (delete-file "_output/local/go/bin/e2e.test")
                 (delete-file "_output/local/go/bin/e2e_node.test")
                 (for-each
                   (lambda (file)
                     (install-file file (string-append out "/bin")))
                   (find-files "_output/local/go/bin" ".*"))
                 ;(mkdir-p (string-append out "/share/bash-completion/completions"))
                 ;(call-with-output-file (string-append out "/share/bash-completion/completions/kubectl")
                 ;  (lambda (port)
                 ;    (format port (invoke "_output/local/go/bin/kubectl" "completion" "bash"))))
                 ;(mkdir-p (string-append out "/share/zsh/site-function"))
                 ;(call-with-output-file (string-append out "/share/zsh/site-functions/_kubectl")
                 ;  (lambda (port)
                 ;    (format port (invoke "_output/local/go/bin/kubectl" "completion" "zsh"))))
                 (install-file "LICENSE"
                               (string-append out "/share/doc/"
                                              ,name "-" ,version)))
               #t)))
         (add-after 'install 'install-man-pages
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/man/man1"))
               (with-directory-excursion "src/k8s.io/kubernetes"
                 (for-each
                   (lambda (file)
                     (invoke "_output/local/go/bin/genman"
                             (string-append out "/share/man/man1") file))
                   '("kube-apiserver" "kube-controller-manager" "kube-proxy"
                     "kube-scheduler" "kubelet" "kubectl")))
               #t))))))
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("rsync" ,rsync)))
    (propagated-inputs
     `(("crictl" ,crictl))) ; Must be the same major+minor version as kubernetes.
    (home-page "https://kubernetes.io/")
    (synopsis "Production-Grade Container Scheduling and Management")
    (description "Kubernetes is an open source system for managing containerized
applications across multiple hosts.  It provides basic mechanisms for
deployment, maintenance, and scaling of applications.")
    (license license:asl2.0)))

(define-public kubernetes-1.15
  (package
    (inherit kubernetes)
    (name "kubernetes")
    (version "1.15.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kubernetes/kubernetes.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "055xizqpg5yjda7b6l1vnajmbfz2ljh2z85r1d63683rqyw2y078"))))
    (propagated-inputs
     `(("crictl" ,crictl-1.15)))))

(define-public minikube
  (package
    (name "minikube")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/kubernetes/minikube/"
                                  "releases/download/v"
                                  version "/" name "-linux-amd64"))
              (sha256
               (base32
                "1sgpb5k3i6g1slz9f6lvp4br5llgm2wcklpn2804hpp8dnlsjwhr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out     (assoc-ref %outputs "out"))
                (bin     (string-append out "/bin"))
                (target  (string-append bin "/minikube"))
                (source  (assoc-ref %build-inputs "source")))
           (mkdir-p bin)
           (copy-file source target)
           (chmod target #o555))
         #t)))
    (home-page "https://minikube.sigs.k8s.io/")
    (synopsis "Run Kubernetes locally")
    (description "Minikube implements a local Kubernetes cluster.  Minikube's
primary goals are to be the best tool for local Kubernetes application
development and to support all Kubernetes features that fit.")
    (supported-systems '("x86_64-linux"))
    (license license:asl2.0)))

(define-public crictl
  (package
    (name "crictl")
    (version "1.16.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kubernetes-sigs/cri-tools.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qsbqml7jlhf73hlpqrh10bidz4v219bi2m5xg7914dzi0mzm733"))))
    (build-system go-build-system)
    (arguments
     `(;#:import-path "github.com/kubernetes-sigs/cri-tools"
       #:install-source? #f
       #:tests? #f ; tests require 'framwork' from kubernetes
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "src") #t))
         (add-before 'build 'prepare-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("/usr/local") out)
                 (("^VERSION .*") (string-append "VERSION := " ,version "\n")))
               #t)))
         (replace 'build
           (lambda _
             (invoke "make")))
         ;(replace 'check
         ;  (lambda _
         ;    (invoke "make" "test-e2e")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "BINDIR" (string-append out "/bin"))
               (invoke "make" "install")))))))
    (home-page "https://github.com/kubernetes-sigs/cri-tools")
    (synopsis "CLI and validation tools for Kubelet Container Runtime Interface")
    (description "Cri-tools aims to provide a series of debugging and validation
tools for Kubelet CRI.")
    (license license:asl2.0)))

(define-public crictl-1.15
  (package
    (inherit crictl)
    (name "crictl")
    (version "1.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kubernetes-sigs/cri-tools.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03fhddncwqrdyxz43m3bak9dlrsqzibqqja3p94nic4ydk2hry62"))))
    (arguments
     (substitute-keyword-arguments (package-arguments crictl)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build
             (lambda _
               (invoke "make" "windows"))) ; This is the correct invocation
           (add-before 'prepare-source 'update-version
             (lambda _
               (substitute* "Makefile"
                 (("^VERSION .*") (string-append "VERSION := " ,version "\n")))
               #t))
           (replace 'prepare-source
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "Makefile"
                   (("/usr/local") out)
                   ;; So we can use 'make windows'.
                   ((".exe") "")
                   (("GOOS=windows") "CGO_ENABLED=0")
                   (("_output") "../bin"))
                 #t)))))))))