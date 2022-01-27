(define-module (gn packages emacs-xyz)
  #:use-module (guix build-system emacs)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build emacs-utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gnome)
  #:use-module (ice-9 match))

(define-public emacs-trident-mode
  (let ((commit "109a1bc10bd0c4b47679a6ca5c4cd27c7c8d4ccb")
        (revision "0"))
    (package
      (name "emacs-trident-mode")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/johnmastro/trident-mode.el")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0blbxjaddzhkvzlys46yrzszywmyjkmfhwks52v4laya76rgxm45"))))
      (build-system emacs-build-system)
      (propagated-inputs
        (list emacs-skewer-mode emacs-dash emacs-slime))
      (home-page "https://github.com/johnmastro/trident-mode.el")
      (synopsis "Emacs minor mode for live Parenscript interaction")
      (description
"@code{emacs-trident-mode} is an @code{emacs} minor mode and collection
of commands for working with Parenscript code in SLIME and sending it to
the browser via Skewer.  The goal is to create an environment for hacking
Parenscript which fits as naturally as possible into the Lisp style of
interactive development.")
      (license license:unlicense))))

