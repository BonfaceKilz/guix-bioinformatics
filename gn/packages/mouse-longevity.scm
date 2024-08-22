;;; guix-bioinformatics --- Bioinformatics packages for GNU Guix
;;; Copyright © 2024 jgart <jgart@dismail.de>
;;;
;;; This file is part of guix-bioinformatics.
;;;
;;; genenetwork-machines is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; genenetwork-machines is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with genenetwork-machines.  If not, see
;;; <https://www.gnu.org/licenses/>.

(define-module (gn packages mouse-longevity)
  #:use-module (guix)
  #:use-module (gnu)
  #:use-module (gnu services shepherd)
  #:use-module (gn services rshiny)
  #:use-module (guix git-download)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages statistics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module ((gnu packages admin) #:select (shepherd))
  #:use-module ((gnu packages web) #:select (nginx)))

(define-public mouse-longevity-app
  (let ((commit "8a7fdd353e1babcdc3c0453bdfba2aa0dead7c3f")
        (revision "2"))
    (package
      (name "mouse-longevity-app")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.genenetwork.org/mouse-longevity-app/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1bmiq9xmsq8wgxv3nqg3r2j1kwy2m6pw8agapj31agbyw81agyna"))))
      (build-system trivial-build-system)
      (arguments
       (list #:modules '((guix build utils))
             #:builder
             #~(begin
                 (use-modules (guix build utils))
                 (let* ((source    (assoc-ref %build-inputs "source"))
                        (targetdir (string-append #$output "/share/" #$name))
                        (app       (string-append #$output "/bin/" #$name))
                        (Rbin      (search-input-file %build-inputs "/bin/Rscript")))
                   (copy-recursively source targetdir)
                   (mkdir-p (string-append #$output "/bin"))
                   (call-with-output-file app
                     (lambda (port)
                       (format port
                               "#!~a
library(shiny)
setwd(\"~a\")
runApp(launch.browser=0, port=3979)~%\n"
                               Rbin targetdir)))
                   (chmod app #o555)))))
      (propagated-inputs
       (list r
             r-ggplot2
             r-dplyr
             r-plotly
             r-shinydashboard
             r-shinydashboardplus
             r-shinyjs
             r-shiny))
      (home-page "https://github.com/Dashbrook/Mouse_Longevity_app/")
      (synopsis "R shiny app to visualize mouse lifespan data")
      (description
       "This package provides an R shiny app to visualize mouse strain
longevity data.")
      (license license:gpl3+))))
