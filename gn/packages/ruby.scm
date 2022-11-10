;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gn packages ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages rails)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby))


(define-public apache-maven
  (package
    (name "apache-maven")
    (version "3.3.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp.tudelft.nl/apache/maven/maven-3/3.3.9/source/apache-maven-" version "-src.tar.gz"))
              (sha256               (base32
                "1g0iavyb34kvs3jfrx2hfnr8lr11m39sj852cy7528wva1glfl4i"))))
    (build-system gnu-build-system)
    (home-page "http://ant.apache.org")
    (synopsis "Build tool for Java")
    (description
     "Ant is a platform-independent build tool for Java.  It is similar to
make but is implemented using the Java language, requires the Java platform,
and is best suited to building Java projects.  Ant uses XML to describe the
build process and its dependencies, whereas Make uses Makefile format.")
    (license license:asl2.0)))


(define-public jruby
  (package
    (name "jruby")
    (version "9.0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://s3.amazonaws.com/jruby.org/downloads/9.0.5.0/jruby-bin-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1wysymqzc7591743f2ycgwpm232y6i050izn72lck44nhnyr5wwy"))
        ))
    (build-system gnu-build-system)
    (native-inputs
     `(("ant" ,ant)
       ;; ("maven" ,maven)
       ("jdk" ,icedtea "jdk")))
    (inputs
     `(("readline" ,readline)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (native-search-paths
     (list (search-path-specification
            (variable "GEM_PATH")
            (files (list (string-append "lib/ruby/gems/"
                                        (version-major+minor version)
                                        ".0"))))))
    (synopsis "Programming language interpreter")
    (description "Ruby is a dynamic object-oriented programming language with
a focus on simplicity and productivity.")
    (home-page "https://ruby-lang.org")
    (license license:ruby)))

(define-public bio-table ; guix ready with tests
  (package
   (name "bio-table")
   (version "1.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "bio-table" version))
     (sha256
      (base32
       "1jlpls734kd41rffn2y2747nr14k5rwgaj2g3k48i9xgsfcmrn6r"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
   ))
   (propagated-inputs
    `(("ruby-bio-logger" ,ruby-bio-logger)))
   (synopsis
    "Functions and tools for tranforming and changing tab delimited
and comma separated table files - useful for Excel sheets and SQL/RDF
output")
   (description
    "Functions and tools for tranforming and changing tab delimited
and comma separated table files - useful for Excel sheets and SQL/RDF
output")
   (home-page
    "http://github.com/pjotrp/bioruby-table")
   (license license:expat)))

(define-public ruby-ntlm-http
  (package
    (name "ruby-ntlm-http")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ntlm-http" version))
       (sha256
        (base32
         "0yx01ffrw87wya1syivqzf8hz02axk7jdpw6aw221xwvib767d36"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis
     "Ruby/NTLM HTTP provides NTLM authentication over http.")
    (description
     "Ruby/NTLM HTTP provides NTLM authentication over http.")
    (home-page "http://www.mindflowsolutions.net")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-webrobots
  (package
    (name "ruby-webrobots")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "webrobots" version))
       (sha256
        (base32
         "19ndcbba8s8m62hhxxfwn83nax34rg2k5x066awa23wknhnamg7b"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis
     "This library helps write robots.txt compliant web robots in Ruby.
")
    (description
     "This library helps write robots.txt compliant web robots in Ruby.
")
    (home-page "https://github.com/knu/webrobots")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-mechanize ; guix maybe ready
  (package
    (name "ruby-mechanize")
    (version "2.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mechanize" version))
       (sha256
        (base32
         "1f861x62kmggy60krv229s5jl7afq9nblwcfih3kp9bm5c5jn16y"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f)) ; one test fails
    (inputs
     `(("ruby" ,ruby)))
    (propagated-inputs
     `(("ruby-domain-name" ,ruby-domain-name)
       ("ruby-http-cookie" ,ruby-http-cookie)
       ("ruby-mime-types" ,ruby-mime-types)
       ("ruby-net-http-digest-auth"
        ,ruby-net-http-digest-auth)
       ("ruby-net-http-persistent"
        ,ruby-net-http-persistent)
       ("ruby-nokogiri" ,ruby-nokogiri)
       ("ruby-ntlm-http" ,ruby-ntlm-http)
       ("ruby-webrobots" ,ruby-webrobots)))
    (synopsis
     "The Mechanize library is used for automating interaction with websites.
Mechanize automatically stores and sends cookies, follows redirects,
and can follow links and submit forms.  Form fields can be populated and
submitted.  Mechanize also keeps track of the sites that you have visited as
a history.")
    (description
     "The Mechanize library is used for automating interaction with websites.
Mechanize automatically stores and sends cookies, follows redirects,
and can follow links and submit forms.  Form fields can be populated and
submitted.  Mechanize also keeps track of the sites that you have visited as
a history.")
  (home-page
   "http://docs.seattlerb.org/mechanize/")
  (license license:expat)))

(define-public ruby-elasticsearch-transport
(package
  (name "ruby-elasticsearch-transport")
  (version "6.0.2")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "elasticsearch-transport" version))
      (sha256
        (base32
          "0gpwbw70qisx681j1bw8xq6shg5kdxmcdzg6425af0b5881jg7iy"))))
  (build-system ruby-build-system)
  (propagated-inputs
    `(("ruby-faraday" ,ruby-faraday)
      ("ruby-multi-json" ,ruby-multi-json)))
  (arguments
   `(#:tests? #f)) ;; no bundler/cucumber
  (synopsis
    "Ruby client for Elasticsearch. See the `elasticsearch` gem for full integration.
")
  (description
    "Ruby client for Elasticsearch.  See the `elasticsearch` gem for full integration.
")
  (home-page
    "https://github.com/elasticsearch/elasticsearch-ruby/tree/master/elasticsearch-transport")
  (license (license:non-copyleft "will fill in later"))))

(define-public ruby-elasticsearch-api
(package
  (name "ruby-elasticsearch-api")
  (version "6.0.2")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "elasticsearch-api" version))
      (sha256
        (base32
          "1vkahknqn85vvwr1gzh8jf3pvdial0c0d524icg8x06vibqgzd5h"))))
  (build-system ruby-build-system)
  (propagated-inputs
    `(("ruby-multi-json" ,ruby-multi-json)))
  (arguments
   `(#:tests? #f)) ;; no bundler/cucumber
  (synopsis
    "Ruby API for Elasticsearch. See the `elasticsearch` gem for full integration.
")
  (description
    "Ruby API for Elasticsearch.  See the `elasticsearch` gem for full integration.
")
  (home-page
    "https://github.com/elasticsearch/elasticsearch-ruby/tree/master/elasticsearch-api")
  (license (license:non-copyleft "will fill in later"))))

(define-public ruby-elasticsearch
  (package
   (name "ruby-elasticsearch")
   (version "6.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "elasticsearch" version))
     (sha256
      (base32
       "0a08ynvxz5clfm2ndqpgjrv4aiga9m2y1ab34s3qkihdfdzdzhj8"))))
   (build-system ruby-build-system)
   (propagated-inputs
    `(("ruby-elasticsearch-api"
       ,ruby-elasticsearch-api)
      ("ruby-elasticsearch-transport"
       ,ruby-elasticsearch-transport)))
  (arguments
   `(#:tests? #f)) ;; no bundler/cucumber
   (synopsis
    "Ruby integrations for Elasticsearch (client, API, etc.)
")
   (description
    "Ruby integrations for Elasticsearch (client, API, etc.)
")
   (home-page
    "http://github.com/elasticsearch/elasticsearch-ruby")
   (license (license:non-copyleft "will fill in later"))))





;;;

(define-public discourse
  (package
    (name "discourse")
    (version "2.6.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/discourse/discourse")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "06ykn53m7mmdk71szk86nlq87rspqlb3fjpdmqi133z63dbj20ll"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'replace-git-ls-files)
         (add-before 'build 'delete-gemfile-lock
           (lambda _
             (delete-file "Gemfile.lock")
             #t))
         (add-after 'unpack 'adjust-version-dependencies
           (lambda _
             (substitute* "Gemfile"
               ;; Don't require specific versions of these gems
               (("6.0.3.3") ,(package-version ruby-rails))
               (("2.0.1") ,(package-version ruby-sassc))
               (("active_model_serializers.*") "active_model_serializers'\n")
               ;; Add tzinfo-data and figure out how to use non-Ruby version later
               (("active_model_serializers'")
                "active_model_serializers'\ngem 'tzinfo-data'")
               ;; ruby-cppjieba-rb never finishes the install phase with ruby-2.6
               ((".*cppjieba_rb.*") "")
               )
             #t))
         (replace 'build
           (lambda _
             ;; https://github.com/discourse/discourse/blob/v2.6.3/docs/DEVELOPER-ADVANCED.md
             (setenv "HOME" (getcwd))
             (setenv "RAILS_ENV" "test")

             ;; Launch Redis and Postgresql before continuing

             ;(invoke "bundle" "exec" "rake" "db:create" "db:migrate")
             ;(invoke "bundle" "exec" "rake" "autospec")
             ))
         ;(replace 'check
         ;  (lambda _
         ;    (invoke "rubocop")
         ;    (invoke "rubocop" "plugins")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vendor-dir (string-append out "/lib/ruby/vendor_ruby"))
                    )
               (copy-recursively (getcwd) out)
               (for-each make-file-writable (find-files out "\\.gz$"))
               ;(with-output-to-file (string-append out "/bundler")
               ;  (lambda _

               ;(setenv "GEM_VENDOR" vendor-dir)
               ;(setenv "BUNDLE_PATH" (getenv "GEM_PATH"))
               ;(invoke "bundle" "install"
               ;        "--path" out
               ;        "--verbose"
               ;        "--deployment"
               ;        "--local"
               ;        "--standalone"
               ;        )
               ;(invoke "gem" "install"
               ;        "--verbose"
               ;        ;"--vendor"
               ;        "--local"
               ;        "--bindir" (string-append out "/bin")
               )))
         )
       ))
    ;;TODO: What should be moved to native-inputs?
    (inputs
     `(
       ("node" ,(@ (gnu packages node) node))
       ("ruby-actionmailer" ,ruby-actionmailer)
       ("ruby-actionview-precompiler" ,ruby-actionview-precompiler)
       ("ruby-active-model-serializers" ,ruby-active-model-serializers)
       ("ruby-activemodel" ,ruby-activemodel)
       ("ruby-activerecord" ,ruby-activerecord)
       ("ruby-aws-sdk-s3" ,ruby-aws-sdk-s3)
       ("ruby-aws-sdk-sns" ,ruby-aws-sdk-sns)
       ("ruby-bootsnap" ,ruby-bootsnap)
       ("ruby-cbor" ,ruby-cbor)
       ;("ruby-cppjieba-rb" ,ruby-cppjieba-rb)
       ("ruby-colored2" ,ruby-colored2)
       ("ruby-cose" ,ruby-cose)
       ("ruby-css-parser" ,ruby-css-parser)
       ("ruby-diffy" ,ruby-diffy)
       ("ruby-discourse-ember-rails" ,ruby-discourse-ember-rails)
       ("ruby-discourse-ember-source" ,ruby-discourse-ember-source)
       ("ruby-discourse-fonts" ,ruby-discourse-fonts)
       ("ruby-discourse-image-optim" ,ruby-discourse-image-optim)
       ("ruby-email-reply-trimmer" ,ruby-email-reply-trimmer)
       ("ruby-excon" ,ruby-excon)
       ("ruby-fast-blank" ,ruby-fast-blank)
       ("ruby-fast-xs" ,ruby-fast-xs)
       ("ruby-fastimage" ,ruby-fastimage)
       ("ruby-flamegraph" ,ruby-flamegraph)
       ("ruby-gc-tracer" ,ruby-gc-tracer)
       ("ruby-highline" ,ruby-highline)
       ("ruby-http-accept-language" ,ruby-http-accept-language)
       ("ruby-lograge" ,ruby-lograge)
       ("ruby-logstash-event" ,ruby-logstash-event)
       ("ruby-logstash-logger" ,ruby-logstash-logger)
       ("ruby-logster" ,ruby-logster)
       ("ruby-lru-redux" ,ruby-lru-redux)
       ("ruby-lz4-ruby" ,ruby-lz4-ruby)
       ("ruby-maxminddb" ,ruby-maxminddb)
       ("ruby-memory-profiler" ,ruby-memory-profiler)
       ("ruby-message-bus" ,ruby-message-bus)
       ("ruby-mini-mime" ,ruby-mini-mime)
       ("ruby-mini-racer" ,ruby-mini-racer-0.2.4)
       ("ruby-mini-scheduler" ,ruby-mini-scheduler)
       ("ruby-mini-sql" ,ruby-mini-sql)
       ("ruby-mini-suffix" ,ruby-mini-suffix)
       ("ruby-oj" ,ruby-oj)
       ("ruby-omniauth" ,ruby-omniauth)
       ("ruby-omniauth-facebook" ,ruby-omniauth-facebook)
       ("ruby-omniauth-github" ,ruby-omniauth-github)
       ("ruby-omniauth-google-oauth2" ,ruby-omniauth-google-oauth2)
       ("ruby-omniauth-twitter" ,ruby-omniauth-twitter)
       ("ruby-onebox" ,ruby-onebox)
       ("ruby-pg" ,ruby-pg)
       ("ruby-pry-byebug" ,ruby-pry-byebug)
       ("ruby-pry-rails" ,ruby-pry-rails)
       ("ruby-puma" ,ruby-puma)
       ("ruby-r2" ,ruby-r2)
       ("ruby-rack-mini-profiler" ,ruby-rack-mini-profiler)
       ("ruby-rack-protection" ,ruby-rack-protection)
       ("ruby-rails-failover" ,ruby-rails-failover)
       ("ruby-rails-multisite" ,ruby-rails-multisite)
       ("ruby-railties" ,ruby-railties)
       ("ruby-rake" ,ruby-rake)
       ("ruby-rbtrace" ,ruby-rbtrace)
       ("ruby-rchardet" ,ruby-rchardet)
       ("ruby-redis" ,ruby-redis)
       ("ruby-redis-namespace" ,ruby-redis-namespace)
       ("ruby-rinku" ,ruby-rinku)
       ("ruby-rotp" ,ruby-rotp)
       ("ruby-rqrcode" ,ruby-rqrcode)
       ("ruby-rtlit" ,ruby-rtlit)
       ("ruby-ruby-readability" ,ruby-ruby-readability)
       ("ruby-rubyzip" ,ruby-rubyzip)
       ("ruby-sassc" ,ruby-sassc)
       ("ruby-sassc-rails" ,ruby-sassc-rails)
       ("ruby-seed-fu" ,ruby-seed-fu)
       ("ruby-shoulda-matchers" ,ruby-shoulda-matchers)
       ("ruby-sidekiq" ,ruby-sidekiq)
       ("ruby-sprockets-rails" ,ruby-sprockets-rails)
       ("ruby-sshkey" ,ruby-sshkey)
       ("ruby-stackprof" ,ruby-stackprof)
       ("ruby-uglifier" ,ruby-uglifier)
       ("ruby-unf" ,ruby-unf)
       ("ruby-unicorn" ,ruby-unicorn)
       ("ruby-webpush" ,ruby-webpush)
       ("ruby-xorcist" ,ruby-xorcist)
       ))
    (native-inputs
     `(
       ("ruby-annotate" ,ruby-annotate)
       ("ruby-better-errors" ,ruby-better-errors)
       ("ruby-binding-of-caller" ,ruby-binding-of-caller)
       ("ruby-bullet" ,ruby-bullet)
       ("ruby-certified" ,ruby-certified)
       ("ruby-fabrication" ,ruby-fabrication)
       ("ruby-fakeweb" ,ruby-fakeweb)
       ("ruby-listen" ,ruby-listen)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-mock-redis" ,ruby-mock-redis)
       ("ruby-parallel-tests" ,ruby-parallel-tests)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-html-matchers" ,ruby-rspec-html-matchers)
       ("ruby-rspec-rails" ,ruby-rspec-rails)
       ("ruby-rswag-specs" ,ruby-rswag-specs)
       ("ruby-rubocop-discourse" ,ruby-rubocop-discourse)
       ("ruby-ruby-prof" ,ruby-ruby-prof)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-test-prof" ,ruby-test-prof)
       ("ruby-webmock" ,ruby-webmock)
       ("ruby-yaml-lint" ,ruby-yaml-lint)
       ;("tzdata" ,(@ (gnu packages base) tzdata))
       ))
    (synopsis "Platform for community discussion")
    (description "Discourse is the 100% open source discussion platform built
for the next decade of the Internet.  Use it as a mailing list, discussion
forum, long-form chat room, and more!")
    (home-page "https://www.discourse.org/")
    (license license:gpl2)))

(define-public ruby-seed-fu
  (package
    (name "ruby-seed-fu")
    (version "2.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "seed-fu" version))
        (sha256
         (base32
          "0y7lzcshsq6i20qn1p8zczir4fivr6nbl1km91ns320vvh92v43d"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no tests
    (propagated-inputs
     `(("ruby-activerecord" ,ruby-activerecord)
       ("ruby-activesupport" ,ruby-activesupport)))
    (synopsis "Advanced seed data handling for Rails")
    (description
     "Seed Fu is an attempt to once and for all solve the problem of inserting
and maintaining seed data in a database.  It uses a variety of techniques
gathered from various places around the web and combines them to create what is
hopefully the most robust seed data system around.")
    (home-page "https://github.com/mbleigh/seed-fu")
    (license license:expat)))

;; TODO: deal with bundled libraries
(define-public ruby-mini-suffix
  (package
    (name "ruby-mini-suffix")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mini_suffix" version))
        (sha256
         (base32
          "1r6pwyv1vpyza0rn1pyxls4qdw5jd3vg4k5dp1iaqa57n6fiqrvi"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         ;(add-after 'unpack 'remove-vendored-libraries
         ;  (lambda _
         ;   (delete-file-recursively "vendor") #t))
         (add-after 'install 'replace-vendored-libraries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vendor (string-append out "/lib/ruby/vendor_ruby/gems"
                                           "/mini_suffix-" ,version "/vendor/")))
               (for-each delete-file (find-files vendor))
               (symlink (string-append (assoc-ref inputs "libpsl")
                                       "/lib/libpsl.so")
                        (string-append vendor "libpsl.x86_64.so"))
               #t)))
         )
       #:tests? #f))    ; cannot load such file -- spec_helper
    (propagated-inputs
     `(("ruby-ffi" ,ruby-ffi)))
    (inputs
     `(("libpsl" ,libpsl)))
    (native-inputs
     `(("ruby-rake" ,ruby-rake)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "FFI wrapper for libpsl")
    (description "This package provides an FFI wrapper for libpsl in Ruby.")
    (home-page "https://github.com/discourse/mini_suffix")
    (license license:expat)))

(define-public ruby-redis-namespace
  (package
    (name "ruby-redis-namespace")
    (version "1.8.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/resque/redis-namespace")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0dk8kgx6ii3xfv39x7m62bmahp29gv7pz8c8105mxqyxnx9pl525"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- rspec
    (propagated-inputs
     `(("ruby-redis" ,ruby-redis)))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-its" ,ruby-rspec-its)
       ))
    (synopsis "Namespace calls to Redis")
    (description
     "This package provides a @code{Redis::Namespace} class which can be used to
namespace calls to Redis.  This is useful when using a single instance of Redis
with multiple, different applications.")
    (home-page "https://github.com/resque/redis-namespace")
    (license license:expat)))

(define-public ruby-jsonapi-renderer
  (package
    (name "ruby-jsonapi-renderer")
    (version "0.2.2")
    (source
      (origin
        ;; No rakefile included in gem
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jsonapi-rb/jsonapi-renderer")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "11i4jbliidi7r04fg4g33hgn1yvfi1rjbsrc7m6gjj06zhflzn5s"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis "Render JSON API documents")
    (description
     "This package provides a ruby gem for rendering JSON API documents.")
    (home-page "https://github.com/jsonapi-rb/jsonapi-renderer")
    (license license:expat)))

(define-public ruby-case-transform
  (let ((commit "b957f9c933efa3894589af0ee7313660ebb552d8")
        (revision "1"))
    (package
      (name "ruby-case-transform")
      (version (git-version "0.2" revision commit))
      (source
        (origin
          ;; No rakefile included in gem
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/rails-api/case_transform")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0z6pn2c4ksnphy3rbjy2ml40lhl7dznxi19ny4nlk9m071cb0l93"))))
      (build-system ruby-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda _
               (delete-file "Gemfile.lock")
               #t)))))
      (propagated-inputs
       `(("ruby-activesupport" ,ruby-activesupport)))
      (native-inputs
       `(("ruby-awesome-print" ,ruby-awesome-print)
         ("ruby-codeclimate-test-reporter" ,ruby-codeclimate-test-reporter)
         ("ruby-pry-byebug" ,ruby-pry-byebug)
         ("ruby-rubocop" ,ruby-rubocop)))
      (synopsis "@code{key_transform} abilities of ActiveModelSerializers")
      (description
       "This package provides the extraction of the @code{key_transform} abilities
of @code{ActiveModelSerializers}.")
      (home-page "https://github.com/rails-api/case_transform")
      (license license:expat))))

(define-public ruby-active-model-serializers
  (package
    (name "ruby-active-model-serializers")
    (version "0.10.12")
    (source
      (origin
        ;; No rakefile included in gem
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rails-api/active_model_serializers")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1d2lywfzj4h117b67cwl76a6zl7q1vmgajzn51w5ifvdpc5rssli"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; TODO: enable
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-dependency-version-contstriants
           (lambda _
             (setenv "RAILS_VERSION"
                     ,(version-major+minor (package-version ruby-rails)))
             (substitute* "active_model_serializers.gemspec"
               (("grape.*") "grape'\n")
               (("kaminari.*") "kaminari'\n")
               (("minitest.*") "minitest'\n")
               ((".*json_schema.*") "") ; can't seem to find it during the 'check phase
               )
             (substitute* "Gemfile"
               (("rubocop.*") "rubocop'\n")
               )
             #t)))))
    (propagated-inputs
     `(("ruby-actionpack" ,ruby-actionpack)
       ("ruby-activemodel" ,ruby-activemodel)
       ("ruby-case-transform" ,ruby-case-transform)
       ("ruby-jsonapi-renderer" ,ruby-jsonapi-renderer)))
    (native-inputs
     `(
       ("ruby-benchmark-ips" ,ruby-benchmark-ips)
       ("ruby-codeclimate-test-reporter" ,ruby-codeclimate-test-reporter)
       ("ruby-grape" ,ruby-grape)
       ("ruby-json-schema" ,ruby-json-schema)
       ("ruby-kaminari" ,ruby-kaminari)
       ("ruby-m" ,ruby-m)
       ("ruby-minitest" ,ruby-minitest)
       ("ruby-pry" ,ruby-pry)
       ("ruby-rails" ,ruby-rails)
       ("ruby-rubocop" ,ruby-rubocop)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-sqlite3" ,ruby-sqlite3-1.3)
       ("ruby-timecop" ,ruby-timecop)
       ("ruby-will-paginate" ,ruby-will-paginate)
       ("ruby-yard" ,ruby-yard)
       ))
    (synopsis "Generate JSON in an object-oriented manner")
    (description
     "@code{ActiveModel::Serializers} allows one to generate your JSON in an
object-oriented and convention-driven manner.")
    (home-page "https://github.com/rails-api/active_model_serializers")
    (license license:expat)))

(define-public ruby-onebox
  (package
    (name "ruby-onebox")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "onebox" version))
        (sha256
         (base32
          "1lj5x8jrs9whgynfksvlnlds4crdi6dm9bb3vh654s8vpqxbjcbn"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; Tests require ancient version of ruby-twitter.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-version-requirements
           (lambda _
             (substitute* "onebox.gemspec"
               (("twitter.*") "twitter'\n"))
             #t)))))
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-htmlentities" ,ruby-htmlentities)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-mustache" ,ruby-mustache)
       ("ruby-nokogiri" ,ruby-nokogiri)
       ("ruby-sanitize" ,ruby-sanitize)))
    (native-inputs
     `(("ruby-fakeweb" ,ruby-fakeweb)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-pry" ,ruby-pry)
       ("ruby-rake" ,ruby-rake)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop-discourse" ,ruby-rubocop-discourse)
       ("ruby-twitter" ,ruby-twitter)))
    (synopsis "Generate embeddable HTML previews from URLs")
    (description "This package provides a gem for generating embeddable HTML
previews from URLs.")
    (home-page "https://github.com/discourse/onebox")
    (license license:expat)))

(define-public ruby-discourse-ember-source
  (package
    (name "ruby-discourse-ember-source")
    (version "3.12.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "discourse-ember-source" version))
        (sha256
         (base32
          "0mqkwiqb5n64lc5jdjnmpgb9apq08ywkz9yk8mj1sx2lqcsw11pc"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis "Fork of Ember source to permit latest ember versions")
    (description "This package provides a fork of Ember source to permit the
latest ember versions.")
    (home-page "https://github.com/discourse/discourse-ember-source")
    (license license:expat)))

(define-public ruby-jquery-rails
  (package
    (name "ruby-jquery-rails")
    (version "4.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "jquery-rails" version))
        (sha256
         (base32
          "0dkhm8lan1vnyl3ll0ks2q06576pdils8a1dr354vfc1y5dqw15i"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-rails-dom-testing" ,ruby-rails-dom-testing)
       ("ruby-railties" ,ruby-railties)
       ("ruby-thor" ,ruby-thor)))
    (synopsis
     "This gem provides jQuery and the jQuery-ujs driver for your Rails 4+ application.")
    (description
     "This gem provides jQuery and the jQuery-ujs driver for your Rails 4+ application.")
    (home-page "https://github.com/rails/jquery-rails")
    (license license:expat)))

(define-public ruby-barber
  (package
    (name "ruby-barber")
    (version "0.12.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "barber" version))
        (sha256
         (base32
          "07rnlbh7kgamcbnl1sqlcdrjj8src4qc687klqq4a3vqq2slnscx"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (propagated-inputs
     `(("ruby-ember-source" ,ruby-ember-source)
       ("ruby-execjs" ,ruby-execjs)))
    (native-inputs
     `(
       ("ruby-handlebars-source" ,ruby-handlebars-source-4.1)   ; < 4.2
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-simplecov" ,ruby-simplecov)
       ))
    (synopsis "Handlebars precompilation")
    (description "Handlebars precompilation")
    (home-page "https://github.com/tchak/barber")
    (license license:expat)))

(define-public ruby-ember-handlebars-template
  (package
    (name "ruby-ember-handlebars-template")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri
               "ember-handlebars-template"
               version))
        (sha256
         (base32
          "1wxj3vi4xs3vjxrdbzi4j4w6vv45r5dkz2rg2ldid3p8dp3irlf4"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-barber" ,ruby-barber)
       ("ruby-sprockets" ,ruby-sprockets)))
    (synopsis
      "The sprockets template for Ember Handlebars.")
    (description
      "The sprockets template for Ember Handlebars.")
    (home-page "https://github.com/tricknotes/ember-handlebars-template")
    (license license:expat)))

(define-public ruby-babel-source
  (package
    (name "ruby-babel-source")
    (version "5.8.35")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "babel-source" version))
        (sha256
         (base32
          "1ncq8h82k6hypzfb5dk7z95mmcdwnhsxmc53xz17m1nbklm25vvr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis "Babel JS source")
    (description "Babel JS source")
    (home-page "https://github.com/babel/ruby-babel-transpiler")
    (license license:expat)))

(define-public ruby-babel-transpiler
  (package
    (name "ruby-babel-transpiler")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "babel-transpiler" version))
        (sha256
         (base32
          "0w0minwxj56w96xps1msm6n75fs0y7r1vqcr9zlsn74fksnz81jc"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-babel-source" ,ruby-babel-source)
       ("ruby-execjs" ,ruby-execjs)))
    (synopsis
      "    Ruby Babel is a bridge to the JS Babel transpiler.
")
    (description
      "    Ruby Babel is a bridge to the JS Babel transpiler.
")
    (home-page "https://github.com/babel/ruby-babel-transpiler")
    (license license:expat)))

(define-public ruby-ember-es6-template
  (package
    (name "ruby-ember-es6-template")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ember-es6_template" version))
        (sha256
         (base32
          "0cb9yfwwxvi615k1vg20zkm6czkpapcncwbznbch58zkp5rdw7i9"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-babel-transpiler" ,ruby-babel-transpiler)
       ("ruby-sprockets" ,ruby-sprockets)))
    (synopsis
      "The tilt template for Ember specified ES6.")
    (description
      "The tilt template for Ember specified ES6.")
    (home-page "https://github.com/tricknotes/ember-es6_template")
    (license license:expat)))

(define-public ruby-ember-cli-assets
  (package
    (name "ruby-ember-cli-assets")
    (version "0.0.37")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ember-cli-assets" version))
        (sha256
         (base32
          "057dhafsdr4lbagx5k7sdrh2vfmy1llqpv347qy7d5jkhxdmns96"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- ember/cli/assets
    (synopsis "The assets for Ember CLI.")
    (description "The assets for Ember CLI.")
    (home-page "https://github.com/tricknotes/ember-cli-assets")
    (license license:expat)))

(define-public ruby-ember-source
  (package
    (name "ruby-ember-source")
    (version "2.18.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ember-source" version))
        (sha256
         (base32
          "0sixy30ym9j2slhlr0lfq943g958w8arlb0lsizh59iv1w5gmxxy"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "Ember.js source code wrapper for use with Ruby libs.")
    (description
      "Ember.js source code wrapper for use with Ruby libs.")
    (home-page "https://github.com/emberjs/ember.js")
    (license license:expat)))

(define-public ruby-ember-data-source
  (package
    (name "ruby-ember-data-source")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ember-data-source" version))
        (sha256
         (base32
          "1803nh3knvwl12h63jd48qvbbrp42yy291wcb35960daklip0fd8"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-ember-source" ,ruby-ember-source)))
    (synopsis
      "ember-data source code wrapper for use with Ruby libs.")
    (description
      "ember-data source code wrapper for use with Ruby libs.")
    (home-page "https://github.com/emberjs/data")
    (license license:expat)))

(define-public ruby-active-model-adapter-source
  (package
    (name "ruby-active-model-adapter-source")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri
               "active-model-adapter-source"
               version))
        (sha256
         (base32
          "0hr8a4nyppnqs053i1badjwhm095scrp0x1w7v742r9s5sxp8pyd"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-ember-data-source" ,ruby-ember-data-source)))
    (synopsis
      "ember-data active-model-adapter code wrapper for use with Ruby libs.")
    (description
      "ember-data active-model-adapter code wrapper for use with Ruby libs.")
    (home-page "https://github.com/ember-data/active-model-adapter")
    (license license:expat)))

(define-public ruby-ember-rails
  (package
    (name "ruby-ember-rails")
    (version "0.21.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ember-rails" version))
        (sha256
         (base32
          "0f1vd6l35d4q589sbmyxpjfs777kf0r5x6s6aap1v2r0i1x342a3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-active-model-adapter-source" ,ruby-active-model-adapter-source)
       ("ruby-active-model-serializers" ,ruby-active-model-serializers)
       ("ruby-ember-cli-assets" ,ruby-ember-cli-assets)
       ("ruby-ember-data-source" ,ruby-ember-data-source)
       ("ruby-ember-es6-template" ,ruby-ember-es6-template)
       ("ruby-ember-handlebars-template" ,ruby-ember-handlebars-template)
       ("ruby-ember-source" ,ruby-ember-source)
       ("ruby-jquery-rails" ,ruby-jquery-rails)
       ("ruby-railties" ,ruby-railties)))
    (synopsis "Ember for Rails 3.1+")
    (description "Ember for Rails 3.1+")
    (home-page "https://github.com/emberjs/ember-rails")
    (license license:expat)))

(define-public ruby-handlebars-source
  (package
    (name "ruby-handlebars-source")
    (version "4.7.7")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "handlebars-source" version))
        (sha256
         (base32
          "0sjir1fwrqajkcc8blr32vnkamcqma8b0na6fm562hh9rdzcxb0c"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "Handlebars.js source code wrapper for (pre)compilation gems.")
    (description
      "Handlebars.js source code wrapper for (pre)compilation gems.")
    (home-page "https://github.com/wycats/handlebars.js/")
    (license license:expat)))

(define-public ruby-handlebars-source-4.1
  (package
    (inherit ruby-handlebars-source)
    (name "ruby-handlebars-source")
    (version "4.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "handlebars-source" version))
        (sha256
         (base32
          "0w8xq7nnrwhz5bfcamkvkkcb10rw7kjb809n7w949mc5h0b4l8r5"))))))

(define-public ruby-discourse-ember-rails
  (package
    (name "ruby-discourse-ember-rails")
    (version "0.18.6")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "discourse-ember-rails" version))
        (sha256
         (base32
          "0ax5x2d6q6hkm7r58ai9p0sahlg842aqlm7dpv6svrfpnjlaz7sf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-active-model-serializers" ,ruby-active-model-serializers)
       ("ruby-ember-data-source" ,ruby-ember-data-source)
       ("ruby-ember-handlebars-template" ,ruby-ember-handlebars-template)
       ("ruby-ember-source" ,ruby-ember-source)
       ("ruby-jquery-rails" ,ruby-jquery-rails)
       ("ruby-railties" ,ruby-railties)))
    (synopsis "Ember for Rails 3.1+")
    (description "Ember for Rails 3.1+")
    (home-page "https://github.com/emberjs/ember-rails")
    (license license:expat)))

(define-public ruby-discourse-fonts
  (package
    (name "ruby-discourse-fonts")
    (version "0.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "discourse-fonts" version))
        (sha256
         (base32
          "101fhmgzrkwa8rvqrac3ccqp54aizm553n167ikzg7mcgbyrxw4a"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "Bundle of fonts which can be used to customize the look of Discourse")
    (description
      "Bundle of fonts which can be used to customize the look of Discourse")
    (home-page "https://github.com/discourse/discourse-fonts")
    (license license:expat)))

(define-public ruby-message-bus
  (package
    (name "ruby-message-bus")
    (version "3.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "message_bus" version))
        (sha256
         (base32
          "0hckijk9aa628nx66vr7axfsk7zfdkskaxj1mdzikk019q3h54fr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; Tests require running redis instance.
       #:test-target "default"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack)))
    (native-inputs
     `(
       ("ruby-byebug" ,ruby-byebug)
       ("ruby-concurrent" ,ruby-concurrent)
       ("ruby-http-parser.rb" ,ruby-http-parser.rb)
       ("ruby-jasmine" ,ruby-jasmine)
       ("ruby-minitest-global-expectations" ,ruby-minitest-global-expectations)
       ("ruby-minitest-hooks" ,ruby-minitest-hooks)
       ("ruby-pg" ,ruby-pg)
       ("ruby-puma" ,ruby-puma)
       ("ruby-rack-test" ,ruby-rack-test)
       ("ruby-redis" ,ruby-redis)
       ("ruby-rubocop-discourse" ,ruby-rubocop-discourse)
       ("ruby-thin" ,ruby-thin)
       ("ruby-yard" ,ruby-yard)
       ))
    (synopsis "Message bus for rack")
    (description "This package provides a message bus for rack.")
    (home-page "https://github.com/SamSaffron/message_bus")
    (license license:expat)))

(define-public ruby-phantomjs
  (package
    (name "ruby-phantomjs")
    (version "2.1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "phantomjs" version))
        (sha256
         (base32
          "0y8pbbyq9dirxb7igkb2s5limz2895qmr41c09fjhx6k6fxcz4mk"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; don't know how to build task 'test'
    (synopsis
      "Auto-install phantomjs on demand for current platform. Comes with poltergeist integration.")
    (description
      "Auto-install phantomjs on demand for current platform.  Comes with poltergeist integration.")
    (home-page "https://github.com/colszowka/phantomjs-gem")
    (license license:expat)))

(define-public ruby-jasmine-core
  (package
    (name "ruby-jasmine-core")
    (version "3.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "jasmine-core" version))
        (sha256
         (base32
          "0072bf0l1y6gnqw3mm4mlq7ivs19lyzr074sjibpfvryi7b449r6"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "Test your JavaScript without any framework dependencies, in any environment, and with a nice descriptive syntax.")
    (description
      "Test your JavaScript without any framework dependencies, in any environment, and with a nice descriptive syntax.")
    (home-page "http://jasmine.github.io")
    (license license:expat)))

(define-public ruby-jasmine
  (package
    (name "ruby-jasmine")
    (version "3.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "jasmine" version))
        (sha256
         (base32
          "1zbsr1d6507pvcfr3ji5hv0ksaaygi8jfp4dz1y1k1jdkkapy24b"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- rpsec
    (propagated-inputs
     `(("ruby-jasmine-core" ,ruby-jasmine-core)
       ("ruby-phantomjs" ,ruby-phantomjs)
       ("ruby-rack" ,ruby-rack)
       ("ruby-rake" ,ruby-rake)))
    (synopsis
      "Test your JavaScript without any framework dependencies, in any environment, and with a nice descriptive syntax.")
    (description
      "Test your JavaScript without any framework dependencies, in any environment, and with a nice descriptive syntax.")
    (home-page "http://jasmine.github.io/")
    (license license:expat)))

(define-public ruby-rubocop-discourse
  (package
    (name "ruby-rubocop-discourse")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rubocop-discourse" version))
        (sha256
         (base32
          "1z1h8spsjnsqz6c25n9ib1yimkwr7a76bas8w1k9c404hcqhlahv"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; don't know how to build task 'test'
    (propagated-inputs
     `(("ruby-rubocop" ,ruby-rubocop)
       ("ruby-rubocop-rspec" ,ruby-rubocop-rspec)))
    (synopsis
      "Custom rubocop cops used by Discourse")
    (description
      "Custom rubocop cops used by Discourse")
    (home-page "https://github.com/discourse/rubocop-discourse")
    (license license:expat)))

;; 2.1.0+ have ActiveRecord::ConnectionAdapters::ConnectionSpecification which causes failures
(define-public ruby-rails-multisite
  (package
    (name "ruby-rails-multisite")
    (version "2.0.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               ;; No tests included in release gem.
               (url "https://github.com/discourse/rails_multisite")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1k6bvymilcg0mvaszc5g14f87p6bvbm911dv6g4sa3asfgw62cdp"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))    ; tests not working with 2.0.7.
    (propagated-inputs
     `(("ruby-activerecord" ,ruby-activerecord)
       ("ruby-railties" ,ruby-railties)))
    (native-inputs
     `(("ruby-byebug" ,ruby-byebug)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-sqlite3" ,ruby-sqlite3-1.3)))
    (synopsis "Multi tenancy support for Rails")
    (description "This gem provides multi-db support for Rails applications.
Using its middleware you can partition your app so each hostname has its own db.
It provides a series of helper for working with multiple database, and some
additional rails tasks for working with them.")
    (home-page "https://github.com/discourse/rails_multisite")
    (license license:expat)))

(define-public ruby-fast-xs
  (package
    (name "ruby-fast-xs")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "fast_xs" version))
        (sha256
         (base32
          "1iydzaqmvqq7ncxkr182aybkk6xap0cb2w9amr73vbdxi2qf3wjz"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; undefined method `url='
    (propagated-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (synopsis "Ruby extensions for escaping text")
    (description
     "@code{fast_xs} escapes text so it can be embedded more directly into XML
and HTML without having to deal with character set issues.")
    (home-page "http://fast-xs.rubyforge.org/")
    (license license:expat)))

(define-public ruby-xorcist
  (package
    (name "ruby-xorcist")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "xorcist" version))
        (sha256
         (base32
          "1q7hr3qyn1hczv9fglqc2cbaax0fb37gjjr0y24x19mmp817csdn"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "Blazing-fast-cross-platform-monkey-patch-free string XOR. Yes, that means JRuby too.")
    (description
      "Blazing-fast-cross-platform-monkey-patch-free string XOR.  Yes, that means JRuby too.")
    (home-page "https://github.com/fny/xorcist")
    (license license:expat)))

(define-public ruby-fastimage
  (package
    (name "ruby-fastimage")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "fastimage" version))
        (sha256
         (base32
          "1zgv6588jiaisgng3bkcf5a3rlia82yrs39g7n27jhmpmhgk8j1w"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "FastImage finds the size or type of an image given its uri by fetching as little as needed.")
    (description
      "FastImage finds the size or type of an image given its uri by fetching as little as needed.")
    (home-page "https://github.com/sdsykes/fastimage")
    (license license:expat)))

(define-public ruby-progress
  (package
    (name "ruby-progress")
    (version "3.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "progress" version))
        (sha256
         (base32
          "1pm3bv5n8c8j0vfm7wghd7xf6yq4m068cksxjldmna11qi0h0s8s"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis "Show progress of long running tasks")
    (description
      "Show progress of long running tasks")
    (home-page "https://github.com/toy/progress")
    (license license:expat)))

(define-public ruby-in-threads
  (package
    (name "ruby-in-threads")
    (version "1.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "in_threads" version))
        (sha256
         (base32
          "0m71806p1gm4kxiz4gvkyr8qip16hifn2kdf926jz44jj6kc6bbs"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "Run all possible enumerable methods in concurrent/parallel threads")
    (description
      "Run all possible enumerable methods in concurrent/parallel threads")
    (home-page "https://github.com/toy/in_threads")
    (license license:expat)))

(define-public ruby-image-size
  (package
    (name "ruby-image-size")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "image_size" version))
        (sha256
         (base32
          "00irlgdpg67ay3wf5ljmphpdk6wc6khs6nhc5ysl5l10mmfi62p6"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "Measure following file dimensions: apng, bmp, cur, gif, ico, j2c, jp2, jpeg, jpx, mng, pam, pbm, pcx, pgm, png, ppm, psd, svg, swf, tiff, webp, xbm, xpm")
    (description
      "Measure following file dimensions: apng, bmp, cur, gif, ico, j2c, jp2, jpeg, jpx, mng, pam, pbm, pcx, pgm, png, ppm, psd, svg, swf, tiff, webp, xbm, xpm")
    (home-page "https://github.com/toy/image_size")
    (license license:ruby)))

(define-public ruby-image-size-1.5
  (package
    (inherit ruby-image-size)
    (name "ruby-image-size")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "image_size" version))
        (sha256
         (base32
          "0zrn2mqaf1kk548wn1y35i1a6kwh3320q62m929kn9m8sqpy4fk7"))))))

(define-public ruby-fspath
  (package
    (name "ruby-fspath")
    (version "3.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "fspath" version))
        (sha256
         (base32
          "0xcxikkrjv8ws328nn5ax5pyfjs8pn7djg1hks7qyb3yp6prpb5m"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis "Better than Pathname")
    (description "Better than Pathname")
    (home-page "https://github.com/toy/fspath")
    (license license:expat)))

(define-public ruby-exifr
  (package
    (name "ruby-exifr")
    (version "1.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "exifr" version))
        (sha256
         (base32
          "0mylhwmh6n4xihxr9s3zj0lc286f5maxbqd4dgk3paqnd7afz88s"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; test files missing?
    (synopsis
      "EXIF Reader is a module to read EXIF from JPEG and TIFF images.")
    (description
      "EXIF Reader is a module to read EXIF from JPEG and TIFF images.")
    (home-page "https://remvee.github.io/exifr/")
    (license license:expat)))

(define-public ruby-image-optim
  (package
    (name "ruby-image-optim")
    (version "0.28.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "image_optim" version))
        (sha256
         (base32
          "04n7xia22pr4ihzyf7bprnn630284cnfy2p3pk9q4b2cxaf5gj3s"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-exifr" ,ruby-exifr)
       ("ruby-fspath" ,ruby-fspath)
       ("ruby-image-size" ,ruby-image-size)
       ("ruby-in-threads" ,ruby-in-threads)
       ("ruby-progress" ,ruby-progress)))
    (synopsis
      "Command line tool and ruby interface to optimize (lossless compress, optionally lossy) jpeg, png, gif and svg images using external utilities (advpng, gifsicle, jhead, jpeg-recompress, jpegoptim, jpegrescan, jpegtran, optipng, pngcrush, pngout, pngquant, svgo)")
    (description
      "Command line tool and ruby interface to optimize (lossless compress, optionally lossy) jpeg, png, gif and svg images using external utilities (advpng, gifsicle, jhead, jpeg-recompress, jpegoptim, jpegrescan, jpegtran, optipng, pngcrush, pngout, pngquant, svgo)")
    (home-page "https://github.com/toy/image_optim")
    (license license:expat)))

(define-public ruby-discourse-image-optim
  (package
    (name "ruby-discourse-image-optim")
    (version "0.26.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "discourse_image_optim" version))
        (sha256
         (base32
          "11nqmga5ygxyhjmsc07gsa0fwwyhdpwi20yyr4fnh263xs1xylvv"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-exifr" ,ruby-exifr)
       ("ruby-fspath" ,ruby-fspath)
       ("ruby-image-size" ,ruby-image-size-1.5)
       ("ruby-in-threads" ,ruby-in-threads)
       ("ruby-progress" ,ruby-progress)))
    (synopsis
      "Optimize (lossless compress, optionally lossy) images (jpeg, png, gif, svg) using external utilities (advpng, gifsicle, jhead, jpeg-recompress, jpegoptim, jpegrescan, jpegtran, optipng, pngcrush, pngout, pngquant, svgo)")
    (description
      "Optimize (lossless compress, optionally lossy) images (jpeg, png, gif, svg) using external utilities (advpng, gifsicle, jhead, jpeg-recompress, jpegoptim, jpegrescan, jpegtran, optipng, pngcrush, pngout, pngquant, svgo)")
    (home-page "https://github.com/toy/discourse_image_optim")
    (license license:expat)))

(define-public ruby-omniauth-facebook
  (package
    (name "ruby-omniauth-facebook")
    (version "8.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "omniauth-facebook" version))
        (sha256
         (base32
          "1z0f5sr2ddnvfva0jrfd4926nlv4528rfj7z595288n39304r092"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-omniauth-oauth2" ,ruby-omniauth-oauth2)))
    (native-inputs
     `(("ruby-mocha" ,ruby-mocha)))
    (synopsis "Facebook OAuth2 Strategy for OmniAuth")
    (description
      "Facebook OAuth2 Strategy for OmniAuth")
    (home-page "https://github.com/simi/omniauth-facebook")
    (license license:expat)))

(define-public ruby-oauth
  (package
    (name "ruby-oauth")
    (version "0.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "oauth" version))
        (sha256
         (base32
          "1m08365nyp0fgw2iyzj8q8qy8zml0c1hw2dd8cp82pp6656ahbh3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis "OAuth Core Ruby implementation")
    (description "OAuth Core Ruby implementation")
    (home-page #f)
    (license license:expat)))

(define-public ruby-omniauth-oauth
  (package
    (name "ruby-omniauth-oauth")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "omniauth-oauth" version))
        (sha256
         (base32
          "0yw2vzx633p9wpdkd4jxsih6mw604mj7f6myyfikmj4d95c8d9z7"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- rspec/core/rake_task
    (propagated-inputs
     `(("ruby-oauth" ,ruby-oauth)
       ("ruby-omniauth" ,ruby-omniauth)))
    (synopsis
      "A generic OAuth (1.0/1.0a) strategy for OmniAuth.")
    (description
      "This package provides a generic OAuth (1.0/1.0a) strategy for OmniAuth.")
    (home-page "https://github.com/intridea/omniauth-oauth")
    (license license:expat)))

(define-public ruby-omniauth-twitter
  (package
    (name "ruby-omniauth-twitter")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "omniauth-twitter" version))
        (sha256
         (base32
          "0r5j65hkpgzhvvbs90id3nfsjgsad6ymzggbm7zlaxvnrmvnrk65"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- rspec/core/rake_task
    (propagated-inputs
     `(("ruby-omniauth-oauth" ,ruby-omniauth-oauth)
       ("ruby-rack" ,ruby-rack)))
    (synopsis "OmniAuth strategy for Twitter")
    (description "OmniAuth strategy for Twitter")
    (home-page "https://github.com/arunagw/omniauth-twitter")
    (license license:expat)))

(define-public ruby-omniauth-github
  (package
    (name "ruby-omniauth-github")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "omniauth-github" version))
        (sha256
         (base32
          "0jc66zp4bhwy7c6s817ws0nkimski3crrhwd7xyy55ss29v6b8hw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- rspec/core/rake_task
    (propagated-inputs
     `(("ruby-omniauth" ,ruby-omniauth)
       ("ruby-omniauth-oauth2" ,ruby-omniauth-oauth2)))
    (synopsis "Official OmniAuth strategy for GitHub")
    (description
      "Official OmniAuth strategy for GitHub.")
    (home-page "https://github.com/intridea/omniauth-github")
    (license license:expat)))

(define-public ruby-omniauth-google-oauth2
  (package
    (name "ruby-omniauth-google-oauth2")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "omniauth-google-oauth2" version))
        (sha256
         (base32
          "10awaj2s3c46knpv9vawhjzbbaygp685dm9n8blq2d3j4w5m3d53"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- rspec/core/rake_task
    (propagated-inputs
     `(("ruby-jwt" ,ruby-jwt)
       ("ruby-oauth2" ,ruby-oauth2)
       ("ruby-omniauth" ,ruby-omniauth)
       ("ruby-omniauth-oauth2" ,ruby-omniauth-oauth2)))
    (synopsis
      "A Google OAuth2 strategy for OmniAuth 1.x. This allows you to login to Google with your ruby app.")
    (description
      "This package provides a Google OAuth2 strategy for OmniAuth 1.x.  This allows you to login to Google with your ruby app.")
    (home-page "https://github.com/zquestz/omniauth-google-oauth2")
    (license license:expat)))

(define-public ruby-mini-sql
  (package
    (name "ruby-mini-sql")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mini_sql" version))
        (sha256
         (base32
          "1a2s8n4nq3w86hx3ya7xs2fkzz8rp0kmz0zqzhf9r5rybprr702m"))))
    (build-system ruby-build-system)
    (synopsis
      "A fast, safe, simple direct SQL executor for PG")
    (description
      "This package provides a fast, safe, simple direct SQL executor for PG")
    (home-page "https://github.com/discourse/mini_sql")
    (license license:expat)))

(define-public ruby-rinku
  (package
    (name "ruby-rinku")
    (version "2.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rinku" version))
        (sha256
         (base32
          "0zcdha17s1wzxyc5814j6319wqg33jbn58pg6wmxpws36476fq4b"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- rake/extensiontask
    (synopsis
      "    A fast and very smart autolinking library that
    acts as a drop-in replacement for Rails `auto_link`
")
    (description
      "    A fast and very smart autolinking library that
    acts as a drop-in replacement for Rails `auto_link`
")
    (home-page "https://github.com/vmg/rinku")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-sidekiq
  (package
    (name "ruby-sidekiq")
    (version "6.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "sidekiq" version))
        (sha256
         (base32
          "0ir95jdcv7ch57xiirmy6mjh8z2lm39nnwcpkwcjqx5698w0lsvs"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- standard/rake
    (propagated-inputs
     `(("ruby-connection-pool" ,ruby-connection-pool)
       ("ruby-rack" ,ruby-rack)
       ("ruby-redis" ,ruby-redis)))
    (synopsis
      "Simple, efficient background processing for Ruby.")
    (description
      "Simple, efficient background processing for Ruby.")
    (home-page "https://sidekiq.org")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-mini-scheduler
  (package
    (name "ruby-mini-scheduler")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mini_scheduler" version))
        (sha256
         (base32
          "1cy9c2wv19m4h2sv9fs66hh1an7hq3y9513678dzx43vm3kjvhz5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- rspec/core/rake_task
    (propagated-inputs
     `(("ruby-sidekiq" ,ruby-sidekiq)))
    (synopsis "Adds recurring jobs for Sidekiq")
    (description "Adds recurring jobs for Sidekiq")
    (home-page "https://github.com/discourse/mini_scheduler")
    (license license:expat)))

;; TODO: This package needs fixing!
(define-public ruby-libv8
  (package
    (name "ruby-libv8")
    (version "8.4.255.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "libv8" version))
        (sha256
         (base32
          "0317sr3nrl51sp844bps71smkrwim3fjn47wdfpbycixnbxspivm"))))
    (build-system ruby-build-system)
    (arguments
     `(
       ;#:test-target "spec binary"
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (assoc-ref %standard-phases 'check))
         (add-after 'unpack 'adjust-version-requirements
           (lambda _
             (substitute* "libv8.gemspec"
               (("rake-compiler.*") "rake-compiler'\n"))
             #t))
         (add-before 'build 'pre-build
           (lambda _
             (setenv "HOME" (getcwd))
             ;(invoke "bundle" "install")   ; no network access
             (invoke "bundle" "exec" "rake" "compile")
             ))
         )
       ))
    (native-inputs
     `(
       ("glib" ,(@ (gnu packages glib) glib))
       ("pkg-config" ,(@ (gnu packages pkg-config) pkg-config))
       ("python" ,python-2)
       ("ruby-rake" ,ruby-rake)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "Distributes the V8 JavaScript engine in binary and source forms in order to support fast builds of The Ruby Racer")
    (description
      "Distributes the V8 JavaScript engine in binary and source forms in order to support fast builds of The Ruby Racer")
    (home-page "https://github.com/rubyjs/libv8")
    (license license:expat)))

(define-public ruby-libv8-7.3
  (package
    (name "ruby-libv8")
    (version "7.3.492.27.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "libv8" version))
        (sha256
         (base32
          "1jivcckillfvd4n2jnsnnlf93z3gpvqbwsczs0fvv9hc90zpj7yh"))))
        ;(method git-fetch)
        ;(uri (git-reference
        ;       (url "https://github.com/rubyjs/libv8")
        ;       (commit (string-append "v" version))
        ;       (recursive? #t)))
        ;(file-name (git-file-name name version))
        ;(sha256
        ; (base32
        ;  "0sq026lxspglvnad2w3qiplcg8dc6ffj5130zm379yz7dncwgwdf"))))
    (build-system ruby-build-system)
    (arguments
     `(
       #:tests? #f  ; no test target
       #:gem-flags (list "--" "--with-system-v8"
                         (string-append "--with-v8-dir=" (assoc-ref %build-inputs "libnode")))
       #:phases
       (modify-phases %standard-phases
         ;(delete 'check)
         ;(add-after 'install 'check
         ;  (assoc-ref %standard-phases 'check))
         ;(add-after 'unpack 'patch-source
         ;  (lambda* (#:key inputs #:allow-other-keys)
         ;    (copy-file (assoc-ref inputs "cipd-client")
         ;               "vendor/depot_tools/.cipd_client")
         ;    (invoke "ls" "vendor/depot_tools/" "-la")
         ;    #t))
         (replace 'replace-git-ls-files
           (lambda _
             (substitute* "libv8.gemspec"
               ;(("git submodule.*`") "ls -d vendor/*`")
               ;(("^\\s+submodules.*") "")
               ;(("^\\s+end.*") "")
               ;(("^\\s+s.files \\+=.*") "")
               ;(("^\\s+`git.*") "")

               ;(("',.*") "'\n")

               (("`git ls-files`")  "`find . -type f`")
               )
             #t))
         ;; Unconditionally clones v8 for compilation.
         ;(delete 'build)
         ;(add-before 'build 'pre-build
         ;  (lambda _
         ;    ;(setenv "HOME" (getcwd))
         ;    (invoke "rake" "compile")
         ;    ))
         )
       ))
    (native-inputs
     `(
       ("glib" ,(@ (gnu packages glib) glib))
       ;("llvm" ,(@ (gnu packages llvm) llvm))
       ("pkg-config" ,(@ (gnu packages pkg-config) pkg-config))
       ("python" ,python-2)
       ;("ruby-rake" ,ruby-rake)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ("which" ,(@ (gnu packages base) which))
       ;("cipd-client"
       ; ,(origin
       ;    (method url-fetch)
       ;    (uri "https://chrome-infra-packages.appspot.com/client?platform=linux-amd64&version=git_revision:bd09df254cc0d6ca4319f23c16b9039091be5b00")
       ;    (file-name "ruby-libv8-cipd-client")
       ;    (sha256
       ;     (base32 "1wznd036rj7a6wpqdbbp0imlxqcv20iic1xhwg0rz7fp9x9zciz6"))))
       ))
    (inputs
     `(
       ("libnode" ,(@ (gnu packages node) libnode))
       ))
    (synopsis
      "Distributes the V8 JavaScript engine in binary and source forms in order to support fast builds of The Ruby Racer")
    (description
      "Distributes the V8 JavaScript engine in binary and source forms in order to support fast builds of The Ruby Racer")
    (home-page "https://github.com/rubyjs/libv8")
    (license license:expat)))

;; The last version of libv8 that can use system v8?
(define-public ruby-libv8-6.3
  (package
    (name "ruby-libv8")
    (version "6.3.292.48.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "libv8" version))
        (sha256
         (base32
          "0fispdxscqnghahxhcb360lly25r7zsg695ygb8d79g4n03wx2js"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; no test target / can't find rake-compiler
       #:gem-flags (list "--"
                         "--with-system-v8"
                         (string-append "--with-v8-dir="
                                        (assoc-ref %build-inputs "libnode"))
                         ;(string-append "--with-v8-include="
                         ;               (assoc-ref %build-inputs "libnode")
                         ;               "/include/node")
                         )
       #:phases
       (modify-phases %standard-phases
         (replace 'replace-git-ls-files
           (lambda _
             (substitute* "libv8.gemspec"
               (("`git ls-files`") "`find . -type f`"))
             #t))
         (add-after 'unpack 'patch-source
           ;; v8.h includes <memory>, which breaks the autotools-like detection scripts
           (lambda _
             (substitute* "ext/libv8/location.rb"
               ;((".*find_header, 'v8.h.*") ""))
               ((".*find_header.*") ""))
             #t))
         (add-after 'install 'remove-depot-tools
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively
               (string-append (assoc-ref outputs "out")
                              "/lib/ruby/vendor_ruby/gems/libv8-"
                              ,version "/vendor"))
             #t)))))
    (native-inputs
     `(("ruby-rake" ,ruby-rake)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)))
    (inputs
     `(("libnode" ,(@ (gnu packages node) libnode))))
    (synopsis "V8 JavaScript engine for The Ruby Racer")
    (description
     "This package provides a Ruby wrapper around the Javascript V8 engine.")
    (home-page "https://github.com/rubyjs/libv8")
    (license license:expat)))

(define-public libv8-7.3
  (package
    (name "libv8")
    (version "7.3.492.27")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://chromium.googlesource.com/v8/v8")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0y9awgbryap4z9kb9zhih4ri3kqkw7imxnjn3fzcm9mwzbj4wn0j"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (substitute* ".gn"
               ;; Files refered to seems to not exist in repository.
               ((".*dotfile_settings.*") "")
               ;((".*BUILDCONFIG.*") "")
               )
             ;(invoke "tools/dev/gm.py" "x64.release")
             (invoke "gn" "gen" "out/x64" "--args is_debug=false target_cpu=\"x64\" v8_target_cpu=\"arm64\" use_goma=false")
             ))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "tools/dev/gm.py" "x64.release.check"))
             #t))
         )
       ))
    (native-inputs
     `(
       ("gn" ,generate-ninja)
       ;("python-gyp" ,python2-gyp)
       ))
    (synopsis "")
    (description "")
    (home-page "")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public generate-ninja   ; or 'gn'
  (let ((commit "dfcbc6fed0a8352696f92d67ccad54048ad182b3")
        (revision "1"))
    (package
      (name "generate-ninja")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gn.googlesource.com/gn")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1941bzg37c4dpsk3sh6ga3696gpq6vjzpcw9rsnf6kdr9mcgdxvn"))))
      (build-system meson-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda _
               (setenv "CC" ,(cc-for-target))
               (setenv "CXX" ,(cxx-for-target))
               (substitute* "build/gen.py"
                 (("      \\['git.*\\]")
                  (string-append "      ['" (which "echo")
                                 "', 'initial-commit-1111-g"
                                 ,(string-take commit 10) "']")))
               (invoke "python" "build/gen.py")
               ;; This is an expected part of 'configure in the meson-build-system.
               (chdir "out")
               #t))
           (delete 'patch-generated-file-shebangs)
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "./gn_unittests"))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "gn" (string-append out "/bin"))
                 (install-file "../LICENSE"
                               (string-append out "/share/doc/"
                                              ,name "-" ,version))
                 #t))))))
      (native-inputs
       `(("python" ,python-wrapper)))
      (home-page "https://gn.googlesource.com/gn")
      (synopsis "Meta-build system for ninja")
      (description "Generate-Ninja, or GN, is a meta-build system that generates
Ninja build files so that you can build your project with Ninja.  GN was,
originally, part of the Chromium source tree, and has since been extracted into
its own standalone repo.")
      (license license:bsd-3))))

(define-public ruby-mini-racer
  (package
    (name "ruby-mini-racer")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mini_racer" version))
        (sha256
         (base32
          "0r7j241mvhyyc017bqgp0pvf3jyrwbcqvz2pzm0r8zn2r85ks1jl"))))
    (build-system ruby-build-system)
    (propagated-inputs `(("ruby-libv8" ,ruby-libv8)))
    (synopsis "Minimal embedded v8 engine for Ruby")
    (description
      "Minimal embedded v8 engine for Ruby")
    (home-page "https://github.com/discourse/mini_racer")
    (license license:expat)))

(define-public ruby-mini-racer-0.2
  (package
    (inherit ruby-mini-racer)
    (version "0.2.15")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mini_racer" version))
        (sha256
         (base32
          "1gm4lin39pj0xi9ip22ynafxhq9xn79fq4fspqhph6bqv02cyv6a"))))
    (arguments
     `(#:gem-flags (list "--" "--with-system-v8"
                         ;(string-append "--with-v8-dir="
                         ;               (assoc-ref %build-inputs "libnode"))
                         (string-append "--with-v8-include="
                                        (assoc-ref %build-inputs "libnode")
                                        "/include/node")
                         )
       ))
    (propagated-inputs
     `(
       ("ruby-libv8" ,ruby-libv8-7.3)
       ))
    (inputs
     `(("libnode" ,(@ (gnu packages node) libnode))))
    (native-inputs
     `(
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ))
    ))

;; The last version which uses ruby-libv8@6.3
(define-public ruby-mini-racer-0.2.4
  (package
    (inherit ruby-mini-racer)
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mini_racer" version))
        (sha256
         (base32
          "1c3a61l805slbvqscmc2wbv6bvw37gs2dchyhbfql178mnb7vnwg"))))
    (arguments
     `(#:gem-flags
       (list "--"
             ;; Include the correct lib directory and then fake
             ;; linking to '-llibnode' / '-lv8'.
             (string-append "--with-v8-lib="
                            (assoc-ref %build-inputs "libnode")
                            "/lib")
             "--with-v8lib=ruby"
             (string-append "--with-v8-include="
                            (assoc-ref %build-inputs "libnode")
                            "/include/node"))))
    (propagated-inputs
     `(("ruby-libv8" ,ruby-libv8-6.3)))
    (inputs
     `(("libnode" ,(@ (gnu packages node) libnode))))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)))))

(define-public ruby-uglifier
  (package
    (name "ruby-uglifier")
    (version "4.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "uglifier" version))
        (sha256
         (base32
          "0wgh7bzy68vhv9v68061519dd8samcy8sazzz0w3k8kqpy3g4s5f"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Tests not included in gem
    (propagated-inputs
     `(("ruby-execjs" ,ruby-execjs)))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis
      "Uglifier minifies JavaScript files by wrapping UglifyJS to be accessible in Ruby")
    (description
      "Uglifier minifies JavaScript files by wrapping UglifyJS to be accessible in Ruby")
    (home-page "https://github.com/lautis/uglifier")
    (license license:expat)))

(define-public ruby-fast-blank
  (package
    (name "ruby-fast-blank")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "fast_blank" version))
        (sha256
         (base32
          "16s1ilyvwzmkcgmklbrn0c2pch5n02vf921njx0bld4crgdr6z56"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "Provides a C-optimized method for determining if a string is blank")
    (description
      "This package provides a C-optimized method for determining if a string is blank")
    (home-page "https://github.com/SamSaffron/fast_blank")
    (license license:expat)))

(define-public ruby-lru-redux
  (package
    (name "ruby-lru-redux")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "lru_redux" version))
        (sha256
         (base32
          "1yxghzg7476sivz8yyr9nkak2dlbls0b89vc2kg52k0nmg6d0wgf"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-timecop" ,ruby-timecop)))
    (synopsis
      "An efficient implementation of an lru cache")
    (description
      "An efficient implementation of an lru cache")
    (home-page "https://github.com/SamSaffron/lru_redux")
    (license license:expat)))

(define-public ruby-rack-mini-profiler
  (package
    (name "ruby-rack-mini-profiler")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rack-mini-profiler" version))
        (sha256
         (base32
          "1zir2lf9vc6h98gly4qmsd2gdvly4pn8576pl9kzx7i9j4v54ysh"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack)))
    (synopsis
      "Profiling toolkit for Rack applications with Rails integration. Client Side profiling, DB profiling and Server profiling.")
    (description
      "Profiling toolkit for Rack applications with Rails integration.  Client Side profiling, DB profiling and Server profiling.")
    (home-page "https://miniprofiler.com")
    (license license:expat)))

(define-public ruby-logster
  (package
    (name "ruby-logster")
    (version "2.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "logster" version))
        (sha256
         (base32
          "1r0s6y2gh81vsajnc1ny5k9lv9zdm7667v4b3kh2w0w4v82kiiw1"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; TODO: only disable network tests
    (native-inputs
     `(
       ("ruby-byebug" ,ruby-byebug)
       ("ruby-rack" ,ruby-rack)
       ("ruby-redis" ,ruby-redis)
       ("ruby-timecop" ,ruby-timecop)
       ))
    (synopsis "UI for viewing logs in Rack")
    (description "UI for viewing logs in Rack")
    (home-page "https://github.com/discourse/logster")
    (license license:expat)))

(define-public ruby-sassc-rails
  (package
    (name "ruby-sassc-rails")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "sassc-rails" version))
        (sha256
         (base32
          "1d9djmwn36a5m8a83bpycs48g8kh1n2xkyvghn7dr6zwh4wdyksz"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; cannot find tzinfo-data
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             (substitute* "sassc-rails.gemspec"
               (("~> 10.0") ">= 10.0"))
             #t)))))
    (propagated-inputs
     `(("ruby-railties" ,ruby-railties)
       ("ruby-sassc" ,ruby-sassc)
       ("ruby-sprockets" ,ruby-sprockets)
       ("ruby-sprockets-rails" ,ruby-sprockets-rails)
       ("ruby-tilt" ,ruby-tilt)))
    (native-inputs
     `(
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-pry" ,ruby-pry)
       ("ruby-rake" ,ruby-rake)
       ("ruby-tzinfo-data" ,ruby-tzinfo-data)
       ))
    (synopsis "Integrate SassC-Ruby into Rails.")
    (description "Integrate SassC-Ruby into Rails.")
    (home-page "https://github.com/sass/sassc-rails")
    (license license:expat)))

(define-public ruby-rqrcode-core
  (package
    (name "ruby-rqrcode-core")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rqrcode_core" version))
        (sha256
         (base32
          "00kqasqja8zyzqvlgiwd9r0wndqk01qk5j68a8lhlz4ayrd4qy0y"))))
    (build-system ruby-build-system)
    (synopsis
      "rqrcode_core is a Ruby library for encoding QR Codes. The simple
interface (with no runtime dependencies) allows you to create QR Code data structures.
")
    (description
      "rqrcode_core is a Ruby library for encoding QR Codes.  The simple
interface (with no runtime dependencies) allows you to create QR Code data structures.
")
    (home-page "https://github.com/whomwah/rqrcode_core")
    (license license:expat)))

(define-public ruby-rqrcode
  (package
    (name "ruby-rqrcode")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rqrcode" version))
        (sha256
         (base32
          "0f1cv9a9sjqc898qm3h7zmkhwglrjw5blsskbg3gsaws01d4bc47"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-chunky-png" ,ruby-chunky-png)
       ("ruby-rqrcode-core" ,ruby-rqrcode-core)))
    (synopsis
      "rqrcode is a library for encoding QR Codes. The simple
interface allows you to create QR Code data structures
and then render them in the way you choose.
")
    (description
      "rqrcode is a library for encoding QR Codes.  The simple
interface allows you to create QR Code data structures
and then render them in the way you choose.
")
    (home-page "https://github.com/whomwah/rqrcode")
    (license license:expat)))

(define-public ruby-sqlite3-1.3
  (package
    (inherit ruby-sqlite3)
    (name "ruby-sqlite3")
    (version "1.3.13")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "sqlite3" version))
        (sha256
         (base32
          "01ifzp8nwzqppda419c9wcvr8n82ysmisrs0hph9pdmv1lpa4f5i"))))))

(define-public ruby-bbcode-to-md
  (let ((commit "3b9aaf2b634704a415788c94f7dee93a81f616b5")
           (revision "1"))
    (package
      (name "ruby-bbcode-to-md")
      (version (git-version "0.0.15" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/nlalonde/ruby-bbcode-to-md")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1c173jr1y4z7vxk7cshh17mjdkq8d690zzs3kc76j2wbvfbmcpcp"))))
      (build-system ruby-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda _
               (delete-file "Gemfile.lock")
               #t)))))
      (propagated-inputs
       `(("ruby-activesupport" ,ruby-activesupport)))
      (native-inputs
       `(("ruby-pry" ,ruby-pry)))
      (synopsis
        "Convert BBCode to Markdown and check whether the BBCode is valid.")
      (description
        "Convert BBCode to Markdown and check whether the BBCode is valid.")
      (home-page "https://github.com/rikkit/ruby-bbcode-to-md")
      (license license:expat))))

(define-public ruby-tiny-tds
  (package
    (name "ruby-tiny-tds")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "tiny_tds" version))
        (sha256
         (base32
          "0hy3kbcb6nwydy312rhjm4b30yavmayszzzyjpfdv6p0s8d9mfvb"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; File does not exist: tiny_tds/tiny_tds
    (inputs
     `(("freetds" ,freetds)))
    (native-inputs
     `(("ruby-connection-pool" ,ruby-connection-pool)
       ("ruby-mini-portile" ,ruby-mini-portile-2)
       ("ruby-rake" ,ruby-rake)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rake-compiler-dock" ,ruby-rake-compiler-dock)))
    (synopsis
      "TinyTDS - A modern, simple and fast FreeTDS library for Ruby using DB-Library. Developed for the ActiveRecord SQL Server adapter.")
    (description
      "TinyTDS - A modern, simple and fast FreeTDS library for Ruby using DB-Library.  Developed for the ActiveRecord SQL Server adapter.")
    (home-page "https://github.com/rails-sqlserver/tiny_tds")
    (license license:expat)))

(define-public ruby-rake-compiler-dock
  (package
    (name "ruby-rake-compiler-dock")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rake-compiler-dock" version))
        (sha256
         (base32
          "0z70p0jdp4ww0ax783nvfz1ppr8bf31kgy3la8wrcyhz1lvpq799"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load rake_compiler_dock
    (synopsis
      "Easy to use and reliable cross compiler environment for building Windows and Linux binary gems.
Use rake-compiler-dock to enter an interactive shell session or add a task to your Rakefile to automate your cross build.")
    (description
      "Easy to use and reliable cross compiler environment for building Windows and Linux binary gems.
Use rake-compiler-dock to enter an interactive shell session or add a task to your Rakefile to automate your cross build.")
    (home-page "https://github.com/rake-compiler/rake-compiler-dock")
    (license license:expat)))

(define-public ruby-csv
  (package
    (name "ruby-csv")
    (version "3.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "csv" version))
        (sha256
         (base32
          "07mgyalwdxaxnff86j5p6n5szmhqz7nrlkb40826mzggrmva8v1m"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "The CSV library provides a complete interface to CSV files and data. It offers tools to enable you to read and write to and from Strings or IO objects, as needed.")
    (description
      "The CSV library provides a complete interface to CSV files and data.  It offers tools to enable you to read and write to and from Strings or IO objects, as needed.")
    (home-page "https://github.com/ruby/csv")
    (license (list (license:non-copyleft "will fill in later")
                   (license:non-copyleft "will fill in later")))))

(define-public ruby-maxminddb
  (package
    (name "ruby-maxminddb")
    (version "0.1.22")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "maxminddb" version))
        (sha256
         (base32
          "0zlhqilyggiryywgswfi624bv10qnkm66hggmg79vvgv73j3p4sh"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; don't how how to build task 'test'
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-its" ,ruby-rspec-its)
       ))
    (synopsis
      "Pure Ruby MaxMind DB (GeoIP2) binary file reader.")
    (description
      "Pure Ruby MaxMind DB (GeoIP2) binary file reader.")
    (home-page "https://github.com/yhirose/maxminddb")
    (license license:expat)))

(define-public ruby-codeclimate-test-reporter
  (package
    (name "ruby-codeclimate-test-reporter")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri
               "codeclimate-test-reporter"
               version))
        (sha256
         (base32
          "1hq1f9c3f9lh0wr0apccgl6w1q2w39q93raagg50kchi7zp288cm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-simplecov" ,ruby-simplecov-0.13)))
    (synopsis
      "Collects test coverage data from your Ruby test suite and sends it to Code Climate's hosted, automated code review service. Based on SimpleCov.")
    (description
      "Collects test coverage data from your Ruby test suite and sends it to Code Climate's hosted, automated code review service.  Based on SimpleCov.")
    (home-page "https://github.com/codeclimate/ruby-test-reporter")
    (license license:expat)))

(define-public ruby-simplecov-0.13
  (package
    (inherit ruby-simplecov)
    (name "ruby-simplecov")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "simplecov" version))
        (sha256
         (base32
          "1r46dxq6r5rc7mgfb4w68qsm27w4qrp9kwjpssch9d5ngr12g0n7"))))))

(define-public ruby-kaminari-activerecord
  (package
    (name "ruby-kaminari-activerecord")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "kaminari-activerecord" version))
        (sha256
         (base32
          "02n5xxv6ilh39q2m6vcz7qrdai7ghk3s178dw6f0b3lavwyq49w3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-activerecord" ,ruby-activerecord)
       ("ruby-kaminari-core" ,ruby-kaminari-core)))
    (synopsis
      "kaminari-activerecord lets your Active Record models be paginatable")
    (description
      "kaminari-activerecord lets your Active Record models be paginatable")
    (home-page "https://github.com/kaminari/kaminari")
    (license license:expat)))

(define-public ruby-kaminari-core
  (package
    (name "ruby-kaminari-core")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "kaminari-core" version))
        (sha256
         (base32
          "0h04cr4y1jfn81gxy439vmczifghc2cvsyw47aa32is5bbxg1wlz"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "kaminari-core includes pagination logic independent from ORMs and view libraries")
    (description
      "kaminari-core includes pagination logic independent from ORMs and view libraries")
    (home-page "https://github.com/kaminari/kaminari")
    (license license:expat)))

(define-public ruby-kaminari-actionview
  (package
    (name "ruby-kaminari-actionview")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "kaminari-actionview" version))
        (sha256
         (base32
          "0w0p1hyv6lgf6h036cmn2kbkdv4x7g0g9q9kc5gzkpz7amlxr8ri"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-actionview" ,ruby-actionview)
       ("ruby-kaminari-core" ,ruby-kaminari-core)))
    (synopsis
      "kaminari-actionview provides pagination helpers for your Action View templates")
    (description
      "kaminari-actionview provides pagination helpers for your Action View templates")
    (home-page "https://github.com/kaminari/kaminari")
    (license license:expat)))

(define-public ruby-kaminari
  (package
    (name "ruby-kaminari")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "kaminari" version))
        (sha256
         (base32
          "1vxkqciny5v4jgmjxl8qrgbmig2cij2iskqbwh4bfcmpxf467ch3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; cannot load such file -- kaminari
    (propagated-inputs
      `(("ruby-activesupport" ,ruby-activesupport)
        ("ruby-kaminari-actionview" ,ruby-kaminari-actionview)
        ("ruby-kaminari-activerecord" ,ruby-kaminari-activerecord)
        ("ruby-kaminari-core" ,ruby-kaminari-core)))
    (synopsis
      "Kaminari is a Scope & Engine based, clean, powerful, agnostic, customizable and sophisticated paginator for Rails 4+")
    (description
      "Kaminari is a Scope & Engine based, clean, powerful, agnostic, customizable and sophisticated paginator for Rails 4+")
    (home-page "https://github.com/kaminari/kaminari")
    (license license:expat)))

;; TODO? Replace ruby-kaminari-* inputs with correct version?
(define-public ruby-kaminari-0.16
  (package
    (inherit ruby-kaminari)
    (name "ruby-kaminari")
    (version "0.16.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "kaminari" version))
        (sha256
         (base32
          "14vx3kgssl4lv2kn6grr5v2whsynx5rbl1j9aqiq8nc3d7j74l67"))))))

(define-public ruby-will-paginate
  (package
    (name "ruby-will-paginate")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "will_paginate" version))
        (sha256
         (base32
          "10qk4mf3rfc0vr26j0ba6vcz7407rdjfn13ph690pkzr94rv8bay"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis
      "will_paginate provides a simple API for performing paginated queries with Active Record, DataMapper and Sequel, and includes helpers for rendering pagination links in Rails, Sinatra, Hanami, and Merb web apps.")
    (description
      "will_paginate provides a simple API for performing paginated queries with Active Record, DataMapper and Sequel, and includes helpers for rendering pagination links in Rails, Sinatra, Hanami, and Merb web apps.")
    (home-page "https://github.com/mislav/will_paginate")
    (license license:expat)))

(define-public ruby-rack-accept
  (package
    (name "ruby-rack-accept")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rack-accept" version))
        (sha256
         (base32
          "18jdipx17b4ki33cfqvliapd31sbfvs4mv727awynr6v95a7n936"))))
    (build-system ruby-build-system)
    (propagated-inputs `(("ruby-rack" ,ruby-rack)))
    (synopsis
      "HTTP Accept, Accept-Charset, Accept-Encoding, and Accept-Language for Ruby/Rack")
    (description
      "HTTP Accept, Accept-Charset, Accept-Encoding, and Accept-Language for Ruby/Rack")
    (home-page "http://mjijackson.github.com/rack-accept")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-mustermann
 (package
  (name "ruby-mustermann")
  (version "1.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "mustermann" version))
      (sha256
        (base32
          "0ccm54qgshr1lq3pr1dfh7gphkilc19dp63rw6fcx7460pjwy88a"))))
  (build-system ruby-build-system)
  (propagated-inputs
    `(("ruby-ruby2-keywords" ,ruby-ruby2-keywords)))
  (synopsis
    "A library implementing patterns that behave like regular expressions.")
  (description
    "This package provides a library implementing patterns that behave like regular expressions.")
  (home-page
    "https://github.com/sinatra/mustermann")
  (license license:expat)))

(define-public ruby-mustermann-grape
  (package
    (name "ruby-mustermann-grape")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mustermann-grape" version))
        (sha256
         (base32
          "0djlbi7nh161a5mwjdm1ya4hc6lyzc493ah48gn37gk6vyri5kh0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; You must use Bundler 2 or greater with this lockfile.
    (propagated-inputs
     `(("ruby-mustermann" ,ruby-mustermann)))
    (synopsis
      "Adds Grape style patterns to Mustermman")
    (description
      "Adds Grape style patterns to Mustermman")
    (home-page "https://github.com/ruby-grape/mustermann-grape")
    (license license:expat)))

(define-public ruby-dry-logic
  (package
    (name "ruby-dry-logic")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "dry-logic" version))
        (sha256
         (base32
          "17dnc3g9y2nj42rdx2bdvsvvms10vgw4qzjb2iw2gln9hj8b797c"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-concurrent-ruby" ,ruby-concurrent)
       ("ruby-dry-core" ,ruby-dry-core)))
    (synopsis
      "Predicate logic with rule composition")
    (description
      "Predicate logic with rule composition")
    (home-page "https://dry-rb.org/gems/dry-logic")
    (license license:expat)))

(define-public ruby-dry-inflector
  (package
    (name "ruby-dry-inflector")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "dry-inflector" version))
        (sha256
         (base32
          "17mkdwglqsd9fg272y3zms7rixjgkb1km1xcb88ir5lxvk1jkky7"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (synopsis "String inflections for dry-rb")
    (description "String inflections for dry-rb")
    (home-page "https://dry-rb.org")
    (license license:expat)))

(define-public ruby-dry-core
  (package
    (name "ruby-dry-core")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "dry-core" version))
        (sha256
         (base32
          "14s45hxcqpp2mbvwlwzn018i8qhcjzgkirigdrv31jd741rpgy9s"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-concurrent-ruby" ,ruby-concurrent)))
    (synopsis
      "A toolset of small support modules used throughout the dry-rb ecosystem")
    (description
      "This package provides a toolset of small support modules used throughout the dry-rb ecosystem")
    (home-page "https://dry-rb.org/gems/dry-core")
    (license license:expat)))

(define-public ruby-dry-configurable
  (package
    (name "ruby-dry-configurable")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "dry-configurable" version))
        (sha256
         (base32
          "1fxr1352sgjbyk85qh4nfj974czw5b3rqjnl71q9p8v8fxrl6ln3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-concurrent-ruby" ,ruby-concurrent)
       ("ruby-dry-core" ,ruby-dry-core)))
    (synopsis
      "A mixin to add configuration functionality to your classes")
    (description
      "This package provides a mixin to add configuration functionality to your classes")
    (home-page "https://dry-rb.org/gems/dry-configurable")
    (license license:expat)))

(define-public ruby-dry-container
  (package
    (name "ruby-dry-container")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "dry-container" version))
        (sha256
         (base32
          "1npnhs3x2xcwwijpys5c8rpcvymrlab0y8806nr4h425ld5q4wd0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-concurrent-ruby" ,ruby-concurrent)
       ("ruby-dry-configurable" ,ruby-dry-configurable)))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "A simple container intended for use as an IoC container")
    (description
      "This package provides a simple container intended for use as an IoC container")
    (home-page "https://github.com/dry-rb/dry-container")
    (license license:expat)))

(define-public ruby-dry-types
  (package
    (name "ruby-dry-types")
    (version "1.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "dry-types" version))
        (sha256
         (base32
          "1gv0s396lzxlr882qgwi90462wn6f99wq6g0y204r94i3yfh1lvd"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-concurrent-ruby" ,ruby-concurrent)
       ("ruby-dry-container" ,ruby-dry-container)
       ("ruby-dry-core" ,ruby-dry-core)
       ("ruby-dry-inflector" ,ruby-dry-inflector)
       ("ruby-dry-logic" ,ruby-dry-logic)))
    (synopsis
      "Type system for Ruby supporting coercions, constraints and complex types like structs, value objects, enums etc")
    (description
      "Type system for Ruby supporting coercions, constraints and complex types like structs, value objects, enums etc")
    (home-page "https://dry-rb.org/gems/dry-types")
    (license license:expat)))

(define-public ruby-grape
  (package
    (name "ruby-grape")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "grape" version))
        (sha256
         (base32
          "0adf01kihxbmh8q84r6zyfgdmpbyb0lwcar3fi8j6bl6qcsbgwqx"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-activesupport" ,ruby-activesupport)
       ("ruby-builder" ,ruby-builder)
       ("ruby-dry-types" ,ruby-dry-types)
       ("ruby-mustermann-grape" ,ruby-mustermann-grape)
       ("ruby-rack" ,ruby-rack)
       ("ruby-rack-accept" ,ruby-rack-accept)))
    (synopsis
      "A Ruby framework for rapid API development with great conventions.")
    (description
      "This package provides a Ruby framework for rapid API development with great conventions.")
    (home-page "https://github.com/ruby-grape/grape")
    (license license:expat)))

(define-public ruby-m
  (package
    (name "ruby-m")
    (version "1.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "m" version))
        (sha256
         (base32
          "15jnbpl7b08im4g42ambc850w01lmc49k1z4438ipj83xsj5x32w"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; needs pygments
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "Gemfile.lock"
               (("\\(.*\\)") ""))
             #t)))))
    (propagated-inputs
     `(("ruby-method-source" ,ruby-method-source)
       ("ruby-rake" ,ruby-rake)))
    (native-inputs
     `(("ruby-activesupport" ,ruby-activesupport)
       ("ruby-allocation-stats" ,ruby-allocation-stats)
       ("ruby-appraisal" ,ruby-appraisal)
       ("ruby-benchmark-ips" ,ruby-benchmark-ips)
       ("ruby-coveralls" ,ruby-coveralls)
       ("ruby-rdiscount" ,ruby-rdiscount)
       ("ruby-rocco" ,ruby-rocco)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis
      "Run test/unit tests by line number. Metal!")
    (description
      "Run test/unit tests by line number.  Metal!")
    (home-page "https://github.com/qrush/m")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-allocation-stats
  (package
    (name "ruby-allocation-stats")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "allocation_stats" version))
        (sha256
         (base32
          "00xrlbprgnval73s0na9365zd6qapr260cgqww4d7l3ir0wb56yb"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; TODO: fix tests
       #:test-target "spec"))
    (native-inputs
     `(
       ("ruby-pry" ,ruby-pry)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-yajl-ruby" ,ruby-yajl-ruby)
       ("ruby-yard" ,ruby-yard)
       ))
    (synopsis
      "Tooling for tracing object allocations in Ruby 2.1")
    (description
      "Tooling for tracing object allocations in Ruby 2.1")
    (home-page "https://github.com/srawlins/allocation_stats")
    (license license:asl2.0)))

(define-public ruby-rdiscount
  (package
    (name "ruby-rdiscount")
    (version "2.2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rdiscount" version))
        (sha256
         (base32
          "16srf8cr8ynlafyh6ls654b9a3bqgai8n3y86zzv9mcpvlk6k27g"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; TODO: figure this out later
    (synopsis
      "Fast Implementation of Gruber's Markdown in C")
    (description
      "Fast Implementation of Gruber's Markdown in C")
    (home-page "http://dafoster.net/projects/rdiscount/")
    (license license:bsd-3)))

(define-public ruby-fakeweb
  (package
    (name "ruby-fakeweb")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "fakeweb" version))
        (sha256
         (base32
          "1a09z9nb369bvwpghncgd5y4f95lh28w0q258srh02h22fz9dj8y"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; File does not exist: samuel/net_http
    (native-inputs
     `(
       ;("ruby-jeweler" ,ruby-jeweler)
       ("ruby-mocha" ,ruby-mocha)
       ;("ruby-rcov" ,ruby-rcov)
       ("ruby-sdoc" ,ruby-sdoc)
       ))
    (synopsis
      "FakeWeb is a helper for faking web requests in Ruby. It works at a global level, without modifying code or writing extensive stubs.")
    (description
      "FakeWeb is a helper for faking web requests in Ruby.  It works at a global level, without modifying code or writing extensive stubs.")
    (home-page "http://github.com/chrisk/fakeweb")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-simple-oauth
  (package
    (name "ruby-simple-oauth")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "simple_oauth" version))
        (sha256
         (base32
          "0dw9ii6m7wckml100xhjc6vxpjcry174lbi9jz5v7ibjr3i94y8l"))))
    (build-system ruby-build-system)
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-yardstick" ,ruby-yardstick)
       ))
    (synopsis "Build and verify OAuth headers")
    (description
      "Simply builds and verifies OAuth headers")
    (home-page "https://github.com/laserlemon/simple_oauth")
    (license license:expat)))

(define-public ruby-naught
  (package
    (name "ruby-naught")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "naught" version))
        (sha256
         (base32
          "1wwjx35zgbc0nplp8a866iafk4zsrbhwwz4pav5gydr2wm26nksg"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (native-inputs
     `(("ruby-coveralls" ,ruby-coveralls)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis "Toolkit for building Null Objects")
    (description "Naught is a toolkit for building Null Objects.")
    (home-page "https://github.com/avdi/naught")
    (license license:expat)))

(define-public ruby-memoizable
  (package
    (name "ruby-memoizable")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "memoizable" version))
        (sha256
         (base32
          "0v42bvghsvfpzybfazl14qhkrjvx0xlmxz0wwqc960ga1wld5x5c"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-thread-safe" ,ruby-thread-safe)))
    (native-inputs
     `(("ruby-coveralls" ,ruby-coveralls)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis "Memoize method return values")
    (description "Memoize method return values")
    (home-page "https://github.com/dkubb/memoizable")
    (license license:expat)))

(define-public ruby-ffi-compiler
  (package
    (name "ruby-ffi-compiler")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ffi-compiler" version))
        (sha256
         (base32
          "0c2caqm9wqnbidcb8dj4wd3s902z15qmgxplwyfyqbwa0ydki7q1"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; LoadError: cannot load such file -- rubygems/tasks
    (propagated-inputs
     `(("ruby-ffi" ,ruby-ffi)
       ("ruby-rake" ,ruby-rake)))
    (synopsis "Ruby FFI library")
    (description "Ruby FFI library")
    (home-page "http://wiki.github.com/ffi/ffi")
    (license license:asl2.0)))

(define-public ruby-http-parser
  (package
    (name "ruby-http-parser")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "http-parser" version))
        (sha256
         (base32
          "18qqvckvqjffh88hfib6c8pl9qwk9gp89w89hl3f2s1x8hgyqka1"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda _
             (invoke "rake" "compile"))))))
    (propagated-inputs
     `(("ruby-ffi-compiler" ,ruby-ffi-compiler)))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis "Http parser for Ruby")
    (description
      "    A super fast http parser for ruby.
    Cross platform and multiple ruby implementation support thanks to ffi.
")
    (home-page "https://github.com/cotag/http-parser")
    (license license:expat)))

(define-public ruby-http-form-data
  (package
    (name "ruby-http-form-data")
    (version "2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "http-form_data" version))
        (sha256
         (base32
          "1wx591jdhy84901pklh1n9sgh74gnvq1qyqxwchni1yrc49ynknc"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (native-inputs
     `(("ruby-coveralls" ,ruby-coveralls)
       ("ruby-guard" ,ruby-guard)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis
      "Utility-belt to build form data request bodies. Provides support for `application/x-www-form-urlencoded` and `multipart/form-data` types.")
    (description
      "Utility-belt to build form data request bodies.  Provides support for `application/x-www-form-urlencoded` and `multipart/form-data` types.")
    (home-page "https://github.com/httprb/form_data.rb")
    (license license:expat)))

(define-public ruby-http
  (package
    (name "ruby-http")
    (version "4.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "http" version))
        (sha256
         (base32
          "0z8vmvnkrllkpzsxi94284di9r63g9v561a16an35izwak8g245y"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"
       ;; A bunch of tests require network access. Should we borrow Debain's patches?
       #:tests? #f))
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-http-cookie" ,ruby-http-cookie)
       ("ruby-http-form-data" ,ruby-http-form-data)
       ("ruby-http-parser" ,ruby-http-parser)))
    (native-inputs
     `(("ruby-certificate-authority" ,ruby-certificate-authority)
       ("ruby-coveralls" ,ruby-coveralls)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-its" ,ruby-rspec-its)
       ("ruby-rubocop" ,ruby-rubocop)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-yardstick" ,ruby-yardstick)))
    (synopsis "Client library for making requests from Ruby")
    (description "This package provides a client library for making requests
from Ruby.  It uses a simple method chaining system for building requests,
similar to Python's Requests.")
    (home-page "https://github.com/httprb/http")
    (license license:expat)))

(define-public ruby-equalizer
  (package
    (name "ruby-equalizer")
    (version "0.0.11")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "equalizer" version))
        (sha256
         (base32
          "1kjmx3fygx8njxfrwcmn7clfhjhb6bvv3scy2lyyi0wqyi3brra4"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Prevent cycle with ruby-devtools.
    (synopsis
      "Module to define equality, equivalence and inspection methods")
    (description
      "Module to define equality, equivalence and inspection methods")
    (home-page "https://github.com/dkubb/equalizer")
    (license license:expat)))

(define-public ruby-buftok
  (package
    (name "ruby-buftok")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "buftok" version))
        (sha256
         (base32
          "1rzsy1vy50v55x9z0nivf23y0r9jkmq6i130xa75pq9i8qrn1mxs"))))
    (build-system ruby-build-system)
    (synopsis
      "BufferedTokenizer extracts token delimited entities from a sequence of arbitrary inputs")
    (description
      "BufferedTokenizer extracts token delimited entities from a sequence of arbitrary inputs")
    (home-page "https://github.com/sferik/buftok")
    (license license:expat)))

(define-public ruby-twitter
  (package
    (name "ruby-twitter")
    (version "7.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sferik/twitter")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "057d3wg3850r7xyhb5xv8xgxv7qra31ic0m317mwi2n5w1p4n480"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-buftok" ,ruby-buftok)
       ("ruby-equalizer" ,ruby-equalizer)
       ("ruby-http" ,ruby-http)
       ("ruby-http-form-data" ,ruby-http-form-data)
       ("ruby-http-parser.rb" ,ruby-http-parser.rb)
       ("ruby-memoizable" ,ruby-memoizable)
       ("ruby-multipart-post" ,ruby-multipart-post)
       ("ruby-naught" ,ruby-naught)
       ("ruby-simple-oauth" ,ruby-simple-oauth)))
    (native-inputs
     `(("ruby-coveralls" ,ruby-coveralls)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-timecop" ,ruby-timecop)
       ("ruby-webmock" ,ruby-webmock)
       ("ruby-yard" ,ruby-yard)
       ("ruby-yardstick" ,ruby-yardstick)))
    (synopsis "Ruby interface to the Twitter API")
    (description "This package provides a Ruby interface to the Twitter API.")
    (home-page "https://sferik.github.io/twitter/")
    (license license:expat)))

(define-public ruby-yardstick
  (package
    (name "ruby-yardstick")
    (version "0.9.9")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "yardstick" version))
        (sha256
         (base32
          "0vn0br8x0n7b9i2raz79g480cn711zichs8rvijb3h1pk9m1d6n3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Prevent cycle with ruby-devtools.
    (propagated-inputs
     `(("ruby-yard" ,ruby-yard)))
    (synopsis "Measure YARD documentation coverage")
    (description
      "Measure YARD documentation coverage")
    (home-page "https://github.com/dkubb/yardstick")
    (license license:expat)))

(define-public ruby-psych
  (package
    (name "ruby-psych")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "psych" version))
        (sha256
         (base32
          "0r8rd9q4g6wda6k2bvsgpwnn9wbaqglb843bm4f1q6xfjkhs5h0l"))))
    (build-system ruby-build-system)
    (native-inputs
     `(
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ))
    (synopsis
      "Psych is a YAML parser and emitter. Psych leverages libyaml[https://pyyaml.org/wiki/LibYAML]
for its YAML parsing and emitting capabilities. In addition to wrapping libyaml,
Psych also knows how to serialize and de-serialize most Ruby objects to and from the YAML format.
")
    (description
      "Psych is a YAML parser and emitter.  Psych leverages libyaml[https://pyyaml.org/wiki/LibYAML]
for its YAML parsing and emitting capabilities.  In addition to wrapping libyaml,
Psych also knows how to serialize and de-serialize most Ruby objects to and from the YAML format.
")
    (home-page "https://github.com/ruby/psych")
    (license license:expat)))

(define-public ruby-kwalify
  (package
    (name "ruby-kwalify")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "kwalify" version))
        (sha256
         (base32
          "1ngxg3ysq5vip9dn3d32ajc7ly61kdin86hfycm1hkrcvkkn1vjf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; old package is old
    (synopsis
      "   Kwalify is a parser, schema validator, and data binding tool for YAML and JSON.
")
    (description
      "   Kwalify is a parser, schema validator, and data binding tool for YAML and JSON.
")
    (home-page "http://www.kuwata-lab.com/kwalify/")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-reek
  (package
    (name "ruby-reek")
    (version "6.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "reek" version))
        (sha256
         (base32
          "1zlfvj1dh064y119sfz9w3rkj3d9qkwm1k6dkcjymr6cwj6cqqp2"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; needs aruba ~> 1.0
    (propagated-inputs
     `(("ruby-kwalify" ,ruby-kwalify)
       ("ruby-parser" ,ruby-parser)
       ("ruby-psych" ,ruby-psych)
       ("ruby-rainbow" ,ruby-rainbow)))
    (native-inputs
     `(
       ("ruby-aruba" ,ruby-aruba)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "Reek is a tool that examines Ruby classes, modules and methods and reports any code smells it finds.")
    (description
      "Reek is a tool that examines Ruby classes, modules and methods and reports any code smells it finds.")
    (home-page "https://github.com/troessner/reek")
    (license license:expat)))

(define-public ruby-procto
  (package
    (name "ruby-procto")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "procto" version))
        (sha256
         (base32
          "13imvg1x50rz3r0yyfbhxwv72lbf7q28qx9l9nfbb91h2n9ch58c"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Break dependency cycle with ruby-devtools.
    (synopsis
      "Turns your object into a method object")
    (description
      "Turns your object into a method object")
    (home-page "https://github.com/snusnu/procto")
    (license license:expat)))

(define-public ruby-flog
  (package
    (name "ruby-flog")
    (version "4.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "flog" version))
        (sha256
         (base32
          "0qy7s5q450wbc78av8h0w8inrdz46vp4rqnm5ikpsnh7dilh7amm"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-path-expander" ,ruby-path-expander)
       ("ruby-ruby-parser" ,ruby-ruby-parser)
       ("ruby-sexp-processor" ,ruby-sexp-processor)))
    (native-inputs
     `(
       ("ruby-hoe" ,ruby-hoe)
       ))
    (synopsis
      "Flog reports the most tortured code in an easy to read pain
report. The higher the score, the more pain the code is in.")
    (description
      "Flog reports the most tortured code in an easy to read pain
report.  The higher the score, the more pain the code is in.")
    (home-page "http://ruby.sadi.st/")
    (license license:expat)))

(define-public ruby-path-expander
  (package
    (name "ruby-path-expander")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "path_expander" version))
        (sha256
         (base32
          "1l40n8i959c8bk5m9cfs4m75h2cq01wjwhahnkw7jxgicpva30gv"))))
    (build-system ruby-build-system)
    (native-inputs
     `(
       ("ruby-hoe" ,ruby-hoe)
       ))
    (synopsis
      "PathExpander helps pre-process command-line arguments expanding
directories into their constituent files. It further helps by
providing additional mechanisms to make specifying subsets easier
with path subtraction and allowing for command-line arguments to be
saved in a file.

NOTE: this is NOT an options processor. It is a path processor
(basically everything else besides options). It does provide a
mechanism for pre-filtering cmdline options, but not with the intent
of actually processing them in PathExpander. Use OptionParser to
deal with options either before or after passing ARGV through
PathExpander.")
    (description
      "PathExpander helps pre-process command-line arguments expanding
directories into their constituent files.  It further helps by
providing additional mechanisms to make specifying subsets easier
with path subtraction and allowing for command-line arguments to be
saved in a file.

NOTE: this is NOT an options processor.  It is a path processor
(basically everything else besides options).  It does provide a
mechanism for pre-filtering cmdline options, but not with the intent
of actually processing them in PathExpander.  Use OptionParser to
deal with options either before or after passing ARGV through
PathExpander.")
    (home-page "https://github.com/seattlerb/path_expander")
    (license license:expat)))

(define-public ruby-flay
  (package
    (name "ruby-flay")
    (version "2.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "flay" version))
        (sha256
         (base32
          "1my4ga8a8wsqb4nqbf31gvml64ngr66r0zim4mx2kvi76zygczv7"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; TODO: fix! only 1 failing test!
    (propagated-inputs
     `(("ruby-erubis" ,ruby-erubis)
       ("ruby-path-expander" ,ruby-path-expander)
       ("ruby-ruby-parser" ,ruby-ruby-parser)
       ("ruby-sexp-processor" ,ruby-sexp-processor)))
    (native-inputs
     `(
       ("ruby-hoe" ,ruby-hoe)
       ))
    (synopsis
      "Flay analyzes code for structural similarities. Differences in literal
values, variable, class, method names, whitespace, programming style,
braces vs do/end, etc are all ignored. Making this totally rad.")
    (description
      "Flay analyzes code for structural similarities.  Differences in literal
values, variable, class, method names, whitespace, programming style,
braces vs do/end, etc are all ignored.  Making this totally rad.")
    (home-page "http://ruby.sadi.st/")
    (license license:expat)))

(define-public ruby-concord
  (package
    (name "ruby-concord")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "concord" version))
        (sha256
         (base32
          "1vznyzcd3z7wiwjfgr941nq405kd7zm5vjb3sv2mzbbrcla9qkhq"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Break dependency cycle with ruby-devtools.
    (propagated-inputs
     `(("ruby-adamantium" ,ruby-adamantium)
       ("ruby-equalizer" ,ruby-equalizer)))
    (synopsis "Helper for object composition")
    (description "Helper for object composition")
    (home-page "https://github.com/mbj/concord")
    (license license:expat)))

(define-public ruby-anima
  (package
    (name "ruby-anima")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "anima" version))
        (sha256
         (base32
          "007wrc8px9ql4nqp34w0ffb9nj2nrbrcxvy036ng28bpbig7fzs6"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Break dependency cycle with ruby-devtools.
    (propagated-inputs
     `(("ruby-abstract-type" ,ruby-abstract-type)
       ("ruby-adamantium" ,ruby-adamantium)
       ("ruby-equalizer" ,ruby-equalizer)))
    (synopsis
      "Initialize object attributes via attributes hash")
    (description
      "Initialize object attributes via attributes hash")
    (home-page "http://github.com/mbj/anima")
    (license license:expat)))

(define-public ruby-ice-nine
  (package
    (name "ruby-ice-nine")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ice_nine" version))
        (sha256
         (base32
          "1nv35qg1rps9fsis28hz2cq2fx1i96795f91q4nmkm934xynll2x"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Break dependency cycle with ruby-devtools.
    (synopsis "Deep Freeze Ruby Objects")
    (description "Deep Freeze Ruby Objects")
    (home-page "https://github.com/dkubb/ice_nine")
    (license license:expat)))

(define-public ruby-adamantium
  (package
    (name "ruby-adamantium")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "adamantium" version))
        (sha256
         (base32
          "0165r2ikgfwv2rm8dzyijkp74fvg0ni72hpdx8ay2v7cj08dqyak"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Break dependency cycle with ruby-devtools.
    (propagated-inputs
     `(("ruby-ice-nine" ,ruby-ice-nine)
       ("ruby-memoizable" ,ruby-memoizable)))
    (synopsis "Immutable extensions to objects")
    (description "Immutable extensions to objects")
    (home-page "https://github.com/dkubb/adamantium")
    (license license:expat)))

(define-public ruby-abstract-type
  (package
    (name "ruby-abstract-type")
    (version "0.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "abstract_type" version))
        (sha256
         (base32
          "09330cmhrc2wmfhdj9zzg82sv6cdhm3qgdkva5ni5xfjril2pf14"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Break dependency cycle with ruby-devtools.
    (synopsis
      "Module to declare abstract classes and methods")
    (description
      "Module to declare abstract classes and methods")
    (home-page "https://github.com/dkubb/abstract_type")
    (license license:expat)))

;; TODO: This package somehow hardcodes the versions of its dependencies
(define-public ruby-devtools
  (package
    (name "ruby-devtools")
    (version "0.1.26")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "devtools" version))
        (sha256
         (base32
          "08c8j2zcq9hhxpz9wsyy9v8mfs4d4smyagi0qr398w1qryb6w4m0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; Devtools requires devtools for the tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; Don't require self
             (substitute* "Rakefile"
               (("require 'devtools'") ""))
             ;; Don't hardcode version dependencies
             (substitute* "devtools.gemspec"
               (("',\\s+'.*") "'\n"))
             #t))
         (add-after 'build 'unpin-versions
           (lambda _
             ;; Don't hardcode version dependencies
             (substitute* "devtools.gemspec"
               ((", \\[\\\"~>.*") ")\n"))
             #t))
         )
       ))
    (propagated-inputs
     `(("ruby-abstract-type" ,ruby-abstract-type)
       ("ruby-adamantium" ,ruby-adamantium)
       ("ruby-anima" ,ruby-anima)
       ("ruby-concord" ,ruby-concord)
       ("ruby-flay" ,ruby-flay)
       ("ruby-flog" ,ruby-flog)
       ("ruby-procto" ,ruby-procto)
       ("ruby-rake" ,ruby-rake)
       ("ruby-reek" ,ruby-reek)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-rspec-its" ,ruby-rspec-its)
       ("ruby-rubocop" ,ruby-rubocop)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-yard" ,ruby-yard)
       ("ruby-yardstick" ,ruby-yardstick)))
    (synopsis "A metagem wrapping development tools")
    (description
      "This package provides a metagem wrapping development tools")
    (home-page "https://github.com/rom-rb/devtools")
    (license license:expat)))

(define-public ruby-certificate-authority
  (package
    (name "ruby-certificate-authority")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "certificate_authority" version))
        (sha256
         (base32
          "1d4j37i40l76pdkxx9964f58d83fjv82x3c0sykrpiixcmjcax44"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       ;; An error occurred while loading spec_helper.
       ;; Failure/Error: return gem_original_require(path)
       #:tests? #f))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)
       ))
    (synopsis
      "Ruby gem for managing the core functions outlined in RFC-3280 for PKI")
    (description
      "Ruby gem for managing the core functions outlined in RFC-3280 for PKI")
    (home-page "https://github.com/cchandler/certificate_authority")
    (license license:expat)))

(define-public ruby-msgpack
  (package
    (name "ruby-msgpack")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "msgpack" version))
        (sha256
         (base32
          "06iajjyhx0rvpn4yr3h1hc4w4w3k59bdmfhxnjzzh76wsrdxxrc6"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-fake-gem
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out") "/lib/ruby/vendor_ruby/cache/./pkg/msgpack-" ,version ".gem")))
               (mkdir-p (dirname target))
               (symlink
                        (string-append "../../../msgpack-" ,version ".gem")
                 target
                        )
               #t)))
         )
       ))
    (native-inputs
     `(
       ("ruby-rake" ,ruby-rake)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-yard" ,ruby-yard)
       ))
    (synopsis
      "MessagePack is a binary-based efficient object serialization library. It enables to exchange structured objects between many languages like JSON. But unlike JSON, it is very fast and small.")
    (description
      "MessagePack is a binary-based efficient object serialization library.  It enables to exchange structured objects between many languages like JSON.  But unlike JSON, it is very fast and small.")
    (home-page "http://msgpack.org/")
    (license license:asl2.0)))

(define-public ruby-bootsnap
  (package
    (name "ruby-bootsnap")
    (version "1.7.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Shopify/bootsnap")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "14l0r074kpy9pwzs6zbgq3zpx32mpk4905k23v0znqgkmrb6s5bm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; can't find rake-compiler
       #:phases
       (modify-phases %standard-phases
         (replace 'replace-git-ls-files
           (lambda _
             (substitute* "bootsnap.gemspec"
               (("git ls-files -z ext lib")
                "find ext lib -type f -print0 | sort -z"))
             #t)))))
    (propagated-inputs
     `(("ruby-msgpack" ,ruby-msgpack)))
    (native-inputs
     `(("ruby-minitest" ,ruby-minitest)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-rake" ,ruby-rake)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis "Boot large ruby/rails apps faster")
    (description "Boot large ruby/rails apps faster")
    (home-page "https://github.com/Shopify/bootsnap")
    (license license:expat)))

(define-public ruby-actionview-precompiler
  (package
    (name "ruby-actionview-precompiler")
    (version "0.2.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jhawthorn/actionview_precompiler")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0hyvzhyx3bmvnmmj247vyfznps835d0zmi3xb6y6s4v570d8mrf0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; tests expect ruby-actionview >=6.0.a
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "actionview_precompiler.gemspec"
               (("actionview\\\".*") "actionview\"\n")
               )
             #t))
         (add-before 'build 'delete-gemfile-lock
           (lambda _
             (delete-file "Gemfile.lock")
             #t)))))
    (propagated-inputs
     `(("ruby-actionview" ,ruby-actionview)))
    (native-inputs
     `(("ruby-minitest" ,ruby-minitest)
       ("ruby-pry" ,ruby-pry)))
    (synopsis
      "Parses templates for render calls and uses them to precompile")
    (description
      "Parses templates for render calls and uses them to precompile")
    (home-page "https://github.com/jhawthorn/actionview_precompiler")
    (license license:expat)))

(define-public ruby-rocco
  (package
    (name "ruby-rocco")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rocco" version))
        (sha256
         (base32
          "0z3wnk8848wphrzyb61adl1jbfjlsqnzkayp2m0qmisg566352l1"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; needs pygments
    (propagated-inputs
     `(("ruby-mustache" ,ruby-mustache)
       ;; pygments
       ("ruby-redcarpet" ,ruby-redcarpet)))
    (synopsis "Docco in Ruby")
    (description "Docco in Ruby")
    (home-page "https://rtomayko.github.com/rocco/")
    (license license:expat)))

(define-public ruby-bundler
  (package
    (name "ruby-bundler")
    (version "2.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "bundler" version))
        (sha256
         (base32
          "1izx6wsjdm6mnbxazgz1z5qbhwrrisbq0np2nmx4ij6lrqjy18jf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile
    (synopsis
      "Bundler manages an application's dependencies through its entire life, across many machines, systematically and repeatably")
    (description
      "Bundler manages an application's dependencies through its entire life, across many machines, systematically and repeatably")
    (home-page "https://bundler.io/")
    (license license:expat)))

(define-public ruby-appraisal
  (package
    (name "ruby-appraisal")
    (version "2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "appraisal" version))
        (sha256
         (base32
          "0j092f2zfgb6afimidgspzqg4iw6n4mrs2zp8hhs2m2giav4mkrn"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Tests require activesupport, cycle with ruby-m
    (propagated-inputs
     `(("bundler" ,bundler)
       ("ruby-rake" ,ruby-rake)
       ("ruby-thor" ,ruby-thor)))
    (synopsis
      "Appraisal integrates with bundler and rake to test your library against different versions of dependencies in repeatable scenarios called \"appraisals.\"")
    (description
      "Appraisal integrates with bundler and rake to test your library against different versions of dependencies in repeatable scenarios called \"appraisals.\"")
    (home-page
      "http://github.com/thoughtbot/appraisal")
    (license license:expat)))

(define-public ruby-http-accept-language
  (package
    (name "ruby-http-accept-language")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "http_accept_language" version))
        (sha256
         (base32
          "0d0nlfz9vm4jr1l6q0chx4rp2hrnrfbx3gadc1dz930lbbaz0hq0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             (substitute* "cucumber.yml"
               (("~@") "'not @'"))
             #t)))))
    (native-inputs
     `(
       ("ruby-aruba" ,ruby-aruba)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-guard-rspec" ,ruby-guard-rspec)
       ("ruby-rack" ,ruby-rack)
       ("ruby-rack-test" ,ruby-rack-test)
       ("ruby-rails" ,ruby-rails)
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "Find out which locale the user preferes by reading the languages they specified in their browser")
    (description
      "Find out which locale the user preferes by reading the languages they specified in their browser")
    (home-page "https://github.com/iain/http_accept_language")
    (license license:expat)))

(define-public ruby-guard-compat
  (package
    (name "ruby-guard-compat")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "guard-compat" version))
        (sha256
         (base32
          "1zj6sr1k8w59mmi27rsii0v8xyy2rnsi09nqvwpgj1q10yq1mlis"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec" ; not default, tests not upgraded for newer rubocop versions.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)
       ))
    (synopsis
      "Helps creating valid Guard plugins and testing them")
    (description
      "Helps creating valid Guard plugins and testing them")
    (home-page "https://github.com/guard/guard-compat")
    (license license:expat)))

(define-public ruby-guard-rspec
  (package
    (name "ruby-guard-rspec")
    (version "4.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "guard-rspec" version))
        (sha256
         (base32
          "1jkm5xp90gm4c5s51pmf92i9hc10gslwwic6mvk72g0yplya0yx4"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Tests not included in release.
    (propagated-inputs
     `(("ruby-guard" ,ruby-guard)
       ("ruby-guard-compat" ,ruby-guard-compat)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
      "Guard::RSpec automatically run your specs (much like autotest).")
    (description
      "Guard::RSpec automatically run your specs (much like autotest).")
    (home-page "https://github.com/guard/guard-rspec")
    (license license:expat)))

(define-public ruby-aws-sdk-kms
  (package
    (name "ruby-aws-sdk-kms")
    (version "1.42.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "aws-sdk-kms" version))
        (sha256
         (base32
          "00wgf83cdy6z77b2y0ld0aqiidfyldi71hx0z8b73gxjdlbwpq1i"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (propagated-inputs
     `(("ruby-aws-sdk-core" ,ruby-aws-sdk-core)
       ("ruby-aws-sigv4" ,ruby-aws-sigv4)))
    (synopsis
      "Official AWS Ruby gem for AWS Key Management Service (KMS). This gem is part of the AWS SDK for Ruby.")
    (description
      "Official AWS Ruby gem for AWS Key Management Service (KMS).  This gem is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-jmespath
  (package
    (name "ruby-jmespath")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "jmespath" version))
        (sha256
         (base32
          "1d4wac0dcd1jf6kc57891glih9w57552zgqswgy74d1xhgnk0ngf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; tests not included
    (synopsis "Implements JMESPath for Ruby")
    (description "Implements JMESPath for Ruby")
    (home-page "https://github.com/trevorrowe/jmespath.rb")
    (license license:asl2.0)))

(define-public ruby-aws-sigv4
  (package
    (name "ruby-aws-sigv4")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "aws-sigv4" version))
        (sha256
         (base32
          "1ll9382c1x2hp750cilh01h1cycgyhdr4cmmgx23k94hyyb8chv5"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (propagated-inputs
     `(("ruby-aws-eventstream" ,ruby-aws-eventstream)))
    (synopsis
      "Amazon Web Services Signature Version 4 signing library. Generates sigv4 signature for HTTP requests.")
    (description
      "Amazon Web Services Signature Version 4 signing library.  Generates sigv4 signature for HTTP requests.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-partitions
  (package
    (name "ruby-aws-partitions")
    (version "1.428.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "aws-partitions" version))
        (sha256
         (base32
          "13rvpllihvpksf1jqwa2i5vbv2hhb34viaidw4rkxr3dyygkdpj8"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (synopsis
      "Provides interfaces to enumerate AWS partitions, regions, and services.")
    (description
      "This package provides interfaces to enumerate AWS partitions, regions, and services.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-eventstream
  (package
    (name "ruby-aws-eventstream")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "aws-eventstream" version))
        (sha256
         (base32
          "0r0pn66yqrdkrfdin7qdim0yj2x75miyg4wp6mijckhzhrjb7cv5"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (synopsis
      "Amazon Web Services event stream library. Decodes and encodes binary stream under `vnd.amazon.event-stream` content-type")
    (description
      "Amazon Web Services event stream library.  Decodes and encodes binary stream under `vnd.amazon.event-stream` content-type")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-sdk-core
  (package
    (name "ruby-aws-sdk-core")
    (version "3.112.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "aws-sdk-core" version))
        (sha256
         (base32
          "15lynby6r91p9hh5h92pg4jr8xgnjr52px5ax0p0wncdw4vz0skp"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (propagated-inputs
     `(("ruby-aws-eventstream" ,ruby-aws-eventstream)
       ("ruby-aws-partitions" ,ruby-aws-partitions)
       ("ruby-aws-sigv4" ,ruby-aws-sigv4)
       ("ruby-jmespath" ,ruby-jmespath)))
    (synopsis
      "Provides API clients for AWS. This gem is part of the official AWS SDK for Ruby.")
    (description
      "This package provides API clients for AWS.  This gem is part of the official AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-sdk-s3
  (package
    (name "ruby-aws-sdk-s3")
    (version "1.88.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "aws-sdk-s3" version))
        (sha256
         (base32
          "01zlv2icx3m0pq94z9fcsp1r9ivdqhfpnpbrv63fpr6m7yqww24y"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (propagated-inputs
     `(("ruby-aws-sdk-core" ,ruby-aws-sdk-core)
       ("ruby-aws-sdk-kms" ,ruby-aws-sdk-kms)
       ("ruby-aws-sigv4" ,ruby-aws-sigv4)))
    (synopsis
      "Official AWS Ruby gem for Amazon Simple Storage Service (Amazon S3). This gem is part of the AWS SDK for Ruby.")
    (description
      "Official AWS Ruby gem for Amazon Simple Storage Service (Amazon S3).  This gem is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-sdk-sns
  (package
    (name "ruby-aws-sdk-sns")
    (version "1.38.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "aws-sdk-sns" version))
        (sha256
         (base32
          "0cqri14igfmcxlapbagg0nmy79zzg29awzybv51gl76m3mljbafb"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (propagated-inputs
     `(("ruby-aws-sdk-core" ,ruby-aws-sdk-core)
       ("ruby-aws-sigv4" ,ruby-aws-sigv4)))
    (synopsis
      "Official AWS Ruby gem for Amazon Simple Notification Service (Amazon SNS). This gem is part of the AWS SDK for Ruby.")
    (description
      "Official AWS Ruby gem for Amazon Simple Notification Service (Amazon SNS).  This gem is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-excon
  (package
    (name "ruby-excon")
    (version "0.79.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/excon/excon")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0cm3rpkzdmq78ni7irw449qrya8wgb6hfzxjwkyq4pkalc21afqb"))))
    (build-system ruby-build-system)
    (arguments
     `(
       #:phases
       (modify-phases %standard-phases
         (replace 'replace-git-ls-files
           (lambda _
             (substitute* "excon.gemspec"
               (("git ls-files -- data/\\* lib/\\*")
                "find data lib -type f"))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Don't demand the latest ruby-rack
             (substitute* "Gemfile"
               (("rack.*") "rack'\n"))
             ;; No network connection inside the container.
             (delete-file "tests/basic_tests.rb")
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" (getcwd))
               (invoke "bundle" "exec" "shindont")
               (invoke "bundle" "exec" "rake" "spec[progress]"))
             #t))
         )))
    (native-inputs
     `(
       ("ruby-activesupport" ,ruby-activesupport)
       ("ruby-delorean" ,ruby-delorean)
       ("ruby-eventmachine" ,ruby-eventmachine)
       ("ruby-open4" ,ruby-open4)
       ("ruby-puma" ,ruby-puma)
       ("ruby-rack" ,ruby-rack)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)
       ("ruby-shindo" ,ruby-shindo)
       ("ruby-sinatra" ,ruby-sinatra)
       ("ruby-sinatra-contrib" ,ruby-sinatra-contrib)
       ("ruby-unicorn" ,ruby-unicorn)
       ))
    (synopsis "EXtended http(s) CONnections")
    (description "EXtended http(s) CONnections")
    (home-page "https://github.com/excon/excon")
    (license license:expat)))

(define-public ruby-chronic
  (package
    (name "ruby-chronic")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "chronic" version))
        (sha256
         (base32
          "1hrdkn4g8x7dlzxwb1rfgr8kw3bp4ywg5l4y4i9c2g5cwv62yvvn"))))
    (build-system ruby-build-system)
    (synopsis
      "Chronic is a natural language date/time parser written in pure Ruby.")
    (description
      "Chronic is a natural language date/time parser written in pure Ruby.")
    (home-page "http://github.com/mojombo/chronic")
    (license license:expat)))

(define-public ruby-delorean
  (package
    (name "ruby-delorean")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "delorean" version))
        (sha256
         (base32
          "0k39ix0a9rf4fd05ncml4h9r29dzwgzdbhp01gp67baid6adxwf4"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; tests not included
    (propagated-inputs
     `(("ruby-chronic" ,ruby-chronic)))
    (synopsis
      "Delorean lets you travel in time with Ruby by mocking Time.now")
    (description
      "Delorean lets you travel in time with Ruby by mocking Time.now")
    (home-page "http://github.com/bebanjo/delorean")
    (license license:expat)))

(define-public ruby-sinatra-contrib
  (package
    (name "ruby-sinatra-contrib")
    (version "2.0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "sinatra-contrib" version))
        (sha256
         (base32
          "1mmrfm4pqh98f3irjpkvfpazhcx6q42bnx6bbms9dqvmck3mid28"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-backports" ,ruby-backports)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-mustermann" ,ruby-mustermann)
       ("ruby-rack-protection" ,ruby-rack-protection)
       ("ruby-sinatra" ,ruby-sinatra)
       ("ruby-tilt" ,ruby-tilt)))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis "Collection of useful Sinatra extensions")
    (description
      "Collection of useful Sinatra extensions")
    (home-page "http://sinatrarb.com/contrib/")
    (license license:expat)))

(define-public ruby-raindrops
  (package
    (name "ruby-raindrops")
    (version "0.19.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "raindrops" version))
        (sha256
         (base32
          "0zjja00mzgx2lddb7qrn14k7qrnwhf4bpmnlqj78m1pfxh7svync"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; not clear how to run tests
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "ruby" "setup.rb" "tests"))
             #t)))))
    (synopsis "real-time stats for preforking Rack servers")
    (description
      "raindrops is a real-time stats toolkit to show statistics for Rack HTTP
servers.  It is designed for preforking servers such as unicorn, but
should support any Rack HTTP server on platforms supporting POSIX shared
memory.  It may also be used as a generic scoreboard for sharing atomic
counters across multiple processes.")
    (home-page "https://yhbt.net/raindrops/")
    (license license:lgpl2.1+)))

(define-public ruby-kgio
  (package
    (name "ruby-kgio")
    (version "2.11.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "kgio" version))
        (sha256
         (base32
          "0ai6bzlvxbzpdl466p1qi4dlhx8ri2wcrp6x1l19y3yfs3a29rng"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; not clear how to run tests
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "ruby" "setup.rb" "tests"))
             #t)))))
    (synopsis "kinder, gentler I/O for Ruby")
    (description
      "This is a legacy project, do not use it for new projects.  Ruby
2.3 and later should make this obsolete.  kgio provides
non-blocking I/O methods for Ruby without raising exceptions on
EAGAIN and EINPROGRESS.
")
    (home-page "https://yhbt.net/kgio/")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-unicorn
  (package
    (name "ruby-unicorn")
    (version "5.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "unicorn" version))
        (sha256
         (base32
          "0ig48f4xhrssq5d11vkc41k7nj6pbv2jh1f8k5gfskfd469mcc2y"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-kgio" ,ruby-kgio)
       ("ruby-raindrops" ,ruby-raindrops)))
    (synopsis
      "unicorn is an HTTP server for Rack applications designed to only serve
fast clients on low-latency, high-bandwidth connections and take
advantage of features in Unix/Unix-like kernels.  Slow clients should
only be served by placing a reverse proxy capable of fully buffering
both the the request and response in between unicorn and slow clients.")
    (description
      "unicorn is an HTTP server for Rack applications designed to only serve
fast clients on low-latency, high-bandwidth connections and take
advantage of features in Unix/Unix-like kernels.  Slow clients should
only be served by placing a reverse proxy capable of fully buffering
both the the request and response in between unicorn and slow clients.")
    (home-page "https://yhbt.net/unicorn/")
    (license (list (license:non-copyleft "will fill in later")
                   (license:non-copyleft "will fill in later")))))

(define-public ruby-pry-rails
  (package
    (name "ruby-pry-rails")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "pry-rails" version))
        (sha256
         (base32
          "1cf4ii53w2hdh7fn8vhqpzkymmchjbwij4l3m7s6fsxvb9bn51j6"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f      ; not clear what the test suite wants
       #:test-target "appraisal"    ; as per the Rakefile
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (propagated-inputs
     `(("ruby-pry" ,ruby-pry)))
    (native-inputs
     `(
       ("ruby-appraisal" ,ruby-appraisal)
       ("ruby-minitest" ,ruby-minitest)
       ))
    (synopsis "Use Pry as your rails console")
    (description "Use Pry as your rails console")
    (home-page "https://github.com/rweng/pry-rails")
    (license license:expat)))

(define-public ruby-r2
  (package
    (name "ruby-r2")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "r2" version))
        (sha256
         (base32
          "0wk0p55zp3l96xy5ps28b33dn5z0jwsjl74bwfdn6z81pzjs5sfk"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "CSS flipper for right-to-left processing. A Ruby port of https://github.com/ded/r2")
    (description
      "CSS flipper for right-to-left processing.  A Ruby port of https://github.com/ded/r2")
    (home-page "https://github.com/mzsanford/R2rb")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-openssl
  (package
    (name "ruby-openssl")
    (version "2.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ruby/openssl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "01xigwxpwha9hj8r2synkl6c7xlhs02f27qv98a1b7cbhgqqs6n3"))))
    (build-system ruby-build-system)
    (inputs
     `(
       ("openssl" ,openssl)
       ))
    (native-inputs
     `(
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ))
    (synopsis "It wraps the OpenSSL library.")
    (description "It wraps the OpenSSL library.")
    (home-page "https://github.com/ruby/openssl")
    (license license:ruby)))

(define-public ruby-openssl-signature-algorithm
  (package
    (name "ruby-openssl-signature-algorithm")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cedarcode/openssl-signature_algorithm")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ilshdmky1z8azm7szzbg129v5g2n54izzckhyqwnn1g8c55bmn5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; cannot load such file -- spec_helper
       #:test-target "spec" ; not default, don't care about rubocop output
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (propagated-inputs
     `(("ruby-openssl" ,ruby-openssl)))
    (native-inputs
     `(("ruby-ed25519" ,ruby-ed25519)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)))
    (synopsis
      "ECDSA, EdDSA, RSA-PSS and RSA-PKCS#1 algorithms for ruby")
    (description
      "ECDSA, EdDSA, RSA-PSS and RSA-PKCS#1 algorithms for ruby")
    (home-page "https://github.com/cedarcode/openssl-signature_algorithm")
    (license license:asl2.0)))

(define-public ruby-ed25519
  (package
    (name "ruby-ed25519")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ed25519" version))
        (sha256
          (base32
            "1f5kr8za7hvla38fc0n9jiv55iq62k5bzclsa5kdb14l3r4w6qnw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f      ; cannot load spec_helper
       #:test-target "default"))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)))
    (synopsis "Ruby binding to the Ed25519 elliptic curve public-key system")
    (description
     "This package provides a Ruby binding to the Ed25519 elliptic curve
public-key signature system described in RFC 8032.")
    (home-page "https://github.com/crypto-rb/ed25519")
    (license license:expat)))

(define-public ruby-cose
  (package
    (name "ruby-cose")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "cose" version))
        (sha256
         (base32
          "1gx239d2fracq9az74wfdwmp5zm7zpzkcgchwnv2ng33d8r33p3m"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; cannot load such file -- spec_helper
       #:test-target "spec" ; not default, don't care about rubocop output
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (propagated-inputs
     `(("ruby-cbor" ,ruby-cbor)
       ("ruby-openssl-signature-algorithm" ,ruby-openssl-signature-algorithm)))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)
       ("ruby-rubocop-performance" ,ruby-rubocop-performance)
       ))
    (synopsis
      "Ruby implementation of RFC 8152 CBOR Object Signing and Encryption (COSE)")
    (description
      "Ruby implementation of RFC 8152 CBOR Object Signing and Encryption (COSE)")
    (home-page "https://github.com/cedarcode/cose-ruby")
    (license license:expat)))

(define-public ruby-rtlit
  (package
    (name "ruby-rtlit")
    (version "0.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rtlit" version))
        (sha256
         (base32
          "0srfh7cl95srjiwbyc9pmn3w739zlvyj89hyj0bm7g92zrsd27qm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; tests don't want to run
    (native-inputs
     `(
       ("ruby-rake", ruby-rake)
       ))
    (synopsis
      "Converts CSS files from left-to-right to right-to-left")
    (description
      "Converts CSS files from left-to-right to right-to-left")
    (home-page "https://github.com/zohararad/rtlit")
    (license license:expat)))

(define-public ruby-test-prof
  (package
    (name "ruby-test-prof")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/test-prof/test-prof")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "092q3m56843yrd94xby2a29vg8kca4lirgvqlkmdjyjckmi3qdzr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; seems to need running database
       #:test-target "spec" ; not default, don't care about rubocop output
       ;; cannot load such file -- standard/cop/semantic_blocks
       ))
    (native-inputs
     `(
       ("ruby-activerecord" ,ruby-activerecord)
       ("ruby-fabrication" ,ruby-fabrication)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)
       ("ruby-sqlite3" ,ruby-sqlite3-1.3)
       ))
    (synopsis
      "
    Ruby applications tests profiling tools.

    Contains tools to analyze factories usage, integrate with Ruby profilers,
    profile your examples using ActiveSupport notifications (if any) and
    statically analyze your code with custom RuboCop cops.
  ")
    (description
      "
    Ruby applications tests profiling tools.

    Contains tools to analyze factories usage, integrate with Ruby profilers,
    profile your examples using ActiveSupport notifications (if any) and
    statically analyze your code with custom RuboCop cops.
  ")
    (home-page "https://test-prof.evilmartians.io/")
    (license license:expat)))

(define-public ruby-fabrication
  (package
    (name "ruby-fabrication")
    (version "2.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "fabrication" version))
        (sha256
         (base32
          "1pdrl55xf76pbc5kjzp7diawxxvgbk2cm38532in6df823431n6z"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; TODO later
    (synopsis
      "Fabrication is an object generation framework for ActiveRecord, Mongoid, DataMapper, Sequel, or any other Ruby object.")
    (description
      "Fabrication is an object generation framework for ActiveRecord, Mongoid, DataMapper, Sequel, or any other Ruby object.")
    (home-page "http://fabricationgem.org")
    (license license:expat)))

(define-public ruby-ruby2-keywords
  (package
    (name "ruby-ruby2-keywords")
    (version "0.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ruby/ruby2_keywords")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1fz81nw92cvpi1h99q1pvsnkdkpmp40jvvpkn1jnjbx9by04bw45"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (synopsis
      "Shim library for Module#ruby2_keywords")
    (description
      "Shim library for Module#ruby2_keywords")
    (home-page "https://github.com/ruby/ruby2_keywords")
    (license (list license:ruby license:bsd-2))))

(define-public ruby-mock-redis
  (package
    (name "ruby-mock-redis")
    (version "0.27.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mock_redis" version))
        (sha256
         (base32
          "0fhwhp0w2n79k9ibmqhq09m88rp2jmr7dknx9ibn84wf7r8a3a8k"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; tests need a running redis server.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (propagated-inputs
     `(("ruby-ruby2-keywords" ,ruby-ruby2-keywords)))
    (native-inputs
     `(
       ("ruby-redis" ,ruby-redis)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-its" ,ruby-rspec-its)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-timecop" ,ruby-timecop)
       ))
    (synopsis
      "Instantiate one with `redis = MockRedis.new` and treat it like you would a normal Redis object. It supports all the usual Redis operations.")
    (description
      "Instantiate one with `redis = MockRedis.new` and treat it like you would a normal Redis object.  It supports all the usual Redis operations.")
    (home-page "https://github.com/sds/mock_redis")
    (license license:expat)))

(define-public ruby-certified
  (package
    (name "ruby-certified")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "certified" version))
        (sha256
         (base32
          "1706p6p0a8adyvd943af2a3093xakvislgffw3v9dvp7j07dyk5a"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile or tests
    (synopsis
      "Ensure net/https uses OpenSSL::SSL::VERIFY_PEER to verify SSL certificates and provides certificate bundle in case OpenSSL cannot find one")
    (description
      "Ensure net/https uses OpenSSL::SSL::VERIFY_PEER to verify SSL certificates and provides certificate bundle in case OpenSSL cannot find one")
    (home-page "https://github.com/stevegraham/certified")
    (license license:expat)))

(define-public ruby-rspec-html-matchers
  (package
    (name "ruby-rspec-html-matchers")
    (version "0.9.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rspec-html-matchers" version))
        (sha256
         (base32
          "0883rqv77n2wawnk5lp3la48l7pckyz8l013qddngzmksi5p1v3f"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no tests in gem release
    (propagated-inputs
     `(("ruby-nokogiri" ,ruby-nokogiri)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
      "Nokogiri based 'have_tag' and 'with_tag' matchers for rspec 3. Does not depend on assert_select matcher, provides useful error messages.
")
    (description
      "Nokogiri based 'have_tag' and 'with_tag' matchers for rspec 3.  Does not depend on assert_select matcher, provides useful error messages.
")
    (home-page "https://github.com/kucaahbe/rspec-html-matchers")
    (license license:expat)))

(define-public ruby-rswag-specs
  (package
    (name "ruby-rswag-specs")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rswag-specs" version))
        (sha256
         (base32
          "1dma3j5vfjhyclg8y0gsp44vs4wn9chf4jgfhc9r6ws018xrbxzd"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no tests in Rakefile
    (propagated-inputs
     `(("ruby-activesupport" ,ruby-activesupport)
       ("ruby-json-schema" ,ruby-json-schema)
       ("ruby-railties" ,ruby-railties)))
    (synopsis
      "Simplify API integration testing with a succinct rspec DSL and generate OpenAPI specification files directly from your rspecs. More about the OpenAPI initiative here: http://spec.openapis.org/")
    (description
      "Simplify API integration testing with a succinct rspec DSL and generate OpenAPI specification files directly from your rspecs.  More about the OpenAPI initiative here: http://spec.openapis.org/")
    (home-page "https://github.com/rswag/rswag")
    (license license:expat)))

(define-public ruby-uniform-notifier
  (package
    (name "ruby-uniform-notifier")
    (version "1.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "uniform_notifier" version))
        (sha256
         (base32
          "05s2y76zqg391q65mvs1zsrmyn5bjxq3x54ikkqpqm3lzjjcg1pp"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; not clear how to successfully run tests
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis "Uniform notifier for rails logger")
    (description
     "@code{uniform_notifier} is extracted from bullet, it gives you the ability to send notification through rails logger, customized logger, javascript alert, javascript console, growl, xmpp, airbrake and honeybadger.")
    (home-page "https://rubygems.org/gems/uniform_notifier")
    (license license:expat)))

(define-public ruby-bullet
  (package
    (name "ruby-bullet")
    (version "6.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "bullet" version))
        (sha256
         (base32
          "04wm807czdixpgnqp446vj8vc7dj96k26p90rmwll9ahlib37mmm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; tests want rails' source directory
       #:test-target "default"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (propagated-inputs
     `(("ruby-activesupport" ,ruby-activesupport)
       ("ruby-uniform-notifier" ,ruby-uniform-notifier)))
    (synopsis
      "help to kill N+1 queries and unused eager loading.")
    (description
      "help to kill N+1 queries and unused eager loading.")
    (home-page "https://github.com/flyerhzm/bullet")
    (license license:expat)))

(define-public ruby-better-errors
  (package
    (name "ruby-better-errors")
    (version "2.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/BetterErrors/better_errors")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "185im2i62j9f3sa0xzq8q806xlckypjmi3lwdivrzln4gw2l4z38"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Don't specify version numbers.
             (substitute* "better_errors.gemspec"
               (("rake.*") "rake\"\n"))
             (substitute* "Gemfile"
               ((".*gem-release.*") ""))
             #t)))))
    (propagated-inputs
     `(("ruby-coderay" ,ruby-coderay)
       ("ruby-erubi" ,ruby-erubi)
       ("ruby-rack" ,ruby-rack)))
    (native-inputs
     `(
       ("ruby-kramdown" ,ruby-kramdown)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-html-matchers" ,ruby-rspec-html-matchers)
       ("ruby-rspec-its" ,ruby-rspec-its)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-simplecov-lcov" ,ruby-simplecov-lcov)
       ("ruby-yard" ,ruby-yard)
       ))
    (synopsis "Error page for Rails and other Rack apps")
    (description
      "This package provides a better error page for Rails and other Rack apps.  Includes source code inspection, a live REPL and local/instance variable inspection for all stack frames.")
    (home-page "https://github.com/BetterErrors/better_errors")
    (license license:expat)))

;; 0.8.0 needs simplecov ~> 0.18
(define-public ruby-simplecov-lcov
  (package
    (name "ruby-simplecov-lcov")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "simplecov-lcov" version))
        (sha256
         (base32
          "0r3pmjjdjvprk8bzmcvarhf57sp5xzgj1c6007qaqhs7b4fhvxvb"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; tests want rails' source directory. Also skip ruby-jeweler
       ;#:test-target "default"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;(setenv "HOME" (getcwd))
             #t)))))
    (native-inputs
     `(
       ;("ruby-jeweler" ,ruby-jeweler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)
       ))
    (synopsis
      "Custom SimpleCov formatter to generate a lcov style coverage.")
    (description
      "Custom SimpleCov formatter to generate a lcov style coverage.")
    (home-page "https://github.com/fortissimo1997/simplecov-lcov")
    (license license:expat)))

(define-public ruby-semver2
  (package
    (name "ruby-semver2")
    (version "3.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/haf/semver")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "10yj3vglkw8h1am80jpzkxq7l9ywbvd9fsdjg1mm0c33k1y2jw65"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     `(
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "maintain versions as per http://semver.org")
    (description
      "maintain versions as per http://semver.org")
    (home-page "https://github.com/haf/semver")
    (license license:expat)))

(define-public ruby-descendants-tracker
  (package
    (name "ruby-descendants-tracker")
    (version "0.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "descendants_tracker" version))
        (sha256
         (base32
          "15q8g3fcqyb41qixn6cky0k3p86291y7xsh1jfd851dvrza1vi79"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; old package is old
    (propagated-inputs
     `(("ruby-thread-safe" ,ruby-thread-safe)))
    (native-inputs
     `(
       ;("ruby-devtools" ,ruby-devtools)
       ))
    (synopsis
      "Module that adds descendant tracking to a class")
    (description
      "Module that adds descendant tracking to a class")
    (home-page "https://github.com/dkubb/descendants_tracker")
    (license license:expat)))

(define-public ruby-github-api
  (package
    (name "ruby-github-api")
    (version "0.19.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/piotrmurach/github")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "13ysd56b473m0yvjdxmqgia2w96m3qq29xzvpdqhz50xjid2mqk2"))))
    (build-system ruby-build-system)
    (arguments
     `(;#:tests? #f  ; tests depend on ruby-vcr, which is non-free
       #:test-target "spec" ; default fails on the ':features' portion
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             ;; Don't specify version numbers.
             (substitute* "github_api.gemspec"
               (("cucumber.*") "cucumber\"\n")
               (("vcr.*") "vcr\"\n")
               )
             #t)))))
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-descendants-tracker" ,ruby-descendants-tracker)
       ("ruby-faraday" ,ruby-faraday)
       ("ruby-hashie" ,ruby-hashie)
       ("ruby-oauth2" ,ruby-oauth2)))
    (native-inputs
     `(
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-its" ,ruby-rspec-its)
       ("ruby-vcr" ,ruby-vcr)
       ("ruby-webmock" ,ruby-webmock)
       ))
    (synopsis "Ruby client of GitHub API methods")
    (description
     "This package provides a Ruby client that supports all of the GitHub API methods.")
    (home-page "https://piotrmurach.github.io/github/")
    (license license:expat)))

(define-public ruby-jeweler
  (package
    (name "ruby-jeweler")
    (version "2.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "jeweler" version))
        (sha256
         (base32
          "0jbivh9vf9wm91kwjnlcvswqyk2g24bnxj9gavinx9jh4bphagi5"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-builder" ,ruby-builder)
       ("bundler" ,bundler)
       ("ruby-git" ,ruby-git)
       ("ruby-github-api" ,ruby-github-api)
       ("ruby-highline" ,ruby-highline)
       ("ruby-nokogiri" ,ruby-nokogiri)
       ("ruby-psych" ,ruby-psych)
       ("ruby-rake" ,ruby-rake)
       ("ruby-rdoc" ,ruby-rdoc)
       ("ruby-semver2" ,ruby-semver2)))
    (synopsis
      "Simple and opinionated helper for creating Rubygem projects on GitHub")
    (description
      "Simple and opinionated helper for creating Rubygem projects on GitHub")
    (home-page "https://github.com/technicalpickles/jeweler")
    (license license:expat)))

;; 5.0.0 is the last version under a free license. 5.1.0+ are under the Hippocratic License
(define-public ruby-vcr
  (package
    (name "ruby-vcr")
    (version "5.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/vcr/vcr")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1anich4cgjz7adbk1sh07q494dgb757np74qdiz0nwjzl41s6iyp"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; TODO: figure out test suite
       #:test-target "default"))
    (native-inputs
     `(
       ("ruby-codeclimate-test-reporter" ,ruby-codeclimate-test-reporter)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)
       ))
    (synopsis
      "Record your test suite's HTTP interactions and replay them during future test runs for fast, deterministic, accurate tests.")
    (description
      "Record your test suite's HTTP interactions and replay them during future test runs for fast, deterministic, accurate tests.")
    (home-page "https://relishapp.com/vcr/vcr/docs")
    (license license:expat)))

(define-public ruby-annotate
  (package
    (name "ruby-annotate")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "annotate" version))
        (sha256
         (base32
          "1dxrfppwfg13vqmambbs56xjj8qsdgcy58r2yc44vvy3z1g5yflw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no tests in gem
    (propagated-inputs
     `(("ruby-activerecord" ,ruby-activerecord)
       ("ruby-rake" ,ruby-rake)))
    (synopsis
      "Annotates Rails/ActiveRecord Models, routes, fixtures, and others based on the database schema.")
    (description
      "Annotates Rails/ActiveRecord Models, routes, fixtures, and others based on the database schema.")
    (home-page "https://github.com/ctran/annotate_models")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-flamegraph
  (package
    (name "ruby-flamegraph")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "flamegraph" version))
        (sha256
         (base32
          "1p785nmhdzbwj0qpxn5fzrmr4kgimcds83v4f95f387z6w3050x6"))))
    (build-system ruby-build-system)
    (synopsis
      "Flamegraph support for arbitrary ruby apps")
    (description
      "Flamegraph support for arbitrary ruby apps")
    (home-page "https://github.com/SamSaffron/flamegraph")
    (license license:expat)))

(define-public ruby-optimist
  (package
    (name "ruby-optimist")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "optimist" version))
        (sha256
         (base32
          "1vg2chy1cfmdj6c1gryl8zvjhhmb3plwgyh1jfnpq4fnfqv7asrk"))))
    (build-system ruby-build-system)
    (native-inputs
     `(
       ;; https://github.com/ManageIQ/optimist/issues/109
       ("ruby-chronic" ,ruby-chronic)
       ))
    (synopsis
      "Optimist is a commandline option parser for Ruby that just
gets out of your way. One line of code per option is all you need to write.
For that, you get a nice automatically-generated help page, robust option
parsing, command subcompletion, and sensible defaults for everything you don't
specify.")
    (description
      "Optimist is a commandline option parser for Ruby that just
gets out of your way.  One line of code per option is all you need to write.
For that, you get a nice automatically-generated help page, robust option
parsing, command subcompletion, and sensible defaults for everything you don't
specify.")
    (home-page "https://manageiq.github.io/optimist/")
    (license license:expat)))

;; TODO: Unbundle msgpack
(define-public ruby-rbtrace
  (package
    (name "ruby-rbtrace")
    (version "0.4.14")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rbtrace" version))
        (sha256
         (base32
          "0s8prj0klfgpmpfcpdzbf149qrrsdxgnb6w6kkqc9gyars4vyaqn"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-msgpack-library
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((msgpack (assoc-ref inputs "msgpack")))
               ;; Don't try to build msgpack during 'install
               (substitute* "rbtrace.gemspec"
                 ((".*s.extensions.*") ""))

               ;; Use shared library, built with -fPIC
               (substitute* "ext/extconf.rb"
                 (("libmsgpackc.a") "libmsgpackc.so")
                 (("libmsgpackc_ext.a") "libmsgpackc_ext.so"))

               (mkdir-p "ext/dst/lib")
               (mkdir-p "ext/dst/include")
               (symlink (string-append msgpack "/lib/libmsgpackc.so")
                        "ext/dst/lib/libmsgpackc.so")
               (symlink (string-append msgpack "/include/msgpack.h")
                        "ext/dst/include/msgpack.h")

               ;; compile the rbtrace binary, linking to msgpack
               (setenv "CC" ,(cc-for-target))
               (with-directory-excursion "ext"
                 (invoke "ruby" "extconf.rb")
                 (invoke "make"))
               #t))))))
    (propagated-inputs
     `(("ruby-ffi" ,ruby-ffi)
       ("ruby-msgpack" ,ruby-msgpack)
       ("ruby-optimist" ,ruby-optimist)))
    (inputs
     `(("msgpack" ,(@ (gnu packages serialization) msgpack))))
    (synopsis
      "rbtrace shows you method calls happening inside another ruby process in real time.")
    (description
      "rbtrace shows you method calls happening inside another ruby process in real time.")
    (home-page "https://github.com/tmm1/rbtrace")
    (license license:expat)))

(define-public ruby-gc-tracer
  (package
    (name "ruby-gc-tracer")
    (version "1.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "gc_tracer" version))
        (sha256
         (base32
          "1yv3mp8lx74lfzs04fd5h4g89209iwhzpc407y35p7cmzgx6a4kv"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (native-inputs
     `(
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "gc_tracer gem adds GC::Tracer module.")
    (description
      "gc_tracer gem adds GC::Tracer module.")
    (home-page "https://github.com/ko1/gc_tracer")
    (license license:expat)))

(define-public ruby-guess-html-encoding
  (package
    (name "ruby-guess-html-encoding")
    (version "0.0.11")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "guess_html_encoding" version))
        (sha256
         (base32
          "16700fk6kmif3q3kpc1ldhy3nsc9pkxlgl8sqhznff2zjj5lddna"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "A small gem that attempts to guess and then force encoding of HTML documents for Ruby 1.9")
    (description
      "This package provides a small gem that attempts to guess and then force encoding of HTML documents for Ruby 1.9")
    (home-page "https://github.com/cantino/guess_html_encoding")
   (license license:expat)))

(define-public ruby-ruby-readability
  (package
    (name "ruby-ruby-readability")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ruby-readability" version))
        (sha256
         (base32
          "15ivhbry7hf82lww1bzcrwfyjymijfb3rb0wdd32g2z0942wdspa"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; ':spec" fails with newer ruby versions
       #:test-target "default"))
    (propagated-inputs
     `(("ruby-guess-html-encoding" ,ruby-guess-html-encoding)
       ("ruby-nokogiri" ,ruby-nokogiri)))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "Port of arc90's readability project to ruby")
    (description
      "Port of arc90's readability project to ruby")
    (home-page "https://github.com/cantino/ruby-readability")
    (license (license:non-copyleft
               "will fill in later"))))

;; TODO: 0.3.3 never finishes the install phase with ruby-2.6
;; TODO: Unbundle cppjieba
(define-public ruby-cppjieba-rb
  (package
    (name "ruby-cppjieba-rb")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "cppjieba_rb" version))
        (sha256
         (base32
          "1sslff7yy8jvp4rcn1b6jn9v0d3iibb68i79shgd94rs2yq8k117"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda _
             (invoke "rake" "compile"))))))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis "Cppjieba binding for Ruby")
    (description
     "This package provides Ruby bindles for @code{cppjieba}, a library to help
with processing Chinese text.")
    (home-page "https://github.com/fantasticfears/cppjieba_rb")
    (license license:expat)))

(define-public ruby-request-store
  (package
    (name "ruby-request-store")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "request_store" version))
        (sha256
         (base32
          "0cx74kispmnw3ljwb239j65a2j14n8jlsygy372hrsa8mxc71hxi"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack)))
    (synopsis
      "RequestStore gives you per-request global storage.")
    (description
      "RequestStore gives you per-request global storage.")
    (home-page "https://github.com/steveklabnik/request_store")
    (license license:expat)))

(define-public ruby-lograge
  (package
    (name "ruby-lograge")
    (version "0.11.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/roidrage/lograge")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1dlxzv0xjnjs95l4fzr1wcf2l7klnhji0fv51gl6wvkjpjdr9zcy"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; cannot find logstash-event
       #:test-target "spec" ; not default, rubocop compains about all the things
       #:phases
       (modify-phases %standard-phases
         (replace 'replace-git-ls-files
           (lambda _
             (substitute* "lograge.gemspec"
               (("git ls-files lib LICENSE.txt")
                "find lib LICENSE.txt -type f | sort"))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (propagated-inputs
     `(("ruby-actionpack" ,ruby-actionpack)
       ("ruby-activesupport" ,ruby-activesupport)
       ("ruby-railties" ,ruby-railties)
       ("ruby-request-store" ,ruby-request-store)))
    (native-inputs
     `(
       ("ruby-activerecord" ,ruby-activerecord)
       ("ruby-lines" ,ruby-lines)
       ("ruby-logstash-event" ,ruby-logstash-event)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)
       ))
    (synopsis
      "Tame Rails' multi-line logging into a single line per request")
    (description
      "Tame Rails' multi-line logging into a single line per request")
    (home-page "https://github.com/roidrage/lograge")
    (license license:expat)))

(define-public ruby-logstash-event
  (package
    (name "ruby-logstash-event")
    (version "1.2.02")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "logstash-event" version))
        (sha256
         (base32
          "1bk7fhhryjxp1klr3hq6i6srrc21wl4p980bysjp0w66z9hdr9w9"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile
    (synopsis
      "Library that contains the classes required to create LogStash events")
    (description
      "Library that contains the classes required to create LogStash events")
    (home-page "https://github.com/logstash/logstash")
    (license license:asl2.0)))

(define-public ruby-lines
  (package
    (name "ruby-lines")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "lines" version))
        (sha256
         (base32
          "17hrpvqvc2li4rjnk75xy5qvylk0vg0z8if7q1m00al79d0mpbma"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis
      "Readable log format for humans and computers")
    (description
     "This package provides a log format that's readable by humans and easily parseable by computers.")
    (home-page "https://github.com/zimbatm/lines-ruby")
    (license license:expat)))

(define-public ruby-logstash-logger
  (package
    (name "ruby-logstash-logger")
    (version "0.26.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "logstash-logger" version))
        (sha256
         (base32
          "1nh0jgz4rl46axqb9l0fa866kh34wb7yf11qc3j30xhprdqb8yjp"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; incompatable dependencies on simplecov
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (propagated-inputs
     `(("ruby-logstash-event" ,ruby-logstash-event)))
    (native-inputs
     `(
       ("ruby-appraisal" ,ruby-appraisal)
       ("ruby-aws-sdk-firehose" ,ruby-aws-sdk-firehose)
       ("ruby-aws-sdk-kinesis" ,ruby-aws-sdk-kinesis)
       ("ruby-codeclimate-test-reporter" ,ruby-codeclimate-test-reporter)
       ("ruby-codecov" ,ruby-codecov)
       ("ruby-poseidon" ,ruby-poseidon)
       ("ruby-pry" ,ruby-pry)
       ("ruby-rails" ,ruby-rails)
       ("ruby-redis" ,ruby-redis)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)
       ("ruby-wwtd" ,ruby-wwtd)
       ))
    (synopsis
      "Ruby logger that writes directly to LogStash")
    (description
      "Ruby logger that writes directly to LogStash")
    (home-page "https://github.com/dwbutler/logstash-logger")
    (license license:expat)))

(define-public ruby-poseidon
  (package
    (name "ruby-poseidon")
    (version "0.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "poseidon" version))
        (sha256
         (base32
          "0lxavrybqwa3xmsajvxp0ngqmn4jxw9q93398yxzdrx83i8a7l8p"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (native-inputs
     `(
       ("ruby-coveralls" ,ruby-coveralls)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-snappy" ,ruby-snappy)
       ))
    (synopsis
      "A Kafka (http://kafka.apache.org/) producer and consumer")
    (description
      "This package provides a Kafka (http://kafka.apache.org/) producer and consumer")
    (home-page "https://github.com/bpot/poseidon")
    (license license:expat)))

(define-public ruby-snappy
  (package
    (name "ruby-snappy")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "snappy" version))
        (sha256
         (base32
          "0xl54r1vvrhlkxf16mxqk4kz18j0igf1f7l66kqd9dbyv6x99zfg"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; Test "VERSION" fails for unknown reasons
             (delete-file "test/snappy_test.rb")
             (with-output-to-file "test/snappy_test.rb"
               (lambda _ ""))
             #t)))))
    (inputs
     `(
       ("snappy" ,snappy)
       ))
    (native-inputs
     `(
       ("ruby-rake" ,ruby-rake)
       ("ruby-test-unit-rr" ,ruby-test-unit-rr)
       ))
    (synopsis "libsnappy binding for Ruby")
    (description "libsnappy binding for Ruby")
    (home-page "https://github.com/miyucy/snappy")
    (license license:expat)))

(define-public ruby-test-unit-rr
  (package
    (name "ruby-test-unit-rr")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "test-unit-rr" version))
        (sha256
         (base32
          "0l42wxvqqd4g1968syy1wmr4j9z3cad8v5vv22lr7my4wjgz0sil"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-rr" ,ruby-rr)
       ("ruby-test-unit" ,ruby-test-unit)))
    (native-inputs
     `(
       ("ruby-packnga" ,ruby-packnga)
       ))
    (synopsis
      "You don't need RR setup codes with test-unit-rr. You just require
\"test/unit/rr\".
")
    (description
      "You don't need RR setup codes with test-unit-rr.  You just require
\"test/unit/rr\".
")
    (home-page "https://github.com/test-unit/test-unit-rr")
    (license (license:non-copyleft
               "will fill in later"))))

(define-public ruby-aws-sdk-kinesis
  (package
    (name "ruby-aws-sdk-kinesis")
    (version "1.31.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "aws-sdk-kinesis" version))
        (sha256
         (base32
          "1wsnn4303q7501xp10gfr8s15cazm4a0xy8knz5b8pmaw93x0g4b"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (propagated-inputs
     `(("ruby-aws-sdk-core" ,ruby-aws-sdk-core)
       ("ruby-aws-sigv4" ,ruby-aws-sigv4)))
    (synopsis
      "Official AWS Ruby gem for Amazon Kinesis (Kinesis). This gem is part of the AWS SDK for Ruby.")
    (description
      "Official AWS Ruby gem for Amazon Kinesis (Kinesis).  This gem is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-aws-sdk-firehose
  (package
    (name "ruby-aws-sdk-firehose")
    (version "1.36.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "aws-sdk-firehose" version))
        (sha256
         (base32
          "0ji75vqfprnkjsy6gdk9qci6wd9kwm7h7lycpx7jsw0fbv6hjx0p"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (propagated-inputs
     `(("ruby-aws-sdk-core" ,ruby-aws-sdk-core)
      ("ruby-aws-sigv4" ,ruby-aws-sigv4)))
    (synopsis
      "Official AWS Ruby gem for Amazon Kinesis Firehose (Firehose). This gem is part of the AWS SDK for Ruby.")
    (description
      "Official AWS Ruby gem for Amazon Kinesis Firehose (Firehose).  This gem is part of the AWS SDK for Ruby.")
    (home-page "https://github.com/aws/aws-sdk-ruby")
    (license license:asl2.0)))

(define-public ruby-codecov
  (package
    (name "ruby-codecov")
    (version "0.4.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/codecov/codecov-ruby")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "01x1n0j2awnpbnhzhsc058ni8bh2zw3vasbz9gkivydgv2r8xkpd"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; can't install gem
    (propagated-inputs
     `(("ruby-simplecov" ,ruby-simplecov)))
    (synopsis "Hosted code coverage Ruby reporter")
    (description
      "Hosted code coverage Ruby reporter.")
    (home-page "https://github.com/codecov/codecov-ruby")
    (license license:expat)))

(define-public ruby-rotp
  (package
    (name "ruby-rotp")
    (version "6.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rotp" version))
        (sha256
         (base32
          "11q7rkjx40yi6lpylgl2jkpy162mjw7mswrcgcax86vgpbpjx6i3"))))
    (build-system ruby-build-system)
    (arguments
     `(
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-timecop" ,ruby-timecop)
       ))
    (synopsis
      "Works for both HOTP and TOTP, and includes QR Code provisioning")
    (description
      "Works for both HOTP and TOTP, and includes QR Code provisioning")
    (home-page "https://github.com/mdp/rotp")
    (license license:expat)))

(define-public ruby-sshkey
  (package
    (name "ruby-sshkey")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "sshkey" version))
        (sha256
         (base32
          "03bkn55qsng484iqwz2lmm6rkimj01vsvhwk661s3lnmpkl65lbp"))))
    (build-system ruby-build-system)
    (synopsis
      "Generate private/public SSH keypairs using pure Ruby")
    (description
      "Generate private/public SSH keypairs using pure Ruby")
    (home-page "https://github.com/bensie/sshkey")
    (license license:expat)))

(define-public ruby-rchardet
  (package
    (name "ruby-rchardet")
    (version "1.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jmhodges/rchardet")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "12xcc4dq0k5gq5r142n7rr34lirfyzlkdh5by64cwz0bg5jfn8il"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'replace-git-ls-files)
         (add-before 'build 'delete-gemfile-lock
           (lambda _
             (delete-file "Gemfile.lock")
             #t))
         )))
    (native-inputs
     `(
       ("ruby-bump" ,ruby-bump)
       ("ruby-minitest-rg" ,ruby-minitest-rg)
       ("ruby-wwtd" ,ruby-wwtd)
       ))
    (synopsis
      "Character encoding auto-detection in Ruby. As smart as your browser. Open source.")
    (description
      "Character encoding auto-detection in Ruby.  As smart as your browser.  Open source.")
    (home-page "https://github.com/jmhodges/rchardet")
    (license license:lgpl2.1+)))

(define-public ruby-lz4-ruby
  (package
    (name "ruby-lz4-ruby")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "lz4-ruby" version))
        (sha256
         (base32
          "12fymsvcb9kw6ycyfzc8b9svriq0afqf1qnl121xrz8c4gpfa6q1"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; tests require ruby-jeweler
       #:phases
       (modify-phases %standard-phases
         (delete 'replace-git-ls-files)
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "Gemfile"
               (("\\\", .*") "\"\n")
               ;; NO JEWELER!
               ((".*jeweler.*") "")
               )
             #t))
         )))
    (native-inputs
     `(
       ;("ruby-rdoc" ,ruby-rdoc)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "Ruby bindings for LZ4. LZ4 is a very fast lossless compression algorithm.")
    (description
      "Ruby bindings for LZ4.  LZ4 is a very fast lossless compression algorithm.")
    (home-page "https://github.com/komiya-atsushi/lz4-ruby")
    (license license:expat)))

(define-public ruby-webpush
  (package
    (name "ruby-webpush")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "webpush" version))
        (sha256
         (base32
          "1z9ma580q80czw46gi1bvsr2iwxr63aiyr7i9gilav6hbhg3sxv3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (propagated-inputs
     `(("ruby-hkdf" ,ruby-hkdf)
       ("ruby-jwt" ,ruby-jwt)))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis
      "Encryption Utilities for Web Push payload.")
    (description
      "Encryption Utilities for Web Push payload.")
    (home-page "https://github.com/zaru/webpush")
    (license license:expat)))

(define-public ruby-colored2
  (package
    (name "ruby-colored2")
    (version "3.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "colored2" version))
        (sha256
         (base32
          "0jlbqa9q4mvrm73aw9mxh23ygzbjiqwisl32d8szfb5fxvbjng5i"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (synopsis
      "This is a heavily modified fork of http://github.com/defunkt/colored gem, with many
sensible pull requests combined. Since the authors of the original gem no longer support it,
this might, perhaps, be considered a good alternative.

Simple gem that adds various color methods to String class, and can be used as follows:

  require 'colored2'

  puts 'this is red'.red
  puts 'this is red with a yellow background'.red.on.yellow
  puts 'this is red with and italic'.red.italic
  puts 'this is green bold'.green.bold &lt;&lt; ' and regular'.green
  puts 'this is really bold blue on white but reversed'.bold.blue.on.white.reversed
  puts 'this is regular, but '.red! &lt;&lt; 'this is red '.yellow! &lt;&lt; ' and yellow.'.no_color!
  puts ('this is regular, but '.red! do
    'this is red '.yellow! do
      ' and yellow.'.no_color!
    end
  end)

")
    (description
      "This is a heavily modified fork of http://github.com/defunkt/colored gem, with many
sensible pull requests combined.  Since the authors of the original gem no longer support it,
this might, perhaps, be considered a good alternative.

Simple gem that adds various color methods to String class, and can be used as follows:

  require 'colored2'

  puts 'this is red'.red
  puts 'this is red with a yellow background'.red.on.yellow
  puts 'this is red with and italic'.red.italic
  puts 'this is green bold'.green.bold &lt;&lt; ' and regular'.green
  puts 'this is really bold blue on white but reversed'.bold.blue.on.white.reversed
  puts 'this is regular, but '.red! &lt;&lt; 'this is red '.yellow! &lt;&lt; ' and yellow.'.no_color!
  puts ('this is regular, but '.red! do
    'this is red '.yellow! do
      ' and yellow.'.no_color!
    end
  end)

")
    (home-page "https://github.com/kigster/colored2")
    (license license:expat)))

(define-public ruby-rails-failover
  (package
    (name "ruby-rails-failover")
    (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rails_failover" version))
        (sha256
         (base32
          "1s8yfpam6qfs2bj596rfwhcgb4pjamg93ijqpa8c5d207sm45a07"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"
       #:tests? #f))    ; cannot load such file -- spec_helper
    (propagated-inputs
     `(("ruby-activerecord" ,ruby-activerecord)
       ("ruby-concurrent-ruby" ,ruby-concurrent)
       ("ruby-railties" ,ruby-railties)))
    (native-inputs
     `(
       ("ruby-rspec" ,ruby-rspec)
       ))
    (synopsis "Failover for ActiveRecord and Redis")
    (description
      "Failover for ActiveRecord and Redis")
    (home-page "https://github.com/discourse/rails_failover")
    (license license:expat)))

(define-public ruby-regressiontest
 (package
  (name "ruby-regressiontest")
  (version "0.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "regressiontest" version))
      (sha256
        (base32
          "03y5ksab6ybd2d586zmihpn2hzvqrxb9pfzh5i6a8f2ivcrfqn9k"))))
  (build-system ruby-build-system)
  (propagated-inputs
    `(("ruby-shoulda" ,ruby-shoulda)))
  (synopsis
    "Regression testing for the command line, and library API")
  (description
    "Regression testing for the command line, and library API")
  (home-page
    "http://github.com/pjotrp/regressiontest")
  (license license:expat)))
