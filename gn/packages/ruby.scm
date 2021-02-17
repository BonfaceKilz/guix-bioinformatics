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
   (license expat)))

(define-public ruby-net-http-digest-auth
  (package
    (name "ruby-net-http-digest-auth")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "net-http-digest_auth" version))
       (sha256
        (base32
         "14801gr34g0rmqz9pv4rkfa3crfdbyfk6r48vpg5a5407v0sixqi"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis
     "An implementation of RFC 2617 - Digest Access Authentication.  At this time
the gem does not drop in to Net::HTTP and can be used for with other HTTP
clients.

In order to use net-http-digest_auth you'll need to perform some request
wrangling on your own.  See the class documentation at Net::HTTP::DigestAuth
for an example.")
    (description
     "An implementation of RFC 2617 - Digest Access Authentication.  At this time
the gem does not drop in to Net::HTTP and can be used for with other HTTP
clients.

In order to use net-http-digest_auth you'll need to perform some request
wrangling on your own.  See the class documentation at Net::HTTP::DigestAuth
for an example.")
    (home-page
     "http://github.com/drbrain/net-http-digest_auth")
    (license #f)))

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
    (license #f)))

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
    (license #f)))

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
  (license expat)))


(define-public ruby-bio-logger ; guix maybe ready
(package
  (name "ruby-bio-logger")
  (version "1.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "bio-logger" version))
      (sha256
        (base32
          "02pylfy8nkdqzyzplvnhn1crzmfkj1zmi3qjhrj2f2imlxvycd28"))))
  (build-system ruby-build-system)
  (propagated-inputs `(("ruby-log4r" ,ruby-log4r)))
  (arguments
   `(#:tests? #f)) ;; no bundler
  (synopsis "Log4r wrapper for BioRuby")
  (description "Log4r wrapper for BioRuby")
  (home-page
    "https://github.com/pjotrp/bioruby-logger-plugin")
  (license #f)))

(define-public ruby-faraday
(package
  (name "ruby-faraday")
  (version "0.14.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "faraday" version))
      (sha256
        (base32
          "1c3x3s8vb5nf7inyfvhdxwa4q3swmnacpxby6pish5fgmhws7zrr"))))
  (build-system ruby-build-system)
  (propagated-inputs
    `(("ruby-multipart-post" ,ruby-multipart-post)))
  (arguments
   `(#:tests? #f)) ;; no bundler/cucumber
  (synopsis "HTTP/REST API client library.")
  (description "HTTP/REST API client library.")
  (home-page
    "https://github.com/lostisland/faraday")
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
  (license #f)))

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
  (license #f)))

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
   (license #f)))


(define-public ruby-redis
  (package
   (name "ruby-redis")
   (version "4.2.5")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "redis" version))
     (sha256
      (base32
       "15x2sr6h094rjbvg8pkq6m3lcd5abpyx93aifvfdz3wv6x55xa48"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f)) ;; no bundler/cucumber
   (synopsis
    "    A Ruby client that tries to match Redis' API one-to-one, while still
    providing an idiomatic interface.
")
   (description
    "    A Ruby client that tries to match Redis' API one-to-one, while still
    providing an idiomatic interface.
")
   (home-page "https://github.com/redis/redis-rb")
   (license license:expat)))





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

(define-public ruby-mini-mime
  (package
    (name "ruby-mini-mime")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mini_mime" version))
        (sha256
         (base32
          "1axm0rxyx3ss93wbmfkm78a6x03l8y4qy60rhkkiq0aza0vwq3ha"))))
    (build-system ruby-build-system)
    (synopsis "Lightweight mime type lookup toy")
    (description "This package provides a lightweight mime type lookup toy.")
    (home-page "https://github.com/discourse/mini_mime")
    (license license:expat)))

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
        ;(method url-fetch)
        ;(uri (rubygems-uri "redis-namespace" version))
        ;(sha256
        ; (base32
        ;  "0k65fr7f8ciq7d9nwc5ziw1d32zsxilgmqdlj3359rz5jgb0f5y8"))))
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
     `(#:phases
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
       ;("ruby-kaminari" ,ruby-kaminari-0.16)
       ("ruby-kaminari" ,ruby-kaminari)
       ("ruby-m" ,ruby-m)
       ;("ruby-minitest" ,ruby-minitest-5.10)
       ("ruby-minitest" ,ruby-minitest)
       ("ruby-rails" ,ruby-rails)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-sqlite3" ,ruby-sqlite3-1.3)
       ("ruby-timecop" ,ruby-timecop)
       ("ruby-will-paginate" ,ruby-will-paginate)
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
     `(#:tests? #f))    ; TODO
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-htmlentities" ,ruby-htmlentities)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-mustache" ,ruby-mustache)
       ("ruby-nokogiri" ,ruby-nokogiri)
       ("ruby-sanitize" ,ruby-sanitize)))
    (native-inputs
     `(
       ("ruby-rake" ,ruby-rake)
       ("ruby-rspec" ,ruby-rspec)
       ;("ruby-fakeweb" ,ruby-fakeweb)
       ))
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
    (home-page
      "https://github.com/tricknotes/ember-handlebars-template")
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
    (home-page
      "https://github.com/babel/ruby-babel-transpiler")
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
     `(#:tests? #f  ; TODO: Upgrade ruby-rubocop >= 1.1.0
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

(define-public ruby-rubocop-rspec
  (package
    (name "ruby-rubocop-rspec")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rubocop-rspec" version))
        (sha256
         (base32
          "0jj6h9ynmacvi2v62dc50qxwrrlvm1hmiblpxc0w2kypik1255ds"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-rubocop" ,ruby-rubocop)
       ("ruby-rubocop-ast" ,ruby-rubocop-ast)))
    (synopsis
      "    Code style checking for RSpec files.
    A plugin for the RuboCop code style enforcing & linting tool.
")
    (description
      "    Code style checking for RSpec files.
    A plugin for the RuboCop code style enforcing & linting tool.
")
    (home-page "https://github.com/rubocop-hq/rubocop-rspec")
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

(define-public ruby-rails-multisite
  (package
    (name "ruby-rails-multisite")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rails_multisite" version))
        (sha256
          (base32
            "0p7g9gkcmw030zfqlw3k933i40j31wf3jh4bj1niihzk7slha97y"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; no rakefile found
    (propagated-inputs
     `(("ruby-activerecord" ,ruby-activerecord)
       ("ruby-railties" ,ruby-railties)))
    (synopsis "Multi tenancy support for Rails")
    (description "Multi tenancy support for Rails")
    (home-page "")
    (license #f)))

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
    (home-page "http://github.com/sdsykes/fastimage")
    (license license:expat)))

(define-public ruby-email-reply-trimmer
  (package
    (name "ruby-email-reply-trimmer")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "email_reply_trimmer" version))
        (sha256
         (base32
          "1jgcxifm48xq5dz9k47q43pqm5bfnf14l62l3bqhmv8f6z8dw4ki"))))
    (build-system ruby-build-system)
    (synopsis
      "EmailReplyTrimmer is a small library to trim replies from plain text email.")
    (description
      "EmailReplyTrimmer is a small library to trim replies from plain text email.")
    (home-page "https://github.com/discourse/email_reply_trimmer")
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
    (home-page "http://github.com/toy/progress")
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
    (license #f)))

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
    (home-page "http://github.com/toy/fspath")
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
       ("ruby-image-size" ,ruby-image-size)
       ("ruby-in-threads" ,ruby-in-threads)
       ("ruby-progress" ,ruby-progress)))
    (synopsis
      "Optimize (lossless compress, optionally lossy) images (jpeg, png, gif, svg) using external utilities (advpng, gifsicle, jhead, jpeg-recompress, jpegoptim, jpegrescan, jpegtran, optipng, pngcrush, pngout, pngquant, svgo)")
    (description
      "Optimize (lossless compress, optionally lossy) images (jpeg, png, gif, svg) using external utilities (advpng, gifsicle, jhead, jpeg-recompress, jpegoptim, jpegrescan, jpegtran, optipng, pngcrush, pngout, pngquant, svgo)")
    (home-page
      "http://github.com/toy/discourse_image_optim")
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
    (license #f)))

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
    (license #f)))

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
    (native-inputs
     `(
       ("glib" ,(@ (gnu packages glib) glib))
       ("pkg-config" ,(@ (gnu packages pkg-config) pkg-config))
       ("python" ,python-2)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ))
    (synopsis
      "Distributes the V8 JavaScript engine in binary and source forms in order to support fast builds of The Ruby Racer")
    (description
      "Distributes the V8 JavaScript engine in binary and source forms in order to support fast builds of The Ruby Racer")
    (home-page "http://github.com/rubyjs/libv8")
    (license license:expat)))

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
     `(#:tests? #f))    ; cannot load such file -- rspec/core/rake_task
    (propagated-inputs
     `(("ruby-execjs" ,ruby-execjs)))
    (synopsis
      "Uglifier minifies JavaScript files by wrapping UglifyJS to be accessible in Ruby")
    (description
      "Uglifier minifies JavaScript files by wrapping UglifyJS to be accessible in Ruby")
    (home-page "http://github.com/lautis/uglifier")
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
     `(
       ("ruby-timecop" ,ruby-timecop)
       ))
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
    (license (list #f #f))))

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

(define-public ruby-minitest-5.10
  (package
    (inherit ruby-minitest)
    (name "ruby-minitest")
    (version "5.10.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "minitest" version))
        (sha256
         (base32
          "05521clw19lrksqgvg2kmm025pvdhdaniix52vmbychrn2jm7kz2"))))))

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
    (license #f)))

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
     `(("ruby-concurrent-ruby" ,ruby-concurrent-ruby)
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
     `(("ruby-concurrent-ruby" ,ruby-concurrent-ruby)))
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
     `(("ruby-concurrent-ruby" ,ruby-concurrent-ruby)
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
     `(("ruby-concurrent-ruby" ,ruby-concurrent-ruby)
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

(define-public ruby-concurrent-ruby
  (package
    (name "ruby-concurrent-ruby")
    (version "1.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "concurrent-ruby" version))
        (sha256
         (base32
          "0mr23wq0szj52xnj0zcn1k0c7j4v79wlwbijkpfcscqww3l6jlg3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ;; TODO: Fix
    (synopsis
      "Modern concurrency tools including agents, futures, promises, thread pools, actors, supervisors, and more.
Inspired by Erlang, Clojure, Go, JavaScript, actors, and classic concurrency patterns.
")
    (description
      "Modern concurrency tools including agents, futures, promises, thread pools, actors, supervisors, and more.
Inspired by Erlang, Clojure, Go, JavaScript, actors, and classic concurrency patterns.
")
    (home-page "http://www.concurrent-ruby.com")
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
     `(("ruby-concurrent-ruby" ,ruby-concurrent-ruby)
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
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;(delete-file "Gemfile.lock")
             (substitute* "Gemfile.lock"
               (("\\(.*\\)") ""))
             #t)))))
    (propagated-inputs
     `(
       ;("ruby-method-source" ,ruby-method-source)
       ;("ruby-rake" ,ruby-rake)
       ))
    (native-inputs
     `(
       ("ruby-activesupport" ,ruby-activesupport)
       ("ruby-allocation-stats" ,ruby-allocation-stats)
       ("ruby-benchmark-ips" ,ruby-benchmark-ips)
       ("ruby-coveralls" ,ruby-coveralls)
       ("ruby-rdiscount" ,ruby-rdiscount)
       ))
    (synopsis
      "Run test/unit tests by line number. Metal!")
    (description
      "Run test/unit tests by line number.  Metal!")
    (home-page "https://github.com/qrush/m")
    (license #f)))

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
     `(#:phases
       (modify-phases %standard-phases
         ;(add-after 'unpack 'use-system-discount
         ;  (lambda _
         ;    (substitute* "Rakefile"
         ;      (("FileList\\['ext.*")
         ;       "FileList['ext/*.rb', 'ext/rdiscount.c', \"ext/ruby-#{RUBYDIGEST}\"] do\n"))
         ;    (substitute* "ext/extconf.rb"
         ;      (("(dir_config\\('rdiscount'\\))" dirconfig)
         ;       (string-append dirconfig "\n\nhave_library('markdown')\n")))
         ;    #t))
         )))
    (inputs
     `(
       ("markdown" ,(@ (gnu packages markup) markdown))
       ))
    (synopsis
      "Fast Implementation of Gruber's Markdown in C")
    (description
      "Fast Implementation of Gruber's Markdown in C")
    (home-page "http://dafoster.net/projects/rdiscount/")
    (license #f)))

