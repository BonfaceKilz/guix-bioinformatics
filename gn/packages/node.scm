(define-module (gn packages node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system node)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages uglifyjs))

(define-public node-asap
  (package
    (name "node-asap")
    (version "2.0.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/kriskowal/asap")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0yclfxrfxlq7daxjfw40y37gbad3y4p4ia79wgycq4g6lrhqvlww"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-events" ,node-events)
       ;("node-jshint" ,node-jshint)
       ;("node-knox" ,node-knox)
       ;("node-mr" ,node-mr)
       ;("node-opener" ,node-opener)
       ;("node-q" ,node-q)
       ;("node-q-io" ,node-q-io)
       ;("node-saucelabs" ,node-saucelabs)
       ;("node-wd" ,node-wd)
       ;("node-weak-map" ,node-weak-map)
       ;("node-benchmark" ,node-benchmark)
       ))
    (home-page "https://github.com/kriskowal/asap")
    (synopsis "High-priority task queue for Node.js and browsers")
    (description "High-priority task queue for Node.js and browsers.")
    (license license:expat)))

;; TODO: Unbundle fonts
(define-public node-async
  (package
    (name "node-async")
    (version "3.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/caolan/async")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "04b8qi2wi8wi3yjs991hh3s0ngk8x1xw0p7xrv3gnr5ck78vf75c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;(replace 'build
         ;  (lambda _
         ;    ;; make build
         ;    (invoke "make" "build-bundle")
         ;    ))
         (delete 'build)
         )
       ))
    (native-inputs
     `(
       ;("node-babel-core" ,node-babel-core)
       ;("node-babel-eslint" ,node-babel-eslint)
       ;("node-babel-minify" ,node-babel-minify)
       ;("node-babel-plugin-add-module-exports" ,node-babel-plugin-add-module-exports)
       ;("node-babel-plugin-instanbul" ,node-babel-plugin-instanbul)
       ;("node-babel-plugin-syntax-async-generators" ,node-babel-plugin-syntax-async-generators)
       ;("node-babel-plugin-transform-es2015-modules-commonjs" ,babel-plugin-transform-es2015-modules-commonjs)
       ;("node-babel-preset-es2015" ,node-babel-preset-es2015)
       ;("node-babel-preset-es2017" ,node-babel-preset-es2017)
       ;("node-babel-register" ,node-babel-register)
       ;("node-babelify" ,noce-babelify)
       ;("node-benchmark" ,node-benchmark)
       ;("node-bluebird" ,node-bluebird)
       ;("node-browserify" ,node-browserify)
       ;("node-chai" ,node-chai)
       ;("node-cheerio" ,noce-cheerio)
       ;("node-coveralls" ,node-coveralls)
       ;("node-es6-promise" ,node-es6-promise)
       ;("node-eslint" ,node-eslint)
       ;("node-eslint-plugin-prefer-arrow" ,node-eslint-plugin-prefer-arrow)
       ;("node-fs-extra" ,node-fs-extra)
       ;("node-jsdoc" ,node-jsdoc)
       ;("node-karma" ,node-karma)
       ;("node-karma-browserify" ,node-karma-browserify)
       ;("node-karma-edge-launcher" ,node-karma-edge-launcher)
       ;("node-karma-firefox-launcher" ,node-karma-firefox-launcher)
       ;("node-karma-junit-reporter" ,node-karma-junit-reporter)
       ;("node-karma-mocha" ,node-karma-mocha)
       ;("node-karma-mocha-reporter" ,node-karma-mocha-reporter)
       ;("node-safari-launcher" ,node-safari-launcher)
       ;("node-mocha" ,node-mocha)
       ;("node-mocha-junit-reporter" ,node-mocha-junit-reporter)
       ;("node-native-promises-only" ,node-native-promises-only)
       ;("node-nyc" ,node-nyc)
       ;("node-rimraf" ,node-rimraf)
       ;("node-rollup" ,node-rollup)
       ;("node-rollup-plugin-node-resolve" ,node-rollup-plugin-node-resolve)
       ;("node-rollup-plugin-npm" ,node-rollup-plugin-npm)
       ;("node-rsvp" ,node-rsvp)
       ;("node-semver" ,node-semver)
       ;("node-yargs" ,node-yargs)
       ))
    (home-page "https://caolan.github.io/async/v3/")
    (synopsis "Async utilities for node and the browser")
    (description "Async is a utility module which provides straight-forward, powerful functions for working with asynchronous JavaScript.")
    (license license:expat)))

(define-public node-async-2
  (package
    (inherit node-async)
    (name "node-async")
    (version "2.6.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/caolan/async")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0lxr8m00f7723p5qpg5b5wlqv5a0y9a301r6rmhy1cv64jnznpxk"))))
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;(replace 'build
         ;  (lambda _
         ;    ;; make build
         ;    (invoke "make" "build-bundle")
         ;    ))
         (delete 'build)))) ; Runs tests by default during 'make all'.
    (inputs
     `(
       ;("node-lodash" ,node-lodash)
       ;("node-lodash-es" ,node-lodash-es)
       ))
    (native-inputs
     `(
       ;("node-babel-cli" ,node-babel-cli)
       ;("node-babel-core" ,node-babel-core)
       ;("node-babel-plugin-add-module-exports" ,node-babel-plugin-add-module-exports)
       ;("node-babel-plugin-instanbul" ,node-babel-plugin-instanbul)
       ;("node-babel-plugin-transform-es2015-modules-commonjs" ,babel-plugin-transform-es2015-modules-commonjs)
       ;("node-babel-preset-es2015" ,node-babel-preset-es2015)
       ;("node-babel-preset-es2017" ,node-babel-preset-es2017)
       ;("node-babelify" ,noce-babelify)
       ;("node-benchmark" ,node-benchmark)
       ;("node-bluebird" ,node-bluebird)
       ;("node-browserify" ,node-browserify)
       ;("node-chai" ,node-chai)
       ;("node-cheerio" ,noce-cheerio)
       ;("node-coveralls" ,node-coveralls)
       ;("node-es6-promise" ,node-es6-promise)
       ;("node-eslint" ,node-eslint)
       ;("node-fs-extra" ,node-fs-extra)
       ;("node-gh-pages-deploy" ,node-gh-pages-deploy)
       ;("node-jsdoc" ,node-jsdoc)
       ;("node-karma" ,node-karma)
       ;("node-karma-browserify" ,node-karma-browserify)
       ;("node-karma-firefox-launcher" ,node-karma-firefox-launcher)
       ;("node-karma-mocha" ,node-karma-mocha)
       ;("node-karma-mocha-reporter" ,node-karma-mocha-reporter)
       ;("node-mocha" ,node-mocha)
       ;("node-native-promises-only" ,node-native-promises-only)
       ;("node-nyc" ,node-nyc)
       ;("node-rimraf" ,node-rimraf)
       ;("node-rollup" ,node-rollup)
       ;("node-rollup-plugin-node-resolve" ,node-rollup-plugin-node-resolve)
       ;("node-rollup-plugin-npm" ,node-rollup-plugin-npm)
       ;("node-rsvp" ,node-rsvp)
       ;("node-semver" ,node-semver)
       ;("node-yargs" ,node-yargs)
       ;("uglify-js" ,uglify-js)
       ))))

(define-public node-async-1
  (package
    (inherit node-async)
    (name "node-async")
    (version "1.5.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/caolan/async")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "17ai1ymw6i13zpls4hj267qmf7wwrdlf8zqvkip6q4kvrjmxhhgq"))))
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "make" "clean")
             (invoke "make" "build"))))))
    (native-inputs
     `(
       ;("node-benchmark" ,node-benchmark)
       ;("node-bluebird" ,node-bluebird)
       ;("node-chai" ,node-chai)
       ;("node-coveralls" ,node-coveralls)
       ;("node-es6-promise" ,node-es6-promise)
       ;("node-jscs" ,node-jscs)
       ;("node-jshint" ,node-jshint)
       ;("node-karma" ,node-karma)
       ;("node-karma-browserify" ,node-karma-browserify)
       ;("node-karma-firefox-launcher" ,node-karma-firefox-launcher)
       ;("node-karma-mocha" ,node-karma-mocha)
       ;("node-karma-mocha-reporter" ,node-karma-mocha-reporter)
       ;("node-lodash" ,node-lodash)
       ;("node-mkdirp" ,node-mkdirp)
       ;("node-mocha" ,node-mocha)
       ;("node-native-promises-only" ,node-native-promises-only)
       ;("node-nodeunit" ,node-nodeunit)
       ;("node-nyc" ,node-nyc)
       ;("node-rsvp" ,node-rsvp)
       ;("node-semver" ,node-semver)
       ("node-uglify-js" ,node-uglify-js)
       ;("node-xyz" ,node-xyz)
       ;("node-yargs" ,node-yargs)
       ))))

(define-public node-closure-library
  (package
    (name "node-closure-library")
    (version "20191111.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/google/closure-library.git")
               (commit (string-append "v" (version-major version)))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1wiknd263wsn7b8dn3i2lv6m1m9b8wvb8r4x4cm3acpnk9rniy09"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(
       ("node-promise" ,node-promise)
       ;("node-protractor" ,node-protractor)
       ))
    ;(native-inputs
    ; `(("node-promises-aplus-test" ,node-promises-aplus-test)))
    (home-page "https://developers.google.com/closure/library/")
    (synopsis "Google's common JavaScript library")
    (description "Closure Library is a powerful, low-level JavaScript library
designed for building complex and scalable web applications.  It is used by many
Google web applications, such as Google Search, Gmail, Google Docs, Google+
Google Maps, and others.")
    (license license:asl2.0)))

(define-public node-color
  (package
    (name "node-color")
    (version "3.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Qix-/color")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1r1v7l6nkmdi81qm670vz33qz5h2agxvd990km7afc4syd2qqc9l"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-color-convert" ,node-color-convert)
       ("node-color-string" ,node-color-string)))
    (native-inputs
     `(
       ;("node-mocha" ,node-mocha)
       ;("node-xo" ,node-xo)
       ))
    (home-page "https://github.com/Qix-/color")
    (synopsis "Javascript color conversion and manipulation library")
    (description "Javascript color conversion and manipulation library.")
    (license license:expat)))

(define-public node-color-convert
  (package
    (name "node-color-convert")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Qix-/color-convert")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0gr892xvcn24ph2wdxbh7g5vpv644hjiyhhxh7d1jwzr2wj5zxkk"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-color-name" ,node-color-name)))
    (native-inputs
     `(
       ;("node-chalk" ,node-chalk)
       ;("node-xo" ,node-xo)
       ))
    (home-page "https://github.com/Qix-/color-convert")
    (synopsis "Plain color conversion functions in JavaScript")
    (description "Plain color conversion functions in JavaScript.")
    (license license:expat)))

(define-public node-color-string
  (package
    (name "node-color-string")
    (version "1.5.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Qix-/color-string")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0nzg0ayvdqvnn7v0v2dnfj7myqca9ylndnqa6bqkj0l6553i1w1r"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-color-name" ,node-color-name)
       ("node-simple-swizzle" ,node-simple-swizzle)))
    ;(native-inputs
    ; `(("node-xo" ,node-xo)))
    (home-page "https://github.com/Qix-/color-string")
    (synopsis "Parser and generator for CSS color strings")
    (description "Parser and generator for CSS color strings.")
    (license license:expat)))

(define-public node-colors
  (package
    (name "node-colors")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Marak/colors.js")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1ih98ycxjprlxn72ygqgkgcp9wkpd20apndjd11270qyyifvkr8y"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-eslint" ,node-eslint)
       ;("node-eslint-config-google" ,node-eslint-config-google)
       ))
    (home-page "https://github.com/Marak/colors.js")
    (synopsis "get colors in your node.js console")
    (description "get colors in your node.js console.")
    (license license:expat)))

(define-public node-colorspace
  (package
    (name "node-colorspace")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/3rd-Eden/colorspace")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0rv01da38hkxyx4q8gs31v0sj9xsclfwq5n2h43nyracs23mqi8r"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-color" ,node-color)
       ("node-text-hex" ,node-text-hex)))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-mocha" ,node-mocha)
       ;("node-pre-commit" ,node-pre-commit)
       ))
    (home-page "https://github.com/3rd-Eden/colorspace")
    (synopsis "Generate HEX colors for a given namespace")
    (description "Generate HEX colors for a given namespace.")
    (license license:expat)))

(define-public node-commander
  (package
    (name "node-commander")
    (version "4.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tj/commander.js.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "10lwicm3kppbnwkcl4z6v6ix0dp5m1ny88wmli8084f3wxb1j3ds"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-eslint" ,node-eslint)
       ;("node-eslint-plugin-jest" ,node-eslint-plugin-jest)
       ;("node-jest" ,node-jest)
       ;("node-standard" ,node-standard)
       ;("node-ts-node" ,node-ts-node)
       ;("node-typescript" ,node-typescript)
       ))
    (home-page "https://github.com/tj/commander.js")
    (synopsis "Complete solution for node.js command-line programs")
    (description "The complete solution for node.js command-line programs.")
    (license license:expat)))

(define-public node-configurable-http-proxy
  (package
    (name "node-configurable-http-proxy")
    (version "4.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jupyterhub/configurable-http-proxy")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1w3kf0g66sq56y6q191w1am99fwhmlfjgc6kmffmphcfl6gkw9ax"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-commander" ,node-commander)
       ("node-http-proxy" ,node-http-proxy)
       ("node-lynx" ,node-lynx)
       ("node-strftime" ,node-strftime)
       ("node-winston" ,node-winston)))
    (native-inputs
     `(
       ;("node-jasmine" ,node-jasmine)
       ;("node-jshint" ,node-jshint)
       ;("node-nyc" ,node-nyc)
       ;("node-prettier" ,node-prettier)
       ;("node-request" ,node-request)
       ;("node-request-promise-native" ,node-request-promise-native)
       ;("node-ws" ,node-ws)
       ))
    (home-page "https://github.com/jupyterhub/configurable-http-proxy")
    (synopsis "Configurable-on-the-fly HTTP Proxy")
    (description "@dfn{configurable-http-proxy} (CHP) provides you with a way to
update and manage a proxy table using a command line interface or REST API.  It
is a simple wrapper around @code{node-http-proxy}.  @code{node-http-proxy} is an
HTTP programmable proxying library that supports websockets and is suitable for
implementing components such as reverse proxies and load balancers.  By wrapping
@code{node-http-proxy}, @code{configurable-http-proxy} extends this
functionality to JupyterHub deployments.")
    (license license:bsd-3)))

(define-public node-configurable-http-proxy-3
  (package
    (inherit node-configurable-http-proxy)
    (name "node-configurable-http-proxy")
    (version "3.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jupyterhub/configurable-http-proxy")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0rzyppyzbgmmkxa5v17rpvlh27ygr0wi5cnn01ajdqn9cfc70j4f"))))
    (arguments '(#:tests? #f))
    (inputs
     `(("node-commander" ,node-commander)
       ("node-http-proxy" ,node-http-proxy)
       ("node-lynx" ,node-lynx)
       ("node-strftime" ,node-strftime)
       ("node-winston" ,node-winston-2)))
    (native-inputs
     `(
       ;("node-jasmine" ,node-jasmine)
       ;("node-jshint" ,node-jshint)
       ;("node-nyc" ,node-nyc)
       ;("node-prettier" ,node-prettier)
       ;("node-request" ,node-request)
       ;("node-request-promise-native" ,node-request-promise-native)
       ;("node-ws" ,node-ws)
       ))))

(define-public node-core-util-is
  (package
    (name "node-core-util-is")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/isaacs/core-util-is")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "11avvk8bp9sp8qrn9fms3d562sc7bsyr3caqn1sc140f0zijh6rz"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    ;(native-inputs
    ; `(("node-tap" ,node-tap)))
    (home-page "https://github.com/isaacs/core-util-is")
    (synopsis "The util.is* functions from Node core")
    (description "The @code{util.is*} functions introduced in Node v0.12.")
    (license license:expat)))

(define-public node-decamelize
  (package
    (name "node-decamelize")
    (version "3.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sindresorhus/decamelize")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "04ia9sqg8s50wwralpdqlmni4bzh0225a6pp159ivfij9zcw47ss"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    ;(inputs
    ; `(("node-xregexp" ,node-xregexp)))
    (native-inputs
     `(
       ;("node-ava" ,node-ava)
       ;("node-tsd" ,node-tsd)
       ;("node-xo" ,node-xo)
       ))
    (home-page "https://github.com/sindresorhus/decamelize")
    (synopsis "")
    (description "Convert a camelized string into a lowercased one with a custom separator: unicornRainbow → unicorn_rainbow.")
    (license license:expat)))

(define-public node-diagnostics
  (package
    (name "node-diagnostics")
    (version "2.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/3rd-Eden/diagnostics")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0fwa5srdg75gvgj13id7ndnj6hvnn5lqqmxj8w4z22p3hvi41qmj"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)))) ; Runs tests by default during 'make all'.
    (inputs
     `(("node-colorspace" ,node-colorspace)
       ("node-enabled" ,node-enabled)
       ("node-kuler" ,node-kuler)
       ("node-storage-engine" ,node-storage-engine)))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-asyncstorageapi" ,node-asyncstorageapi)
       ;("node-mocha" ,node-mocha)
       ;("node-nyc" ,node-nyc)
       ;("node-objstorage" ,node-objstorage)
       ;("node-pre-commit" ,node-pre-commit)
       ;("node-react-native" ,node-react-native)
       ;("node-require-poisoning" ,node-require-poisoning)
       ;("node-webpack" ,node-webpack)
       ;("node-webpack-bundle-size-analyzer" ,node-webpack-bundle-size-analyzer)
       ;("node-webpack-cli" ,node-webpack-cli)
       ))
    (home-page "https://github.com/3rd-Eden/diagnostics")
    (synopsis "Tools for debugging your node.js modules")
    (description "Tools for debugging your node.js modules.")
    (license license:expat)))

(define-public node-enabled
  (package
    (name "node-enabled")
    (version "2.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/3rd-Eden/enabled")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0cxck167l6pv73imvgnz6rdrjdwym5p1sh6w37ayy6l19kpgj3gz"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-istanbul" ,node-istanbul)
       ;("node-mocha" ,node-mocha)
       ;("node-pre-commit" ,node-pre-commit)
       ))
    (home-page "https://github.com/3rd-Eden/enabled")
    (synopsis "Check if a certain flag is enabled")
    (description "Check if a certain flag is enabled.")
    (license license:expat)))

(define-public node-eventemitter3
  (package
    (name "node-eventemitter3")
    (version "4.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/primus/eventemitter3")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0hj37wxbw0ddfwrhkm9qx3qcziksz21ga07m7n608bica997djy9"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-browserify" ,node-browserify)
       ;("node-mocha" ,node-mocha)
       ;("node-nyc" ,node-nyc)
       ;("node-pre-commit" ,node-pre-commit)
       ;("node-sauce-browsers" ,node-sauce-browsers)
       ;("node-sauce-test" ,node-sauce-test)
       ("node-uglify-js" ,node-uglify-js)
       ))
    (home-page "https://github.com/primus/eventemitter3")
    (synopsis "EventEmitter3 focuses on performance while maintaining a Node.js AND browser compatible interface")
    (description "EventEmitter3 is a high performance EventEmitter.  It has
been micro-optimized for various of code paths making this, one of, if not the
fastest EventEmitter available for Node.js and browsers.  The module is API
compatible with the EventEmitter that ships by default with Node.js but there
are some slight differences:
@itemize
@item Domain support has been removed.
@item We do not throw an error when you emit an error event and nobody is listening.
@item The newListener and removeListener events have been removed as they are useful only in some uncommon use-cases.
@item The setMaxListeners, getMaxListeners, prependListener and prependOnceListener methods are not available.
@item Support for custom context for events so there is no need to use fn.bind.
@item The removeListener method removes all matching listeners, not only the first.
@end itemize")
    (license license:expat)))

(define-public node-fast-safe-stringify
  (package
    (name "node-fast-safe-stringify")
    (version "2.0.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/davidmarkclements/fast-safe-stringify")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1x5bpd55q1xagh4kzviy30skfiwb5qb2n39qbpwyd0sn0jl9fw51"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-benchmark" ,node-benchmark)
       ;("node-clone" ,node-clone)
       ;("node-json-stringify-safe" ,node-json-stringify-safe)
       ;("node-standard" ,node-standard)
       ;("node-tap" ,node-tap)
       ))
    (home-page "https://github.com/davidmarkclements/fast-safe-stringify")
    (synopsis "Safely and quickly serialize JavaScript objects")
    (description "Safely and quickly serialize JavaScript objects.")
    (license license:expat)))

(define-public node-fecha
  (package
    (name "node-fecha")
    (version "3.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/taylorhakes/fecha")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0xsvd2c2p4qkrnfy0ncg630w9967w0aphz2mh5nslg9r5nr3m3b6"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)) ; 'npm run build' requires rollup
       ))
    (native-inputs
     `(
       ;("node-eslint" ,node-eslint)
       ;("node-nyc" ,node-nyc)
       ;("node-painless" ,node-painless)
       ;("node-rollup", node-rollup)
       ;("node-rollup-plugin-uglify" ,node-rollup-plugin-uglify)
       ))
    (home-page "https://github.com/taylorhakes/fecha")
    (synopsis "Lightweight Javascript Date formatting and parsing")
    (description "Lightweight Javascript Date formatting and parsing.")
    (license license:expat)))

(define-public node-fn.name
  (package
    (name "node-fn.name")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/3rd-Eden/fn.name")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1r0pgf3ra6wglqy8pvz61kjj5kx3f40ghpni3vbv8dy4jp7bpyyi"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-istanbul" ,node-istanbul)
       ;("node-mocha" ,node-mocha)
       ;("node-pre-commit" ,node-pre-commit)
       ))
    (home-page "https://github.com/3rd-Eden/fn.name")
    (synopsis "Extract the name from a function")
    (description "Extract the name from a function.")
    (license license:expat)))

(define-public node-follow-redirects
  (package
    (name "node-follow-redirects")
    (version "1.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/follow-redirects/follow-redirects")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1h0fikr6wwrcbc3g1wrv5cn84qn7icby2v0wwybhwd7h0ih6g5nm"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-debug" ,node-debug)))
    (native-inputs
     `(
       ;("node-concat-stream" ,node-concat-stream)
       ;("node-eslint" ,node-eslint)
       ;("node-express" ,node-express)
       ;("node-lolex" ,node-lolex)
       ;("node-mocha" ,node-mocha)
       ;("node-nyc" ,node-nyc)
       ))
    (home-page "https://www.npmjs.com/package/follow-redirects")
    (synopsis "Node.js module that automatically follows HTTP(S) redirects")
    (description "Drop-in replacement for Node's http and https modules that automatically follows redirects.")
    (license license:expat)))

(define-public node-http-proxy
  (package
    (name "node-http-proxy")
    (version "1.18.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/http-party/node-http-proxy")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1mgr7cm4smsrvx2jqlagsldz0h1mqc2by59qdhidcpkgbk7xj8h2"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-eventemitter3" ,node-eventemitter3)
       ("node-requires-port" ,node-requires-port)
       ("node-follow-redirects" ,node-follow-redirects)))
    (native-inputs
     `(
       ("node-async" ,node-async)
       ;("node-auto-changelog" ,node-autochangelog)
       ;("node-concat-stream" ,node-concat-stream)
       ;("node-expect-js" ,node-expect-js)
       ;("node-mocha" ,node-mocha)
       ;("node-nyc" ,node-nyc)
       ;("node-semver" ,node-semver)
       ;("node-socket-io" ,node-socket-io)
       ;("node-socket-io-client" ,node-socket-io-client)
       ;("node-sse" ,node-sse)
       ;("node-ws" ,node-ws)
       ))
    (home-page "https://github.com/http-party/node-http-proxy")
    (synopsis "Full-featured http proxy for node.js")
    (description "@code{node-http-proxy} is an HTTP programmable proxying
library that supports websockets.  It is suitable for implementing components
such as reverse proxies and load balancers.")
    (license license:expat)))

(define-public node-is-arrayish
  (package
    (name "node-is-arrayish")
    (version "0.3.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Qix-/node-is-arrayish.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0j0w190b52ysizc3xl2pykbm3iln1x5yis3mb9hsyscv0174w047"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-coffee-script" ,node-coffee-script)
       ;("node-coveralls" ,node-coveralls)
       ;("node-eslint" ,node-eslint)
       ;("node-istanbul" ,node-istanbul)
       ;("node-mocha" ,node-mocha)
       ;("node-should" ,node-should)
       ))
    (home-page "https://github.com/Qix-/node-is-arrayish")
    (synopsis "Check if an object can be used like an Array")
    (description "Check if an object can be used like an Array.")
    (license license:expat)))

(define-public node-is-stream
  (package
    (name "node-is-stream")
    (version "2.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sindresorhus/is-stream")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0vpr89p5zx16jbqz9bhj22x8112v7hfn4nx1kz68mbczcild0p0s"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-ava" ,node-ava)
       ;("node-tempy" ,node-tempy)
       ;("node-tsd" ,node-tsd)
       ;("node-xo" ,node-xo)
       ))
    (home-page "https://github.com/sindresorhus/is-stream")
    (synopsis "Check if something is a Node.js stream")
    (description "Check if something is a Node.js stream.")
    (license license:expat)))

(define-public node-isarray
  (package
    (name "node-isarray")
    (version "2.0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/juliangruber/isarray")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "16693czcxd2pasbk4nad403szl1ns6sq7r0vxmyj5l5d20sajsm4"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)))) ; 'make' runs the tests by default.
    ;(native-inputs
    ; `(("node-tape" ,node-tape)))
    (home-page "https://github.com/juliangruber/isarray")
    (synopsis "Array#isArray for older browsers")
    (description "Array#isArray for older browsers and deprecated Node.js versions.")
    (license license:expat)))

(define-public node-kuler
  (package
    (name "node-kuler")
    (version "2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/3rd-Eden/kuler")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "03513djgh0mp1lfi28gn1a72jx7b17hdb5p3y6pz127vd8afj3az"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-mocha" ,node-mocha)
       ))
    (home-page "https://github.com/3rd-Eden/kuler")
    (synopsis "Color your terminal using CSS/hex color codes")
    (description "Color your terminal using CSS/hex color codes.")
    (license license:expat)))

(define-public node-lodash
  (package
    (name "node-lodash")
    (version "4.17.15")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lodash/lodash")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1hp04cg3b59j3dpnvzixd5p6wpv34mj2pnq8wp60csv3p2s0qk4y"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-async" ,node-async)
       ;("node-benchmark" ,node-benchmark)
       ;("node-chalk" ,node-chalk)
       ;("node-cheerio" ,node-cheerio)
       ;("node-codecov.io" ,node-codecov.io)
       ;("node-coveralls" ,node-coveralls)
       ;("node-curl-amd" ,node-curl-amd)
       ;("node-docdown" ,node-docdown)
       ;("node-dojo" ,node-dojo)
       ;("node-ecstatic" ,node-ecstatic)
       ;("node-fs-extra" ,node-fs-extra)
       ;("node-glob" ,node-glob)
       ;("node-istanbul" ,node-istanbul)
       ;("node-jquery" ,node-jquery)
       ;("node-jscs" ,nodejscd)
       ;("node-lodash" ,node-lodash) ; version-x.y.(z-1)
       ;("node-lodash-doc-globals" ,node-lodash-doc-globals)
       ;("node-markdown-doctest" ,node-markdown-doctest)
       ;("node-optional-dev-dependency" ,node-optional-dev-dependency)
       ;("node-platform" ,node-platform)
       ;("node-qunit-extras" ,node-qunit-extras)
       ;("node-qunitjs" ,node-qunitjs)
       ;("node-request" ,node-request)
       ;("node-requirejs" ,node-requirejs)
       ;("node-sauce-tunnel" ,node-sauce-tunnel)
       ("node-uglify-js" ,node-uglify-js)
       ;("node-webpack" ,node-webpack)
       ))
    (home-page "https://lodash.com/")
    (synopsis "A modern JavaScript utility library delivering modularity, performance, & extras")
    (description "A modern JavaScript utility library delivering modularity, performance, & extras.")
    (license license:expat)))

(define-public node-logform
  (package
    (name "node-logform")
    (version "2.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/winstonjs/logform")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "08yljxd888zzw1qn1w4hr3fyj997gvdi9v9dva03azyxp2db466a"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)))) ; 'build needs rimraf and babel.
    (inputs
     `(("node-colors" ,node-colors)
       ("node-fast-safe-stringify" ,node-fast-safe-stringify)
       ("node-fecha" ,node-fecha)
       ("node-ms" ,node-ms)
       ("node-triple-beam" ,node-triple-beam)))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-babel" ,node-babel)
       ;("node-eslint-config-populist" ,node-eslint-config-populist)
       ;("node-mocha" ,node-mocha)
       ;("node-nyc" ,node-nyc)
       ;("node-rimraf" ,node-rimraf)
       ))
    (home-page "https://github.com/winstonjs/logform")
    (synopsis "A mutable object format designed for chaining & objectMode streams")
    (description "A mutable object format designed for chaining & objectMode streams.")
    (license license:expat)))

(define-public node-lynx
  (package
    (name "node-lynx")
    (version "0.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "git://github.com/dscape/lynx.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1l7pl8qm7kcafsafh8iyzb4cwh344kwlg6gp9ab0bmwaqby6vhzp"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-mersenne" ,node-mersenne)
       ("node-statsd-parser" ,node-statsd-parser)))
    ;(native-inputs
    ; `(("node-tap" ,node-tap)))
    (home-page "https://github.com/dscape/lynx")
    (synopsis "node.js client for Etsy'd StatsD server")
    (description "Minimalistic StatsD client for Node.js programs.")
    (license license:expat)))

(define-public node-one-time
  (package
    (name "node-one-time")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/3rd-Eden/one-time")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0k8b8radmha4cfpv31xa7wf77w6bpsyq3h387yy4vlf9jbg4acpf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)))) ; 'build needs rimraf and babel.
    (inputs
     `(("node-fn.name" ,node-fn.name)))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-mocha" ,node-mocha)
       ;("node-nyc" ,node-nyc)
       ))
    (home-page "https://github.com/3rd-Eden/one-time")
    (synopsis "Run the supplied function exactly one time (once)")
    (description "Run the supplied function exactly one time (once).")
    (license license:expat)))

(define-public node-process-nextick-args
  (package
    (name "node-process-nextick-args")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/calvinmetcalf/process-nextick-args")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "00g4294ijw12mfq5jjicn9q2f6s4hgxmwb7vng09lzmbhhd8jss2"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    ;(native-inputs
    ; `(("node-tap" ,node-tap)))
    (home-page "https://github.com/calvinmetcalf/process-nextick-args")
    (synopsis "process.nextTick always accepts args, always")
    (description "Always be able to pass arguments to process.nextTick, no matter the platform.")
    (license license:expat)))

(define-public node-promise
  (package
    (name "node-promise")
    (version "8.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/then/promise.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1bwfmhvmj9mmkqawpqf09n4b1gnlmbvkjn9gfm2n8i51vsw12ffz"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-asap" ,node-asap)))
    (native-inputs
     `(
       ("node-acorn" ,node-acorn)
       ;("node-better-assert" ,node-better-assert)
       ;("node-istanbul" ,node-istanbul)
       ;("node-mocha" ,node-mocha)
       ;("node-promises-aplus-tests" ,node-promises-aplus-tests)
       ;("node-rimraf" ,node-rimraf)
       ))
    (home-page "https://developers.google.com/closure/library/")
    (synopsis "Google's common JavaScript library")
    (description "Closure Library is a powerful, low-level JavaScript library
designed for building complex and scalable web applications.  It is used by many
Google web applications, such as Google Search, Gmail, Google Docs, Google+
Google Maps, and others.")
    (license license:asl2.0)))

(define-public node-requires-port
  (package
    (name "node-requires-port")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/unshiftio/requires-port")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "051ii93g03np6pnlmjkpqhikq1awqaybqbm85pz1x78wb8synkx0"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-istanbul" ,node-istanbul)
       ;("node-mocha" ,node-mocha)
       ;("node-pre-commit" ,node-pre-commit)
       ))
    (home-page "https://github.com/unshiftio/requires-port")
    (synopsis "Check if a protocol requires a certain port number to be added to an URL.")
    (description "Check if a protocol requires a certain port number to be added to an URL.")
    (license license:expat)))

(define-public node-simple-swizzle
  (package
    (name "node-simple-swizzle")
    (version "0.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Qix-/node-simple-swizzle")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1j6gyqax3w675q1c8gxh2fmk9swkyplhkci96b2qxhkl1aads8qi"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(("node-is-arrayish" ,node-is-arrayish)))
    (native-inputs
     `(
       ;("node-coffee-script" ,node-coffee-script)
       ;("node-coveralls" ,node-coveralls)
       ;("node-istanbul" ,node-istanbul)
       ;("node-mocha" ,node-mocha)
       ;("node-should" ,node-should)
       ;("node-xo" ,node-xo)
       ))
    (home-page "https://github.com/Qix-/node-simple-swizzle")
    (synopsis "Simply swizzle your arguments")
    (description "Simply swizzle your arguments.")
    (license license:expat)))

(define-public node-storage-engine
  (package
    (name "node-storage-engine")
    (version "3.0.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/3rd-Eden/storage-engine")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "07axbp05wmadbmpdphxgyd9siq9z5glf2z626gvg4wm1s3qv37lx"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)))) ; build wants babel
    (inputs
     `(("node-enabled" ,node-enabled)
       ("node-eventemitter3" ,node-eventemitter3)))
    (native-inputs
     `(
       ;(:node-babel" ,node-babel)
       ;("node-assume" ,node-assume)
       ;("node-asyncstorageapi" ,node-asyncstorageapi)
       ;("node-mocha" ,node-mocha)
       ;("node-react-native" ,node-react-native)
       ;("node-require-poisoning" ,node-require-poisoning)
       ;("node-setup-env" ,node-setup-env)
       ))
    (home-page "https://github.com/3rd-Eden/storage-engine")
    (synopsis "EventEmitter abstraction on top the React-Native AsyncStorage API")
    (description "EventEmitter abstraction on top the React-Native AsyncStorage API.")
    (license license:expat)))

(define-public node-strftime
  (package
    (name "node-strftime")
    (version "0.10.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "git://github.com/samsonjs/strftime.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "131nmlivazwxyba25kh9lda99749fq4xsyin6lzfalaaydviby4p"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)))) ; wants 'closure' binary to minify strftime.js.
    ;(native-inputs
    ; `(("node-closure" ,node-closure)))
    (home-page "https://github.com/samsonjs/strftime")
    (synopsis "Strftime for JavaScript")
    (description "@code{strftime} for JavaScript.  Works in (at least) node.js
and browsers.  Supports localization and timezones.  Most standard specifiers
from C are supported as well as some other extensions from Ruby.")
    (license license:expat)))

(define-public node-text-hex
  (package
    (name "node-text-hex")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/3rd-Eden/text-hex")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "10qk72yq7l8zrs099b88k4nag43bamqp5wilv6hg8601qdgbnh2w"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-mocha" ,node-mocha)
       ;("node-pre-commit" ,node-pre-commit)
       ))
    (home-page "https://github.com/3rd-Eden/text-hex")
    (synopsis "Create a hash from a string of text and transforms it to a color")
    (description "Create a hash from a string of text and transforms it to a color.")
    (license license:expat)))

(define-public node-triple-beam
  (package
    (name "node-triple-beam")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/winstonjs/triple-beam")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "03dk13shq6780mmsx4c6y22v5zhf4800b4w92pd2j82w905s9vv5"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     `(
       ;("node-assume" ,node-assume)
       ;("node-eslint-config-populist" ,node-eslint-config-populist)
       ;("node-mocha" ,node-mocha)
       ;("node-nyc" ,node-nyc)
       ))
    (home-page "https://github.com/winstonjs/triple-beam")
    (synopsis "Definitions of levels for logging purposes & shareable Symbol constants")
    (description "Definitions of levels for logging purposes & shareable Symbol constants.")
    (license license:expat)))

(define-public node-winston
  (package
    (name "node-winston")
    (version "3.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/winstonjs/winston.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "14v2pxa6xs9j1zkjszw94kc5njcmrb3c1s1hs8rnyfjxaf5hb32c"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)))) ; 'build needs rimraf and babel.
    (inputs
     `(("node-async" ,node-async)
       ("node-diagnostics" ,node-diagnostics)
       ("node-is-stream" ,node-is-stream)
       ("node-logform" ,node-logform)
       ("node-one-time" ,node-one-time)
       ("node-readable-stream" ,node-readable-stream)
       ("node-stack-trace" ,node-stack-trace)
       ("node-triple-beam" ,node-triple-beam)
       ("node-winston-transport" ,node-winston-transport)))
    (native-inputs
     `(
       ;("node-babel" ,node-babel)
       ;("node-rimraf" ,node-rimraf)
       ))
    (home-page "https://github.com/winstonjs/winston")
    (synopsis "logger for just about everything")
    (description "A logger for just about everything.")
    (license license:expat)))

(define-public node-winston-2
  (package
    (inherit node-winston)
    (name "node-winston")
    (version "2.4.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/winstonjs/winston.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0wiihp0n1pygjynlky4751wfn2x5d46f18diahgnaq1ryppvxjj1"))))
    (arguments '(#:tests? #f))
    (inputs
     `(("node-async" ,node-async-1)
       ("node-colors" ,node-colors)
       ;("node-cycle" ,node-cycle)
       ;("node-eyes" ,node-eyes)
       ;("node-isstream" ,node-isstream)
       ("node-stack-trace" ,node-stack-trace)))
    (native-inputs
     `(
       ;("node-cross-spawn-async" ,node-cross-spawn-async)
       ;("node-hock" .node-hock)
       ;("node-std-mocks" ,node-mocks)
       ;("node-vows" ,node-vows)
       ))))

(define-public node-winston-transport
  (package
    (name "node-winston-transport")
    (version "4.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/winstonjs/winston-transport")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0lq50adzzb8xiy7xrbkl6g6z6lv0rhm7zppkmrqjm0km58qg8fbf"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)))) ; 'build needs rimraf and babel.
    (inputs
     `(("node-readable-stream" ,node-readable-stream)
       ("node-triple-beam" ,node-triple-beam)))
    (native-inputs
     `(
       ;("node-abstract-winston-transport" ,node-abstract-winston-transport)
       ;("node-assume" ,node-assume)
       ;("node-babel-cli" ,node-babel-cli)
       ;("node-babel-preset-env" ,node-babel-preset-env)
       ;("node-eslint-config-populist" ,node-eslint-config-populist)
       ("node-logform" ,node-logform)
       ;("node-mocha" ,node-mocha)
       ;("node-nyc" ,node-nyc)
       ;("node-rimraf" ,node-rimraf)
       ;("node-winston-compat" ,node-winston-compat)
       ))
    (home-page "https://github.com/winstonjs/winston-transport")
    (synopsis "Base stream implementations for winston@@3 and up")
    (description "The base TransportStream implementation for winston >= 3. Use these to write ecosystem Transports for winston.")
    (license license:expat)))

(define-public node-xregexp
  (package
    (name "node-xregexp")
    (version "4.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/slevithan/xregexp")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1pwbr8j6g561kp5980gk4l45azn4q14ab1vhxl2ib8fp44nhyphl"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    ;(inputs
    ; `(("node-babel-runtime-corejs2" ,node-babel-runtime-corejs2)))
    (native-inputs
     `(
       ;("node-babel-cli" ,node-babel-cli)
       ;("node-babel-core" ,node-babel-core)
       ;("node-babel-plugin-transform-runtime" ,node-babel-plugin-transform-runtime)
       ;("node-babel-preset-env" ,node-babel-preset-env)
       ;("node-babel-plugin-add-module-exports" ,node-babel-plugin-add-module-exports)
       ;("node-babel-plugin-array-includes" ,node-babel-plugin-array-includes)
       ;("node-babel-plugin-transform-xregexp" ,node-babel-plugin-transform-xregexp)
       ;("node-browserify" ,node-browserify)
       ;("node-eslint" ,node-eslint)
       ;("node-jasmine" ,node-jasmine)
       ;("node-jsesc" ,node-jsesc)
       ;("node-unicode-11.0.0" ,node-unicode-11.0.0)
       ;("node-unicode-property-value-aliases" ,node-unicode-property-value-aliases)
       ;("node-zuul" ,node-zuul)
       ))
    (home-page "http://xregexp.com/")
    (synopsis "Extended JavaScript regular expressions")
    (description "Extended JavaScript regular expressions/")
    (license license:expat)))

(define-public node-yargs
  (package
    (name "node-yargs")
    (version "15.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yargs/yargs")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0lfgph477cchjm4qrab9ni0wj9i6psbhy4mvpzn2jkrcfcffgvfi"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(
       ;("node-cliui" ,node-cliui)
       ("node-decamelize" ,node-decamelize)
       ;("node-find-up" ,node-find-up)
       ;("node-get-caller-file" ,node-get-caller-file)
       ;("node-require-directory" ,node-require-directory)
       ;("node-require-main-filename" ,node-require-main-filename)
       ;("node-set-blocking" ,node-set-blocking)
       ;("node-string-width" ,node-string-width)
       ;("node-which-module" ,node-which-module)
       ;("node-y18n" ,node-y18n)
       ;("node-yargs-parser" ,node-yargs-parser)
       ))
    (native-inputs
     `(
       ;("node-c8" ,node-c8)
       ;("node-chai" ,node-chai)
       ;("node-chalk" ,node-chalk)
       ;("node-coveralls" ,node-coveralls)
       ;("node-cpr" ,node-cpr)
       ;("node-cross-spawn" ,node-cross-spawn)
       ;("node-es6-promise" ,node-es6-promise)
       ;("node-hashish" ,node-hashish)
       ;("node-mocha" ,node-mocha)
       ;("node-rimraf" ,node-rimraf)
       ;("node-standard" ,node-standard)
       ;("node-which" ,node-which)
       ;("node-yargs-test-extends" ,node-yargs-test-extends)
       ))
    (home-page "http://yargs.js.org/")
    (synopsis "yargs the modern, pirate-themed successor to optimist")
    (description "yargs the modern, pirate-themed successor to optimist.")
    (license license:expat)))

(define-public node-yargs-parser
  (package
    (name "node-yargs-parser")
    (version "16.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yargs/yargs-parser")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "066wl3609ymg9wmrjlcya3c7sv9ba03dzg8qqmzvqxrkz894s89x"))))
    (build-system node-build-system)
    (arguments '(#:tests? #f))
    (inputs
     `(
       ;("node-camelcase" ,node-camelcase)
       ("node-decamelize" ,node-decamelize)
       ))
    (native-inputs
     `(
       ;("node-c8" ,node-c8)
       ;("node-chai" ,node-chai)
       ;("node-coveralls" ,node-coveralls)
       ;("node-mocha" ,node-mocha)
       ;("node-standard" ,node-standard)
       ;("node-standard-version" ,node-standard-version)
       ))
    (home-page "http://yargs.js.org/")
    (synopsis "the mighty option parser used by yargs")
    (description "the mighty option parser used by yargs.")
    (license license:isc)))
