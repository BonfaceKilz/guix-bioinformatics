(define-module (gn packages images)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  )

;; Officially kaleido is built against 88.0.4324.150, but this is close enough. We have to copy a bunch of options from the Guix chromium package because it is so complex and fragile, making it hard to just inherit from that previous version. The rest we can use against the inherited inferior.

;; Adapted from the manual:
(define %channel-commit "a58f9966a50b6cb558fa57b369a5b16de02af255")
(define channels
  ;; This is the old revision from which we want to
  ;; extract ungoogled-chromium.
  (list (channel
          (name 'guix)
          (url "https://git.savannah.gnu.org/git/guix.git")
          (commit %channel-commit))))

(define inferior
    ;; An inferior representing the above revision.
      (inferior-for-channels channels))

(define inferior-ungoogled-chromium
  (first (lookup-inferior-packages inferior "ungoogled-chromium")))

;; Borrowed from commit a58f9966a50b6cb558fa57b369a5b16de02af255 from Guix proper.
(define %preserved-third-party-files
  '("base/third_party/cityhash" ;Expat
    "base/third_party/double_conversion" ;BSD-3
    "base/third_party/dynamic_annotations" ;BSD-2
    "base/third_party/icu" ;Unicode, X11-style
    "base/third_party/superfasthash" ;BSD-3
    "base/third_party/symbolize" ;BSD-3
    "base/third_party/xdg_mime" ;LGPL2.0+ or Academic 2.0
    "base/third_party/xdg_user_dirs" ;Expat
    ;; XXX: Chromium requires a newer C++ standard library.  Remove this when
    ;; the default GCC is 9 or later.
    "buildtools/third_party/libc++" ;ASL2.0, with LLVM exceptions
    "chrome/third_party/mozilla_security_manager" ;MPL-1.1/GPL2+/LGPL2.1+
    "courgette/third_party/bsdiff" ;BSD-2, BSD protection license
    "courgette/third_party/divsufsort" ;Expat
    "net/third_party/mozilla_security_manager" ;MPL-1.1/GPL2+/LGPL2.1+
    "net/third_party/nss" ;MPL-2.0
    "net/third_party/quiche" ;BSD-3
    "net/third_party/uri_template" ;ASL2.0
    "third_party/abseil-cpp" ;ASL2.0
    "third_party/adobe/flash/flapper_version.h" ;no license, trivial
    "third_party/angle" ;BSD-3
    "third_party/angle/src/common/third_party/base" ;BSD-3
    "third_party/angle/src/common/third_party/smhasher" ;Public domain
    "third_party/angle/src/common/third_party/xxhash" ;BSD-2
    "third_party/angle/src/third_party/compiler" ;BSD-2
    "third_party/angle/src/third_party/libXNVCtrl" ;Expat
    "third_party/angle/src/third_party/trace_event" ;BSD-3
    "third_party/angle/src/third_party/volk" ;Expat
    "third_party/angle/third_party/vulkan-headers" ;ASL2.0
    "third_party/angle/third_party/vulkan-loader" ;ASL2.0
    "third_party/angle/third_party/vulkan-tools" ;ASL2.0
    "third_party/angle/third_party/vulkan-validation-layers" ;ASL2.0
    "third_party/apple_apsl" ;APSL2.0
    "third_party/axe-core" ;MPL2.0
    "third_party/blink" ;BSD-3, LGPL2+
    "third_party/boringssl" ;OpenSSL/ISC (Google additions are ISC)
    "third_party/boringssl/src/third_party/fiat" ;Expat
    "third_party/breakpad" ;BSD-3
    "third_party/brotli" ;Expat
    "third_party/catapult" ;BSD-3
    "third_party/catapult/common/py_vulcanize/third_party/rcssmin" ;ASL2.0
    "third_party/catapult/common/py_vulcanize/third_party/rjsmin" ;ASL2.0
    "third_party/catapult/third_party/polymer" ;BSD-3
    ;; XXX: This is a minified version of <https://d3js.org/>.
    "third_party/catapult/tracing/third_party/d3" ;BSD-3
    "third_party/catapult/tracing/third_party/gl-matrix" ;Expat
    "third_party/catapult/tracing/third_party/jpeg-js" ;ASL2.0
    ;; XXX: Minified version of <https://github.com/Stuk/jszip>.
    "third_party/catapult/tracing/third_party/jszip" ;Expat or GPL3
    "third_party/catapult/tracing/third_party/mannwhitneyu" ;Expat
    "third_party/catapult/tracing/third_party/oboe" ;BSD-2
    ;; XXX: Minified version of <https://github.com/nodeca/pako>.
    "third_party/catapult/tracing/third_party/pako" ;Expat
    "third_party/ced" ;BSD-3
    "third_party/cld_3" ;ASL2.0
    "third_party/closure_compiler" ;ASL2.0
    "third_party/crashpad" ;ASL2.0
    "third_party/crashpad/crashpad/third_party/lss" ;ASL2.0
    "third_party/crashpad/crashpad/third_party/zlib/zlib_crashpad.h" ;Zlib
    "third_party/crc32c" ;BSD-3
    "third_party/cros_system_api" ;BSD-3
    "third_party/dav1d" ;BSD-2
    "third_party/dawn" ;ASL2.0
    "third_party/depot_tools/owners.py" ;BSD-3
    "third_party/devtools-frontend" ;BSD-3
    "third_party/devtools-frontend/src/front_end/third_party/acorn" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party/axe-core" ;MPL2.0
    "third_party/devtools-frontend/src/front_end/third_party/chromium" ;BSD-3
    "third_party/devtools-frontend/src/front_end/third_party/codemirror" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party/fabricjs" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party/i18n" ;ASL2.0
    "third_party/devtools-frontend/src/front_end/third_party/intl-messageformat" ;BSD-3
    "third_party/devtools-frontend/src/front_end/third_party/lighthouse" ;ASL2.0
    "third_party/devtools-frontend/src/front_end/third_party/lit-html" ;BSD-3
    "third_party/devtools-frontend/src/front_end/third_party/lodash-isequal" ;Expat
    "third_party/devtools-frontend/src/front_end/third_party/marked" ;Expat, BSD-3
    "third_party/devtools-frontend/src/front_end/third_party/puppeteer" ;ASL2.0
    "third_party/devtools-frontend/src/front_end/third_party/wasmparser" ;ASL2.0
    "third_party/devtools-frontend/src/third_party/pyjson5" ;ASL2.0
    "third_party/devtools-frontend/src/third_party/typescript" ;ASL2.0
    "third_party/dom_distiller_js" ;BSD-3
    "third_party/emoji-segmenter" ;ASL2.0
    "third_party/flatbuffers" ;ASL2.0
    "third_party/fusejs" ;ASL2.0
    "third_party/glslang" ;BSD-3, Expat, ASL2.0
    "third_party/google_input_tools" ;ASL2.0
    "third_party/google_input_tools/third_party/closure_library" ;ASL2.0
    "third_party/google_input_tools/third_party/closure_library/third_party/closure" ;Expat
    "third_party/googletest" ;BSD-3
    "third_party/harfbuzz-ng" ;Expat
    "third_party/hunspell" ;MPL1.1/GPL2+/LGPL2.1+
    "third_party/iccjpeg" ;IJG
    "third_party/inspector_protocol" ;BSD-3
    "third_party/jinja2" ;BSD-3
    ;; XXX: Unbundle this when switching back to libstdc++.
    "third_party/jsoncpp" ;Public Domain or Expat
    "third_party/jstemplate" ;ASL2.0
    "third_party/khronos" ;Expat, SGI
    "third_party/leveldatabase" ;BSD-3
    "third_party/libavif" ;BSD-2
    "third_party/libXNVCtrl" ;Expat
    "third_party/libaddressinput" ;ASL2.0
    "third_party/libaom" ;BSD-2 or "Alliance for Open Media Patent License 1.0"
    "third_party/libaom/source/libaom/third_party/vector" ;Expat
    "third_party/libaom/source/libaom/third_party/x86inc" ;ISC
    "third_party/libgifcodec" ;MPL1.1/GPL2+/LGPL2.1+, BSD-3, BSD-2
    "third_party/libjingle_xmpp" ;BSD-3
    "third_party/libphonenumber" ;ASL2.0
    "third_party/libsecret" ;LGPL2.1+
    "third_party/libsrtp" ;BSD-3
    "third_party/libsync" ;ASL2.0

    ;; This line is added to work around compile time failures.
    "third_party/libusb" ;LGPL2.1+
    "third_party/libudev" ;LGPL2.1+

    ;; FIXME: build/linux/unbundle/libvpx.gn does not work for all users.
    "third_party/libvpx" ;BSD-3
    "third_party/libvpx/source/libvpx/third_party/x86inc" ;Expat

    "third_party/libwebm" ;BSD-3
    "third_party/libxml/chromium" ;BSD-3
    "third_party/libyuv" ;BSD-3
    "third_party/lottie" ;Expat
    "third_party/lss" ;BSD-3
    "third_party/mako" ;Expat
    "third_party/markupsafe" ;BSD-3
    "third_party/mesa_headers" ;Expat, SGI
    "third_party/metrics_proto" ;BSD-3
    "third_party/modp_b64" ;BSD-3
    "third_party/nasm" ;BSD-2
    "third_party/nearby" ;ASL2.0
    "third_party/node" ;Expat
    "third_party/node/node_modules/polymer-bundler/lib/third_party/UglifyJS2" ;BSD-2
    "third_party/one_euro_filter" ;BSD-3
    "third_party/openscreen" ;BSD-3
    "third_party/openscreen/src/third_party/tinycbor" ;Expat
    "third_party/openscreen/src/third_party/mozilla" ;MPL1.1/GPL2+/LGPL2.1+, BSD-3
    "third_party/ots" ;BSD-3
    "third_party/pdfium" ;BSD-3
    "third_party/pdfium/third_party/agg23" ;Expat
    "third_party/pdfium/third_party/base" ;BSD-3
    "third_party/pdfium/third_party/bigint" ;Public domain, BSD-3
    "third_party/pdfium/third_party/skia_shared" ;BSD-3
    "third_party/pdfium/third_party/freetype/include/pstables.h" ;FreeType
    "third_party/perfetto" ;ASL2.0
    "third_party/pffft" ;the "FFTPACK" license, similar to BSD-3
    "third_party/ply" ;BSD-3
    "third_party/polymer" ;BSD-3
    "third_party/private_membership" ;ASL2.0
    "third_party/private-join-and-compute" ;ASL2.0
    "third_party/protobuf" ;BSD-3
    "third_party/protobuf/third_party/six" ;Expat
    "third_party/pyjson5" ;ASL2.0
    "third_party/qcms" ;Expat
    ;; XXX: System re2 cannot be used when Chromium uses libc++ because the re2
    ;; ABI relies on libstdc++ internals.  See build/linux/unbundle/re2.gn.
    "third_party/re2" ;BSD-3
    "third_party/rnnoise" ;BSD-3
    "third_party/s2cellid" ;ASL2.0
    "third_party/schema_org" ;CC-BY-SA3.0
    "third_party/securemessage" ;ASL2.0
    "third_party/shaka-player" ;ASL2.0
    "third_party/shell-encryption" ;ASL2.0
    "third_party/skia" ;BSD-3
    "third_party/skia/include/third_party/skcms" ;BSD-3
    "third_party/skia/third_party/skcms" ;BSD-3
    "third_party/skia/third_party/vulkanmemoryallocator" ;BSD-3, Expat
    "third_party/smhasher" ;Expat, public domain
    "third_party/speech-dispatcher" ;GPL2+
    "third_party/spirv-headers" ;ASL2.0
    "third_party/SPIRV-Tools" ;ASL2.0
    "third_party/sqlite" ;Public domain
    "third_party/swiftshader" ;ASL2.0
    "third_party/swiftshader/third_party/astc-encoder" ;ASL2.0
    "third_party/swiftshader/third_party/llvm-10.0" ;ASL2.0, with LLVM exception
    "third_party/swiftshader/third_party/llvm-subzero" ;NCSA
    "third_party/swiftshader/third_party/marl" ;ASL2.0
    "third_party/swiftshader/third_party/subzero" ;NCSA
    "third_party/swiftshader/third_party/SPIRV-Headers" ;X11-style
    "third_party/tcmalloc/chromium" ;BSD-3
    "third_party/ukey2" ;ASL2.0
    "third_party/usb_ids" ;BSD-3
    "third_party/usrsctp" ;BSD-2
    "third_party/vulkan_memory_allocator" ;Expat
    "third_party/wayland/protocol" ;Expat
    "third_party/wayland/stubs" ;BSD-3, Expat
    "third_party/wayland/wayland_scanner_wrapper.py" ;BSD-3
    "third_party/wayland-protocols" ;Expat
    "third_party/web-animations-js" ;ASL2.0
    "third_party/webdriver" ;ASL2.0
    "third_party/webrtc" ;BSD-3
    "third_party/webrtc/common_audio/third_party/ooura" ;Non-copyleft
    "third_party/webrtc/common_audio/third_party/spl_sqrt_floor" ;Public domain
    "third_party/webrtc/modules/third_party/fft" ;Non-copyleft
    "third_party/webrtc/modules/third_party/g711" ;Public domain
    "third_party/webrtc/modules/third_party/g722" ;Public domain
    "third_party/webrtc/rtc_base/third_party/base64" ;Non-copyleft
    "third_party/webrtc/rtc_base/third_party/sigslot" ;Public domain
    "third_party/widevine/cdm/widevine_cdm_version.h" ;BSD-3
    "third_party/widevine/cdm/widevine_cdm_common.h" ;BSD-3
    "third_party/woff2" ;ASL2.0
    "third_party/xcbproto" ;X11
    "third_party/xdg-utils" ;Expat

    ;; These are forked components of the X11 keybinding code.
    "third_party/libxcb-keysyms" ;X11
    "third_party/libx11/src/KeyBind.c" ;X11
    "third_party/libx11/src/xkb/XKBBind.c" ;X11
    "third_party/x11proto/keysymdef.h" ;X11

    "third_party/zlib/google" ;BSD-3
    "third_party/zxcvbn-cpp" ;Expat
    "url/third_party/mozilla" ;BSD-3, MPL1.1/GPL2+/LGPL2.1+
    "v8/src/third_party/siphash" ;Public domain
    "v8/src/third_party/utf8-decoder" ;Expat
    "v8/src/third_party/valgrind" ;BSD-4
    "v8/third_party/inspector_protocol" ;BSD-3
    "v8/third_party/v8/builtins"))

(define %blacklisted-files
  ;; 'third_party/blink/perf_tests/resources/svg/HarveyRayner.svg' carries a
  ;; nonfree license according to LICENSES in the same directory.  As we don't
  ;; run the Blink performance tests, just remove everything to save ~70MiB.
  '("third_party/blink/perf_tests"))

(define (debian-patch name revision hash)
  (origin
    (method url-fetch)
    (uri (string-append "https://salsa.debian.org/chromium-team/chromium/-/raw/"
                        revision "/debian/patches/" name))
    (file-name (match (string-split name #\/)
                 ((category name)
                  (string-append "ungoogled-chromium-" category "-" name))))
    (sha256 (base32 hash))))

(define %chromium-version "88.0.4324.182")
(define %ungoogled-revision "b98f2d51406c84a75df96f0da9dee3c0d790963d")
(define %debian-revision "debian/84.0.4147.105-1")

(define %debian-patches
  (list (debian-patch "system/zlib.patch" %debian-revision
                      "09vqgs37w9ycc7par14wa7rnvmg9bm0z9pqg6fyl3iqvpghyjyr4")
        (debian-patch "system/openjpeg.patch" %debian-revision
                      "0zd6v5njx1pc7i0y6mslxvpx5j4cq01mmyx55qcqx8qzkm0gm48j")))

(define %ungoogled-origin
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/Eloston/ungoogled-chromium")
                        (commit %ungoogled-revision)))
    (file-name (git-file-name "ungoogled-chromium"
                              (string-take %ungoogled-revision 7)))
    (sha256
     (base32
      "1c9y1dn9s06pskkjw2r8lsbplak8m2rwh4drixvjpif7b4cgdhay"))))

(define %guix-patches
  (list
    (origin
      (method url-fetch)
      (uri (string-append "https://git.savannah.gnu.org/cgit/guix.git/plain/gnu/packages/patches/ungoogled-chromium-extension-search-path.patch?id=" %channel-commit))
      (file-name "ungoogled-chromium-extension-search-path.patch")
      (sha256 (base32 "1yjlcp4jg0cfgrwijqqvlh9vfyxcz48v3ww3vdp3lpaz13ppvplz")))
    (origin
      (method url-fetch)
      (uri (string-append "https://git.savannah.gnu.org/cgit/guix.git/plain/gnu/packages/patches/ungoogled-chromium-system-nspr.patch?id=" %channel-commit))
      (file-name "ungoogled-chromium-system-nspr.patch")
      (sha256 (base32 "02a3y7paaxib8jrzhggwzmvk53s20avq5a6qvdial30wi270dx1h")))))

;; This is a source 'snippet' that does the following:
;; *) Applies various patches for unbundling purposes and libstdc++ compatibility.
;; *) Runs the ungoogled patch-, domain substitution-, and scrubbing scripts.
;; *) Prunes all third_party directories that are not explicitly preserved.
;; *) Adjusts "GN" build files such that system libraries are preferred.
(define ungoogled-chromium-snippet
  ;; Note: delay to cope with cyclic module imports at the top level.
  (delay
    #~(begin
        (let ((chromium-dir (getcwd)))
          (set-path-environment-variable
           "PATH" '("bin")
           (list #+patch #+python-wrapper #+xz))

          ;; Apply patches before running the ungoogled scripts because
          ;; domain substitution may break some of the patches.
          (format #t "Applying assorted build fixes...~%")
          (force-output)
          (for-each (lambda (patch)
                      (invoke "patch" "-p1" "--force" "--input"
                              patch "--no-backup-if-mismatch"))
                    (append '#+%debian-patches '#+%guix-patches))

          (with-directory-excursion #+%ungoogled-origin
            (format #t "Ungooglifying...~%")
            (force-output)
            (invoke "python" "utils/prune_binaries.py" chromium-dir
                    "pruning.list")
            (invoke "python" "utils/patches.py" "apply" chromium-dir
                    "patches")
            (invoke "python" "utils/domain_substitution.py" "apply" "-r"
                    "domain_regex.list" "-f" "domain_substitution.list"
                    "-c" "/tmp/domainscache.tar.gz" chromium-dir))

          ;; Run after the ungoogled scripts to avoid interfering with
          ;; patches or file lists.
          (format #t "Removing blacklisted files...~%")
          (force-output)
          (for-each delete-file-recursively '#$%blacklisted-files)

          (format #t "Pruning third party files...~%")
          (force-output)
          (apply invoke (string-append #+python-2 "/bin/python")
                 "build/linux/unbundle/remove_bundled_libraries.py"
                 "--do-remove" '#$%preserved-third-party-files)

          (format #t "Replacing GN files...~%")
          (force-output)
          (substitute* "tools/generate_shim_headers/generate_shim_headers.py"
            ;; The "is_official_build" configure option enables certain
            ;; release optimizations like those used in the commercial
            ;; Chrome browser.  Unfortunately it also requires using the
            ;; bundled libraries: lose that restriction.
            (("#if defined\\(OFFICIAL_BUILD\\)")
             "#if 0"))
          (invoke "python" "build/linux/unbundle/replace_gn_files.py"
                  "--system-libraries" "ffmpeg" "flac" "fontconfig"
                  "freetype" "harfbuzz-ng" "icu" "libdrm" "libevent"
                  "libjpeg" "libpng" "libwebp" "libxml" "libxslt"
                  "openh264" "opus" "snappy" "zlib")
          #t))))

(define (make-lld-wrapper lld)
  (define lld-as-ld
    (computed-file "lld-ld"
                   #~(begin
                       (mkdir #$output)
                       (mkdir (string-append #$output "/bin"))
                       (symlink #$(file-append lld "/bin/lld")
                                (string-append #$output "/bin/ld")))))

  ;; Create a wrapper for LLD that inserts appropriate -rpath entries.
  (define lld-wrapper
    (make-ld-wrapper "lld-wrapper"
                     #:binutils lld-as-ld))

  ;; Clang looks for an 'ld.lld' executable, so we need to symlink it back.
  (computed-file "lld-wrapped"
                 #~(begin
                     (mkdir #$output)
                     (mkdir (string-append #$output "/bin"))
                     (symlink #$(file-append lld-wrapper "/bin/ld")
                              (string-append #$output "/bin/lld"))
                     (symlink "lld" (string-append #$output "/bin/ld.lld")))))

;; Now lets start the package definition.
(define-public kaleido
  (package
    ;(inherit inferior-ungoogled-chromium)
    (inherit ungoogled-chromium)
    (name "kaleido")
    (version "0.2.1")
    (source (origin
    ;          (inherit (package-source ungoogled-chromium))
    ;          (modules '())
    ;          (snippet #f)))
              (method url-fetch)
              (uri (string-append "https://commondatastorage.googleapis.com"
                                  "/chromium-browser-official/chromium-"
                                  %chromium-version ".tar.xz"))
              (sha256
               (base32
                "10av060ix6lgsvv99lyvyy03r0m3zwdg4hddbi6dycrdxk1iyh9h"))
              (modules '((guix build utils)))
              (snippet (force ungoogled-chromium-snippet))))
    (build-system gnu-build-system)
    (arguments
     ;(substitute-keyword-arguments (package-arguments inferior-ungoogled-chromium)
     (substitute-keyword-arguments (package-arguments ungoogled-chromium)
       ((#:modules modules %gnu-build-system-modules)
        `((srfi srfi-1)
          ,@modules))
       ((#:tests? _ #f) #f)             ; TODO: enable after successfully building.
       ((#:configure-flags flags)
        ;`(list
        ;   "import(\"//build/args/headless.gn\")"
        ;   "enable_nacl=false"
        ;   "is_component_build=false"
        ;   "symbol_level=0"
        ;   "blink_symbol_level=0"
        ;   "is_debug=false"
        ;   "use_sysroot=false"
        ;   "safe_browsing_mode=0"
        ;   )
        `(append
           ;; First we modify the inherited configure-flags.
           (fold delete ,flags
                 '(
                   "use_pulseaudio=true"
                   "link_pulseaudio=true"
                   "use_vaapi=true"
                   "rtc_use_pipewire=true"
                   "rtc_link_pipewire=true"
                   ))
        ;   ,flags
           (list
             "import(\"//build/args/headless.gn\")"
             ;"enable_nacl=false"           ; duplicate
             "is_component_build=false"
             "symbol_level=0"
             "blink_symbol_level=0"
             ;"is_debug=false"              ; duplicate
             )
           )
        )
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'unpack-kaleido-source
             (lambda* (#:key inputs #:allow-other-keys)
               (copy-recursively (assoc-ref inputs "kaleido-source") ".")
               ))
           (add-before 'configure 'prepare-kaleido-build-environment
             (lambda _
               (with-output-to-file "repos/kaleido/version"
                 (lambda _
                   (format #t "~a~%" ,version)))
               (mkdir-p "out/Release")
               ;; Adapted from /repos/mac_scripts/build_kaleido.
               ;; There is no append-to-file macro.
               (let ((build.gn (open-file "headless/BUILD.gn" "a")))
                 (display "
executable(\"kaleido\") {
  sources = [ \"app/kaleido.cc\" ]
  deps = [
    \":headless_shell_lib\",
    \"//skia\",  # we need this to override font render hinting in headless build
  ]
}
" build.gn)
                 (close build.gn))

               (mkdir-p "headless/app")
               (copy-recursively "repos/kaleido/cc" "headless/app")
               ;(with-output-to-file "out/Release/args.gn"
               ;(lambda _
               ;  (format #t
               ;          "import(\"//build/args/headless.gn\")~@
               ;          enable_nacl=false~@
               ;          is_component_build=false~@
               ;          ~@
               ;          symbol_level=0~@
               ;          blink_symbol_level=0~@
               ;          is_debug=false~@
               ;          ~@
               ;          target_cpu=\"~a\"~%"
               ;          "x64")))   ; x86_64 -> x64, aarch64 -> arm64, armhf -> arm
               ))
           (replace 'configure
             (lambda* (#:key configure-flags #:allow-other-keys)
               (let ((args (string-join configure-flags " ")))
                 ;; Generate ninja build files.
                 (invoke "gn" "gen" "out/Release"
                         (string-append "--args=" args))

                 ;; Print the full list of supported arguments as well as
                 ;; their current status for convenience.
                 ;(format #t "Dumping configure flags...\n")
                 ;(invoke "gn" "args" "out/Release" "--list")
                 )))
           (replace 'build
             (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
               (invoke "ninja" "-C" "out/Release"
                       "-j" (if parallel-build?
                                (number->string (parallel-job-count))
                                "1")
                       "kaleido")

               (mkdir-p "repos/build/kaleido/bin")
               (install-file "out/Release/kaleido" "repos/build/kaleido/bin")
               (copy-recursively "out/Release/swiftshader/" "repos/build/kaleido/bin")
               (install-file "repos/kaleido/version" "repos/build/kaleido/")
               (install-file "LICENSE.txt" "repos/build/kakeido/")
               (install-file "README.md" "repos/build/kakeido/")

               (with-directory-excursion "repos/kaleido/py"
                 ;(invoke "python3" "setup.py" "package")    ; builds wheels
                 (invoke "python3" "setup.py" "package_source")
                 )
               ))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (with-directory-excursion "repos/kaleido/tests"
                   (invoke "pytest" "-s" "test_py/")))))
           ))
        ))
    (native-inputs
     `(
       ;("gn" ,gn)
       ("poppler" ,poppler)
       ("python3" ,python)
       ("python-pandas" ,python-pandas)
       ("python-plotly" ,python-plotly)
       ("python-pytest" ,python-pytest)
       ("kaleido-source"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/plotly/Kaleido")
                  (commit (string-append "v" version))))
           (file-name (git-file-name name version))
           (sha256
            (base32 "0p7vddwha4bmb0ihbvjgqwhwa7xmm7xnh2hff5r2xzz64rjcz47x"))))

       ;,@(inferior-package-native-inputs inferior-ungoogled-chromium)
       ;,@(package-native-inputs inferior-ungoogled-chromium)
       ,@(package-native-inputs ungoogled-chromium)

       ;("bison" ,(first (lookup-inferior-packages inferior "bison")))
       ;("clang" ,(first (lookup-inferior-packages inferior "clang" "11")))
       ;("gn" ,(first (lookup-inferior-packages inferior "gn")))
       ;("gperf" ,(first (lookup-inferior-packages inferior "gperf")))
       ;("ld-wrapper" ,(make-lld-wrapper (first (lookup-inferior-packages inferior "lld"))))
       ;;("ld-wrapper" ,(make-lld-wrapper lld))
       ;("ninja" ,(first (lookup-inferior-packages inferior "ninja")))
       ;("node" ,(first (lookup-inferior-packages inferior "node")))
       ;("pkg-config" ,(specification->package "pkg-config"))
       ;;("pkg-config" ,(first (lookup-inferior-packages inferior "pkg-config")))
       ;("which" ,(first (lookup-inferior-packages inferior "which")))

       ;; This file contains defaults for new user profiles.
       ;("master-preferences" ,(local-file "../../master-preferences.json"))

       ;("python2-beautifulsoup4" ,(first (lookup-inferior-packages inferior "python2-beautifulsoup4")))
       ;("python2-html5lib" ,(first (lookup-inferior-packages inferior "python2-html5lib")))
       ;("python" ,(first (lookup-inferior-packages inferior "python2")))
       ;("wayland-scanner" ,(first (lookup-inferior-packages inferior "wayland")))
       ))
    (inputs
     `(
       ;("nspr" ,(first (lookup-inferior-packages inferior "nspr")))
       ;,@(inferior-package-inputs inferior-ungoogled-chromium)
       ;,@(package-inputs inferior-ungoogled-chromium)
       ,@(package-inputs ungoogled-chromium)
       ))
    (home-page "https://github.com/plotly/Kaleido")
    (synopsis "Static image export for web-based visualization libraries")
    (description "Kaleido is a library for generating static images (e.g. png,
svg, pdf, etc.) for web-based visualization libraries, with a particular focus
on eliminating external dependencies.  The project's initial focus is on the
export of @code{plotly.js} images from Python for use by @code{plotly.py}, but
it is designed to be relatively straight-forward to extend to other web-based
visualization libraries, and other programming languages.")
    (license license:expat)))
