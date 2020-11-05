(define-module (gn packages twint)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages check)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time))

(define-public python-googletransx
  (package
    (name "python-googletransx")
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "googletransx" version))
       (sha256
        (base32
         "1nnn08cqc57d8lq1qp82fazk1x83ccdi410hmzlbnajw6vinfrf4"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (home-page
     "https://github.com/x0rkov/py-googletrans")
    (synopsis
     "Free Google Translate API for Python. Translates totally free of charge.")
    (description
     "Free Google Translate API for Python. Translates totally free of charge.")
    (license license:expat)))

(define-public python-statistics
  (package
    (name "python-statistics")
    (version "1.0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statistics" version))
       (sha256
        (base32
         "0f61hrj25p60kvf09nqysd6xlccm5dmx1jl8akfjvgq71fw7khrd"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-docutils" ,python-docutils)))
    (home-page "UNKNOWN")
    (synopsis
     "A Python 2.* port of 3.4 Statistics Module")
    (description
     "A Python 2.* port of 3.4 Statistics Module")
    (license #f)))

(define-public python-fake-useragent
  (package
    (name "python-fake-useragent")
    (version "0.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fake-useragent" version))
        (sha256
          (base32
            "0dfz3bpmjmaxlhda6hfgsac7afb65pljibi8zkp9gc0ffn5rj161"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page
      "https://github.com/hellysmile/fake-useragent")
    (synopsis
      "Up to date simple useragent faker with real world database")
    (description
      "Up to date simple useragent faker with real world database")
    (license #f)))

(define-public python-cchardet
  (package
    (name "python-cchardet")
    (version "2.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cchardet" version))
        (sha256
          (base32
            "1cs6y59qhbal8fgbyjk2lpjykh8kfjhq16clfssylsddb4hgnsmp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/PyYoshi/cChardet")
    (synopsis
      "cChardet is high speed universal character encoding detector.")
    (description
      "cChardet is high speed universal character encoding detector.")
    (license #f)))

;; TODO: Upstream
(define-public python-socks
  (package
    (name "python-socks")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-socks" version))
       (sha256
        (base32
         "1w5gdlz6jaflzmzf8d6s81k4qwzb0q8xq2s76bj04sv11nm2lkiv"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-async-timeout" ,python-async-timeout)
       ("python-trio" ,python-trio)
       ("python-curio" ,python-curio)))
    (home-page
     "https://github.com/romis2012/python-socks")
    (synopsis
     "Core proxy (SOCKS4, SOCKS5, HTTP tunneling) functionality for Python")
    (description
     "Core proxy (SOCKS4, SOCKS5, HTTP tunneling) functionality for Python")
    (license #f)))

;; TODO: Upstream
(define-public python-aiohttp-3.7.2
  (package
    (inherit python-aiohttp)
    (version "3.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiohttp" version))
       (sha256
        (base32
         "0w30pq8x4sf9bhr29nrb4qxhm75mpz2a51nkalrd9rj1k3simnn6"))))
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-typing-extensions" ,python-typing-extensions)
         ,@(package-propagated-inputs python-aiohttp)))))

(define-public python-twint
  (package
    (name "python-twint")
    (version "2.1.20")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "twint" version))
        (sha256
          (base32
            "0pcnn7p114agwk41vayp7wbc61yx7bwa1k7lz7gsa7p3jwcngdxk"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
      `(("python-aiodns" ,python-aiodns)
        ("python-pycares" ,python-pycares)
        ("python-aiohttp" ,python-aiohttp)
        ("python-aiohttp-socks" ,python-aiohttp-socks)
        ("python-beautifulsoup4" ,python-beautifulsoup4)
        ("python-cchardet" ,python-cchardet)
        ("python-elasticsearch" ,python-elasticsearch)
        ("python-fake-useragent" ,python-fake-useragent)
        ("python-geopy" ,python-geopy)
        ("python-googletransx" ,python-googletransx)
        ("python-pandas" ,python-pandas)
        ("python-pysocks" ,python-pysocks)
        ("python-schedule" ,python-schedule)))
    (home-page
      "https://github.com/twintproject/twint")
    (synopsis
      "An advanced Twitter scraping & OSINT tool.")
    (description
      "An advanced Twitter scraping & OSINT tool.")
    (license license:expat)))
