(define-module (clocktower packages agda)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (guix packages))

(define-public agda
  (package
    (name "agda")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/agda/agda/releases/download/v"
                           version "/Agda-v" version "-linux.tar.xz"))
       (sha256
        (base32 "1brgcgr0sp2aiixar11q3k2r1wsiwj1vvihaln4i4hxyvjw82h42"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("agda" "bin/"))))
    (native-inputs (list xz))
    (home-page "https://wiki.portal.chalmers.se/agda/pmwiki.php")
    (supported-systems '("x86_64-linux"))
    (synopsis "Dependently typed functional programming language")
    (description
     "Agda is a dependently typed functional programming language 
and proof assistant.  It is an interactive system for writing and checking proofs.")
    (license license:bsd-3)))
