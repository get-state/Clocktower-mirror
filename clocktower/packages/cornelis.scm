(define-module (clocktower packages cornelis)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (clocktower packages haskell-libs)
  #:use-module (guix gexp)
  #:use-module (guix packages))

(define-public agda-cornelis-2.7.1
  ;; Version should always be matched to the same agda release. Match guix upstream.
  (package
    (name "cornelis")
    (version "2.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/agda/cornelis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c92mjmkqh2xyjav6sjk2ba068mi2s4j8js4rfij8f9711x00pw7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cornelis")))
    (inputs (list ghc-quickcheck
                  ghc-aeson
                  ghc-async
                  ghc-diff-loc
                  ghc-fingertree
                  ghc-generic-lens
                  ghc-hspec
                  ghc-hspec-discover
                  ghc-lens
                  ghc-levenshtein
                  ghc-megaparsec
                  ghc-nvim-hs
                  ghc-nvim-hs-contrib
                  ghc-prettyprinter
                  ghc-random
                  ghc-resourcet
                  ghc-unagi-chan
                  ghc-unliftio-core
                  ghc-vector))
    (native-inputs (list ghc-temporary ghc-hspec ghc-hspec-discover))
    (home-page "https://github.com/agda/cornelis")
    (synopsis "Agda-mode, but for neovim")
    (description
     "It supports highlighting, goal listing, type-context,
refinement, auto, solving, case splitting, go-to definition, normalization,
and helper functions.  These are exposed via vim commands. Most commands
have an equivalent in agda-mode.")
    (license license:bsd-3)))

(define-public agda-cornelis-2.8.0
  (package
    (inherit agda-cornelis-2.7.1)
    (name "cornelis")
    (version "2.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/agda/cornelis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fh3pgddhd70w53blnzlmrrls87sq0l320s3h2mkidf4xl6vlr3l"))))))

(define-public agda-cornelis
  agda-cornelis-2.7.1)
