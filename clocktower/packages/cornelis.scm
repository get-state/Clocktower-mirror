(define-module (clocktower packages cornelis)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (guix gexp)
  #:use-module (guix packages))

(define-public ghc-diff-loc
  (package
    (name "ghc-diff-loc")
    (version "0.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "diff-loc" version))
       (sha256
        (base32 "08sbxkf804bq3lwr9s1k1vigq7yrdjgicrj114zi14cdi4168k3x"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "diff-loc")))
    (inputs (list ghc-fingertree ghc-show-combinators))
    (arguments
     `(#:cabal-revision ("4"
                         "1ns06a8wvilv07z3qvd3q72sdmknvzhgcc0659lbghcv1babbhni")))
    (home-page "https://gitlab.com/lysxia/diff-loc")
    (synopsis "Map file locations across diffs")
    (description "See \"@code{DiffLoc}\".")
    (license license:expat)))

(define-public ghc-levenshtein
  (package
    (name "ghc-levenshtein")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "levenshtein" version))
       (sha256
        (base32 "0n3sbnbagb5am9z0jb8rd3m8d46ipmypbraqjj14zscih4lhapyi"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "levenshtein")))
    (inputs (list ghc-hashable ghc-data-default-class ghc-quickcheck))
    (native-inputs (list ghc-hspec ghc-hspec-discover))
    (home-page "https://github.com/hapytex/levenshtein#readme")
    (synopsis "Calculate the edit distance between two foldables.")
    (description
     "This package provides a package to determine the edit distance between two
Foldable's.  These are converted to lists, and the Levenshtein distance
determine how many additions, removals and changes are necessary to change the
first list into the second list.")
    (license license:bsd-3)))

(define-public ghc-foreign-store
  (package
    (name "ghc-foreign-store")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "foreign-store" version))
       (sha256
        (base32 "0ghs17wx32jwzk1fd1480cmnfakgdb3yc9v4yvzdxcipjc95abgj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "foreign-store")))
    (home-page "https://github.com/chrisdone/foreign-store")
    (synopsis
     "Store a stable pointer in a foreign context to be retrieved later.")
    (description
     "Store a stable pointer in a foreign context to be retrieved later.  Persists
through GHCi reloads.")
    (license license:bsd-3)))

(define-public ghc-messagepack
  (package
    (name "ghc-messagepack")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "messagepack" version))
       (sha256
        (base32 "1naa6lrvy56z6r41r800v4bqa3467j5d6rvgq9npaz6nm2nvrvmk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "messagepack")))
    (inputs (list ghc-cereal))
    (native-inputs (list ghc-quickcheck))
    (home-page "https://github.com/rodrigosetti/messagepack")
    (synopsis "Serialize instance for Message Pack Object")
    (description "Serialize instance for Message Pack Object.")
    (license license:expat)))

(define-public ghc-show-combinators
  (package
    (name "ghc-show-combinators")
    (version "0.2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "show-combinators" version))
       (sha256
        (base32 "07ds87ldl9165hj3k5h84iawc6vqlbggni3dg1nhbxww1spxn0n9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "show-combinators")))
    (arguments
     `(#:cabal-revision ("2"
                         "0n3xlpm41wpw1ybmacg9s7150nx00qrdlw2rq4fzz7iw7333cyjx")))
    (home-page "https://github.com/Lysxia/show-combinators#readme")
    (synopsis "Combinators to write Show instances")
    (description
     "This package provides a minimal pretty-printing library for Show instances in
Haskell.")
    (license license:expat)))

  (define-public ghc-nvim-hs
  (package
    (name "ghc-nvim-hs")
    (version "2.3.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "nvim-hs" version))
       (sha256
        (base32 "1wvmysvhmlmqm2nx8psjj6z38a49krrqwd57v2lf822lch4gi5lc"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "nvim-hs")))
    (inputs (list ghc-data-default
                  ghc-prettyprinter
                  ghc-prettyprinter-ansi-terminal
                  ghc-unliftio
                  ghc-unliftio-core
                  ghc-vector
                  ghc-void
                  ghc-cereal
                  ghc-cereal-conduit
                  ghc-conduit
                  ghc-foreign-store
                  ghc-hslogger
                  ghc-messagepack
                  ghc-network
                  ghc-optparse-applicative
                  ghc-time-locale-compat
                  ghc-megaparsec
                  ghc-streaming-commons
                  ghc-template-haskell-compat-v0208
                  ghc-typed-process
                  ghc-utf8-string))
    (native-inputs (list ghc-hspec ghc-hspec-discover ghc-quickcheck))
    (home-page "https://github.com/neovimhaskell/nvim-hs")
    (synopsis "Haskell plugin backend for neovim")
    (description
     "This package provides a plugin provider for neovim.  It allows you to write
plugins for one of the great editors of our time in the best programming
language of our time! ;-) .  You should find all the documentation you need
inside the \"Neovim\" module.  Most other modules are considered internal, so
don't be annoyed if using things from there may break your code! .  The
following modules may also be of interest and they should not change their API:
\"Neovim.Quickfix\" .  If you want to write unit tests that interact with neovim,
\"Neovim.Test\" provides some useful functions for that. .  If you are keen to
debug /nvim-hs/ or a module you are writing, take a look at the \"Neovim.Debug\"
module. .  If you spot any errors or if you have great ideas, feel free to open
an issue on github.")
    (license license:asl2.0)))

(define-public ghc-nvim-hs-contrib
  (package
    (name "ghc-nvim-hs-contrib")
    (version "2.0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "nvim-hs-contrib" version))
       (sha256
        (base32 "03syv1fsickcmgk9qagavidgh9hyy0v00df29jd5wia631xc60a6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "nvim-hs-contrib")))
    (inputs (list ghc-nvim-hs
                  ghc-prettyprinter
                  ghc-prettyprinter-ansi-terminal
                  ghc-data-default
                  ghc-messagepack
                  ghc-utf8-string
                  ghc-yaml))
    (native-inputs (list ghc-hspec ghc-hspec-discover ghc-quickcheck))
    (home-page "https://github.com/neovimhaskell/nvim-hs-contrib")
    (synopsis "Haskell plugin backend for neovim")
    (description "Library for nvim-hs.")
    (license license:asl2.0)))

(define-public agda-cornelis
  ; Version should always be matched to the same agda release.
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
    (description "It supports highlighting, goal listing, type-context,
refinement, auto, solving, case splitting, go-to definition, normalization,
and helper functions.  These are exposed via vim commands. Most commands
have an equivalent in agda-mode.")
    (license license:bsd-3)))
