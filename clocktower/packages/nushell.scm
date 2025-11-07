(define-module (clocktower packages nushell)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public nu
  (package
    (name "nushell")
    (version "0.108.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nushell/nushell")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15mmfrxcpzg6mhin15xzmb7m7xi6ncsa23i87645yv8fqfqi7qzh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:rust ,rust-1.88
       #:cargo-test-flags
       '("--"
         "--skip=path::canonicalize::canonicalize_tilde"
         "--skip=path::canonicalize::canonicalize_tilde_relative_to"
         "--skip=plugin_persistence"
         "--skip=plugins"
         "--skip=repl")
       #:cargo-package-crates '("nu-cli" "nu-cmd-base" "nu-cmd-extra" "nu-cmd-lang"
             "nu-command" "nu-engine" "nu-experimental" "nu-explore"
             "nu-lsp" "nu-parser" "nu-path" "nu-protocol" "nu-std"
             "nu-system" "nu-utils" "nu-plugin-engine" "nu-cmd-plugin" "nu-mcp" "nu")
       #:install-source? #f))
    (inputs (cons* mimalloc openssl sqlite (cargo-inputs 'nushell #:module '(clocktower packages vendor nu-vendor))))
    (native-inputs (list pkg-config))
    (home-page "https://www.nushell.sh")
    (synopsis "new type of shell")
    (description "This package provides a new type of shell.")
    (license license:expat)))

