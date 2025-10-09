(define-module (clocktower packages neovim)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages julia)
  #:use-module (gnu packages textutils)
  #:use-module (guix licenses))


(define-public tree-sitter-0.25
  (package
    (inherit tree-sitter)
    (name "tree-sitter")
    (version "0.25.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tree-sitter/tree-sitter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wv6nlxishxh6d2nzf2hrmpx41nzii4y219f06qa0bn2y9p36yv8"))))))

(define-public neovim-0.11
  (package
    (inherit neovim)
    (name "neovim")
    (version "0.11.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neovim/neovim")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zwpnmxv90j46cvz8ad3hbffz9pc11l1p5rx1z8nmk2b7jlm0d0g"))))
    (inputs (modify-inputs (package-inputs neovim)
              (replace "tree-sitter" tree-sitter-0.25)))))
