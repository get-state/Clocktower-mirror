(define-module (clocktower packages neovim)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages vim)
  #:use-module ((gnu packages commencement)
                #:select (gcc-toolchain))
  #:use-module (gnu packages curl)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages julia)
  #:use-module (gnu packages textutils)
  #:use-module (guix licenses))

(define-public neovim-0.12
  (package
    (inherit neovim)
    (name "neovim")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neovim/neovim")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "173yaf58iqijvzg5yir94v2g67mi7k91d39mn85xsbvnapzxh9ql"))))
    (arguments
     (substitute-keyword-arguments (package-arguments neovim)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'wrap-everything
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (nvim (string-append out "/bin/nvim"))
                       ;; Find the base directory of the toolchain
                       (gcc-bin (string-append (assoc-ref inputs
                                                          "gcc-toolchain")
                                               "/bin"))
                       (gcc (string-append gcc-bin "/gcc"))
                       (ts-bin (dirname (search-input-file inputs
                                                           "/bin/tree-sitter"))))
                  (wrap-program nvim
                    ;; 1. Set the CC variable so tresitter/lazy can find it
                    `("CC" =
                      (,gcc))
                    ;; 2. Add the toolchain AND tree-sitter to PATH
                    `("PATH" ":" prefix
                      (,gcc-bin ,ts-bin))
                    ;; 3. Ensure the compiler finds standard headers/libs
                    `("CPATH" ":" prefix
                      (,(string-append (assoc-ref inputs "gcc-toolchain")
                                       "/include")))
                    `("LIBRARY_PATH" ":" prefix
                      (,(string-append (assoc-ref inputs "gcc-toolchain")
                                       "/lib")))))))))))
    (inputs (modify-inputs (package-inputs neovim)
              (append gcc-toolchain tree-sitter-cli utf8proc)))))
