(define-module (clocktower packages gh)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages))

(define-public github-cli
  (package
    (name "github-cli")
    (version "2.78.0")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/cli/cli/releases/download/v" version
                           "/gh_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "018sgs8f7i7va4bx22fcjn2mvjk5rf1cwf315v425cfnqmq9yc5c"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:validate-runpath? #t
      #:phases
      #~(modify-phases
            %standard-phases
          (add-after 'unpack 'change-directory
            (lambda _
              (chdir #$(string-append "gh_" version "_linux_amd64")))))))
    (home-page "https://github.com/cli/cli")
    (supported-systems '("x86_64-linux"))
    (synopsis "GitHub’s official command line tool")
    (description "gh is GitHub on the command line. It brings pull requests,
issues, and other GitHub concepts to the terminal next to where you are already
working with git and your code.")
    (license license:expat)))
