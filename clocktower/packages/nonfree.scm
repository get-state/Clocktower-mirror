(define-module (clocktower packages nonfree)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  ;; #:use-module (guix records) ; You can comment this out if alist-delete is the only thing you need from it
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages scanner)
  #:use-module (nongnu packages printers)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages julia)
  #:use-module (gnu packages textutils)
  #:use-module (guix licenses))

(define-public sane-backends-nonfree
  (package
   (inherit sane-backends)
   (name "sane-backends-nonfree")
   (inputs (append
            `(("hplip" ,hplip-plugin))
            ;; Use filter to remove "hplip" from the inputs
            (filter (lambda (input-pair)
                      (not (string=? (car input-pair) "hplip")))
                    (package-inputs sane-backends))))))
