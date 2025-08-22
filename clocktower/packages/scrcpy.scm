(define-module (clocktower packages scrcpy)
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



