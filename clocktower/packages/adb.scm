(define-module (clocktower packages adb)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc))

(define-public adb-latest
  (package
    (name "adb")
    (version "latest")
    (source (origin
              (method url-fetch)
              (uri "https://dl.google.com/android/repository/platform-tools-latest-linux.zip")
              (sha256
               (base32
                "1s7vrckwnv1zappfybnz8d7wxi393igqzjnc3xq7kzizjhn69b8f"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          
          (let* ((source #$source)
                 (out #$output)
                 (bin (string-append out "/bin"))
                 (unzip #$(file-append unzip "/bin/unzip"))
                 (patchelf #$(file-append patchelf "/bin/patchelf"))
                 (gcc-lib #$(file-append gcc-toolchain "/lib"))
                 (tmpdir (getenv "TMPDIR")))
            
            ;; Extract the zip file
            (invoke unzip source "-d" tmpdir)
            
            ;; Create output directories
            (mkdir-p bin)
            
            ;; Copy binaries to output
            (for-each
             (lambda (file)
               (let ((src (string-append tmpdir "/platform-tools/" file))
                     (dst (string-append bin "/" file)))
                 (when (file-exists? src)
                   (copy-file src dst)
                   (chmod dst #o755))))
             '("adb" "fastboot" "mke2fs" "e2fsdroid"))
            
            ;; Patch ELF binaries
            (for-each
             (lambda (binary)
               (let ((bin-path (string-append bin "/" binary)))
                 (when (file-exists? bin-path)
                   ;; Set interpreter
                   (invoke patchelf "--set-interpreter"
                           #$(file-append (canonical-package glibc)
                                        "/lib/ld-linux-x86-64.so.2")
                           bin-path)
                   ;; Set rpath to include gcc-toolchain lib
                   (invoke patchelf "--set-rpath"
                           (string-append gcc-lib ":" 
                                        #$(file-append (canonical-package glibc) "/lib"))
                           bin-path))))
             '("adb" "fastboot" "mke2fs" "e2fsdroid"))
            
            #t))))
    (native-inputs
     (list unzip patchelf))
    (inputs
     (list gcc-toolchain glibc))
    (home-page "https://developer.android.com/studio/command-line/adb")
    (synopsis "Android Debug Bridge (ADB)")
    (description
     "ADB is a versatile command-line tool that lets you communicate with a device.
The adb command facilitates a variety of device actions, such as installing and
debugging apps, and it provides access to a Unix shell that you can use to run
a variety of commands on a device.  This package includes the official Google
prebuilt binaries patched to work on Guix.")
    (license license:asl2.0)))

adb
