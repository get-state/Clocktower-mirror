(define-module (clocktower packages adb)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages base) #:select (glibc))
  #:use-module (gnu packages compression)
  #:use-module (( gnu packages commencement ) #:select (gcc-toolchain))
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:export (adb))

(define-public adb
  (package
    (name "adb")
    (version "36.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dl.google.com/android/repository/platform-tools_r"
                    version "-linux.zip"))
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
                 (glibc-lib #$(file-append glibc "/lib"))
                 (ld-so #$(file-append glibc "/lib/ld-linux-x86-64.so.2"))
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
             '("adb" "fastboot" "mke2fs" "e2fsdroid" "make_f2fs" 
               "make_f2fs_casefold" "etc1tool"))
            
            ;; Copy lib64 directory if it exists (contains additional libraries)
            (let ((lib64-src (string-append tmpdir "/platform-tools/lib64"))
                  (lib64-dst (string-append out "/lib64")))
              (when (file-exists? lib64-src)
                (mkdir-p lib64-dst)
                (copy-recursively lib64-src lib64-dst)))
            
            ;; Patch ELF binaries
            (for-each
             (lambda (binary)
               (let ((bin-path (string-append bin "/" binary)))
                 (when (file-exists? bin-path)
                   ;; Set interpreter
                   (invoke patchelf "--set-interpreter" ld-so bin-path)
                   ;; Set rpath to include gcc-toolchain lib and glibc lib
                   (invoke patchelf "--set-rpath"
                           (string-append gcc-lib ":" glibc-lib ":"
                                        out "/lib64")
                           bin-path))))
             '("adb" "fastboot" "mke2fs" "e2fsdroid" "make_f2fs"
               "make_f2fs_casefold" "etc1tool"))
            
            #t))))
    (native-inputs
     (list unzip patchelf))
    (inputs
     (list gcc-toolchain glibc))
    (home-page "https://developer.android.com/tools/releases/platform-tools")
    (synopsis "Android Debug Bridge (ADB) and platform tools")
    (description
     "ADB is a versatile command-line tool that lets you communicate with a device.
The adb command facilitates a variety of device actions, such as installing and
debugging apps, and it provides access to a Unix shell that you can use to run
a variety of commands on a device.  This package includes the official Google
prebuilt binaries patched to work on Guix.

Included tools:
@itemize
@item adb - Android Debug Bridge for device communication
@item fastboot - Tool for flashing system images
@item mke2fs - Create ext2/ext3/ext4 filesystems
@item e2fsdroid - Build an ext2/ext3/ext4 filesystem from a directory
@item make_f2fs - Create F2FS filesystems
@end itemize")
    (license license:asl2.0)))
