(define-module (clocktower packages scrcpy)
  #:use-module (gnu packages video)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages android)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system copy)
  #:use-module (guix licenses))

(define-public scrcpy
  (package
    (name "scrcpy")
    (version "3.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Genymobile/scrcpy.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10w0whpay80cw2lhbv2h2l75id24zcc9g9hll5lva51dp4nnd2ss"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'pre-configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (call-with-output-file "server/meson.build"
                        (lambda (port)
                          ;; truncate the file
                          (display "" port)))))
                  (add-after 'install 'wrap-adb
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((adb-bin (string-append (assoc-ref inputs "adb")
                                                    "/bin"))
                            (out-bin (string-append (assoc-ref outputs "out")
                                                    "/bin")))
                        (wrap-program (string-append out-bin "/scrcpy")
                          `("PATH" ":" prefix
                            (,adb-bin))))))
                  (add-after 'wrap-adb 'copy-server-bin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((scrcpy-server-bin (string-append (assoc-ref
                                                               inputs
                                                               "scrcpy-server")
                                                "/share/scrcpy/scrcpy-server"))
                            (out (assoc-ref outputs "out")))
                        (install-file scrcpy-server-bin
                                      (string-append out "/share/scrcpy/"))))))))
    (inputs (list ffmpeg sdl2 adb libusb scrcpy-server))
    (native-inputs (list pkg-config))
    (synopsis "Display and control Android devices over USB or TCP/IP")
    (description
     "scrcpy provides display and control of Android devices connected
over USB or over TCP/IP.  It does not require any root access.  It works by
running a server on the Android device and communicating with it over adb.")
    (home-page "https://github.com/Genymobile/scrcpy")
    (license asl2.0)))

(define scrcpy-server
  (package
    (name "scrcpy-server")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Genymobile/scrcpy/releases/download/v"
             version "/scrcpy-server-v" version))
       (sha256
        (base32 "1f6bxg0hpms4ngd3nyk4n2xnmawddk6ihhf9b3vgp629m8h0pxx0"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("scrcpy-server-v3.3.1" "share/scrcpy/scrcpy-server"))))
    (synopsis "Server component for scrcpy Android screen mirroring")
    (description
     "This package contains the Android server APK file required by
scrcpy to mirror and control Android devices.  The server runs on the Android
device and communicates with the scrcpy client.")
    (home-page "https://github.com/Genymobile/scrcpy")
    (license asl2.0)))
