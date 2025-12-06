(define-module (clocktower packages ppsspp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg))

(define-public ppsspp
  (package
    (name "ppsspp")
    (version "1.19.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hrydgard/ppsspp")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0m64sfv4pc24w78v75i8m0bihs3p86p3zl65hmr5666q8n6hhnpg"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; No test suite
      #:configure-flags
      #~(list "-DUSING_QT_UI=OFF"
              ; "-DUSE_SYSTEM_FFMPEG=ON"
              "-DUSE_SYSTEM_SNAPPY=ON"
              "-DUSE_SYSTEM_LIBZIP=ON"
              "-DUSE_WAYLAND_WSI=ON"
              "-DVULKAN=ON"
              (string-append "-DCMAKE_INSTALL_PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda _
              (substitute* "SDL/SDLMain.cpp"
                (("/usr/share") (string-append #$output "/share")))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/PPSSPPSDL")
                `("LD_LIBRARY_PATH" ":" prefix
                  (,(string-append #$(this-package-input "vulkan-loader") "/lib")))))))))
    (native-inputs
     (list pkg-config python))
    (inputs
     (list glew 
           ; ffmpeg-6 currently broken TODO: fix this, and uncomment above.
           libzip
           vulkan-headers
           vulkan-loader
           mesa
           sdl2
           snappy
           zlib))
    (home-page "https://www.ppsspp.org/")
    (synopsis "PSP emulator")
    (description
     "PPSSPP is a PlayStation Portable emulator capable of playing the majority
of PSP games in full HD resolution.  It can even upscale textures that would
otherwise be too blurry as they were made for the small screen of the original
PSP.  The emulator supports save states, external controllers, and has a
built-in screen recording feature.")
    (license gpl2+)))
