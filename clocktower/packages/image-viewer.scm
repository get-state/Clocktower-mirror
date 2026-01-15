(define-module (clocktower packages image-viewer)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages check)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

;; Special thanks to Murilo (6a046e63e2ccdd0d9af9364883597b4bb50e3ab9)
(define-public ueberzugpp
  (package
    (name "ueberzugpp")
    (version "2.9.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/jstkdng/ueberzugpp")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1d8qrbcndhb51xfbsyzs9ca2mb13hrv50fyf9l9xc29k5lwswcq5"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DENABLE_OPENCV=ON"
                   "-DENABLE_X11=ON"
                   "-DENABLE_WAYLAND=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'check))))
    (native-inputs
     (list cairo
           cli11
           expat
           fftw
           fmt
           gobject-introspection
           imagemagick
           imath
           lcms
           libexif
           libgsf
           librsvg
           libwebp
           libxcb
           matio
           nlohmann-json
           opencv
           openexr
           orc
           pango
           pkg-config
           poppler
           range-v3
           spdlog
           xcb-util-image))
    (inputs
     (list libsixel
           vips
           chafa
           openssl
           tbb))
    (home-page "https://github.com/jstkdng/ueberzugpp")
    (synopsis "Drop in replacement for ueberzug written in C++.")
    (description "Überzug++ is a command line utility written in C++ which
allows to draw images on terminals by using X11/wayland child windows, sixels, 
kitty and iterm2..

Advantages over w3mimgdisplay and ueberzug:

@itemize
@item support for wayland: sway, hyprland and wayfire
@item support for MacOS
@item no race conditions as a new window is created to display images
@item expose events will be processed, so images will be redrawn on switch
workspaces
@item tmux support on X11, sway and hyprland
@item terminals without the WINDOWID environment variable are supported
@item chars are used as position - and size unit
@item No memory leak (usage of smart pointers)
@item A lot of image formats supported (through opencv and libvips).
@item GIF and animated WEBP support on X11, Sixel, Sway and hyprland
@item Fast image downscaling (through opencv and opencl)
@item Cache resized images for faster viewing
@end itemize")
    (license license:gpl3)))
