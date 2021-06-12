;;;; gtk-demo.lisp

(asdf:defsystem :gtk-demo
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:gtk-example
               :gtk-tutorial
               :gio-example
               :cl-cffi-gtk :split-sequence)
  :components ((:file "package")
               (:file "alignment")
               (:file "app-chooser-button")
               (:file "app-chooser-dialog")
               (:file "application-window")
               (:file "assistant")
               (:file "clipboard")
               (:file "color-chooser-widget")
               (:file "color-chooser-palette")
               (:file "cursor")
               (:file "css-accordion")
               (:file "css-basics")
               (:file "css-blendmodes")
               (:file "css-pixbufs")
               (:file "grid")
               (:file "menu")
               (:file "numerable-icon")
               (:file "pixbuf-scale")
               (:file "pixbufs")
               (:file "popover")
               (:file "search-entry")
               (:file "selections-1")
               (:file "size-management")
               (:file "simple-application-window")
               (:file "simple-message")
               (:file "../cairo-demo/cairo-demo")
               (:file "../cairo-demo/cairo-clock")
               (:file "gtk-demo")))

;;; 2021-6-11
