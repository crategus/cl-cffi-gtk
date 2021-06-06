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
               (:file "arrows")
               (:file "application-window")
               (:file "assistant")
               (:file "calendar")
               (:file "clipboard")
               (:file "color-chooser-widget")
               (:file "color-chooser-palette")
               (:file "cursor")
               (:file "css-accordion")
               (:file "css-basics")
               (:file "css-blendmodes")
               (:file "css-pixbufs")
               (:file "entry-completion")
               (:file "entry-buffer")
               (:file "event-box")
               (:file "file-chooser-custom-filter")
               (:file "file-chooser-dialog")
               (:file "file-chooser-preview")
               (:file "file-chooser-widget")
               (:file "font-button")
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
               (:file "spin-button")
               (:file "text-entry")
               (:file "../cairo-demo/cairo-demo")
               (:file "../cairo-demo/cairo-clock")
               (:file "gtk-demo")))

;;; 2021-6-1
