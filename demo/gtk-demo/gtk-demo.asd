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
               (:file "pixbuf-scale")
               (:file "pixbufs")
               (:file "popover")
               (:file "selections-1")
               (:file "size-management")
               (:file "simple-application-window")
               (:file "simple-message")
               (:file "../cairo-demo/cairo-demo")
               (:file "../cairo-demo/cairo-clock")
               (:file "gtk-demo")))

;;; 2021-11-5
