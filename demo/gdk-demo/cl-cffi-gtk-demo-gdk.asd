;;;; cl-cffi-gtk-demo-gdk.asd

(asdf:defsystem :cl-cffi-gtk-demo-gdk
  :author "Dieter Kaiser"
  :license "LLGPL"
  :description "GDK demos for CL-CFFI-GTK"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "composited-window")
               (:file "events")
               (:file "pango")))
