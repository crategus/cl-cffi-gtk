;;;; cl-cffi-gtk-demo-cairo.asd

(asdf:defsystem :cl-cffi-gtk-demo-cairo
  :author "Dieter Kaiser"
  :license "LLGPL"
  :description "Cairo demos for CL-CFFI-GTK"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "cairo-demo")
               (:file "cairo-clock")))
