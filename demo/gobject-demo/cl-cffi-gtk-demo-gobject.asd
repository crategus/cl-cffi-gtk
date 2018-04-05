;;;; cl-cffi-gtk-demo-gobject.asd

(asdf:defsystem :cl-cffi-gtk-demo-gobject
  :author "Dieter Kaiser"
  :license "LLGPL"
  :description "GObject demos for CL-CFFI-GTK"
  :serial t
  :depends-on (:cl-cffi-gtk-gobject)
  :components ((:file "gobject.demo-g-value")))
