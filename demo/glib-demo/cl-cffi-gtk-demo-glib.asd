;;;; cl-cffi-gtk-demo-glib.asd

(asdf:defsystem :cl-cffi-gtk-demo-glib
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "glib-commandline")
               (:file "glib-event-loop")))
