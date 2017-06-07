;;;; cl-cffi-gtk-example-gtk.asd

(asdf:defsystem :cl-cffi-gtk-example-gtk
  :author "Dieter Kaiser"
  :license "LLGPL"
  :description "Example for CL-CFFI-GTK"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "gtk-example")
               (:file "dialog")
               (:file "file-chooser-dialog")
               (:file "grab")
               (:file "list-store")
               (:file "menu-builder")
               (:file "message-dialog-new")
               (:file "show-about-dialog")))
