;;;; gtk-example.asd

(asdf:defsystem :gtk-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "gtk-example")
               (:file "dialog")
               (:file "file-chooser-dialog")
               (:file "grab")
               (:file "image-button-press")
               (:file "info-bar")
               (:file "level-bar")
               (:file "list-store")
               (:file "menu-builder")
               (:file "message-dialog-new")
               (:file "page-setup-unix-dialog")
               (:file "print-dialog")
               (:file "print-operation")
               (:file "show-about-dialog")
               (:file "simple-application")
               (:file "image-menu-item")))

