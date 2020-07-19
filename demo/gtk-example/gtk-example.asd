;;;; gtk-example.asd

(asdf:defsystem :gtk-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "gtk-example")
               (:file "application-inhibit")
               (:file "application-simple")

               (:file "dialog")
               (:file "file-chooser-dialog")
               (:file "grab")
               (:file "grid-packing")
               (:file "image-button-press")
               (:file "info-bar")
               (:file "level-bar")
               (:file "list-store")
               (:file "menu-builder")
               (:file "message-dialog-new")
               (:file "page-setup-unix-dialog")
               (:file "print-dialog")
               (:file "print-operation")
               (:file "radio-button")
               (:file "show-about-dialog")
               (:file "image-menu-item")
               (:file "widget-pointer")
              ))

