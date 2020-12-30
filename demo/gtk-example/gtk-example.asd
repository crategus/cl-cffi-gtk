;;;; gtk-example.asd

(asdf:defsystem :gtk-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "gtk-example")
               (:file "application-inhibit")
               (:file "application-simple")
               (:file "bloat-pad")
               (:file "custom-window")
               (:file "dialog")
               (:file "event-handler")
               (:file "file-chooser-dialog")
;               (:file "geometry-hints")
               (:file "grab")
               (:file "grid-packing")
               (:file "image-button-press")
               (:file "image-menu-item")
               (:file "info-bar")
               (:file "level-bar")
               (:file "list-store")
               (:file "menu-builder")
               (:file "message-dialog-new")
;               (:file "page-setup")
               (:file "page-setup-unix-dialog")
               (:file "pointer-device")
               (:file "print-dialog")
               (:file "print-operation")
               (:file "radio-button")
               (:file "show-about-dialog")
               (:file "widget-pointer")
              ))

;;; 2020-12-19
