;;;; gio-example.asd

(asdf:defsystem :gio-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "gio-example")
               (:file "application-action")
               (:file "application-commandline")
               (:file "application-open")
               (:file "emblemed-icon")
))

;;; 2021-5-7
