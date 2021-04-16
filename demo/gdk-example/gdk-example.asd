;;;; gdk-example.asd

(asdf:defsystem :gdk-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "gdk-example")
               (:file "event-key")
               (:file "app-launch")
              ))

;;; 2021-4-14
