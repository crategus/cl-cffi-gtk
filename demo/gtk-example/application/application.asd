;;;; example-application.asd

(asdf:defsystem :application
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "application-1/application-1")
               (:file "application-2/application-2")
              ))

;; 2020-12-11
