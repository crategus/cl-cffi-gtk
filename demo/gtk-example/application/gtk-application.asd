;;;; gtk-application.asd

(asdf:defsystem :gtk-application
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "gtk-application")
               (:file "application-simple")
               (:file "application-command-line")
;               (:file "application-1/application-1")
;               (:file "application-2/application-2")
              ))

;; 2021-8-12
