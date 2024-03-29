;;;; gtk-application.asd

(asdf:defsystem :gtk-application
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "gtk-application")
               (:file "utils")
               (:file "application-command-line")
               (:file "application-inhibit")
               (:file "application-menu")
               (:file "application-notification")
               (:file "application-properties")
               (:file "application-simple")
               (:file "bloatpad")
               (:file "sunny")
               (:file "application/application-1/application-1")
               (:file "application/application-2/application-2")
               (:file "application/application-3/application-3")
               (:file "application/application-4/application-4")
              ))

;; 2021-10-30
