;;;; gtk-widget-factory.asd

(asdf:defsystem :gtk-widget-factory
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "package")
               (:file "gtk-widget-factory")
              ))

;;; 2020-11-14
