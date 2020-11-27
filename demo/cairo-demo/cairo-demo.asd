;;;; cairo-demo.asd

(asdf:defsystem :cairo-demo
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "cairo-demo")
               (:file "cairo-clock")))

;;; 2020-11-25
