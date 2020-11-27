;;;; gdk-demo.asd

(asdf:defsystem :gdk-demo
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "gdk-demo")
               (:file "events")
               (:file "pango")
               (:file "put-pixel")
               (:file "mandelbrot")))

;;; 2020-11-21
