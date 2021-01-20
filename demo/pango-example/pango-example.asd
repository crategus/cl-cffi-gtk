;;;; pango-example.asd

(asdf:defsystem :pango-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "pango-example")
               (:file "pango-text-centered")
               (:file "pango-text-metrics")
               (:file "pango-text-soulmate")
))

;;; 2021-1-19
