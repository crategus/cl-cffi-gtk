;;;; cairo-example.asd

(asdf:defsystem :cairo-example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk)
  :components ((:file "cairo-example")
               (:file "png-image")
               (:file "png-image-for-data")
               (:file "png-image-from-png")
               (:file "svg-file")
               (:file "pdf-file")
               (:file "drawing")
               (:file "drawing-lines")
               (:file "drawing-dashes")
               (:file "drawing-caps")
               (:file "drawing-joins")
               (:file "text-soulmate")
               (:file "text-centered")
               (:file "text-shaded")
               (:file "text-gradient")
               (:file "text-glyph")
))

;;; 2020-12-29
