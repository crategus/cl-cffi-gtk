(asdf:defsystem :cl-cffi-gtk-opengl-demo
  :author "Olof-Joachim Frahm"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk #:cl-opengl)
  :components ((:file "package")
               (:file "gl-area")))
