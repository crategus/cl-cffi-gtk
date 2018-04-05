(asdf:defsystem :cl-cffi-gtk-opengl-demo
  :author "Olof-Joachim Frahm"
  :license "LLGPL"
  :description "OpenGL demos for CL-CFFI-GTK"
  :serial t
  :depends-on (:cl-cffi-gtk #:cl-opengl)
  :components ((:file "package")
               (:file "gl-area")))
