;;;; gtk-tutorial.lisp

(asdf:defsystem :gtk-tutorial
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk :split-sequence)
  :components ((:file "gtk-tutorial")
              ))

;;; 2021-6-5
