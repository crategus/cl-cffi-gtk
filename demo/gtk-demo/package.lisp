(defpackage :gtk-demo
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:main))

(in-package :gtk-demo)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :cl-cffi-gtk-demo-gtk)))
    (princ-to-string (merge-pathnames filename system-path))))

(defvar *demo-window*)
