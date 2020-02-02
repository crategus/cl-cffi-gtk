(defpackage :gtk-demo
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:gtk-demo))

(in-package :gtk-demo)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-demo)))
    (princ-to-string (merge-pathnames filename system-path))))
