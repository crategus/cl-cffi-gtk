(defpackage :gdk-demo
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :cairo :cffi :common-lisp)
  (:export #:demo-put-pixel))

(in-package :gdk-demo)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-example)))
    (princ-to-string (merge-pathnames filename system-path))))

;;; 2020-11-21
