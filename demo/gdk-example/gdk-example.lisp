(defpackage :gdk-example
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :cairo :cffi :common-lisp)
  (:export #:example-event-key))

(in-package :gdk-example)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-example)))
    (princ-to-string (merge-pathnames filename system-path))))

;;; 2021-4-3
