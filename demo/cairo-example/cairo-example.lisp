(defpackage :cairo-example
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :common-lisp)
  (:export #:example-png-image
           #:example-png-image-for-data
           #:example-png-image-from-png
           #:example-pdf-file))

(in-package :cairo-example)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :cairo-example)))
    (princ-to-string (merge-pathnames filename system-path))))

;;; 2020-12-21
