(defpackage :pango-example
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :common-lisp)
  (:export #:example-text-centered
           #:example-text-soulmate
  ))

(in-package :pango-example)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :pango-example)))
    (princ-to-string (merge-pathnames filename system-path))))

;;; 2021-1-17
