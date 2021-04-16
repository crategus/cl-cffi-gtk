(defpackage :gdk-example
  (:use :gtk :gdk :gdk-pixbuf :gobject :gio :glib :cairo :cffi :common-lisp)
  (:export #:example-event-key)
  (:export #:example-app-launch)
)

(in-package :gdk-example)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-example)))
    (princ-to-string (merge-pathnames filename system-path))))

;;; 2021-4-14
