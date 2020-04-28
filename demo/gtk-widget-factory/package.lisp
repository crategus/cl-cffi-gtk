(defpackage :gtk-widget-factory
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:gtk-widget-factory))

(in-package :gtk-widget-factory)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-widget-factory)))
    (princ-to-string (merge-pathnames filename system-path))))

