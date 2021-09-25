;;; gtk-application.lisp

(defpackage :gtk-application
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:application-command-line
           #:application-inhibit
           #:application-menu
           #:application-properties
           #:application-simple
           #:bloatpad
           #:sunny
))

(in-package :gtk-application)

(defun sys-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-application)))
    (princ-to-string (merge-pathnames filename system-path))))

;;; 2021-9-9
