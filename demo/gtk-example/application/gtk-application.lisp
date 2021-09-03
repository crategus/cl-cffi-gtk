;;; gtk-applicaton.lisp

(defpackage :gtk-application
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:application-command-line
           #:application-inhibit
           #:application-menu
           #:application-properties
           #:application-simple
           #:bloatpad
))

(in-package :gtk-application)

;;; 2021-9-2
