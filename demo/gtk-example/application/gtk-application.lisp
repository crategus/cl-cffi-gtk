;;; gtk-applicaton.lisp

(defpackage :gtk-application
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:application-simple
           #:application-command-line))

(in-package :gtk-application)

;;; 2021-8-12

