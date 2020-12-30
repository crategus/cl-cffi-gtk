(defpackage :gio-example
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:application-open)
  (:export #:application-action))

(in-package :gio-example)

;;; 2020-12-10
