(defpackage :gio-example
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:application-action)
  (:export #:application-commandline)
  (:export #:application-open)
  (:export #:example-emblemed-icon)
)

;;; 2021-5-7
