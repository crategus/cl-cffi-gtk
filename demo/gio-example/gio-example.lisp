(defpackage :gio-example
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:application-open)
  (:export #:application-action)
  (:export #:example-emblemed-icon)
)

;;; 2021-4-27
