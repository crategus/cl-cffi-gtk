(defpackage :gio-example
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:application-action
           #:application-cmdline
           #:application-open
           #:example-emblemed-icon
))

;;; 2021-9-19
