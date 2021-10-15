;;; gtk-application.lisp

(defpackage :gtk-application
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:application-cmdline
           #:application-inhibit
           #:application-menu
           #:application-notification
           #:application-properties
           #:application-simple
           #:bloatpad
           #:sunny
))

;;; 2021-10-13
