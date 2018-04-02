(defpackage :gtk-testsuite
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo
   :cffi :common-lisp :bordeaux-threads :fiveam))

(in-package :gtk-testsuite)

(def-suite gtk-testsuite)
(in-suite gtk-testsuite)

