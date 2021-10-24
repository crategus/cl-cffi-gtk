#-5am
(progn
  (format t "~&Loading of package fiveam ...~%")
  (asdf:load-system :fiveam)
  (format t "Loading is finished.~%"))

#-gtk
(progn
  (format t "~&Loading of package cl-cffi-gtk ...~%")
  (asdf:operate 'asdf:load-op :cl-cffi-gtk)
  (format t "Loading is finished.~%"))

(defpackage :gtk-testsuite
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo
   :cffi :common-lisp :bordeaux-threads :fiveam))

(in-package :gtk-testsuite)

(def-suite gtk-testsuite)
(in-suite gtk-testsuite)

(load "rtest-utilities.lisp")
(load "rtest-glib.lisp")
(load "rtest-gobject.lisp")
(load "rtest-gdk.lisp")
(load "rtest-gdk-pixbuf.lisp")
(load "rtest-gtk.lisp")
(load "rtest-gio.lisp")
(load "rtest-pango.lisp")
(load "rtest-cairo.lisp")

;;; 2021-10-20
