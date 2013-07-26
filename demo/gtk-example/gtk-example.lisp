(asdf:load-system :cl-cffi-gtk)

(defpackage :gtk-example
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :gtk-example)

(load "dialog.lisp")
(load "message-dialog-new.lisp")

