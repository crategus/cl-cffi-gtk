(asdf:load-system :cl-cffi-gtk)

(defpackage :gtk-example
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :gtk-example)

(load "dialog.lisp")
(load "file-chooser-dialog.lisp")
(load "grab.lisp")
(load "list-store.lisp")
(load "menu-builder.lisp")
(load "message-dialog-new.lisp")
(load "show-about-dialog.lisp")

