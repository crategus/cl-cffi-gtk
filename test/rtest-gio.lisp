
(in-package :gtk-testsuite)

(def-suite gio-suite :in gtk-testsuite)
(in-suite gio-suite)

(load "rtest-gio-action.lisp")
(load "rtest-gio-icon.lisp")
(load "rtest-gio-simple-action.lisp")
(load "rtest-gio-simple-action-group.lisp")
(load "rtest-gio-themed-icon.lisp")

