(in-package :gtk-testsuite)

(def-suite glib-suite :in gtk-testsuite)
(in-suite glib-suite)

(load "rtest-glib-version.lisp")
(load "rtest-glib-stable-pointer.lisp")
(load "rtest-glib-misc.lisp")
(load "rtest-glib-quark.lisp")
(load "rtest-glib-main-loop.lisp")
(load "rtest-glib-utils.lisp")
(load "rtest-glib-option.lisp")
(load "rtest-glib-key-file.lisp")
(load "rtest-glib-variant.lisp")
(load "rtest-glib-variant-type.lisp")
(load "rtest-glib-bytes.lisp")

;;; 2021-10-17
