(in-package :gtk-testsuite)

(def-suite gobject-suite :in gtk-testsuite)
(in-suite gobject-suite)

(load "rtest-gobject-gc")
(load "rtest-gobject-type-info.lisp")
(load "rtest-gobject-g-value.lisp")
(load "rtest-gobject-enumeration.lisp")
(load "rtest-gobject-param.lisp")
(load "rtest-gobject-base.lisp")
(load "rtest-gobject-utils.lisp")
(load "rtest-gobject-signals.lisp")

