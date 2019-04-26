
(in-package :gtk-testsuite)

(def-suite gdk-suite :in gtk-testsuite)
(in-suite gdk-suite)

(load "rtest-gdk-general.lisp")
(load "rtest-gdk-display-manager.lisp")
(load "rtest-gdk-display.lisp")
(load "rtest-gdk-screen.lisp")
(load "rtest-gdk-seat.lisp")
(load "rtest-gdk-monitor.lisp")

(load "rtest-gdk-events.lisp")

(load "rtest-gdk-key-values.lisp")
(load "rtest-gdk-selections.lisp")
(load "rtest-gdk-visual.lisp")

(load "rtest-gdk-color.lisp")

(load "rtest-gdk-window.lisp")

