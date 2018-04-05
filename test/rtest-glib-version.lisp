(in-package :gtk-testsuite)

(def-suite glib-version :in glib-suite)
(in-suite glib-version)
  
(test glib-check-version
  (is-true (integerp +glib-major-version+))
  (is-true (integerp +glib-minor-version+))
  (is-true (integerp +glib-micro-version+))
  (is-true (integerp +glib-binary-age+))
  (is-true (integerp +glib-interface-age+))
  (is-false (glib-check-version 2 24 0))
  (is-false (glib-check-version 2 32 3))
  (is (equal "GLib version too old (micro mismatch)"
             (glib-check-version 2 42 0))))
