
(def-suite glib-version :in glib-suite)
(in-suite glib-version)
  
(test glib-check-version
  (is (= 2 glib-major-version))
  (is (= 36 glib-minor-version))
  (is (= 0 glib-micro-version))
  (is (= 3600 glib-binary-age))
  (is (= 0 glib-interface-age))
  (is-false (glib-check-version 2 24 0))
  (is-false (glib-check-version 2 32 3))
  (is (equal "GLib version too old (micro mismatch)"
             (glib-check-version 2 37 0))))

