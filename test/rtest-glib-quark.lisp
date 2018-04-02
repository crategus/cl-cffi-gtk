(in-package :gtk-testsuite)

(def-suite glib-quark :in glib-suite)
(in-suite glib-quark)

(test g-quark-from-string
  (is-true (integerp (g-quark-from-string "string1")))
  (is-true (integerp (g-quark-from-string "string2"))))

(test g-quark-to-string
  (let ((id-1 (g-quark-from-string "string1"))
        (id-2 (g-quark-from-string "string2")))
    (is (equal "string1" (g-quark-to-string id-1)))
    (is (equal "string2" (g-quark-to-string id-2)))))

