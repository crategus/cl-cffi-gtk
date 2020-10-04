(def-suite glib-quark :in glib-suite)
(in-suite glib-quark)

(test g-quark-convert-to-foreign
  (is (= 0 (convert-to-foreign nil 'g-quark)))
  (is (= 0 (convert-to-foreign (null-pointer) 'g-quark)))
  (is (integerp (convert-to-foreign "gboolean" 'g-quark))))

(test g-quark-convert-from-foreign
  (let ((id (convert-to-foreign "string1" 'g-quark)))
    (is (string= "string1" (convert-from-foreign id 'g-quark))))
  (is-false (convert-from-foreign 0 'g-quark))
  (is (stringp (convert-from-foreign 9 'g-quark))))

;;; 2020-10-3
