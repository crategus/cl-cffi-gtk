(def-suite gdk-rgba :in gdk-suite)
(in-suite gdk-rgba)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkRGBA

(test gdk-rgba-structure
  ;; Type check
  (is (g-type-is-a (gtype "GdkRGBA") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GdkRGBA")
          (gtype (foreign-funcall "gdk_rgba_get_type" g-size)))))

;;; --- Constructors -----------------------------------------------------------

(test gdk-rgba-accessors
  (is (typep (make-gdk-rgba) 'gdk-rgba))
  (is (typep (copy-gdk-rgba (make-gdk-rgba)) 'gdk-rgba)))

;;; --- Accessors --------------------------------------------------------------

(test gdk-rgba-accessors
  (let ((rgba (make-gdk-rgba :red 1.0d0 :green 2.0d0 :blue 3.0d0 :alpha 0.5d0)))
    (is (= 1.0d0 (gdk-rgba-red rgba)))
    (is (= 2.0d0 (gdk-rgba-green rgba)))
    (is (= 3.0d0 (gdk-rgba-blue rgba)))
    (is (= 0.5d0 (gdk-rgba-alpha rgba)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_rgba_copy
;;;     gdk_rgba_free

;;;     gdk_rgba_parse

(test gdk-rgba-parse
  (is (typep (gdk-rgba-parse "red") 'gdk-rgba))
  (is (= 1.0d0 (gdk-rgba-red (gdk-rgba-parse "red")))))

;;;     gdk_rgba_equal

(test gdk-rgba-equal
  (is (gdk-rgba-equal (gdk-rgba-parse "red") (gdk-rgba-parse "red"))))

;;;     gdk_rgba_hash

(test gdk-rgba-hash
  (is (integerp (gdk-rgba-hash (gdk-rgba-parse "red")))))

;;;     gdk_rgba_to_string

(test gdk-rgba-to-string
  (is (string= "rgb(255,0,0)" (gdk-rgba-to-string (gdk-rgba-parse "red")))))

;;; 2020-11-10
