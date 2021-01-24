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

;;; --- Accessors --------------------------------------------------------------

(test gdk-rgba-accessors
  (let ((rgba (gdk-rgba-new :red 0.1d0 :green 0.2d0 :blue 0.3d0 :alpha 0.5d0)))
    (is (= 0.1d0 (gdk-rgba-red rgba)))
    (is (= 0.2d0 (gdk-rgba-green rgba)))
    (is (= 0.3d0 (gdk-rgba-blue rgba)))
    (is (= 0.5d0 (gdk-rgba-alpha rgba)))))

;;; --- Functions --------------------------------------------------------------

;;;    gdk-rgba-new

(test gdk-rgba-new
  (is (typep (gdk-rgba-new) 'gdk-rgba))
  (is (typep (gdk-rgba-new :red 0.1 :green 0.2 :blue 0.3 :alpha 0.5) 'gdk-rgba)))

;;;     gdk_rgba_copy

(test gdk-rgba-copy
  (let ((color (gdk-rgba-new :red 0.1 :green 0.2 :blue 0.3 :alpha 0.4)))
    (is (gdk-rgba-equal color (gdk-rgba-copy color)))))

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

;;; 2021-1-22
