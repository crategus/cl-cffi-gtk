(def-suite gdk-color :in gdk-suite)
(in-suite gdk-color)

;;;     GdkColor

(test gdk-color-structure
  ;; Type check
  (is (g-type-is-a (gtype "GdkColor") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GdkColor")
          (gtype (foreign-funcall "gdk_color_get_type" g-size)))))

;;;     gdk-color-new

(test gdk-color-new
  (let ((color (gdk-color-new :red 255 :green 155 :blue 55)))
    (is (typep color 'gdk-color))
    (is (= 255 (gdk-color-red color)))
    (is (= 155 (gdk-color-green color)))
    (is (=  55 (gdk-color-blue color)))))

;;;     gdk_color_copy

(test gdk-color-copy
  (let* ((color (gdk-color-new :red 255 :green 155 :blue 55))
         (color2 (gdk-color-copy color)))
    (is (typep color2 'gdk-color))
    (is (= 255 (gdk-color-red color2)))
    (is (= 155 (gdk-color-green color2)))
    (is (=  55 (gdk-color-blue color2)))))

;;;     gdk_color_parse

(test gdk-color-parse.1
  (let ((color (gdk-color-parse "Red")))
    (is (typep color 'gdk-color))
    (is (=  65535 (gdk-color-red color)))
    (is (=      0 (gdk-color-green color)))
    (is (=      0 (gdk-color-blue color)))))

(test gdk-color-parse.2
  (let ((color (gdk-color-parse "#ffff0000aaaa")))
    (is (typep color 'gdk-color))
    (is (=  65535 (gdk-color-red color)))
    (is (=      0 (gdk-color-green color)))
    (is (=  43690 (gdk-color-blue color)))))

(test gdk-color-parse.3
  (let ((color (gdk-color-parse "#ff00aa")))
    (is (typep color 'gdk-color))
    (is (=  65535 (gdk-color-red color)))
    (is (=      0 (gdk-color-green color)))
    (is (=  43690 (gdk-color-blue color)))))

(test gdk-color-parse.4
  (let ((color (gdk-color-parse "#f0a")))
    (is (typep color 'gdk-color))
    (is (=  65535 (gdk-color-red color)))
    (is (=      0 (gdk-color-green color)))
    (is (=  43690 (gdk-color-blue color)))))

;;;     gdk_color_equal

(test gdk-color-equal.1
  (let ((color1 (gdk-color-new :red 1 :green 2 :blue 2))
        (color2 (gdk-color-new :red 1 :green 2 :blue 2)))
    (is-true (gdk-color-equal color1 color2))))

(test gdk-color-equal.2
  (let ((color (gdk-color-new :red 1 :green 2 :blue 2)))
    (is-true (gdk-color-equal color (gdk-color-copy color)))))

;;;     gdk_color_hash

(test gdk-color-to-hash
  (let ((color (gdk-color-parse "Red")))
    (is (= 65535 (gdk-color-hash color)))))

;;;     gdk_color_to_string

(test gdk-color-to-string.1
  (let ((color (gdk-color-parse "Red")))
    (is (string= "#ffff00000000" (gdk-color-to-string color)))))

(test gdk-color-to-string.2
  (let ((color (gdk-color-parse "#ffff0000aaaa")))
    (is (string= "#ffff0000aaaa" (gdk-color-to-string color)))))

;;; 2021-1-22
