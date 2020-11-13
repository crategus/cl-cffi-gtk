(def-suite gdk-color :in gdk-suite)
(in-suite gdk-color)

;;;   gdk-color-structure

(test gdk-color-structure
  ;; Type checks
  (is-false (g-type-is-object "GdkColor"))
  (is-false (g-type-is-abstract "GdkColor"))
  (is-true  (g-type-is-derived "GdkColor"))
  (is-false (g-type-is-fundamental "GdkColor"))
  (is-true  (g-type-is-value-type "GdkColor"))
  (is-true  (g-type-has-value-table "GdkColor"))
  (is-false (g-type-is-classed "GdkColor"))
  (is-false (g-type-is-instantiatable "GdkColor"))
  (is-true  (g-type-is-derivable "GdkColor"))
  (is-false (g-type-is-deep-derivable "GdkColor"))
  (is-false (g-type-is-interface "GdkColor"))

  ;; Check the fundamental type
  (is (eq (gtype "GBoxed") (g-type-fundamental "GdkColor")))

  ;; Check some more GType information
  (is (eq (gtype "GBoxed") (g-type-parent "GdkColor")))
  (is (= 2 (g-type-depth "GdkColor")))
  (is (eq (gtype "GdkColor")
          (g-type-next-base "GdkColor" "GBoxed")))
  (is-false (g-type-is-a "GdkColor" "GtkWidget"))
  (is-false (g-type-is-a "GdkColor" "GtkContainer"))
  (is-false (g-type-is-a "GdkColor" "gboolean"))
  (is-true  (g-type-is-a "GdkColor" "GBoxed")))

;;;   make-gdk-color

(test make-gdk-color
  (let ((color (make-gdk-color :red 255 :green 155 :blue 55)))
    (is (eq 'gdk-color (type-of color)))
    (is (eql 255 (gdk-color-red color)))
    (is (eql 155 (gdk-color-green color)))
    (is (eql  55 (gdk-color-blue color)))))

;;;   gdk_color_copy

(test gdk-color-copy
  (let* ((color (make-gdk-color :red 255 :green 155 :blue 55))
         (color2 (gdk-color-copy color)))
    (is (eq 'gdk-color (type-of color2)))
    (is (eql 255 (gdk-color-red color2)))
    (is (eql 155 (gdk-color-green color2)))
    (is (eql  55 (gdk-color-blue color2)))))

;;;   gdk_color_free

;;;   not implemented

;;;   gdk_color_parse

(test gdk-color-parse.1
  (let ((color (gdk-color-parse "Red")))
    (is (eq 'gdk-color (type-of color)))
    (is (eql  65535 (gdk-color-red color)))
    (is (eql      0 (gdk-color-green color)))
    (is (eql      0 (gdk-color-blue color)))))

(test gdk-color-parse.2
  (let ((color (gdk-color-parse "#ffff0000aaaa")))
    (is (eq 'gdk-color (type-of color)))
    (is (eql  65535 (gdk-color-red color)))
    (is (eql      0 (gdk-color-green color)))
    (is (eql  43690 (gdk-color-blue color)))))

(test gdk-color-parse.3
  (let ((color (gdk-color-parse "#ff00aa")))
    (is (eq 'gdk-color (type-of color)))
    (is (eql  65535 (gdk-color-red color)))
    (is (eql      0 (gdk-color-green color)))
    (is (eql  43690 (gdk-color-blue color)))))

(test gdk-color-parse.4
  (let ((color (gdk-color-parse "#f0a")))
    (is (eq 'gdk-color (type-of color)))
    (is (eql  65535 (gdk-color-red color)))
    (is (eql      0 (gdk-color-green color)))
    (is (eql  43690 (gdk-color-blue color)))))

;;;   gdk_color_equal

(test gdk-color-equal.1
  (let ((color1 (make-gdk-color :red 1 :green 2 :blue 2))
        (color2 (make-gdk-color :red 1 :green 2 :blue 2)))
    (is-true (gdk-color-equal color1 color2))))

(test gdk-color-equal.2
  (let ((color (make-gdk-color :red 1 :green 2 :blue 2)))
    (is-true (gdk-color-equal color (gdk-color-copy color)))))

;;;   gdk_color_hash

(test gdk-color-to-hash
  (let ((color (gdk-color-parse "Red")))
    (is (eql 65535 (gdk-color-hash color)))))

;;;   gdk_color_to_string

(test gdk-color-to-string.1
  (let ((color (gdk-color-parse "Red")))
    (is (equal "#ffff00000000" (gdk-color-to-string color)))))

(test gdk-color-to-string.2
  (let ((color (gdk-color-parse "#ffff0000aaaa")))
    (is (equal "#ffff0000aaaa" (gdk-color-to-string color)))))

