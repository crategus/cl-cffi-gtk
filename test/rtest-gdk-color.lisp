
(def-suite gdk-color :in gdk-suite)
(in-suite gdk-color)

(test gdk-color
  (let ((color (make-gdk-color :red 255)))
    (is (eq 'gdk-color (type-of color)))
    (is (eq 'gdk-color (type-of (gdk-color-copy color))))
    (is (eq 'gdk-color (type-of (gdk-color-parse "Red"))))
;    (is (equal "#00ff00000000" (gdk-color-to-string color)))
;    (is (equal "#ffff00000000" (gdk-color-to-string (gdk-color-parse "Red"))))
;    (is-true (gdk-color-equal color (gdk-color-copy color)))
;    (is (= 255 (gdk-color-hash color)))
))

