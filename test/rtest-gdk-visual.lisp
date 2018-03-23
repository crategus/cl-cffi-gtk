(in-package :gtk-testsuite)

(def-suite gdk-visual :in gdk-suite)
(in-suite gdk-visual)

;;;     GdkVisual
;;;     GdkVisualType
;;;     GdkByteOrder

;;;   gdk_query_depths

(test gdk-query-depths
  (is-true (listp (gdk-query-depths))))

;;;   gdk_query_visual_types

(test gdk-query-visual-types
  (is-true (listp (gdk-query-visual-types))))

;;;   gdk_list_visuals

(test gdk-list-visuals
  (let ((visuals (gdk-list-visuals)))
    (is (> (length visuals) 0))
    (is (eq 'gdk-visual (type-of (first visuals))))))

;;;   gdk_visual_get_bits_per_rgb

(test gdk-visual-get-bits-per-rgb
  (let ((visual (gdk-visual-get-system)))
    (is-true (integerp (gdk-visual-get-bits-per-rgb visual)))))

;;;   gdk_visual_get_blue_pixel_details

(test gdk-visual-get-blue-pixel-details
  (let ((visual (gdk-visual-get-system)))
    (multiple-value-bind (mask shift prec)
        (gdk-visual-get-blue-pixel-details visual)
      (is (= #xff mask))
      (is (= 0 shift))
      (is (= 8 prec)))))

;;;   gdk_visual_get_byte_order

(test gdk-visual-get-byte-order
  (let ((visual (gdk-visual-get-system)))
    (is (eq :lsb-first (gdk-visual-get-byte-order visual)))))

;;;   gdk_visual_get_colormap_size

(test gdk-visual-get-colormap-size
  (let ((visual (gdk-visual-get-system)))
    (is (= 256 (gdk-visual-get-colormap-size visual)))))

;;;   gdk_visual_get_depth

(test gdk-visual-get-depth
  (let ((visual (gdk-visual-get-system)))
    (is (= 24 (gdk-visual-get-depth visual)))))

;;;   gdk_visual_get_green_pixel_details

(test gdk-visual-get-green-pixel-details
  (let ((visual (gdk-visual-get-system)))
    (multiple-value-bind (mask shift prec)
        (gdk-visual-get-green-pixel-details visual)
      (is (= #xff00 mask))
      (is (= 8 shift))
      (is (= 8 prec)))))

;;;   gdk_visual_get_red_pixel_details

(test gdk-visual-get-red-pixel-details
  (let ((visual (gdk-visual-get-system)))
    (multiple-value-bind (mask shift prec)
        (gdk-visual-get-red-pixel-details visual)
      (is (= #xff0000 mask))
      (is (= 16 shift))
      (is (= 8 prec)))))

;;;   gdk_visual_get_visual_type

(test gdk-visual-get-visual-type
  (let ((visual (gdk-visual-get-system)))
    (is (member (gdk-visual-get-visual-type visual)
                '(:static-gray 
                  :grayscale
                  :static-color :pseudo-color :true-color :direct-color)))))

;;;   gdk_visual_get_best_depth

(test gdk-visual-get-best-depth
  (is-true (integerp (gdk-visual-get-best-depth))))

;;;   gdk_visual_get_best_type

(test gdk-visual-get-best-type
  (is (member (gdk-visual-get-best-type)
              '(:static-gray 
                :grayscale
                :static-color :pseudo-color :true-color :direct-color))))

;;;   gdk_visual_get_system

(test gdk-visual-get-system
  (is (eq 'gdk-visual (type-of (gdk-visual-get-system)))))

;;;     gdk_visual_get_best
;;;     gdk_visual_get_best_with_depth
;;;     gdk_visual_get_best_with_type
;;;     gdk_visual_get_best_with_both
;;;     gdk_visual_get_screen

