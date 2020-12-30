(def-suite cairo-image-surface :in cairo-suite)
(in-suite cairo-image-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     CAIRO_HAS_IMAGE_SURFACE

;;;     cairo_format_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_format_stride_for_width

(test cairo-format-stride-for-width
  (is (= 400 (cairo-format-stride-for-width :argb32 100)))
  (is (= 400 (cairo-format-stride-for-width :rgb24 100)))
  (is (= 100 (cairo-format-stride-for-width :a8 100)))
  (is (=  16 (cairo-format-stride-for-width :a1 100)))
  (is (= 200 (cairo-format-stride-for-width :rgb16-565 100)))
  (is (= 400 (cairo-format-stride-for-width :rgb30 100))))

;;;     cairo_image_surface_create

(test cairo-image-surface-create
  (is (eq :success
          (cairo-surface-status (cairo-image-surface-create :argb32 200 150))))
  (is (eq :success
          (cairo-surface-status (cairo-image-surface-create :rgb24 200 150))))
  (is (eq :success
          (cairo-surface-status (cairo-image-surface-create :a8 200 150))))
  (is (eq :success
          (cairo-surface-status (cairo-image-surface-create :a1 200 150))))
  (is (eq :success
          (cairo-surface-status (cairo-image-surface-create :rgb16-565 200 150))))
  (is (eq :success
          (cairo-surface-status (cairo-image-surface-create :rgb30 200 150)))))

;;;     cairo_image_surface_create_for_data

(test cairo-image-surface-create-for-data
  (let* ((height 150)
         (width 200)
         (stride (cairo-format-stride-for-width :argb32 width))
         (data (g-malloc (* height stride))))
    (is (eq :success
            (cairo-surface-status
              (cairo-image-surface-create-for-data data
                                                   :argb32
                                                   width height stride))))))

;;;     cairo_image_surface_get_data
;;;     cairo_image_surface_get_format
;;;     cairo_image_surface_get_width
;;;     cairo_image_surface_get_height
;;;     cairo_image_surface_get_stride

(test cairo-image-surface-get
  (let* ((height 150)
         (width 200)
         (stride (cairo-format-stride-for-width :argb32 width))
         (data (g-malloc (* height stride)))
         (surface (cairo-image-surface-create-for-data data
                                                       :argb32
                                                       width height stride)))
    (is (pointer-eq data (cairo-image-surface-data surface)))
    (is (eq :argb32 (cairo-image-surface-format surface)))
    (is (= 200 (cairo-image-surface-width surface)))
    (is (= height (cairo-image-surface-height surface)))
    (is (= stride (cairo-image-surface-stride surface)))))

;;; 2020-12-21
