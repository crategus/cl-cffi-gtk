(def-suite cairo-context :in cairo-suite)
(in-suite cairo-context)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_antialias_t
;;;     cairo_fill_rule_t
;;;     cairo_line_cap_t
;;;     cairo_line_join_t
;;;     cairo_operator_t
;;;
;;;     cairo_rectangle_t
;;;     cairo_rectangle_list_t

;;;     cairo_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_create

(test cairo-create
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is (pointerp context))
    (is (eq :success (cairo-status context)))
    (is-false (cairo-destroy context))))

;;;     cairo_reference
;;;     cairo_destroy

(test cairo-reference
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is (pointerp context))
    (is (eq :success (cairo-status context)))
    (is (= 1 (cairo-get-reference-count context)))
    (is (pointerp (cairo-reference context)))
    (is (= 2 (cairo-get-reference-count context)))
    (is-false (cairo-destroy context))
    (is (= 1 (cairo-get-reference-count context)))
    (is-false (cairo-destroy context))
    (is (= 0 (cairo-get-reference-count context)))))

;;;     cairo_status

(test cairo-status
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is (eq :success (cairo-status context)))
    (is-false (cairo-destroy context))))

;;;   cairo_save
;;;   cairo_restore

;;;   cairo_get_target

(test cairo-get-target
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is (eq :success (cairo-status context)))
    (is-true (pointer-eq surface (cairo-get-target context)))
    (cairo-destroy context)
    (cairo-surface-destroy surface)))

;;;     cairo_push_group
;;;     cairo_push_group_with_content
;;;     cairo_pop_group
;;;     cairo_pop_group_to_source
;;;     cairo_get_group_target
;;;     cairo_set_source_rgb
;;;     cairo_set_source_rgba
;;;     cairo_set_source
;;;     cairo_set_source_surface
;;;     cairo_get_source

;;;     cairo_set_antialias
;;;     cairo_get_antialias

(test cairo-antialias
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is (eq :default (cairo-get-antialias context)))
    (is-false (cairo-set-antialias context :none))
    (is (eq :none (cairo-get-antialias context)))
    (is-false (cairo-set-antialias context :gray))
    (is (eq :gray (cairo-get-antialias context)))
    (is-false (cairo-set-antialias context :subpixel))
    (is (eq :subpixel (cairo-get-antialias context)))
    (is-false (cairo-set-antialias context :fast))
    (is (eq :fast (cairo-get-antialias context)))
    (is-false (cairo-set-antialias context :good))
    (is (eq :good (cairo-get-antialias context)))
    (is-false (cairo-set-antialias context :best))
    (is (eq :best (cairo-get-antialias context)))
    (is-false (cairo-destroy context))
    (is-false (cairo-surface-destroy surface))))

;;;     cairo_set_dash
;;;     cairo_get_dash_count
;;;     cairo_get_dash

(test cairo-dash
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    ;; Get default values
    (is (equal '(() 0.0d0)
                (multiple-value-list (cairo-get-dash context))))
    (is (= 0 (cairo-get-dash-count context)))
    ;; Set dashes
    (is-false (cairo-set-dash context '(1.0 2.0 3.0 4.0) 3))
    (is (equal '((1.0d0 2.0d0 3.0d0 4.0d0) 3.0d0)
               (multiple-value-list (cairo-get-dash context))))
    (is (= 4 (cairo-get-dash-count context)))
    ;; Clear dashes
    (is-false (cairo-set-dash context '() 0))
    (is (equal '(() 0.0d0)
               (multiple-value-list (cairo-get-dash context))))
    (is (= 0 (cairo-get-dash-count context)))
    ;; Set an invalid dash
    (is-false (cairo-set-dash context '(1.0 -2.0) 0))
    (is (eq :invalid-dash (cairo-status context)))
    ;; Free resources
    (is-false (cairo-destroy context))
    (is-false (cairo-surface-destroy surface))))

;;;     cairo_set_fill_rule
;;;     cairo_get_fill_rule
;;;
;;;     cairo_set_line_cap
;;;     cairo_get_line_cap
;;;
;;;     cairo_set_line_join
;;;     cairo_get_line_join
;;;     cairo_set_line_width
;;;     cairo_get_line_width
;;;     cairo_set_miter_limit
;;;     cairo_get_miter_limit
;;;
;;;     cairo_set_operator
;;;     cairo_get_operator
;;;     cairo_set_tolerance
;;;     cairo_get_tolerance
;;;     cairo_clip
;;;     cairo_clip_preserve
;;;     cairo_clip_extents
;;;     cairo_in_clip
;;;     cairo_reset_clip
;;;
;;;     cairo_rectangle_list_destroy
;;;     cairo_copy_clip_rectangle_list
;;;     cairo_fill
;;;     cairo_fill_preserve
;;;     cairo_fill_extents
;;;     cairo_in_fill
;;;     cairo_mask
;;;     cairo_mask_surface
;;;     cairo_paint
;;;     cairo_paint_with_alpha
;;;     cairo_stroke
;;;     cairo_stroke_preserve
;;;     cairo_stroke_extents
;;;     cairo_in_stroke
;;;     cairo_copy_page
;;;     cairo_show_page
;;;     cairo_get_reference_count

;;;     cairo_set_user_data
;;;     cairo_get_user_data

;;; 2020-12-5
