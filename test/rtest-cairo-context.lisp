(in-package :gtk-testsuite)

(def-suite cairo-context :in cairo-suite)
(in-suite cairo-context)

;;;   cairo_t

;;;   cairo_create

(test cairo-create
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is-true (pointerp context))
    (is (eq :success (cairo-status context)))
    (cairo-destroy context)))

;;;   cairo_reference
;;;   cairo_destroy

(test cairo-create
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is-true (pointerp context))
    (is (eq :success (cairo-status context)))
    (is (eql 1 (cairo-get-reference-count context)))
    (is-true (pointerp (cairo-reference context)))
    (is (eql 2 (cairo-get-reference-count context)))
    (cairo-destroy context)
    (is (eql 1 (cairo-get-reference-count context)))
    (cairo-destroy context)
    (is (eql 0 (cairo-get-reference-count context)))))

;;;   cairo_status

(test cairo-status
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is (eq :success (cairo-status context)))
    (cairo-destroy context)))

;;;   cairo_save
;;;   cairo_restore

;;;   cairo_get_target

(test cairo-get-target
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is (eq :success (cairo-status context)))
    (is-true (pointer-eq surface (cairo-get-target context)))
    (cairo-destroy context)))

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
;;;
;;;     cairo_antialias_t
;;;
;;;     cairo_set_antialias
;;;     cairo_get_antialias
;;;     cairo_set_dash
;;;     cairo_get_dash_count
;;;     cairo_get_dash
;;;
;;;     cairo_fill_rule_t
;;;
;;;     cairo_set_fill_rule
;;;     cairo_get_fill_rule
;;;
;;;     cairo_line_cap_t
;;;
;;;     cairo_set_line_cap
;;;     cairo_get_line_cap
;;;
;;;     cairo_line_join_t
;;;
;;;     cairo_set_line_join
;;;     cairo_get_line_join
;;;     cairo_set_line_width
;;;     cairo_get_line_width
;;;     cairo_set_miter_limit
;;;     cairo_get_miter_limit
;;;
;;;     cairo_operator_t
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
;;;     cairo_rectangle_t
;;;     cairo_rectangle_list_t
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

