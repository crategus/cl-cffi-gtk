(def-suite cairo-pattern :in cairo-suite)
(in-suite cairo-pattern)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_pattern_t
;;;     cairo_extend_t
;;;     cairo_filter_t
;;;     cairo_pattern_type_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_pattern_add_color_stop_rgb
;;;     cairo_pattern_add_color_stop_rgba
;;;     cairo_pattern_get_color_stop_count
;;;     cairo_pattern_get_color_stop_rgba
;;;     cairo_pattern_create_rgb
;;;     cairo_pattern_create_rgba
;;;     cairo_pattern_get_rgba
;;;     cairo_pattern_create_for_surface
;;;     cairo_pattern_get_surface
;;;     cairo_pattern_create_linear
;;;     cairo_pattern_get_linear_points
;;;     cairo_pattern_create_radial
;;;     cairo_pattern_get_radial_circles
;;;     cairo_pattern_create_mesh
;;;     cairo_mesh_pattern_begin_patch
;;;     cairo_mesh_pattern_end_patch
;;;     cairo_mesh_pattern_move_to
;;;     cairo_mesh_pattern_line_to
;;;     cairo_mesh_pattern_curve_to
;;;     cairo_mesh_pattern_set_control_point
;;;     cairo_mesh_pattern_set_corner_color_rgb
;;;     cairo_mesh_pattern_set_corner_color_rgba
;;;     cairo_mesh_pattern_get_patch_count
;;;     cairo_mesh_pattern_get_path
;;;     cairo_mesh_pattern_get_control_point
;;;     cairo_mesh_pattern_get_corner_color_rgba
;;;     cairo_pattern_reference
;;;     cairo_pattern_destroy
;;;     cairo_pattern_status

;;;     cairo_pattern_set_extend
;;;     cairo_pattern_get_extend

;;;     cairo_pattern_set_filter
;;;     cairo_pattern_get_filter

;;;     cairo_pattern_set_matrix
;;;     cairo_pattern_get_matrix

#+nil
(test cairo-pattern-matrix
  (let ((pattern (cairo-pattern-create-rgb 1.0 0.0 0.0)))

    (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)
               (cairo-matrix-to-list (cairo-pattern-get-matrix pattern))))

))

;;;     cairo_pattern_get_type
;;;     cairo_pattern_get_reference_count
;;;     cairo_pattern_set_user_data
;;;     cairo_pattern_get_user_data


;;; 2020-12-25
