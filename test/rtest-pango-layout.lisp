(def-suite pango-layout :in pango-suite)
(in-suite pango-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoWrapMode
;;;     PangoEllipsizeMode
;;;     PangoAlignment
;;;     PangoLayoutLine
;;;     PangoLayoutRun

;;;     PangoLayout
;;;     PangoLayoutIter

;;; --- Functions --------------------------------------------------------------

;;;     pango-layout-new

(test pango-layout-new
  (let* ((widget (make-instance 'gtk-button))
         (context (gtk-widget-pango-context widget)))
  (is (eq (gtype "PangoContext") (g-object-type context)))
  (is (eq (gtype "PangoLayout") (g-object-type (pango-layout-new context))))))

;;;     pango_layout_copy

;;;     pango-layout-context

(test pango-layout-context
  (let* ((widget (make-instance 'gtk-button))
         (context (gtk-widget-pango-context widget))
         (layout (pango-layout-new context)))
  ;; Type checks
  (is (eq (gtype "PangoContext") (g-object-type context)))
  (is (eq (gtype "PangoLayout") (g-object-type layout)))
  ;; Retrieve the context from the layout
  (is (eq (gtype "PangoContext") (g-object-type (pango-layout-context layout))))))

;;;     pango-layout-context-changed
;;;     pango-layout-serial

(test pango-layout-serial
 (let* ((widget (make-instance 'gtk-button))
        (context (gtk-widget-pango-context widget))
        (layout (pango-layout-new context)))
    (is (= 1 (pango-layout-serial layout)))
    (is-false (pango-layout-context-changed layout))
    (is (= 2 (pango-layout-serial layout)))
    (is-false (pango-layout-context-changed layout))
    (is (= 3 (pango-layout-serial layout)))))

;;;     pango-layout-text

(test pango-layout-text
  (let ((layout (make-instance 'pango-layout)))
    (is (string= "" (pango-layout-text layout)))
    (is (string= "text" (setf (pango-layout-text layout) "text")))
    (is (string= "text" (pango-layout-text layout)))))

;;;     pango-layout-character-count

(test pango-layout-character-count
  (let ((layout (make-instance 'pango-layout)))
    (is (= 0  (pango-layout-character-count layout)))
    (is (string= "text" (setf (pango-layout-text layout) "text")))
    (is (= 4 (pango-layout-character-count layout)))))

;;;     pango_layout_set_markup
;;;     pango_layout_set_markup_with_accel
;;;     pango_layout_set_attributes
;;;     pango_layout_get_attributes
;;;     pango_layout_set_font_description
;;;     pango_layout_get_font_description
;;;     pango_layout_set_width
;;;     pango_layout_get_width
;;;     pango_layout_set_height
;;;     pango_layout_get_height
;;;     pango_layout_set_wrap
;;;     pango_layout_get_wrap
;;;     pango_layout_is_wrapped
;;;     pango_layout_set_ellipsize
;;;     pango_layout_get_ellipsize
;;;     pango_layout_is_ellipsized
;;;     pango_layout_set_indent
;;;     pango_layout_get_indent
;;;     pango_layout_get_spacing
;;;     pango_layout_set_spacing
;;;     pango_layout_set_line_spacing
;;;     pango_layout_get_line_spacing
;;;     pango_layout_set_justify
;;;     pango_layout_get_justify
;;;     pango_layout_set_auto_dir
;;;     pango_layout_get_auto_dir
;;;     pango_layout_get_direction
;;;     pango_layout_set_alignment
;;;     pango_layout_get_alignment
;;;     pango_layout_set_tabs
;;;     pango_layout_get_tabs
;;;     pango_layout_set_single_paragraph_mode
;;;     pango_layout_get_single_paragraph_mode
;;;     pango_layout_get_unknown_glyphs_count
;;;     pango_layout_get_log_attrs
;;;     pango_layout_get_log_attrs_readonly
;;;     pango_layout_index_to_pos
;;;     pango_layout_index_to_line_x
;;;     pango_layout_xy_to_index
;;;     pango_layout_get_cursor_pos
;;;     pango_layout_move_cursor_visually
;;;     pango_layout_get_extents
;;;     pango_layout_get_pixel_extents
;;;     pango_layout_get_size
;;;     pango_layout_get_pixel_size
;;;     pango_layout_get_baseline
;;;     pango_layout_get_line_count
;;;     pango_layout_get_line
;;;     pango_layout_get_line_readonly
;;;     pango_layout_get_lines
;;;     pango_layout_get_lines_readonly
;;;     pango_layout_get_iter
;;;     pango_layout_iter_copy
;;;     pango_layout_iter_free
;;;     pango_layout_iter_next_run
;;;     pango_layout_iter_next_char
;;;     pango_layout_iter_next_cluster
;;;     pango_layout_iter_next_line
;;;     pango_layout_iter_at_last_line
;;;     pango_layout_iter_get_index
;;;     pango_layout_iter_get_baseline
;;;     pango_layout_iter_get_run
;;;     pango_layout_iter_get_run_readonly
;;;     pango_layout_iter_get_line
;;;     pango_layout_iter_get_line_readonly
;;;     pango_layout_iter_get_layout
;;;     pango_layout_iter_get_char_extents
;;;     pango_layout_iter_get_cluster_extents
;;;     pango_layout_iter_get_run_extents
;;;     pango_layout_iter_get_line_yrange
;;;     pango_layout_iter_get_line_extents
;;;     pango_layout_iter_get_layout_extents
;;;     pango_layout_line_ref
;;;     pango_layout_line_unref
;;;     pango_layout_line_get_extents
;;;     pango_layout_line_get_pixel_extents
;;;     pango_layout_line_index_to_x
;;;     pango_layout_line_x_to_index
;;;     pango_layout_line_get_x_ranges
;;;     pango_layout_line_get_height

;;; 2020-10-18
