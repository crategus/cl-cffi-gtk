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
  (is (eq (gtype "PangoContext")
      (g-object-type (pango-layout-context layout))))))

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

(test pango-layout-width/height
  (let* ((widget (make-instance 'gtk-label))
         (context (gtk-widget-pango-context widget))
         (layout (pango-layout-new context)))

    (is (string= "some text"
                 (setf (pango-layout-text layout) "some text")))

    (is (= -1 (pango-layout-width layout)))
    (is (= -1 (pango-layout-height layout)))

))

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

;; FIXME: The implementation does not work

#+nil
(test pango-layout-log-attrs
  (let* ((widget (make-instance 'gtk-label))
         (context (gtk-widget-pango-context widget))
         (layout (pango-layout-new context)))
    (is (string= "some text"
                 (setf (pango-layout-text layout) "some text")))

    (is-false (pango-layout-log-attrs layout))
))

;;;     pango_layout_get_log_attrs_readonly

;; FIXME: The implementation does not work

#+nil
(test pango-layout-log-attrs-readonly
  (let* ((widget (make-instance 'gtk-label))
         (context (gtk-widget-pango-context widget))
         (layout (pango-layout-new context)))
    (is (string= "some text"
                 (setf (pango-layout-text layout) "some text")))

    (is-false (pango-layout-log-attrs-readonly layout))
))

;;;     pango_layout_index_to_pos

#-windows
(test pango-layout-index-to-pos
  (let* ((widget (make-instance 'gtk-label))
         (context (gtk-widget-pango-context widget))
         (layout (pango-layout-new context)))
    (is (string= "some text"
                 (setf (pango-layout-text layout) "some text")))
    (is (pointerp (pango-layout-index-to-pos layout 0)))
    (let ((rect (pango-layout-index-to-pos layout 0)))
      (is (=     0 (pango-rectangle-x rect)))
      (is (=     0 (pango-rectangle-y rect)))
      (is (=  7168 (pango-rectangle-width rect)))
      (is (= 17408 (pango-rectangle-height rect))))
    (let ((rect (pango-layout-index-to-pos layout 1)))
      (is (=  7168 (pango-rectangle-x rect)))
      (is (=     0 (pango-rectangle-y rect)))
      (is (=  9216 (pango-rectangle-width rect)))
      (is (= 17408 (pango-rectangle-height rect))))
    (let ((rect (pango-layout-index-to-pos layout 2)))
      (is (= 16384 (pango-rectangle-x rect)))
      (is (=     0 (pango-rectangle-y rect)))
      (is (= 13312 (pango-rectangle-width rect)))
      (is (= 17408 (pango-rectangle-height rect))))))

#+windows
(test pango-layout-index-to-pos
  (let* ((widget (make-instance 'gtk-label))
         (context (gtk-widget-pango-context widget))
         (layout (pango-layout-new context)))
    (is (string= "some text"
                 (setf (pango-layout-text layout) "some text")))
    (is (pointerp (pango-layout-index-to-pos layout 0)))
    (let ((rect (pango-layout-index-to-pos layout 0)))
      (is (=     0 (pango-rectangle-x rect)))
      (is (=     0 (pango-rectangle-y rect)))
      (is (=  5120 (pango-rectangle-width rect)))
      (is (= 15360 (pango-rectangle-height rect))))
    (let ((rect (pango-layout-index-to-pos layout 1)))
      (is (=  5120 (pango-rectangle-x rect)))
      (is (=     0 (pango-rectangle-y rect)))
      (is (=  7168 (pango-rectangle-width rect)))
      (is (= 15360 (pango-rectangle-height rect))))
    (let ((rect (pango-layout-index-to-pos layout 2)))
      (is (= 12288 (pango-rectangle-x rect)))
      (is (=     0 (pango-rectangle-y rect)))
      (is (= 10240 (pango-rectangle-width rect)))
      (is (= 15360 (pango-rectangle-height rect))))))

;;;     pango_layout_index_to_line_x

#-windows
(test pango-layout-index-to-line-x
  (let* ((widget (make-instance 'gtk-label))
         (context (gtk-widget-pango-context widget))
         (layout (pango-layout-new context)))
    (is (string= "some text"
                 (setf (pango-layout-text layout) "some text")))
    (is (equal '(0 0)
               (multiple-value-list 
                   (pango-layout-index-to-line-x layout 0 nil))))
    (is (equal '(0 7168)
               (multiple-value-list 
                   (pango-layout-index-to-line-x layout 1 nil))))
    (is (equal '(0 16384)
               (multiple-value-list 
                   (pango-layout-index-to-line-x layout 2 nil))))))

#+windows
(test pango-layout-index-to-line-x
  (let* ((widget (make-instance 'gtk-label))
         (context (gtk-widget-pango-context widget))
         (layout (pango-layout-new context)))
    (is (string= "some text"
                 (setf (pango-layout-text layout) "some text")))
    (is (equal '(0 0)
               (multiple-value-list 
                   (pango-layout-index-to-line-x layout 0 nil))))
    (is (equal '(0 5120)
               (multiple-value-list 
                   (pango-layout-index-to-line-x layout 1 nil))))
    (is (equal '(0 12288)
               (multiple-value-list 
                   (pango-layout-index-to-line-x layout 2 nil))))))

;;;     pango_layout_xy_to_index

(test pango-layout-xy-to-index
  (let* ((widget (make-instance 'gtk-label))
         (context (gtk-widget-pango-context widget))
         (layout (pango-layout-new context)))
    (is (string= "some text"
                 (setf (pango-layout-text layout) "some text")))
    (is (= 0 (pango-layout-xy-to-index layout     0 0)))
    (is (= 1 (pango-layout-xy-to-index layout  7168 0)))
    (is (= 2 (pango-layout-xy-to-index layout 16384 0)))))

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

;;; 2021-1-15
