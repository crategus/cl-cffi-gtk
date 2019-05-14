
(def-suite gtk-text-iter :in gtk-suite)
(in-suite gtk-text-iter)

;;;   GtkTextIter

;;;   gtk_text_iter_get_buffer

(test gtk-text-iter-get-buffer
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-start-iter buffer)))
    (is (eq buffer (gtk-text-iter-get-buffer iter)))))

;;;   gtk_text_iter_copy
;;;   gtk_text_iter_assign
;;;   gtk_text_iter_free

;;;   gtk_text_iter_get_offset

(test gtk-text-iter-get-offset
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-get-offset iter)))))

;;;   gtk_text_iter_get_line

(test gtk-text-iter-get-line
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 0 (gtk-text-iter-get-line iter)))))

;;;   gtk_text_iter_get_line_offset

(test gtk-text-iter-get-line-offset
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-get-line-offset iter)))))

;;;  gtk_text_iter_get_line_index

(test gtk-text-iter-get-line-index
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-get-line-index iter)))))

;;;   gtk_text_iter_get_visible_line_index

(test gtk-text-iter-get-visible-line-index
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-get-visible-line-index iter)))))

;;;   gtk_text_iter_get_visible_line_offset

(test gtk-text-iter-get-visible-line-offset
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-get-visible-line-offset iter)))))

;;;   gtk_text_iter_get_char

(test gtk-text-iter-get-char
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (eql #\t (gtk-text-iter-get-char iter)))))

;;;   gtk_text_iter_get_slice

(test gtk-text-iter-get-slice
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (is (equal "text" (gtk-text-iter-get-slice start end)))))

;;;   gtk_text_iter_get_text

(test gtk-text-iter-get-text
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (is (equal "text" (gtk-text-iter-get-text start end)))))

;;;   gtk_text_iter_get_visible_slice

(test gtk-text-iter-get-visible-slice
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (is (equal "text" (gtk-text-iter-get-visible-slice start end)))))

;;;   gtk_text_iter_get_visible_text

(test gtk-text-iter-get-visible-text
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (is (equal "text" (gtk-text-iter-get-visible-text start end)))))

;;;   gtk_text_iter_get_pixbuf

(test gtk-text-iter-get-pixbuf
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (gtk-text-buffer-insert-pixbuf buffer iter (make-instance 'gdk-pixbuf))
    (let ((iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
      (is (eq 'gdk-pixbuf (type-of (gtk-text-iter-get-pixbuf iter)))))))

;;;   gtk_text_iter_get_marks

(test gtk-text-iter-get-marks
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (gtk-text-buffer-add-mark buffer (gtk-text-mark-new nil t) iter)
    (is (eq 'gtk-text-mark
            (type-of (first (gtk-text-iter-get-marks iter)))))))

;;;   gtk_text_iter_get_toggled_tags

(test gtk-text-iter-get-toggled-tags
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (gtk-text-tag-table-add (gtk-text-buffer-tag-table buffer)
                             (make-instance 'gtk-text-tag
                                            :name "bold"
                                            :weight 700))
    (gtk-text-buffer-apply-tag-by-name buffer "bold" start end)
    (is (eq 'gtk-text-tag
            (type-of (first (gtk-text-iter-get-toggled-tags start t)))))))

;;;   gtk_text_iter_get_child_anchor

#+nil
(test gtk-text-iter-get-child-anchor
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (gtk-text-buffer-create-child-anchor buffer iter)
    (is (eq 'gtk-text-tag
            (type-of (gtk-text-iter-get-child-anchor iter))))))


;;;     gtk_text_iter_begins_tag
;;;     gtk_text_iter_ends_tag
;;;     gtk_text_iter_toggles_tag
;;;     gtk_text_iter_has_tag
;;;     gtk_text_iter_get_tags
;;;     gtk_text_iter_editable
;;;     gtk_text_iter_can_insert
;;;     gtk_text_iter_starts_word
;;;     gtk_text_iter_ends_word
;;;     gtk_text_iter_inside_word
;;;     gtk_text_iter_starts_line
;;;     gtk_text_iter_ends_line
;;;     gtk_text_iter_starts_sentence
;;;     gtk_text_iter_ends_sentence
;;;     gtk_text_iter_inside_sentence
;;;     gtk_text_iter_is_cursor_position
;;;     gtk_text_iter_get_chars_in_line
;;;     gtk_text_iter_get_bytes_in_line
;;;     gtk_text_iter_get_attributes
;;;     gtk_text_iter_get_language
;;;     gtk_text_iter_is_end
;;;     gtk_text_iter_is_start
;;;     gtk_text_iter_forward_char
;;;     gtk_text_iter_backward_char
;;;     gtk_text_iter_forward_chars
;;;     gtk_text_iter_backward_chars
;;;     gtk_text_iter_forward_line
;;;     gtk_text_iter_backward_line
;;;     gtk_text_iter_forward_lines
;;;     gtk_text_iter_backward_lines
;;;     gtk_text_iter_forward_word_ends
;;;     gtk_text_iter_backward_word_starts
;;;     gtk_text_iter_forward_word_end
;;;     gtk_text_iter_backward_word_start
;;;     gtk_text_iter_forward_cursor_position
;;;     gtk_text_iter_backward_cursor_position
;;;     gtk_text_iter_forward_cursor_positions
;;;     gtk_text_iter_backward_cursor_positions
;;;     gtk_text_iter_backward_sentence_start
;;;     gtk_text_iter_backward_sentence_starts
;;;     gtk_text_iter_forward_sentence_end
;;;     gtk_text_iter_forward_sentence_ends
;;;     gtk_text_iter_forward_visible_word_ends
;;;     gtk_text_iter_backward_visible_word_starts
;;;     gtk_text_iter_forward_visible_word_end
;;;     gtk_text_iter_backward_visible_word_start
;;;     gtk_text_iter_forward_visible_cursor_position
;;;     gtk_text_iter_backward_visible_cursor_position
;;;     gtk_text_iter_forward_visible_cursor_positions
;;;     gtk_text_iter_backward_visible_cursor_positions
;;;     gtk_text_iter_forward_visible_line
;;;     gtk_text_iter_backward_visible_line
;;;     gtk_text_iter_forward_visible_lines
;;;     gtk_text_iter_backward_visible_lines
;;;     gtk_text_iter_set_offset
;;;     gtk_text_iter_set_line
;;;     gtk_text_iter_set_line_offset
;;;     gtk_text_iter_set_line_index
;;;     gtk_text_iter_set_visible_line_index
;;;     gtk_text_iter_set_visible_line_offset
;;;     gtk_text_iter_forward_to_end
;;;     gtk_text_iter_forward_to_line_end
;;;     gtk_text_iter_forward_to_tag_toggle
;;;     gtk_text_iter_backward_to_tag_toggle
;;;     gtk_text_iter_forward_find_char
;;;     gtk_text_iter_backward_find_char
;;;
;;;     GtkTextSearchFlags
;;;
;;;     gtk_text_iter_forward_search
;;;     gtk_text_iter_backward_search
;;;     gtk_text_iter_equal
;;;     gtk_text_iter_compare
;;;     gtk_text_iter_in_range
;;;     gtk_text_iter_order

