(def-suite gtk-text-view :in gtk-suite)
(in-suite gtk-text-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextViewLayer
;;;     GtkTextWindowType
;;;     GtkTextExtendSelection
;;;     GtkWrapMode                               ---> gtk.text-attributes.lisp
;;;     GtkTextChildAnchor

;;;     GtkTextView


;;; --- Properties -------------------------------------------------------------

(test gtk-text-view-properties
  (let ((view (make-instance 'gtk-text-view)))
    (is-true (gtk-text-view-accepts-tab view))
    (is (= 0 (gtk-text-view-bottom-margin view)))
    (is (typep (gtk-text-view-buffer view) 'gtk-text-buffer))
    (is-true (gtk-text-view-cursor-visible view))
    (is-true (gtk-text-view-editable view))
    (is-false (gtk-text-view-im-module view))
    (is-true (gtk-text-view-indent view))
    (is-false (gtk-text-view-input-hints view))
    (is (eq :free-form (gtk-text-view-input-purpose view)))
    (is (eq :left (gtk-text-view-justification view)))
    (is (= 0 (gtk-text-view-left-margin view)))
    (is-false (gtk-text-view-monospace view))
    (is-false (gtk-text-view-overwrite view))
    (is (= 0 (gtk-text-view-pixels-above-lines view)))
    (is (= 0 (gtk-text-view-pixels-below-lines view)))
    (is (= 0 (gtk-text-view-pixels-inside-wrap view)))
    (is-false (gtk-text-view-populate-all view))
    (is (= 0 (gtk-text-view-right-margin view)))
    (is-false (gtk-text-view-tabs view))
    (is (= 0 (gtk-text-view-top-margin view)))
    (is (eq :none (gtk-text-view-wrap-mode view)))))

;;; --- Style Properties -------------------------------------------------------

;;;            GdkColor*   error-underline-color    Read

;;; --- Signals ----------------------------------------------------------------

;;;                void    backspace                Action
;;;                void    copy-clipboard           Action
;;;                void    cut-clipboard            Action
;;;                void    delete-from-cursor       Action
;;;            gboolean    extend-selection         Run Last
;;;                void    insert-at-cursor         Action
;;;                void    insert-emoji             Action
;;;                void    move-cursor              Action
;;;                void    move-viewport            Action
;;;                void    paste-clipboard          Action
;;;                void    populate-popup           Run Last
;;;                void    preedit-changed          Action
;;;                void    select-all               Action
;;;                void    set-anchor               Action
;;;                void    toggle-cursor-visible    Action
;;;                void    toggle-overwrite         Action

;;;     GTK_TEXT_VIEW_PRIORITY_VALIDATE


;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_view_new
;;;     gtk_text_view_new_with_buffer
;;;     gtk_text_view_set_buffer                           Accessor
;;;     gtk_text_view_get_buffer                           Accessor
;;;     gtk_text_view_get_hadjustment                      deprecated
;;;     gtk_text_view_get_vadjustment                      deprecated
;;;     gtk_text_view_scroll_to_mark
;;;     gtk_text_view_scroll_to_iter
;;;     gtk_text_view_scroll_mark_onscreen
;;;     gtk_text_view_move_mark_onscreen
;;;     gtk_text_view_place_cursor_onscreen
;;;
;;;     gtk_text_view_get_visible_rect
;;;     gtk_text_view_get_iter_location
;;;     gtk_text_view_get_cursor_locations
;;;     gtk_text_view_get_line_at_y
;;;     gtk_text_view_get_line_yrange
;;;     gtk_text_view_get_iter_at_location
;;;     gtk_text_view_get_iter_at_position
;;
;;;     gtk_text_view_buffer_to_window_coords
;;;     gtk_text_view_window_to_buffer_coords
;;;
;;;     gtk_text_view_get_window
;;;     gtk_text_view_get_window_type
;;;     gtk_text_view_set_border_window_size
;;;     gtk_text_view_get_border_window_size
;;;
;;;     gtk_text_view_forward_display_line
;;;     gtk_text_view_backward_display_line
;;;     gtk_text_view_forward_display_line_end
;;;     gtk_text_view_backward_display_line_start
;;;     gtk_text_view_starts_display_line
;;;     gtk_text_view_move_visually
;;;     gtk_text_view_add_child_at_anchor

;;;     gtk_text_child_anchor_new
;;;     gtk_text_child_anchor_get_widgets
;;;     gtk_text_child_anchor_get_deleted
;;;     gtk_text_view_add_child_in_window
;;;     gtk_text_view_move_child
;;;     gtk_text_view_set_wrap_mode                        Accessor
;;;     gtk_text_view_get_wrap_mode                        Accessor
;;;     gtk_text_view_set_editable                         Accessor
;;;     gtk_text_view_get_editable                         Accessor
;;;     gtk_text_view_set_cursor_visible                   Accessor
;;;     gtk_text_view_get_cursor_visible                   Accessor
;;;     gtk_text_view_reset_cursor_blink
;;;     gtk_text_view_set_overwrite                        Accessor
;;;     gtk_text_view_get_overwrite                        Accessor
;;;     gtk_text_view_set_pixels_above_lines               Accessor
;;;     gtk_text_view_get_pixels_above_lines               Accessor
;;;     gtk_text_view_set_pixels_below_lines               Accessor
;;;     gtk_text_view_get_pixels_below_lines               Accessor
;;;     gtk_text_view_set_pixels_inside_wrap               Accessor
;;;     gtk_text_view_get_pixels_inside_wrap               Accessor
;;;     gtk_text_view_set_justification                    Accessor
;;;     gtk_text_view_get_justification                    Accessor
;;;     gtk_text_view_set_left_margin                      Accessor
;;;     gtk_text_view_get_left_margin                      Accessor
;;;     gtk_text_view_set_right_margin                     Accessor
;;;     gtk_text_view_get_right_margin                     Accessor
;;;     gtk_text_view_set_top_margin                       Accessor
;;;     gtk_text_view_get_top_margin                       Accessor
;;;     gtk_text_view_set_bottom_margin                    Accessor
;;;     gtk_text_view_get_bottom_margin                    Accessor
;;;     gtk_text_view_set_indent                           Accessor
;;;     gtk_text_view_get_indent                           Accessor
;;;     gtk_text_view_set_tabs                             Accessor
;;;     gtk_text_view_get_tabs                             Accessor
;;;     gtk_text_view_set_accepts_tab                      Accessor
;;;     gtk_text_view_get_accepts_tab                      Accessor

;;;     gtk-text-view-default-attributes

;; FIXME: The implementation of gtk-text-attributes does not work.

#+nil
(test gtk-text-view-default-attributes
  (let* ((view (make-instance 'gtk-text-view
                              :buffer (make-instance 'gtk-text-buffer)))
         (attr (gtk-text-view-default-attributes view)))
    (is (typep attr 'gtk-text-attributes))
    ;; Check all values of the gtk-text-attributes instance
    (is (pointerp (gtk-text-attributes-appearance attr)))
    (is (eq :dummy2 (gtk-text-attributes-justification attr)))
    (is (eq :none (gtk-text-attributes-direction attr)))
    (is (typep (gtk-text-attributes-font attr) 'pango-font-description))
    (is (= 0.0d0 (gtk-text-attributes-font-scale attr)))
    (is (= 0 (gtk-text-attributes-left-margin attr)))
    (is (= 0 (gtk-text-attributes-right-margin attr)))
    (is (= 0 (gtk-text-attributes-indent attr)))
    (is (= 0 (gtk-text-attributes-pixels-above-lines attr)))
    (is (= 0 (gtk-text-attributes-pixels-below-lines attr)))
    (is (= 1 (gtk-text-attributes-pixels-inside-wrap attr)))
    (is (pointerp (gtk-text-attributes-tabs attr)))
    (is (eq :none (gtk-text-attributes-wrap-mode attr)))
    (is (typep (gtk-text-attributes-language attr) 'pango-language))

    (is (= 0 (gtk-text-attributes-invisible attr)))
    (is (= 0 (gtk-text-attributes-bg-full-height attr)))
    (is (= 0 (gtk-text-attributes-editable attr)))
    (is (= 0 (gtk-text-attributes-no-fallback attr)))

    (is (integerp (gtk-text-attributes-letter-spacing attr)))
    (is (null-pointer-p (gtk-text-attributes-font-features attr)))))

;;;     gtk_text_view_im_context_filter_keypress
;;;     gtk_text_view_reset_im_context
;;;     gtk_text_view_set_input_purpose                    Accessor
;;;     gtk_text_view_get_input_purpose                    Accessor
;;;     gtk_text_view_set_input_hints                      Accessor
;;;     gtk_text_view_get_input_hints                      Accessor
;;;     gtk_text_view_set_monospace                        Accessor
;;;     gtk_text_view_get_monospace                        Accessor

;;; 2021-10-16
