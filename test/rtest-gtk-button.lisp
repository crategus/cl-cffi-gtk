(def-suite gtk-button :in gtk-suite)
(in-suite gtk-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkButton

;;; --- Properties -------------------------------------------------------------

(test gtk-button-properties
  (let ((button (make-instance 'gtk-button)))
    (is-false (gtk-button-always-show-image button))
    (is-false (gtk-button-image button))
    (is (eq :left (gtk-button-image-position button)))
    (is-false (gtk-button-label button))
    (is (eq :normal (gtk-button-relief button)))
    (is-false (gtk-button-use-stock button))
    (is-false (gtk-button-use-underline button))
    (is (= 0.5 (gtk-button-xalign button)))
    (is (= 0.5 (gtk-button-yalign button)))))

;;; --- Style Properties -------------------------------------------------------

;;;          gint   child-displacement-x      Read
;;;          gint   child-displacement-y      Read
;;;     GtkBorder*  default-border            Read
;;;     GtkBorder*  default-outside-border    Read
;;;      gboolean   displace-focus            Read
;;;          gint   image-spacing             Read
;;;     GtkBorder*  inner-border              Read



;;; --- Functions --------------------------------------------------------------

;;;     gtk_button_new
;;;     gtk_button_new_with_label
;;;     gtk_button_new_with_mnemonic

;;;     gtk_button_new_from_icon_name

(test gtk-button-new-from-icon-name
  (let ((button (gtk-button-new-from-icon-name "edit-copy" :button)))
    (is-false (gtk-button-always-show-image button))
    (is (eq 'gtk-image (type-of (gtk-button-image button))))
    (is (eq :left (gtk-button-image-position button)))
    (is-false (gtk-button-label button))
    (is (eq :normal (gtk-button-relief button)))
    (is-false (gtk-button-use-stock button))
    (is-false (gtk-button-use-underline button))
    (is (= 0.5 (gtk-button-xalign button)))
    (is (= 0.5 (gtk-button-yalign button)))))

;;;     gtk_button_new_from_stock

;;;     gtk_button_clicked

;;;     gtk_button_set_alignment                         * deprecated
;;;     gtk_button_get_alignment                         * deprecated

(test gtk-button-alignment
  (let ((button (make-instance 'gtk-button))
        (label (make-instance 'gtk-label)))
    (is-false (gtk-container-add button label))
    (is (equal '(0.5 0.5) (gtk-button-alignment button)))
    (is (equal '(0.1 0.9) (setf (gtk-button-alignment button) '(0.1 0.9))))
    (is (equal '(0.1 0.9) (gtk-button-alignment button)))))

;;;     gtk_button_get_event_window

(test gtk-button-event-window
  (is-false (gtk-button-event-window (make-instance 'gtk-button))))

