(def-suite gdk-selection :in gdk-suite)
(in-suite gdk-selection)

(defparameter *verbose-gdk-selection* nil)

;;;     gdk_selection_owner_set
;;;     gdk_selection_owner_get

;; TODO: Check this again. In a second run the owner is not unset.

#+nil
(test gdk-selection-owner.1
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      ;; No owner
      (is-false (gdk-selection-owner-get "PRIMARY"))
      ;; Set the owner
      (is-true (gdk-selection-owner-set window
                                        "PRIMARY"
                                        +gdk-current-time+
                                        nil))
      ;; Check the owner
      (is (eq window (gdk-selection-owner-get "PRIMARY")))
      ;; Unset the owner
      (is-true (gdk-selection-owner-set nil
                                        "PRIMARY"
                                        +gdk-current-time+ nil))
      ;; No owner
      (is-false (gdk-selection-owner-get "PRIMARY")))))

;; TODO: Check this again. In a second run the owner is not unset.

#+nil
(test gdk-selection-owner.2
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (is-false (gdk-selection-owner-get "SECONDARY"))
      (is-true (gdk-selection-owner-set window
                                        "SECONDARY"
                                        +gdk-current-time+
                                        nil))
      (is (eq window (gdk-selection-owner-get "SECONDARY")))
      (is-true (gdk-selection-owner-set nil
                                        "SECONDARY"
                                        +gdk-current-time+ nil))
      (is-false (gdk-selection-owner-get "SECONDARY")))))

;; TODO: Check this again. In a second run the owner is not unset.

#+nil
(test gdk-selection-owner.3
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (is-false (gdk-selection-owner-get "CLIPBOARD"))
      (is-true (gdk-selection-owner-set window
                                        "CLIPBOARD"
                                        +gdk-current-time+
                                        nil))
      (is (eq window (gdk-selection-owner-get "CLIPBOARD")))
      (is-true (gdk-selection-owner-set nil
                                        "CLIPBOARD"
                                        +gdk-current-time+ nil))
      (is-false (gdk-selection-owner-get "CLIPBOARD")))))

;;;     gdk_selection_owner_set_for_display
;;;     gdk_selection_owner_get_for_display

;; TODO: Check this again. In a second run the owner is not unset.

#+nil
(test gdk-selection-owner-for-display
  (let ((display (gdk-display-default))
        (widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      ;; No owner
      (is-false (gdk-selection-owner-get-for-display display "PRIMARY"))
      ;; Set the owner
      (is-true (gdk-selection-owner-set-for-display display
                                                    window
                                                    "PRIMARY"
                                                    +gdk-current-time+
                                                    nil))
      (is (eq window
              (gdk-selection-owner-get-for-display display "PRIMARY")))
      ;; Unset the owner
      (is-true (gdk-selection-owner-set-for-display display
                                                    nil
                                                    "PRIMARY"
                                                    +gdk-current-time+ nil))
      ;; No owner
      (is-false (gdk-selection-owner-get-for-display display "PRIMARY")))))

;;;     gdk_selection_convert
;;;     gdk_selection_property_get

(test gdk-selection-convert
  (let ((clipboard (gtk-clipboard-default (gdk-display-default)))
        (widget (make-instance 'gtk-window :type :toplevel)))
    (is (string= "CLIPBOARD" (gtk-clipboard-selection clipboard)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (gdk-selection-owner-set window "CLIPBOARD" +gdk-current-time+ nil)
      (is (eq window (gdk-selection-owner-get "CLIPBOARD")))
      ;; Put some text in  the default clipboard
      (gtk-clipboard-set-text clipboard "This is the text.")
      ;; FIXME: Works not as expeceted. We do not get data from the clipboard.
      ;; What is the correct usage of the GDK-SELECTION-CONVERT function.
      (gdk-selection-convert window "CLIPBOARD" "STRING" +gdk-current-time+)
      (multiple-value-bind (length data type format)
          (gdk-selection-property-get window)
        (when *verbose-gdk-selection*
          (format t "~%PROPERTY-GET~%")
          (format t " length : ~a~%" length)
          (format t "   data : ~a~%" data)
          (format t "        : ~a~%" (convert-from-foreign data :string))
          (format t "   type : ~a~%" type)
          (format t " format : ~a~%" format)))
      (gdk-selection-owner-set nil "CLIPBOARD" +gdk-current-time+ nil))))

;;;     gdk_selection_send_notify
;;;     gdk_selection_send_notify_for_display

;; TODO: Implement a test.

;;; 2021-10-3
