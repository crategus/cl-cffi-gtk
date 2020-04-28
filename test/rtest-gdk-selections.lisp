
(def-suite gdk-selections :in gdk-suite)
(in-suite gdk-selections)

;;;   GDK_SELECTION_PRIMARY

(test gdk-selection-primary
  (is (equal "PRIMARY" +gdk-selection-primary+)))

;;;   GDK_SELECTION_SECONDARY

(test gdk-selection-secondary
  (is (equal "SECONDARY" +gdk-selection-secondary+)))

;;;   GDK_SELECTION_CLIPBOARD

(test gdk-selection-clipboard
  (is (equal "CLIPBOARD" +gdk-selection-clipboard+)))

;;;     GDK_TARGET_BITMAP
;;;     GDK_TARGET_COLORMAP
;;;     GDK_TARGET_DRAWABLE
;;;     GDK_TARGET_PIXMAP
;;;     GDK_TARGET_STRING
;;;     GDK_SELECTION_TYPE_ATOM
;;;     GDK_SELECTION_TYPE_BITMAP
;;;     GDK_SELECTION_TYPE_COLORMAP
;;;     GDK_SELECTION_TYPE_DRAWABLE
;;;     GDK_SELECTION_TYPE_INTEGER
;;;     GDK_SELECTION_TYPE_PIXMAP
;;;     GDK_SELECTION_TYPE_WINDOW
;;;     GDK_SELECTION_TYPE_STRING

;;;   gdk_selection_owner_set

(test gdk-selection-owner-set.1
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (is (eq 'gdk-window (type-of window)))
      (is-true (gdk-selection-owner-set window
                                        "PRIMARY"
                                        +gdk-current-time+
                                        nil)))))

(test gdk-selection-owner-set.2
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (is (eq 'gdk-window (type-of window)))
      (is-true (gdk-selection-owner-set window
                                        "SECONDARY"
                                        +gdk-current-time+
                                        nil)))))

(test gdk-selection-owner-set.3
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (is (eq 'gdk-window (type-of window)))
      (is-true (gdk-selection-owner-set window
                                        "CLIPBOARD"
                                        +gdk-current-time+
                                        nil)))))

;;;   gdk_selection_owner_set_for_display

(test gdk-selection-owner-set-for-display
  (let ((display (gdk-display-default))
        (widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (is (eq 'gdk-window (type-of window)))
      (is-true (gdk-selection-owner-set-for-display display
                                                    window
                                                    "PRIMARY"
                                                    +gdk-current-time+
                                                    nil)))))

;;;   gdk_selection_owner_get

(test gdk-selection-owner-get
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (is (eq 'gdk-window (type-of window)))
      (is-true (gdk-selection-owner-set window
                                        "PRIMARY"
                                        +gdk-current-time+
                                        nil))
      (is (eq 'gdk-window (type-of (gdk-selection-owner-get "PRIMARY"))))
      (is (eq window (gdk-selection-owner-get "PRIMARY"))))))

;;;   gdk_selection_owner_get_for_display

(test gdk-selection-owner-get-for-display
  (let ((display (gdk-display-default))
        (widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (is (eq 'gdk-window (type-of window)))
      (is-true (gdk-selection-owner-set window
                                        "PRIMARY"
                                        +gdk-current-time+
                                        nil))
      (is (eq 'gdk-window (type-of (gdk-selection-owner-get "PRIMARY"))))
      (is-true (gdk-selection-owner-set-for-display display
                                                    window
                                                    "PRIMARY"
                                                    +gdk-current-time+
                                                    nil))
      (is (eq 'gdk-window
              (type-of (gdk-selection-owner-get-for-display display
                                                            "PRIMARY"))))
      (is (eq window
              (gdk-selection-owner-get-for-display display "PRIMARY"))))))

;;;   gdk_selection_convert

(test gdk-selection-convert
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      (is (eq 'gdk-window (type-of window)))
      (gdk-selection-convert window "PRIMARY" "STRING" +gdk-current-time+))))

;;;     gdk_selection_property_get
;;;     gdk_selection_send_notify
;;;     gdk_selection_send_notify_for_display

