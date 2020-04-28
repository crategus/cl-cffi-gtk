(def-suite gdk-display :in gdk-suite)
(in-suite gdk-display)

;;;     GdkDisplay

(test gdk-display-class
  ;; Type checks
  (is-true  (g-type-is-object "GdkDisplay"))
  (is-false (g-type-is-abstract "GdkDisplay"))
  (is-true  (g-type-is-derived "GdkDisplay"))
  (is-false (g-type-is-fundamental "GdkDisplay"))
  (is-true  (g-type-is-value-type "GdkDisplay"))
  (is-true  (g-type-has-value-table "GdkDisplay"))
  (is-true  (g-type-is-classed "GdkDisplay"))
  (is-true  (g-type-is-instantiatable "GdkDisplay"))
  (is-true  (g-type-is-derivable "GdkDisplay"))
  (is-true  (g-type-is-deep-derivable "GdkDisplay"))
  (is-false (g-type-is-interface "GdkDisplay"))

  ;; Check the registered name
  (is (eq 'gdk-display
          (registered-object-type-by-name "GdkDisplay")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GdkDisplay"))))
    (is (equal (gtype "GdkDisplay") (g-type-from-class class)))
    (is (equal (gtype "GdkDisplay") (g-object-class-type class)))
    (is (equal "GdkDisplay" (g-object-class-name class)))
    (is (equal (gtype "GdkDisplay")
               (g-type-from-class  (g-type-class-peek "GdkDisplay"))))
    (is (equal (gtype "GdkDisplay")
               (g-type-from-class (g-type-class-peek-static "GdkDisplay"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gdk-display)))
    ;; Check the class name and type of the class
    (is (eq 'gdk-display (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GdkDisplay" (gobject-class-g-type-name class)))
    (is (equal "GdkDisplay" (gobject-class-direct-g-type-name class)))
    (is (equal "gdk_display_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GObject") (g-type-parent "GdkDisplay")))
  (is (= 2 (g-type-depth "GdkDisplay")))
  (is (equal (gtype "GdkDisplay")
             (g-type-next-base "GdkDisplay" "GObject")))
  (is-true  (g-type-is-a "GdkDisplay" "GObject"))
  (is-false (g-type-is-a "GdkDisplay" "GtkWidget"))
  (is-false (g-type-is-a "GdkDisplay" "gboolean"))

  ;; Check the children
  (is (equal '("GdkX11Display" "GdkBroadwayDisplay" "GdkWaylandDisplay")
             (mapcar #'gtype-name (g-type-children "GdkDisplay"))))

  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GdkDisplay"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GdkDisplay" query)
    (is (equal (gtype "GdkDisplay")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GdkDisplay"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 536 (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (= 136 (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal '()
             (mapcar #'param-spec-name
                     (g-object-class-list-properties "GdkDisplay"))))

  ;; No style properties
  ;; No child properties

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDisplay" GDK-DISPLAY
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_display_get_type")
                       NIL)
             (get-g-type-definition "GdkDisplay"))))

;;; ----------------------------------------------------------------------------
;;; Functions
;;; ----------------------------------------------------------------------------

;;;     gdk_display_open

(test gdk-display-open
  (let ((name (gdk-display-get-name (gdk-display-default))))
    (is-true (gdk-display-open name))
    (is-false (gdk-display-open "test"))
))

;;;     gdk_display_get_default

(test gdk-display-default
  (is (eq 'gdk-display (type-of (gdk-display-default)))))

;;;     gdk_display_get_name

(test gdk-display-get-name
  (is (equal ":0" (gdk-display-get-name (gdk-display-default)))))

;;;     gdk_display_get_n_screens                     * deprecated
;;;     gdk_display_get_screen                        * deprecated

;;;     gdk_display_get_default_screen

(test gdk-display-default-screen
  (let ((display (gdk-display-default)))
    (is (eq 'gdk-screen (type-of (gdk-display-default-screen display))))))

;;;     gdk_display_get_device_manager                * deprecated
;;;     gdk_display_pointer_ungrab                    * deprecated
;;;     gdk_display_keyboard_ungrab                   * deprecated
;;;     gdk_display_pointer_is_grabbed                * deprecated

;;;     gdk_display_device_is_grabbed



;;;     gdk_display_beep
;;;     gdk_display_sync
;;;     gdk_display_flush

;;;     gdk_display_close



;;;     gdk_display_is_closed

;;;     gdk_display_get_event
;;;     gdk_display_peek_event
;;;     gdk_display_put_event
;;;     gdk_display_has_pending
;;;     gdk_display_set_double_click_time
;;;     gdk_display_set_double_click_distance
;;;     gdk_display_get_pointer                       * deprecated
;;;     gdk_display_list_devices                      * deprecated
;;;     gdk_display_get_window_at_pointer             * deprecated
;;;     gdk_display_warp_pointer                      * deprecated
;;;     gdk_display_supports_cursor_color
;;;     gdk_display_supports_cursor_alpha
;;;     gdk_display_get_default_cursor_size
;;;     gdk_display_get_maximal_cursor_size
;;;     gdk_display_get_default_group
;;;     gdk_display_supports_selection_notification
;;;     gdk_display_request_selection_notification
;;;     gdk_display_supports_clipboard_persistence
;;;     gdk_display_store_clipboard
;;;     gdk_display_supports_shapes
;;;     gdk_display_supports_input_shapes
;;;     gdk_display_supports_composite                * deprecated
;;;     gdk_display_get_app_launch_context
;;;     gdk_display_notify_startup_complete

;;;     gdk_display_get_default_seat

(test gdk-display-default-seat
  (let ((display (gdk-display-default)))
    (is (eq 'gdk-seat (type-of (gdk-display-default-seat display))))))

;;;     gdk_display_list_seats

(test gdk-display-list-seats
  (let ((display (gdk-display-default)))
    (is (listp (gdk-display-list-seats display)))
    (is (eq 'gdk-seat (type-of (first (gdk-display-list-seats display)))))))

;;;     gdk_display_get_n_monitors

(test gdk-display-get-n-monitors
  (let ((display (gdk-display-default)))
    (is (equal 1 (gdk-display-get-n-monitors display)))))

;;;     gdk_display_get_monitor

(test gdk-display-get-monitor
  (let ((display (gdk-display-default)))
    (is (eq 'gdk-monitor (type-of (gdk-display-get-monitor display 0))))))

;;;     gdk_display_get_primary_monitor

(test gdk-display-get-primary-monitor
  (let ((display (gdk-display-default)))
    (is (eq 'gdk-monitor (type-of (gdk-display-get-primary-monitor display))))))

;;;     gdk_display_get_monitor_at_point
;;;     gdk_display_get_monitor_at_window

