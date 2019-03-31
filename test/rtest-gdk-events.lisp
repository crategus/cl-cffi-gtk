(def-suite gdk-events :in gdk-suite)
(in-suite gdk-events)

;;; enum GdkEventType

(test gdk-event-type
  ;; Type checks
  (is-false (g-type-is-object "GdkEventType"))
  (is-false (g-type-is-abstract "GdkEventType"))
  (is-true  (g-type-is-derived "GdkEventType"))
  (is-false (g-type-is-fundamental "GdkEventType"))
  (is-true  (g-type-is-value-type "GdkEventType"))
  (is-true  (g-type-has-value-table "GdkEventType"))
  (is-true  (g-type-is-classed "GdkEventType"))
  (is-false (g-type-is-instantiatable "GdkEventType"))
  (is-true  (g-type-is-derivable "GdkEventType"))
  (is-false (g-type-is-deep-derivable "GdkEventType"))
  (is-false (g-type-is-interface "GdkEventType"))

  ;; Check the registered name
;  (is (eq 'gtk-frame
;          (registered-object-type-by-name "GdkEventType")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GdkEventType"))))
    (is (equal (gtype "GdkEventType") (g-type-from-class class)))
    (is (equal (gtype "GdkEventType") (g-object-class-type class)))
    (is (equal "GdkEventType" (g-object-class-name class)))
    (is (equal (gtype "GdkEventType")
               (g-type-from-class  (g-type-class-peek "GdkEventType"))))
    (is (equal (gtype "GdkEventType")
               (g-type-from-class  (g-type-class-peek-static "GdkEventType"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  ;; no Lisp class implementation for an Enum

  ;; Check some more GType information
  (is (equal (gtype "GEnum") (g-type-parent "GdkEventType")))
  (is (= 2 (g-type-depth "GdkEventType")))
  (is (equal (gtype "GdkEventType")
             (g-type-next-base "GdkEventType" "GEnum")))
  (is-false (g-type-is-a "GdkEventType" "GtkWidget"))
  (is-true  (g-type-is-a "GdkEventType" "GEnum"))
  (is-false (g-type-is-a "GdkEventType" "gboolean"))
  (is-false (g-type-is-a "GdkEventType" "GtkWindow"))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GdkEventType"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GdkEventType"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GdkEventType" query)
    (is (equal (gtype "GdkEventType")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GdkEventType"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 32  (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  0  (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  ;; no class properties for an Enum

  ;; Get the names of the style properties.
  ;; no style properties for an Enum

  ;; Get the names to the child properties
  ;; no child properties for an Enum

  ;; Get the class definition
  (is (equal '(DEFINE-G-ENUM "GdkEventType"
    GDK-EVENT-TYPE
    (:EXPORT T :TYPE-INITIALIZER "gdk_event_type_get_type")
  (:NOTHING -1)
  (:DELETE 0)
  (:DESTROY 1)
  (:EXPOSE 2)
  (:MOTION-NOTIFY 3)
  (:BUTTON-PRESS 4)
  (:2BUTTON-PRESS 5)
  (:DOUBLE-BUTTON-PRESS 5)
  (:3BUTTON-PRESS 6)
  (:TRIPLE-BUTTON-PRESS 6)
  (:BUTTON-RELEASE 7)
  (:KEY-PRESS 8)
  (:KEY-RELEASE 9)
  (:ENTER-NOTIFY 10)
  (:LEAVE-NOTIFY 11)
  (:FOCUS-CHANGE 12)
  (:CONFIGURE 13)
  (:MAP 14)
  (:UNMAP 15)
  (:PROPERTY-NOTIFY 16)
  (:SELECTION-CLEAR 17)
  (:SELECTION-REQUEST 18)
  (:SELECTION-NOTIFY 19)
  (:PROXIMITY-IN 20)
  (:PROXIMITY-OUT 21)
  (:DRAG-ENTER 22)
  (:DRAG-LEAVE 23)
  (:DRAG-MOTION 24)
  (:DRAG-STATUS 25)
  (:DROP-START 26)
  (:DROP-FINISHED 27)
  (:CLIENT-EVENT 28)
  (:VISIBILITY-NOTIFY 29)
  (:SCROLL 31)
  (:WINDOW-STATE 32)
  (:SETTING 33)
  (:OWNER-CHANGE 34)
  (:GRAB-BROKEN 35)
  (:DAMAGE 36)
  (:TOUCH-BEGIN 37)
  (:TOUCH-UPDATE 38)
  (:TOUCH-END 39)
  (:TOUCH-CANCEL 40)
  (:TOUCHPAD-SWIPE 41)
  (:TOUCHPAD-PINCH 42)
  (:PAD-BUTTON-PRESS 43)
  (:PAD-BUTTON-RELEASE 44)
  (:PAD-RING 45)
  (:PAD-STRIP 46)
  (:PAD-GROUP-MODE 47)
  (:EVENT-LAST 48))
             (get-g-type-definition "GdkEventType")))
)

(test gdk-event-type-value
  (is (= -1 (foreign-enum-value 'gdk-event-type :nothing)))
  (is (=  5 (foreign-enum-value 'gdk-event-type :2button-press)))
  (is (=  5 (foreign-enum-value 'gdk-event-type :double-button-press)))
  (is (=  6 (foreign-enum-value 'gdk-event-type :3button-press)))
  (is (=  6 (foreign-enum-value 'gdk-event-type :triple-button-press))))

(test gdk-event-type-keyword
  (is (eq :nothing (foreign-enum-keyword 'gdk-event-type -1)))
  (is (eq :double-button-press (foreign-enum-keyword 'gdk-event-type 5)))
  (is (eq :triple-button-press (foreign-enum-keyword 'gdk-event-type 6))))


;;;     GdkEventMask --> gdk.event-structures.lisp

;;;   GDK_CURRENT_TIME

(test +gdk-current-time+
  (is (= 0 +gdk-current-time+)))

;;;     GDK_PRIORITY_EVENTS
;;;     GDK_PRIORITY_REDRAW
;;;     GDK_EVENT_PROPAGATE
;;;     GDK_EVENT_STOP
;;;     GDK_BUTTON_PRIMARY
;;;     GDK_BUTTON_MIDDLE
;;;     GDK_BUTTON_SECONDARY

;;;   gdk_events_pending

(test gdk-events-pending
  (let ((event (gdk-event-new :key-press)))
    ;; We have to set the slot string to a non-nil value
    (setf (gdk-event-key-string event) "")
    (gdk-event-put event)
    (is-true (gdk-events-pending))
    (gdk-event-get)))

;;;   gdk_event_peek

(test gdk-event-peek
  (let ((event (gdk-event-new :key-press)))
    ;; We have to set the slot string to a non-nil value
    (setf (gdk-event-key-string event) "")
    (gdk-event-put event)
    (is (eq 'gdk-event-key (type-of (gdk-event-peek))))))

;;;   gdk_event_get
;;;   gdk_event_put

(test gdk-event-get
  (let ((event (gdk-event-new :key-press)))
    ;; We have to set the slot string to a non-nil value
    (setf (gdk-event-key-string event) "")
    (gdk-event-put event)
    (is (eq 'gdk-event-key (type-of (gdk-event-get))))))

;;;   gdk_event_new

(test gdk-event-new
  (is (eq 'gdk-event-key (type-of (gdk-event-new :key-press))))
  (is (eq 'gdk-event-button (type-of (gdk-event-new :button-press)))))

;;;   gdk_event_copy

(test gdk-event-copy
  (let ((event (gdk-event-new :key-press)))
    (is (eq 'gdk-event-key (type-of (gdk-event-copy event))))))

;;;   gdk_event_free
;;;   gdk_event_get_axis

;;;     gdk_event_get_button

(test gdk-event-get-button
  (let ((event (gdk-event-new :button-press)))
    (is (= 0 (gdk-event-get-button event)))
    (setf (gdk-event-button-button event) 1)
    (is (= 1 (gdk-event-get-button event)))))

;;;     gdk_event_get_click_count
;;;     gdk_event_get_coords
;;;     gdk_event_get_keycode
;;;     gdk_event_get_keyval
;;;     gdk_event_get_root_coords
;;;     gdk_event_get_scroll_direction
;;;     gdk_event_get_scroll_deltas
;;;     gdk_event_get_state
;;;     gdk_event_get_time
;;;
;;;     GdkEventSequence  --> gdk.event-structures.lisp
;;;
;;;     gdk_event_get_event_sequence
;;;     gdk_event_request_motions
;;;     gdk_events_get_angle
;;;     gdk_events_get_center
;;;     gdk_events_get_distance
;;;     gdk_event_triggers_context_menu
;;;
;;;     gdk_event_handler_set
;;;
;;;     gdk_get_show_events
;;;     gdk_set_show_events

;;;     gdk_event_set_screen
;;;     gdk_event_get_screen

(test gdk-event-get-screen
  (let* ((event (gdk-event-new :button-press))
         (display (gdk-display-get-default))
         (screen (gdk-display-get-default-screen display)))
    (is-false (gdk-event-get-screen event))
    (gdk-event-set-screen event screen)
; This does not work as expected
;    (is (eq 'gdk-screen (type-of (gdk-event-get-screen event))))
))

;;;     gdk_event_get_device
;;;     gdk_event_set_device

(test gdk-event-get-device
  (let* ((event (gdk-event-new :button-press))
         (display (gdk-display-get-default))
         (device-manager (gdk-display-get-device-manager display))
         (device (gdk-device-manager-get-client-pointer device-manager)))
    (is-false (gdk-event-get-device event))
    (gdk-event-set-device event device)
    #-windows
    (is (eq 'GDK-X11-DEVICE-XI2 (type-of (gdk-event-get-device event))))
    #+windows
    (is (eq 'GDK-DEVICE (type-of (gdk-event-get-device event))))))

;;;     gdk_event_get_source_device
;;;     gdk_event_set_source_device

(test gdk-event-get-source-device
  (let* ((event (gdk-event-new :motion-notify))
         (display (gdk-display-get-default))
         (device-manager (gdk-display-get-device-manager display))
         (device (gdk-device-manager-get-client-pointer device-manager)))
    (is-false (gdk-event-get-source-device event))
    (gdk-event-set-source-device event device)
; This does not work as expected.
;    (is (eq 'gdk-device (type-of (gdk-event-get-source-device event))))
))

;;;     gdk_setting_get



