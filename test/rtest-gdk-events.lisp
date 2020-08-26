(def-suite gdk-events :in gdk-suite)
(in-suite gdk-events)

(defvar *verbose-gdk-events* nil)

;;;   GDK_CURRENT_TIME

(test +gdk-current-time+
  (is (= 0 +gdk-current-time+)))

;;;     GDK_PRIORITY_EVENTS

;;;     GDK_PRIORITY_REDRAW

(test +gdk-priority-redraw+
  (is (= 120 +gdk-priority-redraw+)))

;;;     GDK_EVENT_PROPAGATE

(test +gdk-event-propagate+
  (is-false +gdk-event-propagate+))

;;;     GDK_EVENT_STOP

(test +gdk-event-stop+
  (is-true +gdk-event-stop+))

;;;     GDK_BUTTON_PRIMARY
;;;     GDK_BUTTON_MIDDLE
;;;     GDK_BUTTON_SECONDARY

;;; --- Functions --------------------------------------------------------------

;;;     gdk_events_pending

(defun my-event-handler (event)
  (when *verbose-gdk-events*
    (format t "~&in MY-EVENT-HANDLER~%")
    (format t "~a~%" event))
  ;; Pass the event to GTK event handler
  (gtk-main-do-event event))

;; Install an event handler to inspect the main loop
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gdk-event-handler-set #'my-event-handler))

(defun clear-event-loop ()
  (let ((*verbose-gdk-events* t))
    (loop while (gdk-events-pending)
          do (when *verbose-gdk-events*
               (format t "~&in CLEAR-EVENT-LOOP~%"))
               (gtk-main-iteration-do nil))))

(defun events-pending-callback ()
  (let ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default)))))
    (is (= 1 (gtk-main-level)))
    ;; Put two events on the main loop, the following code find these events
    (gdk-event-put (gdk-event-new :button-press :device device))
    (gdk-event-put (gdk-event-new :key-press))
    ;; Look for pending events
    (loop while (gdk-events-pending)
          do (format t "~&Event is pending.~%")
             (gtk-main-iteration-do nil))
    ;; Quit the callback
    (gtk-main-quit)
    +g-source-remove+))

(test gdk-events-pending
  (let ((*verbose-gdk-events* t))
    (is (= 0 (gtk-main-level)))
    (g-timeout-add 100 #'events-pending-callback)
    (gtk-main)
    (is (= 0 (gtk-main-level)))))

;;;     gdk_event_peek

(test gdk-event-peek
  (let ((*verbose-gdk-events* t)
        (event (gdk-event-new :key-press)))
    (is-false (gdk-event-put event))
    (is (eq 'gdk-event-key (type-of (gdk-event-peek))))
    (clear-event-loop)
    (is-false (gdk-event-peek))))

;;;     gdk_event_get
;;;     gdk_event_put

(test gdk-event-get
  (let ((*verbose-gdk-events* t)
        (event (gdk-event-new :key-press)))
    (is-false (gdk-event-put event))
    (is (eq 'gdk-event-key (type-of (gdk-event-get))))
    (clear-event-loop)))

;;;   gdk_event_new

(test gdk-event-new
  (is (eq 'gdk-event (type-of (gdk-event-new :nothing))))
  (is (eq 'gdk-event (type-of (gdk-event-new :delete))))
  (is (eq 'gdk-event (type-of (gdk-event-new :destroy))))
  (is (eq 'gdk-event-expose (type-of (gdk-event-new :expose))))
  (is (eq 'gdk-event-motion (type-of (gdk-event-new :motion-notify))))
  (is (eq 'gdk-event-button (type-of (gdk-event-new :button-press))))
  (is (eq 'gdk-event-button (type-of (gdk-event-new :2button-press))))
  (is (eq 'gdk-event-button (type-of (gdk-event-new :double-button-press))))
  (is (eq 'gdk-event-button (type-of (gdk-event-new :3button-press))))
  (is (eq 'gdk-event-button (type-of (gdk-event-new :triple-button-press))))
  (is (eq 'gdk-event-button (type-of (gdk-event-new :button-release))))
  (is (eq 'gdk-event-key (type-of (gdk-event-new :key-press))))
  (is (eq 'gdk-event-key (type-of (gdk-event-new :key-release))))
  (is (eq 'gdk-event-crossing (type-of (gdk-event-new :enter-notify))))
  (is (eq 'gdk-event-crossing (type-of (gdk-event-new :leave-notify))))
  (is (eq 'gdk-event-focus (type-of (gdk-event-new :focus-change))))
  (is (eq 'gdk-event-configure (type-of (gdk-event-new :configure))))
  (is (eq 'gdk-event (type-of (gdk-event-new :map))))
  (is (eq 'gdk-event (type-of (gdk-event-new :unmap))))
  (is (eq 'gdk-event-property (type-of (gdk-event-new :property-notify))))
  (is (eq 'gdk-event-selection (type-of (gdk-event-new :selection-clear))))
  (is (eq 'gdk-event-selection (type-of (gdk-event-new :selection-request))))
  (is (eq 'gdk-event-selection (type-of (gdk-event-new :selection-notify))))
  (is (eq 'gdk-event-proximity (type-of (gdk-event-new :proximity-in))))
  (is (eq 'gdk-event-proximity (type-of (gdk-event-new :proximity-out))))
  (is (eq 'gdk-event-dnd (type-of (gdk-event-new :drag-enter))))
  (is (eq 'gdk-event-dnd (type-of (gdk-event-new :drag-leave))))
  (is (eq 'gdk-event-dnd (type-of (gdk-event-new :drag-motion))))
  (is (eq 'gdk-event-dnd (type-of (gdk-event-new :drag-status))))
  (is (eq 'gdk-event-dnd (type-of (gdk-event-new :drop-start))))
  (is (eq 'gdk-event-dnd (type-of (gdk-event-new :drop-finished))))
  (is (eq 'gdk-event (type-of (gdk-event-new :client-event))))
  (is (eq 'gdk-event-visibility (type-of (gdk-event-new :visibility-notify))))
  (is (eq 'gdk-event (type-of (gdk-event-new :not-used))))
  (is (eq 'gdk-event-scroll (type-of (gdk-event-new :scroll))))
  (is (eq 'gdk-event-window-state (type-of (gdk-event-new :window-state))))
  (is (eq 'gdk-event-setting (type-of (gdk-event-new :setting))))
  (is (eq 'gdk-event-owner-change (type-of (gdk-event-new :owner-change))))
  (is (eq 'gdk-event-grab-broken (type-of (gdk-event-new :grab-broken))))
  (is (eq 'gdk-event (type-of (gdk-event-new :damage))))
  (is (eq 'gdk-event-touch (type-of (gdk-event-new :touch-begin))))
  (is (eq 'gdk-event-touch (type-of (gdk-event-new :touch-update))))
  (is (eq 'gdk-event-touch (type-of (gdk-event-new :touch-end))))
  (is (eq 'gdk-event-touch (type-of (gdk-event-new :touch-cancel))))
  (is (eq 'gdk-event-touchpad-swipe (type-of (gdk-event-new :touchpad-swipe))))
  (is (eq 'gdk-event-touchpad-pinch (type-of (gdk-event-new :touchpad-pinch))))
  (is (eq 'gdk-event-pad-button (type-of (gdk-event-new :pad-button-press))))
  (is (eq 'gdk-event-pad-button (type-of (gdk-event-new :pad-button-release))))
  (is (eq 'gdk-event-pad-axis (type-of (gdk-event-new :pad-ring))))
  (is (eq 'gdk-event-pad-axis (type-of (gdk-event-new :pad-strip))))
  (is (eq 'gdk-event-pad-group-mode (type-of (gdk-event-new :pad-group-mode)))))

;;;     gdk_event_copy

(test gdk-event-copy
  (let ((event (gdk-event-new :key-press)))
    (is (eq 'gdk-event-key (type-of (gdk-event-copy event))))))

;;;     gdk_event_free

;;;     gdk_event_get_axis

(test gdk-event-axis
  (let ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default)))))
    (is (= 0.0d0 (gdk-event-axis (gdk-event-new :button-press :device device) :ignore)))
    (is (= 0.0d0 (gdk-event-axis (gdk-event-new :button-press :device device) :x)))
    (is (= 0.0d0 (gdk-event-axis (gdk-event-new :button-press :device device) :y)))
    (is-false (gdk-event-axis (gdk-event-new :button-press :device device) :pressure))
    (is-false (gdk-event-axis (gdk-event-new :button-press :device device) :xtilt))
    (is-false (gdk-event-axis (gdk-event-new :button-press :device device) :ytilt))
    (is-false (gdk-event-axis (gdk-event-new :button-press :device device) :wheel))
    (is-false (gdk-event-axis (gdk-event-new :button-press :device device) :distance))
    (is-false (gdk-event-axis (gdk-event-new :button-press :device device) :rotation))
    (is-false (gdk-event-axis (gdk-event-new :button-press :device device) :slider))))

;;;     gdk_event_get_button

(test gdk-event-button
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :button-press :device device)))
    (is (= 0 (gdk-event-button event)))
    (is (= 1 (setf (gdk-event-button-button event) 1)))
    (is (= 1 (gdk-event-button event)))))

;;;     gdk_event_get_click_count

(test gdk-event-click-count
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :button-press :device device)))
    (is (= 1 (gdk-event-click-count event)))))

;;;     gdk_event_get_coords

(test gdk-event-coords
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :button-press :device device)))

    (is (equal '(0.0d0 0.0d0) (multiple-value-list (gdk-event-coords event))))

))

;;;     gdk_event_get_keycode
;;;     gdk_event_get_keyval

(test gdk-event-key
  (let ((event (gdk-event-new :key-press :keyval 12 :hardware-keycode 13)))
    (is (= 12 (gdk-event-keyval event)))
    (is (= 13 (gdk-event-keycode event)))))

;;;     gdk_event_get_root_coords

(test gdk-event-root-coords
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :button-press :device device)))

    (is (equal '(0.0d0 0.0d0) (multiple-value-list (gdk-event-root-coords event))))

))

;;;     gdk_event_get_scroll_direction

(test gdk-event-get-scroll-direction
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :scroll :device device :direction :smooth)))
    (is (eq :smooth (gdk-event-scroll-direction event)))
    ;; FIXME: Should be :smooth
    (is-false (gdk-event-get-scroll-direction event))
))

;;;     gdk_event_get_scroll_deltas

(test gdk-event-scroll-deltas
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :scroll :device device :direction :smooth)))
    (is (eq :scroll (gdk-event-type event)))
    (is (eq :smooth (gdk-event-scroll-direction event)))
    (is (= 0.0d0 (gdk-event-scroll-delta-x event)))
    (is (= 0.0d0 (gdk-event-scroll-delta-y event)))
    (is (equal '(0.0d0 0.0d0)
                (multiple-value-list (gdk-event-scroll-deltas event))))))

;;;     gdk_event_is_scroll_stop_event

(test gdk-event-is-scroll-stop-event
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :scroll :device device :direction :smooth)))

    (is-false (gdk-event-is-scroll-stop-event event))
))

;;;     gdk_event_get_state

(test gdk-event-state
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :scroll :device device :state '(:shift-mask))))

  (is (= 0 (gdk-event-state nil)))
  (is (equal '(:shift-mask) (gdk-event-state event)))

))

;;;     gdk_event_get_time

(test gdk-event-state
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :scroll :device device :state '(:shift-mask))))

  (is (= 0 (gdk-event-time event)))

))


;;;     gdk_event_get_window
;;;     gdk_event_get_event_type

;;;     gdk_event_get_event_sequence

(test gdk-event-event-sequence
  (let* ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default))))
         (event (gdk-event-new :touch-begin :device device)))

  (is (eq 'gdk-event-sequence (type-of (gdk-event-event-sequence event))))

))

;;;     gdk_event_request_motions



;;;     gdk_events_get_angle
;;;     gdk_events_get_center
;;;     gdk_events_get_distance
;;;     gdk_event_triggers_context_menu
;;;     gdk_event_get_seat
;;;     gdk_event_get_scancode
;;;     gdk_event_get_pointer_emulated

;;;     gdk_event_handler_set

;;;     gdk_get_show_events
;;;     gdk_set_show_events

;;;     gdk_event_set_screen
;;;     gdk_event_get_screen

#+nil
(test gdk-event-get-screen
  (let* ((event (gdk-event-new :button-press))
         (display (gdk-display-default))
         (screen (gdk-display-default-screen display)))
    (is-false (gdk-event-get-screen event))
    (gdk-event-set-screen event screen)
; This does not work as expected
;    (is (eq 'gdk-screen (type-of (gdk-event-get-screen event))))
))

;;;     gdk_event_get_device
;;;     gdk_event_set_device

#+nil
(test gdk-event-get-device
  (let* ((event (gdk-event-new :button-press))
         (display (gdk-display-default))
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

#+nil
(test gdk-event-get-source-device
  (let* ((event (gdk-event-new :motion-notify))
         (display (gdk-display-default))
         (device-manager (gdk-display-get-device-manager display))
         (device (gdk-device-manager-get-client-pointer device-manager)))
    (is-false (gdk-event-get-source-device event))
    (gdk-event-set-source-device event device)
; This does not work as expected.
;    (is (eq 'gdk-device (type-of (gdk-event-get-source-device event))))
))

;;;     gdk_event_get_device_tool
;;;     gdk_event_set_device_tool
;;;     gdk_setting_get

