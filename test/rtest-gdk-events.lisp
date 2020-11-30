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
          do (progn
               (when *verbose-gdk-events*
                 (format t "~&in CLEAR-EVENT-LOOP~%"))
                 (gtk-main-iteration-do nil)))))

(defun events-pending-callback ()
  (let ((device (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default)))))
    (is (= 1 (gtk-main-level)))
    ;; Put two events on the main loop, the following code find these events
    (gdk-event-put (gdk-event-new :button-press :device device))
    (gdk-event-put (gdk-event-new :key-press))
    ;; Look for pending events
    (loop while (gdk-events-pending)
          do (progn
               (format t "~&Event is pending.~%")
               (gtk-main-iteration-do nil)))
    ;; Quit the callback
    (gtk-main-quit)
    +g-source-remove+))

;; TODO: The following functions can cause an infinite loop, improve the code

#+nil
(test gdk-events-pending
  (let ((*verbose-gdk-events* t))
    (is (= 0 (gtk-main-level)))
    (g-timeout-add 100 #'events-pending-callback)
    (gtk-main)
    (is (= 0 (gtk-main-level)))))

;;;     gdk_event_peek

#+nil
(test gdk-event-peek
  (let ((*verbose-gdk-events* t)
        (event (gdk-event-new :key-press)))
    (is-false (gdk-event-put event))
    (is (typep (gdk-event-peek) 'gdk-event-key))
    (clear-event-loop)
    (is-false (gdk-event-peek))))

;;;     gdk_event_get
;;;     gdk_event_put

#+nil
(test gdk-event-get
  (let ((*verbose-gdk-events* t)
        (event (gdk-event-new :key-press)))
    (is-false (gdk-event-put event))
    (is (typep (gdk-event-get) 'gdk-event-key))
    (clear-event-loop)))

;;;   gdk_event_new

(test gdk-event-new
  (is (typep (gdk-event-new :nothing) 'gdk-event))
  (is (typep (gdk-event-new :delete) 'gdk-event))
  (is (typep (gdk-event-new :destroy) 'gdk-event))
  (is (typep (gdk-event-new :expose) 'gdk-event-expose))
  (is (typep (gdk-event-new :motion-notify) 'gdk-event-motion))
  (is (typep (gdk-event-new :button-press) 'gdk-event-button))
  (is (typep (gdk-event-new :2button-press) 'gdk-event-button))
  (is (typep (gdk-event-new :double-button-press) 'gdk-event-button))
  (is (typep (gdk-event-new :3button-press) 'gdk-event-button))
  (is (typep (gdk-event-new :triple-button-press) 'gdk-event-button))
  (is (typep (gdk-event-new :button-release) 'gdk-event-button))
  (is (typep (gdk-event-new :key-press) 'gdk-event-key))
  (is (typep (gdk-event-new :key-release) 'gdk-event-key))
  (is (typep (gdk-event-new :enter-notify) 'gdk-event-crossing))
  (is (typep (gdk-event-new :leave-notify) 'gdk-event-crossing))
  (is (typep (gdk-event-new :focus-change) 'gdk-event-focus))
  (is (typep (gdk-event-new :configure) 'gdk-event-configure))
  (is (typep (gdk-event-new :map) 'gdk-event))
  (is (typep (gdk-event-new :unmap) 'gdk-event))
  (is (typep (gdk-event-new :property-notify) 'gdk-event-property))
  (is (typep (gdk-event-new :selection-clear) 'gdk-event-selection))
  (is (typep (gdk-event-new :selection-request) 'gdk-event-selection))
  (is (typep (gdk-event-new :selection-notify) 'gdk-event-selection))
  (is (typep (gdk-event-new :proximity-in) 'gdk-event-proximity))
  (is (typep (gdk-event-new :proximity-out) 'gdk-event-proximity))
  (is (typep (gdk-event-new :drag-enter) 'gdk-event-dnd))
  (is (typep (gdk-event-new :drag-leave) 'gdk-event-dnd))
  (is (typep (gdk-event-new :drag-motion) 'gdk-event-dnd))
  (is (typep (gdk-event-new :drag-status) 'gdk-event-dnd))
  (is (typep (gdk-event-new :drop-start) 'gdk-event-dnd))
  (is (typep (gdk-event-new :drop-finished) 'gdk-event-dnd))
  (is (typep (gdk-event-new :client-event) 'gdk-event))
  (is (typep (gdk-event-new :visibility-notify) 'gdk-event-visibility))
  (is (typep (gdk-event-new :not-used) 'gdk-event))
  (is (typep (gdk-event-new :scroll) 'gdk-event-scroll))
  (is (typep (gdk-event-new :window-state) 'gdk-event-window-state))
  (is (typep (gdk-event-new :setting) 'gdk-event-setting))
  (is (typep (gdk-event-new :owner-change) 'gdk-event-owner-change))
  (is (typep (gdk-event-new :grab-broken) 'gdk-event-grab-broken))
  (is (typep (gdk-event-new :damage) 'gdk-event))
  (is (typep (gdk-event-new :touch-begin) 'gdk-event-touch))
  (is (typep (gdk-event-new :touch-update) 'gdk-event-touch))
  (is (typep (gdk-event-new :touch-end) 'gdk-event-touch))
  (is (typep (gdk-event-new :touch-cancel) 'gdk-event-touch))
  (is (typep (gdk-event-new :touchpad-swipe) 'gdk-event-touchpad-swipe))
  (is (typep (gdk-event-new :touchpad-pinch) 'gdk-event-touchpad-pinch))
  (is (typep (gdk-event-new :pad-button-press) 'gdk-event-pad-button))
  (is (typep (gdk-event-new :pad-button-release) 'gdk-event-pad-button))
  (is (typep (gdk-event-new :pad-ring) 'gdk-event-pad-axis))
  (is (typep (gdk-event-new :pad-strip) 'gdk-event-pad-axis))
  (is (typep (gdk-event-new :pad-group-mode) 'gdk-event-pad-group-mode)))

;;;     gdk_event_copy

(test gdk-event-copy
  (let ((event (gdk-event-new :key-press)))
    (is (typep (gdk-event-copy event) 'gdk-event-key))))

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

  (is (typep (gdk-event-event-sequence event) 'gdk-event-sequence))

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
         (device-manager (gdk-display-device-manager display))
         (device (gdk-device-manager-get-client-pointer device-manager)))
    (is-false (gdk-event-get-device event))
    (gdk-event-set-device event device)
    (is (typep (gdk-event-get-device event) 'GDK-DEVICE))))

;;;     gdk_event_get_source_device
;;;     gdk_event_set_source_device

#+nil
(test gdk-event-get-source-device
  (let* ((event (gdk-event-new :motion-notify))
         (display (gdk-display-default))
         (device-manager (gdk-display-device-manager display))
         (device (gdk-device-manager-get-client-pointer device-manager)))
    (is-false (gdk-event-get-source-device event))
    (gdk-event-set-source-device event device)
; This does not work as expected.
;    (is (eq 'gdk-device (type-of (gdk-event-get-source-device event))))
))

;;;     gdk_event_get_device_tool
;;;     gdk_event_set_device_tool
;;;     gdk_setting_get

;;; 2020-11-28
