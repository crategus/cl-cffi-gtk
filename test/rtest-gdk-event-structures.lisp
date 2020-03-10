(def-suite gdk-event-structures :in gdk-suite)
(in-suite gdk-event-structures)

;;;     GdkScrollDirection
;;;     GdkVisibilityState
;;;     GdkCrossingMode
;;;     GdkNotifyType
;;;     GdkPropertyState
;;;     GdkWindowState
;;;     GdkSettingAction
;;;     GdkOwnerChange

;;;     GdkEventType        <-- gdk.events.lisp
;;;     GdkModifierType     <-- gdk.window.lisp
;;;     GdkEventMask        <-- gdk.events.lisp
;;;     GdkEventSequence    <-- gdk-events.lisp

;;;     GdkEvent
;;;     GdkEventAny


;;;  (:nothing -1)

;;  (type gdk-event-type)
;;  (window (g-object gdk-window))
;;  (send-event (:boolean :int8))

(test gdk-event-nothing
   (let ((event (gdk-event-new :nothing)))
      (is (eq 'gdk-event (type-of event)))
      (is (eq :nothing (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))))

;;;  (:delete 0)

(test gdk-event-delete
   (let ((event (gdk-event-new :delete)))
      (is (eq 'gdk-event (type-of event)))

      (is (eq :delete (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))))

;;;  (:destroy 1)

(test gdk-event-destroy
   (let ((event (gdk-event-new :destroy)))
      (is (eq 'gdk-event (type-of event)))
      (is (eq :destroy (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))))

;;;  (:expose 2)

;;            ;; GdkEventExpose
;;            ((:expose) gdk-event-expose
;;             (area gdk-rectangle :inline t)
;;             (region (:pointer (:struct cairo-region-t)))
;;             (count :int))

(test gdk-event-expose
   (let ((event (gdk-event-new :expose)))
      (is (eq 'gdk-event-expose (type-of event)))
      (is (eq :expose (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))
      (is (eq 'gdk-rectangle (type-of (gdk-event-expose-area event))))
      (is-true (gdk-rectangle-equal (make-gdk-rectangle)
                                    (gdk-event-expose-area event)))
      (is (eq 'sb-sys:system-area-pointer (type-of (gdk-event-expose-region event))))
      (is-true (null-pointer-p (gdk-event-expose-region event)))
      (is (eq 'bit (type-of (gdk-event-expose-count event))))
      (is (= 0 (gdk-event-expose-count event)))))

;;;  (:motion-notify 3)

;;            ;; GdkEventMotion
;;            ((:motion-notify) gdk-event-motion
;;             (time :uint32)
;;             (x :double)
;;             (y :double)
;;             (axes (fixed-array :double 2))
;;             (state gdk-modifier-type)
;;             (is-hint :int16)
;;             (device (g-object gdk-device))
;;             (x-root :double)
;;             (y-root :double))

(test gdk-event-motion-notify
   (let ((event (gdk-event-new :motion-notify)))
      (is (eq 'gdk-event-motion (type-of event)))
      (is (eq :motion-notify (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))
      (is (eq 'bit (type-of (gdk-event-motion-time event))))
      (is (= 0 (gdk-event-motion-time event)))
      (is (eq 'double-float (type-of (gdk-event-motion-x event))))
      (is (= 0.0d0 (gdk-event-motion-x event)))
      (is (eq 'double-float (type-of (gdk-event-motion-y event))))
      (is (= 0.0d0 (gdk-event-motion-y event)))
      (is-true (eq 'NULL (type-of (gdk-event-motion-axes event))))
      (is-false (gdk-event-motion-axes event))
      (is (eq 'NULL (type-of (gdk-event-motion-state event))))
      (is-false (gdk-event-motion-state event))
      (is (eq 'bit (type-of (gdk-event-motion-is-hint event))))
      (is (= 0 (gdk-event-motion-is-hint event)))
      (is (eq 'NULL (type-of (gdk-event-motion-device event))))
      (is-false (gdk-event-motion-device event))
      (is (eq 'double-float (type-of (gdk-event-motion-x-root event))))
      (is (= 0.0d0 (gdk-event-motion-x-root event)))
      (is (eq 'double-float (type-of (gdk-event-motion-y-root event))))
      (is (= 0.0d0 (gdk-event-motion-y-root event)))))

;;;  (:button-press 4)

;;            ;; GdkEventButton
;;            ((:button-press
;;              :2button-press
;;              :double-button-press
;;              :3button-press
;;              :triple-button-press
;;              :button-release) gdk-event-button
;;             (time :uint32)
;;             (x :double)
;;             (y :double)
;;             (axes (fixed-array :double 2))
;;             (state :uint)
;;             (button :uint)
;;             (device (g-object gdk-device))
;;             (x-root :double)
;;             (y-root :double))

(test gdk-event-button-press
   (let ((event (gdk-event-new :button-press)))
      (is (eq 'gdk-event-button (type-of event)))
      (is (eq :button-press (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))
      (is (eq 'bit (type-of (gdk-event-button-time event))))
      (is (= 0 (gdk-event-button-time event)))
      (is (eq 'double-float (type-of (gdk-event-button-x event))))
      (is (= 0.0d0 (gdk-event-button-x event)))
      (is (eq 'double-float (type-of (gdk-event-button-y event))))
      (is (= 0 (gdk-event-button-y event)))
      (is (eq 'NULL (type-of (gdk-event-button-axes event))))
      (is-false (gdk-event-button-axes event))
      (is (eq 'NULL (type-of (gdk-event-button-state event))))
      (is-false (gdk-event-button-state event))
      (is (eq 'bit (type-of (gdk-event-button-button event))))
      (is (= 0 (gdk-event-button-button event)))
      (is (eq 'NULL (type-of (gdk-event-button-device event))))
      (is-false (gdk-event-button-device event))
      (is (eq 'double-float (type-of (gdk-event-button-x-root event))))
      (is (= 0.0d0 (gdk-event-button-x-root event)))
      (is (eq 'double-float (type-of (gdk-event-button-y-root event))))
      (is (= 0.0d0 (gdk-event-button-y-root event)))))

;;;  (:2button-press 5)
;;;  (:double-button-press 5) ; Alias for :2button-press

(test gdk-event-double-button-press
   (let ((event (gdk-event-new :double-button-press)))
      (is (eq 'gdk-event-button (type-of event)))
      (is (eq :double-button-press (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))
      (is (eq 'bit (type-of (gdk-event-button-time event))))
      (is (= 0 (gdk-event-button-time event)))
      (is (eq 'double-float (type-of (gdk-event-button-x event))))
      (is (= 0.0d0 (gdk-event-button-x event)))
      (is (eq 'double-float (type-of (gdk-event-button-y event))))
      (is (= 0 (gdk-event-button-y event)))
      (is (eq 'NULL (type-of (gdk-event-button-axes event))))
      (is-false (gdk-event-button-axes event))
      (is (eq 'NULL (type-of (gdk-event-button-state event))))
      (is-false (gdk-event-button-state event))
      (is (eq 'bit (type-of (gdk-event-button-button event))))
      (is (= 0 (gdk-event-button-button event)))
      (is (eq 'NULL (type-of (gdk-event-button-device event))))
      (is-false (gdk-event-button-device event))
      (is (eq 'double-float (type-of (gdk-event-button-x-root event))))
      (is (= 0.0d0 (gdk-event-button-x-root event)))
      (is (eq 'double-float (type-of (gdk-event-button-y-root event))))
      (is (= 0.0d0 (gdk-event-button-y-root event)))))

;;;  (:3button-press 6)
;;;  (:triple-button-press 6) ; Alias for :3button-press

(test gdk-event-triple-button-press
   (let ((event (gdk-event-new :triple-button-press)))
      (is (eq 'gdk-event-button (type-of event)))
      (is (eq :triple-button-press (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))
      (is (eq 'bit (type-of (gdk-event-button-time event))))
      (is (= 0 (gdk-event-button-time event)))
      (is (eq 'double-float (type-of (gdk-event-button-x event))))
      (is (= 0.0d0 (gdk-event-button-x event)))
      (is (eq 'double-float (type-of (gdk-event-button-y event))))
      (is (= 0 (gdk-event-button-y event)))
      (is (eq 'NULL (type-of (gdk-event-button-axes event))))
      (is-false (gdk-event-button-axes event))
      (is (eq 'NULL (type-of (gdk-event-button-state event))))
      (is-false (gdk-event-button-state event))
      (is (eq 'bit (type-of (gdk-event-button-button event))))
      (is (= 0 (gdk-event-button-button event)))
      (is (eq 'NULL (type-of (gdk-event-button-device event))))
      (is-false (gdk-event-button-device event))
      (is (eq 'double-float (type-of (gdk-event-button-x-root event))))
      (is (= 0.0d0 (gdk-event-button-x-root event)))
      (is (eq 'double-float (type-of (gdk-event-button-y-root event))))
      (is (= 0.0d0 (gdk-event-button-y-root event)))))

;;;  (:button-release 7)

(test gdk-event-button-release
   (let ((event (gdk-event-new :button-release)))
      (is (eq 'gdk-event-button (type-of event)))
      (is (eq :button-release (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))
      (is (eq 'bit (type-of (gdk-event-button-time event))))
      (is (= 0 (gdk-event-button-time event)))
      (is (eq 'double-float (type-of (gdk-event-button-x event))))
      (is (= 0.0d0 (gdk-event-button-x event)))
      (is (eq 'double-float (type-of (gdk-event-button-y event))))
      (is (= 0 (gdk-event-button-y event)))
      (is (eq 'NULL (type-of (gdk-event-button-axes event))))
      (is-false (gdk-event-button-axes event))
      (is (eq 'NULL (type-of (gdk-event-button-state event))))
      (is-false (gdk-event-button-state event))
      (is (eq 'bit (type-of (gdk-event-button-button event))))
      (is (= 0 (gdk-event-button-button event)))
      (is (eq 'NULL (type-of (gdk-event-button-device event))))
      (is-false (gdk-event-button-device event))
      (is (eq 'double-float (type-of (gdk-event-button-x-root event))))
      (is (= 0.0d0 (gdk-event-button-x-root event)))
      (is (eq 'double-float (type-of (gdk-event-button-y-root event))))
      (is (= 0.0d0 (gdk-event-button-y-root event)))))

;;;  (:key-press 8)

;            ;; GdkEventKey
;            ((:key-press :key-release) gdk-event-key
;             (time :uint32)
;             (state gdk-modifier-type)
;             (keyval :uint)
;             (length :int)
;             (string (:string :free-from-foreign nil
;                              :free-to-foreign nil))
;             (hardware-keycode :uint16)
;             (group :uint8)
;             (is-modifier :uint))

(test gdk-event-key-press
   (let ((event (gdk-event-new :key-press)))
      (is (eq 'gdk-event-key (type-of event)))
      (is (eq :key-press (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))
      (is (eq 'bit (type-of (gdk-event-key-time event))))
      (is (= 0 (gdk-event-key-time event)))
      (is (eq 'NULL (type-of (gdk-event-key-state event))))
      (is-false (gdk-event-key-state event))
      (is (eq 'bit (type-of (gdk-event-key-keyval event))))
      (is (= 0 (gdk-event-key-keyval event)))
      (is (eq 'bit (type-of (gdk-event-key-length event))))
      (is (= 0 (gdk-event-key-length event)))
      (is (eq 'NULL (type-of (gdk-event-key-string event))))
      (is-false (gdk-event-key-string event))
      (is (eq 'bit (type-of (gdk-event-key-hardware-keycode event))))
      (is (= 0 (gdk-event-key-hardware-keycode event)))
      (is (eq 'bit (type-of (gdk-event-key-group event))))
      (is (= 0 (gdk-event-key-group event)))
      (is (eq 'bit (type-of (gdk-event-key-is-modifier event))))
      (is (= 0 (gdk-event-key-is-modifier event)))))

;;;  (:key-release 9)

(test gdk-event-key-release
   (let ((event (gdk-event-new :key-release)))
      (is (eq 'gdk-event-key (type-of event)))
      (is (eq :key-release (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))
      (is (eq 'bit (type-of (gdk-event-key-time event))))
      (is (= 0 (gdk-event-key-time event)))
      (is (eq 'NULL (type-of (gdk-event-key-state event))))
      (is-false (gdk-event-key-state event))
      (is (eq 'bit (type-of (gdk-event-key-keyval event))))
      (is (= 0 (gdk-event-key-keyval event)))
      (is (eq 'bit (type-of (gdk-event-key-length event))))
      (is (= 0 (gdk-event-key-length event)))
      (is (eq 'NULL (type-of (gdk-event-key-string event))))
      (is-false (gdk-event-key-string event))
      (is (eq 'bit (type-of (gdk-event-key-hardware-keycode event))))
      (is (= 0 (gdk-event-key-hardware-keycode event)))
      (is (eq 'bit (type-of (gdk-event-key-group event))))
      (is (= 0 (gdk-event-key-group event)))
      (is (eq 'bit (type-of (gdk-event-key-is-modifier event))))
      (is (= 0 (gdk-event-key-is-modifier event)))))

;;;  (:enter-notify 10)

;;            ;; GdkEventCrossing
;;            ((:enter-notify :leave-notify) gdk-event-crossing
;;             (subwindow (g-object gdk-window))
;;             (time :uint32)
;;             (x :double)
;;             (y :double)
;;             (x-root :double)
;;             (y-root :double)
;;             (mode gdk-crossing-mode)
;;             (detail gdk-notify-type)
;;             (focus :boolean)
;;             (state :uint))

(test gdk-event-enter-notify
   (let ((event (gdk-event-new :enter-notify)))
      (is (eq 'gdk-event-crossing (type-of event)))
      (is (eq :enter-notify (gdk-event-type event)))
      (is-false (gdk-event-window event))
      (is-false (gdk-event-send-event event))
      (is (eq 'NULL (type-of (gdk-event-crossing-subwindow event))))
      (is-false (gdk-event-crossing-subwindow event))
      (is (eq 'bit (type-of (gdk-event-crossing-time event))))
      (is (= 0 (gdk-event-crossing-time event)))
      (is (eq 'double-float (type-of (gdk-event-crossing-x event))))
      (is (= 0.0d0 (gdk-event-crossing-x event)))
      (is (eq 'double-float (type-of (gdk-event-crossing-y event))))
      (is (= 0.0d0 (gdk-event-crossing-y event)))
      (is (eq 'double-float (type-of (gdk-event-crossing-x-root event))))
      (is (= 0.0d0 (gdk-event-crossing-x-root event)))
      (is (eq 'double-float (type-of (gdk-event-crossing-y-root event))))
      (is (= 0.0d0 (gdk-event-crossing-y-root event)))))

;;;  (:leave-notify 11)
;;;  (:focus-change 12)
;;;  (:configure 13)
;;;  (:map 14)
;;;  (:unmap 15)
;;;  (:property-notify 16)
;;;  (:selection-clear 17)
;;;  (:selection-request 18)
;;;  (:selection-notify 19)
;;;  (:proximity-in 20)
;;;  (:proximity-out 21)
;;;  (:drag-enter 22)
;;;  (:drag-leave 23)
;;;  (:drag-motion 24)
;;;  (:drag-status 25)
;;;  (:drop-start 26)
;;;  (:drop-finished 27)
;;;  (:client-event 28)
;;;  (:visibility-notify 29)
;;;  (:not-used 30)          ; not used
;;;  (:scroll 31)
;;;  (:window-state 32)
;;;  (:setting 33)
;;;  (:owner-change 34)
;;;  (:grab-broken 35)
;;;  (:damage 36)
;;;  (:touch-begin 37)
;;;  (:touch-update 38)
;;;  (:touch-end 39)
;;;  (:touch-cancel 40)
;;;  (:touchpad-swipe 41)
;;;  (:touchpad-pinch 42)
;;;  (:pad-button-press 43)
;;;  (:pad-button-release 44)
;;;  (:pad-ring 45)
;;;  (:pad-strip 46)
;;;  (:pad-group-mode 47)
;;;  (:event-last 48)

;;;     GdkEventKey

;;;     GdkEventButton

;            ((:button-press
;              :2button-press
;              :double-button-press
;              :3button-press
;              :triple-button-press
;              :button-release) gdk-event-button
;             (time :uint32)
;             (x :double)
;             (y :double)
;             (axes (fixed-array :double 2))
;             (state :uint)
;             (button :uint)
;             (device (g-object gdk-device))
;             (x-root :double)
;             (y-root :double))

;;;     GdkEventTouch
;;;     GdkEventScroll
;;;     GdkEventMotion
;;;     GdkEventExpose
;;;     GdkEventVisibility
;;;     GdkEventCrossing
;;;     GdkEventFocus
;;;     GdkEventConfigure
;;;     GdkEventProperty
;;;     GdkEventSelection
;;;     GdkEventDND
;;;     GdkEventProximity
;;;     GdkEventWindowState
;;;     GdkEventSetting
;;;     GdkEventOwnerChange
;;;     GdkEventGrabBroken
;;;     GdkEventTouchpadSwipe
;;;     GdkEventTouchpadPinch
;;;     GdkEventPadButton
;;;     GdkEventPadAxis
;;;     GdkEventPadGroupMode

