(def-suite gdk-display :in gdk-suite)
(in-suite gdk-display)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDisplay

(test gdk-display-class
  ;; Type check
  (is (g-type-is-object "GdkDisplay"))
  ;; Check the registered name
  (is (eq 'gdk-display
          (registered-object-type-by-name "GdkDisplay")))
  ;; Check the type initializer
  (is (eq (gtype "GdkDisplay")
          (gtype (foreign-funcall "gdk_display_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GdkDisplay")))
  ;; Check the children
  (is (equal '("GdkX11Display" "GdkBroadwayDisplay" "GdkWaylandDisplay")
             (mapcar #'g-type-name (g-type-children "GdkDisplay"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GdkDisplay"))))
  ;; Check the class properties
  (is (equal '()
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GdkDisplay"))
                   #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDisplay" GDK-DISPLAY
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_display_get_type")
                       NIL)
             (get-g-type-definition "GdkDisplay"))))

;;; --- Signals ----------------------------------------------------------------

;;;     closed

#+nil
(test gdk-display-closed-signal
  (let* ((message nil)
         (display (gdk-display-default))
         (handler-id (g-signal-connect display "closed"
                       (lambda (display is-error)
                         (format t "in signal handler closed ~a~%" is-error)
                         (setf message "Signal closed")
                         (is (typep display 'gdk-display))
;                         FIXME: See the comment below.
;                         (is (eq t is-error))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit display "closed" t))
    (is (string= "Signal closed" message))
    ;; FIXME: The signal handler is not disconnected. The call of the function
    ;; gdk-display-close in the test for gdk-display-close calls again this
    ;; signal-handler.
    (is-false (g-signal-handler-disconnect display handler-id))))

;;;     monitor-added

#+nil
(test gdk-display-monitor-added-signal
  (let* ((message nil)
         (display (gdk-display-default))
         (handler-id (g-signal-connect display "monitor-added"
                       (lambda (display monitor)
                         (setf message "Signal monitor-added")
                         (is (typep display 'gdk-display))
                         (is (typep monitor 'gdk-monitor))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit display "monitor-added"
                                     (gdk-display-monitor display 0)))
    (is (string= "Signal monitor-added" message))
    (is-false (g-signal-handler-disconnect display handler-id))))

;;;     monitor-removed

#+nil
(test gdk-display-monitor-removed-signal
  (let* ((message nil)
         (display (gdk-display-default))
         (handler-id (g-signal-connect display "monitor-removed"
                       (lambda (display monitor)
                         (setf message "Signal monitor-removed")
                         (is (typep display 'gdk-display))
                         (is (typep monitor 'gdk-monitor))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit display "monitor-removed"
                                     (gdk-display-monitor display 0)))
    (is (string= "Signal monitor-removed" message))
    (is-false (g-signal-handler-disconnect display handler-id))))

;;;     opened

#+nil
(test gdk-display-opened-signal
  (let* ((message nil)
         (display (gdk-display-default))
         (handler-id (g-signal-connect display "opened"
                       (lambda (display)
                         (setf message "Signal opened")
                         (is (typep display 'gdk-display))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit display "opened"))
    (is (string= "Signal opened" message))
    (is-false (g-signal-handler-disconnect display handler-id))))

;;;     seat-added

#+nil
(test gdk-display-seat-added-signal
  (let* ((message nil)
         (display (gdk-display-default))
         (handler-id (g-signal-connect display "seat-added"
                       (lambda (display seat)
                         (setf message "Signal seat-added")
                         (is (typep display 'gdk-display))
                         (is (typep seat 'gdk-seat))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit display "seat-added"
                                     (gdk-display-default-seat display)))
    (is (string= "Signal seat-added" message))
    (is-false (g-signal-handler-disconnect display handler-id))))

;;;     seat-removed

#+nil
(test gdk-display-seat-removed-signal
  (let* ((message nil)
         (display (gdk-display-default))
         (handler-id (g-signal-connect display "seat-removed"
                       (lambda (display seat)
                         (setf message "Signal seat-removed")
                         (is (typep display 'gdk-display))
                         (is (typep seat 'gdk-seat))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit display "seat-removed"
                                     (gdk-display-default-seat display)))
    (is (string= "Signal seat-removed" message))
    (is-false (g-signal-handler-disconnect display handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-display-open

(test gdk-display-open
  (let ((name (gdk-display-name (gdk-display-default))))
    (is-true (gdk-display-open name))))

;;;     gdk-display-default

(test gdk-display-default
  (is (typep (gdk-display-default) 'gdk-display)))

;;;     gdk-display-name

(test gdk-display-name
  (is (string= ":0" (gdk-display-name (gdk-display-default)))))

;;;     gdk_display_get_n_screens                          deprecated
;;;     gdk_display_get_screen                             deprecated

;;;     gdk-display-default-screen

(test gdk-display-default-screen
  (let ((display (gdk-display-default)))
    (is (typep (gdk-display-default-screen display) 'gdk-screen))))

;;;     gdk_display_get_device_manager                     deprecated
;;;     gdk_display_pointer_ungrab                         deprecated
;;;     gdk_display_keyboard_ungrab                        deprecated
;;;     gdk_display_pointer_is_grabbed                     deprecated

;;;     gdk_display_device_is_grabbed

;; TODO: The grap is successfully but it is not released.

#+nil
(test gdk-display-device-is-grabbed
  (let* ((display (gdk-display-default))
         (seat (gdk-display-default-seat display))
         (device (gdk-seat-pointer seat))
         (window (gdk-default-root-window)))
    (is-false (gdk-display-device-is-grabbed display device))
    (is-false (gdk-window-show window))
    (is (eq :success (gdk-seat-grab seat window :pointer nil nil nil nil)))
    (is-true (gdk-display-device-is-grabbed display device))
    (is-false (gdk-seat-ungrab seat))
    (is-false (gdk-display-device-is-grabbed display device))))

;;;     gdk-display-beep

(test gdk-display-beep
  (is-false (gdk-display-beep (gdk-display-default))))

;;;     gdk-display-sync

(test gdk-display-sync
  (is-false (gdk-display-sync (gdk-display-default))))

;;;     gdk-display-flush

(test gdk-display-flush
  (is-false (gdk-display-flush (gdk-display-default))))

;;;     gdk-display-close
;;;     gdk-display-is-closed

(test gdk-display-close
  (let ((display (gdk-display-default)))
    (is-false (gdk-display-is-closed display))
    (is-false (gdk-display-close display))
    (is-true (gdk-display-is-closed display))))

;;;     gdk_display_get_event
;;;     gdk_display_peek_event
;;;     gdk_display_put_event
;;;     gdk_display_has_pending

;;;     gdk-display-set-double-click-time
;;;     gdk-display-set-double-click-distance

(test gdk-display-double-click
  (let ((display (gdk-display-default)))
    (is-false (gdk-display-set-double-click-time display 500))
    (is-false (gdk-display-set-double-click-distance display 10))))

;;;     gdk_display_get_pointer                            deprecated
;;;     gdk_display_list_devices                           deprecated
;;;     gdk_display_get_window_at_pointer                  deprecated
;;;     gdk_display_warp_pointer                           deprecated

;;;     gdk-display-supports-cursor-color
;;;     gdk-display-supports-cursor-alpha

(test gdk-display-supports-cursor
  (let ((display (gdk-display-default)))
    (is-true (gdk-display-supports-cursor-color display))
    (is-true (gdk-display-supports-cursor-alpha display))))

;;;     gdk-display-default-cursor-size
;;;     gdk-display-maximal-cursor-size

(test gdk-display-cursor-size
  (let ((display (gdk-display-default)))
    (is (= 24 (gdk-display-default-cursor-size display)))
    (is (equal '(128 128)
                (multiple-value-list
                    (gdk-display-maximal-cursor-size display))))))

;;;     gdk-display-default-group

(test gdk-display-default-group
  (let ((display (gdk-display-default)))
    (is (typep (gdk-display-default-group display) 'gdk-window))))

;;;     gdk-display-supports-selection-notification
;;;     gdk-display-request-selection-notification

(test gdk-display-selection-notification
  (let ((display (gdk-display-default)))
    (is-true (gdk-display-supports-selection-notification display))
    (is-true (gdk-display-request-selection-notification display "CLIPBOARD"))))

;;;     gdk-display-supports-clipboard-persistence
;;;     gdk-display-store-clipboard

(test gdk-display-clipboard
  (let* ((display (gdk-display-default))
         (window (gdk-display-default-group display)))
    (is-false (gdk-display-supports-clipboard-persistence display))
    (is-false (gdk-display-store-clipboard display window 0 '("ClIPBOARD")))))

;;;     gdk-display-supports-shapes
;;;     gdk-display-supports-input_shapes

(test gdk-display-shapes
  (let ((display (gdk-display-default)))
    (is-true (gdk-display-supports-shapes display))
    (is-true (gdk-display-supports-input-shapes display))))

;;;     gdk_display_supports_composite                     deprecated

;;;     gdk-display-app-launch-context

(test gdk-display-app-launch-context
  (let ((display (gdk-display-default)))
    (is (typep (gdk-display-app-launch-context display)
               'gdk-app-launch-context))))

;;;     gdk-display-notify-startup-complete

(test gdk-display-notify-startup-complete
  (let ((display (gdk-display-default)))
    (is-false (gdk-display-notify-startup-complete display "crategus.com"))))

;;;     gdk-display-default-seat

(test gdk-display-default-seat
  (let ((display (gdk-display-default)))
    (is (typep (gdk-display-default-seat display) 'gdk-seat))))

;;;     gdk-display-list-seats

(test gdk-display-list-seats
  (let ((display (gdk-display-default)))
    (is (listp (gdk-display-list-seats display)))
    (is (every (lambda (x) (typep x 'gdk-seat))
               (gdk-display-list-seats display)))))

;;;     gdk-display-n-monitors

(test gdk-display-n-monitors
  (let ((display (gdk-display-default)))
    (is (<= 1 (gdk-display-n-monitors display)))))

;;;     gdk-display-monitor

(test gdk-display-monitor
  (let ((display (gdk-display-default)))
    (is (typep (gdk-display-monitor display 0) 'gdk-monitor))))

;;;     gdk-display-primary-monitor

(test gdk-display-primary-monitor
  (let ((display (gdk-display-default)))
    (is (typep (gdk-display-primary-monitor display) 'gdk-monitor))))

;;;     gdk-display-monitor-at-point

(test gdk-display-monitor-at-point
  (let ((display (gdk-display-default)))
    (is (typep (gdk-display-monitor-at-point display 0 0) 'gdk-monitor))))

;;;     gdk-display-monitor-at-window

(test gdk-display-monitor-at-window
  (let* ((display (gdk-display-default))
         (window (gdk-display-default-group display)))
    (is (typep (gdk-display-monitor-at-window display window) 'gdk-monitor))))

;;; 2021-8-20
