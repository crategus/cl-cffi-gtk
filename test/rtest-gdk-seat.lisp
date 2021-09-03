(def-suite gdk-seat :in gdk-suite)
(in-suite gdk-seat)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkSeatCapabilities

(test gdk-seat-capabilities-flags
  ;; Check the type
  (is (g-type-is-flags "GdkSeatCapabilities"))
  ;; Check the registered name
  (is (eq 'gdk-seat-capabilities
          (registered-flags-type "GdkSeatCapabilities")))
  ;; Check the type initializer
  (is (eq (gtype "GdkSeatCapabilities")
          (gtype (foreign-funcall "gdk_seat_capabilities_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_SEAT_CAPABILITY_NONE" "GDK_SEAT_CAPABILITY_POINTER"
               "GDK_SEAT_CAPABILITY_TOUCH" "GDK_SEAT_CAPABILITY_TABLET_STYLUS"
               "GDK_SEAT_CAPABILITY_KEYBOARD" "GDK_SEAT_CAPABILITY_ALL_POINTING"
               "GDK_SEAT_CAPABILITY_ALL")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkSeatCapabilities"))))
  ;; Check the values
  (is (equal '(0 1 2 4 8 7 15)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkSeatCapabilities"))))
  ;; Check the nick names
  (is (equal '("none" "pointer" "touch" "tablet-stylus" "keyboard"
               "all-pointing" "all")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkSeatCapabilities"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkSeatCapabilities"
                              GDK-SEAT-CAPABILITIES
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_seat_capabilities_get_type")
                              (:NONE 0)
                              (:POINTER 1)
                              (:TOUCH 2)
                              (:TABLET-STYLUS 4)
                              (:KEYBOARD 8)
                              (:ALL-POINTING 7)
                              (:ALL 15))
             (get-g-type-definition "GdkSeatCapabilities"))))

;;;     GdkSeat

(test gtk-seat-class
  ;; Type check
  (is (g-type-is-object "GdkSeat"))
  ;; Check the registered name
  (is (eq 'gdk-seat
          (registered-object-type-by-name "GdkSeat")))
  ;; Check the type initializer
  (is (eq (gtype "GdkSeat")
          (gtype (foreign-funcall "gdk_seat_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GdkSeat")))
  ;; Check the children
  (is (equal '("GdkSeatDefault")
             (mapcar #'g-type-name (g-type-children "GdkSeat"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GdkSeat"))))
  ;; Check the class properties
  (is (equal '("display")
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GdkSeat"))
                   #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkSeat" GDK-SEAT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_seat_get_type")
                       ((DISPLAY GDK-SEAT-DISPLAY "display" "GdkDisplay" T
                         NIL)))
             (get-g-type-definition "GdkSeat"))))

;;; --- Properties -------------------------------------------------------------

;;;     gdk-seat-display

(test gdk-seat-display
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is (typep (gdk-seat-display seat) 'gdk-display))))

;;; --- Signals ----------------------------------------------------------------

;;;     device-added

#+nil
(test gdk-seat-device-added-signal
  (let* ((message nil)
         (seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat))
         (handler-id (g-signal-connect seat "device-added"
                       (lambda (seat device)
                         (setf message "Signal device-added")
                         (is (typep seat 'gdk-seat))
                         (is (typep device 'gdk-device))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit seat "device-added" device))
    (is (string= "Signal device-added" message))
    (is-false (g-signal-handler-disconnect seat handler-id))))

;;;     device-removed

#+nil
(test gdk-seat-device-removed-signal
  (let* ((message nil)
         (seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat))
         (handler-id (g-signal-connect seat "device-removed"
                       (lambda (seat device)
                         (setf message "Signal device-removed")
                         (is (typep seat 'gdk-seat))
                         (is (typep device 'gdk-device))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit seat "device-removed" device))
    (is (string= "Signal device-removed" message))
    (is-false (g-signal-handler-disconnect seat handler-id))))

;;;     tool-added

#+nil
(test gdk-seat-tool-added-signal
  (let* ((message nil)
         (seat (gdk-display-default-seat (gdk-display-default)))
         (tool (make-instance 'gdk-device-tool))
         (handler-id (g-signal-connect seat "tool-added"
                       (lambda (seat tool)
                         (setf message "Signal tool-added")
                         (is (typep seat 'gdk-seat))
                         (is (typep tool 'gdk-device-tool))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit seat "tool-added" tool))
    (is (string= "Signal tool-added" message))
    (is-false (g-signal-handler-disconnect seat handler-id))))

;;;     tool-removed

#+nil
(test gdk-seat-tool-removed-signal
  (let* ((message nil)
         (seat (gdk-display-default-seat (gdk-display-default)))
         (tool (make-instance 'gdk-device-tool))
         (handler-id (g-signal-connect seat "tool-removed"
                       (lambda (seat tool)
                         (setf message "Signal tool-removed")
                         (is (typep seat 'gdk-seat))
                         (is (typep tool 'gdk-device-tool))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit seat "tool-removed" tool))
    (is (string= "Signal tool-removed" message))
    (is-false (g-signal-handler-disconnect seat handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-seat-grab
;;;     gdk-seat-ungrab

(test gdk-seat-grab
  (let ((seat (gdk-display-default-seat (gdk-display-default)))
        (window (gdk-default-root-window)))
    (is-false (gdk-window-show window))
    (is (eq :success (gdk-seat-grab seat window :all nil nil nil nil)))
    (is-false (gdk-seat-ungrab seat))))

;;;     gdk-seat-capabilities

(test gdk-seat-capabilities
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is (equal '(:POINTER :KEYBOARD)
               (gdk-seat-capabilities seat)))))

;;;     gdk-seat-pointer

(test gdk-seat-pointer
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is (typep (gdk-seat-pointer seat) 'gdk-device))))

;;;     gdk-seat-keyboard

(test gdk-seat-keyboard
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is (typep (gdk-seat-keyboard seat) 'gdk-device))))

;;;     gdk-seat-slaves

(test gdk-seat-slaves
  (let ((seat (gdk-display-default-seat (gdk-display-default))))
    (is (every (lambda (x) (typep x 'gdk-device))
               (gdk-seat-slaves seat :pointer)))
    (is (every (lambda (x) (typep x 'gdk-device))
               (gdk-seat-slaves seat :touch)))
    (is (every (lambda (x) (typep x 'gdk-device))
               (gdk-seat-slaves seat :tablet-stylus)))
    (is (every (lambda (x) (typep x 'gdk-device))
               (gdk-seat-slaves seat :keyboard)))
    (is (every (lambda (x) (typep x 'gdk-device))
               (gdk-seat-slaves seat :all-pointing)))
    (is (every (lambda (x) (typep x 'gdk-device))
               (gdk-seat-slaves seat :all)))
    (is (every (lambda (x) (typep x 'gdk-device))
               (gdk-seat-slaves seat '(:pointer :keyboard))))))

;;; 2020-11-8
