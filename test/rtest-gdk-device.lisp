(def-suite gdk-device :in gdk-suite)
(in-suite gdk-device)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkInputSource

(test gdk-input-source
  ;; Check the type
  (is (g-type-is-enum "GdkInputSource"))
  ;; Check the type initializer
  (is (eq (gtype "GdkInputSource")
          (gtype (foreign-funcall "gdk_input_source_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-input-source
          (registered-enum-type "GdkInputSource")))
  ;; Check the names
  (is (equal '("GDK_SOURCE_MOUSE" "GDK_SOURCE_PEN" "GDK_SOURCE_ERASER"
               "GDK_SOURCE_CURSOR" "GDK_SOURCE_KEYBOARD"
               "GDK_SOURCE_TOUCHSCREEN" "GDK_SOURCE_TOUCHPAD"
               "GDK_SOURCE_TRACKPOINT" "GDK_SOURCE_TABLET_PAD")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkInputSource"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkInputSource"))))
  ;; Check the nick names
  (is (equal '("mouse" "pen" "eraser" "cursor" "keyboard" "touchscreen"
               "touchpad" "trackpoint" "tablet-pad")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkInputSource"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkInputSource"
                             GDK-INPUT-SOURCE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_input_source_get_type")
                             (:MOUSE 0)
                             (:PEN 1)
                             (:ERASER 2)
                             (:CURSOR 3)
                             (:KEYBOARD 4)
                             (:TOUCHSCREEN 5)
                             (:TOUCHPAD 6)
                             (:TRACKPOINT 7)
                             (:TABLET-PAD 8))
             (get-g-type-definition "GdkInputSource"))))

;;;     GdkInputMode

(test gdk-input-mode
  ;; Check the type
  (is (g-type-is-enum "GdkInputMode"))
  ;; Check the type initializer
  (is (eq (gtype "GdkInputMode")
          (gtype (foreign-funcall "gdk_input_mode_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-input-mode
          (registered-enum-type "GdkInputMode")))
  ;; Check the names
  (is (equal '("GDK_MODE_DISABLED" "GDK_MODE_SCREEN" "GDK_MODE_WINDOW")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkInputMode"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkInputMode"))))
  ;; Check the nick names
  (is (equal '("disabled" "screen" "window")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkInputMode"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkInputMode"
                             GDK-INPUT-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_input_mode_get_type")
                             (:DISABLED 0)
                             (:SCREEN 1)
                             (:WINDOW 2))
             (get-g-type-definition "GdkInputMode"))))

;;;     GdkAxisUse

(test gdk-axis-use
  ;; Check the type
  (is-true (g-type-is-enum "GdkAxisUse"))
  ;; Check the type initializer
  (is (eq (gtype "GdkAxisUse")
          (gtype (foreign-funcall "gdk_axis_use_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-axis-use
          (registered-enum-type "GdkAxisUse")))
  ;; Check the names
  (is (equal '("GDK_AXIS_IGNORE" "GDK_AXIS_X" "GDK_AXIS_Y" "GDK_AXIS_PRESSURE"
               "GDK_AXIS_XTILT" "GDK_AXIS_YTILT" "GDK_AXIS_WHEEL"
               "GDK_AXIS_DISTANCE" "GDK_AXIS_ROTATION" "GDK_AXIS_SLIDER"
               "GDK_AXIS_LAST")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkAxisUse"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkAxisUse"))))
  ;; Check the nick names
  (is (equal '("ignore" "x" "y" "pressure" "xtilt" "ytilt" "wheel" "distance"
               "rotation" "slider" "last")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkAxisUse"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkAxisUse"
                             GDK-AXIS-USE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_axis_use_get_type")
                             (:IGNORE 0)
                             (:X 1)
                             (:Y 2)
                             (:PRESSURE 3)
                             (:XTILT 4)
                             (:YTILT 5)
                             (:WHEEL 6)
                             (:DISTANCE 7)
                             (:ROTATION 8)
                             (:SLIDER 9)
                             (:LAST 10))
             (get-g-type-definition "GdkAxisUse"))))

;;;     GdkAxisFlags                                       Since 3.22

(test gdk-axis-flags
  ;; Check the type
  (is (g-type-is-flags "GdkAxisFlags"))
  ;; Check the registered name
  (is (eq 'gdk-axis-flags
          (registered-flags-type "GdkAxisFlags")))
  ;; Check the type initializer
  (is (eq (gtype "GdkAxisFlags")
          (gtype (foreign-funcall "gdk_axis_flags_get_type" g-size))))
  ;; Check the names
  (is (equal '("GDK_AXIS_FLAG_X" "GDK_AXIS_FLAG_Y" "GDK_AXIS_FLAG_PRESSURE"
               "GDK_AXIS_FLAG_XTILT" "GDK_AXIS_FLAG_YTILT" "GDK_AXIS_FLAG_WHEEL"
               "GDK_AXIS_FLAG_DISTANCE" "GDK_AXIS_FLAG_ROTATION"
               "GDK_AXIS_FLAG_SLIDER")
             (mapcar #'flags-item-name
                     (get-flags-items "GdkAxisFlags"))))
  ;; Check the values
  (is (equal '(2 4 8 16 32 64 128 256 512)
             (mapcar #'flags-item-value
                     (get-flags-items "GdkAxisFlags"))))
  ;; Check the nick names
  (is (equal '("x" "y" "pressure" "xtilt" "ytilt" "wheel" "distance" "rotation"
               "slider")
             (mapcar #'flags-item-nick
                     (get-flags-items "GdkAxisFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkAxisFlags"
                              GDK-AXIS-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_axis_flags_get_type")
                              (:X 2)
                              (:Y 4)
                              (:PRESSURE 8)
                              (:XTILT 16)
                              (:YTILT 32)
                              (:WHEEL 64)
                              (:DISTANCE 128)
                              (:ROTATION 256)
                              (:SLIDER 512))
             (get-g-type-definition "GdkAxisFlags"))))

;;;     GdkDeviceToolType                                  Since 3.22

(test gdk-device-tool-type
  ;; Check the type
  (is (g-type-is-enum "GdkDeviceToolType"))
  ;; Check the type initializer
  (is (eq (gtype "GdkDeviceToolType")
          (gtype (foreign-funcall "gdk_device_tool_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-device-tool-type
          (registered-enum-type "GdkDeviceToolType")))
  ;; Check the names
  (is (equal '("GDK_DEVICE_TOOL_TYPE_UNKNOWN" "GDK_DEVICE_TOOL_TYPE_PEN"
               "GDK_DEVICE_TOOL_TYPE_ERASER" "GDK_DEVICE_TOOL_TYPE_BRUSH"
               "GDK_DEVICE_TOOL_TYPE_PENCIL" "GDK_DEVICE_TOOL_TYPE_AIRBRUSH"
               "GDK_DEVICE_TOOL_TYPE_MOUSE" "GDK_DEVICE_TOOL_TYPE_LENS")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkDeviceToolType"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkDeviceToolType"))))
  ;; Check the nick names
  (is (equal '("unknown" "pen" "eraser" "brush" "pencil" "airbrush" "mouse"
               "lens")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkDeviceToolType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkDeviceToolType"
                             GDK-DEVICE-TOOL-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_device_tool_type_get_type")
                             (:UNKNOWN 0)
                             (:PEN 1)
                             (:ERASER 2)
                             (:BRUSH 3)
                             (:PENCIL 4)
                             (:AIRBRUSH 5)
                             (:MOUSE 6)
                             (:LENS 7))
             (get-g-type-definition "GdkDeviceToolType"))))

;;;     GdkDeviceType

(test gdk-device-type
  ;; Check the type
  (is (g-type-is-enum "GdkDeviceType"))
  ;; Check the type initializer
  (is (eq (gtype "GdkDeviceType")
          (gtype (foreign-funcall "gdk_device_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-device-type
          (registered-enum-type "GdkDeviceType")))
  ;; Check the names
  (is (equal '("GDK_DEVICE_TYPE_MASTER" "GDK_DEVICE_TYPE_SLAVE"
               "GDK_DEVICE_TYPE_FLOATING")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkDeviceType"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkDeviceType"))))
  ;; Check the nick names
  (is (equal '("master" "slave" "floating")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkDeviceType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkDeviceType"
                             GDK-DEVICE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_device_type_get_type")
                             (:MASTER 0)
                             (:SLAVE 1)
                             (:FLOATING 2))
             (get-g-type-definition "GdkDeviceType"))))

;;;     GdkGrabOwnership

(test gdk-grab-ownership
  ;; Check the type
  (is (g-type-is-enum "GdkGrabOwnership"))
  ;; Check the type initializer
  (is (eq (gtype "GdkGrabOwnership")
          (gtype (foreign-funcall "gdk_grab_ownership_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-grab-ownership
          (registered-enum-type "GdkGrabOwnership")))
  ;; Check the names
  (is (equal '("GDK_OWNERSHIP_NONE" "GDK_OWNERSHIP_WINDOW"
               "GDK_OWNERSHIP_APPLICATION")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkGrabOwnership"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkGrabOwnership"))))
  ;; Check the nick names
  (is (equal '("none" "window" "application")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkGrabOwnership"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkGrabOwnership"
                             GDK-GRAB-OWNERSHIP
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_grab_ownership_get_type")
                             (:NONE 0)
                             (:WINDOW 1)
                             (:APPLICATION 2))
             (get-g-type-definition "GdkGrabOwnership"))))

;;;     GdkTimeCoord

;;;     GdkGrabStatus

(test gdk-grab-status
  ;; Check the type
  (is (g-type-is-enum "GdkGrabStatus"))
  ;; Check the type initializer
  (is (eq (gtype "GdkGrabStatus")
          (gtype (foreign-funcall "gdk_grab_status_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-grab-status
          (registered-enum-type "GdkGrabStatus")))
  ;; Check the names
  (is (equal '("GDK_GRAB_SUCCESS" "GDK_GRAB_ALREADY_GRABBED"
               "GDK_GRAB_INVALID_TIME" "GDK_GRAB_NOT_VIEWABLE"
               "GDK_GRAB_FROZEN" "GDK_GRAB_FAILED")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkGrabStatus"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkGrabStatus"))))
  ;; Check the nick names
  (is (equal '("success" "already-grabbed" "invalid-time" "not-viewable"
               "frozen" "failed")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkGrabStatus"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkGrabStatus"
                             GDK-GRAB-STATUS
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_grab_status_get_type")
                             (:SUCCESS 0)
                             (:ALREADY-GRABBED 1)
                             (:INVALID-TIME 2)
                             (:NOT-VIEWABLE 3)
                             (:FROZEN 4)
                             (:FAILED 5))
             (get-g-type-definition "GdkGrabStatus"))))

;;;     GdkDeviceTool                                      Since 3.22

(test gdk-device-tool-class
  ;; Type check
  (is (g-type-is-object "GdkDeviceTool"))
  ;; Check the registered name
  (is (eq 'gdk-device-tool
          (registered-object-type-by-name "GdkDeviceTool")))
  ;; Check the type initializer
  (is (eq (gtype "GdkDeviceTool")
          (gtype (foreign-funcall "gdk_device_tool_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GdkDeviceTool")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GdkDeviceTool"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GdkDeviceTool"))))
  ;; Check the class properties
  (is (equal '("axes" "hardware-id" "serial" "tool-type")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GdkDeviceTool"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDeviceTool" GDK-DEVICE-TOOL
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_tool_get_type")
                       ((AXES GDK-DEVICE-TOOL-AXES "axes" "GdkAxisFlags" T NIL)
                        (HARDWARE-ID GDK-DEVICE-TOOL-HARDWARE-ID "hardware-id"
                         "guint64" T NIL)
                        (SERIAL GDK-DEVICE-TOOL-SERIAL "serial" "guint64" T
                         NIL)
                        (TOOL-TYPE GDK-DEVICE-TOOL-TOOL-TYPE "tool-type"
                         "GdkDeviceToolType" T NIL)))
             (get-g-type-definition "GdkDeviceTool"))))

;;;     GdkDevice

(test gdk-device-class
  ;; Type check
  (is (g-type-is-object "GdkDevice"))
  ;; Check the registered name
  (is (eq 'gdk-device
          (registered-object-type-by-name "GdkDevice")))
  ;; Check the type initializer
  (is (eq (gtype "GdkDevice")
          (gtype (foreign-funcall "gdk_device_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GdkDevice")))
  ;; Check the children
  (is (equal '("GdkX11DeviceXI2" "GdkX11DeviceCore")
             (mapcar #'g-type-name (g-type-children "GdkDevice"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GdkDevice"))))
  ;; Check the class properties
  (is (equal '("associated-device" "axes" "device-manager" "display"
               "has-cursor" "input-mode" "input-source" "n-axes" "name"
               "num-touches" "product-id" "seat" "tool" "type" "vendor-id")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GdkDevice"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDevice" GDK-DEVICE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_get_type")
                       ((ASSOCIATED-DEVICE GDK-DEVICE-ASSOCIATED-DEVICE
                         "associated-device" "GdkDevice" T NIL)
                        (AXES GDK-DEVICE-AXES "axes" "GdkAxisFlags" T NIL)
                        (DEVICE-MANAGER GDK-DEVICE-DEVICE-MANAGER
                         "device-manager" "GdkDeviceManager" T NIL)
                        (DISPLAY GDK-DEVICE-DISPLAY "display" "GdkDisplay" T
                         NIL)
                        (HAS-CURSOR GDK-DEVICE-HAS-CURSOR "has-cursor"
                         "gboolean" T NIL)
                        (INPUT-MODE GDK-DEVICE-INPUT-MODE "input-mode"
                         "GdkInputMode" T T)
                        (INPUT-SOURCE GDK-DEVICE-INPUT-SOURCE "input-source"
                         "GdkInputSource" T NIL)
                        (N-AXES GDK-DEVICE-N-AXES "n-axes" "guint" T NIL)
                        (NAME GDK-DEVICE-NAME "name" "gchararray" T NIL)
                        (NUM-TOUCHES GDK-DEVICE-NUM-TOUCHES "num-touches"
                         "guint" T NIL)
                        (PRODUCT-ID GDK-DEVICE-PRODUCT-ID "product-id"
                         "gchararray" T NIL)
                        (SEAT GDK-DEVICE-SEAT "seat" "GdkSeat" T T)
                        (TOOL GDK-DEVICE-TOOL "tool" "GdkDeviceTool" T NIL)
                        (TYPE GDK-DEVICE-TYPE "type" "GdkDeviceType" T NIL)
                        (VENDOR-ID GDK-DEVICE-VENDOR-ID "vendor-id"
                         "gchararray" T NIL)))
             (get-g-type-definition "GdkDevice"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-device-properties.1
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-keyboard seat)))
    (is (typep (gdk-device-associated-device device) 'gdk-device))
    (is-false (gdk-device-axes device))
    (is (typep (gdk-device-device-manager device) 'gdk-device-manager))
    (is (typep (gdk-device-display device) 'gdk-display))
    (is-false (gdk-device-has-cursor device))
    (is (eq :screen (gdk-device-input-mode device)))
    (is (eq :keyboard (gdk-device-input-source device)))
    (is (= 0 (gdk-device-n-axes device)))
    (is (string= "Virtual core keyboard" (gdk-device-name device)))
    (is (= 0 (gdk-device-num-touches device)))
    (is-false (gdk-device-product-id device))
    (is (typep (gdk-device-seat device) 'gdk-seat))
    (is-false (gdk-device-tool device))
    (is (eq :master (gdk-device-type device)))
    (is-false (gdk-device-vendor-id device))))

(test gdk-device-properties.2
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat)))
    (is (typep (gdk-device-associated-device device) 'gdk-device))
    (is-false (gdk-device-axes device))
    (is (typep (gdk-device-device-manager device) 'gdk-device-manager))
    (is (typep (gdk-device-display device) 'gdk-display))
    (is-true (gdk-device-has-cursor device))
    (is (eq :screen (gdk-device-input-mode device)))
    (is (eq :mouse (gdk-device-input-source device)))
    (is (= 4 (gdk-device-n-axes device)))
    (is (string= "Virtual core pointer" (gdk-device-name device)))
    (is (= 0 (gdk-device-num-touches device)))
    (is-false (gdk-device-product-id device))
    (is (typep (gdk-device-seat device) 'gdk-seat))
    (is-false (gdk-device-tool device))
    (is (eq :master (gdk-device-type device)))
    (is-false (gdk-device-vendor-id device))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gtk-device-changed-signal
  (let* ((message nil)
         (seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat))
         (handler-id (g-signal-connect device "changed"
                       (lambda (device)
                         (setf message "Signal changed")
                         (is (typep device 'gdk-device))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit device "changed"))
    (is (string= "Signal changed" message))
    (is-false (g-signal-handler-disconnect device handler-id))))

;;;     tool-changed

(test gtk-device-tool-changed-signal
  (let* ((message nil)
         (seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat))
         (handler-id (g-signal-connect device "tool-changed"
                       (lambda (device tool)
                         (setf message "Signal tool-changed")
                         (is (typep device 'gdk-device))
                         (is (typep tool 'gdk-device-tool))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit device "tool-changed" (make-instance 'gdk-device-tool)))
    (is (string= "Signal tool-changed" message))
    (is-false (g-signal-handler-disconnect device handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-device-key

(test gdk-device-key
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-keyboard seat)))
    (is (equal '(97 :control-mask)
               (multiple-value-list (setf (gdk-device-key device 0)
                                          '(97 :control-mask)))))
    (is (equal '(97 (:control-mask))
               (multiple-value-list (gdk-device-key device 0))))
    (is (equal '(65 (:shift-mask :control-mask))
               (multiple-value-list (setf (gdk-device-key device 0)
                                          '(65 (:shift-mask :control-mask))))))
    (is (equal '(65 (:SHIFT-MASK :CONTROL-MASK))
               (multiple-value-list (gdk-device-key device 0))))))

;;;     gdk-device-axis-use

(test gdk-device-axis-use
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat)))
    (is (eq :ignore (gdk-device-axis-use device 0)))
    (is (eq :x (setf (gdk-device-axis-use device 0) :x)))
    (is (eq :x (gdk-device-axis-use device 0)))
    (is (eq :ignore (setf (gdk-device-axis-use device 0) :ignore)))
    (is (eq :ignore (gdk-device-axis-use device 0)))))

;;;     gdk-device-list-slave-devices

(test gdk-device-list-slave-devices
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat)))
    (is (every #'stringp
               (mapcar #'gdk-device-name
                       (gdk-device-list-slave-devices device))))))

;;;     gdk-device-n-keys

(test gdk-device-n-keys
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat)))
    (is (= 0 (gdk-device-n-keys device)))))

;;;     gdk_device_warp
;;;     gdk_device_grab
;;;     gdk_device_ungrab

;;;     gdk-device-state

(test gdk-device-state
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat))
         (window (gdk-default-root-window)))
    (is (listp (multiple-value-list (gdk-device-state device window))))
    (is (every (lambda (x) (typep x 'double-float))
               (first (multiple-value-list (gdk-device-state device window)))))
    (is (equal '(:MOD2-MASK)
               (second (multiple-value-list (gdk-device-state device window)))))))

;;;     gdk_device_get_position
;;;     gdk_device_get_position_double
;;;     gdk_device_get_window_at_position
;;;     gdk_device_get_window_at_position_double

;;;     gdk-device-history

;; TODO: We need an example to test the functions.

(test gdk-device-history
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat))
         (window (gdk-default-root-window)))
    (is-false (gdk-device-history device window 0 1000))))

;;;     gdk-device-axis

;; TODO: Find an example with a result different from nil.

(test gdk-device-axis
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat))
         (window (gdk-default-root-window))
         (axes (gdk-device-state device window)))
    (is (every #'numberp axes))
    (is-false (gdk-device-axis device axes :x))
    (is-false (gdk-device-axis device axes :y))
    (is-false (gdk-device-axis device axes :pressure))
    (is-false (gdk-device-axis device axes :xtilt))
    (is-false (gdk-device-axis device axes :ytilt))
    (is-false (gdk-device-axis device axes :wheel))
    (is-false (gdk-device-axis device axes :distance))
    (is-false (gdk-device-axis device axes :rotation))
    (is-false (gdk-device-axis device axes :slider))
    (is-false (gdk-device-axis device axes :last))))

;;;     gdk-device-list-axes

(test gdk-device-list-axes
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat)))
    (is (equal '("Rel X" "Rel Y" "Rel Horiz Scroll" "Rel Vert Scroll")
                (gdk-device-list-axes device)))))

;;;     gdk-device-axis-value

(test gdk-device-axis-value
  (let* ((seat (gdk-display-default-seat (gdk-display-default)))
         (device (gdk-seat-pointer seat))
         (window (gdk-default-root-window))
         (axes (gdk-device-state device window)))
    (is (typep (gdk-device-axis-value device axes "Rel X") 'double-float))
    (is (typep (gdk-device-axis-value device axes "Rel Y") 'double-float))
    (is (typep (gdk-device-axis-value device axes "Rel Horiz Scroll") 'double-float))
    (is (typep (gdk-device-axis-value device axes "Rel Vert Scroll") 'double-float))))

;;;     gdk_device_get_last_event_window

;;;     gdk_device_tool_get_serial
;;;     gdk_device_tool_get_tool_type

;;; 2020-11-9
