(def-suite gdk-screen :in gdk-suite)
(in-suite gdk-screen)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkScreen

(test gdk-screen-class
  ;; Type check
  (is (g-type-is-object "GdkScreen"))
  ;; Check the registered name
  (is (eq 'gdk-screen
          (registered-object-type-by-name "GdkScreen")))
  ;; Check the type initializer
  (is (eq (gtype "GdkScreen")
          (gtype (foreign-funcall "gdk_screen_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GdkScreen")))
  ;; Check the children
  (is (equal '("GdkX11Screen")
             (mapcar #'g-type-name (g-type-children "GdkScreen"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GdkScreen"))))
  ;; Check the class properties
  (is (equal '("font-options" "resolution")
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GdkScreen"))
                   #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkScreen" GDK-SCREEN
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_screen_get_type")
                       ((FONT-OPTIONS GDK-SCREEN-FONT-OPTIONS "font-options"
                         "gpointer" T T)
                        (RESOLUTION GDK-SCREEN-RESOLUTION "resolution"
                         "gdouble" T T)))
             (get-g-type-definition "GdkScreen"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-screen-properties
  (let ((screen (gdk-screen-default)))
    (is (pointerp  (gdk-screen-font-options screen)))
    (is (typep (gdk-screen-resolution screen) 'double-float))))

;;; --- Signals ----------------------------------------------------------------

;;;         composited-changed

#+nil
(test gdk-screen-composited-changed-signal
  (let* ((message nil)
         (screen (gdk-screen-default))
         (handler-id (g-signal-connect screen "composited-changed"
                       (lambda (screen)
                         (setf message "Signal composited-changed")
                         (is (typep screen 'gdk-screen))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit screen "composited-changed"))
    (is (string= "Signal composited-changed" message))
    (is-false (g-signal-handler-disconnect screen handler-id))))

;;;         monitors-changed

#+nil
(test gdk-screen-monitors-changed-signal
  (let* ((message nil)
         (screen (gdk-screen-default))
         (handler-id (g-signal-connect screen "monitors-changed"
                       (lambda (screen)
                         (setf message "Signal monitors-changed")
                         (is (typep screen 'gdk-screen))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit screen "monitors-changed"))
    (is (string= "Signal monitors-changed" message))
    (is-false (g-signal-handler-disconnect screen handler-id))))

;;;         size-changed

#+nil
(test gdk-screen-size-changed-signal
  (let* ((message nil)
         (screen (gdk-screen-default))
         (handler-id (g-signal-connect screen "size-changed"
                       (lambda (screen)
                         (setf message "Signal size-changed")
                         (is (typep screen 'gdk-screen))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit screen "size-changed"))
    (is (string= "Signal size-changed" message))
    (is-false (g-signal-handler-disconnect screen handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-screen-default

(test gdk-screen-default
  (is (typep (gdk-screen-default) 'gdk-screen)))

;;;     gdk-screen-system-visual

(test gdk-screen-system-visual
  (is (typep (gdk-screen-system-visual (gdk-screen-default)) 'gdk-visual)))

;;;     gdk-screen-rgba-visual

(test gdk-screen-rgba-visual
  (is (typep (gdk-screen-rgba-visual (gdk-screen-default)) 'gdk-visual)))

;;;     gdk-screen-is-composited

(test gdk-screen-is-composited
  (is (gdk-screen-is-composited (gdk-screen-default))))

;;;     gdk-screen-root-window

(test gdk-screen-root-window
  (is (typep (gdk-screen-root-window (gdk-screen-default)) 'gdk-window)))

;;;     gdk-screen-display

(test gdk-screen-display
  (is (typep (gdk-screen-display (gdk-screen-default)) 'gdk-display)))

;;;     gdk-screen-number                                  deprecated

(test gdk-screen-number
  (is (integerp (gdk-screen-number (gdk-screen-default)))))

;;;     gdk-screen-width                                   deprecated

(test gdk-screen-width
  (is (integerp (gdk-screen-width)))
  (is (= (gdk-screen-width) (gdk-screen-width (gdk-screen-default)))))

;;;     gdk-screen-height                                  deprecated

(test gdk-screen-height
  (is (integerp (gdk-screen-height)))
  (is (= (gdk-screen-height) (gdk-screen-height (gdk-screen-default)))))

;;;     gdk-screen-width-mm                                deprecated

(test gdk-screen-width-mm
  (is (integerp (gdk-screen-width-mm)))
  (is (>= (gdk-screen-width-mm) (gdk-screen-width-mm (gdk-screen-default)))))

;;;     gdk-screen-height-mm                               deprecated

(test gdk-screen-height-mm
  (is (integerp (gdk-screen-height-mm)))
  (is (>= (gdk-screen-height-mm) (gdk-screen-height-mm (gdk-screen-default)))))

;;;     gdk-screen-list-visuals

(test gdk-screen-list-visuals
  (let ((screen (gdk-screen-default)))
    (is (> (length (gdk-screen-list-visuals screen)) 0))
    (is (every (lambda (x) (typep x 'gdk-visual))
               (gdk-screen-list-visuals screen)))))

;;;     gdk-screen-toplevel-windows

(test gdk-screen-toplevel-windows
  (is (listp (gdk-screen-toplevel-windows (gdk-screen-default))))
  (is (every (lambda (x) (typep x 'gdk-window))
             (gdk-screen-toplevel-windows (gdk-screen-default)))))

;;;     gdk-screen-make-display-name                       deprecated

(test gdk-screen-make-display-name
  (is (stringp (gdk-screen-make-display-name (gdk-screen-default)))))

;;;     gdk-screen-n-monitors                              deprecated

(test gdk-screen-n-monitors
  (is (<= 1 (gdk-screen-n-monitors (gdk-screen-default)))))

;;;     gdk_screen_get_primary_monitor                     deprecated

(test gdk-screen-primary-monitor
  (is (= 0 (gdk-screen-primary-monitor (gdk-screen-default)))))

;;;     gdk-screen-monitor-geometry                        deprecated

(test gdk-screen-monitor-geometry
  (is (typep (gdk-screen-monitor-geometry (gdk-screen-default) 0) 'gdk-rectangle))
  (let ((rect (gdk-screen-monitor-geometry (gdk-screen-default) 0)))
    (is (= 0 (gdk-rectangle-x rect)))
    (is (= 0 (gdk-rectangle-y rect)))
    (is (>= (gdk-screen-width) (gdk-rectangle-width rect)))
    (is (>= (gdk-screen-height) (gdk-rectangle-height rect)))))

;;;     gdk-screen-monitor-workarea                        deprecated

(test gdk-screen-monitor-workarea
  (is (typep (gdk-screen-monitor-workarea (gdk-screen-default) 0) 'gdk-rectangle))
  (let ((rect (gdk-screen-monitor-workarea (gdk-screen-default) 0)))
    (is (<=  0 (gdk-rectangle-x rect)))
    (is (<= 27 (gdk-rectangle-y rect)))
    (is (>= (gdk-screen-width) (gdk-rectangle-width rect)))
    (is (<= 741 (gdk-rectangle-height rect)))))

;;;     gdk-screen-monitor-at-point                        deprecated

(test gdk-screen-monitor-at-point
  (is (= 0 (gdk-screen-monitor-at-point (gdk-screen-default)  0  0)))
  (is (= 0 (gdk-screen-monitor-at-point (gdk-screen-default) 10 10))))

;;;     gdk-screen-monitor-at-window                       deprecated

(test gdk-screen-monitor-at-window
  (let ((screen (gdk-screen-default)))
    (is (= 0 (gdk-screen-monitor-at-window screen (gdk-screen-root-window screen))))))

;;;     gdk-screen-monitor-height-mm                       deprecated

(test gdk-screen-monitor-height-mm
  (is (<= 193 (gdk-screen-monitor-height-mm (gdk-screen-default) 0))))

;;;     gdk-screen-monitor-width-mm                        deprecated

(test gdk-screen-monitor-width-mm
  (is (<= 344 (gdk-screen-monitor-width-mm (gdk-screen-default) 0))))

;;;     gdk-screen-monitor-plug-name                       deprecated

(test gdk-screen-monitor-plug-name
  (is (stringp (gdk-screen-monitor-plug-name (gdk-screen-default) 0))))

;;;     gdk-screen-monitor-scale-factor                    deprecated

(test gdk-screen-monitor-scale-factor
  (is (= 1 (gdk-screen-monitor-scale-factor (gdk-screen-default) 0))))

;;;     gdk-screen-setting

(test gdk-screen-setting
  (let ((screen (gdk-display-default-screen (gdk-display-default))))
    (is (= 400 (gdk-screen-setting screen "gtk-double-click-time" "gint")))))

;;;     gdk-screen-active-window                           deprecated

(test gdk-screen-active-window
  (is (typep (gdk-screen-active-window (gdk-screen-default)) 'gdk-window)))

;;;     gdk-screen-window-stack

(test gdk-screen-window-stack
  (is (listp (gdk-screen-window-stack (gdk-screen-default))))
  (is (every (lambda (x) (typep x 'gdk-window))
             (gdk-screen-window-stack (gdk-screen-default)))))

;;; 2021-8-20
