(def-suite gdk-display-manager :in gdk-suite)
(in-suite gdk-display-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDisplayManager

(test gdk-display-manager-class
  ;; Type check
  (is (g-type-is-object "GdkDisplayManager"))
  ;; Check the registered name
  (is (eq 'gdk-display-manager
          (registered-object-type-by-name "GdkDisplayManager")))
  ;; Check the type initializer
  (is (eq (gtype "GdkDisplayManager")
          (gtype (foreign-funcall "gdk_display_manager_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GdkDisplayManager")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GdkDisplayManager"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GdkDisplayManager"))))
  ;; Check the class properties
  (is (equal '("default-display")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GdkDisplayManager"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDisplayManager" GDK-DISPLAY-MANAGER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_display_manager_get_type")
                       ((DEFAULT-DISPLAY GDK-DISPLAY-MANAGER-DEFAULT-DISPLAY
                         "default-display" "GdkDisplay" T T)))
             (get-g-type-definition "GdkDisplayManager"))))

;;; --- Properties -------------------------------------------------------------

;;;   gdk-display-manager-default-display

(test gdk-display-manager-default-display.1
  (let ((manager (gdk-display-manager-get)))
    (is (typep (gdk-display-manager-default-display manager) 'gdk-display))))

(test gdk-display-manager-default-display.2
  (let* ((manager (gdk-display-manager-get))
         (display (gdk-display-manager-default-display manager)))
    (setf (gdk-display-manager-default-display manager) display)
    (is (eq display
            (gdk-display-manager-default-display manager)))))

;;; --- Signals ----------------------------------------------------------------

;;;     display-opened

(test gdk-display-manager-display-opened-signal
  (let* ((message nil)
         (manager (gdk-display-manager-get))
         (display (gdk-display-manager-default-display manager))
         (handler-id (g-signal-connect manager "display-opened"
                       (lambda (manager display)
                         (setf message "Signal display-opened")
                         (is (typep manager 'gdk-display-manager))
                         (is (typep display 'gdk-display))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit manager "display-opened" display))
    (is (string= "Signal display-opened" message))
    (is-false (g-signal-handler-disconnect manager handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-display-manager-get

(test gdk-display-manager-get
  (is (typep (gdk-display-manager-get) 'gdk-display-manager)))

;;;     gdk-display-manager-list-displays

(test gdk-display-manager-list-displays
  (let ((manager (gdk-display-manager-get)))
    (is (listp (gdk-display-manager-list-displays manager)))
    (is (every (lambda (x) (typep x 'gdk-display))
               (gdk-display-manager-list-displays manager)))))

;;;     gdk-display-manager-open-display

(test gdk-display-manager-open-display
  (let* ((manager (gdk-display-manager-get))
         (name (gdk-display-name (gdk-display-manager-default-display manager))))
    (is-true (gdk-display-manager-open-display manager name))))

;;; 2020-11-7
