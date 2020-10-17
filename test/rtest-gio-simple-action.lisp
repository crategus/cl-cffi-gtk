(def-suite g-simple-action :in gio-suite)
(in-suite g-simple-action)

(defvar *g-simple-action-verbose* nil)

;;; --- Types and Values -------------------------------------------------------

;;;   GSimpleAction

(test g-simple-action-class
  ;; Type check
  (is-true  (g-type-is-object "GSimpleAction"))
  ;; Check the registered name
  (is (eq 'g-simple-action
          (registered-object-type-by-name "GSimpleAction")))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GSimpleAction")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GSimpleAction"))))
  ;; Check the interfaces
  (is (equal '("GAction")
             (mapcar #'gtype-name (g-type-interfaces "GSimpleAction"))))
  ;; Check the class properties
  (is (equal '("enabled" "name" "parameter-type" "state" "state-type")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GSimpleAction"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GSimpleAction" G-SIMPLE-ACTION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES ("GAction"))
                       ((ENABLED G-SIMPLE-ACTION-ENABLED "enabled" "gboolean" T
                         T)
                        (NAME G-SIMPLE-ACTION-NAME "name" "gchararray" T NIL)
                        (PARAMETER-TYPE G-SIMPLE-ACTION-PARAMETER-TYPE
                         "parameter-type" "GVariantType" T NIL)
                        (STATE G-SIMPLE-ACTION-STATE "state" "GVariant" T T)
                        (STATE-TYPE G-SIMPLE-ACTION-STATE-TYPE "state-type"
                         "GVariantType" T NIL)))
             (get-g-type-definition "GSimpleAction"))))

;;; --- Properties and Accessors -----------------------------------------------

(test g-simple-action-properties
  (let ((action (make-instance 'g-simple-action :name "simple"
                                                :parameter-type (g-variant-type-new "b"))))
    (is-true (g-simple-action-enabled action))
    (is (string= "simple" (g-simple-action-name action)))
    (is (eq 'g-variant-type (type-of (g-simple-action-parameter-type action))))
    (is (equal "b" (g-variant-type-dup-string (g-simple-action-parameter-type action))))
    (is-true (null-pointer-p (g-simple-action-state action)))
    ;; The slot state-type is not initialized, the accessor signals an error
    (signals (error) (g-simple-action-state-type action)))

  (let ((action (make-instance 'g-simple-action :name "simple"
                                                :parameter-type (g-variant-type-new "b")
                                                :state (g-variant-new-string "text"))))
    (is-true (g-simple-action-enabled action))
    (is (string= "simple" (g-simple-action-name action)))
    (is (eq 'g-variant-type (type-of (g-simple-action-parameter-type action))))
    (is (equal "b" (g-variant-type-dup-string (g-simple-action-parameter-type action))))
    ;; Slot state
    (is-true (pointerp (g-simple-action-state action)))
    (is (string= "s" (g-variant-type-dup-string (g-variant-type (g-simple-action-state action)))))
    (is (string= "text" (g-variant-string (g-simple-action-state action))))
    ;; Slot state-type
    (is (eq 'g-variant-type (type-of (g-simple-action-state-type action))))
    (is (string= "s" (g-variant-type-dup-string (g-simple-action-state-type action))))
    ;; Slot state-type is not writeable
    (signals (error) (setf (g-simple-action-state-type action) (g-variant-type-new "b")))))

;;; --- Functions --------------------------------------------------------------

;;;   g_simple_action_new

(test g-simple-action-new
  (let ((action (g-simple-action-new "simple" (g-variant-type-new "b"))))
    (is (eq 'g-simple-action (type-of action)))
    (is (equal "simple" (g-action-name action)))
    (is (eq 'g-variant-type (type-of (g-action-parameter-type action))))
    (is (equal "b" (g-variant-type-peek-string (g-action-parameter-type action))))))

;;;   g_simple_action_new_stateful

(test g-simple-action-new-stateful
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    (is (eq 'g-simple-action (type-of action)))
    (is (equal "simple" (g-action-name action)))
    (is (eq 'g-variant-type (type-of (g-action-parameter-type action))))
    (is (equal "b" (g-variant-type-peek-string (g-action-parameter-type action))))
    (is-true (g-variant-boolean (g-action-state action)))))

;;; --- Functions from the interface -------------------------------------------

(test g-simple-action-interface-functions
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    ;; g-action-name
    (is (equal "simple" (g-action-name action)))
    ;; g-action-parameter-type
    (is (eq 'g-variant-type (type-of (g-action-parameter-type action))))
    ;; g-action-state-type
    (is (eq 'g-variant-type (type-of (g-action-state-type action))))
    ;; We have to initialize the state with a g-variant-array to set a hint
    (is-true (null-pointer-p (g-action-get-state-hint action)))
    ;; g-action-enabled
    (is-true (g-action-enabled action))
    ;; g-action-state
    (is-true (g-variant-boolean (g-action-state action)))))

;;;   g_action_change_state
;;;   g_action_activate

#+nil
(test g-simple-action-signals
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    ;; Connect available signals
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (when *g-simple-action-verbose*
           (format t "~&GSimpleAction: signal 'activate' occured.~%")
           (format t "~&    action    : ~A~%" action)
           (format t "~&    name      : ~A~%" (g-action-name action))
           (format t "~&    parameter : ~A~%" parameter))))
    (g-signal-connect action "change-state"
       (lambda (action value)
         (setf (g-simple-action-state action) value)
         (when *g-simple-action-verbose*
           (format t "~&GSimpleAction: signal 'change-state' occured.~%")
           (format t "~&    action : ~A~%" action)
           (format t "~&    name   : ~A~%" (g-action-name action))
           (format t "~&    value  : ~A~%" value))))
    ;; g-action-change-state
    (g-action-change-state action (g-variant-new-boolean nil))
    (is-false (g-variant-boolean (g-action-state action)))
    ;; g-action-activate
    (g-action-activate action (g-variant-new-boolean t))))

;;;   g_simple_action_enabled

#+nil
(test g-simple-action-enabled
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    ;; Connect available signals
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (when *g-simple-action-verbose*
           (format t "~&GSimpleAction: signal 'activate'.~%")
           (format t "~&    action    : ~A~%" action)
           (format t "~&    name      : ~A~%" (g-action-name action))
           (format t "~&    parameter : ~A~%" parameter))))
    (g-signal-connect action "change-state"
       (lambda (action value)
         (setf (g-simple-action-state action) value)
         (when *g-simple-action-verbose*
           (format t "~&GSimpleAction: signal 'change-state'.~%")
           (format t "~&    action : ~A~%" action)
           (format t "~&    name   : ~A~%" (g-action-name action))
           (format t "~&    value  : ~A~%" value))))

    (is-true (setf (g-simple-action-enabled action)  t))
    (g-signal-emit action "activate" (g-variant-new-boolean nil))
    ;; TODO: This seems not to work. We can activate the action.
    (is-false (setf (g-simple-action-enabled action) nil))
    (is-false (g-action-enabled action))
    (g-signal-emit action "activate" (g-variant-new-boolean nil))))

;;;   g_simple_action_set_state

(test g-simple-action-state
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    ;;g-simple-action-state
    (setf (g-simple-action-state action) (g-variant-new-boolean nil))
    (is-false (g-variant-boolean (g-action-state action)))
    (setf (g-simple-action-state action) (g-variant-new-boolean t))
    (is-true (g-variant-boolean (g-action-state action)))))

;;;   Example from the API documentation

#+nil
(test change-volume-state
  (let ((action (g-simple-action-new-stateful "volume"
                                              (g-variant-type-new "i") ; int32
                                              (g-variant-new-int32 0))))
    (g-signal-connect action "change-state"
                      (lambda (simple-action value)
                        (let ((requested (g-variant-int32 value)))
                          ;; Volume only goes from 0 to 10
                          (when (and (>= requested 0) (<= requested 10))
                            (setf (g-simple-action-state simple-action) value)))))

    ;; Emit the "change-state" signal on action
    (g-action-change-state action (g-variant-new-int32 5))
    (is (= 5 (g-variant-int32 (g-action-state action))))
    ;; Emit the "change-state" signal for 10
    (g-action-change-state action (g-variant-new-int32 10))
    (is (= 10 (g-variant-int32 (g-action-state action))))
    ;; Emit the "change-state" signal for 20
    (g-action-change-state action (g-variant-new-int32 20))
    ;; The state has not changed.
    (is (= 10 (g-variant-int32 (g-action-state action))))))
